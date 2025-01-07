import * as fs from 'fs';
import * as path from 'path';
import { CompilerEvent, CompilerCallbacks, CompilerPathCallbacks, CompilerFileSystemCallbacks,
  CompilerError, CompilerNetAsyncCallbacks, DefaultCompilerFileSystemAsyncCallbacks,
  CompilerFileSystemAsyncCallbacks } from '@keymanapp/developer-utils';
import { fileURLToPath } from 'url';

const { TEST_SAVE_FIXTURES } = process.env;

/**
 * A CompilerCallbacks implementation for testing
 */
export class TestCompilerCallbacks implements CompilerCallbacks {
  /* TestCompilerCallbacks */

  messages: CompilerEvent[] = [];
  readonly _net: TestCompilerNetAsyncCallbacks;
  readonly _fsAsync: DefaultCompilerFileSystemAsyncCallbacks = new DefaultCompilerFileSystemAsyncCallbacks(this);

  constructor(basePath?: string) {
    if(basePath) {
      this._net = new TestCompilerNetAsyncCallbacks(basePath);
    }
  }

  clear() {
    this.messages = [];
  }

  printMessages() {
    if(this.messages.length) {
      process.stdout.write(CompilerError.formatEvent(this.messages));
    }
  }

  hasMessage(code: number): boolean {
    return this.messages.find((item) => item.code == code) === undefined ? false : true;
  }

  fileURLToPath(url: string | URL): string {
    return fileURLToPath(url);
  }

  /** true of at least one error */
  hasError(): boolean {
    return CompilerError.hasError(this.messages);
  }

  /* CompilerCallbacks */

  loadFile(filename: string): Uint8Array {
    try {
      return fs.readFileSync(filename);
    } catch(e) {
      if (e.code === 'ENOENT') {
        return null;
      } else {
        throw e;
      }
    }
  }

  fileSize(filename: string): number {
    return fs.statSync(filename)?.size;
  }

  isDirectory(filename: string): boolean {
    return fs.statSync(filename)?.isDirectory();
  }

  get path(): CompilerPathCallbacks {
    return {
      ...path,
      isAbsolute: path.win32.isAbsolute
    };
  }

  get fs(): CompilerFileSystemCallbacks {
    return fs;
  }

  get net(): CompilerNetAsyncCallbacks {
    return this._net;
  }

  get fsAsync(): CompilerFileSystemAsyncCallbacks {
    return this._fsAsync;
  }

  resolveFilename(baseFilename: string, filename: string): string {
    const basePath =
      baseFilename.endsWith('/') || baseFilename.endsWith('\\') ?
      baseFilename :
      path.dirname(baseFilename);
    // Transform separators to platform separators -- we are agnostic
    // in our use here but path prefers files may use
    // either / or \, although older kps files were always \.
    if(path.sep == '/') {
      filename = filename.replace(/\\/g, '/');
    } else {
      filename = filename.replace(/\//g, '\\');
    }
    if(!path.isAbsolute(filename)) {
      filename = path.resolve(basePath, filename);
    }
    return filename;
  }

  reportMessage(event: CompilerEvent): void {
    // console.log(event.message);
    this.messages.push(event);
  }

  debug(msg: string) {
    console.debug(msg);
  }
};

class TestCompilerNetAsyncCallbacks implements CompilerNetAsyncCallbacks {
  constructor(private basePath: string) {
  }

  urlToPath(url: string): string {
    const p = new URL(url);
    return path.join(this.basePath, p.hostname, p.pathname.replaceAll(/[^a-z0-9_.!@#$%() -]/ig, '#'));
  }

  async fetchBlob(url: string): Promise<Uint8Array> {
    const p = this.urlToPath(url);

    if(TEST_SAVE_FIXTURES) {
      // When TEST_SAVE_FIXTURES env variable is set, we will do the actual
      // fetch from the origin so that we can build the fixtures easily
      console.log(`Downloading file ${url} --> ${p}`);
      let response: Response;
      try {
        response = await fetch(url);
      } catch(e) {
        console.error(`failed to download ${url}`);
        console.error(e);
        throw e; // yes, we want to abort the download
      }

      fs.mkdirSync(path.dirname(p), {recursive: true});
      if(!response.ok) {
        // We won't save a file, just delete any existing file
        if(fs.existsSync(p)) {
          fs.rmSync(p);
        }
      } else {
        const data = new Uint8Array(await response.arrayBuffer());
        fs.writeFileSync(p, data);
      }
    }

    if(!fs.existsSync(p)) {
      // missing file, this is okay
      return null;
    }
    const data: Uint8Array = fs.readFileSync(p);
    return data;
  }

  async fetchJSON(url: string): Promise<any> {
    const data = await this.fetchBlob(url);
    return data ? JSON.parse(new TextDecoder().decode(data)) : null;
  }
}
