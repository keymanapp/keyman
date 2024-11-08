import * as fs from 'fs';
import * as path from 'path';
import { CompilerEvent, CompilerCallbacks, CompilerPathCallbacks, CompilerFileSystemCallbacks, CompilerError } from '@keymanapp/developer-utils';
import { fileURLToPath } from 'url';
export { verifyCompilerMessagesObject } from './verifyCompilerMessagesObject.js';

/**
 * A CompilerCallbacks implementation for testing
 */
export class TestCompilerCallbacks implements CompilerCallbacks {
  /* TestCompilerCallbacks */

  messages: CompilerEvent[] = [];

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
    return path;
  }

  get fs(): CompilerFileSystemCallbacks {
    return fs;
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
