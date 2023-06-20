import * as fs from 'fs';
import * as path from 'path';
import { CompilerEvent, CompilerCallbacks, CompilerSchema, CompilerPathCallbacks, CompilerFileSystemCallbacks, compilerErrorSeverityName } from '@keymanapp/common-types';
export { verifyCompilerMessagesObject } from './verifyCompilerMessagesObject.js';

// TODO: schemas are only used by kmc-ldml for now, so this works at this
// time, but it's a little fragile if we need them elsewhere in the future
const SCHEMA_BASE = '../../../../kmc-ldml/build/src/';

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
    this.messages.forEach(event => {
      const code = event.code.toString(16);
      if(event.line) {
        console.log(`${compilerErrorSeverityName(event.code)} ${code} [${event.line}]: ${event.message}`);
      } else {
        console.log(`${compilerErrorSeverityName(event.code)} ${code}: ${event.message}`);
      }
    });
  }

  hasMessage(code: number): boolean {
    return this.messages.find((item) => item.code == code) === undefined ? false : true;
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

  loadSchema(schema: CompilerSchema): Uint8Array {
    return fs.readFileSync(new URL(SCHEMA_BASE + schema + '.schema.json', import.meta.url));
  }

  debug(msg: string) {
    console.debug(msg);
  }
};