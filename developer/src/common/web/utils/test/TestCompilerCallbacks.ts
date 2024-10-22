import * as fs from 'fs';
import * as path from 'path';
import { loadFile, resolveFilename } from './helpers/index.js';
import { CompilerCallbacks, CompilerError, CompilerEvent, CompilerFileSystemCallbacks, CompilerPathCallbacks } from '../src/compiler-interfaces.js';
import { fileURLToPath } from 'url';

// This is related to developer/src/common/web/test-helpers/index.ts but has a slightly different API surface
// as this runs at a lower level than the compiler.
/**
 * A CompilerCallbacks implementation for testing
 */
export class TestCompilerCallbacks implements CompilerCallbacks {
  clear() {
    this.messages = [];
  }
  debug(msg: string): void {
    console.debug(msg);
  }

  printMessages() {
    process.stdout.write(CompilerError.formatEvent(this.messages));
  }

  messages: CompilerEvent[] = [];

  get path(): CompilerPathCallbacks {
    return path;
  }

  get fs(): CompilerFileSystemCallbacks {
    return fs;
  }

  resolveFilename(baseFilename: string, filename: string): string {
    return resolveFilename(baseFilename, filename);
  }

  fileURLToPath(url: string | URL): string {
    return fileURLToPath(url);
  }

  loadFile(filename: string): Uint8Array {
    // TODO: error management, does it belong here?
    try {
      return loadFile(filename);
    } catch (e) {
      if (e.code === 'ENOENT') {
        return <Uint8Array><unknown>null;
      } else {
        throw e;
      }
    }
  }

  fileSize(filename: string): number {
    return fs.statSync(filename).size;
  }

  isDirectory(filename: string): boolean {
    return fs.statSync(filename)?.isDirectory();
  }

  reportMessage(event: CompilerEvent): void {
    // console.log(event.message);
    this.messages.push(event);
  }
}
