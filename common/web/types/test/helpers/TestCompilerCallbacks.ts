import * as fs from 'fs';
import * as path from 'path';
import { loadFile, loadSchema, resolveFilename } from '../helpers/index.js';
import { CompilerCallbacks, CompilerError, CompilerEvent, CompilerFileSystemCallbacks, CompilerPathCallbacks, CompilerSchema } from '../../src/util/compiler-interfaces.js';

// This is related to developer/src/common/web/test-helpers/index.ts but has a slightly different API surface
// as this runs at a lower level than the compiler.
/**
 * A CompilerCallbacks implementation for testing
 */
export class TestCompilerCallbacks implements CompilerCallbacks {
  loadSchema(schema: CompilerSchema): Buffer {
    switch (schema) {
      case 'kpj':
      case 'kpj-9.0':
      case 'kvks':
      case 'displaymap':
        // listing all unimplemented schemas rather than using 'default'
        throw new Error(`loadSchema(${schema}) not implemented.`); // not needed for this test
      case 'ldml-keyboard':
        return loadSchema(schema);
      case 'ldml-keyboardtest':
        return loadSchema(schema);
    }
  }
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

  loadFile(filename: string): Uint8Array {
    // TODO: error management, does it belong here?
    try {
      return loadFile(filename);
    } catch (e) {
      if (e.code === 'ENOENT') {
        return null;
      } else {
        throw e;
      }
    }
  }

  reportMessage(event: CompilerEvent): void {
    // console.log(event.message);
    this.messages.push(event);
  }
}
