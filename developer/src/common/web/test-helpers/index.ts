import * as fs from 'fs';
import { CompilerEvent, CompilerCallbacks, CompilerSchema } from '@keymanapp/common-types';

// TODO: schemas are only used by kmc-keyboard for now, so this works at this
// time, but it's a little fragile if we need them elsewhere in the future
const SCHEMA_BASE = '../../../../kmc-keyboard/build/src/';

/**
 * A CompilerCallbacks implementation for testing
 */
export class TestCompilerCallbacks implements CompilerCallbacks {
  /* TestCompilerCallbacks */

  messages: CompilerEvent[] = [];

  clear() {
    this.messages = [];
  }

  hasMessage(code: number): boolean {
    return this.messages.find((item) => item.code == code) === undefined ? false : true;
  }

  /* CompilerCallbacks */

  loadFile(baseFilename: string, filename: string | URL): Buffer {
    // TODO: translate filename based on the baseFilename
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
  reportMessage(event: CompilerEvent): void {
    // console.log(event.message);
    this.messages.push(event);
  }

  loadSchema(schema: CompilerSchema): Buffer {
    return fs.readFileSync(new URL(SCHEMA_BASE + schema + '.schema.json', import.meta.url));
  }

  debug(msg: string) {
    console.debug(msg);
  }
};