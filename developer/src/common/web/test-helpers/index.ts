import * as fs from 'fs';
import { CompilerEvent, CompilerCallbacks } from '@keymanapp/common-types';

// TODO: schemas are only used by kmc-keyboard for now, so this works at this
// time, but it's a little fragile if we need them elsewhere in the future
const SCHEMA_BASE = '../../../../kmc-keyboard/build/src/';

/**
 * A CompilerCallbacks implementation for testing
 */
export class TestCompilerCallbacks implements CompilerCallbacks {
  clear() {
    this.messages = [];
  }
  messages: CompilerEvent[] = [];
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
  loadLdmlKeyboardSchema(): Buffer {
    return fs.readFileSync(new URL(SCHEMA_BASE + 'ldml-keyboard.schema.json', import.meta.url));
  }
  loadKvksJsonSchema(): Buffer {
    return fs.readFileSync(new URL(SCHEMA_BASE + 'kvks.schema.json', import.meta.url));
  }
  loadKpjJsonSchema(): Buffer {
    return fs.readFileSync(new URL(SCHEMA_BASE + 'kpj.schema.json', import.meta.url));
  }
  loadLdmlKeyboardTestSchema(): Buffer {
    return fs.readFileSync(new URL(SCHEMA_BASE + 'ldml-keyboardtest.schema.json', import.meta.url));
  }
};