import * as fs from 'fs';
import * as path from 'path';
import { CompilerEvent, CompilerCallbacks } from '@keymanapp/common-types';

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
    return fs.readFileSync(new URL(path.join('..', '..', 'src', 'ldml-keyboard.schema.json'), import.meta.url));
  }
  loadKvksJsonSchema(): Buffer {
    return fs.readFileSync(new URL(path.join('..', '..', 'src', 'kvks.schema.json'), import.meta.url));
  }
  loadKpjJsonSchema(): Buffer {
    return fs.readFileSync(new URL(path.join('..', '..', 'src', 'kpj.schema.json'), import.meta.url));
  }
  loadLdmlKeyboardTestSchema(): Buffer {
    return fs.readFileSync(new URL(path.join('..', '..', 'src', 'ldml-keyboardtest.schema.json'), import.meta.url));
  }
};