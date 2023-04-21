import * as fs from 'fs';
import { CompilerCallbacks, CompilerEvent, compilerErrorSeverityName } from '@keymanapp/common-types';

/**
 * Concrete implementation for CLI use
 */
export class NodeCompilerCallbacks implements CompilerCallbacks {
  loadFile(baseFilename: string, filename: string | URL): Buffer {
    // TODO: translate filename based on the baseFilename
    try {
      return fs.readFileSync(filename);
    } catch (e) {
      if (e.code === 'ENOENT') {
        return null;
      } else {
        throw e;
      }
    }
  }

  reportMessage(event: CompilerEvent): void {
    const code = event.code.toString(16);
    if(event.line) {
      console.log(`${compilerErrorSeverityName(event.code)} ${code} [${event.line}]: ${event.message}`);
    } else {
      console.log(`${compilerErrorSeverityName(event.code)} ${code}: ${event.message}`);
    }
  }

  loadLdmlKeyboardSchema(): Buffer {
    let schemaPath = new URL('ldml-keyboard.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }
  loadLdmlKeyboardTestSchema(): Buffer {
    let schemaPath = new URL('ldml-keyboardtest.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }
  loadKvksJsonSchema(): Buffer {
    let schemaPath = new URL('kvks.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }
  loadKpjJsonSchema(): Buffer {
    let schemaPath = new URL('kpj.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }
}
