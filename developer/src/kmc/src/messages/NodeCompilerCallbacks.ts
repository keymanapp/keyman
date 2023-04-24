import * as fs from 'fs';
import { CompilerCallbacks, CompilerSchema, CompilerEvent, compilerErrorSeverityName } from '@keymanapp/common-types';

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

  debug(msg: string) {
    console.debug(msg);
  }

  loadSchema(schema: CompilerSchema) {
    let schemaPath = new URL('../util/' + schema + '.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }
}
