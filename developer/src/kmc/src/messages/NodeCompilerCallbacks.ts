import * as fs from 'fs';
import * as path from 'path';
import { CompilerCallbacks, CompilerSchema, CompilerEvent, compilerErrorSeverity,
         compilerErrorSeverityName, CompilerPathCallbacks, CompilerFileSystemCallbacks,
         CompilerLogLevel, compilerLogLevelToSeverity } from '@keymanapp/common-types';
import { InfrastructureMessages } from './messages.js';

/**
 * Concrete implementation for CLI use
 */

// TODO: Make a common class for all the CompilerCallbacks implementations


export interface CompilerCallbackOptions {
  logLevel?: CompilerLogLevel;
}

export class NodeCompilerCallbacks implements CompilerCallbacks {
  /* NodeCompilerCallbacks */

  messages: CompilerEvent[] = [];

  constructor(private options: CompilerCallbackOptions) {
  }

  clear() {
    this.messages = [];
  }

  hasMessage(code: number): boolean {
    return this.messages.find((item) => item.code == code) === undefined ? false : true;
  }

  private verifyFilenameConsistency(originalFilename: string): void {
    if(fs.existsSync(originalFilename)) {
      // Note, we only check this if the file exists, because
      // if it is not found, that will be returned as an error
      // from loadFile anyway.
      const filename = fs.realpathSync(originalFilename);
      const nativeFilename = fs.realpathSync.native(filename);
      if(filename != nativeFilename) {
        this.reportMessage(InfrastructureMessages.Hint_FilenameHasDifferingCase({
          reference: originalFilename,
          filename: nativeFilename
        }));
      }
    }
  }

  /* CompilerCallbacks */

  loadFile(filename: string): Uint8Array {
    this.verifyFilenameConsistency(filename);
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

  get path(): CompilerPathCallbacks {
    return path;
  }

  get fs(): CompilerFileSystemCallbacks {
    return fs;
  }

  reportMessage(event: CompilerEvent): void {
    this.messages.push(event);

    if(compilerErrorSeverity(event.code) < compilerLogLevelToSeverity[this.options.logLevel]) {
      // collect messages but don't print to console
      return;
    }

    const code = event.code.toString(16);
    if(event.line) {
      console.log(`${compilerErrorSeverityName(event.code)} ${code} [${event.line}]: ${event.message}`);
    } else {
      console.log(`${compilerErrorSeverityName(event.code)} ${code}: ${event.message}`);
    }
  }

  debug(msg: string) {
    if(this.options.logLevel == 'debug') {
      console.debug(msg);
    }
  }

  loadSchema(schema: CompilerSchema): Uint8Array {
    let schemaPath = new URL('../util/' + schema + '.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }

  fileExists(filename: string) {
    return fs.existsSync(filename);
  }

  resolveFilename(baseFilename: string, filename: string) {
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

}
