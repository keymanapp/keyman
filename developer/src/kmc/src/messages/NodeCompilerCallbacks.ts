import * as fs from 'fs';
import * as path from 'path';
import { CompilerCallbacks, CompilerSchema, CompilerEvent, compilerErrorSeverityName, CompilerPathCallbacks, CompilerFileSystemCallbacks, CompilerErrorSeverity, compilerErrorSeverity } from '@keymanapp/common-types';

/**
 * Concrete implementation for CLI use
 */

interface NodeCompilerCallbacksOptions {
  quiet?: boolean;
}

const defaultOptions: NodeCompilerCallbacksOptions = {
  quiet: false
};

export class NodeCompilerCallbacks implements CompilerCallbacks {
  private options: NodeCompilerCallbacksOptions;
  private events: CompilerEvent[] = [];

  constructor(options?: NodeCompilerCallbacksOptions) {
    this.options = {...defaultOptions, ...options};
  }

  // TODO: consolidate with this.fs.readFileSync
  loadFile(filename: string | URL): Buffer {
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
    this.events.push(event);
    if(this.options.quiet && compilerErrorSeverity(event.code) < CompilerErrorSeverity.Error) {
      // We'll only print errors if we are in 'quiet' mode
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
    if(!this.options.quiet) {
      console.debug(msg);
    }
  }

  loadSchema(schema: CompilerSchema) {
    let schemaPath = new URL('../util/' + schema + '.schema.json', import.meta.url);
    return fs.readFileSync(schemaPath);
  }

  fileExists(filename: string) {
    return fs.existsSync(filename);
  }

  resolveFilename(baseFilename: string, filename: string) {
    const basePath = path.dirname(baseFilename);
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
