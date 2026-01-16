/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Code related to Extension support for callbacks.
 */

import { CompilerCallbackOptions, CompilerCallbacks, CompilerError, CompilerEvent, CompilerFileCallbacks, CompilerFileSystemAsyncCallbacks, CompilerFileSystemCallbacks, CompilerNetAsyncCallbacks, CompilerPathCallbacks, FileSystemFolderEntry } from "@keymanapp/developer-utils";
import * as fs from 'fs';
import * as path from 'node:path';


interface ErrorWithCode {
    code?: string;
}

function resolveFilename(baseFilename: string, filename: string) {
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

class NodeCompilerFileSystemAsyncCallbacks implements CompilerFileSystemAsyncCallbacks {
  async exists(filename: string): Promise<boolean> {
    return fs.existsSync(filename);
  }

  async readFile(filename: string): Promise<Uint8Array> {
    return fs.readFileSync(filename);
  }

  async readdir(filename: string): Promise<FileSystemFolderEntry[]> {
    return fs.readdirSync(filename).map(item => ({
      filename: item,
      type: fs.statSync(path.join(filename, item))?.isDirectory() ? 'dir' : 'file'
    }));
  }

  resolveFilename(baseFilename: string, filename: string): string {
    return resolveFilename(baseFilename, filename);
  }
}

class NodeCompilerNetAsyncCallbacks implements CompilerNetAsyncCallbacks {
  async fetchBlob(url: string, options?: RequestInit): Promise<Uint8Array> {
    try {
      const response = await fetch(url, options);
      if(!response.ok) {
        throw new Error(`HTTP error ${response.status}: ${response.statusText}`);
      }

      const data = await response.blob();
      return new Uint8Array(await data.arrayBuffer());
    } catch(e) {
      throw new Error(`Error downloading ${url}`, {cause:e});
    }
  }

  async fetchJSON(url: string, options?: RequestInit): Promise<any> {
    try {
      const response = await fetch(url, options);
      if(!response.ok) {
        throw new Error(`HTTP error ${response.status}: ${response.statusText}`);
      }

      return await response.json();
    } catch(e) {
      throw new Error(`Error downloading ${url}`, {cause:e});
    }
  }
}


export class ExtensionCallbacks implements CompilerCallbacks {
    /* from NodeCompilerCallbacks */

    messages: CompilerEvent[] = [];
    messageCount = 0;
    messageFilename: string = '';

    constructor(private options: CompilerCallbackOptions, private msg: (m: string)=>void) {
    }
  resolveFilename(baseFilename: string, filename: string): string {
    return resolveFilename(baseFilename, filename);
  }
  isDirectory(filename: string): boolean {
    return fs.statSync(filename)?.isDirectory();
  }
  get fsAsync(): CompilerFileSystemAsyncCallbacks {
    return new NodeCompilerFileSystemAsyncCallbacks();
  }
  get net(): CompilerNetAsyncCallbacks {
    return new NodeCompilerNetAsyncCallbacks();;
  }
  fileURLToPath(url: string | URL): string {
    throw new Error("Method not implemented.");
  }

    clear() {
      this.messages = [];
      this.messageCount = 0;
      this.messageFilename = '';
    }

    /**
     * Returns true if any message in the log is a Fatal, Error, or if we are
     * treating warnings as errors, a Warning. The warning option will be taken
     * from the CompilerOptions passed to the constructor, or the parameter, to
     * allow for per-file overrides (as seen with projects, for example).
     * @param compilerWarningsAsErrors
     * @returns
     */
    hasFailureMessage(compilerWarningsAsErrors?: boolean): boolean {
      return CompilerFileCallbacks.hasFailureMessage(
        this.messages,
        // parameter overrides global option
        false, // compilerWarningsAsErrors ?? this.options.compilerWarningsAsErrors
      );
    }

    hasMessage(code: number): boolean {
      return this.messages.find((item) => item.code == code) === undefined ? false : true;
    }


    /* CompilerCallbacks */

    loadFile(filename: string): Uint8Array {
        //   this.verifyFilenameConsistency(filename);
        try {
            return fs.readFileSync(filename);
        } catch (e) {
            const { code } = e as ErrorWithCode;
            if (code === 'ENOENT') {
                // code smell…
                return (null as unknown) as Uint8Array;
            } else {
                throw e;
            }
        }
    }

    fileSize(filename: string): number {
      return fs.statSync(filename)?.size;
    }

    get path(): CompilerPathCallbacks {
      return path;
    }

    get fs(): CompilerFileSystemCallbacks {
      return (fs as unknown) as CompilerFileSystemCallbacks;
    }

    reportMessage(event: CompilerEvent): void {
      if(!event.filename) {
        event.filename = this.messageFilename;
      }

      if(this.messageFilename != event.filename) {
        // Reset max message limit when a new file is being processed
        this.messageFilename = event.filename;
        this.messageCount = 0;
      }


      this.messages.push({...event});

    //   // report fatal errors to Sentry, but don't abort; note, it won't be
    //   // reported if user has disabled the Sentry setting
    //   if(CompilerError.severity(event.code) == CompilerErrorSeverity.Fatal) {
    //     // this is async so returns a Promise, we'll let it resolve in its own
    //     // time, and it will emit a message to stderr with details at that time
    //     KeymanSentry.reportException(event.exceptionVar ?? event.message, false);
    //   }

    //   if(disable || CompilerError.severity(event.code) < compilerLogLevelToSeverity[this.options.logLevel]) {
    //     // collect messages but don't print to console
    //     return;
    //   }

      // We don't use this.messages.length because we only want to count visible
      // messages, and there's no point in recalculating the total for every
      // message emitted.

      this.messageCount++;
    //   if(this.messageCount > 99/*MaxMessagesDefault*/) {
    //     return;
    //   }

    //   if(this.messageCount == 99/*MaxMessagesDefault*/) {
    //     // We've hit our event limit so we'll suppress further messages, and emit
    //     // our little informational message so users know what's going on. Note
    //     // that this message will not be included in the this.messages array, and
    //     // that will continue to collect all messages; this only affects the
    //     // console emission of messages.
    //     event = InfrastructureMessages.Info_TooManyMessages({count: MaxMessagesDefault});
    //     event.filename = this.messageFilename;
    //   }

      this.printMessage(event);
    }

    private printMessage(event: CompilerEvent) {
      if(this.options.logFormat == 'tsv') {
        this.printTsvMessage(event);
      } else {
        this.printFormattedMessage(event);
      }
    }

    private printTsvMessage(event: CompilerEvent) {
        this.msg([
        CompilerError.formatFilename(event.filename || '<file>', {fullPath:true, forwardSlashes:false}),
        CompilerError.formatLine(event.line || -1),
        CompilerError.formatSeverity(event.code),
        CompilerError.formatCode(event.code),
        CompilerError.formatMessage(event.message)
      ].join('\t') + '\n');
    }

    private printFormattedMessage(event: CompilerEvent) {
    //   const severityColor = severityColors[CompilerError.severity(event.code)] ?? color.reset;
    //   const messageColor = this.messageSpecialColor(event) ?? color.reset;
      this.msg(
        (
          event.filename
          ? CompilerError.formatFilename(event.filename) +
            (event.line ? ':' + CompilerError.formatLine(event.line) : '') + ' - '
          : ''
        ) +
        CompilerError.formatSeverity(event.code) + ' ' +
        CompilerError.formatCode(event.code) + ': ' +
        CompilerError.formatMessage(event.message) + '\r\n'
    );

    //   if(event.code == InfrastructureMessages.INFO_ProjectBuiltSuccessfully) {
    //     // Special case: we'll add a blank line after project builds
    //     process.stdout.write('\n');
    //   }
    }

    debug(msg: string) {
      if(this.options.logLevel == 'debug') {
        console.debug(msg);
      }
    }

    fileExists(filename: string) {
      return fs.existsSync(filename);
    }


  }
