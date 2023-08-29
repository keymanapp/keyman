import * as fs from 'fs';
import * as path from 'path';
import { CompilerCallbacks, CompilerEvent,
         CompilerPathCallbacks, CompilerFileSystemCallbacks,
         compilerLogLevelToSeverity, CompilerErrorSeverity,
         CompilerError,
         CompilerCallbackOptions,
         CompilerFileCallbacks} from '@keymanapp/common-types';
import { InfrastructureMessages } from '../messages/infrastructureMessages.js';
import chalk from 'chalk';
import supportsColor from 'supports-color';
import { KeymanSentry } from './KeymanSentry.js';

const color = chalk.default;
const severityColors: {[value in CompilerErrorSeverity]: chalk.Chalk} = {
  [CompilerErrorSeverity.Info]: color.reset,
  [CompilerErrorSeverity.Hint]: color.blueBright,
  [CompilerErrorSeverity.Warn]: color.hex('FFA500'), // orange
  [CompilerErrorSeverity.Error]: color.redBright,
  [CompilerErrorSeverity.Fatal]: color.redBright,
};

/**
 * Maximum messages that will be emitted before suppressing further messages.
 * We may in the future make this user configurable?
 */
const MaxMessagesDefault = 100;

/**
 * Concrete implementation for CLI use
 */
export class NodeCompilerCallbacks implements CompilerCallbacks {
  /* NodeCompilerCallbacks */

  messages: CompilerEvent[] = [];
  messageCount = 0;
  messageFilename: string = '';

  constructor(private options: CompilerCallbackOptions) {
    color.enabled = this.options.color ?? (supportsColor.stdout ? supportsColor.stdout.hasBasic : false);
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
      compilerWarningsAsErrors ?? this.options.compilerWarningsAsErrors
    );
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
          reference: path.basename(originalFilename),
          filename: path.basename(nativeFilename)
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

  fileSize(filename: string): number {
    return fs.statSync(filename)?.size;
  }

  get path(): CompilerPathCallbacks {
    return path;
  }

  get fs(): CompilerFileSystemCallbacks {
    return fs;
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

    // report fatal errors to Sentry, but don't abort; note, it won't be
    // reported if user has disabled the Sentry setting
    if(CompilerError.severity(event.code) == CompilerErrorSeverity.Fatal) {
      // this is async so returns a Promise, we'll let it resolve in its own
      // time, and it will emit a message to stderr with details at that time
      KeymanSentry.reportException(event.exceptionVar, false);
    }

    if(CompilerError.severity(event.code) < compilerLogLevelToSeverity[this.options.logLevel]) {
      // collect messages but don't print to console
      return;
    }

    // We don't use this.messages.length because we only want to count visible
    // messages, and there's no point in recalculating the total for every
    // message emitted.

    this.messageCount++;
    if(this.messageCount > MaxMessagesDefault) {
      return;
    }

    if(this.messageCount == MaxMessagesDefault) {
      // We've hit our event limit so we'll suppress further messages, and emit
      // our little informational message so users know what's going on. Note
      // that this message will not be included in the this.messages array, and
      // that will continue to collect all messages; this only affects the
      // console emission of messages.
      event = InfrastructureMessages.Info_TooManyMessages({count: MaxMessagesDefault});
      event.filename = this.messageFilename;
    }

    const severityColor = severityColors[CompilerError.severity(event.code)] ?? color.reset;
    const messageColor = this.messageSpecialColor(event) ?? color.reset;
    process.stdout.write(
      (
        event.filename
        ? color.cyan(CompilerError.formatFilename(event.filename)) +
          (event.line ? ':' + color.yellowBright(CompilerError.formatLine(event.line)) : '') + ' - '
        : ''
      ) +
      severityColor(CompilerError.formatSeverity(event.code)) + ' ' +
      color.grey(CompilerError.formatCode(event.code)) + ': ' +
      messageColor(CompilerError.formatMessage(event.message)) + '\n'
    );

    if(event.code == InfrastructureMessages.INFO_ProjectBuiltSuccessfully) {
      // Special case: we'll add a blank line after project builds
      process.stdout.write('\n');
    }

  }

  /**
   * We treat a few certain infrastructure messages with special colours
   * @param event
   * @returns
   */
  messageSpecialColor(event: CompilerEvent) {
    switch(event.code) {
      case InfrastructureMessages.INFO_BuildingFile:
        return color.whiteBright;
      case InfrastructureMessages.INFO_FileNotBuiltSuccessfully:
      case InfrastructureMessages.INFO_ProjectNotBuiltSuccessfully:
        return color.red;
      case InfrastructureMessages.INFO_FileBuiltSuccessfully:
      case InfrastructureMessages.INFO_ProjectBuiltSuccessfully:
        return color.green;
    }
    return null;
  }

  debug(msg: string) {
    if(this.options.logLevel == 'debug') {
      console.debug(msg);
    }
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
