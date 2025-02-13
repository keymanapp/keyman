import { CompilerCallbacks } from "./compiler-callbacks.js";

/**
 * Abstract interface for compiler error and warning messages
 */
export interface CompilerEvent {
  filename?: string;
  line?: number;
  code: number;
  message: string;
  /**
   * detailed Markdown-formatted description of the error including
   * references to documentation, remediation options.
   */
  detail?: string;
  /**
   * an internal error occurred that should be captured with a stack trace
   * e.g. to the Keyman sentry instance by kmc
   */
  exceptionVar?: any;
};

export enum CompilerErrorSeverity {
  Debug =         0x000000, // log everything including internal debug
  Verbose =       0x100000, // log everything, except debug
  Info =          0x200000, // Informational, not necessarily a problem
  Hint =          0x300000, // Something the user might want to be aware of
  Warn =          0x400000, // Warning: Not great, but we can keep going.
  Error =         0x500000, // Severe error where we can't continue
  Fatal =         0x600000, // OOM or should-not-happen internal problem
};

export const CompilerErrorSeverityValues = [
  CompilerErrorSeverity.Debug,
  CompilerErrorSeverity.Verbose,
  CompilerErrorSeverity.Info,
  CompilerErrorSeverity.Hint,
  CompilerErrorSeverity.Warn,
  CompilerErrorSeverity.Error,
  CompilerErrorSeverity.Fatal,
]

/**
 * Mask values for mapping compiler errors
 */
export enum CompilerErrorMask {
  Severity =  0x00F00000,  // includes reserved bits, 16 possible severity levels
  Error =     0x000FFFFF,  // error | namespace
  Namespace = 0x000FF000,  // 256 possible namespaces
  BaseError = 0x00000FFF,  // error code, 2,048 possible error codes per namespace
  Reserved  = 0xFF000000,  // do not use these error values at this time
};

const errorSeverityName = {
  [CompilerErrorSeverity.Debug]: 'debug',
  [CompilerErrorSeverity.Verbose]: 'verbose',
  [CompilerErrorSeverity.Info]: 'info',
  [CompilerErrorSeverity.Hint]: 'hint',
  [CompilerErrorSeverity.Warn]: 'warn',
  [CompilerErrorSeverity.Error]: 'error',
  [CompilerErrorSeverity.Fatal]: 'fatal',
};

export class CompilerError {
  static severity(code: number): CompilerErrorSeverity {
    return code & CompilerErrorMask.Severity;
  }
  static error(code: number): number {
    return code & CompilerErrorMask.Error;
  }
  static baseError(code: number): number {
    return code & CompilerErrorMask.BaseError;
  }
  static namespace(code: number): CompilerErrorNamespace {
    return code & CompilerErrorMask.Namespace;
  }
  static formatSeverity(code: number): string {
    return errorSeverityName[CompilerError.severity(code)] ?? 'UNKNOWN';
  }
  /** true if events has at least one message of the atLeast severity */
  static hasSeverity(events: CompilerEvent[], atLeast: CompilerErrorSeverity): boolean {
    for (const { code } of events) {
      if (CompilerError.severity(code) >= atLeast) {
        return true;
      }
    }
    return false;
  }
  /** true if events has at least one Error or worse */
  static hasError(events: CompilerEvent[]): boolean {
    return CompilerError.hasSeverity(events, CompilerErrorSeverity.Error);
  }
  /**
   * Format an error code number. The error code number does not include
   * the severity mask, as this is reported in text form separately; see
   * `severityName`.
   * @example
   *
   * The following call returns `KM03004`
   * ```
   *   formatCode(CompilerMessage.ERROR_InvalidDisplayMapFile)
   * ```
   */
  static formatCode(code: number): string {
    return Number.isInteger(code) ? 'KM' + CompilerError.error(code).toString(16).toUpperCase().padStart(5, '0') : 'KM?????';
  }

  /**
   * Formats an event filename for an error report,
   * stripping off path component
   * @param filename
   * @returns
   */
  static formatFilename(filename: string, options?: {
    fullPath?: boolean,
    forwardSlashes?: boolean
  }): string {
    if(!filename) {
      return '';
    }

    if(options?.fullPath) {
      return options?.forwardSlashes ?
        filename.replaceAll(/\\/g, '/') :
        filename.replaceAll(/\//g, '\\');
    }

    let x = filename.lastIndexOf('/');
    if(x < 0) {
      x = filename.lastIndexOf('\\');
    }
    return x >= 0 ? filename.substring(x+1) : filename;
  }

  /**
   * Formats an event line for an error report
   * @param line
   * @returns
   */
  static formatLine(line: number): string {
    return line ? line.toString() : '';
  }

  /**
   * Formats an event message for an error report
   * @param message
   * @returns
   */
  static formatMessage(message: string): string {
    return message ?? '';
  }

  /**
   * Formats a compiler message, without coloring; an ANSI color version is
   * implemented in NodeCompilerCallbacks.
   * @param event event or array of events
   */
  static formatEvent(event : CompilerEvent | CompilerEvent[]): string {
    if (!event) {
      return "";
    }
    if (Array.isArray(event)) {
      return event.map(item => CompilerError.formatEvent(item)).join('\n') + '\n';
    }

    return (
      event.filename
      ? CompilerError.formatFilename(event.filename) +
        (event.line ? ':' + CompilerError.formatLine(event.line) : '') + ' - '
      : ''
    ) +
    CompilerError.formatSeverity(event.code) + ' ' +
    CompilerError.formatCode(event.code) + ': ' +
    CompilerError.formatMessage(event.message);
  }

  /**
   * @param e Error-like
   */
  static exceptionToString(e?: any) : string {
    return `${(e ?? 'unknown error').toString()}\n\nCall stack:\n${(e instanceof Error ? e.stack : (new Error()).stack)}`;
  }

  /**
   * Returns the corresponding error severity value from a partial name match,
   * e.g. 'inf' returns CompilerErrorSeverity.Info, or returns null if not found
   * @param name
   * @returns
   */
  static severityNameToValue(name: string): CompilerErrorSeverity {
    name = name.toLowerCase();
    for(const level of CompilerErrorSeverityValues) {
      if(errorSeverityName[level].startsWith(name)) {
        return level;
      }
    }
    return null;
  }
};

/** @deprecated use `CompilerError.severity` instead */
export function compilerErrorSeverity(code: number): CompilerErrorSeverity {
  return CompilerError.severity(code);
}

/** @deprecated use `CompilerError.formatSeverity` instead */
export function compilerErrorSeverityName(code: number): string {
  return CompilerError.formatSeverity(code);
}

/** @deprecated use `CompilerError.formatCode` instead */
export function compilerErrorFormatCode(code: number): string {
  return CompilerError.formatCode(code);
}

/** @deprecated use `CompilerError.formatEvent` instead */
export function compilerEventFormat(e : CompilerEvent | CompilerEvent[]) : string {
  return CompilerError.formatEvent(e);
}

/**
 * Defines the error code ranges for various compilers. Once defined, these
 * ranges must not be changed as external modules may depend on specific error
 * codes. Individual errors are defined at a compiler level, for example,
 * kmc-ldml/src/compiler/messages.ts.
 *
 * kmc defines a mapping between each namespace and the corresponding compiler's
 * error reporting class in kmc/src/messages/messageNamespaces.ts
 */
export enum CompilerErrorNamespace {
  /**
   * kmc-ldml errors between 0x0000…0x0FFF
   */
  LdmlKeyboardCompiler = 0x0000,
  /**
   * common/web/types errors between 0x1000…0x1FFF
   */
  CommonTypes = 0x1000,
  /**
   * kmc-kmn errors between 0x2000…0x2FFF; these map to
   * the base codes found in kmn_compiler_errors.h, exclusive severity flags
   */
  KmnCompiler = 0x2000,
  /**
   * kmc-model errors between 0x3000…0x3FFF
   */
  ModelCompiler = 0x3000,
  /**
   * kmc-package errors between 0x4000…0x4FFF
   */
  PackageCompiler = 0x4000,
  /**
   * kmc and related infrastructure errors between 0x5000…0x5FFF
   */
  Infrastructure = 0x5000,
  /**
   * kmc-analyze 0x6000…0x6FFF
   */
  Analyzer = 0x6000,
  /**
   * kmc-kmn/kmw-compiler errors between 0x7000…0x7FFF; note that some errors
   * generated by kmc-kmn/kmw-compiler are from kmc-kmn namespace for legacy
   * reasons
   */
  KmwCompiler = 0x7000,
  /**
   * kmc-model-info 0x8000…0x8FFF
   */
  ModelInfoCompiler = 0x8000,
  /**
   * kmc-keyboard-info 0x9000…0x9FFF
   */
  KeyboardInfoCompiler = 0x9000,
  /**
   * kmc-generate 0xA000…0xAFFF
   */
  Generator = 0xA000,
  /**
   * kmc-copy 0xB000…0xBFFF
   */
  Copier = 0xB000,
};

type CompilerErrorSeverityOverride = CompilerErrorSeverity | 'disable';
export interface CompilerMessageOverrideMap {
  [code:number]: CompilerErrorSeverityOverride;
};

export interface CompilerMessageOverride {
  code: number;
  level: CompilerErrorSeverityOverride;
};

export interface CompilerCallbackOptions extends CompilerBaseOptions {
  // TODO: these overlap with CompilerOptions, should refactor
  compilerWarningsAsErrors?: boolean;
  messageOverrides?: CompilerMessageOverrideMap;
};

export interface KeymanCompilerArtifact {
  data: Uint8Array;
  filename: string;
};

export type KeymanCompilerArtifactOptional = KeymanCompilerArtifact | undefined;

export interface KeymanCompilerArtifacts {
  readonly [type:string]: KeymanCompilerArtifactOptional;
};

export interface KeymanCompilerResult {
  artifacts: KeymanCompilerArtifacts;
};

export interface KeymanCompiler {
  init(callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean>;
  /**
   * Run the compiler, and save the result in memory arrays. Note that while
   * `outputFilename` is provided here, the output file is not written to in
   * this function.
   * @param inputFilename
   * @param outputFilename The intended output filename, optional, if missing,
   *                       calculated from inputFilename
   * @param data
   */
  run(inputFilename:string, outputFilename?:string /*, data?: any*/): Promise<KeymanCompilerResult>;
  /**
   * Writes the compiled output files to disk
   * @param artifacts
   */
  write(artifacts: KeymanCompilerArtifacts): Promise<boolean>;
};

/**
 * Abstract interface for compiler options
 */

export interface CompilerBaseOptions {
  /**
   * Reporting level to console, used by NodeCompilerCallbacks (not used in compiler modules;
   * all messages are still reported to the internal log)
   */
  logLevel?: CompilerLogLevel;
  /**
   * Format of output for log to console
   */
  logFormat?: CompilerLogFormat;
  /**
   * Colorize log output, default is detected from console
   */
  color?: boolean;
}

export interface CompilerOptions extends CompilerBaseOptions {
  /**
   * Add metadata about the compiler version to .kmx file when compiling
   */
  shouldAddCompilerVersion?: boolean;
  /**
   * Add debug information to the .kmx file when compiling
   */
  saveDebug?: boolean;
  /**
   * Upgrade any warnings produced in the compile to errors
   */
  compilerWarningsAsErrors?: boolean;
  /**
   * Emit warnings if deprecated code is encountered
   */
	warnDeprecatedCode?: boolean;
  /**
   * Check filename conventions in packages
   */
  checkFilenameConventions?: boolean;
};

export const defaultCompilerOptions: CompilerOptions = {
  logLevel: 'info',
  logFormat: 'formatted',
  // outFile: (undefined)
  saveDebug: false,
  shouldAddCompilerVersion: true,
  compilerWarningsAsErrors: false,
  warnDeprecatedCode: true,
  checkFilenameConventions: false,
}

/**
 * Convenience function for constructing CompilerEvents
 * @param code     Unique numeric value of the event
 * @param message  A short description of the error presented to the user
 * @param detail   Detailed Markdown-formatted description of the error
 *                 including references to documentation, remediation options.
 * @returns
 */
export const CompilerMessageSpec = (code: number, message: string, detail?: string) : CompilerEvent => ({
  code,
  message,
  detail,
});

/**
 * Remove initial whitespace from compiler detail messages, to enable
 * indented formatting of message detail strings inside the message
 * definitions
 * @param event
 * @returns dedented event detail
 */
export function dedentCompilerMessageDetail(event: CompilerEvent) {
  // TODO(lowpri): dedent may be too naive -- should use least
  // non-zero whitespace line as amount to dedent
  return (event.detail ?? '').replace(/^[ ]+/gm, '');
}

export const CompilerMessageDef = (param: any) => String(param ?? `<param>`);

export const CompilerMessageSpecWithException = (code: number, message: string, exceptionVar: any, detail?: string) : CompilerEvent => ({
  code,
  message: exceptionVar
    ? (message ?? `Unexpected exception`) + `: ${exceptionVar.toString()}\n\nCall stack:\n${(exceptionVar instanceof Error ? exceptionVar.stack : (new Error()).stack)}` :
    message,
  detail,
  exceptionVar,
});

/**
 * Compiler logging level and correspondence to severity
 */

export const ALL_COMPILER_LOG_LEVELS = [
  'silent',     /// Nothing is emitted to stdout, not even errors (fatal exceptions may still emit to stdout)
  'error',      /// Only errors emitted
  'warn',       /// Errors + warnings
  'hint',       /// Errors + warnings + hints
  'info',       /// All normal messages: errors + warnings + hints + info
  'verbose',    /// All messages + verbose logging
  'debug',      /// All messages + verbose + internal debug
] as const;

type CompilerLogLevelTuple = typeof ALL_COMPILER_LOG_LEVELS;
export type CompilerLogLevel = CompilerLogLevelTuple[number];

export const compilerLogLevelToSeverity: {[index in CompilerLogLevel]: number} = {
  'silent': CompilerErrorMask.Severity,  // effectively excludes all reporting
  'error': CompilerErrorSeverity.Error,
  'warn': CompilerErrorSeverity.Warn,
  'hint': CompilerErrorSeverity.Hint,
  'info': CompilerErrorSeverity.Info,
  'verbose': CompilerErrorSeverity.Verbose,
  'debug': CompilerErrorSeverity.Debug,
};

export const ALL_COMPILER_LOG_FORMATS = [
  'tsv',
  'formatted'
] as const;

type CompilerLogFormatTuple = typeof ALL_COMPILER_LOG_FORMATS;
export type CompilerLogFormat = CompilerLogFormatTuple[number];
