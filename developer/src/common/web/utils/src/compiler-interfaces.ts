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
  Info =          0x000000, // Informational, not necessarily a problem
  Hint =          0x100000, // Something the user might want to be aware of
  Warn =          0x200000, // Warning: Not great, but we can keep going.
  Error =         0x300000, // Severe error where we can't continue
  Fatal =         0x400000, // OOM or should-not-happen internal problem
};

export const CompilerErrorSeverityValues = [
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
};

/**
 * A mapping for common path operations, maps to Node path module. This only
 * defines the functions we are actually using, so that we can port more easily
 * between different systems.
 */
export interface CompilerPathCallbacks {
  dirname(name: string): string;
  extname(name: string): string;
  basename(name: string, ext?: string): string;
  isAbsolute(name: string): boolean;
  join(...paths: string[]): string;
  normalize(p: string): string;
}

/**
 * A mapping for common filesystem operations, maps to Node fs module. This only
 * defines the functions we are actually using, so that we can port more easily
 * between different systems.
 */
export interface CompilerFileSystemCallbacks {
  readdirSync(name: string): string[];
  readFileSync(path: string, options?: { encoding?: null; flag?: string; } | null): Uint8Array;
  readFileSync(path: string, options: { encoding: string; flag?: string; } | string): string;
  readFileSync(path: string, options?: { encoding?: string | null; flag?: string; } | string | null): string | Uint8Array;
  writeFileSync(path: string, data: Uint8Array): void;

  existsSync(name: string): boolean;
}

type CompilerErrorSeverityOverride = CompilerErrorSeverity | 'disable';
export interface CompilerMessageOverrideMap {
  [code:number]: CompilerErrorSeverityOverride;
};

export interface CompilerMessageOverride {
  code: number;
  level: CompilerErrorSeverityOverride;
};

export interface CompilerCallbackOptions {
  logLevel?: CompilerLogLevel;
  logFormat?: CompilerLogFormat;
  color?: boolean; // null or undefined == use console default
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
 * Abstract interface for callbacks, to abstract out file i/o
 */
export interface CompilerCallbacks {
  /**
   * Attempt to load a file. Return falsy if not found.
   * TODO: never return falsy, just throw if not found?
   * @param filename
   */
  loadFile(filename: string): Uint8Array;

  /**
   * Get file size, returns undefined if not found
   */
  fileSize(filename: string): number;

  get path(): CompilerPathCallbacks;
  get fs(): CompilerFileSystemCallbacks;

  /**
   * Resolves a file path relative to the baseFilename
   * @param baseFilename
   * @param filename
   */
  resolveFilename(baseFilename: string, filename: string): string;

  reportMessage(event: CompilerEvent): void;

  debug(msg: string): void;
};

/**
 * Wrapper class for CompilerCallbacks for a given input file
 */
export class CompilerFileCallbacks implements CompilerCallbacks {
  messages: CompilerEvent[] = [];

  constructor(private filename: string, private options: CompilerCallbackOptions, private parent: CompilerCallbacks) {
  }

  /**
   * Returns `true` if any message in the `messages` array is a Fatal or Error
   * message, and if `compilerWarningsAsErrors` is `true`, then also returns
   * `true` if any message is a Warning.
   */
  static hasFailureMessage(messages: CompilerEvent[], compilerWarningsAsErrors: boolean) {
    const failureCodes = [
      CompilerErrorSeverity.Fatal, CompilerErrorSeverity.Error
    ].concat(compilerWarningsAsErrors ? [CompilerErrorSeverity.Warn] : []);
    return messages.find(m => failureCodes.includes(CompilerError.severity(m.code))) != undefined;
  }

  /**
   *
   * @param event
   * @param overrides
   * @returns true if event has been suppressed
   */
  static applyMessageOverridesToEvent(event: CompilerEvent, overrides: CompilerMessageOverrideMap) {
    // Override event severity from user preference -- this will not override
    // fatal or error events
    const severity = overrides?.[CompilerError.error(event.code)] ??
      CompilerError.severity(event.code);

    if(severity == 'disable') {
      return true;
    }

    // Override the default event severity with the command line option
    event.code = severity | (event.code & ~CompilerErrorMask.Severity);

    return false;
  }

  /**
   * Returns `true` if any message in the `messages` array is a Fatal or Error
   * message, and if `compilerWarningsAsErrors` is `true`, then also returns
   * `true` if any message is a Warning.
   *
   * If passed a defined `compilerWarningsAsErrors` value, then uses that,
   * otherwise uses `options.compilerWarningsAsErrors`, or `false` if that is
   * also `undefined`.
   */
  hasFailureMessage(compilerWarningsAsErrors?: boolean) {
    return CompilerFileCallbacks.hasFailureMessage(
      this.messages,
      compilerWarningsAsErrors ?? this.options.compilerWarningsAsErrors ?? false
    );
  }

  clear() {
    this.messages = [];
  }

  loadFile(filename: string): Uint8Array {
    return this.parent.loadFile(filename);
  }

  fileSize(filename: string): number {
    return this.parent.fileSize(filename);
  }

  get path(): CompilerPathCallbacks {
    return this.parent.path;
  }

  get fs(): CompilerFileSystemCallbacks {
    return this.parent.fs;
  }

  resolveFilename(baseFilename: string, filename: string): string {
    return this.parent.resolveFilename(baseFilename, filename);
  }

  reportMessage(event: CompilerEvent): void {
    const disable = CompilerFileCallbacks.applyMessageOverridesToEvent(event, this.options.messageOverrides);
    this.messages.push(event);
    if(!disable) {
      this.parent.reportMessage({filename: this.filename, ...event});
    }
  }

  debug(msg: string): void {
    return this.parent.debug(msg);
  }
}

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
  'info',       /// All messages: errors + warnings + hints + info
  'debug'       /// All messages: errors + warnings + hints + info, plus debug logs
] as const;

type CompilerLogLevelTuple = typeof ALL_COMPILER_LOG_LEVELS;
export type CompilerLogLevel = CompilerLogLevelTuple[number];

export const compilerLogLevelToSeverity: {[index in CompilerLogLevel]: number} = {
  'silent': CompilerErrorMask.Severity,  // effectively excludes all reporting
  'error': CompilerErrorSeverity.Error,
  'warn': CompilerErrorSeverity.Warn,
  'hint': CompilerErrorSeverity.Hint,
  'info': CompilerErrorSeverity.Info,
  'debug': CompilerErrorSeverity.Info
};

export const ALL_COMPILER_LOG_FORMATS = [
  'tsv',
  'formatted'
] as const;

type CompilerLogFormatTuple = typeof ALL_COMPILER_LOG_FORMATS;
export type CompilerLogFormat = CompilerLogFormatTuple[number];
