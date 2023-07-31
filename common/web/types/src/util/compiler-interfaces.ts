/**
 * Abstract interface for compiler error and warning messages
 */
export interface CompilerEvent {
  filename?: string;
  line?: number;
  code: number;
  message: string;
};

export enum CompilerErrorSeverity {
  Info =          0x000000, // Informational, not necessarily a problem
  Hint =          0x100000, // Something the user might want to be aware of
  Warn =          0x200000, // Warning: Not great, but we can keep going.
  Error =         0x300000, // Severe error where we can't continue
  Fatal =         0x400000, // OOM or should-not-happen internal problem

  // Mask values for mapping compiler errors
  // TODO: make this a separate enum?
  Severity_Mask =  0x00F00000,  // includes reserved bits, 16 possible severity levels
  Error_Mask =     0x000FFFFF,  // error | namespace
  Namespace_Mask = 0x000FF000,  // 256 possible namespaces
  BaseError_Mask = 0x00000FFF,  // error code, 2,048 possible error codes per namespace
  Reserved_Mask  = 0xFF000000,  // do not use these error values at this time
};

export function compilerErrorSeverityName(code: number): string {
  let severity = code & CompilerErrorSeverity.Severity_Mask;
  switch(severity) {
    case CompilerErrorSeverity.Info: return 'INFO';
    case CompilerErrorSeverity.Hint: return 'HINT';
    case CompilerErrorSeverity.Warn: return 'WARN';
    case CompilerErrorSeverity.Error: return 'ERROR';
    case CompilerErrorSeverity.Fatal: return 'FATAL';
    /* istanbul ignore next */
    default: return 'UNKNOWN';
  }
}

/**
 * Format the error code number
 * example: "FATAL:0x03004"
 */
export function compilerErrorFormatCode(code: number): string {
  const severity = code & CompilerErrorSeverity.Severity_Mask;
  const severityName = compilerErrorSeverityName(severity);
  const errorCode = code & CompilerErrorSeverity.Error_Mask;
  const errorCodeString = Number(errorCode).toString(16).padStart(5,'0');
  return `${severityName}:0x${errorCodeString}`;
}

/**
 * @param e event or array of events
 * @returns string
 */
export function compilerEventFormat(e : CompilerEvent | CompilerEvent[]) : string {
  if (!e) {
    return "";
  }
  if (Array.isArray(e)) {
    return e.map(item => compilerEventFormat(item)).join('\n');
  }
  const {code, message} = e;
  return `${compilerErrorFormatCode(code)}: “${message}”`;
}

/**
 * Defines the error code ranges for various compilers. Once defined, these
 * ranges must not be changed as external modules may depend on specific error
 * codes. Individual errors are defined at a compiler level, for example,
 * kmc-ldml/src/compiler/messages.ts.
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
   * kmc and related infrastructure errors between 0x5000…0x5FFF;
   */
  Infrastructure = 0x5000,
};

export type CompilerSchema =
  'ldml-keyboard' |
  'ldml-keyboardtest' |
  'kvks' |
  'kpj';
  // | 'keyman-touch-layout.clean'; TODO this has the wrong name pattern, .spec.json instead of .schema.json

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

/**
 * Abstract interface for callbacks, to abstract out file i/o
 */
export interface CompilerCallbacks {
  /**
   * Attempt to load a file. Return falsy if not found.
   * TODO: never return falsy, just throw if not found?
   * @param baseFilename
   * @param filename
   */
  loadFile(filename: string): Uint8Array;

  get path(): CompilerPathCallbacks;
  get fs(): CompilerFileSystemCallbacks;

  /**
   * Resolves a file path relative to the baseFilename
   * @param baseFilename
   * @param filename
   */
  resolveFilename(baseFilename: string, filename: string): string;

  loadSchema(schema: CompilerSchema): Uint8Array;
  reportMessage(event: CompilerEvent): void;
  debug(msg: string): void;
};

/**
 * Convenience function for constructing CompilerEvents
 * @param code
 * @param message
 * @returns
 */
export const CompilerMessageSpec = (code: number, message: string) : CompilerEvent => { return { code, message } };

/**
 * @param e Error-like
 */
export function compilerExceptionToString(e?: any) : string {
  return `${(e ?? 'unknown error').toString()}\n\nCall stack:\n${(e instanceof Error ? e.stack : (new Error()).stack)}`;
}
