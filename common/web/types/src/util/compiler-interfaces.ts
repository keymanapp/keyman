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

  Severity_Mask = 0xF00000, // includes reserved bits
  Error_Mask =    0x0FFFFF,
};

export function compilerErrorSeverity(code: number): number {
  return code & CompilerErrorSeverity.Severity_Mask;
}

export function compilerErrorSeverityName(code: number): string {
  switch(compilerErrorSeverity(code)) {
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
 * Defines the error code ranges for various compilers. Once defined, these
 * ranges must not be changed as external modules may depend on specific error
 * codes. Individual errors are defined at a compiler level, for example,
 * kmn-keyboard/src/compiler/messages.ts.
 */
export enum CompilerErrorNamespace {
  /**
   * kmc-keyboard errors between 0x0000…0x0FFF
   */
  KeyboardCompiler = 0x0000,
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

  existsSync(name: string): boolean;
}

/**
 * Abstract interface for callbacks, to abstract out file i/o
 */
export interface CompilerCallbacks {
  /**
   * Attempt to load a file. Return falsy if not found.
   * TODO: accept only string
   * TODO: never return falsy, just throw if not found?
   * TODO: Buffer is Node-only.
   * TODO: rename to readFile, consolidate with fs.readFileSync?
   * @param baseFilename
   * @param filename
   */
  loadFile(filename: string | URL): Buffer;

  get path(): CompilerPathCallbacks;
  get fs(): CompilerFileSystemCallbacks;

  /**
   * Resolves a file path relative to the baseFilename
   * @param baseFilename
   * @param filename
   */
  resolveFilename(baseFilename: string, filename: string): string;

  loadSchema(schema: CompilerSchema): Buffer;
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
