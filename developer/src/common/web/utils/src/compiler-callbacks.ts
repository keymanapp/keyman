/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Callbacks for Keyman Developer compilers to ensure independence from
 * filesystem, network, and console.
 */

import { CompilerEvent, CompilerCallbackOptions, CompilerErrorSeverity, CompilerError, CompilerMessageOverrideMap, CompilerErrorMask } from "./compiler-interfaces.js";

/**
 * Abstract interface for callbacks, to abstract out file i/o
 */

export interface CompilerCallbacks {
  /**
   * Attempt to load a file. Return falsy if not found.
   * @param filename
   */
  loadFile(filename: string): Uint8Array;

  /**
   * Get file size, returns undefined if not found
   */
  fileSize(filename: string): number;

  /**
   * Returns true if file is a directory, undefined if not found
   */
  isDirectory(filename: string): boolean;

  get path(): CompilerPathCallbacks;
  get fs(): CompilerFileSystemCallbacks
  get fsAsync(): CompilerFileSystemAsyncCallbacks;
  get net(): CompilerNetAsyncCallbacks;

  /**
   * Resolves a file path relative to the baseFilename
   * @param baseFilename
   * @param filename
   */
  resolveFilename(baseFilename: string, filename: string): string;

  reportMessage(event: CompilerEvent): void;

  debug(msg: string): void;

  fileURLToPath(url: string | URL): string;
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
  relative(from: string, to: string): string;
  resolve(...args: string[]): string;
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
  mkdirSync(path: string, options?: { recursive?: boolean; }): string;
  existsSync(name: string): boolean;
}

export interface CompilerAsyncCallbacks {
  // TODO: readonly log: CompilerLogCallbacks;
  readonly fsAsync: CompilerFileSystemAsyncCallbacks;
  readonly path: CompilerPathCallbacks;
  readonly net: CompilerNetAsyncCallbacks;
}

export interface CompilerFileSystemCallbacksFolderEntry {
  filename: string;
  type: 'file' | 'dir';
}

export interface CompilerNetAsyncCallbacks {
  fetchJSON(url: string): Promise<any>;
  fetchBlob(url: string): Promise<Uint8Array>;
}

export interface CompilerFileSystemAsyncCallbacks {
  readFile(filename: string): Promise<Uint8Array>;
  //fileSize(filename: string): Promise<number>;
  //isDirectory(filename: string): Promise<boolean>;
  readdir(filename: string): Promise<CompilerFileSystemCallbacksFolderEntry[]>;
  // writeFile(filename: string, data: Uint8Array): Promise<void>;
  // mkdir(path: string, options?: {recursive?: boolean}): Promise<string>;
  exists(filename: string): Promise<boolean>;
  resolveFilename(baseFilename: string, filename: string): string;
}


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

    if (severity == 'disable') {
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

  isDirectory(filename: string): boolean {
    return this.parent.isDirectory(filename);
  }

  get path(): CompilerPathCallbacks {
    return this.parent.path;
  }

  get fs(): CompilerFileSystemCallbacks {
    return this.parent.fs;
  }

  get net(): CompilerNetAsyncCallbacks {
    return this.parent.net;
  }

  get fsAsync(): CompilerFileSystemAsyncCallbacks {
    return this.parent.fsAsync;
  }

  resolveFilename(baseFilename: string, filename: string): string {
    return this.parent.resolveFilename(baseFilename, filename);
  }

  reportMessage(event: CompilerEvent): void {
    const disable = CompilerFileCallbacks.applyMessageOverridesToEvent(event, this.options.messageOverrides);
    this.messages.push(event);
    if (!disable) {
      this.parent.reportMessage({ filename: this.filename, ...event });
    }
  }

  debug(msg: string): void {
    return this.parent.debug(msg);
  }

  fileURLToPath(url: string | URL): string {
    return this.parent.fileURLToPath(url);
  }
}

export class DefaultCompilerFileSystemAsyncCallbacks implements CompilerFileSystemAsyncCallbacks {
  constructor(private owner: CompilerCallbacks) {

  }
  async exists(filename: string): Promise<boolean> {
    return this.owner.fs.existsSync(filename);
  }

  async readFile(filename: string): Promise<Uint8Array> {
    return this.owner.loadFile(filename);
  }

  async readdir(filename: string): Promise<CompilerFileSystemCallbacksFolderEntry[]> {
    return this.owner.fs.readdirSync(filename).map(item => ({
      filename: item,
      type: this.owner.isDirectory(this.owner.path.join(filename, item)) ? 'dir' : 'file'
    }));
  }

  resolveFilename(baseFilename: string, filename: string): string {
    return this.owner.resolveFilename(baseFilename, filename);
  }
}
