/*
TODO: implement additional interfaces:

  extern "C" bool kmcmp_CompileKeyboardFileToBuffer(
    char* pszInfile,
    void* pfkBuffer,
    bool ACompilerWarningsAsErrors,
    bool AWarnDeprecatedCode,
    kmcmp_CompilerMessageProc pMsgproc,
    void* AmsgprocContext,
    int Target
  );

  typedef bool (*kmcmp_ValidateJsonMessageProc)(int64_t offset, const char* szText, void* context);

  extern "C" bool kmcmp_ValidateJsonFile(
    std::fstream& f,
    std::fstream& fd,
    kmcmp_ValidateJsonMessageProc MessageProc,
    void* context
  );
*/

// TODO: rename wasm-host?
import { CompilerCallbacks } from '@keymanapp/common-types';
import loadWasmHost from '../import/kmcmplib/wasm-host.js';
import { CompilerMessages, mapErrorFromKmcmplib } from './messages.js';

export interface CompilerOptions {
  shouldAddCompilerVersion?: boolean;
  saveDebug?: boolean;
  compilerWarningsAsErrors?: boolean;
	warnDeprecatedCode?: boolean;
};

const baseOptions: CompilerOptions = {
  shouldAddCompilerVersion: true,
  saveDebug: true,
  compilerWarningsAsErrors: false,
  warnDeprecatedCode: true
};

/**
 * Allows multiple instances of the Compiler class, by ensuring that the
 * 'unique' kmnCompilerCallback global will be correlated with a specific
 * instance of the Compiler class
 */
let callbackProcIdentifier = 0;

export class Compiler {
  wasmModule: any;
  compileKeyboardFile: any;
  setCompilerOptions: any;
  callbackName: string;
  callbacks: CompilerCallbacks;
  _parseUnicodeSet: any;

  constructor() {
    this.callbackName = 'kmnCompilerCallback' + callbackProcIdentifier;
    callbackProcIdentifier++;
  }

  public async init(): Promise<boolean> {
    if(!this.wasmModule) {
      this.wasmModule = await loadWasmHost();
      this.compileKeyboardFile = this.wasmModule.cwrap('kmcmp_Wasm_CompileKeyboardFile', 'boolean', ['string', 'string',
        'number', 'number', 'number', 'string']);
      this.setCompilerOptions = this.wasmModule.cwrap('kmcmp_Wasm_SetCompilerOptions', 'boolean', ['number']);
      this._parseUnicodeSet = this.wasmModule.cwrap('kmcmp_Wasm_ParseUnicodeSet', 'number', [ 'string', 'number', 'number']);
    }
    return this.compileKeyboardFile !== undefined
        && this.setCompilerOptions  !== undefined
        && this._parseUnicodeSet    !== undefined;
  }

  public run(infile: string, outfile: string, callbacks: CompilerCallbacks, options?: CompilerOptions): boolean {
    this.callbacks = callbacks;

    if(!this.wasmModule) {
      this.callbacks.reportMessage(CompilerMessages.Fatal_MissingWasmModule());
      return false;
    }

    options = {...baseOptions, ...options};
    (globalThis as any)[this.callbackName] = this.compilerMessageCallback;
    // TODO: use callbacks for file access -- so kmc-kmn is entirely fs agnostic
    let result = this.runCompiler(infile, outfile, options);
    delete (globalThis as any)[this.callbackName];
    return result;
  }

  private compilerMessageCallback = (line: number, code: number, msg: string): number => {
    this.callbacks.reportMessage(mapErrorFromKmcmplib(line, code, msg));
    return 1;
  }

  private runCompiler(infile: string, outfile: string, options: CompilerOptions): boolean {
    try {
      if(!this.setCompilerOptions(options.shouldAddCompilerVersion)) {
        this.callbacks.reportMessage(CompilerMessages.Fatal_UnableToSetCompilerOptions());
      }
      return this.compileKeyboardFile(
        infile,
        outfile,
        options.saveDebug ? 1 : 0,
        options.compilerWarningsAsErrors ? 1 : 0,
        options.warnDeprecatedCode ? 1 : 0,
        this.callbackName);
    } catch(e) {
      this.callbacks.reportMessage(CompilerMessages.Fatal_UnexpectedException({e:e}));
      return false;
    }
  }

  /**
   *
   * @param pattern UnicodeSet pattern such as `[a-z]`
   * @param bufferSize guess as to the buffer size
   * @returns UnicodeSet accessor object
   */
  public async parseUnicodeSet(pattern: string, bufferSize: number) : Promise<UnicodeSet> {
    if (!bufferSize) {
      bufferSize = 100;
    }

    const initOk = await this.init();
    if (!initOk) {
      throw Error(`WASM machinery didn't start up properly`);
    }
    // Module seems to be the usual name for it
    const Module = this.wasmModule;
    const buf = Module.asm.malloc(bufferSize * 2 * Module.HEAPU32.BYTES_PER_ELEMENT);
    const rc = this._parseUnicodeSet(pattern, buf, bufferSize);
    if (rc < 0) {
      throw new UnicodeSetError(rc);
    } else { // rc ≥0
      const ranges = [];
      const startu = (buf / Module.HEAPU32.BYTES_PER_ELEMENT);
      for (let i = 0; i < rc; i++) {
        const low = Module.HEAPU32[startu + (i * 2) + 0];
        const high = Module.HEAPU32[startu + (i * 2) + 1];
        ranges.push([low, high]);
      }
      return new UnicodeSet(pattern, ranges);
    }
  }
}

export class UnicodeSetError extends Error {
  code: number;
  constructor(code:number) {
    super();
    this.code = code;
    this.message = `UnicodeSet error: ${code}`;
  }
}

// from kmcmplib.h
export const KMCMP_USET_OK = 0;
export const KMCMP_ERROR_SYNTAX_ERR = -1;
export const KMCMP_ERROR_HAS_STRINGS = -2;
export const KMCMP_ERROR_UNSUPPORTED_PROPERTY = -3;
export const KMCMP_ERROR_UNSUPPORTED = -4;
export const KMCMP_FATAL_OUT_OF_RANGE = -5;

/**
 * Represents a parsed UnicodeSet
 */
export class UnicodeSet {
  pattern: string;
  ranges: number[][];
  constructor(pattern: string, ranges: number[][]) {
    this.pattern = pattern;
    this.ranges = ranges;
  }
  /**
   * Number of ranges
   */
  get length() : number {
    return this.ranges.length;
  }
}
