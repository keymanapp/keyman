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

/**
 * The wrapped functions
 */
class WrappedWasmFuncs {
  compileKeyboardFile?: (a0: string, a1: string, a2: number, a3: number, a4: number, a5: string) => boolean;
  parseUnicodeSet?: (a0: string, a1: number, a2: number) => number;
  setCompilerOptions?: (shouldAddCompilerVersion: number) => boolean;

  constructor(wasmModule: any) {
    this.compileKeyboardFile = wasmModule.cwrap('kmcmp_Wasm_CompileKeyboardFile', 'boolean', ['string', 'string', 'number', 'number', 'number', 'string']);
    this.parseUnicodeSet = wasmModule.cwrap('kmcmp_Wasm_ParseUnicodeSet', 'number', ['string', 'number', 'number']);
    this.setCompilerOptions = wasmModule.cwrap('kmcmp_Wasm_SetCompilerOptions', 'boolean', ['number']);
  }

  /**
   * @returns true if the functions are setup ok
   */
  get ok(): boolean {
    return this.parseUnicodeSet !== undefined
      && this.setCompilerOptions !== undefined
      && this.compileKeyboardFile !== undefined;
  }
};

export class Compiler {
  wasmModule: any;
  callbackName: string;
  callbacks: CompilerCallbacks;
  wasm: WrappedWasmFuncs;

  constructor() {
    this.callbackName = 'kmnCompilerCallback' + callbackProcIdentifier;
    callbackProcIdentifier++;
  }

  public async init(callbacks: CompilerCallbacks): Promise<boolean> {
    if(!this.callbacks) {
      this.callbacks = callbacks;
    }
    if(!this.wasmModule) {
      this.wasmModule = await loadWasmHost();
      this.wasm = new WrappedWasmFuncs(this.wasmModule);
    }
    return this.verifyInitted();
  }

  /**
   * Verify that wasm is spun up OK.
   * @returns true if OK
   */
  public verifyInitted() : boolean {
    if(!this.callbacks) {
      // Can't report a message here.
      throw Error('Must call Compiler.init(callbacks) before proceeding');
    }
    if(!this.wasmModule || !this.wasm.ok) { // fail if wasm not loaded or function not found
      this.callbacks.reportMessage(CompilerMessages.Fatal_MissingWasmModule());
      return false;
    }
    return true;
  }

  public run(infile: string, outfile: string, options?: CompilerOptions): boolean {
    if(!this.verifyInitted()) {
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
      if (!this.wasm.setCompilerOptions(options.shouldAddCompilerVersion ? 1 : 0)) {
        this.callbacks.reportMessage(CompilerMessages.Fatal_UnableToSetCompilerOptions());
      }
      return this.wasm.compileKeyboardFile(
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
   * @returns UnicodeSet accessor object, or null on failure
   */
  public parseUnicodeSet(pattern: string, bufferSize: number) : UnicodeSet | null {
    if(!this.verifyInitted()) {
      return null;
    }

    if (!bufferSize) {
      bufferSize = 100;
    }

    // Module seems to be the usual name for it
    const Module = this.wasmModule;
    const buf = Module.asm.malloc(bufferSize * 2 * Module.HEAPU32.BYTES_PER_ELEMENT);
    const rc = this.wasm.parseUnicodeSet(pattern, buf, bufferSize);
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
  constructor(public pattern: string, public ranges: number[][]) {
  }
  /**
   * Number of ranges
   */
  get length() : number {
    return this.ranges.length;
  }
}
