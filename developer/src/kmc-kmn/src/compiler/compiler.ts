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
import { CompilerCallbacks, CompilerEvent } from '@keymanapp/common-types';
import loadWasmHost from '../import/kmcmplib/wasm-host.js';
import { CompilerMessages, mapErrorFromKmcmplib } from './messages.js';

export interface CompilerOptions {
  shouldAddCompilerVersion?: boolean;
  saveDebug?: boolean;
  compilerWarningsAsErrors?: boolean;
	warnDeprecatedCode?: boolean;
  target?: 'kmx' | 'js';
};

const baseOptions: CompilerOptions = {
  shouldAddCompilerVersion: true,
  saveDebug: true,
  compilerWarningsAsErrors: false,
  warnDeprecatedCode: true,
  target: 'kmx'
};

/**
 * Allows multiple instances of the Compiler class, by ensuring that the
 * 'unique' kmnCompilerCallback global will be correlated with a specific
 * instance of the Compiler class
 */
let callbackProcIdentifier = 0;

/**
 * Pointer in wasm-space
 */
type WasmPtr = number;

/**
 * The wrapped functions
 */
class WasmWrapper {
  Module: any;

  compileKeyboardFile?: (pszInfile: string, pszOutfile: string, aSaveDebug: number, aCompilerWarningsAsErrors: number, aWarnDeprecatedCode: number, msgProc: string) => boolean;
  parseUnicodeSet?: (pat: string, buf: WasmPtr, length: number) => number;
  setCompilerOptions?: (shouldAddCompilerVersion: number) => boolean;

  constructor(wasmModule: any) {
    this.Module = wasmModule;
    if (!wasmModule) {
      throw Error(`wasm host did not load`);
    }
    this.compileKeyboardFile = this.Module.cwrap('kmcmp_Wasm_CompileKeyboardFile', 'boolean', ['string', 'string', 'number', 'number', 'number', 'string']);
    this.parseUnicodeSet = this.Module.cwrap('kmcmp_Wasm_ParseUnicodeSet', 'number', ['string', 'number', 'number']);
    this.setCompilerOptions = this.Module.cwrap('kmcmp_Wasm_SetCompilerOptions', 'boolean', ['number']);

    if (this.parseUnicodeSet === undefined
      || this.setCompilerOptions === undefined
      || this.compileKeyboardFile === undefined) {
        throw Error(`some wasm functions did not load properly.`);
    }
  }

  /**
   * Entry point into Wasm functions
   * @returns WasmWrapper
   */
  public static async load() : Promise<WasmWrapper> {
    return new WasmWrapper(await loadWasmHost());
  }
};

export class KmnCompiler {
  callbackName: string;
  callbacks: CompilerCallbacks;
  wasm: WasmWrapper;

  constructor() {
    this.callbackName = 'kmnCompilerCallback' + callbackProcIdentifier;
    callbackProcIdentifier++;
  }

  public async init(callbacks: CompilerCallbacks): Promise<boolean> {
    this.callbacks = callbacks;
    if(!this.wasm) {
      try {
        this.wasm = await WasmWrapper.load();
      } catch(e: any) {
        this.callbacks.reportMessage(CompilerMessages.Fatal_MissingWasmModule({e}));
        return false;
      }

    return this.verifyInitialized();
  }

  /**
   * Verify that wasm is spun up OK.
   * @returns true if OK
   */
  public verifyInitialized() : boolean {
    if(!this.callbacks) {
      // Can't report a message here.
      throw Error('Must call Compiler.init(callbacks) before proceeding');
    }
    if(!this.wasm) { // fail if wasm not loaded or function not found
      this.callbacks.reportMessage(CompilerMessages.Fatal_MissingWasmModule({}));
      return false;
    }
    return true;
  }

  public run(infile: string, outfile: string, options?: CompilerOptions): boolean {
    if(!this.verifyInitialized()) {
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
    const CKF_KEYMAN = 0;
    // const CKF_KEYMANWEB = 1; // not used -- legacy compiler only
    const CKF_KEYMANWEB_MODERN = 2;

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
        this.callbackName,
        options.target == 'js' ? CKF_KEYMANWEB_MODERN : CKF_KEYMAN);
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
    if(!this.verifyInitialized()) {
      return null;
    }

    if (!bufferSize) {
      bufferSize = 100; // TODO-LDML: Preflight mode? Reuse buffer?
    }
    const { Module } = this.wasm;
    const buf = Module.asm.malloc(bufferSize * 2 * Module.HEAPU32.BYTES_PER_ELEMENT);
    // TODO-LDML: Catch OOM
    const rc = this.wasm.parseUnicodeSet(pattern, buf, bufferSize);
    if (rc >= 0) {
      const ranges = [];
      const startu = (buf / Module.HEAPU32.BYTES_PER_ELEMENT);
      for (let i = 0; i < rc; i++) {
        const low = Module.HEAPU32[startu + (i * 2) + 0];
        const high = Module.HEAPU32[startu + (i * 2) + 1];
        ranges.push([low, high]);
      }
      // TODO-LDML: no free??
      // Module.asm.free(buf);
      return new UnicodeSet(pattern, ranges);
    } else {
      // translate error
      // TODO-LDML: no free??
      // Module.asm.free(buf);
      this.callbacks.reportMessage(getUnicodeSetError(rc));
      return null;
    }
  }
}

/**
 * translate UnicodeSet return code into a compiler event
 * @param rc parseUnicodeSet error code
 * @returns the compiler event
 */
function getUnicodeSetError(rc: number) : CompilerEvent  {
  // from kmcmplib.h
  const KMCMP_ERROR_SYNTAX_ERR = -1;
  const KMCMP_ERROR_HAS_STRINGS = -2;
  const KMCMP_ERROR_UNSUPPORTED_PROPERTY = -3;
  const KMCMP_FATAL_OUT_OF_RANGE = -4;
  switch(rc) {
    case KMCMP_ERROR_SYNTAX_ERR:
       return CompilerMessages.Error_UnicodeSetSyntaxError();
    case KMCMP_ERROR_HAS_STRINGS:
    return CompilerMessages.Error_UnicodeSetHasStrings();
    case KMCMP_ERROR_UNSUPPORTED_PROPERTY:
       return CompilerMessages.Error_UnicodeSetHasProperties();
    case KMCMP_FATAL_OUT_OF_RANGE:
      return CompilerMessages.Fatal_UnicodeSetOutOfRange();
    default:
      return CompilerMessages.Fatal_UnexpectedException({e: `Unexpected UnicodeSet error code ${rc}`});
  }
}

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

  toString() : string {
    return this.pattern;
  }
}
