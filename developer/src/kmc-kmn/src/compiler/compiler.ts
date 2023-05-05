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

export class Compiler {
  wasmModule: any;
  compileKeyboardFile: any;
  setCompilerOptions: any;
  callbackName: string;
  callbacks: CompilerCallbacks;

  constructor() {
    this.callbackName = 'kmnCompilerCallback' + callbackProcIdentifier;
    callbackProcIdentifier++;
  }

  public async init(): Promise<boolean> {
    if(!this.wasmModule) {
      this.wasmModule = await loadWasmHost();
      this.compileKeyboardFile = this.wasmModule.cwrap('kmcmp_Wasm_CompileKeyboardFile', 'boolean', ['string', 'string',
        'number', 'number', 'number', 'string', 'number']);
      this.setCompilerOptions = this.wasmModule.cwrap('kmcmp_Wasm_SetCompilerOptions', 'boolean', ['number']);
    }
    return this.compileKeyboardFile !== undefined && this.setCompilerOptions !== undefined;
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
    const CKF_KEYMAN = 0;
    const CKF_KEYMANWEB = 1;

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
        this.callbackName,
        options.target == 'js' ? CKF_KEYMANWEB : CKF_KEYMAN);
    } catch(e) {
      this.callbacks.reportMessage(CompilerMessages.Fatal_UnexpectedException({e:e}));
      return false;
    }
  }
}