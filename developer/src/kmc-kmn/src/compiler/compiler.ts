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
import loadWasmHost from '../import/kmcmplib/wasm-host.js';

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

export class Compiler {
  wasmModule: any;
  compileKeyboardFile: any;
  setCompilerOptions: any;

  public async init(): Promise<boolean> {
    if(!this.wasmModule) {
      this.wasmModule = await loadWasmHost();
      this.compileKeyboardFile = this.wasmModule.cwrap('kmcmp_Wasm_CompileKeyboardFile', 'number', ['string', 'string',
        'number', 'number', 'number', 'string']);
      this.setCompilerOptions = this.wasmModule.cwrap('kmcmp_Wasm_SetCompilerOptions', 'number', ['number']);
    }
    return this.compileKeyboardFile !== undefined && this.setCompilerOptions !== undefined;
  }

  public run(infile: string, outfile: string, options?: CompilerOptions): boolean {
    if(!this.wasmModule) {
      return false;
    }

    options = {...baseOptions, ...options};

    (globalThis as any).msgproc = function(line: number, code: number, msg: string): number {
      // TODO: link into the kmc error reporting infrastructure
      console.log(`[${line}] ${code.toString(16)}: ${msg}`);
      return 1; // 1 == continue build
    }

    // TODO: use callbacks for file access -- so kmc-kmn is entirely fs agnostic
    let result = this.runCompiler(infile, outfile, options) == 1;

    (globalThis as any).msgproc = null;

    return result;
  }

  private runCompiler(infile: string, outfile: string, options: CompilerOptions): number {
    try {
      if(!this.setCompilerOptions(options.shouldAddCompilerVersion)) {
        console.error('Unable to set compiler options');
      }
      return this.compileKeyboardFile(
        infile,
        outfile,
        options.saveDebug ? 1 : 0,
        options.compilerWarningsAsErrors ? 1 : 0,
        options.warnDeprecatedCode ? 1 : 0,
        'msgproc');
    } catch(e) {
      // TODO: use sentry
      console.error(e);
    }
    return 0;
  }
}