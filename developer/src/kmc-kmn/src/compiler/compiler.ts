/*
TODO: implement additional interfaces:

  typedef bool (*kmcmp_ValidateJsonMessageProc)();
  extern "C" bool kmcmp_ValidateJsonFile();
*/

// TODO: rename wasm-host?
import { UnicodeSetParser, UnicodeSet, Osk } from '@keymanapp/common-types';
import { CompilerCallbacks, CompilerEvent, CompilerOptions, KeymanFileTypes, KvkFileWriter, KvksFileReader } from '@keymanapp/common-types';
import loadWasmHost from '../import/kmcmplib/wasm-host.js';
import { CompilerMessages, mapErrorFromKmcmplib } from './messages.js';
import { WriteCompiledKeyboard } from '../kmw-compiler/write-compiled-keyboard.js';

export interface CompilerResultFile {
  filename: string;
  data: Uint8Array;
};

export const COMPILETARGETS_KMX =   0x01;
export const COMPILETARGETS_JS =    0x02;
export const COMPILETARGETS__MASK = 0x03;

export interface CompilerResultMetadata {
  /**
   * A bitmask, consisting of COMPILETARGETS_KMX and/or COMPILETARGETS_JS
   */
  targets: number;
  kvksFilename?: string;
  displayMapFilename?: string;
  displayMap?: Osk.PuaMap;
};

export interface CompilerResult {
  kmx?: CompilerResultFile;
  kvk?: CompilerResultFile;
  js?: CompilerResultFile;
  data: CompilerResultMetadata;
};

export interface KmnCompilerOptions extends CompilerOptions {
};

const baseOptions: KmnCompilerOptions = {
  shouldAddCompilerVersion: true,
  saveDebug: true,
  compilerWarningsAsErrors: false,
  warnDeprecatedCode: true,
};

/**
 * Allows multiple instances of the Compiler class, by ensuring that the
 * 'unique' kmnCompilerCallback global will be correlated with a specific
 * instance of the Compiler class
 */
let callbackProcIdentifier = 0;

const
  callbackPrefix = 'kmnCompilerCallbacks_';

export class KmnCompiler implements UnicodeSetParser {
  private Module: any;
  callbackID: string; // a unique numeric id added to globals with prefixed names
  callbacks: CompilerCallbacks;

  constructor() {
    this.callbackID = callbackPrefix + callbackProcIdentifier.toString();
    callbackProcIdentifier++;
  }

  public async init(callbacks: CompilerCallbacks): Promise<boolean> {
    this.callbacks = callbacks;
    if(!this.Module) {
      try {
        this.Module = await loadWasmHost();
      } catch(e: any) {
        /* c8 ignore next 3 */
        this.callbacks.reportMessage(CompilerMessages.Fatal_MissingWasmModule({e}));
        return false;
      }
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
    if(!this.Module) {
      /* c8 ignore next 4 */
      // fail if wasm not loaded or function not found
      this.callbacks.reportMessage(CompilerMessages.Fatal_MissingWasmModule({}));
      return false;
    }
    return true;
  }

  // TODO: use outFile from options
  public run(infile: string, outfile: string, options?: KmnCompilerOptions): boolean {
    let result = this.runCompiler(infile, outfile, options);
    if(result) {
      if(result.kmx) {
        this.callbacks.fs.writeFileSync(result.kmx.filename, result.kmx.data);
      }
      if(result.kvk) {
        this.callbacks.fs.writeFileSync(result.kvk.filename, result.kvk.data);
      }
      if(result.js) {
        this.callbacks.fs.writeFileSync(result.js.filename, result.js.data);
      }
    }
    return !!result;
  }

  private compilerMessageCallback = (line: number, code: number, msg: string): number => {
    this.callbacks.reportMessage(mapErrorFromKmcmplib(line, code, msg));
    return 1;
  }

  private cachedFile: {filename: string; data: Uint8Array} = {
    filename: null,
    data: null
  };

  private loadFileCallback = (filename: string, baseFilename: string, buffer: number, bufferSize: number): number => {
    let resolvedFilename = this.callbacks.resolveFilename(baseFilename, filename);
    let data: Uint8Array;
    if(this.cachedFile.filename == resolvedFilename) {
      data = this.cachedFile.data;
    }
    else {
      data = this.callbacks.loadFile(resolvedFilename);
      if(!data) {
        return -1;
      }
      this.cachedFile.filename = resolvedFilename;
      this.cachedFile.data = data;
    }

    if(buffer == 0) {
      /* We need to return buffer size required */
      return data.byteLength;
    }

    if(bufferSize != data.byteLength) {
      /* c8 ignore next 2 */
      throw new Error(`loadFileCallback: second call, expected file size ${bufferSize} == ${data.byteLength}`);
    }

    this.Module.HEAP8.set(data, buffer);

    return 1;
  }

  public runCompiler(infile: string, outfile: string, options: KmnCompilerOptions): CompilerResult {
    if(!this.verifyInitialized()) {
      /* c8 ignore next 2 */
      return null;
    }

    options = {...baseOptions, ...options};

    (globalThis as any)[this.callbackID] = {
      message: this.compilerMessageCallback,
      loadFile: this.loadFileCallback
    };

    let result: CompilerResult = {data:{targets:0}};
    let wasm_interface = new this.Module.CompilerInterface();
    let wasm_options = new this.Module.CompilerOptions();
    let wasm_result = null;
    try {
      wasm_options.saveDebug = options.saveDebug;
      wasm_options.compilerWarningsAsErrors = options.compilerWarningsAsErrors;
      wasm_options.warnDeprecatedCode = options.warnDeprecatedCode;
      wasm_options.shouldAddCompilerVersion = options.shouldAddCompilerVersion;
      wasm_options.target = 0; // CKF_KEYMAN; TODO use COMPILETARGETS_KMX
      wasm_interface.callbacksKey = this.callbackID; // key of object on globalThis
      wasm_result = this.Module.kmcmp_compile(infile, wasm_options, wasm_interface);
      if(!wasm_result.result) {
        return null;
      }

      if(wasm_result.targets & COMPILETARGETS_KMX) {
        result.kmx = {
          filename: outfile,
          data: new Uint8Array(this.Module.HEAP8.buffer, wasm_result.kmx, wasm_result.kmxSize)
        };
      }

      //
      // Visual Keyboard transform
      //

      result.data.kvksFilename = wasm_result.kvksFilename;
      result.data.displayMapFilename = wasm_result.displayMapFilename;
      result.data.displayMap = null;

      if(wasm_result.displayMapFilename) {
        result.data.displayMap = this.loadDisplayMapping(infile, result.data.displayMapFilename)
      }

      if(result.data.kvksFilename) {
        result.kvk = this.runKvkCompiler(result.data.kvksFilename, infile, outfile, result.data.displayMap);
        if(!result.kvk) {
          return null;
        }
      }

      //
      // KeymanWeb compiler
      //

      if(wasm_result.targets & COMPILETARGETS_JS) {
        wasm_options.target = 1; // CKF_KEYMANWEB TODO use COMPILETARGETS_JS
        wasm_result = this.Module.kmcmp_compile(infile, wasm_options, wasm_interface);
        if(!wasm_result.result) {
          return null;
        }

        let web_kmx = {
          filename: outfile,
          data: new Uint8Array(this.Module.HEAP8.buffer, wasm_result.kmx, wasm_result.kmxSize)
        };

        result.js = this.runWebCompiler(infile, outfile, web_kmx.data, result.kvk?.data, result.data.displayMap, options);
        if(!result.js) {
          return null;
        }
      }

      return result;
    } catch(e) {
      /* c8 ignore next 3 */
      this.callbacks.reportMessage(CompilerMessages.Fatal_UnexpectedException({e:e}));
      return null;
    } finally {
      if(wasm_result) {
        wasm_result.delete();
      }
      wasm_interface.delete();
      wasm_options.delete();
      delete (globalThis as any)[this.callbackID];
    }
  }

  private runWebCompiler(
    kmnFilename: string,
    kmxFilename: string,
    web_kmx: Uint8Array,
    kvk: Uint8Array,
    displayMap: Osk.PuaMap,
    options: CompilerOptions
  ): CompilerResultFile {
    const data = WriteCompiledKeyboard(this.callbacks, kmnFilename, web_kmx, kvk, displayMap, options.saveDebug);
    if(!data) {
      return null;
    }

    return {
      filename: this.callbacks.path.join(this.callbacks.path.dirname(kmxFilename),
      this.keyboardIdFromKmnFilename(kmnFilename) + KeymanFileTypes.Binary.WebKeyboard),
      data: new TextEncoder().encode(data)
    };
  }

  private keyboardIdFromKmnFilename(kmnFilename: string): string {
    return this.callbacks.path.basename(kmnFilename, KeymanFileTypes.Source.KeymanKeyboard);
  }

  private runKvkCompiler(kvksFilename: string, kmnFilename: string, kmxFilename: string, displayMap?: Osk.PuaMap) {
    // The compiler detected a .kvks file, which needs to be captured
    let reader = new KvksFileReader();
    kvksFilename = this.callbacks.resolveFilename(kmnFilename, kvksFilename);
    let filename = this.callbacks.path.basename(kvksFilename);
    let kvks = null;
    try {
      kvks = reader.read(this.callbacks.loadFile(kvksFilename));
      reader.validate(kvks, this.callbacks.loadSchema('kvks'));
    } catch(e) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidKvksFile({filename, e}));
      return null;
    }
    let invalidVkeys: string[] = [];
    let vk = reader.transform(kvks, invalidVkeys);
    for(let invalidVkey of invalidVkeys) {
      this.callbacks.reportMessage(CompilerMessages.Warn_InvalidVkeyInKvksFile({filename, invalidVkey}));
    }

    // Make sure that we maintain the correspondence between source keyboard and
    // .kvk. Appears to be used currently only by Windows package installer.
    vk.header.associatedKeyboard = this.keyboardIdFromKmnFilename(kmnFilename);

    if(displayMap) {
      // Remap using the osk-char-use-rewriter
      Osk.remapVisualKeyboard(vk, displayMap);
    }

    let writer = new KvkFileWriter();
    return {
      filename: this.callbacks.path.join(this.callbacks.path.dirname(kmxFilename),
        this.callbacks.path.basename(kvksFilename, KeymanFileTypes.Source.VisualKeyboard) + KeymanFileTypes.Binary.VisualKeyboard),
      data: writer.write(vk)
    };
  }

  private loadDisplayMapping(kmnFilename:string, displayMapFilename: string): Osk.PuaMap {
    // Remap using the osk-char-use-rewriter
    displayMapFilename = this.callbacks.resolveFilename(kmnFilename, displayMapFilename);
    try {
      // Expected file format: displaymap.schema.json
      const data = this.callbacks.loadFile(displayMapFilename);
      const schema = this.callbacks.loadSchema('displaymap');
      const mapping = JSON.parse(new TextDecoder().decode(data));
      return Osk.parseMapping(mapping, schema);
    } catch(e) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidDisplayMapFile({filename: displayMapFilename, e}));
      return null;
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
      /* c8 ignore next 2 */
      return null;
    }

    if (!bufferSize) {
      /* c8 ignore next 2 */
      bufferSize = 100; // TODO-LDML: Preflight mode? Reuse buffer?
    }
    const buf = this.Module.asm.malloc(bufferSize * 2 * this.Module.HEAPU32.BYTES_PER_ELEMENT);
    // TODO-LDML: Catch OOM
    const rc = this.Module.kmcmp_parseUnicodeSet(pattern, buf, bufferSize);
    if (rc >= 0) {
      const ranges = [];
      const startu = (buf / this.Module.HEAPU32.BYTES_PER_ELEMENT);
      for (let i = 0; i < rc; i++) {
        const low = this.Module.HEAPU32[startu + (i * 2) + 0];
        const high = this.Module.HEAPU32[startu + (i * 2) + 1];
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
function getUnicodeSetError(rc: number) : CompilerEvent {
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
      /* c8 ignore next */
      return CompilerMessages.Fatal_UnexpectedException({e: `Unexpected UnicodeSet error code ${rc}`});
  }
}

