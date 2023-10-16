/*
TODO: implement additional interfaces:

  typedef bool (*kmcmp_ValidateJsonMessageProc)();
  extern "C" bool kmcmp_ValidateJsonFile();
*/

// TODO: rename wasm-host?
import { UnicodeSetParser, UnicodeSet, Osk, VisualKeyboard, KvkFileReader } from '@keymanapp/common-types';
import { CompilerCallbacks, CompilerEvent, CompilerOptions, KeymanFileTypes, KvkFileWriter, KvksFileReader } from '@keymanapp/common-types';
import loadWasmHost from '../import/kmcmplib/wasm-host.js';
import { CompilerMessages, mapErrorFromKmcmplib } from './messages.js';
import { WriteCompiledKeyboard } from '../kmw-compiler/kmw-compiler.js';

export interface CompilerResultFile {
  filename: string;
  data: Uint8Array;
};

//
// Matches kmcmplibapi.h definitions
//

export const STORETYPE_STORE       = 0x01;
export const STORETYPE_RESERVED    = 0x02;
export const STORETYPE_OPTION      = 0x04;
export const STORETYPE_DEBUG       = 0x08;
export const STORETYPE_CALL        = 0x10;
export const STORETYPE__MASK       = 0x1F;

export interface CompilerResultExtraStore {
  storeType: number; // STORETYPE__MASK
  name: string;      // when debug=false, the .kmx will not have store names
  line: number;      // source line number where store is defined
};

export interface CompilerResultExtraGroup {
  isReadOnly: boolean;
  name: string;
};

export const COMPILETARGETS_KMX =   0x01;
export const COMPILETARGETS_JS =    0x02;
export const COMPILETARGETS__MASK = 0x03;

/**
 * Data in CompilerResultExtra comes from kmcmplib
 */
export interface CompilerResultExtra {
  /**
   * A bitmask, consisting of COMPILETARGETS_KMX and/or COMPILETARGETS_JS
   */
  targets: number;
  kvksFilename?: string;
  displayMapFilename?: string;
  stores: CompilerResultExtraStore[];
  groups: CompilerResultExtraGroup[];
};

//
// Internal in-memory result from a successful compilation
//

export interface CompilerResult {
  kmx?: CompilerResultFile;
  kvk?: CompilerResultFile;
  js?: CompilerResultFile;
  extra: CompilerResultExtra;
  displayMap?: Osk.PuaMap;
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

interface MallocAndFree {
  malloc(sz: number) : number;
  free(p: number) : null;
};

let
  Module: any;

export class KmnCompiler implements UnicodeSetParser {
  callbackID: string; // a unique numeric id added to globals with prefixed names
  callbacks: CompilerCallbacks;
  wasmExports: MallocAndFree;

  constructor() {
    this.callbackID = callbackPrefix + callbackProcIdentifier.toString();
    callbackProcIdentifier++;
  }

  public async init(callbacks: CompilerCallbacks): Promise<boolean> {
    this.callbacks = callbacks;
    if(!Module) {
      try {
        Module = await loadWasmHost();
      } catch(e: any) {
        /* c8 ignore next 3 */
        this.callbacks.reportMessage(CompilerMessages.Fatal_MissingWasmModule({e}));
        return false;
      }
    }
    this.wasmExports = (Module.wasmExports ?? Module.asm);
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
    if(!Module) {
      /* c8 ignore next 4 */
      // fail if wasm not loaded or function not found
      this.callbacks.reportMessage(CompilerMessages.Fatal_MissingWasmModule({}));
      return false;
    }
    return true;
  }

  public run(infile: string, options?: KmnCompilerOptions): boolean {
    let result = this.runCompiler(infile, options);
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

    Module.HEAP8.set(data, buffer);

    return 1;
  }

  private copyWasmResult(wasm_result: any): CompilerResult {
    let result: CompilerResult = {
      // We cannot Object.assign or {...} on a wasm-defined object, so...
      extra: {
        targets: wasm_result.extra.targets,
        displayMapFilename: wasm_result.extra.displayMapFilename,
        kvksFilename: wasm_result.extra.kvksFilename,
        stores: [],
        groups: [],
      },
      displayMap: null
    };
    for(let store of wasm_result.extra.stores) {
      result.extra.stores.push({storeType: store.storeType, name: store.name, line: store.line});
    }
    for(let group of wasm_result.extra.groups) {
      result.extra.groups.push({isReadOnly: group.isReadOnly, name: group.name});
    }

    return result;
  }

  /**
   * By default, when a `Uint8Array` is created from an `ArrayBuffer` (e.g.
   * `Module.HEAP8.buffer`), it is a dynamic view into that buffer. This module
   * buffer can be dynamically reallocated at any time, which can happen when
   * allocating memory in WASM code (so the change will  look _really_ weird in
   * a stack trace). Thus, to ensure we don't trip over ourselves, we need to
   * copy the buffer. Fortunately, creating a `Uint8Array` from a `Uint8Array`
   * copies the data, and is pretty quick.
   * @param offset    Offset into the WASM memory space, in bytes
   * @param size      Size of the buffer to copy, in bytes
   * @returns         A _copy_ of the data in a new Uint8Array
   */
  private copyWasmBuffer(offset: number, size: number): Uint8Array {
    return new Uint8Array(new Uint8Array(Module.HEAP8.buffer, offset, size));
  }

  public runCompiler(infile: string, options: KmnCompilerOptions): CompilerResult {
    if(!this.verifyInitialized()) {
      /* c8 ignore next 2 */
      return null;
    }

    options = {...baseOptions, ...options};

    options.outFile = options.outFile ?? infile.replace(/\.kmn$/i, '.kmx');

    (globalThis as any)[this.callbackID] = {
      message: this.compilerMessageCallback,
      loadFile: this.loadFileCallback
    };

    let wasm_interface = new Module.CompilerInterface();
    let wasm_options = new Module.CompilerOptions();
    let wasm_result = null;
    try {
      wasm_options.saveDebug = options.saveDebug;
      wasm_options.compilerWarningsAsErrors = options.compilerWarningsAsErrors;
      wasm_options.warnDeprecatedCode = options.warnDeprecatedCode;
      wasm_options.shouldAddCompilerVersion = options.shouldAddCompilerVersion;
      wasm_options.target = 0; // CKF_KEYMAN; TODO use COMPILETARGETS_KMX
      wasm_interface.callbacksKey = this.callbackID; // key of object on globalThis
      wasm_result = Module.kmcmp_compile(infile, wasm_options, wasm_interface);
      if(!wasm_result.result) {
        return null;
      }

      const result: CompilerResult = this.copyWasmResult(wasm_result);

      if(result.extra.targets & COMPILETARGETS_KMX) {
        result.kmx = {
          filename: options.outFile,
          data: this.copyWasmBuffer(wasm_result.kmx, wasm_result.kmxSize)
        };
      }

      //
      // Visual Keyboard transform
      //


      if(result.extra.displayMapFilename) {
        result.displayMap = this.loadDisplayMapping(infile, result.extra.displayMapFilename)
      }

      if(result.extra.kvksFilename) {
        result.kvk = this.runKvkCompiler(result.extra.kvksFilename, infile, options.outFile, result.displayMap);
        if(!result.kvk) {
          return null;
        }
      }

      //
      // KeymanWeb compiler
      //

      if(wasm_result.extra.targets & COMPILETARGETS_JS) {
        wasm_options.target = 1; // CKF_KEYMANWEB TODO use COMPILETARGETS_JS
        wasm_result = Module.kmcmp_compile(infile, wasm_options, wasm_interface);
        if(!wasm_result.result) {
          return null;
        }
        const kmw_result: CompilerResult = this.copyWasmResult(wasm_result);
        kmw_result.displayMap = result.displayMap; // we can safely re-use the kmx compile displayMap

        const web_kmx = this.copyWasmBuffer(wasm_result.kmx, wasm_result.kmxSize);
        result.js = this.runWebCompiler(infile, options.outFile, web_kmx, result.kvk?.data, kmw_result, options);
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
    kmxResult: CompilerResult,
    options: CompilerOptions
  ): CompilerResultFile {
    const data = WriteCompiledKeyboard(this.callbacks, kmnFilename, web_kmx, kvk, kmxResult, options.saveDebug);
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
    kvksFilename = this.callbacks.resolveFilename(kmnFilename, kvksFilename);
    const filename = this.callbacks.path.basename(kvksFilename);
    let basename = null;
    let vk: VisualKeyboard.VisualKeyboard = null;
    if(filename.endsWith('.kvk')) {
      /* Legacy keyboards may reference a binary .kvk. That's not an error */
      // TODO: (lowpri) add hint to convert to .kvks?
      basename = this.callbacks.path.basename(kvksFilename, KeymanFileTypes.Binary.VisualKeyboard);
      const reader = new KvkFileReader();
      try {
        vk = reader.read(this.callbacks.loadFile(kvksFilename));
      } catch(e) {
        this.callbacks.reportMessage(CompilerMessages.Error_InvalidKvkFile({filename, e}));
        return null;
      }
    } else {
      basename = this.callbacks.path.basename(kvksFilename, KeymanFileTypes.Source.VisualKeyboard);
      const reader = new KvksFileReader();
      let kvks = null;
      try {
        kvks = reader.read(this.callbacks.loadFile(kvksFilename));
        reader.validate(kvks);
      } catch(e) {
        this.callbacks.reportMessage(CompilerMessages.Error_InvalidKvksFile({filename, e}));
        return null;
      }
      let invalidVkeys: string[] = [];
      vk = reader.transform(kvks, invalidVkeys);
      for(let invalidVkey of invalidVkeys) {
        this.callbacks.reportMessage(CompilerMessages.Warn_InvalidVkeyInKvksFile({filename, invalidVkey}));
      }
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
        basename + KeymanFileTypes.Binary.VisualKeyboard),
      data: writer.write(vk)
    };
  }

  private loadDisplayMapping(kmnFilename:string, displayMapFilename: string): Osk.PuaMap {
    // Remap using the osk-char-use-rewriter
    displayMapFilename = this.callbacks.resolveFilename(kmnFilename, displayMapFilename);
    try {
      // Expected file format: displaymap.schema.json
      const data = this.callbacks.loadFile(displayMapFilename);
      const mapping = JSON.parse(new TextDecoder().decode(data));
      return Osk.parseMapping(mapping);
    } catch(e) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidDisplayMapFile({filename: displayMapFilename, e}));
      return null;
    }
  }

  public testSentry() {
    if(!this.verifyInitialized()) {
      return null;
    }

    return Module.kmcmp_testSentry();
  }

  /** convert `\u{1234}` to `\u1234` etc */
  public static fixNewPattern(pattern: string) : string {
    pattern = pattern.replaceAll(/\\u\{([0-9a-fA-F]{6})\}/g, `\\U00$1`);
    pattern = pattern.replaceAll(/\\u\{([0-9a-fA-F]{5})\}/g, `\\U000$1`);
    pattern = pattern.replaceAll(/\\u\{([0-9a-fA-F]{4})\}/g, `\\u$1`);
    pattern = pattern.replaceAll(/\\u\{([0-9a-fA-F]{3})\}/g, `\\u0$1`);
    pattern = pattern.replaceAll(/\\u\{([0-9a-fA-F]{2})\}/g, `\\u00$1`);
    pattern = pattern.replaceAll(/\\u\{([0-9a-fA-F]{1})\}/g, `\\u000$1`);
    return pattern;
  }

  /**
   *
   * @param pattern UnicodeSet pattern such as `[a-z]`
   * @param rangeCount number of ranges to allocate
   * @returns UnicodeSet accessor object, or null on failure
   */
  public parseUnicodeSet(pattern: string, rangeCount: number) : UnicodeSet | null {
    if(!this.verifyInitialized()) {
      /* c8 ignore next 2 */
      return null;
    }

    // TODO-LDML: Catch OOM
    const buf = this.wasmExports.malloc(rangeCount * 2 * Module.HEAPU32.BYTES_PER_ELEMENT);
    // fix \u1234 pattern format
    pattern = KmnCompiler.fixNewPattern(pattern);
    /** If <= 0: return code. If positive: range count */
    const rc = Module.kmcmp_parseUnicodeSet(pattern, buf, rangeCount * 2);
    if (rc >= 0) {
      const ranges = [];
      const startu = (buf / Module.HEAPU32.BYTES_PER_ELEMENT);
      for (let i = 0; i < rc; i++) {
        const start  = Module.HEAPU32[startu + (i * 2) + 0];
        const end    = Module.HEAPU32[startu + (i * 2) + 1];
        ranges.push([start, end]);
      }
      this.wasmExports.free(buf);
      return new UnicodeSet(pattern, ranges);
    } else {
      this.wasmExports.free(buf);
      // translate error code into callback
      this.callbacks.reportMessage(getUnicodeSetError(rc));
      return null;
    }
  }
  public sizeUnicodeSet(pattern: string) : number {
    if(!this.verifyInitialized()) {
      /* c8 ignore next 2 */
      return null;
    }
    // fix \u1234 pattern format
    pattern = KmnCompiler.fixNewPattern(pattern);
    // call with rangeCount = 0 to invoke in 'preflight' mode.
    const rc = Module.kmcmp_parseUnicodeSet(pattern, 0, 0);
    if (rc >= 0) {
      return rc;
    } else {
      this.callbacks.reportMessage(getUnicodeSetError(rc));
      return -1;
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

