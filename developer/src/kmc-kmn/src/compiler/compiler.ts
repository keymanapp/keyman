/*
TODO: implement additional interfaces:

  typedef bool (*kmcmp_ValidateJsonMessageProc)();
  extern "C" bool kmcmp_ValidateJsonFile();
*/

// TODO: rename wasm-host?
import { UnicodeSetParser, UnicodeSet, VisualKeyboard, KvkFileReader, KeymanCompiler, KeymanCompilerArtifacts, KeymanCompilerArtifactOptional, KeymanCompilerResult, KeymanCompilerArtifact } from '@keymanapp/common-types';
import { CompilerCallbacks, CompilerEvent, CompilerOptions, KeymanFileTypes, KvkFileWriter, KvksFileReader } from '@keymanapp/common-types';
import * as Osk from './osk.js';
import loadWasmHost from '../import/kmcmplib/wasm-host.js';
import { KmnCompilerMessages, mapErrorFromKmcmplib } from './kmn-compiler-messages.js';
import { WriteCompiledKeyboard } from '../kmw-compiler/kmw-compiler.js';

//
// Matches kmcmplibapi.h definitions
//

export const STORETYPE_STORE       = 0x01;
export const STORETYPE_RESERVED    = 0x02;
export const STORETYPE_OPTION      = 0x04;
export const STORETYPE_DEBUG       = 0x08;
export const STORETYPE_CALL        = 0x10;
export const STORETYPE__MASK       = 0x1F;

/** @internal */
export interface CompilerResultExtraStore {
  storeType: number; // STORETYPE__MASK
  name: string;      // when debug=false, the .kmx will not have store names
  line: number;      // source line number where store is defined
};

/** @internal */
export interface CompilerResultExtraGroup {
  isReadOnly: boolean;
  name: string;
};

export const COMPILETARGETS_KMX =   0x01;
export const COMPILETARGETS_JS =    0x02;
export const COMPILETARGETS__MASK = 0x03;

/**
 * @internal
 * Data in CompilerResultExtra comes from kmcmplib. This is used by other
 * compilers such as KmwCompiler and is not intended for external use.
 */
export interface KmnCompilerResultExtra {
  /**
   * A bitmask, consisting of COMPILETARGETS_KMX and/or COMPILETARGETS_JS
   */
  targets: number;
  kvksFilename?: string;
  displayMapFilename?: string;
  stores: CompilerResultExtraStore[];
  groups: CompilerResultExtraGroup[];
};

/** @internal */
export interface KmnCompilerResultMessage {
  message: string;
  errorCode: number;
  lineNumber: number;
}

/**
 * @public
 * Internal in-memory build artifacts from a successful compilation
 */
export interface KmnCompilerArtifacts extends KeymanCompilerArtifacts {
  /**
   * Binary keyboard filedata and filename - installable into Keyman desktop
   * projects
   */
  kmx?: KeymanCompilerArtifactOptional;
  /**
   * Binary on screen keyboard filedata and filename - installable into Keyman
   * desktop projects alongside .kmx
   */
  kvk?: KeymanCompilerArtifactOptional;
  /**
   * Javascript keyboard filedata and filename - installable into KeymanWeb,
   * Keyman mobile products
   */
  js?: KeymanCompilerArtifactOptional;
};

/**
 * @public
 * Build artifacts from the .kmn compiler
 */
export interface KmnCompilerResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link KmnCompiler.write}
   */
  artifacts: KmnCompilerArtifacts;
  /**
   * Internal additional metadata used by secondary compile phases such as
   * KmwCompiler, not intended for external use
   */
  extra: KmnCompilerResultExtra;
  /**
   * Mapping data for `&displayMap`, intended for use by kmc-analyze
   */
  displayMap?: Osk.PuaMap;
};

/**
 * @public
 * Options for the .kmn compiler
 */
export interface KmnCompilerOptions extends CompilerOptions {
};

const baseOptions: KmnCompilerOptions = {
  shouldAddCompilerVersion: true,
  saveDebug: true,
  compilerWarningsAsErrors: false,
  warnDeprecatedCode: true,
};

interface MallocAndFree {
  malloc(sz: number) : number;
  free(p: number) : null;
};

let
  Module: any;

/**
 * @public
 * Compiles a .kmn file to a .kmx, .kvk, and/or .js. The compiler does not read
 * or write from filesystem or network directly, but relies on callbacks for all
 * external IO.
 */
export class KmnCompiler implements KeymanCompiler, UnicodeSetParser {
  private callbacks: CompilerCallbacks;
  private wasmExports: MallocAndFree;
  private options: KmnCompilerOptions;

  /**
   * Initialize the compiler, including loading the WASM host for kmcmplib.
   * Copies options.
   * @param callbacks - Callbacks for external interfaces, including message
   *                    reporting and file io
   * @param options   - Compiler options
   * @returns false if initialization fails
   */
  public async init(callbacks: CompilerCallbacks, options: KmnCompilerOptions): Promise<boolean> {
    this.callbacks = callbacks;
    this.options = {...options};
    if(!Module) {
      try {
        Module = await loadWasmHost();
      } catch(e: any) {
        /* c8 ignore next 3 */
        this.callbacks.reportMessage(KmnCompilerMessages.Fatal_MissingWasmModule({e}));
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
      this.callbacks.reportMessage(KmnCompilerMessages.Fatal_MissingWasmModule({}));
      return false;
    }
    return true;
  }

  /**
   * Write artifacts from a successful compile to disk, via callbacks methods.
   * The artifacts written may include:
   *
   * - .kmx file - binary keyboard used by Keyman on desktop platforms
   * - .kvk file - binary on screen keyboard used by Keyman on desktop platforms
   * - .js file - Javascript keyboard for web and touch platforms
   *
   * @param artifacts - object containing artifact binary data to write out
   * @returns true on success
   */
  public async write(artifacts: KmnCompilerArtifacts): Promise<boolean> {
    if(!artifacts) {
      throw Error('artifacts must be defined');
    }

    if(artifacts.kmx) {
      this.callbacks.fs.writeFileSync(artifacts.kmx.filename, artifacts.kmx.data);
    }
    if(artifacts.kvk) {
      this.callbacks.fs.writeFileSync(artifacts.kvk.filename, artifacts.kvk.data);
    }
    if(artifacts.js) {
      this.callbacks.fs.writeFileSync(artifacts.js.filename, artifacts.js.data);
    }

    return true;
  }

  private copyWasmResult(wasm_result: any): KmnCompilerResult {
    let result: KmnCompilerResult = {
      // We cannot Object.assign or {...} on a wasm-defined object, so...
      artifacts: {},
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
   * @param offset  - Offset into the WASM memory space, in bytes.
   * @param size    - Size of the buffer to copy, in bytes.
   * @returns         A _copy_ of the data in a new Uint8Array.
   */
  private copyWasmBuffer(offset: number, size: number): Uint8Array {
    return new Uint8Array(new Uint8Array(Module.HEAP8.buffer, offset, size));
  }

  /**
   * Compiles a .kmn file to .kmx, .kvk, and/or .js files. Returns an object
   * containing binary artifacts on success. The files are passed in by name,
   * and the compiler will use callbacks as passed to the
   * {@link KmnCompiler.init} function to read any input files by disk.
   * @param infile  - Path to source file. Path will be parsed to find relative
   *                  references in the .kmn file, such as icon or On Screen
   *                  Keyboard file
   * @param outfile - Path to output file. The file will not be written to, but
   *                  will be included in the result for use by
   *                  {@link KmnCompiler.write}.
   * @returns         Binary artifacts on success, null on failure.
   */
  public async run(infile: string, outfile: string): Promise<KmnCompilerResult> {
    if(!this.verifyInitialized()) {
      /* c8 ignore next 2 */
      return null;
    }

    const options = {...baseOptions, ...this.options};

    outfile = outfile ?? infile.replace(/\.kmn$/i, '.kmx');

    const compiler = this;
    const wasm_callbacks = Module.WasmCallbackInterface.implement({
      message: function(message: KmnCompilerResultMessage) {
        compiler.callbacks.reportMessage(mapErrorFromKmcmplib(message.lineNumber, message.errorCode, message.message));
      },
      loadFile: function(filename: string, baseFilename: string): number[] {
        const data: Uint8Array = compiler.callbacks.loadFile(compiler.callbacks.resolveFilename(baseFilename, filename));
        return data ? Array.from(data) : null;
      }
    });

    let wasm_options = new Module.CompilerOptions();
    let wasm_result = null;
    try {
      wasm_options.saveDebug = options.saveDebug;
      wasm_options.compilerWarningsAsErrors = options.compilerWarningsAsErrors;
      wasm_options.warnDeprecatedCode = options.warnDeprecatedCode;
      wasm_options.shouldAddCompilerVersion = options.shouldAddCompilerVersion;
      wasm_options.target = 0; // CKF_KEYMAN; TODO use COMPILETARGETS_KMX

      wasm_result = Module.kmcmp_compile(infile, wasm_options, wasm_callbacks);
      if(!wasm_result.result) {
        return null;
      }

      const result: KmnCompilerResult = this.copyWasmResult(wasm_result);

      if(result.extra.targets & COMPILETARGETS_KMX) {
        result.artifacts.kmx = {
          filename: outfile,
          data: this.copyWasmBuffer(wasm_result.kmx, wasm_result.kmxSize)
        };
      }

      //
      // Visual Keyboard transform
      //


      if(result.extra.displayMapFilename) {
        result.displayMap = this.loadDisplayMapping(infile, result.extra.displayMapFilename)
        if(!result.displayMap) {
          return null;
        }
      }

      if(result.extra.kvksFilename) {
        result.artifacts.kvk = this.runKvkCompiler(result.extra.kvksFilename, infile, outfile, result.displayMap);
        if(!result.artifacts.kvk) {
          return null;
        }
      }

      //
      // KeymanWeb compiler
      //

      if(wasm_result.extra.targets & COMPILETARGETS_JS) {
        wasm_options.target = 1; // CKF_KEYMANWEB TODO use COMPILETARGETS_JS

        // We always want debug data in the intermediate .kmx, so that error
        // messages from KMW compiler can give line numbers in .kmn. This
        // should have no impact on the final .js if options.debug is false
        wasm_options.saveDebug = true;

        wasm_result = Module.kmcmp_compile(infile, wasm_options, wasm_callbacks);
        if(!wasm_result.result) {
          return null;
        }
        const kmw_result: KmnCompilerResult = this.copyWasmResult(wasm_result);
        kmw_result.displayMap = result.displayMap; // we can safely re-use the kmx compile displayMap

        const web_kmx = this.copyWasmBuffer(wasm_result.kmx, wasm_result.kmxSize);
        result.artifacts.js = this.runWebCompiler(infile, outfile, web_kmx, result.artifacts.kvk?.data, kmw_result, options);
        if(!result.artifacts.js) {
          return null;
        }
      }

      return result;
    } catch(e) {
      /* c8 ignore next 3 */
      this.callbacks.reportMessage(KmnCompilerMessages.Fatal_UnexpectedException({e:e}));
      return null;
    } finally {
      if(wasm_result) {
        wasm_result.delete();
      }
      wasm_callbacks.delete();
      wasm_options.delete();
    }
  }

  private runWebCompiler(
    kmnFilename: string,
    kmxFilename: string,
    web_kmx: Uint8Array,
    kvk: Uint8Array,
    kmxResult: KmnCompilerResult,
    options: CompilerOptions
  ): KeymanCompilerArtifact {
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
    const data = this.callbacks.loadFile(kvksFilename);
    if(!data) {
      this.callbacks.reportMessage(KmnCompilerMessages.Error_FileNotFound({filename: kvksFilename}));
      return null;
    }

    const filename = this.callbacks.path.basename(kvksFilename);
    let basename = null;
    let vk: VisualKeyboard.VisualKeyboard = null;
    if(filename.endsWith('.kvk')) {
      /* Legacy keyboards may reference a binary .kvk. That's not an error */
      // TODO: (lowpri) add hint to convert to .kvks?
      basename = this.callbacks.path.basename(kvksFilename, KeymanFileTypes.Binary.VisualKeyboard);
      const reader = new KvkFileReader();
      try {
        vk = reader.read(data);
      } catch(e) {
        this.callbacks.reportMessage(KmnCompilerMessages.Error_InvalidKvkFile({filename, e}));
        return null;
      }
    } else {
      basename = this.callbacks.path.basename(kvksFilename, KeymanFileTypes.Source.VisualKeyboard);
      const reader = new KvksFileReader();
      let kvks = null;
      try {
        kvks = reader.read(data);
        reader.validate(kvks);
      } catch(e) {
        this.callbacks.reportMessage(KmnCompilerMessages.Error_InvalidKvksFile({filename, e}));
        return null;
      }
      let invalidVkeys: string[] = [];
      vk = reader.transform(kvks, invalidVkeys);
      for(let invalidVkey of invalidVkeys) {
        this.callbacks.reportMessage(KmnCompilerMessages.Warn_InvalidVkeyInKvksFile({filename, invalidVkey}));
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
      if(!data) {
        this.callbacks.reportMessage(KmnCompilerMessages.Error_FileNotFound({filename: displayMapFilename}));
        return null;
      }
      const mapping = JSON.parse(new TextDecoder().decode(data));
      return Osk.parseMapping(mapping);
    } catch(e) {
      this.callbacks.reportMessage(KmnCompilerMessages.Error_InvalidDisplayMapFile({filename: displayMapFilename, e}));
      return null;
    }
  }

  /**
   * @internal
   * Generates an exception in kmcmplib to verify that Sentry error capture is
   * working correctly
   */
  public testSentry() {
    if(!this.verifyInitialized()) {
      return null;
    }

    return Module.kmcmp_testSentry();
  }

  /**
   * @internal
   * convert `\u{1234}` to `\u1234` etc
   */
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
   * @internal
   * @param pattern    - UnicodeSet pattern such as `[a-z]`
   * @param rangeCount - number of ranges to allocate
   * @returns            UnicodeSet accessor object, or null on failure
   */
  public parseUnicodeSet(pattern: string, rangeCount: number) : UnicodeSet | null {
    if(!this.verifyInitialized()) {
      /* c8 ignore next 2 */
      // verifyInitialized will set a callback if needed
      return null;
    }

    if ((rangeCount * 2) < 0) {
      throw new RangeError(`Internal error: negative rangeCount * 2 = ${rangeCount * 2}`);
    }
    const buf = this.wasmExports.malloc(rangeCount * 2 * Module.HEAPU32.BYTES_PER_ELEMENT);
    if (buf <= 0) {
      // out of memory will return zero.
      throw new RangeError(`Internal error: wasm malloc() returned ${buf}`);
    }
    // fix \u1234 pattern format
    pattern = KmnCompiler.fixNewPattern(pattern);
    const rc = Module.kmcmp_parseUnicodeSet(pattern, buf, rangeCount * 2);
    if (rc >= 0) {
      // If >= 0: it's a range count (which could be zero, an empty set).
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
      // rc is negative: it's an error code.
      this.wasmExports.free(buf);
      // translate error code into callback
      this.callbacks.reportMessage(getUnicodeSetError(rc));
      return null;
    }
  }

  /**
   * @internal
   */
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
       return KmnCompilerMessages.Error_UnicodeSetSyntaxError();
    case KMCMP_ERROR_HAS_STRINGS:
    return KmnCompilerMessages.Error_UnicodeSetHasStrings();
    case KMCMP_ERROR_UNSUPPORTED_PROPERTY:
       return KmnCompilerMessages.Error_UnicodeSetHasProperties();
    case KMCMP_FATAL_OUT_OF_RANGE:
      return KmnCompilerMessages.Fatal_UnicodeSetOutOfRange();
    default:
      /* c8 ignore next */
      return KmnCompilerMessages.Fatal_UnexpectedException({e: `Unexpected UnicodeSet error code ${rc}`});
  }
}

