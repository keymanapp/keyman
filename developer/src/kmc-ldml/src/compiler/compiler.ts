/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Compiles a LDML XML keyboard file into a Keyman KMXPlus file
 */
import { KMXPlus, UnicodeSetParser, KvkFileWriter, KMX } from '@keymanapp/common-types';
import {
  CompilerCallbacks, KeymanCompiler, KeymanCompilerResult, KeymanCompilerArtifacts,
  defaultCompilerOptions, LDMLKeyboardXMLSourceFileReader, LDMLKeyboard,
  LDMLKeyboardTestDataXMLSourceFile, KMXBuilder,
  KeymanCompilerArtifactOptional
} from "@keymanapp/developer-utils";
import { LdmlCompilerOptions } from './ldml-compiler-options.js';
import { LdmlCompilerMessages } from './ldml-compiler-messages.js';
import { BkspCompiler, TranCompiler } from './tran.js';
import { DispCompiler } from './disp.js';
import { KeysCompiler } from './keys.js';
import { LayrCompiler } from './layr.js';
import { LocaCompiler } from './loca.js';
import { MetaCompiler } from './meta.js';
import { VarsCompiler } from './vars.js';
import { StrsCompiler, ElemCompiler, ListCompiler, UsetCompiler } from './empty-compiler.js';


import LDMLKeyboardXMLSourceFile = LDMLKeyboard.LDMLKeyboardXMLSourceFile;
import KMXPlusFile = KMXPlus.KMXPlusFile;
import DependencySections = KMXPlus.DependencySections;
import { SectionIdent, constants } from '@keymanapp/ldml-keyboard-constants';
import { KmnCompiler } from '@keymanapp/kmc-kmn';
import { KMXPlusMetadataCompiler } from './metadata-compiler.js';
import { LdmlKeyboardVisualKeyboardCompiler } from './visual-keyboard-compiler.js';
import { LinterKeycaps } from './linter-keycaps.js';
//KMW17.0: import { LdmlKeyboardKeymanWebCompiler } from './keymanweb-compiler.js';

export const SECTION_COMPILERS = [
  // These are in dependency order.

  // First the former 'global' sections
  StrsCompiler,
  // meta depends on strs, but potentially needed by anything else using strs
  MetaCompiler,
  ListCompiler,
  ElemCompiler,
  UsetCompiler,
  // Next, Vars, which depends on others
  VarsCompiler,
  // Now all others:
  BkspCompiler,
  DispCompiler,
  KeysCompiler,
  LayrCompiler,
  LocaCompiler,
  TranCompiler,
];

/** list of linters, in order. */
const LINTERS = [
  LinterKeycaps,
];

/**
 * @public
 * Internal in-memory build artifacts from a successful compilation
 */
export interface LdmlKeyboardCompilerArtifacts extends KeymanCompilerArtifacts {
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

export interface LdmlKeyboardCompilerResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link LdmlKeyboardCompiler.write}
   */
  artifacts: LdmlKeyboardCompilerArtifacts;
};

/**
 * @public
 * Compiles a LDML keyboard .xml file to a .kmx (KMXPlus), .kvk, and/or .js. The
 * compiler does not read or write from filesystem or network directly, but
 * relies on callbacks for all external IO.
 */
export class LdmlKeyboardCompiler implements KeymanCompiler {
  private callbacks: CompilerCallbacks;
  private options: LdmlCompilerOptions;

  // uset parser
  private usetparser?: UnicodeSetParser = undefined;

  /**
   * Initialize the compiler, including loading the WASM host for uset parsing.
   * Copies options.
   * @param callbacks - Callbacks for external interfaces, including message
   *                    reporting and file io
   * @param options   - Compiler options
   * @returns           false if initialization fails
   */
  async init(callbacks: CompilerCallbacks, options: LdmlCompilerOptions): Promise<boolean> {
    this.options = { ...options };
    this.callbacks = callbacks;
    return true;
  }

  /**
   * Compiles a LDML keyboard .xml file to .kmx, .kvk, and/or .js files. Returns
   * an object containing binary artifacts on success. The files are passed in
   * by name, and the compiler will use callbacks as passed to the
   * {@link LdmlKeyboardCompiler.init} function to read any input files by disk.
   * @param infile  - Path to source file.
   * @param outfile - Path to output file. The file will not be written to, but
   *                  will be included in the result for use by
   *                  {@link LdmlKeyboardCompiler.write}.
   * @returns         Binary artifacts on success, null on failure.
   */
  async run(inputFilename: string, outputFilename?: string): Promise<LdmlKeyboardCompilerResult> {

    let compilerOptions: LdmlCompilerOptions = {
      ...defaultCompilerOptions,
      ...this.options,
    };

    let source = this.load(inputFilename);
    if (!source) {
      return null;
    }
    let kmx = await this.compile(source);
    if (!kmx) {
      return null;
    }

    outputFilename = outputFilename ?? inputFilename.replace(/\.xml$/, '.kmx');

    // In order for the KMX file to be loaded by non-KMXPlus components, it is helpful
    // to duplicate some of the metadata
    KMXPlusMetadataCompiler.addKmxMetadata(kmx.kmxplus, kmx.keyboard, compilerOptions);

    // Use the builder to generate the binary output file
    const kmxBuilder = new KMXBuilder(kmx, compilerOptions.saveDebug);
    const keyboardId = this.callbacks.path.basename(outputFilename, '.kmx');
    const vkCompiler = new LdmlKeyboardVisualKeyboardCompiler(this.callbacks);
    const vkCompilerResult = vkCompiler.compile(kmx.kmxplus, keyboardId);
    if(vkCompilerResult === null) {
      return null;
    }
    const vkData = typeof vkCompilerResult == 'object' ? vkCompilerResult : null;

    if(vkData) {
      kmx.keyboard.stores.push({
        dpName: '',
        dpString: keyboardId + '.kvk',
        dwSystemID: KMX.KMXFile.TSS_VISUALKEYBOARD
      });
    }

    const kmxBinary = kmxBuilder.compile();

    const kvkWriter = new KvkFileWriter();
    const kvkBinary = vkData ? kvkWriter.write(vkData) : null;

    // Note: we could have a step of generating source files here
    // KvksFileWriter()...
    // const tlcompiler = new kmc.TouchLayoutCompiler();
    // const tl = tlcompiler.compile(source);
    // const tlwriter = new TouchLayoutFileWriter();

    //KMW17.0: const kmwcompiler = new LdmlKeyboardKeymanWebCompiler(this.callbacks, compilerOptions);
    //KMW17.0: const kmw_string = kmwcompiler.compile(inputFilename, source);
    //KMW17.0: const encoder = new TextEncoder();
    //KMW17.0: const kmw_binary = encoder.encode(kmw_string);

    return {
      artifacts: {
        kmx: { data: kmxBinary, filename: outputFilename },
        kvk: kvkBinary ? { data: kvkBinary, filename: outputFilename.replace(/\.kmx$/, '.kvk') } : null,
        //KMW17.0: js: { data: kmw_binary, filename: outputFilename.replace(/\.kmx$/, '.js') },
      }
    };
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
  async write(artifacts: LdmlKeyboardCompilerArtifacts): Promise<boolean> {
    if (artifacts.kmx) {
      this.callbacks.fs.writeFileSync(artifacts.kmx.filename, artifacts.kmx.data);
    }

    if (artifacts.kvk) {
      this.callbacks.fs.writeFileSync(artifacts.kvk.filename, artifacts.kvk.data);
    }

    if (artifacts.js) {
      this.callbacks.fs.writeFileSync(artifacts.js.filename, artifacts.js.data);
    }

    return true;
  }

  /**
   * @internal
   * Construct or return a UnicodeSetParser, aka KmnCompiler
   * @returns the held UnicodeSetParser
   */
  async getUsetParser(): Promise<UnicodeSetParser> {
    if (this.usetparser === undefined) {
      // initialize
      const compiler = new KmnCompiler();
      const ok = await compiler.init(this.callbacks, null);
      if (ok) {
        this.usetparser = compiler;
      } else {
        /* c8 ignore next 2 */
        this.usetparser = null; // Store null on failure
      }
    }
    return this.usetparser;
  }

  private buildSections(source: LDMLKeyboardXMLSourceFile) {
    return SECTION_COMPILERS.map(c => new c(source, this.callbacks));
  }

  /**
   * @internal
   * Loads a LDML Keyboard xml file and compiles into in-memory xml
   * structures.
   * @param filename - input filename, will use callback to load from disk
   * @returns          the source file, or null if invalid
   */
  public load(filename: string): LDMLKeyboardXMLSourceFile | null {
    const reader = new LDMLKeyboardXMLSourceFileReader(this.options.readerOptions, this.callbacks);
    // load the file from disk into a string
    const data = this.callbacks.loadFile(filename);
    if (!data) {
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidFile({ errorText: 'Unable to read XML file' }));
      return null;
    }
    // parse (load) the string into an object tree
    const source = reader.load(data);
    if (!source) {
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidFile({ errorText: 'Unable to load XML file' }));
      return null;
    }
    try {
      // validate the object tree against the .xsd schema
      if (!reader.validate(source)) {
        return null;
      }
    } catch (e) {
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidFile({ errorText: e.toString() }));
      return null;
    }

    return source;
  }

  /**
   * @internal
   * Loads a LDML Keyboard test data xml file and compiles into in-memory xml
   * structures.
   * @param filename - input filename, will use callback to load from disk
   * @returns          the source file, or null if invalid
   */
  public loadTestData(filename: string): LDMLKeyboardTestDataXMLSourceFile | null {
    const reader = new LDMLKeyboardXMLSourceFileReader(this.options.readerOptions, this.callbacks);
    const data = this.callbacks.loadFile(filename);
    if (!data) {
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidFile({ errorText: 'Unable to read XML file' }));
      return null;
    }
    const source = reader.loadTestData(data);
    /* c8 ignore next 4 */
    if (!source) {
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidFile({ errorText: 'Unable to load XML file' }));
      return null;
    }
    // TODO-LDML: The unboxed data doesn't match the schema anymore. Skipping validation, for now.

    // try {
    //   if (!reader.validate(source)) {
    //     return null;
    //   }
    // } catch(e) {
    //   this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidFile({errorText: e.toString()}));
    //   return null;
    // }

    return source;
  }

  /** Materialize the linters against the built datafile */
  private buildLinters(source: LDMLKeyboardXMLSourceFile, kmx: KMXPlus.KMXPlusFile) {
    return LINTERS.map(c => new c(source, kmx, this.callbacks));
  }

  /**
   * Runs any linter steps, adding hints to the callbacks as needed
   * @internal
   * @returns true unless there was a linter failure.
   */
  private async lint(source: LDMLKeyboardXMLSourceFile, kmx: KMXPlus.KMXPlusFile): Promise<boolean> {
    if (!kmx || !source) {
      return false;
    }
    // run each of the linters
    for (const linter of this.buildLinters(source, kmx)) {
      if (!await linter.lint()) {
        return false;
      }
    }
    return true;
  }


  /**
   * @internal
   * Validates that the LDML keyboard source file and lints.
   * @param   source - in-memory representation of LDML keyboard xml file
   * @returns          true if the file validates
   */
  public async validate(source: LDMLKeyboardXMLSourceFile): Promise<boolean> {
    // We need to compile in order to validate.
    const kmx = await this.compile(source, true);
    if (!kmx) {
      return false;
    }

    // Run the linters
    return (await this.lint(source, kmx));
  }

  /**
   * @internal
   * Transforms in-memory LDML keyboard xml file to an intermediate
   * representation of a .kmx file.
   * @param   source - in-memory representation of LDML keyboard xml file
   * @returns          KMXPlusFile intermediate file
   */
  public async compile(source: LDMLKeyboardXMLSourceFile, postValidate?: boolean): Promise<KMXPlus.KMXPlusFile> {
    const sections = this.buildSections(source);
    let passed = true;

    const kmx = new KMXPlusFile();

    for (let section of sections) {
      if (!section.validate()) {
        // TODO-LDML: coverage
        passed = false;
        // We'll keep validating other sections anyway, so we get a full set of
        // errors for the keyboard developer.
        continue;
      }
      // clone
      const globalSections: DependencySections = Object.assign({}, kmx.kmxplus);
      // pre-initialize the usetparser
      globalSections.usetparser = await this.getUsetParser();
      const dependencies = section.dependencies;
      let dependencyProblem = false;
      Object.keys(constants.section).forEach((sectstr: string) => {
        const sectid: SectionIdent = constants.section[<SectionIdent>sectstr];
        if (dependencies.has(sectid)) {
          /* c8 ignore next 4 */
          if (!kmx.kmxplus[sectid]) {
            if (passed) {
              // Internal error useful during section bring-up
              throw new Error(`Internal error: section ${section.id} depends on uninitialized dependency ${sectid}, check ordering`);
            } else {
              dependencyProblem = true;
              return; // Already failed to validate, so no need for the layering message.
            }
          }
        } else {
          // delete dependencies that aren't referenced
          delete globalSections[sectid];
        }
      });
      if (dependencyProblem && !passed) {
        // Some layering problem, but we've already noted an error (!passed).
        // Just skip this section.
        continue;
      }
      const sect = section.compile(globalSections);

      if (!sect) {
        passed = false;
        continue;
      }
      /* c8 ignore next 4 */
      if (kmx.kmxplus[section.id]) {
        // Internal error useful during section bring-up
        throw new Error(`Internal error: section ${section.id} would be assigned twice`);
      }
      kmx.kmxplus[section.id] = sect as any;
    }

    // give all sections a chance to postValidate
    if (postValidate) {
      for (let section of sections) {
        if (!section.postValidate(kmx.kmxplus[section.id])) {
          passed = false;
        }
      }
    }

    return passed ? kmx : null;
  }
}

