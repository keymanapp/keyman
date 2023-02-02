import { LDMLKeyboardXMLSourceFileReader, LDMLKeyboard, KMXPlus, CompilerCallbacks } from '@keymanapp/common-types';
import CompilerOptions from './compiler-options.js';
import { CompilerMessages } from './messages.js';
import { BkspCompiler, FinlCompiler, TranCompiler } from './tran.js';
import { DispCompiler } from './disp.js';
import { Key2Compiler } from './key2.js';
import { KeysCompiler } from './keys.js';
import { LayrCompiler } from './layr.js';
import { LocaCompiler } from './loca.js';
import { MetaCompiler } from './meta.js';
import { NameCompiler } from './name.js';
import { OrdrCompiler } from './ordr.js';
import { VkeyCompiler } from './vkey.js';

import LDMLKeyboardXMLSourceFile = LDMLKeyboard.LDMLKeyboardXMLSourceFile;
import KMXPlusFile = KMXPlus.KMXPlusFile;

const SECTION_COMPILERS = [
  BkspCompiler,
  DispCompiler,
  FinlCompiler,
  Key2Compiler,
  KeysCompiler,
  LayrCompiler,
  LocaCompiler,
  MetaCompiler,
  NameCompiler,
  OrdrCompiler,
  TranCompiler,
  VkeyCompiler,
];

export default class Compiler {
  private readonly callbacks: CompilerCallbacks;
  // private readonly options: CompilerOptions; // not currently used

  constructor (callbacks: CompilerCallbacks, _options?: CompilerOptions) {
    /*
    this.options = {
      debug: false,
      addCompilerVersion: true,
      ...options
    };
    */
    this.callbacks = callbacks;
  }

  private buildSections(source: LDMLKeyboardXMLSourceFile) {
    return SECTION_COMPILERS.map(c => new c(source, this.callbacks));
  }

  /**
   * Loads a LDML Keyboard xml file and compiles into in-memory xml
   * structures.
   * @param filename  input filename, will use callback to load from disk
   * @returns
   */
  public load(filename: string): LDMLKeyboardXMLSourceFile {
    const reader = new LDMLKeyboardXMLSourceFileReader(this.callbacks);
    const data = this.callbacks.loadFile(filename, filename);
    if(!data) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidFile({errorText: 'Unable to read XML file'}));
      return null;
    }
    const source = reader.load(data);
    if(!source) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidFile({errorText: 'Unable to load XML file'}));
      return null;
    }
    try {
      reader.validate(source, this.callbacks.loadLdmlKeyboardSchema());
    } catch(e) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidFile({errorText: e.toString()}));
      return null;
    }

    return source;
  }

  /**
   * Validates that the LDML keyboard source file and lints. Actually just
   * compiles the keyboard and returns `true` if everything is good...
   * @param     source
   * @returns   true if the file validates
   */
  public validate(source: LDMLKeyboardXMLSourceFile): boolean {
    return !!this.compile(source);
  }

  /**
   * Transforms in-memory LDML keyboard xml file to an intermediate
   * representation of a .kmx file.
   * @param   source  in-memory representation of LDML keyboard xml file
   * @returns         KMXPlusFile intermediate file
   */
  public compile(source: LDMLKeyboardXMLSourceFile): KMXPlusFile {
    const sections = this.buildSections(source);
    let passed = true;

    const kmx = new KMXPlusFile();

    // These two sections are required by other sections
    kmx.kmxplus.strs = new KMXPlus.Strs();
    kmx.kmxplus.elem = new KMXPlus.Elem(kmx.kmxplus.strs);
    kmx.kmxplus.list = new KMXPlus.List(kmx.kmxplus.strs);

    for(let section of sections) {
      if(!section.validate()) {
        passed = false;
        // We'll keep validating other sections anyway, so we get a full set of
        // errors for the keyboard developer.
        continue;
      }
      const sect = section.compile({strs: kmx.kmxplus.strs, elem: kmx.kmxplus.elem, list: kmx.kmxplus.list});

      /* istanbul ignore if */
      if(!sect) {
        // This should not happen -- validate() should have told us
        // if something is going to fail to compile
        this.callbacks.reportMessage(CompilerMessages.Fatal_SectionCompilerFailed({sect:section.id}));
        passed = false;
        continue;
      }
      kmx.kmxplus[section.id] = sect as any;
    }

    return passed ? kmx : null;
  }
}

