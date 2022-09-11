import KMXPlusFile, { Elem, Strs } from '../kmx/kmx-plus';
import LDMLKeyboardXMLSourceFile from '../ldml-keyboard/ldml-keyboard-xml';
import LDMLKeyboardXMLSourceFileReader from '../ldml-keyboard/ldml-keyboard-xml-reader';
import { BkspCompiler } from './bksp';
import CompilerCallbacks from './callbacks';
import { KeysCompiler } from './keys';
import { LocaCompiler } from './loca';
import { MetaCompiler } from './meta';
import { NameCompiler } from './name';
import { OrdrCompiler } from './ordr';
import { FinlCompiler, TranCompiler } from './tran';
import { VkeyCompiler } from './vkey';

const SECTION_COMPILERS = [
  BkspCompiler,
  KeysCompiler,
  FinlCompiler,
  LocaCompiler,
  MetaCompiler,
  NameCompiler,
  OrdrCompiler,
  TranCompiler,
  VkeyCompiler
];

export default class Compiler {
  private readonly callbacks: CompilerCallbacks;

  constructor (callbacks: CompilerCallbacks) {
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
    const source = reader.loadFile(filename);
    return source ?? null;
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
    kmx.kmxplus.strs = new Strs();
    kmx.kmxplus.elem = new Elem(kmx.kmxplus.strs);

    for(let section of sections) {
      if(!section.validate()) {
        passed = false;
        // We'll keep validating other sections anyway, so we get a full set of
        // errors for the keyboard developer.
        continue;
      }
      const sect = section.compile({strs: kmx.kmxplus.strs, elem: kmx.kmxplus.elem});

      /* istanbul ignore if */
      if(!sect) {
        // This should not really happen -- validate() should be telling us
        // if something is going to fail to compiler
        passed = false;
        continue;
      }
      kmx.kmxplus[section.id] = sect as any;
    }

    return passed ? kmx : null;
  }
}

