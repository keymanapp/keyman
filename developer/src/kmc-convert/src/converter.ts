/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Infrastructure for keyboard source file conversion tools
 */
import {
  CompilerCallbacks,
  CompilerOptions,
  defaultCompilerOptions,
  KeymanCompiler,
  KeymanCompilerResult,
} from "@keymanapp/developer-utils";
import { ConverterClassFactory } from './converter-class-factory.js';
import { ConverterArtifacts } from "./converter-artifacts.js";
import { ConverterMessages } from "./converter-messages.js";

export interface ConverterResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link Converter.write}
   */
  artifacts: ConverterArtifacts;
};

/**
 * @public
 * Converts keyboards between different source file formats. The
 * compiler does not read or write from filesystem or network directly, but
 * relies on callbacks for all external IO.
 */
// _S2 method init: uses Interface CompilerCallbacks( loadfile,...)
// _S2 method run: uses Interface CompilerCallbacks( loadfile,...)
export class Converter implements KeymanCompiler {
  private callbacks: CompilerCallbacks;
  private options: CompilerOptions;

  /**
   * Initialize the converter. Copies options.
   * @param callbacks - Callbacks for external interfaces, including message
   *                    reporting and file io
   * @param options   - Compiler options
   * @returns           false if initialization fails
   */
  // _S2 fills interface this.callbacks with array (holding our data), filesize ect
  async init(callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {
    this.options = { ...options };
    this.callbacks = callbacks;
    return true;
  }

  /**
   * Converts a keyboard source file to another format. Returns an object
   * containing source artifacts on success. The files are passed in by name,
   * and the compiler will use callbacks as passed to the {@link Converter.init}
   * function to read any input files by disk.
   * @param infile  - Path to source file.
   * @param outfile - Path to output file. The file will not be written to, but
   *                  will be included in the result for use by
   *                  {@link Converter.write}.
   * @returns         Source artifacts on success, null on failure.
   */
  // _S2 this is Base class of all converters
  // _S2 check for things( file available,...) then
  // _S2 finds converter e.g.keylayout->kmn  ( uses converter-class-factory)
  // _S2 factory uses/instanciates chiled class ( ~ in C++ virtual function in base class <-> use fun of derived class)
  // _S2 loads file
  // _S2 creates a new converter (-object)
  // _S2 runs conversion for this object ( run-method of this converter of keylayout-to-kmn-tonverter.ts  )
  async run(inputFilename: string, outputFilename?: string): Promise<ConverterResult> {
console.log('use run of converter.ts')
    const converterOptions: CompilerOptions = {
      ...defaultCompilerOptions,
      ...this.options,
    };

    if(!outputFilename) {
      this.callbacks.reportMessage(ConverterMessages.Error_OutputFilenameIsRequired());
      return null;
    }

    const ConverterClass = ConverterClassFactory.find(inputFilename, outputFilename);
    if(!ConverterClass) {
      this.callbacks.reportMessage(ConverterMessages.Error_NoConverterFound({inputFilename, outputFilename}));
      return null;
    }

    const binaryData = this.callbacks.loadFile(inputFilename);
    if(!binaryData) {
      this.callbacks.reportMessage(ConverterMessages.Error_FileNotFound({inputFilename}));
      return null;
    }

    const converter = new ConverterClass(this.callbacks, converterOptions);
    const artifacts = await converter.run(inputFilename, outputFilename);
    // Note: any subsequent errors in conversion will have been reported by the converter
    return artifacts ? { artifacts } : null;
  }

  /**
   * Write artifacts from a successful compile to disk, via callbacks methods.
   * The artifacts written may include:
   *
   * - .kmn file - source keyboard used by Keyman on desktop platforms
   * - .kvks file - source on screen keyboard used by Keyman on desktop platforms
   * - .keyman-touch-layout file - source touch layout keyboard for touch platforms
   * - other keyboard source files as implemented
   *
   * @param artifacts - object containing artifact data to write out
   * @returns true on success
   */
  async write(artifacts: ConverterArtifacts): Promise<boolean> {
    for(const key of Object.keys(artifacts)) {
      if(artifacts[key]) {
        this.callbacks.fs.writeFileSync(artifacts[key].filename, artifacts[key].data);
      }
    }

    return true;
  }
}

