/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Infrastructure for keyboard source file conversion tools
 */
import {
  CompilerCallbacks,
  defaultCompilerOptions,
  KeymanCompiler
} from "@keymanapp/developer-utils";
import { ConverterClassFactory } from './converter-class-factory.js';
import { ConverterArtifacts, ConverterOptions, ConverterResult } from "./main.js";
import { ConverterMessages } from "./converter-messages.js";

/**
 * @public
 * Converts keyboards between different source file formats. The
 * compiler does not read or write from filesystem or network directly, but
 * relies on callbacks for all external IO.
 */

export class Converter implements KeymanCompiler {
  private callbacks: CompilerCallbacks;
  private options: ConverterOptions;

  /**
   * Initialize the converter. Copies options.
   * @param callbacks - Callbacks for external interfaces, including message
   *                    reporting and file io
   * @param options   - Compiler options
   * @returns           false if initialization fails
   */
  async init(callbacks: CompilerCallbacks, options: ConverterOptions): Promise<boolean> {
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
  async run(inputFilename: string, outputFilename?: string): Promise<ConverterResult> {
    const converterOptions: ConverterOptions = {
      ...defaultCompilerOptions,
      ...this.options,
    };

    if (!inputFilename) {
      throw new Error('Input filename is required');
    }

    const ConverterClass = ConverterClassFactory.find(inputFilename, converterOptions.target);
    if (!ConverterClass) {
      this.callbacks.reportMessage(ConverterMessages.Error_NoConverterFound({ inputFilename, outputFilename: converterOptions.target }));
      return null;
    }

    const converter = new ConverterClass(this.callbacks, converterOptions);
    const result = await converter.run(inputFilename, outputFilename);
    // Note: any subsequent errors in conversion will have been reported by the converter
    if(result) {
      if(!result.artifacts.defaultOutput || !result.artifacts.defaultOutput.filename) {
        throw new Error(`Expected artifacts.defaultOutput to be set; ${result.artifacts}`);
      }
    }
    return result ? result : null;
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
    for (const key of Object.keys(artifacts)) {
      if (key != 'defaultOutput' && artifacts[key]) {
        this.callbacks.fs.writeFileSync(artifacts[key].filename, artifacts[key].data);
      }
    }

    return true;
  }
}

