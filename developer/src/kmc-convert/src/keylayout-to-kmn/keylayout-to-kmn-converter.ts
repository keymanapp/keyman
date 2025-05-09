/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Converts macOS/Ukelele .keylayout files to Keyman .kmn
 */
import { CompilerCallbacks, CompilerOptions } from "@keymanapp/developer-utils";
import { ConverterToKmnArtifacts } from "../converter-artifacts.js";

export class KeylayoutToKmnConverter {
  static readonly INPUT_FILE_EXTENSION = '.keylayout';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';

  constructor(/*private*/ _callbacks: CompilerCallbacks, /*private*/ _options: CompilerOptions) {
    // TODO: if these are needed, uncomment /*private*/ and remove _, and they will then
    // be available as class properties
  }

  async run(inputFilename: string, outputFilename: string, binaryData: Uint8Array): Promise<ConverterToKmnArtifacts> {
    if(!inputFilename || !outputFilename || !binaryData) {
      throw new Error('Invalid parameters');
    }

    console.error('TODO: implement KeylayoutToKmnConverter');

    return null;
  }
}