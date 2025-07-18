/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Lists all the available converters and finds matching converter
 */
import { KeylayoutToKmnConverter } from './keylayout-to-kmn/keylayout-to-kmn-converter.js';

const converters = [
  KeylayoutToKmnConverter,
];

export class ConverterClassFactory {
  static find(inputFilename: string, outputFilename: string) {

    const converter = converters.find(c =>
      inputFilename.endsWith(c.INPUT_FILE_EXTENSION) &&
      outputFilename.endsWith(c.OUTPUT_FILE_EXTENSION)
    );

    return converter;
  }
}
