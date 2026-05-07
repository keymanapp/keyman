/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Lists all the available converters and finds matching converter
 */
import { KeylayoutToKmnConverter } from './kmc-convert-convert/keylayout-to-kmn-converter.js';
import { XkbToKmnConverter } from './kmc-convert-convert/xkb-to-kmn-converter.js';

const converters = [
  KeylayoutToKmnConverter,
  XkbToKmnConverter,
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
