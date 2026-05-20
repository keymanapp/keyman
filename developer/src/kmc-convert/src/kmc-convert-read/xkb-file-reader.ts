/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Read macOS/Ukelele .keylayout files
 *
 */

import { CompilerCallbacks/*, DeveloperUtilsMessages, Keylayout*/ } from "@keymanapp/developer-utils";
import { ConverterMessages } from '../converter-messages.js';

export class XkbFileReader {

  constructor(private callbacks: CompilerCallbacks /*,private options: CompilerOptions*/) { };

  /**
   * @brief  member function to parse data from a .keylayout-file and store in a json object
   *         we need to be able to ignore an output character of "", process an output character of " " (space) and allow surrounding whitespace in #text (which will be removed later)
   * @param  inputFilename the ukelele .keylayout-file to be parsed
   * @return in case of success: json object containing data of the .keylayout file; else null
   */
  public read(source: Uint8Array): string |null{

    try {
      const data = new TextDecoder().decode(source);

      // ToDo-kmc-convert double msg?

      if (!data) {
        this.callbacks.reportMessage(ConverterMessages.Error_UnableToRead());
        return null;
      }
      return data;
    }
    catch (err) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToRead());
      return null;
    }
  }
}
