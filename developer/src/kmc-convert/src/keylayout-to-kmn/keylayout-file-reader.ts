/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Read macOS/Ukelele .keylayout files
 *
 */

import { CompilerCallbacks, DeveloperUtilsMessages, Keylayout, KeymanXMLReader } from "@keymanapp/developer-utils";
import { util, SchemaValidators } from '@keymanapp/common-types';
import { ConverterMessages } from '../converter-messages.js';
import boxXmlArray = util.boxXmlArray;

export class KeylayoutFileReader {

  constructor(private callbacks: CompilerCallbacks /*,private options: CompilerOptions*/) { };

  /**
   * @returns true if valid, false if invalid
   */
  public validate(source: Keylayout.KeylayoutXMLSourceFile): boolean {
    if (!SchemaValidators.default.keylayout(source)) {
      for (const err of (<any>SchemaValidators.default.keylayout).errors) {
        this.callbacks.reportMessage(DeveloperUtilsMessages.Error_InvalidXml({
          e: err.instancePath
        }));
      }
      return false;
    }
    return true;
  }

  /**
   * @brief  member function to box single-entry objects into arrays
   * @param  source the object to be changed
   * @return object that contain only boxed arrays
   */
  public boxArray(source: any) {

    boxXmlArray(source, 'keyMapSet');

    boxXmlArray(source.layouts, 'layout');
    boxXmlArray(source?.modifierMap, 'keyMapSelect');

    for (const keyMapSelect of source?.modifierMap?.keyMapSelect) {
      boxXmlArray(keyMapSelect, 'modifier');
    }

    boxXmlArray(source.keyMapSet[0], 'keyMap');

    for (const keyMapSet of source?.keyMapSet) {
      for (const keyMap of keyMapSet.keyMap) {
        boxXmlArray(keyMap, 'key');
      }
      boxXmlArray(keyMapSet, 'keyMap');
    }

    boxXmlArray(source?.actions, 'action');
    for (const action of source?.actions?.action) {
      boxXmlArray(action, 'when');
    }

    boxXmlArray(source.terminators, 'when');
    for (const action of source?.actions?.action) {
      boxXmlArray(action, 'when');
    }

    return source;
  }

  /**
   * @brief  member function to parse data from a .keylayout-file and store in a json object
   *         we need to be able to ignore an output character of "", process an output character of " " (space) and allow surrounding whitespace in #text (which will be removed later)
   * @param  inputFilename the ukelele .keylayout-file to be parsed
   * @return in case of success: json object containing data of the .keylayout file; else null
   */
  public read(source: Uint8Array): Keylayout.KeylayoutXMLSourceFile {

    try {
      const data = new TextDecoder().decode(source);
      const jsonObj = new KeymanXMLReader('keylayout').parse(data) as Keylayout.KeylayoutXMLSourceFile;
      this.boxArray(jsonObj.keyboard);            // jsonObj now contains arrays; no single fields
      return jsonObj;
    }
    catch (err) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToRead());
      return null;
    }
  }
}
