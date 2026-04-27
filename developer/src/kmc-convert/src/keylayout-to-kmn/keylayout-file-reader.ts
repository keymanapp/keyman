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
   * @brief  helper function to find a specific keyMap index in a keyMapSet
   * @param  jsonObj the read keylayout data to be checked
   * @param  keyMapSelect the keyMapSelect element to find in keyMapSet
   * @return true if the keyMapSet element is found, false if not
   */
  public findMapIndexinKeymap(jsonObj: any, keyMapSelect: any): boolean {
    for (const keyMapSet of jsonObj.keyboard.keyMapSet) {
      for (const keyMap of keyMapSet.keyMap) {
        if (keyMap['index'] === keyMapSelect) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * @brief  helper function to find a specific keyMapSelect index in a modifierMap
   * @param  jsonObj the read keylayout data to be checked
   * @param  keyMap the keyMap element to find in modifierMap
   * @return true if the keyMap element is found, false if not
   */
  public findIndexinKeymapSelect(jsonObj: any, keyMap: any): boolean {
    for (const modifierMap of jsonObj.keyboard.modifierMap) {
      for (const keyMapSelect of modifierMap.keyMapSelect) {
        if (keyMapSelect['mapIndex'] === keyMap) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * @brief  member function checking if all keyMapSelect elements have a corresponding keyMap
   * element in the .keylayout file (if not, the .keylayout file is invalid and will not be converted)
   * see TN2056 (https://developer.apple.com/library/archive/technotes/tn2056/_index.html#//apple_ref/doc/uid/DTS10003085-CH1-SUBSECTION7)
   * @param  jsonObj the read keylayout data to be checked
   * @return true if all keyMapSelect elements have a corresponding keyMap element, false if not
   */
  public checkForCorrespondingElements(jsonObj: any): boolean {
    let available = true;

    // check if all keyMapSelect elements have a corresponding keyMap element in the .keylayout file
    for (const modifierMap of jsonObj.keyboard.modifierMap) {
      for (const keyMapSelect of modifierMap.keyMapSelect) {
        available = available && this.findMapIndexinKeymap(jsonObj, keyMapSelect['mapIndex']);
      }
    }
    // check if all keyMap elements have a corresponding keyMapSelect element in the .keylayout file
    for (const keyMapSet of jsonObj.keyboard.keyMapSet) {
      for (const keyMap of keyMapSet.keyMap) {
        available = available && this.findIndexinKeymapSelect(jsonObj, keyMap['index']);
      }
    }
    return available;
  }


  /**
   * @returns true if valid, false if invalid
   */
  public validate(source: Keylayout.KeylayoutXMLSourceFile, inputFilename: string): boolean {
    if (!SchemaValidators.default.keylayout(source)) {
      for (const err of (<any>SchemaValidators.default.keylayout).errors) {
        this.callbacks.reportMessage(DeveloperUtilsMessages.Error_InvalidXml({
          e: err.instancePath
        }));
      }
      return false;
    }

    // check if all keyMapSelect elements have a corresponding keyMap element in the .keylayout file
    if (!this.checkForCorrespondingElements(source)) {
      this.callbacks.reportMessage(ConverterMessages.Error_InvalidFile({ errorText: inputFilename }));
      return null;
    }
    return true;
  }

  /**
   * @brief  member function to box single-entry objects into arrays
   * @param  source the object to be changed
   * @return object that contain only boxed arrays
   */
  public boxArray(source: any) {

    boxXmlArray(source, 'modifierMap');
    boxXmlArray(source, 'keyMapSet');

    if (source.layouts) {
      boxXmlArray(source.layouts, 'layout');
    }

    if (source.modifierMap) {
      for (const modifierMap of source.modifierMap) {
        boxXmlArray(modifierMap, 'keyMapSelect');
        if (modifierMap.keyMapSelect) {
          for (const keyMapSelect of modifierMap.keyMapSelect) {
            boxXmlArray(keyMapSelect, 'modifier');
          }
        }
      }
    }

    for (const keyMapSet of source.keyMapSet) {
      boxXmlArray(keyMapSet, 'keyMap');
      for (const keyMap of keyMapSet.keyMap) {
        boxXmlArray(keyMap, 'key');
      }
    }

    if (source.actions) {
      boxXmlArray(source.actions, 'action');
      for (const action of source.actions.action) {
        boxXmlArray(action, 'when');
      }
    }

    boxXmlArray(source?.terminators, 'when');
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

      if (!jsonObj?.keyboard) {
        this.callbacks.reportMessage(ConverterMessages.Error_UnableToParse());
        return null;
      }
      this.boxArray(jsonObj.keyboard);            // jsonObj now contains arrays; no single fields
      return jsonObj;
    }
    catch (err) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToRead());
      return null;
    }
  }
}
