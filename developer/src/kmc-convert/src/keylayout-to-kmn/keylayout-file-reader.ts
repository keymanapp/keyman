/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Read macOS/Ukelele .keylayout files
 *
 */

import { CompilerCallbacks } from "@keymanapp/developer-utils";
import { XMLParser } from 'fast-xml-parser';
import { util } from '@keymanapp/common-types';
import { ConverterMessages } from '../converter-messages.js';
import { SchemaValidators } from '@keymanapp/common-types';
import { DeveloperUtilsMessages } from '@keymanapp/developer-utils';
import { KeylayoutXMLSourceFile } from "@keymanapp/developer-utils";

import boxXmlArray = util.boxXmlArray;

export class KeylayoutFileReader {

  constructor(private callbacks: CompilerCallbacks /*,private options: CompilerOptions*/) { };

  /**
   * @returns true if valid, false if invalid
   */

  public validate(source: KeylayoutXMLSourceFile): boolean {
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
   * If object contains attribute #text it will be removed.
   * @param o Object with possible property #text containing whitespaces
   * @return objects that do not contain property #text
   */
  public remove_whitespace(o: any): void {
    if (o['#text']) {
      delete o['#text'];
    }
  }

  /**
   * @brief wrapper to remove whitespace and box single-entry objects into arrays
   * @param o Object with property to box/remove whitespaces from
   * @param x Name of element to box
   * @return objects that contain only boxed arrays
   */
  public removeWhitespace_boxArray(o: any, x: string): void {

    this.remove_whitespace(o);
    boxXmlArray(o, x);
  }

  /**
   * @brief  member function to box single-entry objects into arrays
   * @param  source the object to be changed
   * @return object that contain only boxed arrays
   */
  public boxArray(source: any) {

    this.remove_whitespace(source);

    this.removeWhitespace_boxArray(source, 'keyMapSet');

    this.removeWhitespace_boxArray(source.layouts, 'layout');
    this.removeWhitespace_boxArray(source?.modifierMap, 'keyMapSelect');

    for (const keyMapSelect of source?.modifierMap?.keyMapSelect) {
      this.removeWhitespace_boxArray(keyMapSelect, 'modifier');
    }

    this.removeWhitespace_boxArray(source.keyMapSet[0], 'keyMap');

    for (const keyMapSet of source?.keyMapSet) {
      for (const keyMap of keyMapSet.keyMap) {
        this.removeWhitespace_boxArray(keyMap, 'key');
      }
      this.removeWhitespace_boxArray(keyMapSet, 'keyMap');
    }

    this.removeWhitespace_boxArray(source?.actions, 'action');
    for (const action of source?.actions?.action) {
      this.removeWhitespace_boxArray(action, 'when');
    }

    this.removeWhitespace_boxArray(source.terminators, 'when');
    for (const action of source?.actions?.action) {
      this.removeWhitespace_boxArray(action, 'when');
    }

    return source;
  }

  /**
   * @brief  member function to parse data from a .keylayout-file and store in a json object
   *         we need to be able to ignore an output character of "", process an output character of " " (space) and allow surrounding whitespace in #text (which will be removed later)
   * @param  inputFilename the ukelele .keylayout-file to be parsed
   * @return in case of success: json object containing data of the .keylayout file; else null
   */
  public read(inputFilename: string): KeylayoutXMLSourceFile {

    const options = {
      ignoreAttributes: [''],       // we do not process an output character of ""
      trimValues: false,            // we do not trim values because if we do we cannot process an output character of " " (space)
      parseTagValue: false,
      attributeNamePrefix: '@_',    // to access the attribute
      ignoreDeclaration: true
    };

    try {
      const xmlFile = this.callbacks.fs.readFileSync(inputFilename, 'utf8');
      const parser = new XMLParser(options);
      const jsonObj = parser.parse(xmlFile);      // get plain Object
      this.boxArray(jsonObj.keyboard);            // jsonObj now contains arrays; no single fields
      return jsonObj;
    }
    catch (err) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToRead({ inputFilename }));
      return null;
    }
  }
}
