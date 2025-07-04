/*
 * Keyman is 2025 copyright (C) SIL International. MIT License.
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

// _S2 which folder should keylayout-xml.js live??
import { KeylayoutXMLSourceFile } from '../../../common/web/utils/src/types/keylayout/keylayout-xml.js';

import boxXmlArray = util.boxXmlArray;

export class KeylayoutFileReader {

  constructor(private callbacks: CompilerCallbacks /*,private options: CompilerOptions*/) { };


  /**
   * @returns true if valid, false if invalid
   */

  public validate(source: KeylayoutXMLSourceFile): boolean {
    console.log("SchemaValidators.default.keylayout(source) ", SchemaValidators.default.keylayout(source));

    if (!SchemaValidators.default.keylayout(source)) {
      console.log("SchemaValidators.default.keylayout).errors ", (<any>SchemaValidators.default.keylayout).errors);

      for (const err of (<any>SchemaValidators.default.keylayout).errors) {
        this.callbacks.reportMessage(ConverterMessages.Error_SchemaValidationError({
          instancePath: err.instancePath,
          keyword: err.keyword,
          message: err.message || 'Unknown AJV Error', // docs say 'message' is optional if 'messages:false' in options
          params: Object.entries(err.params || {}).sort().map(([k, v]) => `${k}="${v}"`).join(' '),
        }));
      }
      return false;
    }
    return true;
  }


  /**
   * @brief  member function to box single-entry objects into arrays
   * @param  source the object to be changed
   * @return objects that contain only boxed arrays
   */
  public boxArray(source: any) {

    boxXmlArray(source.layouts, 'layout');

    //_S2 todo do not use remove_text in util.ts!!
    boxXmlArray(source?.modifierMap, 'keyMapSelect');
    for (const keyMapSelect of source?.modifierMap?.keyMapSelect) {
      boxXmlArray(keyMapSelect, 'modifier');
    }

    if (!Array.isArray(source?.keyMapSet) === false) {
      for (let i = 0; i < source?.keyMapSet.length; i++) {
        for (const keyMap of source?.keyMapSet[i]?.keyMap) {
          boxXmlArray(keyMap, 'key');
        }
        boxXmlArray(source.keyMapSet[i], 'keyMap');
      }
    } else {
      for (const keyMap of source?.keyMapSet?.keyMap) {
        boxXmlArray(keyMap, 'key');
      }
      boxXmlArray(source.keyMapSet, 'keyMap');
      // keyMapSet is the only top level tag that might occur several times
      boxXmlArray(source, 'keyMapSet');
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
   * @brief  member function to parse data from a .keylayout-file and store to a json object
   * @param  absolutefilename the ukelele .keylayout-file to be parsed
   * @return in case of success: json object containing data of the .keylayout file; else null
   */
  public read(inputFilename: string): KeylayoutXMLSourceFile {

    const options = {
      ignoreAttributes: [''],
      trimValues: false,            // we do not trim values because if we do we cannot process an output character of " " (whitespace)
      textNodeName: "#_text",       // #_text will be added for textnodes and will be removed later ( in remove_text() in boxXmlArray 
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
      this.callbacks.reportMessage(ConverterMessages.Error_FileNotFound({ inputFilename }));
      return null;
    }
  }
}


