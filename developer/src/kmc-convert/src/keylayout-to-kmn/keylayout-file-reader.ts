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
import { KeylayoutToKmnConverter } from './keylayout-to-kmn-converter.js';
import { ConverterMessages } from '../converter-messages.js';
import { SchemaValidators } from '@keymanapp/common-types';
import boxXmlArray = util.boxXmlArray;

export class KeylayoutFileReader {

  constructor(private callbacks: CompilerCallbacks /*,private options: CompilerOptions*/) { };


  /**
   * @returns true if valid, false if invalid
   */
  public validate(source: any): boolean {
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
    boxXmlArray(source.terminators, 'when');
    boxXmlArray(source, 'keyMapSet');
    boxXmlArray(source.keyMapSet, 'keyMap');
    boxXmlArray(source.action, 'actions');

    boxXmlArray(source?.modifierMap, 'keyMapSelect');
    for (const keyMapSelect of source?.modifierMap?.keyMapSelect) {
      boxXmlArray(keyMapSelect, 'modifier');
    }
    boxXmlArray(source?.actions, 'action');
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
  public read(inputFilename: string): Object {


    const options = {
      ignoreAttributes: false,
      trimValues: false,           // preserve spaces
      attributeNamePrefix: '@_'    // to access the attribute
    };

    try {
      const xmlFile = this.callbacks.fs.readFileSync(this.callbacks.path.join(process.cwd(), KeylayoutToKmnConverter.TEST_SUBFOLDER, KeylayoutToKmnConverter.DATA_SUBFOLDER, this.callbacks.path.basename(inputFilename)), 'utf8');
      const parser = new XMLParser(options);
      const jsonObj = parser.parse(xmlFile);       // get plain Object
      this.boxArray(jsonObj.keyboard);       // jsonObj now contains arrays; no single fields
      return jsonObj;
    }
    catch (err) {
      this.callbacks.reportMessage(ConverterMessages.Error_FileNotFound({ inputFilename }));
      return null;
    }
  }
}


