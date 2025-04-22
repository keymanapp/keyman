/*
 * Keyman is 2025 copyright (C) SIL International. MIT License.
 *
 * Read macOS/Ukelele .keylayout files
 *
 */

import { CompilerCallbacks } from "@keymanapp/developer-utils";
import { XMLParser } from 'fast-xml-parser';
import { util } from '@keymanapp/common-types';
import { KeylayoutToKmnConverter } from './keylayout-to-kmn-converter.js';
import { ConverterMessages } from '../converter-messages.js';
import boxXmlArray = util.boxXmlArray;

export class KeylayoutFileReader {

  constructor(private callbacks: CompilerCallbacks /*,private options: CompilerOptions*/) { };



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

    let xmlFile;
    let jsonObj = [];

    const options = {
      ignoreAttributes: false,
      trimValues: false,           // preserve spaces
      attributeNamePrefix: '@_'    // to access the attribute
    };

    try {
      xmlFile = this.callbacks.fs.readFileSync(this.callbacks.path.join(process.cwd(), KeylayoutToKmnConverter.TEST_SUBFOLDER, KeylayoutToKmnConverter.DATA_SUBFOLDER, this.callbacks.path.basename(inputFilename)), 'utf8');
      const parser = new XMLParser(options);
      jsonObj = parser.parse(xmlFile);       // get plain Object
      this.boxArray(jsonObj.keyboard);       // jsonObj now contains arrays; no single fields
    }
    catch (err) {
      this.callbacks.reportMessage(ConverterMessages.Error_FileNotFound({ inputFilename }));
      return null;
    }
    return jsonObj;
  }
}


