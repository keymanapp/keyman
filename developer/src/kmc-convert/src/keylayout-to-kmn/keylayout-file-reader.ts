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

//.modifier[k]['@_keys']


export function remove_text(o: any, x: string): void {
console.log("ooooooooooooo",o);

console.log(" o.item ", o.layout);
console.log(" o.item ", o.layout['@_mapSet']);
//console.log(" o.item ", o.['@_first']);
console.log(" o.item", o['@_text'],"----", o['@_text']==="",o['@_text']==='\n        \n    ');


// assume tagName is the name of the tag you want to remove
//const tagsToRemove = o.getElementsByTagName('@_text');
//const tagsToRemove = o.layout['@_mapSet']
const tagsToRemove = o.layout
console.log("tagsToRemove ",/*tagsToRemove.length,*/tagsToRemove);

for (let i = 4 - 1; i >= 0; i -- ) {
tagsToRemove[i].parentNode.removeChild(tagsToRemove[i]);
}


if(o['@_text']==='\n        \n    ')
  o.remove(o['@_text'])

if(o.item[0]==="@'_first'")
  console.log("_first_first ",);



  if (typeof o == 'object' && !Array.isArray(o[x])) {
    if (o[x] === null || o[x] === undefined) {
      o[x] = [];
    }
    else {
      o[x] = [o[x]];
    }
  }

}



export class KeylayoutFileReader {

  constructor(private callbacks: CompilerCallbacks /*,private options: CompilerOptions*/) { };


  /**
   * @returns true if valid, false if invalid
   */

  //public validate(source: LDMLKeyboardXMLSourceFile | LDMLKeyboardTestDataXMLSourceFile): boolean {
  public validate(source: KeylayoutXMLSourceFile): boolean {
    // public validate(source: any): boolean {
    console.log("SchemaValidators.default.keylayout(source) ", SchemaValidators.default.keylayout(source));


    //SchemaValidators.default.keylayout(source);



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

//remove_text(source.layouts, 'layout')


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
  public read(inputFilename: string): KeylayoutXMLSourceFile {

    const options = {
      /*ignoreAttributes: false,
      attributeNamePrefix: '@_',   // to access the attribute
      ignoreDeclaration: true,
      parseTagValue: false,  // does this prevent output of  #text ????,
      // trimValues: false,           // preserve spaces
      trimValues: true,
      textNodeName: "##text",*/




       //ignoreAttributes: false,

     ignoreAttributes: [''],
      //ignoreAttributes: /^[ ]/,
      //trimValues: true,   // prevents '#text' but then cannot use ' ' as output character
      trimValues: false,  //_S2 was true !!!
      textNodeName: "#_text",
      parseTagValue: false,  // does this prevent output of  #text ????,
      attributeNamePrefix: '@_',   // to access the attribute
      ignoreDeclaration: true



  /*
      ignoreAttributes: false,
      //ignoreAttributes: ['#text', ''],
      //ignoreAttributes: (aName) => aName.startsWith('ou')  === 'tag.tag2'
      //ignoreAttributes: false,
     //ignoreAttributes: (/^[ ]/gm),
      //ignoreAttributes: [/^[ ]/gm],
      //ignoreAttributes: [/^[ ]/gm],
      //ignoreAttributes: ('#text', jPath) => aName.startsWith('#tex'),
      //trimValues: true,   // prevents '#text' but then cannot use ' ' as output character
      trimValues: false,  //_S2 was true !!!
      //textNodeName: "##text",
      parseTagValue: false,  // does this prevent output of  #text ????,
      attributeNamePrefix: '@_',   // to access the attribute
      ignoreDeclaration: true


  
  
  suppressBooleanAttributes?: boolean;
  allowBooleanAttributes?: boolean;
      stopNodes?: string[];
  allowBooleanAttributes?: boolean;
  
  cdataPropName?: false | string;
  tagValueProcessor?: (tagName: string, tagValue: string, jPath: string, hasAttributes: boolean, isLeafNode: boolean) => unknown;
*/
    };

    try {
      const xmlFile = this.callbacks.fs.readFileSync(inputFilename, 'utf8');
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


