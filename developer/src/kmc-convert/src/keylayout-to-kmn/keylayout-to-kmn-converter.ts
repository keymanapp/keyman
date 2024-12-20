/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Converts macOS/Ukelele .keylayout files to Keyman .kmn
 */
import { CompilerCallbacks, CompilerOptions } from "@keymanapp/developer-utils";
import { ConverterToKmnArtifacts } from "../converter-artifacts.js";

/*import { util } from '@keymanapp/common-types'; //_S2
import boxXmlArray = util.boxXmlArray;*/
{
  //     TODO keylayout->kmn
  // OK  modifiers: The identifier of the <modifierMap> element to use for this range of hardware keyboard types.
  // OK  defaultIndex: The table number to use for modifier key combinations which are not explicitly specified by any <modifier> element within the <modifierMap>.
  //     write read, convert, write
  //     tests for 3 functions read write convert
  //     add data to object
  //     Use filter functions
  //     action/output:use filter etc to shorten func
  //     deadkeyables:use filter etc to shorten func
  //     dk-> for all action:use filter etc to shorten func
  //     remove unneccessary data from dataObject
  //     rename symbols
  // OK  remove part using kmn_key_Name1
  // OK  remove unnecceaasry map_UkeleleKC_To_kmn_Key_Name_Array_Position_n etc
  // OK  loop throught ANSI, JIS- at moment only use [keyMapSet_count] (keyMapSet_count=0)
  // OK  remove funcs at teh end
  //     import { makePathToFixture } from '../../test/helpers/index.js';
  // OK  Mapping 0->30  or 0->K_A-> missing entries in mapping 
  // OK  Replace any-types
  //     Several steps action-> action-> action->character ( not only  action->character)
  //     TODO waht about using actions twice in a row???
  //     Usable for all keylayout files
  //     Return conditions
  //     Use callbacks as for writeFileSync
  //     Tests throws
  //     Conditions NCAPS,OPT;...
  //     TODO move func outside of class
  //     Functions as object methods? 
  //     objects contain only used stuff READ in: -- out: only read arrays / CONVERT in: only read arrays out: return only to write arrays
  //     Use catch blocks for file read
  //     read:  answer :  + [K_A] > 'a'  is OK / TODO which format to use in output ?  + [K_A] > 'a' (character code)  or    + [K_A] > U+0061 (virt Keycode)
  //     read   TODO which stores?
  //     one entry vs several entry in tags
  //     false arrangement of tags
  //     no added NCAPS when first modifier in modifierMap does not contain "caps" or"caps?"
  //     use length to clear array instead of defining evetry time new (modifierMap_ONE_InKeymapSelect.length=0
  //     naming of for-loop var i,j,k?...
  //     warning if contrADICTING RULES E:G:   [ 'modifier_C0', 'SHIFT NCAPS', 'K_C', 'c', '0', '1' ],  vs [ 'modifier_C0', 'SHIFT NCAPS', 'K_C', 'C', '1', '0' ],
  //     print NCAPS as the first of the modifiers in create_kmn_modifier
}

import { XMLParser } from 'fast-xml-parser';  // for reading a file
import { readFileSync } from 'fs';
//import { writeFileSync } from "fs";           // for writing a file
//import * as fs from 'fs';   // what is this/do I need it? -  either import all or seperately like above


// todo use from elsewhere
function boxXmlArray_S(o: any, x: string): void {
  if (typeof o == 'object' && !Array.isArray(o[x])) {
    if (o[x] === null || o[x] === undefined) {
      o[x] = [];
    }
    else {
      o[x] = [o[x]];
    }
  }
}
/*
// example: 
  <AAA>
    <BBB id="b">
      <CCC directions="nw" keyId="A-grave" />
      <CCC directions="nw se" keyId="A-acute" />
      <CCC directions="e" keyId="A-caron" />
      <CCC directions="s" keyId="numeric" /> <!-- layer shifting B-->
    </BBB>
    <BBB id="a">
      <CCC directions="nw" keyId="a-grave" />
      <CCC directions="nw se" keyId="a-acute" />
      <CCC directions="e" keyId="a-caron" />
    </BBB>
  </AAA>

then write: 
if(source?.keyboard3?.AAA) {
  
  //          parent tag              child-tag
  boxXmlArray(source?.keyboard3?.AAA, 'BBB');
  for(const BBB_Var of source?.keyboard3?.AAA?.BBB) {
    boxXmlArray(BBB_Var, 'CCC');
  }
}
*/

// need to specify all elements!!!
// source = jsonObj.keyboard
function boxArrays_S(source: any) {
  //             parent tag     child-tag
  boxXmlArray_S(source.layouts, 'layout');
  boxXmlArray_S(source.terminators, 'when');
  boxXmlArray_S(source, 'keyMapSet');
  boxXmlArray_S(source.keyMapSet, 'keyMap');
  boxXmlArray_S(source.action, 'actions');

  boxXmlArray_S(source?.modifierMap, 'keyMapSelect');
  for (const keyMapSelect of source?.modifierMap?.keyMapSelect) {
    boxXmlArray_S(keyMapSelect, 'modifier');
  }
  boxXmlArray_S(source?.actions, 'action');
  for (const action of source?.actions?.action) {
    boxXmlArray_S(action, 'when');
  }
  return source;
}



export interface convert_object {
  name: string,                                                 // needed?? remove
  ArrayOf_Element_Layouts: string[],                            // needed?? I think no
  ArrayOf_Element_KeyOutput: Uint8Array[][],
  ArrayOf_Element_KeyAction: Uint8Array[][],
  ArrayOf_Element_ALL_ModifierMaps: string[],
  ArrayOf_Element_ModifierMaps_ALLKeyMapSelect: string[][],
  ArrayOf_Element_Terminators: Uint8Array[]                     // add terminators ( ^,´,`)
  ArrayOf_processed_VK_from_keylayout: (string | number)[][],
  ArrayOf_processed_dk: string[][],                             // add dk-mapping ( dk1 <-> '^' )
  ArrayOf_processed_deadkeyables: string[][],                   // add char that can be modified with dk ( a,e,i,o,u)
  ArrayOf_processed_deadkeyedChar: string[][],                  // add modified keys ( â,ê,î,ô,û)
  ArrayOf_processed_RuleData: Uint8Array[][],                // add modified keys ( â,ê,î,ô,û)
  ArrayOf_processed_dk_U8: Uint8Array[][]
};


export class KeylayoutToKmnConverter {
  static readonly INPUT_FILE_EXTENSION = '.keylayout';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';

  // TODO use callbacks
  //constructor(/*private*/ _callbacks: CompilerCallbacks, /*private*/ _options: CompilerOptions) {
  constructor(private callbacks: CompilerCallbacks, options: CompilerOptions) {
    // TODO: if these are needed, uncomment /*private*/ and remove _, and they will then
    // be available as class properties
  }

  /**
   * @brief  member function to run read/convert/write
   * @param  inputFilename the ukelele .keylayout-file to be converted
   * @param  outputFilename the resulting keyman .kmn-file
   * @return
   */
  async run(inputFilename: string, outputFilename: string): Promise<ConverterToKmnArtifacts> {

    if (!inputFilename || !outputFilename) {
      throw new Error('Invalid parameters');
    }

    console.log(' _S2 first READ file ........................................in:', inputFilename);
    const inArray: convert_object = this.read(inputFilename)

    if (!inArray) {
      return null;
    }

    console.log(' _S2 then CONVERT ........................................');
    const outArray: convert_object = await this.convert(inArray);

    if (!outArray) {
      return null;
    }

    console.log(' _S2 then WRITE to kmn .....................................');

    const out = this.write(outArray)
    if (!out) {
      return null;
    }

    // TODO throws
    console.log(';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FINISHED OK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;');
    throw new Error('Not finished yet');
  }

  /**
   * @brief  take filename, open and read data from .keylayout-file and store in several arrays of  the data object
   * member function to read filename ( a .keylayout-file) and write contents into Uint8Array keys_all_Layers
   * @param  filename the ukelele .keylayout-file to be converted
   * @return in case of success Uint8Array keys_all_Layers; else null
   */
  public read(filename: string): convert_object {

    console.log("inputFilename read", filename)
    const options = {
      ignoreAttributes: false,
      attributeNamePrefix: '@_'    // to access the attribute
    };


    console.log("xmlFile_name", (process.cwd() + "\\data" + filename.substring(filename.lastIndexOf("\\"))).replace(" ", ""))
    //const xmlFile = readFileSync(`${process.cwd()}/data/MySample.keylayout`, 'utf8')
    const xmlFile = readFileSync((process.cwd() + "\\data" + filename.substring(filename.lastIndexOf("\\"))).replace(" ", ""), 'utf8');

    // we don`t need file-read with uint8array return
    /*const fullPath = (process.cwd() + "\\data" + filename.substring(filename.lastIndexOf("\\"))).replace(" ", "")
    const xmlFile1 = this.callbacks.loadFile(fullPath)
    console.log("xmlFile1",xmlFile1)*/


    console.log("layouts_array read ")
    const parser = new XMLParser(options);
    const jsonObj = parser.parse(xmlFile); // get plain Object
    const modifierMap_ALL_KeyMapSelects: string[][] = []            // modifier for each keymapselect
    const modifierMap_all_Layers: string[] = []                   // array holding all MODIFIER strings e.g. "anyShift caps anyOption"  -why not put modifiers into Uint8Array along with values of keys of layer
    const keys_output_all_Layers: Uint8Array[][] = []
    const keys_action_all_Layers: Uint8Array[][] = []             // array holding all values with ACTION attribute (needed for finding deadkeys)
    const terminators_all_Layers: Uint8Array[] = []               // array holding all DEADKEYS for each mod state â, ê, ,....
    const duplicate_layouts_array: string[] = []                  // array holding the layouts e.g. ANSI or JIS // needed?? I think no
    const deadkeyedChars_all_Layers: string[][] = []              // array holding all DEADKEYS for each mod state â, ê, ,....
    const kmn_Rules_AllLayers: Uint8Array[][] = []
    let dk_pairs_all_Layers: string[][] = []                   // add dk-mapping ( dk1 <-> '^' )
    const dk_pairs_all_Layers_U8: Uint8Array[][] = []                   // add dk-mapping ( dk1 <-> '^' )

    boxArrays_S(jsonObj.keyboard);

    // fill arrays
    // TODO call: get only ANSI
    // Do I need to care or is there always ANSI which is always keyMapSet[0]
    // if so code can be shortened
    // .........................................................
    // LAYOUTS: get all groups like ANSI JIS, remove JIS
    // .........................................................

    //   in case we need to find ANSI
    for (let i = 0; i < jsonObj.keyboard.layouts.layout.length; i++) {
      duplicate_layouts_array[i] = jsonObj.keyboard.layouts.layout[i]['@_mapSet']
    }

    // remove duplicates
    const layouts_array: any[] = duplicate_layouts_array.filter(function (item, pos, self) {
      return self.indexOf(item) == pos;
    })
    // remove JIS (if keyMap contains baseMapSet it`s JIS)
    for (let i = 0; i < layouts_array.length; i++) {
      if (jsonObj.keyboard.keyMapSet[i].keyMap[0]['@_baseMapSet'])
        layouts_array.splice(i, 1);
    }
    // in case there always ANSI which is always keyMapSet[0]
    //const layouts_array: string[] = duplicate_layouts_array
    const keyMapSet_count = 0

    //#### end layouts ###############################################################################################################################################

    // .........................................................
    // MODIFIER MAP: get behaviours(=MapIndex) and modifiers (shift? leftShift caps? )
    // .........................................................
    for (let j = 0; j < jsonObj.keyboard.modifierMap.keyMapSelect.length; j++) {
      const modifierMap_ONE_InKeymapSelect: string[] = []

      for (let k = 0; k < jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier.length; k++) {
        modifierMap_all_Layers.push(jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier[k]['@_keys'])
        modifierMap_ONE_InKeymapSelect.push(jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier[k]['@_keys'])
      }
      modifierMap_ALL_KeyMapSelects.push(modifierMap_ONE_InKeymapSelect)
    }
    //#### end modifierMap ###############################################################################################################################################


    for (let j = 0; j < jsonObj.keyboard.modifierMap.keyMapSelect.length; j++) {
      // create a new array of keys_in_Layer (type Uint8tarray)
      const keys_output_One_Layer: Uint8Array[] = []
      const keys_action_One_Layer: Uint8Array[] = []

      // .........................................................
      // KEYMAP - OUTPUT : get all keys for attribute "output" ( y,c,b,...)  - TODO can i use shorter function?
      // output type Uint8Array -> string
      // .........................................................
      // loop keys
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[j].key.length; i++) {
        if (jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[j].key[i]['@_output'] !== "\0") {
          // textencoder converts string -> bytes  ( 'A' -> [ 65 ],   '☺' -> [ 226, 152, 186 ])
          // textencoder is of Uint8Array(1) for A and Uint8Array(3) for ☺
          keys_output_One_Layer[i] = new TextEncoder().encode(jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[j].key[i]['@_output']);
        }
        // ToDo do I need empty fields in keys_output_all_Layers, keys_action_all_Layers
        // .........................................................
        // KEYMAP - ACTION: get all keys for attribute "action" ( ^,a,e,i,...)  - TODO can i use shorter function?
        // action type Uint8Array -> string
        // .........................................................
        else if (jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[j].key[i]['@_action'] !== "\0") {
          keys_action_One_Layer[i] = new TextEncoder().encode(jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[j].key[i]['@_action']);
        }
      }

      // .........................................................
      // create array of "action" and array of "output"
      keys_output_all_Layers.push(keys_output_One_Layer)        // save all @output data ( will be used for conversion, write...)
      keys_action_all_Layers.push(keys_action_One_Layer)        // save all @action data ( will be used for deadkeys)


      //#### end output+action ###############################################################################################################################################
    }

    // .........................................................
    // ACTION: create array of "deadkey" /  "deadkey names"  - TODO can i use shorter function?
    // action type string[] -> string[][]
    // .........................................................
    const dk_pairs_all_Layers_max: string[][] = []
    // here replace with function to find
    // C3 state none + next Nr => deadkeys
    console.log("--------------------------------")

    // loop through actions and get the value of attribute "next" (which indicates this is a deadkey)
    for (let jj = 0; jj < jsonObj.keyboard.actions.action.length; jj++) {
      for (let kk = 0; kk < jsonObj.keyboard.actions.action[jj].when.length; kk++) {

        if (jsonObj.keyboard.actions.action[jj].when[kk]['@_next'] !== undefined) {
          const vec1d: string[] = []
          let dk_to_print

          const text_attribute_next = jsonObj.keyboard.actions.action[jj].when[kk]['@_next']

          // loop through terminators and get output of that next state
          for (let n = 0; n < jsonObj.keyboard.terminators.when.length; n++) {
            if (jsonObj.keyboard.terminators.when[n]['@_state'] === text_attribute_next)
              dk_to_print = jsonObj.keyboard.terminators.when[n]['@_output']
          }

          vec1d.push(text_attribute_next)
          vec1d.push(dk_to_print)
          vec1d.push(jsonObj.keyboard.actions.action[jj]['@_id'])   // todo remove later
          dk_pairs_all_Layers_max.push(vec1d)
        }
      }
    }
    dk_pairs_all_Layers = dk_pairs_all_Layers_max.filter(function (item, pos, self) {
      return self.indexOf(item) == pos;
    })
    console.log("dk_pairs_all_Layers", dk_pairs_all_Layers)
    const dk: string[] = [];

    // .........................................................
    // ACTION: create array of "deadkeyables_all_Layers"  - TODO can i use shorter function?
    // deadkeyables_one type Uint8Array -> string[][]
    // .........................................................
    const deadkeyables_all_Layers: string[][] = []
    // needed? we just want ANSI = the first one
    for (let j = 0; j < jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap.length; j++) {
      const deadkeyables_one: string[] = []
      // loop through all keys of ANSI
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[0].key.length; i++) {

        let dk_char
        // find output in action id
        for (let jj = 0; jj < jsonObj.keyboard.actions.action.length; jj++) {

          // find the action ID which was specified in keymap->key->action e.g. a19
          const action_name = new TextDecoder().decode(keys_action_all_Layers[j][i])

          if (action_name !== "") {
            //console.log("ction_name",action_name)
            // find action id in actions->action e.g. a19
            if (jsonObj.keyboard.actions.action[jj]['@_id'] === action_name) {

              //a85

              // 4 cases:
              //    C1 state none + output  =>
              //    C2 state Nr   + output  => 
              // OK C3 state none + next Nr => deadkeys see above
              //    C4 state Nr   + next Nr => loop deadkeyables


              if (jsonObj.keyboard.actions.action[jj].when[0]['@_state'] === "none") {
                dk_char = jsonObj.keyboard.actions.action[jj].when[0]['@_output']
                /* console.log("++++++++++++++++++++++dk_char", j, i, jj, "action_name", action_name,
                   jsonObj.keyboard.actions.action[jj].when[0], "===>", dk_char, "mytestVar", mytestVar)*/

                // if @_next instead of @_output
                // loop again
                // and again... again...( -> recursive func!!)
              }
            }
          }
        }
        // fills correct entries into modifier 1-3 then false ( because of no action any more)

        //const resulting_character = new TextDecoder().decode(keys_action_all_Layers[j][i])
        const resulting_character = dk_char

        if (resulting_character !== "")
          deadkeyables_one.push(resulting_character)
      }
      for (let k = 0; k < dk_pairs_all_Layers.length; k++) {
        dk.push(dk_pairs_all_Layers[k][1])
      }

      if (deadkeyables_one.length !== 0) {
        deadkeyables_all_Layers.push(deadkeyables_one)
      }
    }
    //console.log("deadkeyables_all_Layers before",deadkeyables_all_Layers)
    for (let j = 0; j < deadkeyables_all_Layers.length; j++) {
      // filter out dk -> plain deadkeyables_all_Layers ( a,^, e,i,´,u   => a,e,i,u)
      // return all that don`t find ^,´, `
      deadkeyables_all_Layers[j] = deadkeyables_all_Layers[j].filter(function (el) {
        return dk.indexOf(el) < 0;
      });

    }

    //console.log("deadkeyables_all_Layers after", deadkeyables_all_Layers)

    //double??? 
    // .........................................................
    // TERMINATOR: create array of "terminators"  - TODO can i use shorter function? what element of terminators do i need
    // terminators when state type Uint8Array[]
    // .........................................................
    for (let i = 0; i < jsonObj.keyboard.terminators.when.length; i++) {
      terminators_all_Layers[i] = jsonObj.keyboard.terminators.when[i]
    }

    // .........................................................
    // TODO can i use shorter function?  -> yes use terminators, filter, map or reduce

    // this addresses state+output and none+output
    // but not state+next or none+next

    // loop through all dk, key-actions and find @_action
    // find this value in <actions><action id=...>
    // in their 'when' find output for dk
    // for all 'action' at keys paragraph

    // ToDo URTGENT  duplicate dk ????? 
    // todo use lookup functions
    for (let j = 0; j < dk_pairs_all_Layers.length; j++) {
      //for (let j = 0; j < keys_action_all_Layers.length; j++) {
      const deadkeys_One_dk: string[] = []
      // find  in action e.g.  <action id="o">
      for (let k = 0; k < keys_action_all_Layers[j].length; k++) {

        const action_from_keys_prargraph = new TextDecoder().decode(keys_action_all_Layers[j][k])
        if (action_from_keys_prargraph !== "") {
          // find the same id (e.g.  <action id="o">) in the actions-paragraph
          for (let l = 0; l < jsonObj.keyboard.actions.action.length; l++) {

            if (jsonObj.keyboard.actions.action[l]['@_id'] === action_from_keys_prargraph) {
              // loop through when until dk name (e.g. dk s0)
              for (let m = 0; m < jsonObj.keyboard.actions.action[l].when.length; m++) {

                // and get their @_output
                if (jsonObj.keyboard.actions.action[l].when[m]['@_state'] === dk_pairs_all_Layers[j][0]) {
                  // todo push only if @output is found. if next is found: get value from terminators
                  //if (typeof jsonObj.keyboard.actions.action[l].when[m]['@_output'].property !== 'undefined')
                  deadkeys_One_dk.push(jsonObj.keyboard.actions.action[l].when[m]['@_output'])
                  //else
                  //console.log(deadkeys_One_dk, "§§§§§§ PROP UNDEFINED", j, "-", k, "-", l, "-", m, "-", dk_pairs_all_Layers[j][0], jsonObj.keyboard.actions.action[l].when[m].property)
                }
              }
            }
          }
        }

      }
      // do e clear

      // consoleconsole.log("deadkeys_One_dk", deadkeys_One_dk)
      deadkeyedChars_all_Layers.push(deadkeys_One_dk)


      //console.consolelog("deadkeyedChars_all_Layers", deadkeyedChars_all_Layers)
    }


    const vk: (string | number)[][] = []

    // TODO remove unneccassary elements
    const DataObject: convert_object = {
      name: "Ukelele-kmn",                                        // needed?? remove
      ArrayOf_Element_Layouts: layouts_array,                     // needed?? I think no
      ArrayOf_Element_KeyOutput: keys_output_all_Layers,
      ArrayOf_Element_KeyAction: keys_action_all_Layers,
      ArrayOf_Element_ALL_ModifierMaps: modifierMap_all_Layers,                   // all 18 modifiers in 1 array
      ArrayOf_Element_ModifierMaps_ALLKeyMapSelect: modifierMap_ALL_KeyMapSelects,   // 18 modifiers in 8 KeyMapSelect(behaviors)
      ArrayOf_Element_Terminators: terminators_all_Layers,        // add terminators ( ^,´,`)
      ArrayOf_processed_VK_from_keylayout: vk,
      ArrayOf_processed_dk: dk_pairs_all_Layers,                  // add dk-mapping ( dk1 <-> '^' )
      ArrayOf_processed_deadkeyables: deadkeyables_all_Layers,    // add char that can be modified with dk ( a,e,i,o,u)
      ArrayOf_processed_deadkeyedChar: deadkeyedChars_all_Layers,  // add modified keys ( â,ê,î,ô,û)
      ArrayOf_processed_RuleData: kmn_Rules_AllLayers,
      ArrayOf_processed_dk_U8: dk_pairs_all_Layers_U8
    };

    // Todo ? move to convert?
    // move up instead of action/output?
    // do i need to return a value?
    const rule_Data = this.createRuleData(DataObject, jsonObj)

    console.log("rule_Data-lenU8", rule_Data.ArrayOf_processed_RuleData.length)
    //console.log("rule_Data_U8", rule_Data.ArrayOf_processed_RuleData)

    // TODO review condition
    return rule_Data
    //return ((keys_output_all_Layers.length === nrOfStates + 5) && keys_output_all_Layers[0].length === nrOfKeys_inLayer) ? keys_output_all_Layers : null;
  }


  /**
   * @brief  member function to convert data of .keylayout-file to kmn-file This will convert/rename modifiers, position of Keys and deadkeys and save into an array 
   * @param  take data_ukelele and create a mapping from mac Keycodes to key-names and save to data_ukelele object
   * @param   data_ukelele (Uint8Array) data of the ukelele .keylayout-file
   * @return outArray Uint8Array keys_all_Layers, the converted data for kmn-files if all layers have been converted; else null
   */
  public convert(data_ukelele: convert_object): convert_object {

    const data_VK_from_keylayout: (string | number)[][] = [];

    for (let i = 0; i < data_ukelele.ArrayOf_Element_KeyOutput[0].length; i++) {

      const data_VK_from_keylayout_pair: (string | number)[] = [];
      const vk_from_Ukelele: string = this.map_UkeleleKC_To_VK(i)

      data_VK_from_keylayout_pair.push(i)
      data_VK_from_keylayout_pair.push(vk_from_Ukelele)
      data_VK_from_keylayout.push(data_VK_from_keylayout_pair)
    }
    data_ukelele.ArrayOf_processed_VK_from_keylayout = data_VK_from_keylayout
    return data_ukelele
  }


  /**
   * @brief   member function to write data fro object to file
   * @param  data_ukelele the array holding keyboard data
   * @return true if data has been written; false if not
   */
  //TODO need to use export const USVirtualKeyCodes here
  public write(data_ukelele: convert_object): boolean {
    console.log("start write")

    //  *************************************************************
    //  **** write stores *******************************************
    //  *************************************************************
    let data = "\n"
    data += "c\n"
    data += "c Keyman keyboard generated by kmn-convert\n"
    data += "c\n"
    data += "\n"

    data += '\########## OK #################################################################\n'
    data += "store(&VERSION) \'...\'\n"
    data += "store(&TARGETS) \'any\'\n"
    data += "store(&KEYBOARDVERSION) \'...\'\n"
    data += "store(&COPYRIGHT) '© 2024 SIL International\n"
    // TODO what else ??

    data += '\########## OK #################################################################\n'
    data += "\n"
    data += "begin Unicode > use(main)\n\n"
    data += "group(main) using keys\n\n"

    data += '\########## OK #################################################################\n'
    data += "Tipp: if K_? is replaced by undefined-> no enry in kmn_Key_Name=> add K_? there and it will be shown here\n"
    data += "Tipp: keys that are marked with sction do not aoear in ukelele_Array_output->do not appear in kmn\n"
    data += "\n"

    //  *************************************************************
    //  **** write rules ********************************************
    //  *************************************************************^
    // todo replace write rules with reading out of rule-array
    console.log("start write 1")
    // if caps is used in .keylayout-file we need to add NCAPS in kmn-file
    let isCAPSused = false
    const used_Keys_count = 50  // do we need more than 50 keys??
    const modi: string[] = data_ukelele.ArrayOf_Element_ALL_ModifierMaps;
    // const modi: string[] = data_ukelele.ArrayOf_Element_ALL_ModifierMaps;   // Todo check 2darray of modifiuers
    const filteredModifiers: string[] = modi.filter((mod) => (String(mod) === ("caps")))
    if (filteredModifiers.indexOf("caps") >= 0)
      isCAPSused = true

    // TODO good explanation
    // find all modifiers used per modifier combination
    // find resulting character from

    // console.log("start write 2, ", "data_ukelele.ArrayOf_ALL_Element_ModifierMaps.length", data_ukelele.ArrayOf_Element_ALL_ModifierMaps.length)
    // loop through keys
    //for (let j = 0; j <kmn_array.ArrayOf_Ukelele_output[0].length; j++) {
    for (let j = 0; j < used_Keys_count; j++) {

      data += '\n'

      // loop through keyMapSelect (8)
      for (let ii = 0; ii < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect.length; ii++) {

        // loop through keyMapSelect[ii] (2,4,1,1,...)
        for (let i = 0; i < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[ii].length; i++) {

          // get the modifier for the layer e.g. "CAPS SHIFT" -  keyMapSelect[ii][i] = "CAPS SHIFT"
          const label_modifier = this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[ii][i], isCAPSused)

          // get Key name e.g. K_A, K_SLASH
          const VK: (string | number)[][] = data_ukelele.ArrayOf_processed_VK_from_keylayout
          const vk_label = VK.filter(item => item[0] === data_ukelele.ArrayOf_processed_VK_from_keylayout[j][0])
          //console.log("---", "vk_label", vk_label)

          //console.log("start write 2C-", j, i)
          //console.log("start write 2C-", j, i, data_ukelele.ArrayOf_Element_KeyOutput[i][j])
          // get the character from keymap-section of .keylayout-file that will be written as result in kmn-file
          const resulting_character = new TextDecoder().decode(data_ukelele.ArrayOf_Element_KeyOutput[ii][j])

          //console.log("start write 2D")
          // TODO remove j +"(modif:" + i + `)
          if (resulting_character !== '') {
            //console.log("start write 2E")
            //   e.g.      [     NCAPS                  K_J                       ] >  '    j                       '
            data += j + "-(modif:" + ii + "-" + i + `) + [` + (label_modifier + ' ' + vk_label[0][1]).trim() + `] > \'` + resulting_character + '\'\n'
          }
          //console.log("start write 2F")
        }
      }
      //console.log("start write 2G")
    }

    //.........................................................................
    //   end write out ........................................................
    //.........................................................................
    //.........................................................................

    const NEWDATA = this.writeLastPart(data_ukelele, isCAPSused)
    data += NEWDATA + '\n'

    // console.log("start write 5")
    /*writeFileSync("data/MyResult.kmn", data, { flag: "w" })*/
    const data_encoded = new TextEncoder().encode(data)
    this.callbacks.fs.writeFileSync("data/MyResult.kmn", data_encoded) // not usable here since it takes UInt8array data

    // ToDo conditions?
    if (data.length > 0)
      return true;
    else
      return false


    //.........................................................................
    //   end write out ........................................................
    //.........................................................................
    //.........................................................................

    //console.log("start write 3")
    data += '\n'

    //  *************************************************************
    //  **** write deadkeys *****************************************
    //  *************************************************************


    data += 'NOW MY DEADKEYS :  \n'

    /* console.log("start write 3:  ",
       data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect.length,
       data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[1].length,
       data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[1][1].length,
       data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect
     )
     for (let i = 0; i < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect.length; i++) {
       for (let j = 0; j < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i].length; j++) {
         console.log("Element: ", i, j,
           data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i][j],
           this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i][j], isCAPSused)
         )
       }
     }*/
    // old

    // all 109 keys with action a9
    for (let i = 0; i < data_ukelele.ArrayOf_Element_KeyAction.length; i++) {
      // loop through keyMapSelect (8)
      for (let ii = 0; ii < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect.length; ii++) {
        //for (let ii = 0; ii < 8; ii++) {
        // loop through modifiers
        //for (let j = 0; j < data_ukelele.ArrayOf_Element_KeyAction.length; j++) {
        // 8 behaviors
        for (let j = 0; j < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect.length; j++) {
          //console.log("start write 3A", i, ii, j)

          // get the modifier for the layer
          //const label_modifier = this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ALL_ModifierMaps[j], isCAPSused)
          const label_modifier = this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ALL_ModifierMaps[j], isCAPSused)

          // get the character from keymap-section of .keylayout-file that will be written as result in kmn-file
          //const resulting_character = new TextDecoder().decode(data_ukelele.ArrayOf_Element_KeyAction[j][i])
          // wrong since it attempts to convert a9 and not 4 ( the termínator id )
          const resulting_character = new TextDecoder().decode(data_ukelele.ArrayOf_Element_KeyAction[ii][i])

          // therefore get character from ~ <- 4 <- a9
          //const realChar = this.getCharacterFromActionName(data_ukelele, data_ukelele.ArrayOf_Element_KeyAction[ii][i])
          //console.log("realChar", realChar,i,ii,j )
          // console.log("data_ukelele.ArrayOf_Element_KeyAction[ii][i],",
          //  data_ukelele.ArrayOf_Element_KeyAction[ii][i],
          //   realChar)

          if (resulting_character !== "") {
            console.log("start write 3B+3C", i, ii, j, label_modifier, "---", resulting_character)

            const VK: (string | number)[][] = data_ukelele.ArrayOf_processed_VK_from_keylayout
            const vk_label = VK.filter(item => item[0] === data_ukelele.ArrayOf_processed_VK_from_keylayout[i][0])
            //  console.log("VK", VK, "vk_label", vk_label)

            //  console.log("start write 3D,data_ukelele.ArrayOf_processed_dk", data_ukelele.ArrayOf_processed_dk)
            for (let k = 0; k < data_ukelele.ArrayOf_processed_dk.length; k++) {
              console.log("start write 3E")
              console.log("resulting_character", resulting_character, "data_ukelele.ArrayOf_processed_dk[k][1]", data_ukelele.ArrayOf_processed_dk[k][1])
              console.log("data_ukelele.ArrayOf_processed_dk is", data_ukelele.ArrayOf_processed_dk)
              console.log("resulting_characte, data_ukelele.ArrayOf_processed_dk is", resulting_character, data_ukelele.ArrayOf_processed_dk[k][2])
              if ((resulting_character !== "") && (resulting_character === data_ukelele.ArrayOf_processed_dk[k][2])) {
                data += '[' + (label_modifier + ' ' + vk_label[0][1]).trim() + '] ' + "> dk(" + this.getHexFromChar(data_ukelele.ArrayOf_processed_dk[k][1]) + ") " + '\n'
                console.log("start write 3D", data)

              }
            }
          }
        }

      }
    }



    // console.log("start write 4")
    data += "\n"
    data += "match > use(deadkeys)\n\n"
    data += "group(deadkeys)\n"
    data += "\n"
    //console.log("data_ukelele.ArrayOf_processed_deadkeyables.length", data_ukelele.ArrayOf_processed_deadkeyables.length)
    // console.log("data_ukelele.String(data_ukelele.ArrayOf_processed_deadkeyables[i])).length", String(data_ukelele.ArrayOf_processed_deadkeyables[0]))
    //console.log("data_ukelele.String(data_ukelele.ArrayOf_processed_deadkeyables[i])).length--", "\'", String(data_ukelele.ArrayOf_processed_deadkeyables[0]).trim().replace(/\,+/g, "' '"),)

    //const char = "\'" + String(data_ukelele.ArrayOf_processed_deadkeyables[0]).trim().replace(/\,+/g, "' '")
    //console.log("char", char)

    //const char1 = char.slice(0, -1)
    //console.log("char1", char1)

    for (let i = 0; i < data_ukelele.ArrayOf_processed_deadkeyables.length; i++) {
      if (data_ukelele.ArrayOf_processed_deadkeyedChar[i] !== undefined) {
        data += "store(dkf" + this.getHexFromChar(data_ukelele.ArrayOf_processed_dk[i][1]) + ") " + ("\'" + String(data_ukelele.ArrayOf_processed_deadkeyables[i])).replace(/\,+/g, "' '").slice(0, -1).trim() + "\n"
        data += "store(dkt" + this.getHexFromChar(data_ukelele.ArrayOf_processed_dk[i][1]) + ") " + ("\'" + String(data_ukelele.ArrayOf_processed_deadkeyedChar[i])).replace(/\,+/g, "' '") + "'\n"

        data += "store(dkf" + this.getHexFromChar(data_ukelele.ArrayOf_processed_dk[i][1]) + ") " + ("\'" + String(data_ukelele.ArrayOf_processed_deadkeyables[i])).replace(/\,+/g, "' '").slice(0, -1).trim() + "\n"
        data += "store(dkt" + this.getHexFromChar(data_ukelele.ArrayOf_processed_dk[i][1]) + ") " + ("\'" + String(data_ukelele.ArrayOf_processed_deadkeyedChar[i])).replace(/\,+/g, "' '") + "'\n"
        data += '\n'
      }
    }


    //TODO better distinction!!!
    data += 'NOW MY RULES ********************************\n'
    for (let kkk = 0; kkk < data_ukelele.ArrayOf_processed_RuleData.length; kkk++) {
      if (data_ukelele.ArrayOf_processed_RuleData[kkk].length <= 6) {
        let line: string = ""
        for (let lll = 0; lll < data_ukelele.ArrayOf_processed_RuleData[kkk].length; lll++) {
          const entry = new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][lll])
          line += " " + entry + "  "
        }
        data += line + '\n'
      }
    }

    data += '\nNOW MY C4 RULES *********** (only to get 02C6 ect) *********************\n\n'
    for (let kkk = 0; kkk < data_ukelele.ArrayOf_processed_RuleData.length; kkk++) {
      if (new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][4]) === "isTerminator") {
        let line: string = ""
        for (let lll = 0; lll < data_ukelele.ArrayOf_processed_RuleData[kkk].length; lll++) {
          const entry = new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][lll])
          line += " " + entry + "  "
        }
        data += line + '\n'
      }
    }



    data += '......................................................\n'
    for (let kkk = 0; kkk < data_ukelele.ArrayOf_processed_RuleData.length; kkk++) {
      if (new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][4]) === "isTerminator") {
        let line: string = ""
        const entry = new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][6])
        line += " Hex dk: " + entry + "  "
        data += line + '\n'
      }
    }

    // array ony of dk (array_dk_char [ '¨', 'ˆ', '´', '˜', '`' ])
    const array_dk_char = [...new Set(data_ukelele.ArrayOf_processed_dk.map(a => a[1]))];
    console.log("array_dk_char", array_dk_char);


    data += '\nNOW DEADKEYABLES ********************************\n\n'


    const deadkeyables_array = []
    // loop al data rules
    for (let kkk = 0; kkk < data_ukelele.ArrayOf_processed_RuleData.length; kkk++) {

      // find dk rules 
      if ((new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][0]) === ("first_modifier_C2"))
        && (data_ukelele.ArrayOf_processed_RuleData[kkk].length === 5)) {

        // if char of pos4 is in array_dk_char [ '¨', 'ˆ', '´', '˜', '`' ])
        if (!array_dk_char.includes(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][4]))) {
          deadkeyables_array.push(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][2]))
        }
      }
    }

    // remove duplicates  -> array_deadkeyables_unique1 [  'K_A', 'K_E',  'K_Y', 'K_O',  'K_U', 'K_I',  'K_N' ]
    const array_deadkeyables_unique = deadkeyables_array.filter(function (item, pos, self) {
      return self.indexOf(item) == pos;
    })


    data += " store dk...  : "
    //console.log("array_deadkeyables_unique", array_deadkeyables_unique)

    for (let i = 0; i < array_deadkeyables_unique.length; i++) {
      data += '\'' + array_deadkeyables_unique[i] + '\' '
    }

    data += '\nNOW DEADKEYED ********************************\n\n'
    let counter = 0
    const deadkeyed_array = []
    // loop al data rules
    for (let kkk = 0; kkk < data_ukelele.ArrayOf_processed_RuleData.length; kkk++) {

      // find dk rules 
      if ((new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][0]) === ("first_modifier_C2"))
        && (data_ukelele.ArrayOf_processed_RuleData[kkk].length === 5)) {

        const deadkeyed_array_1D = []
        if (!array_dk_char.includes(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][4]))) {
          deadkeyed_array_1D.push(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][1]))
          deadkeyed_array_1D.push(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][2]))
          deadkeyed_array_1D.push(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][3]))
          deadkeyed_array_1D.push(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][4]))
        }
        deadkeyed_array.push(deadkeyed_array_1D)
        counter++
      }
    }

    // remove empty 
    const array_deadkeyed_unique = deadkeyed_array.filter(function (arr) {
      return arr.length > 0
    });

    data += " store dk...  : "
    //   console.log("array_deadkeyed_unique", counter, array_deadkeyed_unique)

    for (let i = 0; i < array_deadkeyed_unique.length; i++) {
      if (array_deadkeyed_unique[i].length !== 0)
        data += '\n\'' + array_deadkeyed_unique[i] + '\' '
      //    console.log("array_deadkeyed_unique[i][1]", array_deadkeyed_unique[i][1])
    }

    // console.log("modiArray", modiArray)
    data += '\'\n'
    data += '\nNOW DEADKEYS ********************************\n\n'


    //##############################################################################################
    //######################all#####################################################################

    const All_array: any[] = []
    // loop al data rules and  push  dk rules to array
    for (let kkk = 0; kkk < data_ukelele.ArrayOf_processed_RuleData.length; kkk++) {
      const deadkeyed_array_1D = []
      deadkeyed_array_1D.push(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][0]))    //modifier
      deadkeyed_array_1D.push(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][1]))    //shiftstates
      deadkeyed_array_1D.push(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][2]))    //keyname 1
      deadkeyed_array_1D.push(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][3]))    //output character/ keyname 2 /  shiftstate 2    
      if (data_ukelele.ArrayOf_processed_RuleData[kkk].length === 4)
        deadkeyed_array_1D.push(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][4]))  // --                         / keyname 2
      else
        deadkeyed_array_1D.push("")
      if (data_ukelele.ArrayOf_processed_RuleData[kkk].length === 5)
        deadkeyed_array_1D.push(new TextDecoder().decode(data_ukelele.ArrayOf_processed_RuleData[kkk][5]))  // --                         / output character
      else
        deadkeyed_array_1D.push("")

      if (deadkeyed_array_1D.length > 0)
        All_array.push(deadkeyed_array_1D)
      counter++
    }

    //
    // console.log("All_array", All_array)
    data += " store dk...  : \n"


    let All_C1_Shift_Keyname_all
    let All_C1_Shift_Keyname
    let Shift: any
    const modiDeadkeyable1_all_lines: any[][] = []
    const deadkeyables_combi: any[][] = []
    const deadkeyables_combiAndSS: any[][] = []

    //loop through all behaviours (8)
    for (let j = 0; j < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect.length; j++) {

      // for (let j = 0; j < 1; j++) {
      let deadkeyables_without_VKspace: any[] = []

      //loop through all modifiers (4) for behaviour
      for (let k = 0; k < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[j].length; k++) {
        const deadkeyables_line: any[] = []
        const modiDeadkeyable1_arr: any[] = []
        const deadkeyables_combi2: any[][] = []
        // loop all keys
        for (let i = 0; i < 50; i++) {
          const Keyname = this.map_UkeleleKC_To_VK(i)
          Shift = this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[j][k], isCAPSused)
          // const Shift = "CAPS"
          // console.log("Keyname", Keyname, "xxShift", Shift)


          // find deadkeyable [ 'first_modifier_C1', 'CAPS', 'K_A', 'A', '', '', '' ]
          All_C1_Shift_Keyname_all = All_array.filter(function ([A, B, C, D, E, F, G]) {
            return ((A === "first_modifier_C1") && (B === Shift) && (C === Keyname))
          });
          //console.log("All_C1_Shift_Keyname_all", All_C1_Shift_Keyname_all.length,All_C1_Shift_Keyname_all)





          All_C1_Shift_Keyname = All_C1_Shift_Keyname_all

          // console.log("All_C1_Shift_Keyname", All_C1_Shift_Keyname.length, All_C1_Shift_Keyname)

          // in case there are more rules-should not be the case 
          for (let xi = 0; xi < All_C1_Shift_Keyname.length; xi++) {
            modiDeadkeyable1_arr.push(All_C1_Shift_Keyname[xi])
          }

        }

        //console.log("modiDeadkeyable1_arr", modiDeadkeyable1_arr.length, modiDeadkeyable1_arr, modiDeadkeyable1_arr[0])


        deadkeyables_without_VKspace = modiDeadkeyable1_arr.filter(function (mod) {
          return ((mod !== undefined) && (mod[2] !== "K_SPACE"))
        });
        console.log("deadkeyables_without_VKspace", deadkeyables_without_VKspace.length, deadkeyables_without_VKspace)

        for (let i = 0; i < deadkeyables_without_VKspace.length; i++) {
          //deadkeyables_line.push(deadkeyables_without_VKspace[i][2])
          deadkeyables_line.push(deadkeyables_without_VKspace[i][3])

          const deadkeyables_line_forC: any[] = []
          for (let zz = 2; zz < 4; zz++) {
            deadkeyables_line_forC.push(deadkeyables_without_VKspace[i][zz])
          }
          deadkeyables_combi.push(deadkeyables_line_forC)
          deadkeyables_combi2.push(deadkeyables_line_forC)
        }

        // console.log("deadkeyables_combi2[k][1]", deadkeyables_combi2[k], deadkeyables_combi2[j])

        if (deadkeyables_line.length != 0) {
          modiDeadkeyable1_all_lines.push(deadkeyables_line)
          data += "Deadkeyables:   " + deadkeyables_line + "\n"
          data += "dk          :   " + deadkeyables_combi2[k][j] + "\n"
        }

        deadkeyables_combiAndSS.push(deadkeyables_combi2)
      }
      // THIS IS DEADKEYABLES OUTPUT
      //   console.log("modiDeadkeyable1_all_lines", modiDeadkeyable1_all_lines.length, modiDeadkeyable1_all_lines)
    }
    data += '\n'

    // console.log("deadkeyables_combiAndSS", deadkeyables_combiAndSS)





    //const NEWDATA = this.writeLastPart(data_ukelele, isCAPSused)
    /* data += NEWDATA + '\n'
 
     // console.log("start write 5")
     //writeFileSync("data/MyResult.kmn", data, { flag: "w" })
     const data_encoded = new TextEncoder().encode(data)
     this.callbacks.fs.writeFileSync("data/MyResult.kmn", data_encoded) // not usable here since it takes UInt8array data
 
     // ToDo conditions?
     if (data.length > 0)
       return true;
     else
       return false*/
  }

  //... helpers .............................................................................................

  //   TODO move outside of class?
  // ToDo keep only uint8array-version
  // for more info about mapping and cases C0-C4 
  // see https://docs.google.com/document/d/1ISjACTA9aUBueTo1AoKnOsI6QR5kXeeD_maI0XUyXqg/edit?tab=t.0

  public createRuleData(data_ukelele: convert_object, jsonObj: any): convert_object {
    const DataArray_U8: Uint8Array[][] = []

    let action_id
    let output_id
    const isCapsused = this.checkIfCapsUsed(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect)

    // loop keys
    //for (let j = 0; j < jsonObj.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
    for (let j = 0; j < 52; j++) {

      // loop behaviors
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[0].keyMap.length; i++) {

        // ......................................................................................................
        // case C0: output ......................................................................................
        // a key is mapped to a character ( code-> output) ......................................................
        // ...............e. g. <key code="1" output="s"/> ......................................................
        // ......................................................................................................
        // in keys at top for code 1 (K_S) take output ("s") [italian copy]
        // get modifiers [modifer of Keymap index 0]
        // write [modifer of Keymap index 0] + K_S > s  
        // ......................................................................................................

        if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== undefined) {
          output_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output']

          // loop modifiers
          for (let l = 0; l < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i].length; l++) {

            const DataArraySingleStateC0_U8: Uint8Array[] = []

            if (output_id !== "") {
              const first_modifier_C0_U8 = new TextEncoder().encode(this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i][l], isCapsused));
              const result_C0_U8 = new TextEncoder().encode(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output']);
              const first_key_C0_U8 = new TextEncoder().encode(this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])));

              /*   console.log("### Key Nr  ",
                   jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'],
                   "[ + C0 modifiers->", i, first_modifier_C0.padEnd(25, " "), "] ",
                   this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])).padEnd(8, " "),
                   ">  ",
                   jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'])*/

              DataArraySingleStateC0_U8.push(new TextEncoder().encode("first_modifier_C0"))
              DataArraySingleStateC0_U8.push(first_modifier_C0_U8)
              DataArraySingleStateC0_U8.push(first_key_C0_U8)
              DataArraySingleStateC0_U8.push(result_C0_U8)
            }
            if (DataArraySingleStateC0_U8.length > 0)
              DataArray_U8.push(DataArraySingleStateC0_U8)
          }
        }

        // ......................................................................................................
        // actions ...............................................................................          
        // ......................................................................................................

        else if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] !== undefined) {
          // ......................................................................................................
          // case C1: action + state none + output   ..............................................................
          // a key is mapped to an action and then to an output ...................................................
          // code->action->action(none)->action(output) ...........................................................
          // ...............e. g. <when state="none" output="a" ...................................................
          // ......................................................................................................
          // in keys at top for code 0 (K_A) take actions id (a9) [italian copy]
          // get modifiers [modifer of Keymap index 0]
          // goto id a9 
          // in action id a9 find "none" 
          // get output "a"
          // write [modifer of Keymap index 0] + K_A > a  
          // ......................................................................................................

          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']
          const result_C1 = this.lookup_6_ActionNone__To__ActionOutput(jsonObj, action_id)
          const result_C1_U8 = new TextEncoder().encode(this.lookup_6_ActionNone__To__ActionOutput(jsonObj, action_id))

          // modifiers
          for (let l = 0; l < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i].length; l++) {
            const DataArraySingleStateC1_U8: Uint8Array[] = []

            if (result_C1 !== undefined) {
              const first_modifier_C1_U8 = new TextEncoder().encode(this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i][l], isCapsused))
              const first_key_C1_U8 = new TextEncoder().encode(this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])))

              /*console.log(
                "### Key Nr  ",
                jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'],
                "[ + C1 modifiers->", i, first_modifier_C1.padEnd(25, " "), "] ",
                this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])).padEnd(8, " "),
                ">  ",
                result_C1)*/

              DataArraySingleStateC1_U8.push(new TextEncoder().encode("first_modifier_C1"))
              DataArraySingleStateC1_U8.push(first_modifier_C1_U8)
              DataArraySingleStateC1_U8.push(first_key_C1_U8)
              DataArraySingleStateC1_U8.push(result_C1_U8)
            }

            if (DataArraySingleStateC1_U8.length > 0)
              DataArray_U8.push(DataArraySingleStateC1_U8)
          }
          // ......................................................................................................
          // case C2: action + state Nr + output ..................................................................
          // a key is mapped to an action, then to an state+output ................................................
          // replace state x with all rules that result in x (<when state="none" next="x") e.g. x=2 ...............
          // code->action->action(none) + state-> output) .........................................................
          // ...............e. g. <when state="2" output="à"/> ....................................................
          // ......................................................................................................
          // in keys at top for code 0 (K_A) take actions id (a9) [italian copy]
          // get modifiers [modifer of Keymap index 0]
          // loop all actions and look for next="2"  (state="none" => next="2") (=>action id = a8)
          // get action id of this row (id = a8)
          // look for a8 in keymap-keys action at the top <key code="25" action="a8"/>
          // take code = 25 and map keycode to VK (VK= K_9)
          // get second modifiers [modifer of Keymap index 3] (=anyOption)
          // [modifer of Keymap index 3] + K_A  +  K_9 > à
          // write: [anyOption + K_A]  > dk(dk1)  ; dk(dk1) + [second modifiers K_9]  > à
          // ......................................................................................................

          let result_C2
          let result_C2_U8
          let nextvalArray: string[] = []

          // get action id: e.g.  id a16 ->id nr 8
          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']
          const indexInActions = this.lookup_10_ActionId__To__ActioIndex(jsonObj, action_id)

          // loop through all actionh/when find state and get action id ( 8-> state 3)
          for (let jj = 0; jj < jsonObj.keyboard.actions.action[indexInActions].when.length; jj++) {

            const stateVal = jsonObj.keyboard.actions.action[indexInActions].when[jj]['@_state']

            // if there is a state defined, collect all cases which result in that state ( e.g. which case has next = 3)
            if (stateVal !== undefined) {
              // get output
              result_C2 = jsonObj.keyboard.actions.action[indexInActions].when[jj]['@_output']
              result_C2_U8 = new TextEncoder().encode(jsonObj.keyboard.actions.action[indexInActions].when[jj]['@_output'])
              // get all cases which result in state 3
              nextvalArray = this.lookup_5_ActionState__To__ActionNext_none(jsonObj, stateVal)

              // for all modifier combinations
              for (let l = 0; l < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i].length; l++) {

                //  const DataArraySingleStateC2: string[] = []
                const DataArraySingleStateC2_U8: Uint8Array[] = []

                for (let k = 0; k < nextvalArray.length; k++) {
                  if (result_C2 !== undefined) {

                    /*  const first_modifier_C2 = this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i][l], isCapsused)
                      const first_Key_C2 = this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']))
                      const second_Key_C2 = this.map_UkeleleKC_To_VK(Number(this.lookup_11_KeyMapAction__To__KeyMapCode(jsonObj, nextvalArray[k])))
  */
                    const first_modifier_C2_U8 = new TextEncoder().encode(this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i][l], isCapsused))
                    const first_Key_C2_U8 = new TextEncoder().encode(this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])))
                    const second_Key_C2_U8 = new TextEncoder().encode(this.map_UkeleleKC_To_VK(Number(this.lookup_11_KeyMapAction__To__KeyMapCode(jsonObj, nextvalArray[k]))))

                    /*console.log(
                      "  ### Key Nr",
                      "nextvalArray.length", nextvalArray.length,
                      j,
                      "[ + C2 modifiers->", i, first_modifier_C2.padEnd(25, " "), "] ",
                      //this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])).padEnd(8, " "),
                      first_Key_C2.padEnd(8, " "),
                      ">  ",
                      result_C2,
                      "\t  stateV ->", stateVal,
                      jsonObj.keyboard.keyMapSet[0].keyMap[i].key[jj]['@_code'],
                      nextvalArray[k],

                      "-",
                      nextvalArray,
                      "-----",
                      "### Key Nr",
                      "+modixx2 +key nr",
                      this.lookup_11_KeyMapAction__To__KeyMapCode(jsonObj, nextvalArray[k]),
                      " +keymapindex of this",
                      this.lookup_11_KeyMapAction__To__KeyIndex(jsonObj, nextvalArray[k])
                    )*/

                    DataArraySingleStateC2_U8.push(new TextEncoder().encode("first_modifier_C2"))
                    DataArraySingleStateC2_U8.push(first_modifier_C2_U8)
                    DataArraySingleStateC2_U8.push(first_Key_C2_U8)
                    DataArraySingleStateC2_U8.push(second_Key_C2_U8)// state = none -> no modifier
                    DataArraySingleStateC2_U8.push(result_C2_U8)
                  }

                  if (DataArraySingleStateC2_U8.length > 0)
                    DataArray_U8.push(DataArraySingleStateC2_U8)
                }
              }
            }
          }

          // ......................................................................................................
          // case C3: action + state Nr + Next ....................................................................
          // ...............e. g.<when state="14" next="20"/> .....................................................
          // replace state x with all rules that result in 14 (<when state="x" next="14") .........................
          // a key is mapped to an action and then to a terminator ................................................
          // code->action->action(state)->action(next)->terminator(output) ........................................
          // ......................................................................................................
          {// in keys at top for code 10 (=K_BACKQUOTE) take actions id (a57)  [German standard copy]
            // code 10 = K_BACKQUOTE (or another key 93)
            // get modifiers [modifer of Keymap index 0]
            // goto action id a57 and find 14 in <when state="14" next="20"/>
            // find all rules that result in 14 (next="14" - there might be several)
            // get actionsId and of that rule (a80 )
            // look at top for a80 and find key Code (code 40 = K_K)
            // get modifiers Index of that key (code 40) [modifer of Keymap index 3]
            // lookup modifier names from modifiers Index
            // take 20 ( of next="20") and look for state = 20 in terminators ("next" points to terminators)
            // get output ("̭")
            // [first modifier + K_K]  > dk(dk1)  ; dk(dk1) + [second modifier K_BKQUOTE]  > "̭"
          }
          // ......................................................................................................

          // get a9  in behavior/key
          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']
          const actionIdIndex = this.lookup_10_ActionId__To__ActioIndex(jsonObj, action_id)
          const DataArraySingleStateC3_U8: Uint8Array[] = []

          let value_state
          let value_next
          let the_ContextKeyNr
          let first_key_C3_U8
          let keymapIndexForactionID_2: any[][]

          // loop all action-when and find state-next-pair
          for (let l = 0; l < jsonObj.keyboard.actions.action[actionIdIndex].when.length; l++) {

            // state_next data
            if ((jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_state'] !== "none")
              && (jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_next'] !== undefined)) {

              value_state = jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_state'] // e.g. 3
              value_next = jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_next']   // e.g. 1

              // get actionId of that state/next pair (3,1) e.g. actionId a17
              const theContextID = this.lookup_13_ActionNext__To__ActionID(jsonObj, value_state)
              // find keyNr of that actionID a17-> code=28
              the_ContextKeyNr = this.lookup_11_KeyMapAction__To__KeyMapCode(jsonObj, theContextID)

              // find all occurences of a17 e.g. key 28/3 [ [ '28', 3 ] ]
              keymapIndexForactionID_2 = this.lookup_14_ActionName__To__MapIndex(jsonObj, String(theContextID))
              // get keyname e.g. 28-> K_8
              first_key_C3_U8 = new TextEncoder().encode(this.map_UkeleleKC_To_VK(Number(the_ContextKeyNr)))
            }

            //  const result_C3 = this.lookup_9_TerminatorState__To__TerminatorOutput_str(jsonObj, value_next)
            const result_C3_U8 = new TextEncoder().encode(this.lookup_9_TerminatorState__To__TerminatorOutput_str(jsonObj, value_next))

            for (let l = 0; l < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i].length; l++) {
              // const second_modifier_C3 = this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i][l], isCapsused)
              const second_modifier_C3_U8 = new TextEncoder().encode(this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i][l], isCapsused))

              if (the_ContextKeyNr !== undefined) {

                const second_key_Nr = Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
                // const second_key_C3 = this.map_UkeleleKC_To_VK(second_key_Nr)

                const second_key_C3_U8 = new TextEncoder().encode(this.map_UkeleleKC_To_VK(second_key_Nr))

                for (let kk = 0; kk < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[keymapIndexForactionID_2[0][1]].length; kk++) {

                  const first_modifier_text_C3_U8 = data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[keymapIndexForactionID_2[0][1]][kk]
                  const first_modifier_C3_U8 = new TextEncoder().encode(this.create_kmn_modifier(first_modifier_text_C3_U8, isCapsused))

                  /*console.log("   ### Key Nr", jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'],
                    "[ + C3 modifiers->", i, l,
                    "[", first_modifier_C3, first_key_C3, "]",
                    "> dk(dk1) #########  ", "  dk(dk1) + [",
                    second_modifier_C3, second_key_C3, "] > ", result_C3,
                    the_ContextKeyNr,
                    keymapIndexForactionID_2, first_key_C3)*/

                  DataArraySingleStateC3_U8.push(new TextEncoder().encode("first_modifier_C3"))
                  DataArraySingleStateC3_U8.push(first_modifier_C3_U8)
                  DataArraySingleStateC3_U8.push(first_key_C3_U8)
                  DataArraySingleStateC3_U8.push(second_modifier_C3_U8)
                  DataArraySingleStateC3_U8.push(second_key_C3_U8)
                  DataArraySingleStateC3_U8.push(result_C3_U8)
                }


                if (DataArraySingleStateC3_U8.length > 0)
                  DataArray_U8.push(DataArraySingleStateC3_U8)
              }
            }
          }
          // ......................................................................................................
          // case C4: action + state none + Next ............................................................DONE .
          // ...............e. g. <when state="none" next="4"/> ...................................................
          // a key is mapped to an action and then to a terminator ................................................
          // code->action->action(none)->action(next)->terminator(output) .........................................
          // ......................................................................................................
          // in keys for code 32 (K_U) at top find actions id a16   [italian copy]
          // get modifiers [modifer of Keymap index 3]
          // in actions a16 (<when state="none" next="4"/>) find state "none" and get next (=4)
          // in terminators ( <when state="4" output="¨"/>) find the state (=4) and get output ("¨")
          // write  [modifer of Keymap index 3]  +  K_U -> "¨"  

          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']

          const next_id = this.lookup_3_ActionNone__To__ActionNext(jsonObj, action_id)
          const result_C4 = this.lookup_9_TerminatorState__To__TerminatorOutput_str(jsonObj, next_id)
          const result_C4_U8 = new TextEncoder().encode(this.lookup_9_TerminatorState__To__TerminatorOutput_str(jsonObj, next_id))

          for (let l = 0; l < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i].length; l++) {

            const DataArraySingleStateC4_U8: Uint8Array[] = []

            const first_modifier_C4_U8 = new TextEncoder().encode(this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i][l], isCapsused))
            const first_key_C4_U8 = new TextEncoder().encode(this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])))

            const str_UTF8 = new TextDecoder().decode(result_C4_U8)

            DataArraySingleStateC4_U8.push(new TextEncoder().encode("first_modifier_C4"))
            DataArraySingleStateC4_U8.push(first_modifier_C4_U8)
            DataArraySingleStateC4_U8.push(first_key_C4_U8)
            DataArraySingleStateC4_U8.push(result_C4_U8)

            if (result_C4_U8.length !== 0) {
              DataArraySingleStateC4_U8.push(new TextEncoder().encode("isTerminator"))
              DataArraySingleStateC4_U8.push(result_C4_U8)
              DataArraySingleStateC4_U8.push(new TextEncoder().encode(this.getHexFromChar(str_UTF8)))
            }

            if (result_C4 !== "") {
              console.log(
                "### Key Nr  ",
                jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'],
                "[ + C4 modifiers->", i, "] ",
                this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])).padEnd(8, " "),
                ">  ",
                result_C4,
                "   [",
                action_id,
                next_id,
                "]",
                result_C4_U8, this.getHexFromChar(str_UTF8),
                this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect[i][l], isCapsused)
              )
            }
            if (DataArraySingleStateC4_U8.length > 0)
              DataArray_U8.push(DataArraySingleStateC4_U8)
          }
        }

        else
          console.log("ERROR : some entries are not available")
      }
    }

    data_ukelele.ArrayOf_processed_RuleData = DataArray_U8
    console.log("DataArray_U8", DataArray_U8)
    return data_ukelele
  }

  // public lookup_2_KeyMapAction__To__ActionAction() { }
  public lookup_3_ActionNone__To__ActionNext(data: any, search: string): string {
    for (let jj = 0; jj < data.keyboard.actions.action.length; jj++) {
      if (data.keyboard.actions.action[jj]['@_id'] === search) {
        for (let kk = 0; kk < data.keyboard.actions.action[jj].when.length; kk++) {
          if (data.keyboard.actions.action[jj].when[kk]['@_state'] === "none")
            return data.keyboard.actions.action[jj].when[kk]['@_next']
        }
      }
    }
    return ""
  }
  //public lookup_4_ActionNext__To__ActionState() { }
  public lookup_5_ActionState__To__ActionNext(data: any, action_idName: string): string[] {
    const returnarray: string[] = []
    // e.g. action_idName = 3 
    if (action_idName !== "none") {
      // loop all action/when
      for (let k = 0; k < data.keyboard.actions.action.length; k++) {
        for (let j = 0; j < data.keyboard.actions.action[k].when.length; j++) {
          // find attribute next === 3
          if (data.keyboard.actions.action[k].when[j]['@_next'] === action_idName) {
            returnarray.push(data.keyboard.actions.action[k]['@_id'])
          }
        }
      }
    }
    return returnarray
  }
  public lookup_5_ActionState__To__ActionNext_none(data: any, action_idName: string): string[] {
    const returnarray: string[] = []
    // e.g. action_idName = 3 
    if (action_idName !== "none") {
      // loop all action/when
      for (let k = 0; k < data.keyboard.actions.action.length; k++) {
        for (let j = 0; j < data.keyboard.actions.action[k].when.length; j++) {
          // find attribute next === 3
          if (data.keyboard.actions.action[k].when[j]['@_next'] === action_idName) {
            if (data.keyboard.actions.action[k].when[j]['@_state'] === "none")
              returnarray.push(data.keyboard.actions.action[k]['@_id'])
          }
        }
      }
    }
    return returnarray
  }
  public lookup_6_ActionNone__To__ActionOutput(data: any, search: any): any {
    //  a16-> id ==a16 ->4
    //todo what if duplicate value??
    let OutputValue = ""
    for (let jj = 0; jj < data.keyboard.actions.action.length; jj++) {
      if (data.keyboard.actions.action[jj]['@_id'] === search) {
        for (let kk = 0; kk < data.keyboard.actions.action[jj].when.length; kk++) {
          if (data.keyboard.actions.action[jj].when[kk]['@_state'] === "none") {
            OutputValue = data.keyboard.actions.action[jj].when[kk]['@_output']
          }
        }
      }
    }
    return OutputValue
  }
  //public lookup_7_ActionState__To__ActionOutput() { }
  //public lookup_8_ActionNext__To__TerminatorState() { }

  public lookup_9_TerminatorState__To__TerminatorOutput_ui8(data: any, search: string): Uint8Array {
    for (let jj = 0; jj < data.keyboard.terminators.when.length; jj++) {
      if (data.keyboard.terminators.when[jj]['@_state'] === search) {
        return new TextEncoder().encode(data.keyboard.terminators.when[jj]['@_output']);
      }
    }
    return new TextEncoder().encode("")
  }
  public lookup_9_TerminatorState__To__TerminatorOutput_str(data: any, search: string): string {
    let OutputValue = ""
    for (let jj = 0; jj < data.keyboard.terminators.when.length; jj++) {
      if (data.keyboard.terminators.when[jj]['@_state'] === search) {
        OutputValue = data.keyboard.terminators.when[jj]['@_output']
      }
    }
    return OutputValue
  }
  public lookup_10_ActionId__To__ActioIndex(data: any, search: string): number {
    for (let k = 0; k < data.keyboard.actions.action.length; k++) {
      if (data.keyboard.actions.action[k]['@_id'] === search)
        return k
    }
    return 0

  }
  public lookup_11_KeyMapAction__To__KeyMapCode(data: any, search: string): number {
    for (let kk = 0; kk < data.keyboard.keyMapSet[0].keyMap.length; kk++) {
      for (let jj = 0; jj < data.keyboard.keyMapSet[0].keyMap[kk].key.length; jj++) {
        if (data.keyboard.keyMapSet[0].keyMap[kk].key[jj]['@_action'] === search) {
          return data.keyboard.keyMapSet[0].keyMap[kk].key[jj]['@_code']
        }
      }
    }
    return 999
  }
  public lookup_11_KeyMapAction__To__KeyIndex(data: any, search: string): number {
    for (let kk = 0; kk < data.keyboard.keyMapSet[0].keyMap.length; kk++) {
      for (let jj = 0; jj < data.keyboard.keyMapSet[0].keyMap[kk].key.length; jj++) {
        if (data.keyboard.keyMapSet[0].keyMap[kk].key[jj]['@_action'] === search) {
          return data.keyboard.keyMapSet[0].keyMap[kk]['@_index']
        }
      }
    }
    return 999
  }
  // get all entries that result in state 3
  public lookup_12_ActionNext__from__ActionState(data: any, search: string): string {

    // loop all action
    for (let jj = 0; jj < data.keyboard.actions.action.length; jj++) {
      // loop all when
      for (let kk = 0; kk < data.keyboard.actions.action[jj].when.length; kk++) {
        // if next === a´search 
        if (data.keyboard.actions.action[jj].when[kk]['@_next'] === search) {
          // return actionIDName
          return data.keyboard.actions.action[jj]['@_id']
        }
      }
    }

    return ""
  }
  public lookup_13_ActionNext__To__ActionID(data: any, search: string) {
    for (let ll = 0; ll < data.keyboard.actions.action.length; ll++) {
      for (let mm = 0; mm < data.keyboard.actions.action[ll].when.length; mm++) {
        if ((data.keyboard.actions.action[ll].when[mm]['@_next'] === search)) {
          return data.keyboard.actions.action[ll]['@_id']

        }
      }
    }
    return ""
  }
  public lookup_14_ActionName__To__MapIndex(data: any, search: string): number[][] {
    const mapIndexArray_max: number[][] = []
    for (let kk = 0; kk < data.keyboard.keyMapSet[0].keyMap.length; kk++) {
      for (let jj = 0; jj < data.keyboard.keyMapSet[0].keyMap[kk].key.length; jj++) {
        const mapIndexArrayperKey: number[] = []

        if (data.keyboard.keyMapSet[0].keyMap[kk].key[jj]['@_action'] === search) {
          mapIndexArrayperKey.push(data.keyboard.keyMapSet[0].keyMap[kk].key[jj]['@_code'])
          mapIndexArrayperKey.push(kk)
        }
        if (mapIndexArrayperKey.length > 0)
          mapIndexArray_max.push(mapIndexArrayperKey)
      }
    }
    return mapIndexArray_max
  }


  /**
   * @brief  member function to return the unicode value of a character
   * @param  character the value that will converted
   * @return headecimal value of a character
   */
  public getHexFromChar(character: string): string {
    return character.charCodeAt(0).toString(16).slice(-4).toUpperCase().padStart(4, "0")
  }

  // TODO if the first in the list does not contain caps but later entries do contain caps-> no NCAPS is added(Todo check if caps/NCAPS are there after all entries are completed)
  /** 
   * @brief  member function to create a string of modifiers in kmn-style from the modifierMap section of .keylayout-file
   * @param  keylayout_modifier the modifier value used in the .keylayout-file
   * @return kmn_modifier the modifier value used in the .kmn-file
   */
  public create_kmn_modifier(keylayout_modifier: string, isCAPSused: boolean): string {
    let add_modifier = ""
    let kmn_modifier = ""
    let kmn_ncaps = ""

    // copy each modifier seperate element of array
    const modifier_state: string[] = keylayout_modifier.split(" ");

    // TODO review these conditions-> what else do I need? 
    // TODO spelling entries of .keylayout( uppercase/lowercase, control<->ctrl,...)
    // TODO anyOption?
    // opt?+ LOPT?+ ROPT? -> what will be result??

    for (let i = 0; i < modifier_state.length; i++) {
      /*if (String(modifier_state[i]) === "Command")
        continue*/

      if (isCAPSused && (String(keylayout_modifier).indexOf("caps") === -1))
        kmn_ncaps = " NCAPS "

      // TODO go over, find more conditions & simplify
      if (String(modifier_state[i]) === "anyOption") add_modifier = "RALT "
      else if (String(modifier_state[i]) === "anyShift") add_modifier = "SHIFT "
      else if (String(modifier_state[i]) === "anyControl") add_modifier = "CTRL "

      else if (String(modifier_state[i]) === "anyOption?") add_modifier = ""    // does anyOption?... happen?
      else if (String(modifier_state[i]) === "anyShift?") add_modifier = ""
      else if (String(modifier_state[i]) === "anyControl?") add_modifier = ""

      // TODO naming RSHIFT, rshift, Rshift,...?
      else if ((String(modifier_state[i]) === "shift?") && modifier_state.includes('rshift')) add_modifier = "RSHIFT "
      else if ((String(modifier_state[i]) === "shift?") && modifier_state.includes('lshift')) add_modifier = "LSHIFT "
      else if ((String(modifier_state[i]) === "shift?") && modifier_state.includes('rshift') && modifier_state.includes('lshift')) add_modifier = "SHIFT "

      else if ((String(modifier_state[i]) === "option?") && modifier_state.includes('rOption')) add_modifier = "ROPT "
      else if ((String(modifier_state[i]) === "option?") && modifier_state.includes('lOption')) add_modifier = "LOPT "
      else if ((String(modifier_state[i]) === "option?") && modifier_state.includes('rOption') && modifier_state.includes('lOption')) add_modifier = "OPT "

      else if ((String(modifier_state[i]) === "ctrl?") && modifier_state.includes('rControl')) add_modifier = "RCTRL "
      else if ((String(modifier_state[i]) === "ctrl?") && modifier_state.includes('lControl')) add_modifier = "LCTRL "
      else if ((String(modifier_state[i]) === "ctrl?") && modifier_state.includes('rControl') && modifier_state.includes('lControl')) add_modifier = "CTRL "

      // remove if modifier contains '?' except for 'caps?'  e.g. 'shift?', 'ctrl?', ...
      // TODO is this correct: caps? => caps is not neccessary. If its not neccessary we need to write NCAPS. Correct?
      else if (String(modifier_state[i]) === "caps?") add_modifier = "NCAPS "
      else if (String(modifier_state[i]).charAt(modifier_state[i].length - 1) === "?") add_modifier = ""

      else add_modifier = String(modifier_state[i]) + " "

      kmn_modifier += kmn_ncaps + add_modifier
    }

    // remove duplicate and empty entries
    const duplicate_modifier_array: string[] = kmn_modifier.split(" ").filter(item => item)
    const unique_Modifier: string[] = duplicate_modifier_array.filter(function (item, pos, self) {
      return self.indexOf(item) == pos;
    })

    return unique_Modifier.join(" ").replace(/\s+/g, " ").trim().toUpperCase()
  }

  public checkIfCapsUsed(keylayout_modifier: string[][]): boolean {
    for (let i = 0; i < keylayout_modifier.length; i++) {
      for (let j = 0; j < keylayout_modifier[i].length; j++) {
        const modifier_state: string[] = keylayout_modifier[i][j].split(" ");
        for (let k = 0; k < modifier_state.length; k++) {
          if ((modifier_state[k].indexOf("caps") !== -1) && (modifier_state[k].indexOf("caps?") === -1))
            return true
        }
      }
    }
    return false
  }

  /**
 * @brief  member function to map Ukelele keycodes to a Windows Keycodes
 * @param  pos Ukelele (=mac) keycodes
 * @return keycode on Win Keyboard
 */
  // TODO finish all entries
  public map_UkeleleKC_To_VK(pos: number): string {
    // ukelele KC  -->  // VK_US
    if (pos === 0x0A) return "K_BKQUOTE"    /* ^ */
    if (pos === 0x12) return "K_1"          /* 1 */
    if (pos === 0x13) return "K_2"          /* 2 */
    if (pos === 0x14) return "K_3"          /* 3 */
    if (pos === 0x15) return "K_4"          /* 4 */
    if (pos === 0x17) return "K_5"          /* 5 */
    if (pos === 0x16) return "K_6"          /* 6 */
    if (pos === 0x1A) return "K_7"          /* 7 */
    if (pos === 0x1C) return "K_8"          /* 8 */
    if (pos === 0x19) return "K_9"          /* 9 */
    if (pos === 0x1D) return "K_0"          /* 0 */
    if (pos === 0x1B) return "K_HYPHEN"     /* ß */
    if (pos === 0x18) return "K_EQUAL"      /* ´ */

    if (pos === 0x0C) return "K_Q"          /* Q */
    if (pos === 0x0D) return "K_W"          /* W */
    if (pos === 0x0E) return "K_E"          /* E */
    if (pos === 0x0F) return "K_R"          /* R */
    if (pos === 0x11) return "K_T"          /* T */
    if (pos === 0x10) return "K_Y"          /* Y */
    if (pos === 0x20) return "K_U"          /* U */
    if (pos === 0x22) return "K_I"          /* I */
    if (pos === 0x1F) return "K_O"          /* O */
    if (pos === 0x23) return "K_P"          /* P */
    if (pos === 0x21) return "K_LBRKT"      /* [ */
    if (pos === 0x1E) return "K_RBRKT"      /* ] */
    //if (pos === 0x32) return "K_BKSLASH"    /* \ */
    if (pos === 0x30) return "K_BKSLASH"    /* \ */   // for ANSI  correct??
    if (pos === 0x2A) return "K_BKSLASH"    /* \ */   // for ISO  correct??

    if (pos === 0x00) return "K_A"          /* A */
    if (pos === 0x01) return "K_S"          /* S */
    if (pos === 0x02) return "K_D"          /* D */
    if (pos === 0x03) return "K_F"          /* F */
    if (pos === 0x05) return "K_G"          /* G */
    if (pos === 0x04) return "K_H"          /* H */
    if (pos === 0x26) return "K_J"          /* J */
    if (pos === 0x28) return "K_K"          /* K */
    if (pos === 0x25) return "K_L"          /* L */
    if (pos === 0x29) return "K_COLON"      /* : */
    if (pos === 0x27) return "K_QUOTE"      /* " */

    if (pos === 0x23) return "K_oE2"        /* | */
    if (pos === 0x06) return "K_Z"          /* Z */
    if (pos === 0x07) return "K_X"          /* X */
    if (pos === 0x08) return "K_C"          /* C */
    if (pos === 0x09) return "K_V"          /* V */
    if (pos === 0x0B) return "K_B"          /* B */
    if (pos === 0x2D) return "K_N"          /* N */
    if (pos === 0x2E) return "K_M"          /* M */
    if (pos === 43) return "K_COMMA"        /* , */
    if (pos === 47) return "K_PERIOD"       /* . */
    if (pos === 44) return "K_SLASH"        /* / */

    if (pos === 36) return "K_ENTER"
    if (pos === 49) return "K_SPACE"
    else return ""
  }

  public useElementAsString(u18ArrayElemnent: Uint8Array): string {
    return new TextDecoder().decode(u18ArrayElemnent)
  }
  public useAsString(u18ArrayElemnent: Uint8Array[]): string {
    let str = ""
    for (let i = 0; i < u18ArrayElemnent.length; i++) {
      str = str + " " + new TextDecoder().decode(u18ArrayElemnent[i])
    }
    return str
  }
  //----------------------------------------------------------------------------------------------------
  //----------------------------------------------------------------------------------------------------
  //----------------------------------------------------------------------------------------------------

  public writeLastPart(data_ukelele: convert_object, isCAPSused: boolean): string {
    // create a string array .......................................................
    const workArray2D: string[][] = []
    for (let k = 0; k < data_ukelele.ArrayOf_processed_RuleData.length; k++) {
      const array1D: string[] = []
      for (let l = 0; l < data_ukelele.ArrayOf_processed_RuleData[k].length; l++) {
        const u8data = data_ukelele.ArrayOf_processed_RuleData[k][l]
        array1D.push(this.useElementAsString(u8data))
      }
      workArray2D.push(array1D)
    }
    console.log("workArray2D", workArray2D)
    //°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    // DEADKEYS °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    //°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    //const dk_ss_array: string[][] = []

    // ToDo get data from WorkArray
    // filter for dk values
    let data: string = ""

    //data += '\n.. dk part here (+ [K_BKQUOTE] > dk(005e))\n\n'

    console.log("start dk-part ")
    console.log("data_ukelele.ArrayOf_Element_KeyAction.length ", data_ukelele.ArrayOf_Element_KeyAction.length)
    console.log(" data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect.length", data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect.length)


    /*
        // all 109 keys with action a9
        for (let i = 0; i < data_ukelele.ArrayOf_Element_KeyAction.length; i++) {
          // loop through keyMapSelect (8)
          for (let ii = 0; ii < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect.length; ii++) {
            //for (let ii = 0; ii < 8; ii++) {
            // loop through modifiers
            //for (let j = 0; j < data_ukelele.ArrayOf_Element_KeyAction.length; j++) {
            // 8 behaviors
            for (let j = 0; j < data_ukelele.ArrayOf_Element_ModifierMaps_ALLKeyMapSelect.length; j++) {
              //console.log("start write 3A", i, ii, j)
    
              // get the modifier for the layer
              //const label_modifier = this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ALL_ModifierMaps[j], isCAPSused)
              const label_modifier = this.create_kmn_modifier(data_ukelele.ArrayOf_Element_ALL_ModifierMaps[j], isCAPSused)
              console.log("label_modifier ", label_modifier)
    
              // get the character from keymap-section of .keylayout-file that will be written as result in kmn-file
              //const resulting_character = new TextDecoder().decode(data_ukelele.ArrayOf_Element_KeyAction[j][i])
              // wrong since it attempts to convert a9 and not 4 ( the termínator id )
              const resulting_character = new TextDecoder().decode(data_ukelele.ArrayOf_Element_KeyAction[ii][i])
              console.log("resulting_character ",resulting_character )
              console.log("data_ukelele.ArrayOf_Element_KeyAction ",data_ukelele.ArrayOf_Element_KeyAction )
    
              // therefore get character from ~ <- 4 <- a9
              //const realChar = this.getCharacterFromActionName(data_ukelele, data_ukelele.ArrayOf_Element_KeyAction[ii][i])
              //console.log("realChar", realChar,i,ii,j )
              // console.log("data_ukelele.ArrayOf_Element_KeyAction[ii][i],",
              //  data_ukelele.ArrayOf_Element_KeyAction[ii][i],
              //   realChar)
    
              if (resulting_character !== "") {
                console.log("start write 3B+3C", i, ii, j, label_modifier, "---", resulting_character)
    
                const VK: (string | number)[][] = data_ukelele.ArrayOf_processed_VK_from_keylayout
                const vk_label = VK.filter(item => item[0] === data_ukelele.ArrayOf_processed_VK_from_keylayout[i][0])
                //  console.log("VK", VK, "vk_label", vk_label)
    
                //  console.log("start write 3D,data_ukelele.ArrayOf_processed_dk", data_ukelele.ArrayOf_processed_dk)
                for (let k = 0; k < data_ukelele.ArrayOf_processed_dk.length; k++) {
                  console.log("start write 3E")
                  console.log("resulting_character", resulting_character, "data_ukelele.ArrayOf_processed_dk[k][1]", data_ukelele.ArrayOf_processed_dk[k][1])
                  console.log("data_ukelele.ArrayOf_processed_dk is", data_ukelele.ArrayOf_processed_dk)
                  console.log("resulting_characte, data_ukelele.ArrayOf_processed_dk is", resulting_character, data_ukelele.ArrayOf_processed_dk[k][2])
                  if ((resulting_character !== "") && (resulting_character === data_ukelele.ArrayOf_processed_dk[k][2])) {
                    data += '[' + (label_modifier + ' ' + vk_label[0][1]).trim() + '] ' + "> dk(" + this.getHexFromChar(data_ukelele.ArrayOf_processed_dk[k][1]) + ") " + '\n'
                    console.log("start write 3D", data)
    
                  }
                }
              }
            }
    
          }
        }
    */
    data += '\########## OK #################################################################\n'

    data += '\nNOW MY C4 RULES **ccc ********* (only to get 02C6 ect) *********************\n\n'
    console.log("data_ukelele.ArrayOf_processed_RuleData", data_ukelele.ArrayOf_processed_RuleData);
    console.log("data_ukelele.ArrayOf_processed_RuleData.length ", data_ukelele.ArrayOf_processed_RuleData.length)

    const dk_line_array = workArray2D.filter(function ([A, B, C, D, E, F]) {
      return (E === "isTerminator")
    });
    console.log("dk_line_array", dk_line_array.length, dk_line_array)

    // remove duplicates
    const [uniqueDeadkeys] = dk_line_array.reduce((acc, curr) => {
      const [uniq, set] = acc;
      if (!set.has(curr.join(','))) {
        set.add(curr.join(','));
        uniq.push(curr);
      }
      return acc;
    }, [[], new Set()],
    );
    console.log("uniquedk_line_array,uniqueDeadkeys", uniqueDeadkeys.length, uniqueDeadkeys);


    for (let kkk = 0; kkk < uniqueDeadkeys.length; kkk++) {
      let line: string = ""
      line = "+  [" + uniqueDeadkeys[kkk][1] + " " + uniqueDeadkeys[kkk][2] + "]  >   dk(" + uniqueDeadkeys[kkk][6] + ")"
      data += line + '\n'
    }
    console.log(" data all together", data);




    // remove duplicates
    /*const [uniqueDeadkeys] = dk_ss_array.reduce((acc, curr) => {
      const [uniq, set] = acc;
      if (!set.has(curr.join(','))) {
        set.add(curr.join(','));
        uniq.push(curr);
      }
      return acc;
    }, [[], new Set()],
    );
    console.log("uniqueDeadkeys", uniqueDeadkeys);*/


    // array ony of dk (array_dk_char [ '¨', 'ˆ', '´', '˜', '`' ])
    const array_dk_char = [...new Set(data_ukelele.ArrayOf_processed_dk.map(a => a[1]))];
    console.log("array_dk_charN", array_dk_char);





    // we have 5 different types: 
    /*
    C0: size: 4 [ 'first_modifier_C0',  '33RIGHTSHIFT NCAPS', 'K_S',      'S' ]
    C1: size: 4 [ 'first_modifier_C1',  'CAPS',               'K_N',      'N' ],
    C2: size: 5 [ 'first_modifier_C2',   'CAPS',              'K_E',      'K_9',         'È' ],
        use 2.(K_E)  and 4.('È') to get Name and DEADKEYED
    C3: size: 6 ['first_modifier_C3',    'NCAPS RALT',        'K_8',      'NCAPS RALT',   'K_U',    'ˆ'  
    C4: size: 4 ['first_modifier_C4',    'NCAPS 0',           'K_SPACE',  '' ],
        size: 7 ['first_modifier_C4',    'NCAPS RALT',        'K_N',      '˜',  'isTerminator',      '˜',      '02DC'    ],
        use 2.(K_N) , 5. (isTerminator) and 6.('02DC') to get Name and DEADKEY in hex
    
    Find DEADKEYABLE
     1) first_modifier_C1   CAPS   K_A   A  
     2) first_modifier_C2   CAPS   K_A   K_EQUAL   Â  
     3) first_modifier_C2   CAPS   K_A   K_9   À  
     - Find C2 rule with Deadkeyed ( n= 5)
     - get C1 rule with first n=3 entries -> (first_modifier_C1   CAPS   K_A) 
     - get 4th entry == DEADKEYABLE
    
    Find DEADKEY
    1) look in modifier_C4 (n=7) 
    2) get 7th entry (=02DC)
    
    Find DEADKEYED
    1)look in modifier_C2
    2) get 5th entry (='È')
    2) get 3th entry (='K_E')
    
    
    */

    // create deadkeyables_raw, deadkeyed_raw array .......................................................
    let deadkeyables_raw: string[][] = []
    let deadkeyed_raw: string[][] = []


    // find deadkeyable [ 'first_modifier_C1', 'CAPS', 'K_A', 'A', '', '', '' ]
    deadkeyables_raw = workArray2D.filter(function ([A, B, C, D, E, F, G]) {
      return ((A === "first_modifier_C1") /*&& (B === shiftstate) && (C === key_name)*/
        && ((C !== "K_SPACE") && (D !== ""))
      )
    });
    //console.log("deadkeyables_raw", deadkeyables_raw)


    // find deadkeyed [ 'first_modifier_C2', 'CAPS', 'K_A', 'K_N', 'Ã' ]
    deadkeyed_raw = workArray2D.filter(function ([A, B, C, D, E, F, G]) {
      return ((A === "first_modifier_C2")/* && (B === shiftstate) && (C === key_name)*/
        && ((C !== "K_SPACE"))
      )
    });
    console.log("deadkeyables_raw", deadkeyables_raw.length, deadkeyables_raw)
    console.log("deadkeyed_raw", deadkeyed_raw.length, deadkeyed_raw)


    const dkable_dked_array2D: any[][] = []
    // loop dedkeyables, take first
    for (let rr = 0; rr < deadkeyables_raw.length; rr++) {
      // loop dedkeyed, take first
      for (let ss = 0; ss < deadkeyed_raw.length; ss++) {
        const dkable_dked_array: any[] = []
        //if available ( shifts the same, keyname the same)
        if ((deadkeyables_raw[rr][1] === deadkeyed_raw[ss][1])
          && ((deadkeyables_raw[rr][2] === deadkeyed_raw[ss][2]))) {
          //copy dedkeyables name, deadkeyable
          //copy dedkeyed Secondname, deadkeyed
          // dkable_dked_array.push(deadkeyables_raw[rr][1])
          dkable_dked_array.push(deadkeyables_raw[rr][3])
          dkable_dked_array.push(deadkeyed_raw[ss][3])
          dkable_dked_array.push(deadkeyed_raw[ss][4])
        }
        // dkable_dked_array2D.push(dkable_dked_array)
        if (dkable_dked_array.length > 0)
          dkable_dked_array2D.push(dkable_dked_array)
      }
    }
    console.log("dkable_dked_array2D", dkable_dked_array2D.length, dkable_dked_array2D)


    // remove duplicates
    const [uniqueDeadkeyables] = dkable_dked_array2D.reduce((acc, curr) => {
      const [uniq, set] = acc;
      if (!set.has(curr.join(','))) {
        set.add(curr.join(','));
        uniq.push(curr);
      }
      return acc;
    }, [[], new Set()],
    );
    console.log("uniqueDeadkeyables", uniqueDeadkeyables.length, uniqueDeadkeyables);


    const deadkeyedArray: string[][] = Array.from({ length: uniqueDeadkeys.length }, () => new Array(2).fill(''));
    console.log("deadkeyedArray", deadkeyedArray)
    const deadkeyablesArray: string[][] = Array.from({ length: uniqueDeadkeys.length }, () => new Array(1).fill(''));
    console.log("deadkeyablesArray", deadkeyablesArray)

    console.log("uniqueDeadkeys", uniqueDeadkeys)

    for (let i = 0; i < uniqueDeadkeys.length; i++) {
      for (let j = 0; j < uniqueDeadkeyables.length; j++) {
        console.log("uniqueDeadkeys[i][2]  ", uniqueDeadkeys[i][2])
        console.log("uniqueDeadkeyables[j][1] ", uniqueDeadkeyables[j][1])

        if (uniqueDeadkeys[i][2] === uniqueDeadkeyables[j][1]) {
          deadkeyablesArray[i][0] = deadkeyablesArray[i][0] + "\'" + uniqueDeadkeyables[j][0] + "\'  "
          deadkeyedArray[i][0] = deadkeyedArray[i][0] + "\'" + uniqueDeadkeyables[j][2] + "\'  "
        }
      }
      deadkeyedArray[i][1] = (uniqueDeadkeys[i][6])
    }


    data += "\n"
    data += '\########## OK #################################################################\n'
    
    data += '\nmatch > use(deadkeys)\n\n'
    data += '\ngroup(deadkeys)\n\n'
    // finally write out
    for (let i = 0; i < deadkeyablesArray.length; i++) {

      data += "\n\ndk: " + deadkeyedArray[i][1]
      console.log("dk: ", deadkeyedArray[i][1])

      data += "\ndeadkeyablesArray" + deadkeyablesArray[i][0]
      console.log("deadkeyablesArray", deadkeyablesArray[i][0])

      data += "\ndeadkeyedArray   " + deadkeyedArray[i][0]
      console.log("deadkeyedArray   ", deadkeyedArray[i][0])
    }
    return data
  }

  public writeLines(inArray: string[]): string {
    let editedLine: string = ""
    for (let i = 0; i < inArray.length; i++) {
      if (inArray[i] !== " ")
        editedLine = editedLine + "\'" + inArray[i] + "\' "
      else
        editedLine = editedLine + "    "
    }
    return editedLine
  }



}
