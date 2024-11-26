/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Converts macOS/Ukelele .keylayout files to Keyman .kmn
 */
import { CompilerCallbacks, CompilerOptions } from "@keymanapp/developer-utils";
import { ConverterToKmnArtifacts } from "../converter-artifacts.js";

// TODO keylayout->kmn
//modifiers: The identifier of the <modifierMap> element to use for this range of hardware keyboard types.
//defaultIndex: The table number to use for modifier key combinations which are not explicitly specified by any <modifier> element within the <modifierMap>.
// write read, convert, write
// tests for 3 functions read write convert
// add data to object
// Use filter functions
// action/output:use filter etc to shorten func
// deadkeyables:use filter etc to shorten func
// dk-> for all action:use filter etc to shorten func
// remove unneccessa´ry data from dataObject
// rename symbols
// remove part using kmn_key_Name1
// remove unnecceaasry map_UkeleleKC_To_kmn_Key_Name_Array_Position_n etc
// loop throught ANSI, JIS- art moment only use [keyMapSet_count] (keyMapSet_count=0)
// remove funcs at teh end
// import { makePathToFixture } from '../../test/helpers/index.js';       // _S2 my imports
// Mapping 0->30  or 0->K_A-> missing entries in mapping 
// Replace any-types
// Several steps action-> action-> action->character ( not only  action->character)
// Usable for all keylayout files
// Return conditions
// Use callbacks as for writeFileSync
// Tests throws
// Conditions NCAPS,OPT;...
// Which stores
// TODO move func outside of class
// Functions as object methods? 
// objects contain only used stuff READ in: -- out: only read arrays / CONVERT in: only read arrays out: return only to write arrays
// Use catch blocks for file read

// use


// TODO waht about using actions twice in a row???
// _S2 imports Sabine
import { XMLParser } from 'fast-xml-parser';  // for reading a file
import { readFileSync } from 'fs';
import { writeFileSync } from "fs";           // for writing a file
import * as fs from 'fs';   // what is this/do I need it? -  either import all or seperately like above


export class KeylayoutToKmnConverter {
  static readonly INPUT_FILE_EXTENSION = '.keylayout';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';

  // TODO use callbacks
  constructor(/*private*/ _callbacks: CompilerCallbacks, /*private*/ _options: CompilerOptions) {
 // constructor(private callbacks: CompilerCallbacks, /*private*/ _options: CompilerOptions) {
    // TODO: if these are needed, uncomment /*private*/ and remove _, and they will then
    // be available as class properties
  }

  /**
   * @brief  member function to run read/convert/write
   * @param  inputFilename the ukelele .keylayout-file to be converted
   * @param  outputFilename the resulting keyman .kmn-file
   * @param  binaryData ... _S2
   * @return
   */
  async run(inputFilename: string, outputFilename: string): Promise<ConverterToKmnArtifacts> {

    if (!inputFilename || !outputFilename) {
      throw new Error('Invalid parameters');
    }

    console.log(' _S2 first READ file ........................................in:', inputFilename);
    const inArray: object = this.read(inputFilename)

    if (!inArray) {
      return null;
    }

    console.log(' _S2 then CONVERT ........................................');
    const outArray: any = await this.convert(inArray);

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
   * @brief  member function to read filename ( a .keylayout-file) and write contents into Uint8Array keys_all_Layers
   * @param  filename the ukelele .keylayout-file to be converted
   * @return in case of success Uint8Array keys_all_Layers; else null
   */
  public read(filename: string): object {
    /*
    // _S2 answer :  + [K_A] > 'a'  is OK / TODO which format to use in output ?  + [K_A] > 'a' (character code)  or    + [K_A] > U+0061 (virt Keycode)
    // _S2 TODO which stores?
 */
    console.log("inputFilename read", filename)
    const options = {
      ignoreAttributes: false,
      attributeNamePrefix: '@_',    // to access the attribute
    };

    //const xmlFile = readFileSync(`${process.cwd()}/data/MySample.keylayout`, 'utf8')
    const xmlFile = readFileSync((process.cwd() + "\\data" + filename.substring(filename.lastIndexOf("\\"))).replace(" ", ""), 'utf8');

    // we don`t need file-read with uint8array return
    /*const fullPath = (process.cwd() + "\\data" + filename.substring(filename.lastIndexOf("\\"))).replace(" ", "")
    const xmlFile1 = this.callbacks.loadFile(fullPath)
    console.log("xmlFile1",xmlFile1)*/
    //console.log("xmlFile",xmlFile)

    const parser = new XMLParser(options);
    const jsonObj = parser.parse(xmlFile); // get plain Object

    const keyMapSet_count =0
    const nrOfStates = jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap.length
    const nrOfKeys_inLayer = jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[0].key.length   // will they all have the same length later ?

    // ToDo naming
    // TODO array-> object
    const duplicate_layouts_array: any[] = []
    const modifier_array: any[] = []                    // array holding all MODIFIER strings e.g. "anyShift caps anyOption"  -why not put modifiers into Uint8Array along with values of keys of layer
    const data_output_all_Layers: any[] = []
    const keys_action_all_Layers: any[] = []            // array holding all values with ACTION attribute (needed for finding deadkeys)
    const deadkeyedChars_all_Layers: any[] = []         // array holding all DEADKEYS for each mod state â, ê, ,....
    const terminators_all_Layers: any[] = []            // array holding all DEADKEYS for each mod state â, ê, ,....

    // .........................................................
    // LAYOUTS: get all groups like ANSI JIS
    // .........................................................

    for (let i = 0; i < jsonObj.keyboard.layouts.layout.length; i++) {
      duplicate_layouts_array[i] = jsonObj.keyboard.layouts.layout[i]['@_mapSet']
    }
    // remove duplicates
    const layouts_array: any[] = duplicate_layouts_array.filter(function (item, pos, self) {
      return self.indexOf(item) == pos;
    })

    // .........................................................
    // KEYMAP: get all keys for attribute "output" and "action"  - TODO can i use shorter function?
    // .........................................................

    // loop through all ss-combin
    for (let j = 0; j < nrOfStates; j++) {
      // get modifier list e.g. "anyshift caps? anyOption"
      modifier_array[j] = jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier['@_keys']

      // create a new array of keys_in_Layer (type Uint8tarray)
      const keys_output_One_Layer: Uint8Array[] = []
      const keys_action_One_Layer: Uint8Array[] = []

      // .........................................................
      // KEYMAP: get all keys for attribute "output" ( y,c,b,...)  - TODO can i use shorter function?
      // .........................................................
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[j].key.length; i++) {
        if (jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[j].key[i]['@_output'] !== "\0") {
          // textencoder converts string -> bytes  ( 'A' -> [ 65 ],   '☺' -> [ 226, 152, 186 ])
          // textencoder is of Uint8Array(1) for A and Uint8Array(3) for ☺
          keys_output_One_Layer[i] = new TextEncoder().encode(jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[j].key[i]['@_output']);
        }

        // .........................................................
        // KEYMAP: get all keys for attribute "action" ( ^,a,e,i,...)  - TODO can i use shorter function?
        // .........................................................
        if (jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[j].key[i]['@_action'] !== "\0") {
          keys_action_One_Layer[i] = new TextEncoder().encode(jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[j].key[i]['@_action']);
        }
      }
      // .........................................................
      // create array of "action" and array of "output"
      // data_all_Layers.push(keys_output_One_Layer)         // save all @output data ( will be used for conversion, write...)
      data_output_all_Layers.push(keys_output_One_Layer)        // save all @output data ( will be used for conversion, write...)
      keys_action_all_Layers.push(keys_action_One_Layer)   // save all @action data ( will be used for deadkeys)
    }
    // .........................................................
    // ACTION: create array of "deadkey" /  "deadkey names"  - TODO can i use shorter function?
    // .........................................................
    const dk_pairs_all_Layers: string[][] = []
    for (let jj = 0; jj < jsonObj.keyboard.actions.action.length; jj++) {
      // if there is a "next" attribute-> it is a dk
      if (jsonObj.keyboard.actions.action[jj].when['@_next'] !== undefined) {
        const vec1d: string[] = []
        vec1d.push(jsonObj.keyboard.actions.action[jj].when['@_next'])
        vec1d.push(jsonObj.keyboard.actions.action[jj]['@_id'])
        dk_pairs_all_Layers.push(vec1d)
      }
    }

    // .........................................................
    // ACTION: create array of deadkey base ^ ´ ` // TODO needed to add and distribute?
    // .........................................................
    const dk: string[] = [];
    for (let k = 0; k < dk_pairs_all_Layers.length; k++) {
      dk.push(dk_pairs_all_Layers[k][1])
    }

    // .........................................................
    // ACTION: create array of "deadkeyables_all_Layers"  - TODO can i use shorter function?
    // .........................................................
    const deadkeyables_all_Layers: string[][] = []
    for (let j = 0; j < jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap.length; j++) {
      const deadkeyables_one: string[] = []
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[0].key.length; i++) {
        const resulting_character = new TextDecoder().decode(keys_action_all_Layers[j][i])
        if (resulting_character !== "")
          deadkeyables_one.push(resulting_character)
      }

      if (deadkeyables_one.length !== 0) {
        deadkeyables_all_Layers.push(deadkeyables_one)
        // filter out dk -> plain deadkeyables_all_Layers ( a,^, e,i,´,u   => a,e,i,u)
        // return all that don`t find ^,´, `
        deadkeyables_all_Layers[j] = deadkeyables_all_Layers[j].filter(function (el) {
          return dk.indexOf(el) < 0;
        });
      }
    }
    // .........................................................
    // TERMINATOR: create array of "terminators"  - TODO can i use shorter function? what element of terminators do i need
    // .........................................................
    for (let i = 0; i < jsonObj.keyboard.terminators.when.length; i++) {
      terminators_all_Layers[i] = jsonObj.keyboard.terminators.when[i]
    }

    // .........................................................
    // TODO can i use shorter function?  -> yes use terminators

    // loop through all dk, key-actions and find @_action
    // find this value in <actions><action id=...>
    // in their 'when' find output for dk
    // for all 'action' at keys paragraph
    for (let j = 0; j < dk_pairs_all_Layers.length; j++) {
      const deadkeys_One_dk: any[] = []
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
                  deadkeys_One_dk.push(jsonObj.keyboard.actions.action[l].when[m]['@_output'])
                }
              }
            }
          }
        }
      }
      deadkeyedChars_all_Layers.push(deadkeys_One_dk)
    }

    // TODO remove unneccassary elements
    const DataObject = {
      name: "Ukelele-kmn",
      ArrayOf_Layouts: layouts_array,
      ArrayOf_Ukelele_output: data_output_all_Layers,
      ArrayOf_Ukelele_action: keys_action_all_Layers,
      ArrayOf_Modifiers: modifier_array,
      //ArrayOf_VK_US: "",
      ArrayOf_SC_MacWin: "",
      ArrayOf_VK_DirectFromUku: "",
      ArrayOf_dk: dk_pairs_all_Layers,                  // add dk-mapping ( dk1 <-> '^' )
      ArrayOf_Dk: dk,                                   // add plain dk ( '^', '´','`')
      ArrayOf_deadkeyables: deadkeyables_all_Layers,               // add char that can be modified with dk ( a,e,i,o,u)
      ArrayOf_deadkeyedChar: deadkeyedChars_all_Layers,  // add modified keys ( â,ê,î,ô,û)
      ArrayOf_Terminators: terminators_all_Layers  // add terminators ( ^,´,`)
    };
    //console.log("DataObject", DataObject)
    // TODO review condition
    return DataObject
    return ((data_output_all_Layers.length === nrOfStates + 5) && data_output_all_Layers[0].length === nrOfKeys_inLayer) ? data_output_all_Layers : null;
  }

  /**
   * @brief  member function to convert data of .keylayout-file to kmn-file This will convert/rename modifiers, position of Keys and deadkeys and save into an array 
   * @param  data_ukelele (Uint8Array) data of the ukelele .keylayout-file
   * @return outArray Uint8Array keys_all_Layers, the converted data for kmn-files if all layers have been converted; else null
   */
  public convert(data_ukelele: any): any {

    // TODO remove
   /* const kmn_Key_Name1 = [
      'K_BKSP', 'K_TAB', 'K_ENTER', 'K_SHIFT', 'K_CONTROL', 'K_ALT', 'K_PAUSE', 'K_CAPS',  //7
      'K_ESC', 'K_SPACE', 'K_PGUP', 'K_PGDN', 'K_END', 'K_HOME', 'K_LEFT', 'K_UP', 'K_RIGHT', //16
      'K_DOWN', 'K_SEL', 'K_PRINT', 'K_EXEC', 'K_INS', 'K_DEL', 'K_HELP',   //23

      'K_0', 'K_1', 'K_2', 'K_3', 'K_4', 'K_5', 'K_6', 'K_7', 'K_8', 'K_9',    //33

      'K_A', 'K_B', 'K_C', 'K_D', 'K_E', 'K_F', 'K_G', 'K_H', 'K_I', 'K_J', 'K_K', 'K_L', 'K_M',    //46
      'K_N', 'K_O', 'K_P', 'K_Q', 'K_R', 'K_S', 'K_T', 'K_U', 'K_V', 'K_W', 'K_X', 'K_Y', 'K_Z',    //59

      'K_NP0', 'K_NP1', 'K_NP2', 'K_NP3', 'K_NP4',
      'K_NP5', 'K_NP6', 'K_NP7', 'K_NP8', 'K_NP9',  //69

      'K_NPSTAR', 'K_NPPLUS', 'K_SEPARATOR', 'K_NPMINUS', 'K_NPDOT', 'K_NPSLASH',   //75

      'K_F1', 'K_F2', 'K_F3', 'K_F4', 'K_F5', 'K_F6',
      'K_F7', 'K_F8', 'K_F9', 'K_F10', 'K_F11', 'K_F12',    // 87

      'K_NUMLOCK', 'K_SCROLL', 'K_LSHIFT',
      'K_RSHIFT', 'K_LCONTROL', 'K_RCONTROL',
      'K_LALT', 'K_RALT',     //95

      'K_COLON', 'K_EQUAL', 'K_COMMA', 'K_HYPHEN',    //99
      'K_PERIOD', 'K_SLASH', 'K_BKQUOTE', 'K_LBRKT', 'K_RBRKT',  K_QUOTE  //105
    ]*/

    const data_VKUS: any[][] = [];
    const data_kmn: any[][] = [];

    /*const data_kmn_pair: any[] = [];
    const data_VKUS_pos_pair: any[] = [];*/

   /* const data_VK_US_K_pair: any[] = [];
    const data_VK_US_K_: any[][] = [];*/

    const data_mac_Win: any[][] = [];
    const data_vk_directfromUku: any[][] = [];

    
    // use UkeleleKeyCodeToScanCodes !!!
    for (let i = 0; i < data_ukelele.ArrayOf_Ukelele_output[0].length; i++) {

      const data_mac_US_pair: any[] = [];
      const data_vk_directfromUku_pair: any[] = [];

    const Name_Array_Pos = this.map_UkeleleKC_To_kmn_Key_Name_Array_Position_n(i)
    const keyName1 = this.map_UkeleleKC_To_kmn_Key_Name_Array_Position_new(Name_Array_Pos)

    const nr_Win = this.map_UkeleleKC_To_Win_KC(i)
    const keyName2 = this.map_UkeleleKC_To_kmn_Key_Name_Array_Position_new_2(nr_Win)
   // const keyName = this.map_UkeleleKC_To_kmn_Key_Name_Array_Position_new(nr_Win)
     // console.log("i",i,"nr_Win",nr_Win,"keyName",keyName)
      // data_mac_US_pair.push(this.map_UkeleleKC_To_Win_KC(i))

    console.log("i",i, " -- nr_Win",nr_Win, " -- keyName","-",keyName1, Name_Array_Pos, "!!!",i, keyName2 )
      data_mac_US_pair.push(nr_Win)
      data_mac_US_pair.push(keyName1)  // this gives the part: K_A
      data_mac_Win.push(data_mac_US_pair)


      const vk_from_Ukelele = this.map_UkeleleKC_To_VK(i)
      data_vk_directfromUku_pair.push(i)
      data_vk_directfromUku_pair.push(vk_from_Ukelele)
      data_vk_directfromUku.push(data_vk_directfromUku_pair)


      // write    0 - K_A
      /*  data_VK_US_K_pair.push(i)
      data_VK_US_K_pair.push(keyName)
      data_VK_US_K_.push(data_VK_US_K_pair)
    //console.log("data_VK_US_K_pair",data_VK_US_K_pair)*/
    }

    console.log("data_vk_directfromUku",data_vk_directfromUku)
    //  console.log(".....data_mac_Win",data_mac_Win)
    data_ukelele.ArrayOf_VK_US = data_VKUS
    data_ukelele.ArrayOf_Kmn = data_kmn
    data_ukelele.ArrayOf_SC_MacWin = data_mac_Win
    data_ukelele.ArrayOf_VK_DirectFromUku = data_vk_directfromUku

  //  console.log("data_ukelele.ArrayOf_SC_MacWin", data_ukelele.ArrayOf_SC_MacWin)

    return data_ukelele
  }


  /**
   * @brief   member function to write data to file
   * @param  kmn_array the array holding keyboard data
   * @return true if data has been written; false if not
   */
  //TODO need to use export const USVirtualKeyCodes here
  public write(kmn_array: any): boolean {
    /*const kmn_Key_Name = [
      'K_BKSP', 'K_TAB', 'K_ENTER', 'K_SHIFT', 'K_CONTROL', 'K_ALT', 'K_PAUSE', 'K_CAPS',  //7
      'K_ESC', 'K_SPACE', 'K_PGUP', 'K_PGDN', 'K_END', 'K_HOME', 'K_LEFT', 'K_UP', 'K_RIGHT', //16
      'K_DOWN', 'K_SEL', 'K_PRINT', 'K_EXEC', 'K_INS', 'K_DEL', 'K_HELP',   //23

      'K_0', 'K_1', 'K_2', 'K_3', 'K_4', 'K_5', 'K_6', 'K_7', 'K_8', 'K_9',    //33

      'K_A', 'K_B', 'K_C', 'K_D', 'K_E', 'K_F', 'K_G', 'K_H', 'K_I', 'K_J', 'K_K', 'K_L', 'K_M',    //46
      'K_N', 'K_O', 'K_P', 'K_Q', 'K_R', 'K_S', 'K_T', 'K_U', 'K_V', 'K_W', 'K_X', 'K_Y', 'K_Z',    //59

      'K_NP0', 'K_NP1', 'K_NP2', 'K_NP3', 'K_NP4',
      'K_NP5', 'K_NP6', 'K_NP7', 'K_NP8', 'K_NP9',

      'K_NPSTAR', 'K_NPPLUS', 'K_SEPARATOR', 'K_NPMINUS', 'K_NPDOT', 'K_NPSLASH',   //75

      'K_F1', 'K_F2', 'K_F3', 'K_F4', 'K_F5', 'K_F6',
      'K_F7', 'K_F8', 'K_F9', 'K_F10', 'K_F11', 'K_F12',    // 85

      'K_NUMLOCK', 'K_SCROLL', 'K_LSHIFT',
      'K_RSHIFT', 'K_LCONTROL', 'K_RCONTROL',
      'K_LALT', 'K_RALT',     //93

      'K_COLON', 'K_EQUAL', 'K_COMMA', 'K_HYPHEN',    //97
      'K_PERIOD', 'K_SLASH', 'K_BKQUOTE', 'K_LBRKT',    //101
    ]*/

      console.log("ArrayOf_VK_DirectFromUku",kmn_array.ArrayOf_VK_DirectFromUku )

    //  *************************************************************
    //  **** write stores *******************************************
    //  *************************************************************
    let data = "\n"
    data += "c\n"
    data += "c Keyman keyboard generated by kmn-convert\n"
    data += "c\n"
    data += "\n"
    data += "store(&VERSION) \'...\'\n"
    data += "store(&TARGETS) \'any\'\n"
    data += "store(&KEYBOARDVERSION) \'...\'\n"
    data += "store(&COPYRIGHT) '© 2024 SIL International\n"
    // TODO what else ??

    data += "begin Unicode > use(main)\n\n"
    data += "group(main) using keys\n\n"

    data += "Tipp: if K_? is replaced by undefined-> no enry in kmn_Key_Name=> add K_? there and it will be shown here\n"
    data += "Tipp: keys that are marked with sction do not aoear in ukelele_Array_output->do not appear in kmn\n"
    data += "\n"

    //  *************************************************************
    //  **** write rules ********************************************
    //  *************************************************************^




    /*
        const numbers: number[] = [11, 23, 45, 89, 7, 98];
        const filteredNumbers: number[] = numbers.filter((num) => num > 10);
        console.log(filteredNumbers);

        const words: string[] = ["apple", "banana", "cherry", "date"];
        const filteredWords: string[] = words.filter((word) => word.length > 5);
        console.log(filteredWords);

        // now with kmc-convert:
        const varToFind= kmn_array.ArrayOf_VK_US[2][0]
        const numbersMy: number[][] = kmn_array.ArrayOf_VK_US
        const filteredNumbersMy = numbersMy.filter(innerArray => innerArray[0] === varToFind);

        console.log("ArrayOf_VK_US",kmn_array.ArrayOf_VK_US );
        console.log("kmn_array.ArrayOf_VK_US[x][0]",varToFind );
        console.log("filteredNumbersMy",filteredNumbersMy);*/
    /*
const vk_label_test = this.find_VK_X_in_ArrayOf_VK_US(kmn_array.ArrayOf_VK_US[0][0], kmn_array)
console.log("kmn_array",kmn_array.ArrayOf_VK_US, "(kmn_array.ArrayOf_VK_US[0][0]",kmn_array.ArrayOf_VK_US[0][0])
const vl_label2= kmn_array.ArrayOf_VK_US[0].filter((num:number) => num ===kmn_array.ArrayOf_VK_US[0][0])
console.log("§§§§§§§§§§§§§§§§§§§§§§§§", vk_label_test,"--", vl_label2, vl_label2===vk_label_test,kmn_array.ArrayOf_VK_US[vl_label2][1] )*/




    // if caps is used in .keylayout-file we need to add NCAPS in kmn-file
    let isCAPSused = false
    const modi: string[] = kmn_array.ArrayOf_Modifiers;
    const filteredModifiers: string[] = modi.filter((mod) => (String(mod) === ("caps")))
    if (filteredModifiers.indexOf("caps") >= 0)
      isCAPSused = true

    // TODO good explanation
    // find all modifiers used per modifier combination
    // find resulting character from
  // todo magic 50
    // loop through keys
    //for (let j = 0; j <kmn_array.ArrayOf_Ukelele_output[0].length; j++) {
    for (let j = 0; j < 50; j++) {
      // loop through modifiers

      data += '\n'
      for (let i = 0; i < kmn_array.ArrayOf_Modifiers.length; i++) {

        // get the modifier for the layer
        const label_modifier = this.create_modifier(kmn_array.ArrayOf_Modifiers[i], isCAPSused)

        // get the character from keymap-section of .keylayout-file that will be written as result in kmn-file

        const resulting_character = new TextDecoder().decode(kmn_array.ArrayOf_Ukelele_output[i][j])
        //const resulting_character = new TextDecoder().decode(kmn_array.ArrayOf_VK_DirectFromUku[i][j])
//console.log("j",j,"-",i,"resulting_character",resulting_character )
        // get the VK_ - label
       // const VK: number[][] = kmn_array.ArrayOf_SC_MacWin
       // const vk_label = VK.filter(item => item[0] === kmn_array.ArrayOf_SC_MacWin[j][0])
        const VK: number[][] = kmn_array.ArrayOf_VK_DirectFromUku
        const vk_label = VK.filter(item => item[0] === kmn_array.ArrayOf_VK_DirectFromUku[j][0])

        // TODO remove j
        if (resulting_character !== '')
          data += j + `+ [` + (label_modifier + ' ' + vk_label[0][1]).trim() + `] > \'` + resulting_character + '\'\n'
      }
    }

    data += '\n'

    //  *************************************************************
    //  **** write deadkeys *****************************************
    //  *************************************************************

    for (let i = 0; i < kmn_array.ArrayOf_Ukelele_action[0].length; i++) {
      // loop through modifiers
      for (let j = 0; j < kmn_array.ArrayOf_Modifiers.length; j++) {

        // get the modifier for the layer
        const label_modifier = this.create_modifier(kmn_array.ArrayOf_Modifiers[j], isCAPSused)

        // get the character from keymap-section of .keylayout-file that will be written as result in kmn-file
        const resulting_character = new TextDecoder().decode(kmn_array.ArrayOf_Ukelele_action[j][i])

        if (resulting_character !== "") {
          //get the VK_ - label
          const VK: number[][] = kmn_array.ArrayOf_SC_MacWin
          const vk_label = VK.filter(item => item[0] === kmn_array.ArrayOf_SC_MacWin[i][0])

          for (let k = 0; k < kmn_array.ArrayOf_dk.length; k++) {
            if ((resulting_character !== "") && (resulting_character === kmn_array.ArrayOf_dk[k][1])) {
              data += '[' + (label_modifier + ' ' + vk_label[0][1]).trim() + '] ' + "> dk(" + this.getHexFromChar(kmn_array.ArrayOf_dk[k][1]) + ") " + '\n'
            }
          }
        }
      }
    }

    data += "\n"
    data += "match > use(deadkeys)\n\n"
    data += "group(deadkeys)\n"
    data += "\n"

    for (let i = 0; i < kmn_array.ArrayOf_deadkeyables.length; i++) {
      if (kmn_array.ArrayOf_deadkeyedChar[i] !== undefined) {
        data += "store(dkf" + this.getHexFromChar(kmn_array.ArrayOf_dk[i][1]) + ") " + ("\'" + String(kmn_array.ArrayOf_deadkeyables[i])).replace(/\,+/g, "' '") + "'\n"
        data += "store(dkt" + this.getHexFromChar(kmn_array.ArrayOf_dk[i][1]) + ") " + ("\'" + String(kmn_array.ArrayOf_deadkeyedChar[i])).replace(/\,+/g, "' '") + "'\n"
        data += '\n'
      }
    }

    // Todo use writefile from elsewhere
    writeFileSync("data/MyResult.kmn", data, { flag: "w" })
    fs.writeFileSync("data/MyResult_fs.kmn", data, { flag: "w" })
    //this.callbacks.fs.writeFileSync("data/MyResult_callb_fs.kmn", data) // not usable here since it takes UInt8array data

    // ToDo conditions?
    if (data.length > 0)
      return true;
    else
      return false
  }

  //... helpers .............................................................................................


  // 34->"K_A"
  /*public find_VK_X_in_ArrayOf_VK_US(vk_in: any, data: any): any {
    for (let i = 0; i < data.ArrayOf_VK_US.length; i++) {
      if (data.ArrayOf_VK_US[i][0] === vk_in) {
        return data.ArrayOf_VK_US[i][1]
      }
    }
    return
  }*/

  //   TODO move outside of class?
  /**
   * @brief  member function to return the unicode value of a character
   * @param  character the value that will converted
   * @return headecimal value of a character
   */
  public getHexFromChar(character: string): string {
    return '00' + character.charCodeAt(0).toString(16).slice(-4).toLowerCase()
  }

  /** 
   * @brief  member function to create a string of modifiers in kmn-style from the modifierMap section of .keylayout-file
   * @param  keylayout_modifier the modifier value used in the .keylayout-file
   * @return kmn_modifier the modifier value used in the .kmn-file
   */
  public create_modifier(keylayout_modifier: any, isCAPSused: boolean): string {
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

/**
 * @brief  member function to map Ukelele keycodes to a position of Key_Name_Arra
 * @param  pos Ukelele (=mac) keycodes
 * @return position of Key_Name_Array ( the array holding all keynames)
 */
  // TODO add all other keys
  // TODO replace with mapping from constants
  public map_UkeleleKC_To_kmn_Key_Name_Array_Position_n(pos: any): any {
    // ukelele kc  -->  // kmn-Array kmn_Key_Name
    // .keylayout-file  // position in list
    // code = ..        //  zerobased 0-...
    //const pos = Number(poss)

    if (pos === 0) return 34   //  a
    if (pos === 11) return 35   //  b
    if (pos === 8) return 36   //  c
    if (pos === 2) return 37   //  d
    if (pos === 14) return 38   //  e
    if (pos === 3) return 39   //  f
    if (pos === 5) return 40   //  g
    if (pos === 4) return 41   //  h
    if (pos === 34) return 42   //  i
    if (pos === 38) return 43   //  j
    if (pos === 40) return 44   //  k
    if (pos === 37) return 45   //  l
    if (pos === 46) return 46   //  m
    if (pos === 45) return 47   //  n
    if (pos === 31) return 48   //  o
    if (pos === 35) return 49   //  p
    if (pos === 12) return 50   //  q
    if (pos === 15) return 51   //  r
    if (pos === 1) return 52   //  s
    if (pos === 17) return 53   //  t
    if (pos === 32) return 54   //  u
    if (pos === 9) return 55   //  v
    if (pos === 13) return 56   //  w
    if (pos === 7) return 57   //  x
    if (pos === 6) return 58   //  y
    if (pos === 16) return 59   //  z
    if (pos === 18) return 25   //  1
    if (pos === 19) return 26   //  2
    if (pos === 20) return 27   //  3
    if (pos === 21) return 28   //  4
    if (pos === 23) return 29   //  5
    if (pos === 22) return 30   //  6
    if (pos === 26) return 31   //  7
    if (pos === 28) return 32   //  8
    if (pos === 25) return 33   //  9
    if (pos === 29) return 24   //  0
    if (pos === 24) return 97   //  ´ EQUAL
    if (pos === 10) return 102   // ^ BKQUOTE
    if (pos === 33) return 103   // [ LBKT
    if (pos === 50) return 104   // \ BKSLASH
    if (pos === 30) return 105   // ] RBKT
    if (pos === 44) return 101    // / SLASH
    if (pos === 43) return 98    // , COMMA
    if (pos === 27) return 99    // ß HYPHEN
    if (pos === 36) return 2     // , SPACE  K_ENTER
    if (pos === 39) return 106    // , QUOTE
    if (pos === 41) return 96    // : COLON
    if (pos === 47) return 100    // : PERIOD


    /*if (pos === 187) return 100   //  ^
    if (pos === 192) return 95   //  ´*/


    return 999
  }
  public map_UkeleleKC_To_kmn_Key_Name_Array_Position_new(pos: any): any {
    const firstKey = Object.keys(k)[pos]
    //console.log("pos",pos, firstKey)
    return firstKey
  }

  public map_UkeleleKC_To_kmn_Key_Name_Array_Position_new_2(pos: any): any {
    const firstKey = Object.keys(kk)[pos]
    console.log("pos",pos, firstKey)
    return firstKey
  }

  /**
 * @brief  member function to map Ukelele keycodes to a Windows Keycodes
 * @param  pos Ukelele (=mac) keycodes
 * @return keycode on Win Keyboard
 */
  // TODO finish all entries 
  public map_UkeleleKC_To_Win_KC(pos: any): any {
    // ukelele KC  -->  // kmn win KC

    if (pos === 0x0A) return 0x29   /* ^,        /*--*/
    if (pos === 0x12) return 0x02   /* 1,        /*--*/
    if (pos === 0x13) return 0x03   /* 2,        /*--*/
    if (pos === 0x14) return 0x04   /* 3,        /*--*/
    if (pos === 0x15) return 0x05   /* 4,        /*--*/
    if (pos === 0x17) return 0x06   /* 5,        /*--*/
    if (pos === 0x16) return 0x07   /* 6,        /*--*/
    if (pos === 0x1A) return 0x08   /* 7,        /*  */
    if (pos === 0x1C) return 0x09   /* 8,        /* 28 */
    if (pos === 0x19) return 0x0A   /* 9,        /* 25 */
    if (pos === 0x1D) return 11   /* 0,        /*   29*/
    if (pos === 0x1B) return 12   /* ß,        /*27 HYPHEN   */
    if (pos === 0x18) return 0x0D   /* ´,        / *EQUAL   */

    if (pos === 0x0C) return 0x10   /* Q,        /* 12  */
    if (pos === 0x0D) return 0x11   /* W,        /* 13  */
    if (pos === 0x0E) return 0x12   /* E,        /* 14  */
    if (pos === 0x0F) return 0x13   /* R,        /* 15  */
    if (pos === 0x11) return 0x14   /* T,        /* 17  */
    if (pos === 0x10) return 0x15   /* Y,        /* 16  */
    if (pos === 0x20) return 0x16   /* U,        /* 32  */
    if (pos === 0x22) return 0x17   /* I,        /*  4  */
    if (pos === 0x1F) return 0x18   /* O,        /* 31  */
    if (pos === 0x23) return 0x19   /* P,        /* 35  */
    if (pos === 0x21) return 0x1A   /* Ü,        /* LBRKT  */
    if (pos === 0x1E) return 0x1B   /* +,        /* RBRKT  */
    if (pos === 0x32) return 0x2B   /* +,        /* BKSLASH  */

    if (pos === 0x00) return 0x1E   /* A,        /*--*/
    if (pos === 0x01) return 0x1F   /* S,        /*--*/
    if (pos === 0x02) return 0x20   /* D,        /*--*/
    if (pos === 0x03) return 0x21   /* F,        /*--*/
    if (pos === 0x05) return 0x22   /* G,        /*--*/
    if (pos === 0x04) return 0x23   /* H,        /*--*/
    if (pos === 0x26) return 0x24   /* J,        /*  */
    if (pos === 0x28) return 0x25   /* K,        /*  */
    if (pos === 0x25) return 0x26   /* L,        /*  */
    if (pos === 0x29) return 0x27   /* Ö,        /* COLON  */
    if (pos === 0x27) return 40   /* Ä,        /* QUOTE  */

    if (pos === 0x23) return 0x19   /* Ü,        /* OE2  */
    if (pos === 0x06) return 0x2C   /* Z,        /*--*/
    if (pos === 0x07) return 0x2D   /* X,        /*--*/
    if (pos === 0x08) return 0x2E   /* C,        /*--*/
    if (pos === 0x09) return 0x2F   /* V,        /*--*/
    if (pos === 0x0B) return 0x30   /* B,        /*--*/
    if (pos === 0x2D) return 0x31   /* N,        /*--*/
    if (pos === 0x2E) return 0x32   /* M,        /*--*/
    if (pos === 36) return  28   /* ENTER,*/

    if (pos === 47) return  52   /*  ,*/
    if (pos === 35) return 25   /* +,        /* PERIOD  */
    if (pos === 43) return 51   /* Ü,        /* COMMA  */
    if (pos === 44) return 53   /* Ü,        /* SLASH  */
    if (pos === 49) return  57   /* SPACE,*/

     // if (pos === 0x23) return 0x19   /* +,        /* KC01  */


    /* 0x29) return  0x   /* COLON,
     if (pos === 0x27) return  0x   /* QUOTE,
     if (pos === 0x2A) return  0x   /* BKQUOTE,          /* 192  */  // changed
    /*
      if (pos === 0x32) return  0x   /* BKSLASH,    /* ???*/

    /* if (pos === 0x21) return  k.K_LBRKT,
     if (pos === 0x1E) return  k.K_RBRKT,/*


     if (pos === 0x2B) return  0x   /* COMMA,
     if (pos === 0x2F) return  0x   /* PERIOD,
     if (pos === 0x2C) return  0x   /* SLASH,


     if (pos === 0x56) return  0x   /* oE2, // 86 << Same as 0x7D; found on iso, abnt2
     if (pos === 0x73) return  0x   /* oC1,
     if (pos === 0x7D) return  0x   /* oE2, // << Same as 0x56; found on jis
   */

  }
  // TODO finish all entries
  public map_UkeleleKC_To_VK(pos: any): any {
    // ukelele KC  -->  // VK_US

    if (pos === 0x0A) return "K_BKQUOTE"   /* ^,        /*--*/
    if (pos === 0x12) return "K_1"   /* 1,        /*--*/
    if (pos === 0x13) return "K_2"   /* 2,        /*--*/
    if (pos === 0x14) return "K_3"   /* 3,        /*--*/
    if (pos === 0x15) return "K_4"   /* 4,        /*--*/
    if (pos === 0x17) return "K_5"   /* 5,        /*--*/
    if (pos === 0x16) return "K_6"   /* 6,        /*--*/
    if (pos === 0x1A) return "K_7"   /* 7,     "   /*  */
    if (pos === 0x1C) return "K_8"   /* 8,     "   /* 28 */
    if (pos === 0x19) return "K_9"   /* 9,     "   /* 25 */
    if (pos === 0x1D) return "K_0"   /* 0,     "   /*   29*/
    if (pos === 0x1B) return "K_HYPHEN"   /* ß,        /*27 HYPHEN   */
    if (pos === 0x18) return "K_EQUAL"   /* ´,        / *EQUAL   */

    if (pos === 0x0C) return "K_Q"   /* Q,     "   /* 12  */
    if (pos === 0x0D) return "K_W"   /* W,     "   /* 13  */
    if (pos === 0x0E) return "K_E"   /* E,     "   /* 14  */
    if (pos === 0x0F) return "K_R"   /* R,     "   /* 15  */
    if (pos === 0x11) return "K_T"   /* T,     "   /* 17  */
    if (pos === 0x10) return "K_Y"   /* Y,     "   /* 16  */
    if (pos === 0x20) return "K_U"   /* U,     "   /* 32  */
    if (pos === 0x22) return "K_I"   /* I,     "   /*  4  */
    if (pos === 0x1F) return "K_O"   /* O,     "   /* 31  */
    if (pos === 0x23) return "K_P"   /* P,     "   /* 35  */
    if (pos === 0x21) return "K_LBRKT"   /* Ü,     "   /* LBRKT  */
    if (pos === 0x1E) return "K_RBRKT"   /* +,     "   /* RBRKT  */
    if (pos === 0x32) return "K_BKSLASH"   /* +,     "   /* BKSLASH  */

    if (pos === 0x00) return "K_A"   /* A,        /*--*/
    if (pos === 0x01) return "K_S"   /* S,        /*--*/
    if (pos === 0x02) return "K_D"   /* D,        /*--*/
    if (pos === 0x03) return "K_F"   /* F,        /*--*/
    if (pos === 0x05) return "K_G"   /* G,        /*--*/
    if (pos === 0x04) return "K_H"   /* H,        /*--*/
    if (pos === 0x26) return "K_J"   /* J,     "   /*  */
    if (pos === 0x28) return "K_K"   /* K,     "   /*  */
    if (pos === 0x25) return "K_L"   /* L,     "   /*  */
    if (pos === 0x29) return "K_COLON"   /* Ö,     "   /* COLON  */
    if (pos === 0x27) return "K_QUOTE"   /* Ä,     "   /* QUOTE  */

    if (pos === 0x23) return "K_oE2"   /* |,     "   /* OE2  */
    if (pos === 0x06) return "K_Z"   /* Z,        /*--*/
    if (pos === 0x07) return "K_X"   /* X,        /*--*/
    if (pos === 0x08) return "K_C"   /* C,        /*--*/
    if (pos === 0x09) return "K_V"   /* V,        /*--*/
    if (pos === 0x0B) return "K_B"   /* B,        /*--*/
    if (pos === 0x2D) return "K_N"   /* N,        /*--*/
    if (pos === 0x2E) return "K_M"   /* M,        /*--*/
    if (pos === 43) return "K_COMMA"   /* Ü,     "   /* COMMA  */
    if (pos === 47) return "K_PERIOD"   /*  ,*/
    if (pos === 44) return "K_SLASH"   /* Ü,     "   /* SLASH  */

    if (pos === 36) return "K_ENTER"   /* ENTER,*/
    if (pos === 49) return "K_SPACE"   /* SPACE,*/
  }

  // _S2 can probably go
  /*public map_UkeleleKC_To_kmn_Key_Name_Array_Position(in_from_ukelele: Uint16Array, pos: any): any {
    // ukelele kc  -->  // kmn-Array kmn_Key_Name
    // .keylayout-file  // position in list
    // code = ..        //  zerobased 0-...
    if (pos === 0) return 34   //  a
    if (pos === 11) return 35   //  b
    if (pos === 8) return 36   //  c
    if (pos === 2) return 37   //  d
    if (pos === 14) return 38   //  e
    if (pos === 3) return 39   //  f
    if (pos === 5) return 40   //  g
    if (pos === 4) return 41   //  h
    if (pos === 34) return 42   //  i
    if (pos === 38) return 43   //  j
    if (pos === 40) return 44   //  k
    if (pos === 37) return 45   //  l
    if (pos === 46) return 46   //  m
    if (pos === 45) return 47   //  n
    if (pos === 31) return 48   //  o
    if (pos === 35) return 49   //  p
    if (pos === 12) return 50   //  q
    if (pos === 15) return 51   //  r
    if (pos === 1) return 52   //  s
    if (pos === 17) return 53   //  t
    if (pos === 32) return 54   //  u
    if (pos === 9) return 55   //  v
    if (pos === 13) return 56   //  w
    if (pos === 7) return 57   //  x
    if (pos === 6) return 58   //  y
    if (pos === 16) return 59   //  z
    if (pos === 18) return 25   //  1
    if (pos === 19) return 26   //  2
    if (pos === 20) return 27   //  3
    if (pos === 21) return 28   //  4
    if (pos === 22) return 29   //  5
    if (pos === 23) return 30   //  6
    if (pos === 26) return 31   //  7
    if (pos === 28) return 32   //  8
    if (pos === 25) return 33   //  9
    if (pos === 29) return 34   //  0


    if (pos === 24) return 97   //  ´ EQUAL
    if (pos === 10) return 102   // ^ BKQUOTE

    if (pos === 187) return 100   //  ^


    return
  }*/


  /* public map_UkeleleKC_To_kmn_Key_VK_Name(pos: any): any {
     // ukelele kc  -->  // kmn-Array kmn_Key_Name
     // .keylayout-file  // position in list
     // code = ..        //  zerobased 0-...
     //const pos = Number(poss)
     
     if (pos === 0) return "K_A"   //  a
     if (pos === 1) return "K_S"   //  s
     if (pos === 2) return "K_D"   //  d
     if (pos === 3) return "K_F"   //  f
     if (pos === 11) return "K_B"   //  b
     if (pos === 8) return "K_C"   //  c

     /* if (pos === 14) return 38   //  e
      if (pos === 5) return 40   //  g
      if (pos === 4) return 41   //  h
      if (pos === 34) return 42   //  i
      if (pos === 38) return 43   //  j
      if (pos === 40) return 44   //  k
      if (pos === 37) return 45   //  l
      if (pos === 46) return 46   //  m
      if (pos === 45) return 47   //  n
      if (pos === 31) return 48   //  o
      if (pos === 35) return 49   //  p
      if (pos === 12) return 50   //  q
      if (pos === 15) return 51   //  r
      if (pos === 17) return 53   //  t
      if (pos === 32) return 54   //  u
      if (pos === 9) return 55   //  v
      if (pos === 13) return 56   //  w
      if (pos === 7) return 57   //  x
      if (pos === 6) return 58   //  y
      if (pos === 16) return 59   //  z
      if (pos === 18) return 25   //  1
      if (pos === 19) return 26   //  2
      if (pos === 20) return 27   //  3
      if (pos === 21) return 28   //  4
      if (pos === 22) return 29   //  5
      if (pos === 23) return 30   //  6
      if (pos === 26) return 31   //  7
      if (pos === 28) return 32   //  8
      if (pos === 25) return 33   //  9
      if (pos === 29) return 34   //  0
  
      
      if (pos === 187) return 100   //  ^
      if (pos === 192) return 95   //  ´
 
 
     return
   }*/

  // 34->0
  /*public find_pos_ofVK_in_ArrayOf_VK_US(vk_in: any, data: any): any {
    for (let i = 0; i < data.ArrayOf_VK_US.length; i++) {
      if (data.ArrayOf_VK_US[i][0] === vk_in) {
        return i
      }
    }
    return
  }*/

  // 34->0
  /* public find_pos_ofkmn_in_ArrayOf_kmn(pos_kmn: any, data: any): any {
     for (let i = 0; i < data.ArrayOf_Kmn.length; i++) {
       if (data.ArrayOf_Kmn[i][1] === pos_kmn) {
         return i
       }
     }
     return
   }*/

  //'^' -> 10
  /*public find_dk_in_uku_action(dk_in: any, data: any): any {
    const chr = new TextDecoder().decode(data.ArrayOf_Ukelele_output[0][10])
    console.log("dk_in", dk_in, "chr", chr)
    for (let j = 0; j < 8; j++) {
      // console.log(data.ArrayOf_Ukelele[0])
    }
    return 777
  }*/

  // dk->"dk K_EQUAL"
  /* public find_VK_X_in_ArrayOf_dk(vk_in: any, data: any): any {
    for (let i = 0; i < data.ArrayOf_dk.length; i++) {
      if (data.ArrayOf_dk[i][0] === vk_in) {
        return data.ArrayOf_dk[i][1]
      }
    }
    return
  }*/

}
/*
function iscapsUsedInVal(num:any) {
  if (String(num)===("caps"))
  return true
else return false
}*/

/*function iscapsUsedInVal_(accumulator:any, num:any) {
  if (String(num)===("caps"))
  return (accumulator && true)
else return  (accumulator && false)
}*/


  // _S2 can probably go
export const USVirtualKeyCodes = {
  K_BKSP: 8,
  K_TAB: 9,
  K_ENTER: 13,
  K_SHIFT: 16,
  K_CONTROL: 17,
  K_ALT: 18,
  K_PAUSE: 19,
  K_CAPS: 20,
  K_ESC: 27,
  K_SPACE: 32,
  K_PGUP: 33,
  K_PGDN: 34,
  K_END: 35,
  K_HOME: 36,
  K_LEFT: 37,
  K_UP: 38,
  K_RIGHT: 39,
  K_DOWN: 40,
  K_SEL: 41,
  K_PRINT: 42,
  K_EXEC: 43,
  K_INS: 45,
  K_DEL: 46,
  K_HELP: 47,
  K_0: 48,
  K_1: 49,
  K_2: 50,
  K_3: 51,
  K_4: 52,
  K_5: 53,
  K_6: 54,
  K_7: 55,
  K_8: 56,
  K_9: 57,
  K_A: 65,
  K_B: 66,
  K_C: 67,
  K_D: 68,
  K_E: 69,
  K_F: 70,
  K_G: 71,
  K_H: 72,
  K_I: 73,
  K_J: 74,
  K_K: 75,
  K_L: 76,
  K_M: 77,
  K_N: 78,
  K_O: 79,
  K_P: 80,
  K_Q: 81,
  K_R: 82,
  K_S: 83,
  K_T: 84,
  K_U: 85,
  K_V: 86,
  K_W: 87,
  K_X: 88,
  K_Y: 89,
  K_Z: 90,
  K_NP0: 96,
  K_NP1: 97,
  K_NP2: 98,
  K_NP3: 99,
  K_NP4: 100,
  K_NP5: 101,
  K_NP6: 102,
  K_NP7: 103,
  K_NP8: 104,
  K_NP9: 105,
  K_NPSTAR: 106,
  K_NPPLUS: 107,
  K_SEPARATOR: 108,
  K_NPMINUS: 109,
  K_NPDOT: 110,
  K_NPSLASH: 111,
  K_F1: 112,
  K_F2: 113,
  K_F3: 114,
  K_F4: 115,
  K_F5: 116,
  K_F6: 117,
  K_F7: 118,
  K_F8: 119,
  K_F9: 120,
  K_F10: 121,
  K_F11: 122,
  K_F12: 123,
  K_NUMLOCK: 144,
  K_SCROLL: 145,
  K_LSHIFT: 160,
  K_RSHIFT: 161,
  K_LCONTROL: 162,
  K_RCONTROL: 163,
  K_LALT: 164,
  K_RALT: 165,
  K_COLON: 186,
  K_EQUAL: 187,
  K_COMMA: 188,
  K_HYPHEN: 189,
  K_PERIOD: 190,
  K_SLASH: 191,
  K_BKQUOTE: 192,
  K_LBRKT: 219,
  /**
   * == K_OEM_5, 0xDC
   */
  K_BKSLASH: 220,
  K_RBRKT: 221,
  K_QUOTE: 222,
  /**
   * ISO B00, key to right of left shift, not on US keyboard,
   * 0xE2, K_OEM_102
   */
  K_oE2: 226,
  K_OE2: 226,
  K_oC1: 193,  // ISO B11, ABNT-2 key to left of right shift, not on US keyboard
  K_OC1: 193,
  'K_?C1': 193,
  'k_?C1': 193,
  K_oDF: 0xDF,
  K_ODF: 0xDF,
  K_LOPT: 50001,
  K_ROPT: 50002,
  K_NUMERALS: 50003,
  K_SYMBOLS: 50004,
  K_CURRENCIES: 50005,
  K_UPPER: 50006,
  K_LOWER: 50007,
  K_ALPHA: 50008,
  K_SHIFTED: 50009,
  K_ALTGR: 50010,
  K_TABBACK: 50011,
  K_TABFWD: 50012
};

  // _S2 can probably go
const k = USVirtualKeyCodes;

  // _S2 can probably go
export const UkeleleScanToUSVirtualKeyCodes = {
  0x12: k.K_1,        /* 18 */
  0x13: k.K_2,        /* 19 */
  0x14: k.K_3,        /* 20 */
  0x15: k.K_4,        /* 21 */
  0x17: k.K_5,        /* 23 */
  0x16: k.K_6,        /* 22 */
  0x1A: k.K_7,        /* 26 */
  0x1C: k.K_8,        /* 28 */
  0x19: k.K_9,        /* 25 */
  0x1D: k.K_0,        /* 29 */
  0x18: k.K_HYPHEN,   /* 24 */
  0x0A: k.K_EQUAL,    /* 187 */  //changed

  0x0C: k.K_Q,        /* 12  */
  0x0D: k.K_W,        /* 13  */
  0x0E: k.K_E,        /* 14  */
  0x0F: k.K_R,        /* 15  */
  0x11: k.K_T,        /* 17  */
  0x10: k.K_Y,        /* 16  */
  0x20: k.K_U,        /* 32  */
  0x22: k.K_I,        /* 34  */
  0x1F: k.K_O,        /* 31  */
  0x23: k.K_P,        /* 35  */
  0x21: k.K_LBRKT,
  0x1E: k.K_RBRKT,

  0x00: k.K_A,        /* 0  */
  0x01: k.K_S,        /* 1  */
  0x02: k.K_D,        /* 2  */
  0x03: k.K_F,        /* 3  */
  0x05: k.K_G,        /* 5  */
  0x04: k.K_H,        /* 4  */
  0x26: k.K_J,        /* 38 */
  0x28: k.K_K,        /* 40 */
  0x25: k.K_L,        /* 37 */
  0x29: k.K_COLON,
  0x27: k.K_QUOTE,
  0x2A: k.K_BKQUOTE,          /* 192  */  // changed

  0x32: k.K_BKSLASH,    /* ???*/

  0x06: k.K_Z,
  0x07: k.K_X,
  0x08: k.K_C,
  0x09: k.K_V,
  0x0B: k.K_B,
  0x2D: k.K_N,
  0x2E: k.K_M,
  0x2B: k.K_COMMA,
  0x2F: k.K_PERIOD,
  0x2C: k.K_SLASH,

  0x31: k.K_SPACE,

  0x56: k.K_oE2, // 86 << Same as 0x7D; found on iso, abnt2
  0x73: k.K_oC1,
  0x7D: k.K_oE2, // << Same as 0x56; found on jis

};

const kk = UkeleleScanToUSVirtualKeyCodes;
// _S2 can probably go
/*
export const UkeleleKeyCodeToScanCodes = {
  
  0x00: 0x1E, /* A,        /* 0  
  0x01: 0x1F, /* S,       /* 1   
  0x02: 0x20 ,/* D,        /* 2   
  0x03: 0x21 ,/* F,        /* 3   
  0x05: 0x22 ,/* G,        /* 5   
  0x04: 0x23 ,/* H,        /* 4   
  0x26: 0x24 ,/* J,        /* 38  
  0x28: 0x25 ,/* K,        /* 40  
  0x25: 0x26 ,/* L,        /* 37  
 /* 0x29: 0x ,/* COLON,
  0x27: 0x ,/* QUOTE,
  0x2A: 0x ,/* BKQUOTE,          /* 192     // changed
/*
  0x32: 0x ,/* BKSLASH,    /* ??? 
  0x0C:  0x10  ,/*   Q,        /* 12   
  0x0D:  0x11  ,/*   W,        /* 13   
  0x0E:  0x12  ,/*   E,        /* 14   
  0x0F:  0x13  ,/*   R,        /* 15   
  0x11:  0x14  ,/*   T,        /* 17   
  0x10:  0x15  ,/*   Y,        /* 16   
  0x20:  0x16  ,/*   U,        /* 32   
  0x22:  0x17  ,/*   I,        /* 4   
  0x1F:  0x18  ,/*   O,        /* 31   
  0x23:  0x19  ,/*   P,        /* 35   
  0x21: k.K_LBRKT,
  0x1E: k.K_RBRKT, 

/*
  0x2B: 0x ,/* COMMA,
  0x2F: 0x ,/* PERIOD,
  0x2C: 0x ,/* SLASH,

  0x31: 0x ,/* SPACE,

  0x56: 0x ,/* oE2, // 86 << Same as 0x7D; found on iso, abnt2
  0x73: 0x ,/* oC1,
  0x7D: 0x ,/* oE2, // << Same as 0x56; found on jis

};*/
