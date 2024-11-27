/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Converts macOS/Ukelele .keylayout files to Keyman .kmn
 */
import { CompilerCallbacks, CompilerOptions } from "@keymanapp/developer-utils";
import { ConverterToKmnArtifacts } from "../converter-artifacts.js";

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
//     Replace any-types
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

// _S2 imports Sabine
import { XMLParser } from 'fast-xml-parser';  // for reading a file
import { readFileSync } from 'fs';
import { writeFileSync } from "fs";           // for writing a file
import * as fs from 'fs';   // what is this/do I need it? -  either import all or seperately like above





export interface convert_object {
  name: string,                               // needed?? remove
  ArrayOf_Layouts: string[],                     // needed?? I think no
    ArrayOf_Ukelele_output: any[],
    ArrayOf_Ukelele_action: any[],
  ArrayOf_Modifiers: string[],
  ArrayOf_VK_from_keylayout: (string | number)[][],
  ArrayOf_dk: string[][],                          // add dk-mapping ( dk1 <-> '^' )
  ArrayOf_deadkeyables: string[][],                // add char that can be modified with dk ( a,e,i,o,u)
  ArrayOf_deadkeyedChar: string[][],               // add modified keys ( â,ê,î,ô,û)
  ArrayOf_Terminators: Uint8Array[]                  // add terminators ( ^,´,`)
};


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
   * @brief  member function to read filename ( a .keylayout-file) and write contents into Uint8Array keys_all_Layers
   * @param  filename the ukelele .keylayout-file to be converted
   * @return in case of success Uint8Array keys_all_Layers; else null
   */
  public read(filename: string): convert_object {

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

    console.log("layouts_array read ")
    const parser = new XMLParser(options);
    const jsonObj = parser.parse(xmlFile); // get plain Object

    // ToDo naming
    // TODO array-> object
    const modifierMap_array: string[] = []                 // array holding all MODIFIER strings e.g. "anyShift caps anyOption"  -why not put modifiers into Uint8Array along with values of keys of layer
    const keys_output_all_Layers: Uint8Array[][] = []
    const keys_action_all_Layers: any[] = []            // array holding all values with ACTION attribute (needed for finding deadkeys)
    const deadkeyedChars_all_Layers: string[][] = []         // array holding all DEADKEYS for each mod state â, ê, ,....
    const terminators_all_Layers: Uint8Array[] = []            // array holding all DEADKEYS for each mod state â, ê, ,....
    const duplicate_layouts_array: string[] = []           // array holding the layouts e.g. ANSI or JIS // needed?? I think no

    // TODO call: get only ANSI
    // Do I need to care or is there always ANSI which is always keyMapSet[0]
    // if so code can be shortened
    // .........................................................
    // LAYOUTS: get all groups like ANSI JIS, remove JIS
    // .........................................................

    /*   in case we need to find ANSI
     for (let i = 0; i < jsonObj.keyboard.layouts.layout.length; i++) {
      duplicate_layouts_array[i] = jsonObj.keyboard.layouts.layout[i]['@_mapSet']
    }

    // remove duplicates
    const layouts_array: any[] = duplicate_layouts_array.filter(function (item, pos, self) {
      return self.indexOf(item) == pos;
    })

    // remove JIS (if keyMap contains baseMapSet it`s JIS)
    for (let i = 0; i < layouts_array.length; i++) {
      if(jsonObj.keyboard.keyMapSet[i].keyMap[0]['@_baseMapSet'])
         layouts_array.splice(i, 1);
    }

    // TODO implement error & do what?
    if(layouts_array.length > 1 )
      console.log("ERROR! too many layouts")
    if(layouts_array.length < 1 )
      console.log("One layouts")

      // Todo better way ??
    const keyMapSet_count = layouts_array.length-1
*/

    // in case there always ANSI which is always keyMapSet[0]
    const layouts_array: string[] = duplicate_layouts_array
    const keyMapSet_count = 0

    // -----

    const nrOfStates = jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap.length
    //const nrOfKeys_inLayer = jsonObj.keyboard.keyMapSet[keyMapSet_count].keyMap[0].key.length   // will they all have the same length later ?


    // .........................................................
    // KEYMAP: get all keys for attribute "output" and "action"  - TODO can i use shorter function?
    // .........................................................

    // loop through all ss-combin
    for (let j = 0; j < nrOfStates; j++) {
      // get modifier list e.g. "anyshift caps? anyOption"
      modifierMap_array[j] = jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier['@_keys']

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
      keys_output_all_Layers.push(keys_output_One_Layer)        // save all @output data ( will be used for conversion, write...)
      keys_action_all_Layers.push(keys_action_One_Layer)        // save all @action data ( will be used for deadkeys)
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

      const dk: string[] = [];
      for (let k = 0; k < dk_pairs_all_Layers.length; k++) {
        dk.push(dk_pairs_all_Layers[k][1])
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
    // TODO can i use shorter function?  -> yes use terminators, filter, map or reduce

    // this addresses state+output and none+output
    // but not state+next or none+next

    // loop through all dk, key-actions and find @_action
    // find this value in <actions><action id=...>
    // in their 'when' find output for dk
    // for all 'action' at keys paragraph
    for (let j = 0; j < dk_pairs_all_Layers.length; j++) {
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
      deadkeyedChars_all_Layers.push(deadkeys_One_dk)
    }
    
    const vk: any[] = [""]
    // TODO remove unneccassary elements
    const DataObject: convert_object = {
      name: "Ukelele-kmn",                                // needed?? remove
      ArrayOf_Layouts: layouts_array,                     // needed?? I think no
      ArrayOf_Ukelele_output: keys_output_all_Layers,
      ArrayOf_Ukelele_action: keys_action_all_Layers,
      ArrayOf_Modifiers: modifierMap_array,
      ArrayOf_VK_from_keylayout: vk,
      ArrayOf_dk: dk_pairs_all_Layers,                    // add dk-mapping ( dk1 <-> '^' )
      ArrayOf_deadkeyables: deadkeyables_all_Layers,      // add char that can be modified with dk ( a,e,i,o,u)
      ArrayOf_deadkeyedChar: deadkeyedChars_all_Layers,   // add modified keys ( â,ê,î,ô,û)
      ArrayOf_Terminators: terminators_all_Layers         // add terminators ( ^,´,`)
    };

    // TODO review condition
    return DataObject
    //return ((keys_output_all_Layers.length === nrOfStates + 5) && keys_output_all_Layers[0].length === nrOfKeys_inLayer) ? keys_output_all_Layers : null;
  }

  /**
   * @brief  member function to convert data of .keylayout-file to kmn-file This will convert/rename modifiers, position of Keys and deadkeys and save into an array 
   * @param  data_ukelele (Uint8Array) data of the ukelele .keylayout-file
   * @return outArray Uint8Array keys_all_Layers, the converted data for kmn-files if all layers have been converted; else null
   */
  public convert(data_ukelele: convert_object): convert_object {
    const data_VK_from_keylayout: (string | number)[][] = [];

    for (let i = 0; i < data_ukelele.ArrayOf_Ukelele_output[0].length; i++) {
      const data_VK_from_keylayout_pair: (string | number)[] = [];
      const vk_from_Ukelele: string = this.map_UkeleleKC_To_VK(i)
      data_VK_from_keylayout_pair.push(i)
      data_VK_from_keylayout_pair.push(vk_from_Ukelele)
      data_VK_from_keylayout.push(data_VK_from_keylayout_pair)
    }
    data_ukelele.ArrayOf_VK_from_keylayout = data_VK_from_keylayout

    return data_ukelele
  }


  /**
   * @brief   member function to write data to file
   * @param  kmn_array the array holding keyboard data
   * @return true if data has been written; false if not
   */
  //TODO need to use export const USVirtualKeyCodes here
  public write(kmn_array: convert_object): boolean {

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

    // if caps is used in .keylayout-file we need to add NCAPS in kmn-file
    let isCAPSused = false
    const used_Keys_count = 50  // do we need more than 50 keys??
    const modi: string[] = kmn_array.ArrayOf_Modifiers;
    const filteredModifiers: string[] = modi.filter((mod) => (String(mod) === ("caps")))
    if (filteredModifiers.indexOf("caps") >= 0)
      isCAPSused = true

    // TODO good explanation
    // find all modifiers used per modifier combination
    // find resulting character from

    // loop through keys
    //for (let j = 0; j <kmn_array.ArrayOf_Ukelele_output[0].length; j++) {
    for (let j = 0; j < used_Keys_count; j++) {

      data += '\n'

      // loop through modifiers
      for (let i = 0; i < kmn_array.ArrayOf_Modifiers.length; i++) {

        // get the modifier for the layer
        const label_modifier = this.create_modifier(kmn_array.ArrayOf_Modifiers[i], isCAPSused)

        // get the character from keymap-section of .keylayout-file that will be written as result in kmn-file
        const resulting_character = new TextDecoder().decode(kmn_array.ArrayOf_Ukelele_output[i][j])
        const VK: (string | number)[][] = kmn_array.ArrayOf_VK_from_keylayout
        const vk_label = VK.filter(item => item[0] === kmn_array.ArrayOf_VK_from_keylayout[j][0])

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
          const VK: (string | number)[][] = kmn_array.ArrayOf_VK_from_keylayout
          const vk_label = VK.filter(item => item[0] === kmn_array.ArrayOf_VK_from_keylayout[i][0])

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
  public create_modifier(keylayout_modifier: string, isCAPSused: boolean): string {
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
}


