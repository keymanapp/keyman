/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Converts macOS/Ukelele .keylayout files to Keyman .kmn
 */
import { CompilerCallbacks, CompilerOptions } from "@keymanapp/developer-utils";
import { ConverterToKmnArtifacts } from "../converter-artifacts.js";


//     Todo remove these todos
// OK  TODO keylayout->kmn
// OK  modifiers: The identifier of the <modifierMap> element to use for this range of hardware keyboard types.
// OK  defaultIndex: The table number to use for modifier key combinations which are not explicitly specified by any <modifier> element within the <modifierMap>.
// OK  write read, convert, write
// OK  add data to object
// OK  Use filter functions
// OK  action/output:use filter etc to shorten func
// OK  deadkeyables:use filter etc to shorten func
// OK  dk-> for all action:use filter etc to shorten func
// OK  remove unneccessary data from data_object
// OK  rename symbols
// OK  remove part using kmn_key_Name1
// OK  remove unnecceaasry map_UkeleleKC_To_kmn_Key_Name_Array_Position_n etc
// OK  loop throught ANSI, JIS- at moment only use [keyMapSet_count] (keyMapSet_count=0)
// OK  remove funcs at teh end
// OK  import { makePathToFixture } from '../../test/helpers/index.js';
// OK  Mapping 0->30  or 0->K_A-> missing entries in mapping 
// OK  Replace any-types
// OK  Several steps action-> action-> action->character ( not only  action->character)
// OK  Usable for all keylayout files
// OK  Use callbacks as for writeFileSync
// OK  objects contain only used stuff READ in: -- out: only read arrays / CONVERT in: only read arrays out: return only to write arrays
// OK  Use catch blocks for file read
// OK  read:  answer :  + [K_A] > 'a'  is OK / TODO which format to use in output ?  + [K_A] > 'a' (character code)  or    + [K_A] > U+0061 (virt Keycode)
// OK  one entry vs several entry in tags
// OK  no added NCAPS when first modifier in modifierMap does not contain "caps" or"caps?"
// OK  naming of for-loop var i,j,k?...
// OK  warning if contrADICTING RULES E:G:   [ 'modifier_C0', 'SHIFT NCAPS', 'K_C', 'c'],  vs [ 'modifier_C0', 'SHIFT NCAPS', 'K_C', 'C' ],
// OK  print NCAPS as the first of the modifiers in create_kmn_modifier
// OK  use boxXmlArray from codebase instead of my own
// OK  where are rules for action+none ( a, e, u, etc)
// OK  order of object member var
// OK  readfilesync with paths better way?
// OK  rearrange code to use read, convert, write
// OK  TODO rewrite explanantion for object instead of array
// OK  remove all any types
// OK  public dk: number,                    //todo remove one
// OK  todo use from elsewhere boxXmlArray_S
// OK  prev_deadkeys_Ch: Uint8Array,   /* Todo needed?*/
// OK  Todo remove print_draft
// OK  Todo files/path !!!
// OK  start Tests v ToDo remove......................................
// OK  TODO remove:  test files: checkif kmn gets the same output as ukelele file(except for C3 t works well :))
// OK  ToDo needed methods
// OK  where to put documentation
// OK  dk <-> id_deadkey ???
// OK  check if we use the same  algorithm to get Block1 data
// OK  check unique_prev_deadkey, unique_deadkey, dk_prv, dk, id_deadkey
// OK  check duplicate rule conditions which do I need; which can go
// OK  when I run german with uniqueCAll why will there be [ ] without key anf modifier
// OK  add links to HEADLINE of kmc-convert document
// OK  TODO more  Data stores to add ??
// OK  check for keys < 50 whern working with keys
// OK  NO OUTPUT FOR SPACE in C0C1 !!!
// OK  check code for code styles keyman
//     tests for 3 functions read write convert
//     Return conditions
//     Tests throws
//     Conditions NCAPS,OPT;...
//     TODO move func outside of class
//     Functions as object methods?
//     use length to clear array instead of defining new every time (modifierMap_ONE_InKeymapSelect.length=0
//     Filter functions use the same type
//     TODO need to use export const USVirtualKeyCodes here
//     replace unique_prev_deadkey 0, >0 with true, false
//     TODO what about using actions twice in a row??? -> error msg if chain >4
//     what if keylayout file is not correct e.g missing >
//     read TODO which stores?
//     does order of modifier matter ?
//     remove all markers c0, 1-1, #### C2 ###, ...
//     better function names for get......


import { XMLParser } from 'fast-xml-parser';  // for reading an xml file
import { readFileSync } from 'fs';
import { util } from '@keymanapp/common-types';
import boxXmlArray = util.boxXmlArray;

function boxArrays(source: any) {
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

export interface rule_object {
  rule_type: string,              /* rule type C0-C4 */

  modifier_prev_deadkey: string,  /* string of modifiers for the first key (e.g. "NCAPS RALT CTRL") */
  prev_deadkey: string,           /* name of the first key (e.g. K_U) */
  id_prev_deadkey: number,        /* dk id for prev-deadkeys */
  unique_prev_deadkey: number,    /* marks the first prev_dk */

  modifier_deadkey: string,       /* string of modifiers for the second key (e.g. "NCAPS RALT CTRL") */
  deadkey: string,                /* name of the second key */
  id_deadkey: number,             /* dk id for deadkeys */
  unique_deadkey: number,         /* marks the first dk */

  modifier_key: string,           /* string of modifiers for the third key (e.g. "NCAPS RALT CTRL") */
  key: string,                    /* name of the third key (e.g. K_U) */
  output: Uint8Array,             /* the output character */
};

export interface convert_object {
  keylayout_filename: string,
  arrayOf_Modifiers: string[][],
  arrayOf_Rules: rule_object[],
};


export class KeylayoutToKmnConverter {

  static readonly INPUT_FILE_EXTENSION = '.keylayout';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';
  static readonly USED_KEYS_COUNT = 51

  // TODO use callbacks what about /*private*/ for options
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
    const jsonO: object = this.read(inputFilename)

    if (!jsonO) {
      return null;
    }

    console.log(' _S2 then CONVERT ........................................');
    const outArray: convert_object = await this.convert(jsonO);

    if (!outArray) {
      return null;
    }

    console.log(' _S2 then WRITE to kmn .....................................');

    const out_text: boolean = this.write(outArray)
    if (!out_text) {
      return null;
    }

    // TODO throws
    console.log(';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FINISHED OK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;');
    throw new Error('Not finished yet');
  }

  /**
   * @brief  take filename, open and read data from .keylayout-file and store in several arrays of  the data object
   *         member function to read filename ( a .keylayout-file) and write contents into Uint8Array keys_all_Layers
   * @param  filename the ukelele .keylayout-file to be converted
   * @return in case of success Uint8Array keys_all_Layers; else null
   */ public read(filename: string): Object {
    let xmlFile
    let jsonObj = []

    const options = {
      ignoreAttributes: false,
      trimValues: false,           // preserve spaces
      attributeNamePrefix: '@_'    // to access the attribute
    };

    try {
      xmlFile = readFileSync((process.cwd() + "\\data" + filename.substring(filename.lastIndexOf("\\"))).replace(" ", ""), 'utf8');
      const parser = new XMLParser(options);
      jsonObj = parser.parse(xmlFile);  // get plain Object
      boxArrays(jsonObj.keyboard);      // jsonObj now contains only arrays; no single fields
    }
    catch {
      // Todo how to break correctly; return what??
      console.log(" FILE NOT FOUND")
    }
    return jsonObj
  }


  /**
   * @brief  member function to convert data of .keylayout-file to kmn-file This will convert/rename modifiers, position of Keys and deadkeys and save into an array 
   * @param  take data_ukelele and create a mapping from mac Keycodes to key-names and save to data_ukelele object
   * @param  data_ukelele (Uint8Array) data of the ukelele .keylayout-file
   * @return outArray Uint8Array keys_all_Layers, the converted data for kmn-files if all layers have been converted; else null
   */
  public convert(jsonObj: any): convert_object {

    const modifierBehavior: string[][] = []          // modifier for each keymapselect
    const rule_object: rule_object[] = []             // an array of objects which hold data for a kmn rule

    const jsonObj_any: any = jsonObj
    const keylayout_file: string = jsonObj_any.keyboard['@_name'] + ".keylayout"

    const data_object: convert_object = {
      keylayout_filename: keylayout_file,
      arrayOf_Modifiers: modifierBehavior,   // ukelele uses behaviours e.g. 18 modifiersCombinations in 8 KeyMapSelect(behaviors)
      arrayOf_Rules: rule_object
    };

    // create an array of modifier combinations (e.g. shift? leftShift caps? ) and store in data_object
    for (let j = 0; j < jsonObj.keyboard.modifierMap.keyMapSelect.length; j++) {
      const singleModifierSet: string[] = []
      for (let k = 0; k < jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier.length; k++) {
        singleModifierSet.push(jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier[k]['@_keys'])
      }
      modifierBehavior.push(singleModifierSet)
    }
    // fill rules into arrayOf_Rules of data_object
    return this.createRuleData(data_object, jsonObj)
  }


  /**
   * @brief   member function to write data fro object to file
   * @param  data_ukelele the array holding keyboard data
   * @return true if data has been written; false if not
   */
  //TODO need to use export const USVirtualKeyCodes here
  public write(data_ukelele: convert_object): boolean {

    let data: string = "\n"

    // add top part of kmn file: STORES
    data += this.createData_Stores(data_ukelele)

    // add bottom part of kmn file: RULES
    data += this.createData_Rules(data_ukelele)

    this.callbacks.fs.writeFileSync("data/MyResult.kmn", new TextEncoder().encode(data))

    // ToDo conditions?
    if (data.length > 0) {
      return true;
    } else {
      return false
    }
  }

  //   TODO move outside of class?
  // ToDo keep only uint8array-version
  // for more info about mapping and cases C0-C4 see  https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.g7jwx3lx0ydd  
  public createRuleData(data_ukelele: convert_object, jsonObj: any): convert_object {

    const object_array: rule_object[] = []
    let dk_counter_C3_A: number = 0
    let dk_counter_C2: number = 0
    let action_id: string

    // check if we use CAPS in a modifier. In this case we need to add NCAPS
    const isCapsused: boolean = this.checkIfCapsIsUsed(data_ukelele.arrayOf_Modifiers)

    // loop keys 0-50 (= all keys we use)
    for (let j = 0; j < KeylayoutToKmnConverter.USED_KEYS_COUNT; j++) {

      // loop behaviors (in ukelele it is possible to define multiple modifier combinations that behave in the same way)
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[0].keyMap.length; i++) {

        let rule_obj: rule_object

        // ...............................................................................................................................
        // case C0: output ...............................................................................................................
        // C0 see: https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.g7jwx3lx0ydd ...
        // a key is mapped to a character directly ( code-> output) ......................................................................
        // ...............e. g. <key code="1" output="s"/> ...............................................................................
        // ...............................................................................................................................

        if ((jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== undefined)
          && (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== "")) {

          // loop behaviours
          for (let l = 0; l < data_ukelele.arrayOf_Modifiers[i].length; l++) {
            if (this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']))) {

              rule_obj = new Rules(
                  /*   rule_type */               "C0",

                  /*   modifier_prev_deadkey*/    "",
                  /*   prev_deadkey */            "",
                  /*   id_prev_deadkey */         0,
                  /*   unique A */                0,

                  /*   modifier_deadkey */        "",
                  /*   deadkey */                 "",
                  /*   dk for C2*/                0,
                  /*   unique B */                0,

                  /*   modifier_key*/             this.create_kmn_modifier(data_ukelele.arrayOf_Modifiers[i][l], isCapsused),
                  /*   key */                     this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                  /*   output */                  new TextEncoder().encode(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output']),
              )
              if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== "") {
                object_array.push(rule_obj)
              }
            }
          }

        }
        else if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] !== undefined) {

          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']
          // ...............................................................................................................................
          // case C1: action + state none + output .........................................................................................
          // C1 see: https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.g7jwx3lx0ydd ...
          // a key is mapped to an action and then to an output ............................................................................
          // KeyMap:code->KeyMap:action->action:action_state(none)->action_output ..........................................................
          // ...............e. g. <when state="none" output="a" ............................................................................
          // ...............................................................................................................................

          for (let l = 0; l < data_ukelele.arrayOf_Modifiers[i].length; l++) {

            if ((this.get_Action2ID_NoneOutput__From__ActionID_Id(jsonObj, action_id) !== undefined)
              && (this.get_Action2ID_NoneOutput__From__ActionID_Id(jsonObj, action_id) !== "")) {

              const outputchar: string = this.get_Action2ID_NoneOutput__From__ActionID_Id(jsonObj, action_id)
              const b1_modifierKey_arr: string[][] =
                this.get_Datat_array2D__From__ActionID_stateOutput(jsonObj, data_ukelele.arrayOf_Modifiers, action_id, outputchar, isCapsused)

              for (let m = 0; m < b1_modifierKey_arr.length; m++) {

                rule_obj = new Rules(
                  /*   rule_type */               "C1",

                  /*   modifier_prev_deadkey*/    "",
                  /*   prev_deadkey */            "",
                  /*   id_prev_deadkey */         0,
                  /*   unique A */                0,

                  /*   modifier_deadkey */        "",
                  /*   deadkey */                 "",
                  /*   dk for C2*/                0,
                  /*   unique B */                0,

                  /*   modifier_key*/             b1_modifierKey_arr[m][5],
                  /*   key */                     b1_modifierKey_arr[m][4],
                  /*   output */                  new TextEncoder().encode(outputchar)
                )
                object_array.push(rule_obj)
              }
            }
          }

          // ...............................................................................................................................
          // case C2: action + none + next .................................................................................................
          // ...............e. g.<when state="none" next="20"/> ............................................................................
          // replace state x with all rules that result in 14 (<when state="x" next="14") ..................................................
          // ...............................................................................................................................
          // Definition of C2 and Blocks 1-6 see ...........................................................................................
          // https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.g7jwx3lx0ydd   .........
          // ...............................................................................................................................

          const b1_actionIndex: number = this.get_ActionID_Index__From__ActionID_Id(jsonObj, action_id)

          // with action_id from above loop all 'action' and search for a state(none)-next-pair ............................................................................................................
          // e.g. in Block 5: find <when state="none" next="1"/> for action id a18 ......................................................................................................................
          for (let l = 0; l < jsonObj.keyboard.actions.action[b1_actionIndex].when.length; l++) {
            if ((jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_state'] === "none")         // find "none"
              && (jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_next'] !== undefined)) {   // find "next"

              // Data of Block Nr 5 .....................................................................................................................................................................
              // of this state(none)-next-pair get value of next (next="1") .............................................................................................................................
              /* eg: 1  */                            const b5_value_next: string = jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_next']
              // ........................................................................................................................................................................................


              // Data of Block Nr 4 .....................................................................................................................................................................
              // with present action_id (a18) find all keycode-behaviour-pairs that use this action (a18) => (keymapIndex 0/keycode 24 and keymapIndex 3/keycode 24) ....................................
              // from these create an array of modifier combinations  e.g. [['','caps?'], ['Caps']] .....................................................................................................
              /* eg: [['24', 0], ['24', 3]] */        const b4_deadkey_arr: number[][] = this.get_KeyMap_Code_array__From__ActionID_Action(jsonObj, action_id)
              /* e.g. [['','caps?'], ['Caps']]*/      const b4_deadkeyModifier_arr: string[] = this.get_KeyMap_Modifier_array__From__behaviour_arr(data_ukelele.arrayOf_Modifiers, b4_deadkey_arr)
              // ........................................................................................................................................................................................


              // Data of Block Nr 6 .....................................................................................................................................................................
              // create an array[action id,state,output] from all state-output-pairs that use state = b5_value_next (e.g. use 1 in  <when state="1" output="â"/> ) ......................................
              /*  eg: [ 'a9','1','â'] */              const b6_actionId_arr: string[][] = this.get_ActionID_Output_array__From__ActionID_State(jsonObj, b5_value_next)
              // ........................................................................................................................................................................................


              // Data of Block Nr 1  ....................................................................................................................................................................
              // create array[Keycode,Keyname,action id,actionIndex,output] and array[Keyname,action id,behaviour,modifier,output] ......................................................................
              /*  eg: ['0','K_A','a9','0','â'] */     const b1_keycode_arr: string[][] = this.get_KeyMap_Code_array__From__KeyMap_Action_array2D(jsonObj, b6_actionId_arr)
              /*  eg: ['K_A','a9','0','NCAPS','â']*/  const b1_modifierKey_arr: string[][] = this.get_KeyMapModiKeyArray__from__array(jsonObj, b1_keycode_arr, isCapsused)
              // .......................................................................................................................................................................................

              for (let n1 = 0; n1 < b4_deadkeyModifier_arr.length; n1++) {
                for (let n2 = 0; n2 < b4_deadkeyModifier_arr[n1].length; n2++) {
                  for (let n3 = 0; n3 < b4_deadkey_arr.length; n3++) {
                    for (let n4 = 0; n4 < b1_modifierKey_arr.length; n4++) {

                      rule_obj = new Rules(
                        /*   rule_type */             "C2",

                        /*   modifier_prev_deadkey*/  "",
                        /*   prev_deadkey */          "",
                        /*   id_prev_deadkey */       0,
                        /*   unique A */              0,

                        /*   modifier_deadkey */      this.create_kmn_modifier(b4_deadkeyModifier_arr[n1][n2], isCapsused),
                        /*   deadkey */               this.map_UkeleleKC_To_VK(Number(b4_deadkey_arr[n3][0])),
                        /*   dk for C2*/              dk_counter_C2++,
                        /*   unique B */              0,

                        /*   modifier_key*/           b1_modifierKey_arr[n4][3],
                        /*   key */                   b1_modifierKey_arr[n4][0],
                        /*   output */                new TextEncoder().encode(b1_modifierKey_arr[n4][4]),
                      )
                      if (b1_modifierKey_arr[n4][4] !== undefined) {
                        object_array.push(rule_obj)
                      }
                    }
                  }
                }
              }
            }
          }

          // ...............................................................................................................................
          // case C3: action + state Nr + Next .............................................................................................
          // ...............e. g.<when state="3" next="1"/> ................................................................................
          // replace state x with all rules that result in 1 (<when state="x" next="1") ....................................................
          // ...............................................................................................................................
          // Definition of C3 and Blocks 1-6 see ...........................................................................................
          // https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.g7jwx3lx0ydd  ..........
          // ...............................................................................................................................

          // loop all action-when and find state-next-pair

          // with action_id from above loop all 'action' and search for a state-next-pair ...................................................................................................................
          // e.g. in Block 5: find <when state="3" next="1"/> for action id a16 .............................................................................................................................
          for (let l = 0; l < jsonObj.keyboard.actions.action[b1_actionIndex].when.length; l++) {
            if ((jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_state'] !== "none")
              && (jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_next'] !== undefined)) {

              // Data of Block Nr 5 ........................................................................................................................................................................
              // of this state-next-pair get value of next (next="1") and state="3" ........................................................................................................................
              /* e.g. state = 3  */                       const b5_value_state: string = jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_state']
              /* e.g. next  = 1  */                       const b5_value_next: string = jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_next']
              // ...........................................................................................................................................................................................

              // Data of Block Nr 4 ........................................................................................................................................................................
              // with present action_id (a16) find all keycode-behaviour-pairs that use this action (a16) => (keymapIndex 3/keycode 32) ....................................................................
              // from these create an array of modifier combinations  e.g. [ [ 'anyOption', 'Caps' ] ] .....................................................................................................
              /* e.g. [['32', 3]] */                      const b4_deadkey_arr: number[][] = this.get_KeyMap_Code_array__From__ActionID_Action(jsonObj, String(action_id))
              /* e.g. [ [ 'anyOption', 'Caps' ] ]*/       const b4_deadkeyModifier_arr: string[] = this.get_KeyMap_Modifier_array__From__behaviour_arr(data_ukelele.arrayOf_Modifiers, b4_deadkey_arr)
              // ...........................................................................................................................................................................................

              // Data of Block Nr 3 ........................................................................................................................................................................
              // get an action id from a state-output-pair that use state = b5_value_state (e.g. use 3 in  <when state="none" next="3"/> ) .................................................................
              /* e.g. actioniD = a17  */                  const b3_actionId: string = this.get_ActionID_Id__From__ActionID_next(jsonObj, b5_value_state)
              // ...........................................................................................................................................................................................

              // Data of Block Nr 2  .......................................................................................................................................................................
              // with present action_id (a17) find all keynames and behaviours that use this action (a17) => (keymapIndex 3/keycode 28) ....................................................................
              // from these create an array of modifier combinations  e.g. [ [ 'anyOption', 'Caps' ] ] .....................................................................................................
              /* eg: index=3 */                           const b2_prev_deadkey_arr: number[][] = this.get_KeyMap_Code_array__From__ActionID_Action(jsonObj, String(b3_actionId))
              /* e.g. [ [ 'anyOption', 'Caps' ] ] */      const b2_prev_deadkeyModifier_arr: string[] = this.get_KeyMap_Modifier_array__From__behaviour_arr(data_ukelele.arrayOf_Modifiers, b2_prev_deadkey_arr)
              // ...........................................................................................................................................................................................

              // Data of Block Nr 6 ........................................................................................................................................................................
              // create an array[action id,state,output] from all state-output-pairs that use state = b5_value_next (e.g. use 1 in  <when state="1" output="â"/> ) .........................................
              /*  eg: [ 'a9','1','â'] */                  const b6_actionId_arr: string[][] = this.get_ActionID_Output_array__From__ActionID_State(jsonObj, b5_value_next)
              // ...........................................................................................................................................................................................

              // Data of Block Nr 1  .......................................................................................................................................................................
              // create array[Keycode,Keyname,action id,actionIndex,output] and array[Keyname,action id,behaviour,modifier,output] .........................................................................
              /*  eg: ['49','K_SPACE','a0','0','Â'] */    const b1_keycode_arr: string[][] = this.get_KeyMap_Code_array__From__KeyMap_Action_array2D(jsonObj, b6_actionId_arr)
              /*  eg: ['K_SPACE','a0','0','NCAPS','Â'] */ const b1_modifierKey_arr: string[][] = this.get_KeyMapModiKeyArray__from__array(jsonObj, b1_keycode_arr, isCapsused)
              // ...........................................................................................................................................................................................

              for (let n1 = 0; n1 < b2_prev_deadkeyModifier_arr.length; n1++) {
                for (let n2 = 0; n2 < b2_prev_deadkeyModifier_arr[n1].length; n2++) {
                  for (let n3 = 0; n3 < b2_prev_deadkey_arr.length; n3++) {
                    for (let n4 = 0; n4 < b4_deadkeyModifier_arr.length; n4++) {
                      for (let n5 = 0; n5 < b4_deadkeyModifier_arr[n4].length; n5++) {
                        for (let n6 = 0; n6 < b4_deadkey_arr.length; n6++) {
                          for (let n7 = 0; n7 < b1_modifierKey_arr.length; n7++) {

                            rule_obj = new Rules(
                              /*   rule_type */             "C3",
                              /*   modifier_prev_deadkey*/  this.create_kmn_modifier(b2_prev_deadkeyModifier_arr[n1][n2], isCapsused),
                              /*   prev_deadkey */          this.map_UkeleleKC_To_VK(Number(b2_prev_deadkey_arr[n3][0])),
                              /*   id_prev_deadkey */        dk_counter_C3_A++,
                              /*   unique A */              0,

                              /*   modifier_deadkey */      this.create_kmn_modifier(b4_deadkeyModifier_arr[n4][n5], isCapsused),
                              /*   deadkey */               this.map_UkeleleKC_To_VK(Number(b4_deadkey_arr[n6][0])),
                              /*   dk for C2*/              0,
                              /*   unique B */              0,

                              /*   modifier_key*/           b1_modifierKey_arr[n7][3],
                              /*   key */                   b1_modifierKey_arr[n7][0],
                              /*   output */                new TextEncoder().encode(b1_modifierKey_arr[n7][4]),
                            )
                            if (b1_modifierKey_arr[n7][4] !== undefined) {
                              object_array.push(rule_obj)
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        } else {
          console.log("ERROR : some entries are not available")
        }
      }
    }

    // --------------------------------------------------------------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------------------------------------------------------------------------------------
    // check for duplicate C2 and C3 rules in object_array (e.g. [NCAPS RALT K_8]  >  dk(C12) ): create a separate array of unique rules,
    // then compare to object_array and mark first occurance of a rule in object_array
    // --------------------------------------------------------------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------------------------------------------------------------------------------------
    let unique_dkB_count = 0
    const list_of_unique_rules: string[][] = []

    //------------------------------------ C2: dk ----------------------------------
    // first rule is always unique
    object_array[0].unique_deadkey = unique_dkB_count
    object_array[0].id_deadkey = unique_dkB_count
    unique_dkB_count++

    for (let i = 0; i < object_array.length; i++) {
      if ((object_array[i].modifier_deadkey !== "") && (object_array[i].deadkey !== "")) {
        let isFirstUsedHere_dk: boolean = true

        // check if not used before
        for (let j = 0; j < i; j++) {
          if ((object_array[i].modifier_deadkey === object_array[j].modifier_deadkey)
            && (object_array[i].deadkey === object_array[j].deadkey)) {
            isFirstUsedHere_dk = isFirstUsedHere_dk && false
          }
        }

        if (isFirstUsedHere_dk) {
          const ruleArray: string[] = []
          object_array[i].unique_deadkey = unique_dkB_count
          ruleArray.push(object_array[i].modifier_deadkey)
          ruleArray.push(object_array[i].deadkey)
          ruleArray.push(String(unique_dkB_count))
          unique_dkB_count++
          list_of_unique_rules.push(ruleArray)
        }
      }
    }

    //----------------------------------- C3: prev-dk ----------------------------------
    let unique_dkA_count = 0

    // first rule is always unique
    object_array[0].unique_prev_deadkey = unique_dkA_count
    unique_dkA_count++

    for (let i = 0; i < object_array.length; i++) {
      if ((object_array[i].modifier_prev_deadkey !== "") && (object_array[i].prev_deadkey !== "")) {
        let isFirstUsedHere_prev_dk: boolean = true

        // check if not used before
        for (let j = 0; j < i; j++) {
          if ((object_array[i].modifier_prev_deadkey === object_array[j].modifier_prev_deadkey)
            && (object_array[i].prev_deadkey === object_array[j].prev_deadkey)) {
            isFirstUsedHere_prev_dk = isFirstUsedHere_prev_dk && false
          }
        }

        // check if first part of C3 rule contains already defined rule of C2
        if (isFirstUsedHere_prev_dk) {
          object_array[i].unique_prev_deadkey = unique_dkA_count
          unique_dkA_count++
          for (let k = 0; k < list_of_unique_rules.length; k++) {
            if ((list_of_unique_rules[k][0] === object_array[i].modifier_deadkey) && ((list_of_unique_rules[k][1] === object_array[i].deadkey))) {
              object_array[i].unique_deadkey = Number(list_of_unique_rules[k][2])
            }
          }
        }

        if (isFirstUsedHere_prev_dk) {
          const ruleArray: string[] = []
          object_array[i].unique_deadkey = unique_dkB_count
          ruleArray.push(object_array[i].modifier_prev_deadkey)
          ruleArray.push(object_array[i].prev_deadkey)
          ruleArray.push(String(unique_dkB_count))
          unique_dkB_count++
          list_of_unique_rules.push(ruleArray)
        }
      }
    }

    // loop through object_array and mark first occurence each rule of list_of_unique_rules
    for (let i = 0; i < object_array.length; i++) {
      for (let j = 0; j < list_of_unique_rules.length; j++) {
        if ((object_array[i].modifier_prev_deadkey === list_of_unique_rules[j][0]) && (object_array[i].prev_deadkey === list_of_unique_rules[j][1])) {
          object_array[i].id_prev_deadkey = Number(list_of_unique_rules[j][2])
        }
        if ((object_array[i].modifier_deadkey === list_of_unique_rules[j][0]) && (object_array[i].deadkey === list_of_unique_rules[j][1])) {
          object_array[i].id_deadkey = Number(list_of_unique_rules[j][2])
        }
      }
    }
    data_ukelele.arrayOf_Rules = object_array
    return data_ukelele
  }


  // ---------------------------------------------------------------------------------------------------------------------
  // ---------------------------------------------------------------------------------------------------------------------
  /*
   in:  Block 3 - b3_actionId a19
   out: Block 2 - b2_keyname_arr ['K_8', 'K_M]
   do:  create array of Array of Keycode eg: ['K_8', 'K_M] from an actionId a16
  */
  public get_KecCode_arr__From__ActionId(data: any, search: string): string[] {
    const returnarray: string[] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search
          && data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'] < KeylayoutToKmnConverter.USED_KEYS_COUNT) {
          returnarray.push(this.map_UkeleleKC_To_VK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])))

        }
      }
    }
    return returnarray
  }
  /*
  in:  Block 5 - b5_actionId_arr a16, a18
  out: Block 4 - b4_code_arr [ '6', '31', '32' ]
  do:  create array of keycodes from an array of keymapIndex
 */
  public get_KeyMap_Code_array__From__KeyMap_Action(data: any, search: string[]): string[] {
    const returnarray: string[] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search
          && data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'] < KeylayoutToKmnConverter.USED_KEYS_COUNT) {
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])

        }
      }
    }
    return returnarray
  }
  /*
  in:  Block 6 - b6_actionId_arr [ 'a9','1','â']
  out: Block 1 - b1_keycode_arr ['49','K_SPACE','a0','0','Â']
  do: create array of ['49','K_SPACE','a0','0','Â']    from create array of  [ 'a9','1','â']
 */
  public get_KeyMap_Code_array__From__KeyMap_Action_array2D(data: any, search: string[][]): string[][] {
    const returnarray2D: string[][] = []
    for (let k = 0; k < search.length; k++) {
      for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
        for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
          const returnarray: string[] = []
          if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search[k][0] &&
            data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'] < KeylayoutToKmnConverter.USED_KEYS_COUNT) {
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
            returnarray.push(this.map_UkeleleKC_To_VK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])))
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'])
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i]['@_index'])
            returnarray.push(search[k][2])
          }
          if (returnarray.length > 0) {
            returnarray2D.push(returnarray)
          }
        }
      }
    }
    return returnarray2D
  }
  /*
  in:  action_id a19
  out: b1_actionIndex  behav. 1
  do: get the actionIdIndex from action id a18
 */
  public get_ActionID_Index__From__ActionID_Id(data: any, search: string): number {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      if (data.keyboard.actions.action[i]['@_id'] === search) {
        return i
      }
    }
    return 0
  }

  /*
 in:  Block 5 - b5_value_state   state = 3 
 out: Block 3 - b3_actionId a19
 do: get the actionIdI from state / next
*/
  public get_ActionID_Id__From__ActionID_next(data: any, search: string): string {
    if (search !== "none") {
      for (let i = 0; i < data.keyboard.actions.action.length; i++) {
        for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
          if (data.keyboard.actions.action[i].when[j]['@_next'] === search) {
            return data.keyboard.actions.action[i]['@_id']
          }
        }
      }
    }
    return ""
  }
  /*
 in:  Block 5 - b5_value_next  next  = 1
 out: Block 6 - b6_actionId_arr  'a9','1','â']
 do: create array of [ 'a9','1','â']    from state / next
*/
  public get_ActionID_Output_array__From__ActionID_State(data: any, search: string) {
    const returnarray2D: string[][] = []
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
        const returnarray: string[] = []
        if ((data.keyboard.actions.action[i].when[j]['@_state'] === search)) {
          returnarray.push(data.keyboard.actions.action[i]['@_id'])
          returnarray.push(data.keyboard.actions.action[i].when[j]['@_state'])
          returnarray.push(data.keyboard.actions.action[i].when[j]['@_output'])
        }
        if (returnarray.length > 0) {
          returnarray2D.push(returnarray)
        }
      }
    }
    return returnarray2D
  }
  /*
 in:  Block 1 - action_id a18
 out: Block 6  - outputchar 'A'
 do: create output Array from action_id
*/
  public get_Action2ID_NoneOutput__From__ActionID_Id(data: any, search: string): string {
    let OutputValue: string = ""

    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      if (data.keyboard.actions.action[i]['@_id'] === search) {
        for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
          if (data.keyboard.actions.action[i].when[j]['@_state'] === "none") {
            OutputValue = data.keyboard.actions.action[i].when[j]['@_output']
          }
        }
      }
    }
    return OutputValue
  }
  /*
 in:  Block 1 - b1_keycode_arr ['49','K_SPACE','a0','0','Â']
 out: Block 1 - b1_modifierKey_arr ['K_SPACE','a0','0','NCAPS','Â']
 do: create array of  key+modi+out   from  array of b1_keycode_arr
*/
  public get_KeyMapModiKeyArray__from__array(data: any, search: string[][], isCAPSused: boolean): string[][] {
    const returnarray: string[][] = []

    for (let i = 0; i < search.length; i++) {
      const behaviour: number = Number(search[i][3])

      for (let j = 0; j < data.keyboard.modifierMap.keyMapSelect[behaviour].modifier.length; j++) {
        const returnarray1D: string[] = []
        returnarray1D.push(search[i][1])  /* KeyName*/
        returnarray1D.push(search[i][2])  /* action*/
        returnarray1D.push(search[i][3])  /* behaviour*/
        returnarray1D.push(this.create_kmn_modifier(data.keyboard.modifierMap.keyMapSelect[behaviour].modifier[j]['@_keys'], isCAPSused)) /* modifier */
        returnarray1D.push(search[i][4])  /* char*/
        if (returnarray1D.length > 0) {
          returnarray.push(returnarray1D)
        }
      }
    }
    // remove duplicates
    const [unique_returnarray] = returnarray.reduce((acc, curr) => {
      const [uniq, set] = acc;
      if (!set.has(curr.join(','))) {
        set.add(curr.join(','));
        uniq.push(curr);
      }
      return acc;
    },
      [[], new Set()],
    );
    return unique_returnarray
  }
  /*  ??
 in:  Block C1 - action_id a19
 in:  Block C1 - out 'A'
 out: Block C1 - b1_modifierKey_arr
 do: create array of  key+mod+out   from  array of b1_modifierKey_arr 
*/
  public get_Datat_array2D__From__ActionID_stateOutput(data: any, modi: any, search: string, outchar: string, isCapsused: boolean): string[][] {
    const returnarray2D: string[][] = []

    // loop behaviors ( in ukelele it is possible to define multiple modifier combinations that behave in the same)
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < KeylayoutToKmnConverter.USED_KEYS_COUNT; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          for (let k = 0; k < modi[data.keyboard.keyMapSet[0].keyMap[i]['@_index']].length; k++) {
            const returnarray: string[] = []
            const behaviour: string = data.keyboard.keyMapSet[0].keyMap[i]['@_index']
            const modifierkmn: string = this.create_kmn_modifier(modi[behaviour][k], isCapsused)
            const keyName: string = this.map_UkeleleKC_To_VK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']))

            returnarray.push(search)
            returnarray.push(outchar)
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'])
            returnarray.push(behaviour)
            returnarray.push(keyName)
            returnarray.push(modifierkmn)

            if (returnarray.length > 0) {
              returnarray2D.push(returnarray)
            }
          }
        }
      }
    }
    // remove duplicates
    const [unique_returnarray] = returnarray2D.reduce((acc, curr) => {
      const [uniq, set] = acc;
      if (!set.has(curr.join(','))) {
        set.add(curr.join(','));
        uniq.push(curr);
      }
      return acc;
    },
      [[], new Set()],
    );

    return unique_returnarray
  }
  /*
 in:  Block  - action_id a19
 out: Block 4 - b4_deadkey_arr [['24', 0], ['24', 3]]
 do: create array of  [['24', 0], ['24', 3]]   from  behaviour id 
*/
  public get_KeyMap_Code_array__From__ActionID_Action(data: any, search: string): number[][] {
    const mapIndexArray_max: number[][] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < KeylayoutToKmnConverter.USED_KEYS_COUNT; j++) {

        const mapIndexArrayperKey: number[] = []

        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          mapIndexArrayperKey.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
          mapIndexArrayperKey.push(i)
        }
        if (mapIndexArrayperKey.length > 0) {
          mapIndexArray_max.push(mapIndexArrayperKey)
        }
      }
    }
    return mapIndexArray_max
  }
  /*
 in:  Block 4 - b4_deadkey_arr [['24', 0], ['24', 3]]
 out: Block 4 - b4_deadkeyModifier_arr  [['','caps?'], ['Caps']]
 do: create array of     from  array of b4_deadkey_arr
*/
  public get_KeyMap_Modifier_array__From__behaviour_arr(data: any, search: number[][]): string[] {
    const mapIndexArray_max: string[] = []
    for (let i = 0; i < search.length; i++) {
      mapIndexArray_max.push(data[search[i][1]])
    }
    return mapIndexArray_max
  }

  /** 
   * @brief  member function to create a string of modifiers in kmn-style from the modifierMap section of .keylayout-file
   * @param  keylayout_modifier the modifier value used in the .keylayout-file
   * @return kmn_modifier the modifier value used in the .kmn-file
   */
  //ToDo review lower part
  public create_kmn_modifier(keylayout_modifier: string, isCAPSused: boolean): string {
    let add_modifier: string = ""
    let kmn_modifier: string = ""
    let kmn_ncaps: string = ""

    // copy each modifier seperate element of array
    const modifier_state: string[] = keylayout_modifier.split(" ");

    for (let i = 0; i < modifier_state.length; i++) {

      if (isCAPSused && (keylayout_modifier).toUpperCase().indexOf("CAPS") === -1) {
        kmn_ncaps = " NCAPS "
      }

      // if we find a modifier containing a '?' e.g. SHIFT?: => SHIFT is not neccessary. If it is not neccessary we don`t write this modifier
      if (modifier_state[i].toUpperCase().includes('?') && (!modifier_state[i].toUpperCase().includes('CAPS?'))) {
        add_modifier = "";
      }

      // TODO is this correct: caps? => caps is not neccessary. If its not neccessary and isCAPSused we need to write out NCAPS. Correct?
      else if (isCAPSused && (modifier_state[i].toUpperCase().includes('CAPS?'))) {
        add_modifier = "NCAPS ";
      }
      else if (!isCAPSused && (modifier_state[i].toUpperCase().includes('CAPS?'))) {
        add_modifier = "";
      }
      else if (modifier_state[i].toUpperCase().includes('CAPS')) {
        add_modifier = "CAPS ";
      }
      else if (isCAPSused && (modifier_state[i].toUpperCase().includes('NCAPS'))) {
        add_modifier = "NCAPS ";
      }

      // Keyman does not use the right or left version of a modifier (e.g. rightshift, leftshift). If those are used in keylayout files
      // they will not be changed but written to the kmn as they are with a warning placed in front of them
      else if ((modifier_state[i].toUpperCase() === 'ANYSHIFT') || (modifier_state[i].toUpperCase() === 'SHIFT')) {
        add_modifier = "SHIFT ";
      }
      else if ((modifier_state[i].toUpperCase() === "LEFTSHIFT") || (modifier_state[i].toUpperCase() === "LSHIFT")) {
        add_modifier = "SHIFT ";
      }
      else if ((modifier_state[i].toUpperCase() === "RIGHTSHIFT") || (modifier_state[i].toUpperCase() === "RSHIFT")) {
        add_modifier = "SHIFT ";
      }
      else if ((modifier_state[i].toUpperCase() === 'ANYCONTROL') || (modifier_state[i].toUpperCase() === 'CONTROL')) {
        add_modifier = "RCTRL ";
      }
      else if ((modifier_state[i].toUpperCase() === "LEFTCONTROL") || (modifier_state[i].toUpperCase() === "LCONTROL")) {
        add_modifier = "CTRL ";
      }
      else if ((modifier_state[i].toUpperCase() === "RIGHTCONTROL") || (modifier_state[i].toUpperCase() === "LCONTROL")) {
        add_modifier = "CTRL ";
      }
      else if ((modifier_state[i].toUpperCase() === "LEFTOPTION") || (modifier_state[i].toUpperCase() === "LOPTION")) {
        add_modifier = "RALT ";
      }
      else if ((modifier_state[i].toUpperCase() === "RIGHTOPTION") || (modifier_state[i].toUpperCase() === "ROPTION")) {
        add_modifier = "RALT ";
      }
      else if ((modifier_state[i].toUpperCase() === 'ANYOPTION') || (modifier_state[i].toUpperCase() === 'OPTION')) {
        add_modifier = "RALT ";
      }
      else {
        add_modifier = String(modifier_state[i]) + " "
      }
      kmn_modifier += kmn_ncaps + add_modifier
    }

    // remove duplicate and empty entries
    const duplicate_modifier_array: string[] = kmn_modifier.split(" ").filter(item => item)
    const unique_modifier: string[] = duplicate_modifier_array.filter(function (item, pos, self) {
      return self.indexOf(item) === pos;
    })
    return unique_modifier.flat().toString().replace(/,/g, " ")
  }

  public checkIfCapsIsUsed(keylayout_modifier: string[][]): boolean {
    return JSON.stringify(keylayout_modifier).includes("caps")
  }

  public isAcceptableKeymanModifier(keylayout_modifier: string): boolean {
    let iskKeymanModifier: boolean = true
    const modifier_single: string[] = keylayout_modifier.split(" ");

    for (let i = 0; i < modifier_single.length; i++) {
      if (
        (modifier_single[i].toUpperCase() === "NCAPS")
        || (modifier_single[i].toUpperCase() === "CAPS")
        || (modifier_single[i].toUpperCase() === "SHIFT")
        || (modifier_single[i].toUpperCase() === "ALT")
        || (modifier_single[i].toUpperCase() === "RALT")
        || (modifier_single[i].toUpperCase() === "LALT")
        || (modifier_single[i].toUpperCase() === "CTRL")
        || (modifier_single[i].toUpperCase() === "LCTRL")
        || (modifier_single[i].toUpperCase() === "RCTRL")
        || (modifier_single[i].toUpperCase() === "")
      ) {
        iskKeymanModifier &&= true
      } else {
        iskKeymanModifier &&= false
      }
    }
    return iskKeymanModifier
  }

  // definition of comparisons 1-1, 2-4, 6-6,...  see https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.pcz8rjyrl5ug
  // todo remove comments n filters after check of several keylayout files
  public reviewRules(rule: rule_object[], index: number): string[] {

    const warningTextArray: string[] = Array(3).fill("");

    // +++++++++++++++++++++++++ check unavailable modifiers ++++++++++++++++++++++++++++++++++++++

    // Todo remoce C1-C3's and other markers
    if ((rule[index].rule_type === "C0") || (rule[index].rule_type === "C1")) {
      if (!this.isAcceptableKeymanModifier(rule[index].modifier_key)) {
        warningTextArray[2] = "C1 unavailable modifier : "
      }
    }

    else if (rule[index].rule_type === "C2") {
      if (!this.isAcceptableKeymanModifier(rule[index].modifier_deadkey)) {
        warningTextArray[1] = "C2 unavailable modifier : "
      }
      if (!this.isAcceptableKeymanModifier(rule[index].modifier_key)) {
        warningTextArray[2] = "C2 unavailable modifier : "
      }
    }

    else if (rule[index].rule_type === "C3") {
      if (!this.isAcceptableKeymanModifier(rule[index].modifier_prev_deadkey)) {
        warningTextArray[2] = "C3 unavailable modifier : "
      }
      if (!this.isAcceptableKeymanModifier(rule[index].modifier_deadkey)) {
        warningTextArray[1] = "C3 unavailable modifier : "
      }
      if (!this.isAcceptableKeymanModifier(rule[index].modifier_key)) {
        warningTextArray[0] = "C3 unavailable modifier : "
      }
    }


    // +++++++++++++++++++++++++ check ambiguous/duplicate rules ++++++++++++++++++++++++++++++++++++++

    if ((rule[index].rule_type === "C0") || (rule[index].rule_type === "C1")) {

      // 1-1 + [CAPS K_N]  > 'N' <-> + [CAPS K_N]  >  'A'
      const amb_1_1 = rule.filter((curr, idx) =>
        (curr.rule_type === "C0" || curr.rule_type === "C1")
        && curr.modifier_prev_deadkey === ""
        && curr.prev_deadkey === ""
        && curr.modifier_deadkey === ""
        && curr.deadkey === ""
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output)
        && idx < index
      );

      // 1-1 + [CAPS K_N]  > 'N' <-> + [CAPS K_N]  >  'N'
      const dup_1_1 = rule.filter((curr, idx) =>
        (curr.rule_type === "C0" || curr.rule_type === "C1")
        && curr.modifier_prev_deadkey === ""
        && curr.prev_deadkey === ""
        && curr.modifier_deadkey === ""
        && curr.deadkey === ""
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output)
        && idx < index
      );

      if (amb_1_1.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("C1 ambiguous 1-1 rule: earlier: ["
            + amb_1_1[0].modifier_key
            + " "
            + amb_1_1[0].key
            + "]  >  \'"
            + new TextDecoder().decode(amb_1_1[0].output)
            + "\' here: [")
      }

      if (dup_1_1.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("C1 duplicate 1-1 rule: earlier: ["
            + dup_1_1[0].modifier_key
            + " "
            + dup_1_1[0].key
            + "]  >  \'"
            + new TextDecoder().decode(dup_1_1[0].output)
            + "\' here: [")
      }
    }

    if (rule[index].rule_type === "C2") {

      // 2-2 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(C1)
      const amb_2_2 = rule.filter((curr, idx) =>
        curr.rule_type === "C2"
        && curr.modifier_deadkey === rule[index].modifier_deadkey
        && curr.deadkey === rule[index].deadkey
        && curr.id_deadkey !== rule[index].id_deadkey
        && idx < index
      );

      //2-2 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(C11)
      const dup_2_2 = rule.filter((curr, idx) =>
        curr.rule_type === "C2"
        && curr.modifier_deadkey === rule[index].modifier_deadkey
        && curr.deadkey === rule[index].deadkey
        && curr.id_deadkey === rule[index].id_deadkey
        && idx < index
      );

      //3-3  dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'B'
      const amb_3_3 = rule.filter((curr, idx) =>
        (curr.rule_type === "C2")
        && curr.id_deadkey === rule[index].id_deadkey
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output)
        && idx < index
        // && rule[index].unique_deadkey !== 0
      );

      //3-3 dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'Ã'
      const dup_3_3 = rule.filter((curr, idx) =>
        (curr.rule_type === "C2")
        && curr.id_deadkey === rule[index].id_deadkey
        && rule[index].unique_deadkey === 0
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output)
        && idx < index
      );

      //1-2 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  'Ñ'
      const amb_1_2 = rule.filter((curr, idx) =>
        ((curr.rule_type === "C0") || (curr.rule_type === "C1"))
        && curr.modifier_key === rule[index].modifier_deadkey
        && curr.key === rule[index].deadkey
        //&& rule[index].unique_deadkey === 0  // include and the first occurance will be printed
        //&& (idx < index)              // include and the first occurance will be printed
      );

      if (amb_2_2.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("C2 SECONDTEXT "
            + "ambiguous 2-2 rule: earlier: ["
            + amb_2_2[0].modifier_deadkey
            + " "
            + amb_2_2[0].deadkey
            + "]  >  dk(C"
            + amb_2_2[0].id_deadkey
            + ") here: ")
      }
      if (dup_2_2.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("C2 SECONDTEXT "
            + "duplicate 2-2 rule: earlier: ["
            + dup_2_2[0].modifier_deadkey
            + " "
            + dup_2_2[0].deadkey
            + "]  >  dk(C"
            + dup_2_2[0].id_deadkey
            + ") here: ")
      }
      if (amb_3_3.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("C2 THIRDTEXT "
            + "ambiguous 3-3 rule: earlier: dk(C"
            + amb_3_3[0].id_deadkey
            + ") + ["
            + amb_3_3[0].modifier_key
            + " "
            + amb_3_3[0].key
            + "]  >  \'"
            + new TextDecoder().decode(amb_3_3[0].output)
            + "\' here: ")
      }
      if (dup_3_3.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("C2 THIRDTEXT "
            + "duplicate 3-3 rule: earlier: dk(C"
            + dup_3_3[0].id_deadkey
            + ") + ["
            + dup_3_3[0].modifier_key
            + " "
            + dup_3_3[0].key
            + "]  >  \'"
            + new TextDecoder().decode(dup_3_3[0].output)
            + "\' here: ")
      }
      if (amb_1_2.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("C2 SECONDTEXT "
            + "ambiguous 1-2 rule: earlier: ["
            + amb_1_2[0].modifier_key
            + " "
            + amb_1_2[0].key
            + "]  >  \'"
            + new TextDecoder().decode(amb_1_2[0].output)
            + "\' here: ")
      }
    }

    if (rule[index].rule_type === "C3") {

      // 1-4 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  'Ñ'
      const amb_1_4 = rule.filter((curr, idx) =>
        ((curr.rule_type === "C0") || (curr.rule_type === "C1"))
        && curr.modifier_key === rule[index].modifier_prev_deadkey
        && curr.key === rule[index].prev_deadkey
        //&& rule[index].unique_prev_deadkey === 0 // include and the first occurance will be printed
        //&& (idx < index)
      );

      // 2-4 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(C1)
      const amb_2_4 = rule.filter((curr, idx) =>
        ((curr.rule_type === "C2"))
        && curr.modifier_deadkey === rule[index].modifier_prev_deadkey
        && curr.deadkey === rule[index].prev_deadkey
        && curr.id_deadkey === rule[index].id_prev_deadkey
        // && rule[index].unique_prev_deadkey === 0    // include and the first occurance will be printed
        // && (idx < index)
      );

      // 6-3  dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'B'
      const amb_6_3 = rule.filter((curr, idx) =>
        (curr.rule_type === "C2")
        && curr.id_prev_deadkey === rule[index].id_prev_deadkey
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && (new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output))
        // && rule[index].unique_deadkey !== 0
        // && idx < index
      );

      // 6-3 dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'Ã'
      const dup_6_3 = rule.filter((curr, idx) =>
        (curr.rule_type === "C2")
        && curr.id_prev_deadkey === rule[index].id_prev_deadkey
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output)
        // && rule[index].unique_deadkey === 0
        // && idx < index
      );

      // 4-4 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  'dk(C1)'
      const amb_4_4 = rule.filter((curr, idx) =>
        curr.rule_type === "C3"
        && curr.modifier_prev_deadkey === rule[index].modifier_prev_deadkey
        && curr.id_prev_deadkey !== rule[index].id_prev_deadkey
        && curr.prev_deadkey === rule[index].prev_deadkey
        && rule[index].unique_prev_deadkey !== 0
        && idx < index
      );

      // 4-4 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(C11)
      const dup_4_4 = rule.filter((curr, idx) =>
        curr.rule_type === "C3"
        && curr.modifier_prev_deadkey === rule[index].modifier_prev_deadkey
        && curr.prev_deadkey === rule[index].prev_deadkey
        && curr.id_prev_deadkey === rule[index].id_prev_deadkey
        && idx < index
        // && rule[index].unique_prev_deadkey !== 0
      );

      // 6-6 dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'B'
      const amb_6_6 = rule.filter((curr, idx) =>
        (curr.rule_type === "C3")
        && curr.id_deadkey === rule[index].id_deadkey
        && rule[index].unique_prev_deadkey !== 0
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && (new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output))

        && idx < index
      );

      // 6-6 dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'Ã'
      const dup_6_6 = rule.filter((curr, idx) =>
        (curr.rule_type === "C3")
        && curr.id_deadkey === rule[index].id_deadkey
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && (new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output))
        && idx < index
      );

      // 5-5
      const amb_5_5 = rule.filter((curr, idx) =>
        (curr.rule_type === "C3")
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && curr.id_deadkey === rule[index].id_deadkey
        && (new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output))
        && idx < index
        //&& rule[index].unique_prev_deadkey !== 0
      );

      // 5-5
      const dup_5_5 = rule.filter((curr, idx) =>
        (curr.rule_type === "C3")
        && curr.modifier_deadkey === rule[index].modifier_deadkey
        && curr.deadkey === rule[index].deadkey
        && curr.id_prev_deadkey === rule[index].id_prev_deadkey
        && curr.id_deadkey === rule[index].id_deadkey
        && rule[index].unique_deadkey === 0
        //&& idx < index
      );

      if (amb_1_4.length > 0) {
        warningTextArray[0] = warningTextArray[0]
          + ("C3 FIRSTTEXT "
            + "ambiguous 1-4 rule: earlier(Todo check if print out or not): ["
            + amb_1_4[0].modifier_key
            + " "
            + amb_1_4[0].key
            + "]  >  \'"
            + new TextDecoder().decode(amb_1_4[0].output)
            + "\' here: ")
      }

      if (amb_2_4.length > 0) {
        warningTextArray[0] = warningTextArray[0]
          + ("C3 FIRSTTEXT "
            + "ambiguous 2-4 rule: earlier(Todo check if print out or not): ["
            + amb_2_4[0].modifier_deadkey
            + " "
            + amb_2_4[0].deadkey
            + "]  >  dk(C"
            + amb_2_4[0].id_deadkey
            + ") here: ")
      }

      if (amb_6_3.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("C3 SECONDTEXT "
            + "ambiguous 6-3 rule: earlier: dk(C"
            + amb_6_3[0].id_deadkey
            + ") + ["
            + amb_6_3[0].modifier_key
            + " "
            + amb_6_3[0].key
            + "]  >  \'"
            + new TextDecoder().decode(amb_6_3[0].output)
            + "\' here: ")
      }

      if (dup_6_3.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("C3 SECONDTEXT "
            + "duplicate 6-3 rule: earlier: dk(C"
            + dup_6_3[0].id_deadkey
            + ") + ["
            + dup_6_3[0].modifier_key
            + " "
            + dup_6_3[0].key
            + "]  >  \'"
            + new TextDecoder().decode(dup_6_3[0].output)
            + "\' here: ")
      }

      if (amb_4_4.length > 0) {
        warningTextArray[0] = warningTextArray[0]
          + ("C3 FIRSTTEXT "
            + "ambiguous 4-4 rule: earlier: ["
            + amb_4_4[0].modifier_prev_deadkey
            + " "
            + amb_4_4[0].prev_deadkey
            + "]  >  dk(C"
            + amb_4_4[0].id_prev_deadkey
            + ") here: ")
      }

      if (dup_4_4.length > 0) {
        warningTextArray[0] = warningTextArray[0]
          + ("C3 FIRSTTEXT "
            + "duplicate 4-4 rule: earlier: ["
            + dup_4_4[0].modifier_prev_deadkey
            + " "
            + dup_4_4[0].prev_deadkey
            + "]  >  dk(C"
            + dup_4_4[0].id_prev_deadkey
            + ") here: ")
      }

      if (amb_5_5.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("C3 SECONDTEXT "
            + "ambiguous 5-5 rule: earlier: dk(B"
            + amb_5_5[0].id_prev_deadkey
            + ") + ["
            + amb_5_5[0].modifier_deadkey
            + " "
            + amb_5_5[0].deadkey
            + "]  >  dk(B"
            + amb_5_5[0].id_deadkey
            + ") here: ")
      }

      if (dup_5_5.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("C3 SECONDTEXT "
            + "duplicate 5-5 rule: earlier: dk(B"
            + dup_5_5[0].id_prev_deadkey
            + ") + ["
            + dup_5_5[0].modifier_deadkey
            + " "
            + dup_5_5[0].deadkey
            + "]  >  dk(B"
            + dup_5_5[0].id_deadkey
            + ") here: ")
      }

      if (amb_6_6.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("C3 THIRDTEXT "
            + "ambiguous 6-6 rule: earlier: dk(B"
            + amb_6_6[0].id_deadkey
            + ") + ["
            + amb_6_6[0].modifier_key
            + " "
            + amb_6_6[0].key
            + "]  >  \'"
            + new TextDecoder().decode(amb_6_6[0].output)
            + "\' here: ")
      }

      if (dup_6_6.length > 0) {
        warningTextArray[2] = warningTextArray[2]

          + ("C3 THIRDTEXT "
            + "duplicate 6-6x rule: earlier: dk(B"
            + dup_6_6[0].id_deadkey
            + ") + ["
            + dup_6_6[0].modifier_key
            + " "
            + dup_6_6[0].key
            + "]  >  \'"
            + new TextDecoder().decode(dup_6_6[0].output)
            + "\' here: ")
      }
    }

    if (warningTextArray[0] !== "") {
      warningTextArray[0] = "c WARNING: " + warningTextArray[0]
    }
    if (warningTextArray[1] !== "") {
      warningTextArray[1] = "c WARNING: " + warningTextArray[1]
    }
    if (warningTextArray[2] !== "") {
      warningTextArray[2] = "c WARNING: " + warningTextArray[2]
    }

    return warningTextArray
  }

  /**
   * @brief  member function to map Ukelele keycodes to Windows Keycodes
   * @param  pos Ukelele (=mac) keycodes
   * @return keycode on a Windows Keyboard
   */
  public map_UkeleleKC_To_VK(pos: number): string {

    // ukelele KC  -->  // VK_US
    if (pos === 0x0A) return "K_BKQUOTE"    /* ^ */
    else if (pos === 0x12) return "K_1"          /* 1 */
    else if (pos === 0x13) return "K_2"          /* 2 */
    else if (pos === 0x14) return "K_3"          /* 3 */
    else if (pos === 0x15) return "K_4"          /* 4 */
    else if (pos === 0x17) return "K_5"          /* 5 */
    else if (pos === 0x16) return "K_6"          /* 6 */
    else if (pos === 0x1A) return "K_7"          /* 7 */
    else if (pos === 0x1C) return "K_8"          /* 8 */
    else if (pos === 0x19) return "K_9"          /* 9 */
    else if (pos === 0x1D) return "K_0"          /* 0 */
    else if (pos === 0x1B) return "K_HYPHEN"     /* ß */
    else if (pos === 0x18) return "K_EQUAL"      /* ´ */

    else if (pos === 0x0C) return "K_Q"          /* Q */
    else if (pos === 0x0D) return "K_W"          /* W */
    else if (pos === 0x0E) return "K_E"          /* E */
    else if (pos === 0x0F) return "K_R"          /* R */
    else if (pos === 0x11) return "K_T"          /* T */
    else if (pos === 0x10) return "K_Y"          /* Y */
    else if (pos === 0x20) return "K_U"          /* U */
    else if (pos === 0x22) return "K_I"          /* I */
    else if (pos === 0x1F) return "K_O"          /* O */
    else if (pos === 0x23) return "K_P"          /* P */
    else if (pos === 0x21) return "K_LBRKT"      /* [ */
    else if (pos === 0x1E) return "K_RBRKT"      /* ] */
    else if (pos === 0x31) return "K_SPACE"      /* \ */
    else if (pos === 0x2A) return "K_BKSLASH"    /* \ */   // 42 for ISO  correct??
    // else if (pos === 0x30) return "K_?C1"     /* \ */   // 48 for ANSI  correct??

    else if (pos === 0x00) return "K_A"          /* A */
    else if (pos === 0x01) return "K_S"          /* S */
    else if (pos === 0x02) return "K_D"          /* D */
    else if (pos === 0x03) return "K_F"          /* F */
    else if (pos === 0x05) return "K_G"          /* G */
    else if (pos === 0x04) return "K_H"          /* H */
    else if (pos === 0x26) return "K_J"          /* J */
    else if (pos === 0x28) return "K_K"          /* K */
    else if (pos === 0x25) return "K_L"          /* L */
    else if (pos === 0x29) return "K_COLON"      /* : */
    else if (pos === 0x27) return "K_QUOTE"      /* " */

    else if (pos === 0x23) return "K_oE2"        /* | */
    else if (pos === 0x06) return "K_Z"          /* Z */
    else if (pos === 0x07) return "K_X"          /* X */
    else if (pos === 0x08) return "K_C"          /* C */
    else if (pos === 0x09) return "K_V"          /* V */
    else if (pos === 0x0B) return "K_B"          /* B */
    else if (pos === 0x2D) return "K_N"          /* N */
    else if (pos === 0x2E) return "K_M"          /* M */
    else if (pos === 0x2B) return "K_COMMA"      /* , */
    else if (pos === 0x2F) return "K_PERIOD"     /* . */
    else if (pos === 0x2C) return "K_SLASH"      /* / */

    else if (pos === 0x24) return "K_ENTER"
    else return ""
  }
  //----------------------------------------------------------------------------------------------------
  //----------------------------------------------------------------------------------------------------
  //----------------------------------------------------------------------------------------------------

  public createData_Rules(data_ukelele: convert_object): string {

    let data: string = ""

    // create array of all rules and remove duplicates
    const data_rules: rule_object[] = data_ukelele.arrayOf_Rules.filter((curr) => {
      if ((curr.output !== new TextEncoder().encode("") || curr.output !== undefined)
        && (curr.rule_type === "C0") || (curr.rule_type === "C1") || (curr.rule_type === "C2") || (curr.rule_type === "C3")) {
        return curr;
      } else {
        return ""
      }
    });
    const unique_data_Rules: rule_object[] = data_rules.reduce((unique, o) => {
      if (!unique.some((obj: {
        modifier_prev_deadkey: string; prev_deadkey: string;
        modifier_deadkey: string; deadkey: string;
        rule_type: string; modifier_key: string; key: string;
        output: Uint8Array;
      }) =>
        new TextDecoder().decode(obj.output) === new TextDecoder().decode(o.output)

        && obj.rule_type === o.rule_type
        && obj.modifier_key === o.modifier_key
        && obj.key === o.key

        && obj.modifier_deadkey === o.modifier_deadkey
        && obj.deadkey === o.deadkey

        && obj.modifier_prev_deadkey === o.modifier_prev_deadkey
        && obj.prev_deadkey === o.prev_deadkey)
      ) {
        unique.push(o);
      }
      return unique;
    }, []);

    // ToDo remove
    //const unique_data_Rules: rule_object[] = data_ukelele.arrayOf_Rules
    console.log("xx  data_ukelele.arrayOf_Rules", data_ukelele.arrayOf_Rules.length)
    //console.log("xx  data_ukelele.arrayOf_Rules", this.writeDataset(data_ukelele.arrayOf_Rules))
    console.log("xx  unique_data_Rules", unique_data_Rules.length)
    console.log("xx  unique_data_Rules", this.writeDataset(unique_data_Rules))

    //................................................ C0 C1 ................................................................
    //................................................ C0 C1 ................................................................
    //................................................ C0 C1 ................................................................

    for (let k = 0; k < unique_data_Rules.length; k++) {

      if ((unique_data_Rules[k].rule_type === "C0") || (unique_data_Rules[k].rule_type === "C1")) {

        // lookup key nr of the key that is being processed
        let keyNr: number = 0;
        for (let j = 0; j < KeylayoutToKmnConverter.USED_KEYS_COUNT; j++) {
          if (this.map_UkeleleKC_To_VK(j) === unique_data_Rules[k].key) {
            keyNr = j
            break
          }
        }

        // skip keyNr 48 (K_TAB) and 36 (K_ENTER)
        if ((keyNr === 48) || (keyNr === 36)) {
          continue
        }

        //---------------------------------------------------------------------------------------------

        // add a line after rules of each key
        if ((k > 1) && (unique_data_Rules[k - 1].key !== unique_data_Rules[k].key) && (unique_data_Rules[k - 1].rule_type === unique_data_Rules[k].rule_type)) {
          data += '\n'
        }

        const warn_text = this.reviewRules(unique_data_Rules, k)

        // add a warning in front of rules that use unavailable modifiers or ambiguous rules
        // if warning contains duplicate rules we do not write out this rule (even if there are other warnings for the same rule) since that rule had been written before

        // ToDo include condition again
        if ((warn_text[2].indexOf("duplicate") < 0)) {
          data +=
            warn_text[2]
            + "+ ["
            + (unique_data_Rules[k].modifier_key + ' ' + unique_data_Rules[k].key).trim()
            + `]  > \'`
            + new TextDecoder().decode(unique_data_Rules[k].output)
            + '\'\n'
        }
      }
    }

    // Todo remove this marker
    data += "\n c ########## C2 #################################################################\n"

    //................................................ C2 ...................................................................
    //................................................ C2 ...................................................................
    //................................................ C2 ...................................................................
    for (let k = 0; k < unique_data_Rules.length; k++) {

      if (unique_data_Rules[k].rule_type === "C2") {

        const warn_text = this.reviewRules(unique_data_Rules, k)

        //SECONDTEXT print
        // ToDo include condition again
        //      if ((warn_text[1].indexOf("duplicate") < 0)) {
        data += warn_text[1]
          + "+ ["
          + (unique_data_Rules[k].modifier_deadkey + " " + unique_data_Rules[k].deadkey).trim()
          + "]  >  dk(C"
          + String(unique_data_Rules[k].id_deadkey)
          + ")\n"
        //      }

        // THIRDTEXT print OK
        // ToDo include condition again
        //     if ((warn_text[2].indexOf("duplicate") < 0)) {
        data +=
          warn_text[2]
          + "dk(C"
          + (String(unique_data_Rules[k].id_deadkey) + ") + [" + unique_data_Rules[k].modifier_key).trim()
          + " "
          + unique_data_Rules[k].key + "]  >  \'"
          + new TextDecoder().decode(unique_data_Rules[k].output)
          + "\'\n"

        data += "\n"
        //     }
      }
    }

    //................................................ C3 ...................................................................
    //................................................ C3 ...................................................................
    //................................................ C3 ...................................................................

    data += "\nc ########## C3 #################################################################\n"

    for (let k = 0; k < unique_data_Rules.length; k++) {
      if (unique_data_Rules[k].rule_type === "C3") {

        const warn_text = this.reviewRules(unique_data_Rules, k)

        // ToDo include condition again
        // FIRSTTEXT print
        //    if ((warn_text[0].indexOf("duplicate") < 0)) {
        data +=
          warn_text[0]
          + " ["
          + (unique_data_Rules[k].modifier_prev_deadkey + " " + unique_data_Rules[k].prev_deadkey).trim()
          + "]   >   dk(A"
          + String(unique_data_Rules[k].id_prev_deadkey) + ")\n"
        //      }

        // ToDo include condition again
        //SECONDTEXT print
        //      if ((warn_text[1].indexOf("duplicate") < 0)) {
        data +=
          warn_text[1]
          + "dk(A"
          + (String(unique_data_Rules[k].id_prev_deadkey) + ")  + [" + unique_data_Rules[k].modifier_deadkey).trim()
          + " "
          + unique_data_Rules[k].deadkey
          + "]  >  dk(B"
          + String(unique_data_Rules[k].id_deadkey)
          + ")\n"
        //      }

        // ToDo include condition again
        // THIRDTEXT print OK
        //      if ((warn_text[2].indexOf("duplicate") < 0)) {
        data +=
          warn_text[2] + "dk(B"
          + (String(unique_data_Rules[k].id_deadkey) + ") + [" + unique_data_Rules[k].modifier_key).trim()
          + " "
          + unique_data_Rules[k].key
          + "]  >  \'"
          + new TextDecoder().decode(unique_data_Rules[k].output)
          + "\'\n"
        //      }

        // if ((warn_text[0].indexOf("duplicate") < 0) || (warn_text[1].indexOf("duplicate") < 0) || (warn_text[2].indexOf("duplicate") < 0)) {
        data += "\n"
        //   }

      }
    }

    return data
  }


  public createData_Stores(data_ukelele: convert_object): string {

    let data: string = ""
    data += "c ......................................................................\n"
    data += "c ......................................................................\n"
    data += "c Keyman keyboard generated by kmn-convert\n"
    data += "c from Ukelele file: " + data_ukelele.keylayout_filename + "\n"
    data += "c ......................................................................\n"
    data += "c ......................................................................\n"
    data += "\n"

    data += "store(&VERSION) \'10.0\'\n"
    data += "store(&TARGETS) \'any\'\n"
    data += "store(&KEYBOARDVERSION) \'1.0\'\n"
    data += "store(&COPYRIGHT) '© 2024 SIL International\'\n"

    data += "\n"
    data += "begin Unicode > use(main)\n\n"
    data += "group(main) using keys\n\n"

    data += "\n"
    return data
  }

  /// console log all entries C3  TODO remove
  public writeDataset(dataRules: rule_object[]) {
    for (let i = 0; i < dataRules.length; i++) {

      console.log("dataRules ", i,

        dataRules[i].rule_type !== "" ? dataRules[i].rule_type : "--".padEnd(4, " "),

        "|  ", (dataRules[i].modifier_prev_deadkey !== "" ? dataRules[i].modifier_prev_deadkey.padEnd(33, " ") : "--".padEnd(33, " ")),
        (dataRules[i].prev_deadkey !== "" ? dataRules[i].prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        (dataRules[i].id_prev_deadkey !== 0 ? ("dk(A" + String(dataRules[i].id_prev_deadkey) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),
        (dataRules[i].unique_prev_deadkey !== 0 ? ("unique(A" + String(dataRules[i].unique_prev_deadkey) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),

        (dataRules[i].modifier_deadkey !== "" ? dataRules[i].modifier_deadkey.padEnd(30, " ") : "--".padEnd(30, " ")),
        (dataRules[i].deadkey !== "" ? dataRules[i].deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        dataRules[i].rule_type === "C2" ? (dataRules[i].unique_deadkey !== 0 ? ("unique(C" + String(dataRules[i].unique_deadkey) + ")").padEnd(9, " ") : "--".padEnd(9, " ")) : ((dataRules[i].unique_prev_deadkey !== 0 ? ("unique(B" + ")").padEnd(9, " ") : "--".padEnd(9, " "))),
        dataRules[i].id_deadkey,
        dataRules[i].id_prev_deadkey,

        "|  ", (dataRules[i].modifier_key !== "" ? dataRules[i].modifier_key.padEnd(40, " ") : "--".padEnd(40, " ")),
        (dataRules[i].key !== "" ? dataRules[i].key.padEnd(8, " ") : "--".padEnd(8, " ")),
        (new TextDecoder().decode(dataRules[i].output) !== "" ? new TextDecoder().decode(dataRules[i].output).padEnd(10, " ") : ("--" + dataRules[i].output) + "--"),

        "| °°")
    }

  }
  // TODO remove
  public writeDatasetSingle(dataRules: rule_object) {

    console.log("dataRules ",
      dataRules.rule_type !== "" ? dataRules.rule_type : "--".padEnd(4, " "),

      "|  ", (dataRules.modifier_prev_deadkey !== "" ? dataRules.modifier_prev_deadkey.padEnd(33, " ") : "--".padEnd(33, " ")),
      (dataRules.prev_deadkey !== "" ? dataRules.prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
      (dataRules.id_prev_deadkey !== 0 ? ("dk(A" + String(dataRules.id_prev_deadkey) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),
      (dataRules.unique_prev_deadkey !== 0 ? ("unique(A" + String(dataRules.unique_prev_deadkey) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),

      (dataRules.modifier_deadkey !== "" ? dataRules.modifier_deadkey.padEnd(30, " ") : "--".padEnd(30, " ")),
      (dataRules.deadkey !== "" ? dataRules.deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
      dataRules.rule_type === "C2" ? (dataRules.unique_deadkey !== 0 ? ("unique(C" + String(dataRules.unique_deadkey) + ")").padEnd(9, " ") : "--".padEnd(9, " ")) : ((dataRules.unique_prev_deadkey !== 0 ? ("unique(B" + ")").padEnd(9, " ") : "--".padEnd(9, " "))),

      dataRules.id_deadkey,
      dataRules.id_prev_deadkey,

      "|  ", (dataRules.modifier_key !== "" ? dataRules.modifier_key.padEnd(40, " ") : "--".padEnd(40, " ")),
      (dataRules.key !== "" ? dataRules.key.padEnd(8, " ") : "--".padEnd(8, " ")),
      (new TextDecoder().decode(dataRules.output) !== "" ? new TextDecoder().decode(dataRules.output).padEnd(10, " ") : ("--" + dataRules.output) + "--"),

      "| °°")

  }
}

class Rules {
  constructor(
    public rule_type: string,             /* C0, C1, C2, C3, or C4 */

    public modifier_prev_deadkey: string, /* first key used by C3 rules*/
    public prev_deadkey: string,
    public id_prev_deadkey: number,
    public unique_prev_deadkey: number,

    public modifier_deadkey: string,      /* second key used by C2,C3 rules*/
    public deadkey: string,
    public id_deadkey: number,
    public unique_deadkey: number,

    public modifier_key: string,          /* third key used by C0,C1,C2,C3,C4 rules*/
    public key: string,
    public output: Uint8Array,            /* output used by C0,C1,C2,C3,C4 rules*/

  ) { }

}
