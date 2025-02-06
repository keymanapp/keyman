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
  // OK  write read, convert, write
  //     tests for 3 functions read write convert
  // OK  add data to object
  // OK  Use filter functions
  // OK  action/output:use filter etc to shorten func
  // OK  deadkeyables:use filter etc to shorten func
  // OK  dk-> for all action:use filter etc to shorten func
  //     remove unneccessary data from dataObject
  //     rename symbols
  // OK  remove part using kmn_key_Name1
  // OK  remove unnecceaasry map_UkeleleKC_To_kmn_Key_Name_Array_Position_n etc
  // OK  loop throught ANSI, JIS- at moment only use [keyMapSet_count] (keyMapSet_count=0)
  // OK  remove funcs at teh end
  //     import { makePathToFixture } from '../../test/helpers/index.js';
  // OK  Mapping 0->30  or 0->K_A-> missing entries in mapping 
  // OK  Replace any-types
  // OK  Several steps action-> action-> action->character ( not only  action->character)
  //     TODO waht about using actions twice in a row??? -> error msg if chain >4
  // OK  Usable for all keylayout files
  //     Return conditions
  // OK  Use callbacks as for writeFileSync
  //     Tests throws
  //     Conditions NCAPS,OPT;...
  //     TODO move func outside of class
  //     Functions as object methods? 
  //     objects contain only used stuff READ in: -- out: only read arrays / CONVERT in: only read arrays out: return only to write arrays
  // OK  Use catch blocks for file read
  // OK  read:  answer :  + [K_A] > 'a'  is OK / TODO which format to use in output ?  + [K_A] > 'a' (character code)  or    + [K_A] > U+0061 (virt Keycode)
  //     read   TODO which stores?
  // OK  one entry vs several entry in tags
  //     false arrangement of tags ?
  // OK  no added NCAPS when first modifier in modifierMap does not contain "caps" or"caps?"
  //     use length to clear array instead of defining evetry time new (modifierMap_ONE_InKeymapSelect.length=0
  // OK  naming of for-loop var i,j,k?...
  //     warning if contrADICTING RULES E:G:   [ 'modifier_C0', 'SHIFT NCAPS', 'K_C', 'c'],  vs [ 'modifier_C0', 'SHIFT NCAPS', 'K_C', 'C' ],
  // OK  print NCAPS as the first of the modifiers in create_kmn_modifier
  // OK  use boxXmlArray from codebase instead of my own
  // OK  where are rules for action+none ( a, e, u, etc)
  // OK  order of object member var
  // OK  readfilesync with paths better way?
  // OK  rearrange code to use read, convert, write
  // OK  TODO rewrite explanantion for object instead of array
  // OK  remove all any types
  //     check code for code styles keyman
  //     public dk: number,                    //todo remove one
  // OK  todo use from elsewhere boxXmlArray_S
  // OK  prev_deadkeys_Ch: Uint8Array,   /* Todo needed?*/
  //     Todo remove print_draft
  //     TODO throws
  // OK  Todo files/path !!!
  //     TODO need to use export const USVirtualKeyCodes here
  //     start Tests v ToDo remove......................................
  //     TODO remove:  test files: checkif kmn gets the same output as ukelele file(except for C3 t works well :))
  //     ToDo needed methods
  //     Filter functions use the same type
  //     where to put documentation
  //     dk <-> dk_C2 ???
  //     check if we use the same  algorithm to get Block1 data
  //     check uniqueA, uniqueV, dk_prv, dk, dk_C2
}

import { XMLParser } from 'fast-xml-parser';  // for reading a file
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
  dk_prev: number,                /* Todo needed?*/
  uniqueA: number,

  modifier_deadkey: string,       /* string of modifiers for the second key (e.g. "NCAPS RALT CTRL") */
  deadkey: string,                /* name of the second key */
  dk: number,                     /* Todo needed?*/
  dk_C2: number,
  uniqueB: number,

  modifier_key: string,           /* string of modifiers for the third key (e.g. "NCAPS RALT CTRL") */
  key: string,                    /* name of the third key (e.g. K_U) */
  output: Uint8Array,             /* the output character */
};

export interface convert_object {
  ArrayOf_Modifiers: string[][],
  ArrayOf_Rules: rule_object[],
};


export class KeylayoutToKmnConverter {
  static readonly INPUT_FILE_EXTENSION = '.keylayout';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';

  //Todo remove print_draft
  static print_draft = false
  static used_Keys_count = 50

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
    const JsonO: object = this.read(inputFilename)

    if (!JsonO) {
      return null;
    }

    console.log(' _S2 then CONVERT ........................................');
    //const outArray: convert_object = await this.convert(inArray);
    const outArray: convert_object = await this.convert(JsonO);

    if (!outArray) {
      return null;
    }

    console.log(' _S2 then WRITE to kmn .....................................');

    const out: boolean = this.write(outArray)
    if (!out) {
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
      attributeNamePrefix: '@_'    // to access the attribute
    };

    try {

      //const xmlFile = readFileSync(`${process.cwd()}/data/MySample.keylayout`, 'utf8')
      xmlFile = readFileSync((process.cwd() + "\\data" + filename.substring(filename.lastIndexOf("\\"))).replace(" ", ""), 'utf8');
      const parser = new XMLParser(options);
      jsonObj = parser.parse(xmlFile); // get plain Object
      boxArrays(jsonObj.keyboard);// jsonObj now contains only arrays; no single fields

      console.log("inputFilename read", filename)
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
    const RuleObject: rule_object[] = []             // an array of objects which hold data for a kmn rule

    const DataObject: convert_object = {
      ArrayOf_Modifiers: modifierBehavior,   // e.g. 18 modifiersCombinations in 8 KeyMapSelect(behaviors)
      ArrayOf_Rules: RuleObject
    };

    // create an array of modifier Combinations (e.g. shift? leftShift caps? )
    for (let j = 0; j < jsonObj.keyboard.modifierMap.keyMapSelect.length; j++) {
      const singleModifierSet: string[] = []
      for (let k = 0; k < jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier.length; k++) {
        singleModifierSet.push(jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier[k]['@_keys'])
      }
      modifierBehavior.push(singleModifierSet)
    }

    return this.createRuleData(DataObject, jsonObj)
  }


  /**
   * @brief   member function to write data fro object to file
   * @param  data_ukelele the array holding keyboard data
   * @return true if data has been written; false if not
   */
  //TODO need to use export const USVirtualKeyCodes here
  public write(data_ukelele: convert_object): boolean {
    console.log("\n###################################################\n")
    let data: string = "\n"

    // add top part of kmn file: STORES
    data += this.createData_Stores(data_ukelele)

    // add lower part of kmn file: RULES
    data += this.createData_Rules(data_ukelele)

    /*writeFileSync("data/MyResult.kmn", data, { flag: "w" })*/
    this.callbacks.fs.writeFileSync("data/MyResult.kmn", new TextEncoder().encode(data)) // not usable here since it takes UInt8array data

    // ToDo conditions?
    if (data.length > 0)
      return true;
    else
      return false
  }

  //   TODO move outside of class?
  // ToDo keep only uint8array-version
  // for more info about mapping and cases C0-C4 see https://docs.google.com/document/d/1ISjACTA9aUBueTo1AoKnOsI6QR5kXeeD_maI0XUyXqg/edit?tab=t.0

  public createRuleData(data_ukelele: convert_object, jsonObj: any): convert_object {
    const ObjectArray: rule_object[] = []
    let dk_counter_C3_A: number = 0
    let dk_counter_C3_B: number = 0
    let dk_counter_C2: number = 0

    // start Tests v ToDo remove......................................
    /*const testArray_Ukelele: string[] = []
    let testArray_Ukelele_count = 0
    const testArray_Ukelele_action: string[] = []
    let testArray_Ukelele_action_count = 0
    const testArray_kmn: string[] = []
    //const testArray_kmn_action1: string[] = []
    let testArray_kmn_count = 0

    // loop behaviors ( in ukelele it is possible to define multiple modifier combinations that behave in the same)
    for (let i = 0; i < jsonObj.keyboard.keyMapSet[0].keyMap.length; i++) {
      // loop keys 0-50 (= all keys we use)
      for (let j = 0; j <= KeylayoutToKmnConverter.used_Keys_count; j++) {

        if ((jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== undefined)) {
          testArray_Ukelele.push(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'])
          testArray_Ukelele_count++
        }
      }
    }

    for (let k = 0; k < jsonObj.keyboard.actions.action.length; k++) {
      for (let j = 0; j < jsonObj.keyboard.actions.action[k].when.length; j++) {
        if (jsonObj.keyboard.actions.action[k].when[j]['@_output'] !== undefined) {
          testArray_Ukelele_action.push(jsonObj.keyboard.actions.action[k].when[j]['@_output'])
          testArray_Ukelele_action_count++
        }
      }
    }*/
    // End Tests ToDo remove   ^.......................................


    let action_id: string

    const isCapsused: boolean = this.checkIfCapsIsUsed(data_ukelele.ArrayOf_Modifiers)

    // loop keys 0-50 (= all keys we use)
    for (let j = 0; j <= KeylayoutToKmnConverter.used_Keys_count; j++) {

      // loop behaviors (in ukelele it is possible to define multiple modifier combinations that behave in the same way)
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[0].keyMap.length; i++) {
        let RuleObj: rule_object

        // ......................................................................................................
        // case C0: output ......................................................................................
        // a key is mapped to a character directly ( code-> output) .............................................
        // ...............e. g. <key code="1" output="s"/> ......................................................
        // ......................................................................................................

        if ((jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== undefined)) {

          if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== "") {

            // loop behaviours
            for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {
              if (this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']))) {

                RuleObj = new Rules(
                          /*   rule_type */               "C0",

                          /*   modifier_prev_deadkey*/    "",
                          /*   prev_deadkey */            "",
                          /*   dk_prev */                 0,
                          /*   unique A */                0,

                          /*   modifier_deadkey */        "",
                          /*   deadkey */                 "",
                          /*   dk*/                       0,
                          /*   dk for C2*/                0,
                          /*   unique B */                0,

                          /*   modifier_key*/             this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                          /*   key */                     this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                          /*   output */                  new TextEncoder().encode(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output']),
                )
                if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== "")
                  ObjectArray.push(RuleObj)
              }
            }
          }
        }
        else if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] !== undefined) {

          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']

          // ......................................................................................................
          // case C1: action + state none + output ................................................................
          // a key is mapped to an action and then to an output ...................................................
          // KeyMap:code->KeyMap:action->action:action_state(none)->action_output .................................
          // ...............e. g. <when state="none" output="a" ...................................................
          // ......................................................................................................

          for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {

            if ((this.get_Action2ID_NoneOutput__From__ActionID_Id(jsonObj, action_id) !== undefined) && (this.get_Action2ID_NoneOutput__From__ActionID_Id(jsonObj, action_id) !== "")) {

              const outputchar: string = this.get_Action2ID_NoneOutput__From__ActionID_Id(jsonObj, action_id)
              const resultArraySS_Key_Out: string[][] = this.get_Datat_array2D__From__ActionID_stateOutput(jsonObj, data_ukelele.ArrayOf_Modifiers, action_id, outputchar, isCapsused)

              for (let m = 0; m < resultArraySS_Key_Out.length; m++) {

                RuleObj = new Rules(
                          /*   rule_type */               "C1",

                          /*   modifier_prev_deadkey*/    "",
                          /*   prev_deadkey */            "",
                          /*   dk_prev */                 0,
                          /*   unique A */                0,

                          /*   modifier_deadkey */        "",
                          /*   deadkey */                 "",
                          /*   dk*/                       0,
                          /*   dk for C2*/                0,
                          /*   unique B */                0,

                          /*   modifier_key*/             resultArraySS_Key_Out[m][5],
                          /*   key */                     resultArraySS_Key_Out[m][4],
                          /*   output */                  new TextEncoder().encode(outputchar)
                )
                ObjectArray.push(RuleObj)
              }
            }
          }

          // ......................................................................................................
          // case C2: action + none + next ........................................................................
          // ...............e. g.<when state="none" next="20"/> ...................................................
          // replace state x with all rules that result in 14 (<when state="x" next="14") .........................
          // ......................................................................................................
          // Definition of Blocks 1-6 see .........................................................................
          // https://docs.google.com/document/d/1ISjACTA9aUBueTo1AoKnOsI6QR5kXeeD_maI0XUyXqg/edit?tab=t.0 .........
          // ......................................................................................................

          const actionIdIndex2: number = this.get_ActionID_Index__From__ActionID_Id(jsonObj, action_id)

          // loop all action-when and find state-next-pair
          for (let l = 0; l < jsonObj.keyboard.actions.action[actionIdIndex2].when.length; l++) {

            // find state ="none" - next data
            if ((jsonObj.keyboard.actions.action[actionIdIndex2].when[l]['@_state'] === "none")
              && (jsonObj.keyboard.actions.action[actionIdIndex2].when[l]['@_next'] !== undefined)) {

              // Data of Block Nr 5 ....................................................................................................................
              /*  eg: next  = 1  */                       const b5_value_next: string = jsonObj.keyboard.actions.action[actionIdIndex2].when[l]['@_next']
              /*  eg: StateNextID = a16  */               const b5_actionId: string[] = jsonObj.keyboard.actions.action[actionIdIndex2]['@_id']
              // .......................................................................................................................................

              // Data of Block Nr 4 ....................................................................................................................
              /*  eg: [ '6', '31', '32' ]*/               const b4_code_arr: string[] = this.get_KeyMap_Code_array__From__KeyMap_Action(jsonObj, b5_actionId)
              /*  eg: [['24', 0], ['24', 3]] */           const b4_keyBehaviour_arr: number[][] = this.get_KeyMap_Code_array__From__ActionID_Action(jsonObj, String(action_id))
              /* e.g. [['','caps?'], ['Caps']]*/          const b4_modifier2D_array: string[] = this.get_KeyMap_Modifier_array__From__behaviour_arr(data_ukelele.ArrayOf_Modifiers, b4_keyBehaviour_arr)
              // .......................................................................................................................................

              // Data of Block Nr 6 ....................................................................................................................
              /*  eg: [ 'a9','1','â'] */                  const b6_actionId_arr: string[][] = this.get_ActionID_Output_array__From__ActionID_State(jsonObj, b5_value_next)
              // .......................................................................................................................................

              // Data of Block Nr 1  ....................................................................................................................
              /*  eg: ['49','K_SPACE','a0','0','Â'] */    const b1_keycode_arr: string[][] = this.get_KeyMap_Code_array__From__KeyMap_Action_array2D(jsonObj, b6_actionId_arr)
              /*  eg: ['K_SPACE','a0','0','NCAPS','Â']*/  const b1_KeyMapModiKeyArray: string[][] = this.get_KeyMapModiKeyArray__from__array(jsonObj, b1_keycode_arr, isCapsused)
              // .......................................................................................................................................

              for (let n1 = 0; n1 < b4_modifier2D_array.length; n1++) {
                for (let n2 = 0; n2 < b4_modifier2D_array[n1].length; n2++) {
                  for (let n3 = 0; n3 < b4_code_arr.length; n3++) {
                    for (let n4 = 0; n4 < b1_KeyMapModiKeyArray.length; n4++) {

                      RuleObj = new Rules(
                                /*   rule_type */             "C2",

                                /*   modifier_prev_deadkey*/  "",
                                /*   prev_deadkey */          "",
                                /*   dk_prev */               0,
                                /*   unique A */              0,

                                /*   modifier_deadkey */      this.create_kmn_modifier(b4_modifier2D_array[n1][n2], isCapsused),
                                /*   deadkey */               this.map_UkeleleKC_To_VK(Number(b4_code_arr[n3])),
                                /*   dk*/                     0,
                                /*   dk for C2*/              dk_counter_C2++,
                                /*   unique B */              0,

                                /*   modifier_key*/           b1_KeyMapModiKeyArray[n4][3],
                                /*   key */                   b1_KeyMapModiKeyArray[n4][0],
                                /*   output */                new TextEncoder().encode(b1_KeyMapModiKeyArray[n4][4]),
                      )
                      if (b1_KeyMapModiKeyArray[n4][4] !== undefined)
                        ObjectArray.push(RuleObj)
                    }
                  }
                }
              }
            }
          }

          // ......................................................................................................
          // case C3: action + state Nr + Next ....................................................................
          // ...............e. g.<when state="3" next="1"/> .......................................................
          // replace state x with all rules that result in 1 (<when state="x" next="1") ...........................
          // ......................................................................................................
          // Definition of Blocks 1-6 see .........................................................................
          // https://docs.google.com/document/d/1ISjACTA9aUBueTo1AoKnOsI6QR5kXeeD_maI0XUyXqg/edit?tab=t.0 .........
          // ......................................................................................................

          const actionIdIndex: number = this.get_ActionID_Index__From__ActionID_Id(jsonObj, action_id)

          // loop all action-when and find state-next-pair
          for (let l = 0; l < jsonObj.keyboard.actions.action[actionIdIndex].when.length; l++) {

            // find state_next data
            if ((jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_state'] !== "none")
              && (jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_next'] !== undefined)) {

              // Data of Block Nr 5 ....................................................................................................................
              /*  eg: state = 3  */                       const b5_value_state: string = jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_state']
              /*  eg: next  = 1  */                       const b5_value_next: string = jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_next']
              /*  eg: StateNextID = a16  */               const b5_actionId: string[] = jsonObj.keyboard.actions.action[actionIdIndex]['@_id']
              // .......................................................................................................................................

              // Data of Block Nr 4 ....................................................................................................................
              /*  eg: [ '6', '31', '32' ]*/               const b4_code_arr: string[] = this.get_KeyMap_Code_array__From__KeyMap_Action(jsonObj, b5_actionId)
              /*  eg: [['24', 0], ['24', 3]] */           const b4_keyBehaviour_arr: number[][] = this.get_KeyMap_Code_array__From__ActionID_Action(jsonObj, String(action_id))
              /* e.g. [['','caps?'], ['Caps']]*/          const b4_modifier2D_array: string[] = this.get_KeyMap_Modifier_array__From__behaviour_arr(data_ukelele.ArrayOf_Modifiers, b4_keyBehaviour_arr)
              // .......................................................................................................................................

              // Data of Block Nr 3 ....................................................................................................................
              /*  eg: actioniD = a17  */                  const b3_actionId: string = this.get_ActionID_Id__From__ActionID_next(jsonObj, b5_value_state)
              // .......................................................................................................................................

              // Data of Block Nr 2  ....................................................................................................................
              /*  eg: ['K_8', 'K_M]  */                   const b2_keyname_arr: string[] = this.get_KecCode_arr__From__ActionId(jsonObj, b3_actionId)
              /*  eg: index=3 */                          const b2_keyBehaviour_arr: number[][] = this.get_KeyMap_Code_array__From__ActionID_Action(jsonObj, String(b3_actionId))
              /* e.g. [[ '0','1shift? caps?']]*/          const b2_modifier_arr_all: string[] = this.get_KeyMap_Modifier_array__From__behaviour_arr(data_ukelele.ArrayOf_Modifiers, b2_keyBehaviour_arr)
              // .......................................................................................................................................

              // Data of Block Nr 6 ....................................................................................................................
              /*  eg: [ 'a9','1','â'] */                  const b6_actionId_arr: string[][] = this.get_ActionID_Output_array__From__ActionID_State(jsonObj, b5_value_next)
              // .......................................................................................................................................

              // Data of Block Nr 1  ....................................................................................................................
              /*  eg: ['49','K_SPACE','a0','0','Â'] */    const b1_keycode_arr: string[][] = this.get_KeyMap_Code_array__From__KeyMap_Action_array2D(jsonObj, b6_actionId_arr)
              /*  eg: ['K_SPACE','a0','0','NCAPS','Â']*/  const b1_KeyMapModiKeyArray: string[][] = this.get_KeyMapModiKeyArray__from__array(jsonObj, b1_keycode_arr, isCapsused)
              // .......................................................................................................................................

              for (let n1 = 0; n1 < b2_modifier_arr_all.length; n1++) {
                for (let n2 = 0; n2 < b2_modifier_arr_all[n1].length; n2++) {
                  for (let n3 = 0; n3 < b2_keyname_arr.length; n3++) {
                    for (let n4 = 0; n4 < b4_modifier2D_array.length; n4++) {
                      for (let n5 = 0; n5 < b4_modifier2D_array[n4].length; n5++) {
                        for (let n6 = 0; n6 < b4_code_arr.length; n6++) {
                          for (let n7 = 0; n7 < b1_KeyMapModiKeyArray.length; n7++) {

                            RuleObj = new Rules(
                                /*   rule_type */              "C3",

                                /*   modifier_prev_deadkey*/  this.create_kmn_modifier(b2_modifier_arr_all[n1][n2], isCapsused),
                                /*   prev_deadkey */          b2_keyname_arr[n3],
                                /*   dk_prev */               dk_counter_C3_A++,
                                /*   unique A */              0,

                                /*   modifier_deadkey */      this.create_kmn_modifier(b4_modifier2D_array[n4][n5], isCapsused),
                                /*   deadkey */               this.map_UkeleleKC_To_VK(Number(b4_code_arr[n6])),
                                /*   dk*/                     dk_counter_C3_B++,
                                /*   dk for C2*/              0,
                                /*   unique B */              0,

                                /*   modifier_key*/           b1_KeyMapModiKeyArray[n7][3],
                                /*   key */                   b1_KeyMapModiKeyArray[n7][0],
                                /*   output */                new TextEncoder().encode(b1_KeyMapModiKeyArray[n7][4]),
                            )
                            if (b1_KeyMapModiKeyArray[n7][4] !== undefined)
                              ObjectArray.push(RuleObj)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }

          // ......................................................................................................
          // case C4: action + state none + Next ..................................................................
          // ...............e. g. <when state="none" next="4"/> ...................................................
          // a key is mapped to an action and then to a terminator ................................................
          // action:state(none) -> action:next -> terminators:output ..............................................
          // action:id -> keyMap:action -> keyMap:code ( dk ) .....................................................
          // action:id -> keyMap:action -> keyMap:code ( key) .....................................................
          // ......................................................................................................

          const next_id: string = this.get_ActionID_Next__From__ActionID_None(jsonObj, action_id)

          for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {
            if (new TextEncoder().encode(this.get_Terminator_Output__From__Terminator_State(jsonObj, next_id)).length !== 0) {

              RuleObj = new Rules(
                        /*   rule_type */               "C4",

                        /*   modifier_prev_deadkey*/    "",
                        /*   prev_deadkey */            "",
                        /*   dk_prev */                 0,
                        /*   unique A */                0,

                        /*   modifier_deadkey */        "",
                        /*   deadkey */                 "",
                        /*   dk*/                       0,
                        /*   dk for C2*/                0,
                        /*   unique B */                0,

                        /*   modifier_key*/             this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                        /*   key */                     this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                        /*   output */                  new TextEncoder().encode(this.get_Terminator_Output__From__Terminator_State(jsonObj, next_id))
              )
            }
            if (this.get_Terminator_Output__From__Terminator_State(jsonObj, next_id) !== "")
              ObjectArray.push(RuleObj)
          }
        }
        else
          console.log("ERROR : some entries are not available")
      }
    }

    // ---------------------------------------------------------------------------------------------------------------------
    // ---------------------------------------------------------------------------------------------------------------------
    // now prepare for printing i.e. check for duplicate C2 and C3 rules and mark first occurance of rule
    // add nr to uniqueA, uniqueB if it is the first occurence of declaration of dk e.g. [NCAPS RALT K_8]  >  dk(C12) 
    // ---------------------------------------------------------------------------------------------------------------------
    // ---------------------------------------------------------------------------------------------------------------------
    let unique_dkB_count = 0
    const unique_ruleArrayC2: string[][] = []

    //----------------------------------- -dk----------------------------------
    // first rule is always unique
    ObjectArray[0].uniqueB = unique_dkB_count
    ObjectArray[0].dk_C2 = unique_dkB_count
    unique_dkB_count++

    for (let i = 0; i < ObjectArray.length; i++) {
      if ((ObjectArray[i].modifier_deadkey !== "") && (ObjectArray[i].deadkey !== "")) {
        let isUnique_dkB: boolean = true

        // check if not used before
        for (let j = 0; j < i; j++) {
          if ((ObjectArray[i].modifier_deadkey === ObjectArray[j].modifier_deadkey)
            && (ObjectArray[i].deadkey === ObjectArray[j].deadkey)) {
            isUnique_dkB = isUnique_dkB && false
          }
        }

        if (isUnique_dkB) {
          const ruleArray: string[] = []
          ObjectArray[i].uniqueB = unique_dkB_count
          ruleArray.push(ObjectArray[i].modifier_deadkey)
          ruleArray.push(ObjectArray[i].deadkey)
          ruleArray.push(String(unique_dkB_count))
          unique_dkB_count++
          unique_ruleArrayC2.push(ruleArray)
        }
      }
    }

    //-----------------------------------prev-dk----------------------------------
    let unique_dkA_count = 0

    // first rule is always unique
    ObjectArray[0].uniqueA = unique_dkA_count
    unique_dkA_count++

    for (let i = 0; i < ObjectArray.length; i++) {
      if ((ObjectArray[i].modifier_prev_deadkey !== "") && (ObjectArray[i].prev_deadkey !== "")) {
        let isUnique_dkA: boolean = true

        // check if not used before
        for (let j = 0; j < i; j++) {
          if ((ObjectArray[i].modifier_prev_deadkey === ObjectArray[j].modifier_prev_deadkey)
            && (ObjectArray[i].prev_deadkey === ObjectArray[j].prev_deadkey)) {
            isUnique_dkA = isUnique_dkA && false
          }
        }

        if (isUnique_dkA) {
          ObjectArray[i].uniqueA = unique_dkA_count
          unique_dkA_count++
          // check if first part of C3 rule contains already defined rule
          for (let k = 0; k < unique_ruleArrayC2.length; k++) {
            if ((unique_ruleArrayC2[k][0] === ObjectArray[i].modifier_deadkey) && ((unique_ruleArrayC2[k][1] === ObjectArray[i].deadkey)))
              ObjectArray[i].uniqueB = Number(unique_ruleArrayC2[k][2])
          }
        }

        if (isUnique_dkA) {
          const ruleArray: string[] = []
          ObjectArray[i].uniqueB = unique_dkB_count
          ruleArray.push(ObjectArray[i].modifier_prev_deadkey)
          ruleArray.push(ObjectArray[i].prev_deadkey)
          ruleArray.push(String(unique_dkB_count))
          unique_dkB_count++
          unique_ruleArrayC2.push(ruleArray)
        }
      }
    }

    for (let i = 0; i < ObjectArray.length; i++) {
      for (let j = 0; j < unique_ruleArrayC2.length; j++) {
        if ((ObjectArray[i].modifier_prev_deadkey === unique_ruleArrayC2[j][0]) && (ObjectArray[i].prev_deadkey === unique_ruleArrayC2[j][1])) {
          // write unique nr into rule obj
          ObjectArray[i].dk_prev = Number(unique_ruleArrayC2[j][2])
        }
        if ((ObjectArray[i].modifier_deadkey === unique_ruleArrayC2[j][0]) && (ObjectArray[i].deadkey === unique_ruleArrayC2[j][1])) {
          // write nr into rule obj
          ObjectArray[i].dk_C2 = Number(unique_ruleArrayC2[j][2])
        }
      }
    }

    // ---------------------------------------------------------------------------------------------------------------------
    // ---------------------------------------------------------------------------------------------------------------------


    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------
    // TODO remove:  test files: checkif kmn gets the same output as ukelele file
    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------
    /*
       //remove entries that are available in both arrays
       for (let i = 0; i < (testArray_Ukelele.length); i++) {
         for (let j = 0; j < (testArray_kmn.length); j++) {
           if (testArray_Ukelele[i] === testArray_kmn[j]) {
             testArray_Ukelele[i] = ""
             testArray_kmn[j] = ""
           }
         }
       }
       // remove empty fields
      const testArray_Ukelele_filter = testArray_Ukelele.filter((el) => (el !== ""))
       const testArray_kmn_filter = testArray_kmn.filter((el) => (el !== ""))
       // should be empty now
       if (testArray_kmn_filter.length !== testArray_Ukelele_filter.length)
         console.log("OOOOHHHH DIFFERENT AMOUNTS ")
       else
         console.log("####### SAME AMOUNTS :))  ",
           "testArray_kmn_filter:", testArray_kmn_filter, "testArray_Ukelele_filter:", testArray_Ukelele_filter)
   
       const testArray_Ukelele_action_uni: string[] = testArray_Ukelele_action.filter(function (item, pos, self) {
         return self.indexOf(item) == pos;
       })
   
       //remove entries that are available in both arrays
       for (let i = 0; i < (testArray_Ukelele_action_uni.length); i++) {
         for (let j = 0; j < (testArray_kmn_action1.length); j++) {
           if (testArray_Ukelele_action_uni[i] === testArray_kmn_action1[j]) {
             testArray_Ukelele_action_uni[i] = ""
             testArray_kmn_action1[j] = ""
           }
         }
       }
       // remove empty fields
       const testArray_Ukelele_action_uni_filter = testArray_Ukelele_action_uni.filter((el) => (el !== ""))
       const testArray_kmn_action_uni_filter = testArray_kmn_action1.filter((el) => (el !== ""))
       // should be empty now
       if ((testArray_kmn_action_uni_filter.length === 0) && (testArray_Ukelele_action_uni_filter.length === 0))
         console.log("####### SAME AMOUNTS :))   HERE ALSo",
           "testArray_kmn_action_uni_filter:", testArray_kmn_action_uni_filter, "testArray_Ukelele_action_uni_filter:", testArray_Ukelele_action_uni_filter)
       else
         console.log("OOOOHHHH DIFFERENT AMOUNTS ")*/



    //  test modifiers

    /*
      if (this.create_kmn_modifier("", true) === "NCAPS") console.log("testNr 1 OK "); else console.log("testNr 1 NOT OK  - ", (this.create_kmn_modifier("", true)))
      if (this.create_kmn_modifier("", false) === "") console.log("testNr 2 OK "); else console.log("testNr 2 NOT OK  - ", (this.create_kmn_modifier("", false)))
      if (this.create_kmn_modifier("caps", true) === "CAPS") console.log("testNr 5 OK "); else console.log("testNr 5 NOT OK  - ", (this.create_kmn_modifier("caps", true)))
      if (this.create_kmn_modifier("6caps", true) === "CAPS") console.log("testNr 6 OK "); else console.log("testNr 6 NOT OK - ", (this.create_kmn_modifier("6caps", true)))
      if (this.create_kmn_modifier("rightControl", false) === "CTRL") console.log("testNr 13 OK "); else console.log("testNr 13 NOT OK - ", (this.create_kmn_modifier("rightControl", false)))
      if (this.create_kmn_modifier("ComMand", false) === "ComMand") console.log("testNr 26 OK "); else console.log("testNr 26 NOT OK  - ", (this.create_kmn_modifier("ComMand", false)))
      if (this.create_kmn_modifier("rshift?", false) === "") console.log("testNr 19 OK "); else console.log("testNr 19 NOT OK  - ", (this.create_kmn_modifier("Shift?", false)))
      if (this.create_kmn_modifier("rshift?", true) === "NCAPS") console.log("testNr 20 OK "); else console.log("testNr 20 NOT OK  - ", (this.create_kmn_modifier("Shift?", true)))
      if (this.create_kmn_modifier("rshift", false) === "SHIFT") console.log("testNr 8 OK "); else console.log("testNr 8 NOT OK - ", (this.create_kmn_modifier("rshift", false)))
      if (this.create_kmn_modifier("rightshift", false) === "SHIFT") console.log("testNr 9 OK "); else console.log("testNr 9 NOT OK - ", (this.create_kmn_modifier("rshift", false)))
      if (this.create_kmn_modifier("anyOption", false) === "RALT") console.log("testNr 4 OK "); else console.log("testNr 4 NOT OK  - ", (this.create_kmn_modifier("anyOption", false)))
      if (this.create_kmn_modifier("anyShift", false) === "SHIFT") console.log("testNr 7 OK "); else console.log("testNr 7NOT OK - ", (this.create_kmn_modifier("anyShift", false)))
      if (this.create_kmn_modifier("rightShift? rightOption", false) === "RALT") console.log("testNr 17 OK "); else console.log("testNr 17 NOT OK  - ", (this.create_kmn_modifier("rightShift? rightOption", false)))
      if (this.create_kmn_modifier("option", false) === "RALT") console.log("testNr 15 OK "); else console.log("testNr 15 NOT OK  - ", (this.create_kmn_modifier("option", false)))
      if (this.create_kmn_modifier("rightOption", false) === "RALT") console.log("testNr 10 OK "); else console.log("testNr 10 NOT OK - ", (this.create_kmn_modifier("rightOption", false)))
      if (this.create_kmn_modifier("anyOption", true) === "NCAPS RALT") console.log("testNr 3 OK "); else console.log("testNr 3 NOT OK  - ", (this.create_kmn_modifier("anyOption", true)))
      if (this.create_kmn_modifier("shift? caps", true) === "CAPS") console.log("testNr 21 OK "); else console.log("testNr 21 NOT OK  - ", (this.create_kmn_modifier("shift? caps", true)))
      if (this.create_kmn_modifier("caps?", false) === "") console.log("testNr 11 OK "); else console.log("testNr 11 NOT OK - ", (this.create_kmn_modifier("caps?", false)))
      if (this.create_kmn_modifier("caps?", false) === "") console.log("testNr 22 OK "); else console.log("testNr 22 NOT OK - ", (this.create_kmn_modifier("caps?", false)))
      if (this.create_kmn_modifier("caps?", true) === "NCAPS") console.log("testNr 12 OK "); else console.log("testNr 12 NOT OK - ", (this.create_kmn_modifier("caps?", true)))
      if (this.create_kmn_modifier("shift? caps", true) === "CAPS") console.log("testNr 23 OK "); else console.log("testNr 23 NOT OK  - ", (this.create_kmn_modifier("shift? caps", true)))
      if (this.create_kmn_modifier("shift? caps", false) === "CAPS") console.log("testNr 24 OK "); else console.log("testNr 24 NOT OK  - ", (this.create_kmn_modifier("shift? caps", false)))
      if (this.create_kmn_modifier("shift caps?", true) === "SHIFT NCAPS") console.log("testNr 28 OK "); else console.log("testNr 28 NOT OK  - ", (this.create_kmn_modifier("shift caps?", true)))
      if (this.create_kmn_modifier("shift", true) === "NCAPS SHIFT") console.log("testNr 29 OK "); else console.log("testNr 29 NOT OK  - ", (this.create_kmn_modifier("shift", true)))
      if (this.create_kmn_modifier("shift", true) === "NCAPS SHIFT") console.log("testNr 30 OK "); else console.log("testNr 30 NOT OK  - ", (this.create_kmn_modifier("shift", true)))
      if (this.create_kmn_modifier("shift caps?", true) === "SHIFT NCAPS") console.log("testNr 27 OK "); else console.log("testNr 27 NOT OK  - ", (this.create_kmn_modifier("shift caps?", true)))
      if (this.create_kmn_modifier("rightShift anyOption", false) === "SHIFT RALT") console.log("testNr 18 OK "); else console.log("testNr 18 NOT OK  - ", (this.create_kmn_modifier("rightShift anyOption", false)))
      if (this.create_kmn_modifier("rightControl", true) === "NCAPS CTRL") console.log("testNr 14 OK "); else console.log("testNr 14 NOT OK - ", (this.create_kmn_modifier("rightControl", true)))
      if (this.create_kmn_modifier("Caps", true) === "CAPS") console.log("testNr 28 OK "); else console.log("testNr 28 NOT OK  - ", (this.create_kmn_modifier("Caps", true)))
      if (this.create_kmn_modifier("caps", true) === "CAPS") console.log("testNr 31 OK "); else console.log("testNr 31 NOT OK  - ", (this.create_kmn_modifier("Caps", true)))
      if (this.create_kmn_modifier("anyOption", true) === "NCAPS RALT") console.log("testNr 32 OK "); else console.log("testNr 32 NOT OK  - ", (this.create_kmn_modifier("anyOption", true)))
      if (this.create_kmn_modifier("anyOption", false) === "RALT") console.log("testNr 32 OK "); else console.log("testNr 32 NOT OK  - ", (this.create_kmn_modifier("anyOption", false)))
      if (this.create_kmn_modifier("Caps", true) === "CAPS") console.log("testNr 33 OK "); else console.log("testNr 33 NOT OK  - ", (this.create_kmn_modifier("Caps", true)))
      if (this.create_kmn_modifier("caps", false) === "CAPS") console.log("testNr 34 OK "); else console.log("testNr 34 NOT OK  - ", (this.create_kmn_modifier("Caps", false)))
      if (this.create_kmn_modifier("shift rightShift", false) === "SHIFT") console.log("testNr 25 OK "); else console.log("testNr 25 NOT OK  - ", (this.create_kmn_modifier("shift rightShift", false)))

      if (this.create_kmn_modifier("shift? caps?", true) === "NCAPS") console.log("testNr 18 OK "); else console.log("testNr 18 NOT OK  - ", (this.create_kmn_modifier("shift? caps?", true)))
      if (this.create_kmn_modifier("shift? caps?", false) === "") console.log("testNr 18a OK "); else console.log("testNr 18a NOT OK  - ", (this.create_kmn_modifier("shift? caps?", false)))

      if (this.create_kmn_modifier("", true) === "NCAPS") console.log("testNr 40 OK "); else console.log("testNr 40 NOT OK  - ", (this.create_kmn_modifier("", true)))
      if (this.create_kmn_modifier("shift? caps? ", true) === "NCAPS") console.log("testNr 41 OK "); else console.log("testNr 41 NOT OK  - ", (this.create_kmn_modifier("shift? caps? ", true)))
      if (this.create_kmn_modifier("anyShift caps?", true) === "SHIFT NCAPS") console.log("testNr 42 OK "); else console.log("testNr 42 NOT OK  - ", (this.create_kmn_modifier("anyShift caps?", true)))
      if (this.create_kmn_modifier("shift? rightShift caps?", true) === "SHIFT NCAPS") console.log("testNr 44 OK "); else console.log("testNr 44 NOT OK  - ", (this.create_kmn_modifier("shift? rightShift caps?", true)))
      if (this.create_kmn_modifier("shift? leftShift caps?", true) === "SHIFT NCAPS") console.log("testNr 45 OK "); else console.log("testNr 45 NOT OK  - ", (this.create_kmn_modifier("shift? leftShift caps?", true)))
      if (this.create_kmn_modifier("shift leftShift caps ", true) === "SHIFT CAPS") console.log("testNr 46 OK "); else console.log("testNr 46 NOT OK  - ", (this.create_kmn_modifier("shift leftShift caps ", true)))
      if (this.create_kmn_modifier("caps", true) === "CAPS") console.log("testNr 47 OK "); else console.log("testNr 47 NOT OK  - ", (this.create_kmn_modifier("caps", true)))
      if (this.create_kmn_modifier("anyOption", true) === "NCAPS RALT") console.log("testNr 48 OK "); else console.log("testNr 48 NOT OK  - ", (this.create_kmn_modifier("anyOption", true)))
      if (this.create_kmn_modifier("Caps", true) === "CAPS") console.log("testNr 49 OK "); else console.log("testNr 49 NOT OK  - ", (this.create_kmn_modifier("Caps", true)))
      if (this.create_kmn_modifier("anyShift caps? anyOption ?", true) === "SHIFT NCAPS RALT") console.log("testNr 50 OK "); else console.log("testNr 50 NOT OK  - ", (this.create_kmn_modifier("anyShift caps? anyOption ?", true)))
      if (this.create_kmn_modifier("caps anyOption ?", true) === "CAPS RALT") console.log("testNr 51 OK "); else console.log("testNr 51 NOT OK  - ", (this.create_kmn_modifier("caps anyOption ?", true)))
      if (this.create_kmn_modifier("anyShift? caps? anyOption? anyControl", true) === "NCAPS CTRL") console.log("testNr 52 OK "); else console.log("testNr 52 NOT OK  - ", (this.create_kmn_modifier("anyShift? caps? anyOption? anyControl", true)))
      if (this.create_kmn_modifier("anyShift? caps? anyOption anyControl", true) === "NCAPS RALT CTRL") console.log("testNr 53 OK "); else console.log("testNr 53 NOT OK  - ", (this.create_kmn_modifier("anyShift? caps? anyOption anyControl", true)))
      if (this.create_kmn_modifier("command", false) === "command") console.log("testNr 16 OK "); else console.log("testNr 16 NOT OK  - ", (this.create_kmn_modifier("command", false)))
      if (this.create_kmn_modifier("XXX", false) === "command") console.log("testNr 16 OK "); else console.log("testNr 16 NOT OK  - ", (this.create_kmn_modifier("XXX", false)))


      if (this.create_kmn_modifier("", false) === "") console.log("testNr 40 OK "); else console.log("testNr 40 NOT OK  - ", (this.create_kmn_modifier("", false)))
      if (this.create_kmn_modifier("shift? caps? ", false) === "") console.log("testNr 41 OK "); else console.log("testNr 41 NOT OK  - ", (this.create_kmn_modifier("shift? caps? ", false)))
      if (this.create_kmn_modifier("anyShift caps?", false) === "SHIFT") console.log("testNr 42 OK "); else console.log("testNr 42 NOT OK  - ", (this.create_kmn_modifier("anyShift caps?", false)))
      if (this.create_kmn_modifier("shift? rightShift caps?", false) === "SHIFT") console.log("testNr 44 OK "); else console.log("testNr 44 NOT OK  - ", (this.create_kmn_modifier("shift? rightShift caps?", false)))
      if (this.create_kmn_modifier("shift? leftShift caps?", false) === "SHIFT") console.log("testNr 45 OK "); else console.log("testNr 45 NOT OK  - ", (this.create_kmn_modifier("shift? leftShift caps?", false)))
      if (this.create_kmn_modifier("shift leftShift caps ", false) === "SHIFT CAPS") console.log("testNr 46 OK "); else console.log("testNr 46 NOT OK  - ", (this.create_kmn_modifier("shift leftShift caps ", false)))
      if (this.create_kmn_modifier("caps", false) === "CAPS") console.log("testNr 47 OK "); else console.log("testNr 47 NOT OK  - ", (this.create_kmn_modifier("caps", false)))
      if (this.create_kmn_modifier("anyOption", false) === "RALT") console.log("testNr 48 OK "); else console.log("testNr 48 NOT OK  - ", (this.create_kmn_modifier("anyOption", false)))
      if (this.create_kmn_modifier("Caps", false) === "CAPS") console.log("testNr 49 OK "); else console.log("testNr 49 NOT OK  - ", (this.create_kmn_modifier("Caps", false)))
      if (this.create_kmn_modifier("anyShift caps? anyOption ?", false) === "SHIFT RALT") console.log("testNr 50 OK "); else console.log("testNr 50 NOT OK  - ", (this.create_kmn_modifier("anyShift caps? anyOption ?", false)))
      if (this.create_kmn_modifier("caps anyOption ?", false) === "CAPS RALT") console.log("testNr 51 OK "); else console.log("testNr 51 NOT OK  - ", (this.create_kmn_modifier("caps anyOption ?", false)))
      if (this.create_kmn_modifier("anyShift? caps? anyOption? anyControl", false) === "CTRL") console.log("testNr 52 OK "); else console.log("testNr 52 NOT OK  - ", (this.create_kmn_modifier("anyShift? caps? anyOption? anyControl", false)))
      if (this.create_kmn_modifier("anyShift? caps? anyOption anyControl", false) === "RALT CTRL") console.log("testNr 53 OK "); else console.log("testNr 53 NOT OK  - ", (this.create_kmn_modifier("anyShift? caps? anyOption anyControl", false)))
      if (this.create_kmn_modifier("CAPS", false) === "CAPS") console.log("testNr 531 OK "); else console.log("testNr 531 NOT OK  - ", (this.create_kmn_modifier("CAPS", true)))
      if (this.create_kmn_modifier("shift leftShift caps", false) === "SHIFT CAPS") console.log("testNr 532 OK "); else console.log("testNr 532 NOT OK  - ", (this.create_kmn_modifier("shift leftShift caps", false)))
      if (this.create_kmn_modifier("shift leftShift caps", true) === "SHIFT CAPS") console.log("testNr 532 OK "); else console.log("testNr 532 NOT OK  - ", (this.create_kmn_modifier("shift leftShift caps", true)))

      console.log("------------------ ")

      if (this.isAcceptableKeymanModifier("SHIFT CAPS") === true) console.log("testNr A 1 OK "); else console.log("testNr A 1 NOT OK  - ", (this.isAcceptableKeymanModifier("SHIFT CAPS")))
      if (this.isAcceptableKeymanModifier("SHIFT") === true) console.log("testNr A 2 OK "); else console.log("testNr A 2 NOT OK  - ", (this.isAcceptableKeymanModifier("SHIFT")))
      if (this.isAcceptableKeymanModifier("shift") === true) console.log("testNr A 12 OK "); else console.log("testNr A 12 NOT OK  - ", (this.isAcceptableKeymanModifier("shift")))
      if (this.isAcceptableKeymanModifier("SHIFT ") === true) console.log("testNr A 3 OK "); else console.log("testNr A 3 NOT OK  - ", (this.isAcceptableKeymanModifier("SHIFT ")))
      if (this.isAcceptableKeymanModifier(" SHIFT ") === true) console.log("testNr A 10 OK "); else console.log("testNr A 10 NOT OK  - ", (this.isAcceptableKeymanModifier(" SHIFT ")))
      if (this.isAcceptableKeymanModifier(" SHIFT") === true) console.log("testNr A 11 OK "); else console.log("testNr A 11 NOT OK  - ", (this.isAcceptableKeymanModifier(" SHIFT")))
      if (this.isAcceptableKeymanModifier("rightshift") === false) console.log("testNr A 4 OK "); else console.log("testNr A 4 NOT OK  - ", (this.isAcceptableKeymanModifier("rightshift")))
      if (this.isAcceptableKeymanModifier("rightshift ") === false) console.log("testNr A 5 OK "); else console.log("testNr A 5 NOT OK  - ", (this.isAcceptableKeymanModifier("rightshift ")))
      if (this.isAcceptableKeymanModifier(" ") === true) console.log("testNr A 6 OK "); else console.log("testNr A 6 NOT OK  - ", (this.isAcceptableKeymanModifier(" ")))
      if (this.isAcceptableKeymanModifier("") === true) console.log("testNr A 7 OK "); else console.log("testNr A 7 NOT OK  - ", (this.isAcceptableKeymanModifier("")))
      if (this.isAcceptableKeymanModifier("rightoption") === false) console.log("testNr A 8 OK "); else console.log("testNr A 8 NOT OK  - ", (this.isAcceptableKeymanModifier("rightoption")))
      if (this.isAcceptableKeymanModifier("abc") === false) console.log("testNr A 9 OK "); else console.log("testNr A 9 NOT OK  - ", (this.isAcceptableKeymanModifier("abc")))
      if (this.isAcceptableKeymanModifier("ralt") === true) console.log("testNr A 13 OK "); else console.log("testNr A 13 NOT OK  - ", (this.isAcceptableKeymanModifier("ralt")))
    */

    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------
    // Test files end
    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------

    data_ukelele.ArrayOf_Rules = ObjectArray
    return data_ukelele
  }

  public get_KecCode_arr__From__ActionId(data: any, search: string): string[] {
    const returnarray: string[] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          returnarray.push(this.map_UkeleleKC_To_VK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])))
        }
      }
    }
    return returnarray
  }
  public get_KeyMap_Code_array__From__KeyMap_Action(data: any, search: string[]): string[] {
    const returnarray: string[] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
        }
      }
    }
    return returnarray
  }
  public get_KeyMap_Code_array__From__KeyMap_Action_array2D(data: any, search: string[][]): string[][] {
    const returnarray2D: string[][] = []
    for (let k = 0; k < search.length; k++) {
      for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
        for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
          const returnarray: string[] = []
          if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search[k][0]) {
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
            returnarray.push(this.map_UkeleleKC_To_VK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])))
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'])
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i]['@_index'])
            returnarray.push(search[k][2])
          }
          if (returnarray.length > 0)
            returnarray2D.push(returnarray)
        }
      }
    }
    return returnarray2D
  }
  public get_ActionID_Index__From__ActionID_Id(data: any, search: string): number {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      if (data.keyboard.actions.action[i]['@_id'] === search)
        return i
    }
    return 0
  }
  public get_ActionID_Next__From__ActionID_None(data: any, search: string): string {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      if (data.keyboard.actions.action[i]['@_id'] === search) {
        for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
          if (data.keyboard.actions.action[i].when[j]['@_state'] === "none")
            return data.keyboard.actions.action[i].when[j]['@_next']
        }
      }
    }
    return ""
  }
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
        if (returnarray.length > 0)
          returnarray2D.push(returnarray)
      }
    }
    return returnarray2D
  }
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
        if (returnarray1D.length > 0)
          returnarray.push(returnarray1D)
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
  public get_Datat_array2D__From__ActionID_stateOutput(data: any, modi: any, search: string, outchar: string, isCapsused: boolean): string[][] {
    const returnarray2D: string[][] = []

    // loop behaviors ( in ukelele it is possible to define multiple modifier combinations that behave in the same)
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j <= KeylayoutToKmnConverter.used_Keys_count; j++) {
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

            if (returnarray.length > 0)
              returnarray2D.push(returnarray)
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
  public get_Terminator_Output__From__Terminator_State(data: any, search: string): string {
    for (let i = 0; i < data.keyboard.terminators.when.length; i++) {
      if (data.keyboard.terminators.when[i]['@_state'] === search) {
        return data.keyboard.terminators.when[i]['@_output']
      }
    }
    return ""
  }
  public get_KeyMap_Code_array__From__ActionID_Action(data: any, search: string): number[][] {
    const mapIndexArray_max: number[][] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        const mapIndexArrayperKey: number[] = []

        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          mapIndexArrayperKey.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
          mapIndexArrayperKey.push(i)
        }
        if (mapIndexArrayperKey.length > 0)
          mapIndexArray_max.push(mapIndexArrayperKey)
      }
    }
    return mapIndexArray_max
  }
  public get_KeyMap_Modifier_array__From__behaviour_arr(data: any, search: number[][]): string[] {
    const mapIndexArray_max: string[] = []
    for (let i = 0; i < search.length; i++) {
      mapIndexArray_max.push(data[search[i][1]])
    }
    return mapIndexArray_max
  }

  /*  public get_KeyMap_Code__From__KeyMap_Action(data: any, search: string): number {
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          return data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']
        }
      }
    }
    return -1
  }*/
  /*public get_KeyMap_Behaviour_array__From__KeyMap_Action(data: any, search: string[]): string[][] {
    const returnarray2D: string[][] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        const returnarray: string[] = []
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          returnarray.push(this.map_UkeleleKC_To_VK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])))
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i]['@_index'])
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'])
        }
        if (returnarray.length > 0)
          returnarray2D.push(returnarray)
      }
    }
    return returnarray2D
  }*/
  /*public get_KeyMap_Code_array__From__KeyMap_Action_array(data: any, search: string[][]): string[] {
    const returnarray: string[] = []
    for (let i = 0; i < search.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap.length; j++) {
        for (let k = 0; k < data.keyboard.keyMapSet[0].keyMap[j].key.length; k++) {
          if (data.keyboard.keyMapSet[0].keyMap[j].key[k]['@_action'] === search[i][0]) {
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[j].key[k]['@_code'])
          }
        }
      }
    }
    return returnarray
  }*/
  /*public get_ActionID_array__From__ActionID_NoneNext(data: any, search: string): string[] {
    const returnarray: string[] = []
    if (search !== "none") {
      for (let i = 0; i < data.keyboard.actions.action.length; i++) {
        for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
          if (data.keyboard.actions.action[i].when[j]['@_next'] === search) {
            if (data.keyboard.actions.action[i].when[j]['@_state'] === "none")
              returnarray.push(data.keyboard.actions.action[i]['@_id'])
          }
        }
      }
    }
    return returnarray
  }*/
  /* public get_keyModifierArray__from__Action(data: any, search: string): string[][] {
     // search is a9
     const returnarray2D: string[][] = []
     for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
       for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
         const returnarray: string[] = []
         if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
           returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'])
           returnarray.push(this.map_UkeleleKC_To_VK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])))
           returnarray.push(data.keyboard.keyMapSet[0].keyMap[i]['@_index'])
         }
         if (returnarray.length > 0)
           returnarray2D.push(returnarray)
       }
     }
     return returnarray2D
   }*/

  // ToDo needed??
  /*public get_ActionID_Output__From__ActionID_IdState(data: any, search_id: string, search_state: string): Uint8Array {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
        if ((data.keyboard.actions.action[i]['@_id'] === search_id) &&
          (data.keyboard.actions.action[i].when[j]['@_state'] === search_state)) {
          return new TextEncoder().encode(data.keyboard.actions.action[i].when[j]['@_output'])
        }
      }
    }
    return new TextEncoder().encode("")
  }*/
  // ToDo needed??
  /*public get_ActionID_Key__From__ActionID_IdState(data: any, search_id: string, search_state: string): Uint8Array {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
        if ((data.keyboard.actions.action[i]['@_id'] === search_id) &&
          (data.keyboard.actions.action[i].when[j]['@_state'] === search_state)) {
          return new TextEncoder().encode(data.keyboard.actions.action[i].when[j]['@_output'])
        }
      }
    }
    return new TextEncoder().encode("")
  }*/
  // ToDo needed??
  /*public get_KeyMap_Keymaparray__From__KeyMap_Code_array(data: any, search: string[]): string[][] {
    const returnarray: string[][] = []
    for (let k = 0; k < search.length; k++) {
      const returnarray1D: string[] = []
      for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
        for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
          if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'] === search[k]) {
            returnarray1D.push(search[k])
            returnarray1D.push(data.keyboard.keyMapSet[0].keyMap[i]['@_index'])
          }
        }
      }
      returnarray.push(returnarray1D)
    }
    return returnarray
  }*/
  /*public get_KeyMap_Keymaparray__From__KeyMap_Action_array(data: any, search: string): string[][] {
    const returnarray: string[][] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      const returnarray1D: string[] = []
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          returnarray1D.push(search)
          returnarray1D.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
          returnarray1D.push(data.keyboard.keyMapSet[0].keyMap[i]['@_index'])
        }
        if (returnarray1D.length > 0)
          returnarray.push(returnarray1D)
      }
    }
    //ToDo check if I can use this filer somewhere else
    const [uniqueElements] = returnarray.reduce((acc, curr) => {
      const [uniq, set] = acc;
      if (!set.has(curr.join(','))) {
        set.add(curr.join(','));
        uniq.push(curr);
      }
      return acc;
    },
      [[], new Set()],
    );
    return uniqueElements
  }*/

  // ToDo needed??
  /*public get_KeyMap_Keymaparray__From__KeyMap_Action(data: any, search: string): string[] {
    const returnarray: string[] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i]['@_index'])
        }
      }
    }
    return returnarray
  }*/
  // ToDo needed??
  /*public get_ActionID_Id_array__From__ActionID_next(data: any, search: string): string[][] {
    const returnarray2D: string[][] = []
    if (search !== "none") {
      for (let i = 0; i < data.keyboard.actions.action.length; i++) {
        const returnarray: string[] = []
        for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
          if (data.keyboard.actions.action[i].when[j]['@_next'] === search) {
            returnarray.push(data.keyboard.actions.action[i]['@_id'])
          }
        }
        if (returnarray.length > 0)
          returnarray2D.push(returnarray)
      }
    }
    return returnarray2D
  }*/
  // ToDo needed?? get all entries for state= and a given action id
  /*public get_Action2ID_State_arrayState_Array__From__ActionID_Id(data: any, search: string): string[] {
    const returnarray: string[] = []
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      if (data.keyboard.actions.action[i]['@_id'] === search) {
        for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
          if (data.keyboard.actions.action[i].when[j]['@_state'] !== "none") {
            returnarray.push(data.keyboard.actions.action[i].when[j]['@_state'])
          }
        }
      }
    }
    return returnarray
  }*/
  // ToDo needed?? get all entries for state= and a given action id
  /* public get_ActionID_Id_arrayNext_Array__From__ActionID_StateNext(data: any, search: string): string[] {
     const returnarray: string[] = []
     for (let i = 0; i < data.keyboard.actions.action.length; i++) {
       for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
         if ((data.keyboard.actions.action[i].when[j]['@_next'] === search) && (data.keyboard.actions.action[i].when[j]['@_state'] !== "none")) {
           returnarray.push(data.keyboard.actions.action[i]['@_id'])
         }
       }
     }
     return returnarray
   }*/
  /*public map_UkeleleKC_To_VK_array(search: string[]): string[] {
    const returnarray: string[] = []
    for (let i = 0; i < search.length; i++) {
      returnarray.push(this.map_UkeleleKC_To_VK(Number(search[i])))
    }
    return returnarray
  }*/
  /*public get_ActionID_Id__From__ActionID_Next(data: any, search: string) {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
        if ((data.keyboard.actions.action[i].when[j]['@_next'] === search)) {
          return data.keyboard.actions.action[i]['@_id']
        }
      }
    }
    return ""
  }*/
  /*public get_Modifier_Text__From__Modifier_Index(data: any, search: string): string[] {
    const mapIndexArray_max: string[] = []
    const mapIndexArrayperKey: string[] = []
    for (let i = 0; i < data[search].length; i++) {
      mapIndexArrayperKey.push((data[search][i]))
    }
    for (let i = 0; i < data[search].length; i++) {
      mapIndexArray_max.push(data[search][i])
    }
    return mapIndexArray_max
  }*/
  /*public get_KeyMap_Code__From__ActionID_Action(data: any, search: string): number {
    const mapIndexArray_max: number[][] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        const mapIndexArrayperKey: number[] = []

        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          mapIndexArrayperKey.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
          mapIndexArrayperKey.push(i)
        }
        if (mapIndexArrayperKey.length > 0)
          mapIndexArray_max.push(mapIndexArrayperKey)
      }
    }
    return mapIndexArray_max[0][1]
  }*/
  /*public get_KeyMap_Code__From__ActionID_Action2(data: any, search: string): number {
    const mapIndexArray_max: number[][] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        const mapIndexArrayperKey: number[] = []

        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          mapIndexArrayperKey.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
          mapIndexArrayperKey.push(i)
        }
        if (mapIndexArrayperKey.length > 0)
          mapIndexArray_max.push(mapIndexArrayperKey)
      }
    }
    return mapIndexArray_max[0][1]
  }*/
  /*public get_ActionID_Next__From__ActionID_Id(data: any, search: string): string {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      if ((data.keyboard.actions.action[i]['@_id'] === search))
        return data.keyboard.actions.action[i].when[1]['@_next']
    }
    return ""
  }*/
  /*public get_KeyMap_Index__From__KeyMap_Action(data: any, search: string): number {
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          return data.keyboard.keyMapSet[0].keyMap[i]['@_index']
        }
      }
    }
    return -1
  }*/
  /*public get_KeyMap_IndexCodeAction_array__From__KeyMap_Action(data: any, search: string): string[][] {
    const returnarray2D: string[][] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        const returnarray: string[] = []
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i]['@_index'])
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'])
        }
        if (returnarray.length > 0) {
          returnarray2D.push(returnarray)
        }
      }
    }
    return returnarray2D
  }*/
  /*public get_KeyMap_IndexCodeAction_array__From__KeyMap_Action_Array(data: any, search: string[][]): string[][] {
    console.log("search uuuu ", search)
    const modifierArray: number[] = []
    const modifierArray2D: number[][] = []

    const returnarray2D: string[][] = []
    for (let i = 0; i < search.length; i++) {
      modifierArray.push(Number(search[i][0]))
    }
    modifierArray2D.push(modifierArray)

    returnarray2D.push(this.get_KeyMap_Modifier_array__From__behaviour_arr(data, modifierArray2D))
    return returnarray2D
  }*/
  /*public get_KeyMap_ModiIndex_array__From__KeyMap_Action(data: any, search_code: string, search_action: string): string[][] {
    const returnarray2D: string[][] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        const returnarray: string[] = []
        if ((data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search_action) &&
          (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'] === search_code)) {
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i]['@_index'])
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'])
        }
        if (returnarray.length > 0) {
          returnarray2D.push(returnarray)
        }
      }
    }
    return returnarray2D
  }*/
  /*public get_ActionID_Output__From__ActionID_State(data: any, search: string, nextval: string): string {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
        if ((data.keyboard.actions.action[i].when[j]['@_state'] === search) && (data.keyboard.actions.action[i]['@_id'] === nextval)) {
          return data.keyboard.actions.action[i].when[j]['@_output']
        }
      }
    }
    return ""
  }*/
  /* public get_KeyMap_Modifier_aray__From__KeyMap_Index(data: any, search: number): string[] {
     return data.ArrayOf_Modifiers[search]
   }*/


  /*public get_data_From__KeyMap_Modifier_Index(data: any, search: number): string[] {
    return data[search]
  }*/

  /*public get_dataArray2D__From__KeyMap_Modifier_Index(data: any, search: string[]): string[][] {
    const returnarray: string[][] = []
    for (let i = 0; i < search.length; i++) {
      returnarray.push(data[search[i]])
    }
    return returnarray
  }*/
  /**
   * @brief  member function to return the unicode value of a character
   * @param  character the value that will converted
   * @return headecimal value of a character
   */

  /* public getHexFromChar(character: string): string {
    return character.charCodeAt(0).toString(16).slice(-4).toUpperCase().padStart(4, "0")
  }*/
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

      if (isCAPSused && (String(keylayout_modifier).toUpperCase().indexOf("CAPS") === -1))
        kmn_ncaps = " NCAPS "

      // if we find a modifier containing a '?' e.g. SHIFT?: => SHIFT is not neccessary. If it is not neccessary we don`t write this modifier
      if ((String(modifier_state[i]).toUpperCase().includes('?') && (!String(modifier_state[i]).toUpperCase().includes('CAPS?')))) add_modifier = "";

      // TODO is this correct: caps? => caps is not neccessary. If its not neccessary and isCAPSused we need to write out NCAPS. Correct?
      else if ((isCAPSused) && (String(modifier_state[i]).toUpperCase().includes('CAPS?'))) add_modifier = "NCAPS ";
      else if ((!isCAPSused) && (String(modifier_state[i]).toUpperCase().includes('CAPS?'))) add_modifier = "";
      else if ((String(modifier_state[i]).toUpperCase().includes('CAPS'))) add_modifier = "CAPS ";
      else if ((isCAPSused) && (String(modifier_state[i]).toUpperCase().includes('NCAPS'))) add_modifier = "NCAPS ";

      // we do not use the right or left version of a modifier ( e.g. rightshift, leftshift). If they are used in keylayout files they will not be changed but used as is. 
      // Later when we write out the rules to the kmn file instead of writing those rules we print out a warning 
      else if ((String(modifier_state[i]).toUpperCase() === 'ANYOPTION') || (String(modifier_state[i]).toUpperCase() === 'OPTION')) add_modifier = "RALT ";
      else if ((String(modifier_state[i]).toUpperCase() === 'RIGHTOPTION')) add_modifier = "RALT ";
      else if ((String(modifier_state[i]).toUpperCase() === 'ANYSHIFT') || (String(modifier_state[i]).toUpperCase() === 'SHIFT')) add_modifier = "SHIFT ";
      else if ((String(modifier_state[i]).toUpperCase() === 'ANYCONTROL') || (String(modifier_state[i]).toUpperCase() === 'CONTROL')) add_modifier = "RCTRL ";

      else add_modifier = String(modifier_state[i]) + " "
      kmn_modifier += kmn_ncaps + add_modifier
    }

    // remove duplicate and empty entries
    const duplicate_modifier_array: string[] = kmn_modifier.split(" ").filter(item => item)
    const unique_Modifier: string[] = duplicate_modifier_array.filter(function (item, pos, self) {
      return self.indexOf(item) === pos;   // ToDo == or === ????
    })

    //ToDo review lower part
    const unique_Modifier_string: string = unique_Modifier.join(" ").replace(/\s+/g, " ").trim()

    const modifier_array: string[] = unique_Modifier_string.split(" ");

    for (let i = 0; i < modifier_array.length; i++) {
      if ((modifier_array[i].toUpperCase() === "RIGHTSHIFT") || (modifier_array[i].toUpperCase() === "RSHIFT")) modifier_array[i] = "SHIFT"
      else if ((modifier_array[i].toUpperCase() === "LEFTSHIFT") || (modifier_array[i].toUpperCase() === "LSHIFT")) modifier_array[i] = "SHIFT"
      else if ((modifier_array[i].toUpperCase() === "LEFTCONTROL") || (modifier_array[i].toUpperCase() === "LCONTROL")) modifier_array[i] = "CTRL"
      else if ((modifier_array[i].toUpperCase() === "RIGHTCONTROL") || (modifier_array[i].toUpperCase() === "LCONTROL")) modifier_array[i] = "CTRL"
      else if ((modifier_array[i].toUpperCase() === "LEFTOPTION") || (modifier_array[i].toUpperCase() === "LOPTION")) modifier_array[i] = "RALT"
      else if ((modifier_array[i].toUpperCase() === "RIGHTOPTION") || (modifier_array[i].toUpperCase() === "ROPTION")) modifier_array[i] = "RALT"
    }

    const unique_modifier_array: string[] = modifier_array.filter(function (item, pos, self) {
      return self.indexOf(item) == pos;
    })

    return unique_modifier_array.flat().toString().replace(/,/g, " ")
  }

  public checkIfCapsIsUsed(keylayout_modifier: string[][]): boolean {
    return keylayout_modifier.flat().flat().includes("caps")
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

      ) { iskKeymanModifier = iskKeymanModifier && true }
      else { iskKeymanModifier = iskKeymanModifier && false }
    }
    return iskKeymanModifier
  }
  public findDuplicateRules(rules: rule_object[], index: number, rule_type: string): string {

    for (let i = 0; i < index - 1; i++) {
      if (
        ((rule_type === "C01")
          && rules[i].modifier_key === rules[index].modifier_key
          && rules[i].key === rules[index].key &&
          new TextDecoder().decode(rules[i].output) === new TextDecoder().decode(rules[index].output)
        )
        || (rule_type === "C2"
          && rules[i].modifier_deadkey === rules[index].modifier_deadkey
          && rules[i].deadkey === rules[index].deadkey
          && rules[i].modifier_key === rules[index].modifier_key
          && rules[i].key === rules[index].key
          && new TextDecoder().decode(rules[i].output) === new TextDecoder().decode(rules[index].output)
        )
        || (rule_type === "C3"
          && rules[i].modifier_prev_deadkey === rules[index].modifier_prev_deadkey
          && rules[i].prev_deadkey === rules[index].prev_deadkey
          && rules[i].modifier_deadkey === rules[index].modifier_deadkey
          && rules[i].deadkey === rules[index].deadkey
          && rules[i].modifier_key === rules[index].modifier_key
          && rules[i].key === rules[index].key
          && new TextDecoder().decode(rules[i].output) === new TextDecoder().decode(rules[index].output)
        )
      )
        return ("duplicate Rule: old: modif[ modi; " + rules[i].dk_prev + "[" +
          rules[i].modifier_prev_deadkey + " " + rules[i].prev_deadkey + "]  >  XXX [" +
          rules[i].modifier_deadkey + " " + rules[i].deadkey + "]  >  YYY  [" +
          rules[i].modifier_key + " " + rules[i].key +
          "]  °°->  \'" + new TextDecoder().decode(rules[i].output) + "\'  <--> ")
    }
    return ""
  }

  // C0C1  |   |   | 1 |                                                         CAPS K_A > 'A'
  // C2    |   | 2 | 3 |                         CAPS K_A > dk(A1)               dk(A1) + SHIFT K_B > 'B'
  // C3    | 4 | 5 | 6 |   CAPS K_X > dk(B1)     dk(B1) + SHIFT K_Y > dk(C1)     dk(C1) + NCAPS K_Z > 'Z'

  public findAmbiguousRules(rule: rule_object[], index: number): string {

    // ToDo is the return statement/filter func correct???

    // 1-1
    // find ambiguous C0 or C1 rules vs. C0 or C1  .......................................................................................................
    // e.g. CAPS K_A > 'A'   <->  CAPS K_A > 'B'  ........................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C0C1_vs_C0C1: rule_object[] = rule.filter((curr, idx) => {
      if (((rule[index].rule_type === "C0") || (rule[index].rule_type === "C1"))
        && ((curr.rule_type === "C0") || (curr.rule_type === "C1"))
        && curr.modifier_prev_deadkey === ""
        && curr.prev_deadkey === ""
        && curr.modifier_deadkey === ""
        && curr.deadkey === ""
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output)
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C0C1_vs_C0C1.length > 0)
      return ("ambiguous 1-1 rule: earlier: ["
        + ambiguous_C0C1_vs_C0C1[0].modifier_key + " "
        + ambiguous_C0C1_vs_C0C1[0].key + "]  >  \'"
        + new TextDecoder().decode(ambiguous_C0C1_vs_C0C1[0].output)
        + "\' here: [")

    // 2-2
    // find ambiguous C2 rules vs. C2 ....................................................................................................................
    // e.g. CAPS K_A > dk(A1)   <->  CAPS K_A > dk(A2) ...................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C2_vs_C2: rule_object[] = rule.filter((curr, idx) => {
      if (((rule[index].rule_type === "C2"))
        && ((curr.rule_type === "C2"))
        && curr.modifier_prev_deadkey === ""
        && curr.prev_deadkey === ""
        && curr.modifier_deadkey === rule[index].modifier_deadkey
        && curr.deadkey === rule[index].deadkey
        && curr.dk_C2 !== rule[index].dk_C2
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C2_vs_C2.length > 0)
      return ("ambiguous 2-2 rule: earlier: ["
        + ambiguous_C2_vs_C2[0].modifier_deadkey + " "
        + ambiguous_C2_vs_C2[0].deadkey + "]  >  dk( C'"
        + ambiguous_C2_vs_C2[0].dk_C2
        + ") here: [")

    // 3-3
    // find ambiguous C0 or C1 rules vs. C0 or C1  .......................................................................................................
    // e.g. CAPS K_A > 'A'   <->  CAPS K_A > 'B'  ........................................................................................................
    // ...................................................................................................................................................
    /*  const ambiguous_C2_vs_C2_A: rule_object[] = rule.filter((curr, idx) => {
        if (((rule[index].rule_type === "C2"))
          && ((curr.rule_type === "C2"))
          && curr.modifier_key === rule[index].modifier_key
          && curr.key === rule[index].key
          && curr.output !== rule[index].output
          && curr.uniqueB !== 0
          && (idx < index)
        )
          return curr;
        else return ""
      });
      console.log("ambiguous_C2_vs_C2_A " )
      this.writeDalaset(ambiguous_C2_vs_C2_A)

      if (ambiguous_C2_vs_C2_A.length > 0)
        return ("ambiguous 3-3 rule: earlier: ["
          + ambiguous_C2_vs_C2_A[0].modifier_key + " "
          + ambiguous_C2_vs_C2_A[0].key + "]  >  \'"
          + new TextDecoder().decode(ambiguous_C2_vs_C2_A[0].output)
          + ") here: [")

  */


    // 4-4
    // find ambiguous C3 rules vs. C3 ....................................................................................................................
    // e.g. CAPS K_A > dk(B1)   <->  CAPS K_A > dk(B2) ...................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C3_vs_C3: rule_object[] = rule.filter((curr, idx) => {
      if (((rule[index].rule_type === "C3"))
        && ((curr.rule_type === "C3"))
        && curr.modifier_prev_deadkey === rule[index].modifier_prev_deadkey
        && curr.prev_deadkey === rule[index].prev_deadkey
        && curr.dk_prev !== rule[index].dk_prev
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C2_vs_C2.length > 0)
      return ("ambiguous 4-4 rule: earlier: ["
        + ambiguous_C3_vs_C3[0].modifier_prev_deadkey + " "
        + ambiguous_C3_vs_C3[0].prev_deadkey + "]  >  dk( C'"
        + ambiguous_C3_vs_C3[0].dk_prev
        + ") here: [")


    //6-6
    // find ambiguous C3 rules vs. C3  ...................................................................................................................
    // e.g.  dk(C1) + NCAPS K_Z > 'Z''   <->   dk(C1) + NCAPS K_Z > 'X'  .................................................................................
    // ...................................................................................................................................................
    const ambiguous_C3_vs_C3_B: rule_object[] = rule.filter((curr, idx) => {
      if (((rule[index].rule_type === "C3"))
        && ((curr.rule_type === "C3"))
        && curr.modifier_prev_deadkey === rule[index].modifier_prev_deadkey
        && curr.prev_deadkey === rule[index].prev_deadkey
        && curr.dk_prev !== rule[index].dk_prev
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C3_vs_C3_B.length > 0)
      return ("ambiguous 6-6 rule: earlier: ["
        + ambiguous_C3_vs_C3_B[0].modifier_key + " "
        + ambiguous_C3_vs_C3_B[0].key + "]  >  \'"
        + new TextDecoder().decode(ambiguous_C3_vs_C3_B[0].output)
        + ") here: [")

    // 1-2
    // find ambiguous C0 or C1 rules vs. C2 ..............................................................................................................
    // e.g. CAPS K_A > 'A''   <->  CAPS K_A > dk(A2) .....................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C1C1_vs_C2: rule_object[] = rule.filter((curr) => {
      if ((rule[index].rule_type === "C2")
        && ((curr.rule_type === "C0") || (curr.rule_type === "C1"))
        && curr.modifier_prev_deadkey === ""
        && curr.prev_deadkey === ""
        && rule[index].modifier_deadkey === curr.modifier_key
        && rule[index].deadkey === curr.key
        && (rule[index].uniqueB !== 0)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C1C1_vs_C2.length > 0)
      return ("ambiguous 1-2 rule: earlier: ["
        + ambiguous_C1C1_vs_C2[0].modifier_key + " "
        + ambiguous_C1C1_vs_C2[0].key + "]  >  \'"
        + new TextDecoder().decode(ambiguous_C1C1_vs_C2[0].output)
        + "\' here: [")

    // 1-4
    // find ambiguous C0 or C1 rules vs. C3 ..............................................................................................................
    // e.g. CAPS K_A > 'A''   <->  CAPS K_A > dk(B2) .....................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C0C1_vs_C3: rule_object[] = rule.filter((curr, idx) => {
      if ((rule[index].rule_type === "C3")
        && ((curr.rule_type === "C0") || (curr.rule_type === "C1"))
        && rule[index].modifier_prev_deadkey === curr.modifier_key
        && rule[index].prev_deadkey === curr.key
        && (rule[index].uniqueA !== 0)
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C0C1_vs_C3.length > 0)
      return ("ambiguous 1-4 rule: earlier: ["
        + ambiguous_C0C1_vs_C3[0].modifier_key + " "
        + ambiguous_C0C1_vs_C3[0].key + "]  >  \'"
        + new TextDecoder().decode(ambiguous_C0C1_vs_C3[0].output)
        + "\' here: [")

    // 3-6
    // find ambiguous C2 rules vs. C3 ....................................................................................................................
    // e.g. CAPS K_A > 'A'   <->  CAPS K_A > 'B' .........................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C2_vs_C3: rule_object[] = rule.filter((curr, idx) => {
      if ((rule[index].rule_type === "C3")
        && ((curr.rule_type === "C2"))
        && rule[index].modifier_prev_deadkey === curr.modifier_deadkey
        && rule[index].prev_deadkey === curr.deadkey
        && curr.dk_prev !== rule[index].dk_C2
        && (rule[index].uniqueA !== 0)
        && (rule[index].uniqueB !== 0)
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C2_vs_C3.length > 0)
      return ("ambiguous 1-6 rule: earlier: ["
        + ambiguous_C2_vs_C3[0].modifier_key + " "
        + ambiguous_C2_vs_C3[0].key + "]  >  \'"
        + new TextDecoder().decode(ambiguous_C2_vs_C3[0].output)
        + "\' here: [")

    return ""
  }

  public findDuplicateRules_new(rule: rule_object[], index: number): string {

    // ToDo is the return statement/filter func correct???

    // 1-1
    // find ambiguous C0 or C1 rules vs. C0 or C1  .......................................................................................................
    // e.g. CAPS K_A > 'A'   <->  CAPS K_A > 'B'  ........................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C0C1_vs_C0C1: rule_object[] = rule.filter((curr, idx) => {
      if (((rule[index].rule_type === "C0") || (rule[index].rule_type === "C1"))
        && ((curr.rule_type === "C0") || (curr.rule_type === "C1"))
        && curr.modifier_prev_deadkey === ""
        && curr.prev_deadkey === ""
        && curr.modifier_deadkey === ""
        && curr.deadkey === ""
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output)
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C0C1_vs_C0C1.length > 0)
      return ("duplicate 1-1 rule: earlier: ["
        + ambiguous_C0C1_vs_C0C1[0].modifier_key + " "
        + ambiguous_C0C1_vs_C0C1[0].key + "]  >  \'"
        + new TextDecoder().decode(ambiguous_C0C1_vs_C0C1[0].output)
        + "\' here: [")

    // 2-2
    // find ambiguous C2 rules vs. C2 ....................................................................................................................
    // e.g. CAPS K_A > dk(A1)   <->  CAPS K_A > dk(A2) ...................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C2_vs_C2: rule_object[] = rule.filter((curr, idx) => {
      if (((rule[index].rule_type === "C2"))
        && ((curr.rule_type === "C2"))
        && curr.modifier_prev_deadkey === ""
        && curr.prev_deadkey === ""
        && curr.modifier_deadkey === rule[index].modifier_deadkey
        && curr.deadkey === rule[index].deadkey
        && curr.dk_C2 === rule[index].dk_C2
        && rule[index].uniqueB !== 0
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if ((ambiguous_C2_vs_C2.length > 0))
      return ("duplicate 2-2 ccrule: earlier: ["
        + ambiguous_C2_vs_C2[0].modifier_deadkey + " "
        + String(ambiguous_C2_vs_C2[0].uniqueB)
        + ambiguous_C2_vs_C2[0].deadkey + "]  >  dk(C"
        + ambiguous_C2_vs_C2[0].dk_C2
        + "xx) here: [")

    /*// 3-3
    // find ambiguous C0 or C1 rules vs. C0 or C1  .......................................................................................................
    // e.g. CAPS K_A > 'A'   <->  CAPS K_A > 'B'  ........................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C2_vs_C2_A: rule_object[] = rule.filter((curr, idx) => {
      if (((rule[index].rule_type === "C2"))
        && ((curr.rule_type === "C2"))
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && curr.output !== rule[index].output
        && curr.uniqueB !== 0
        && (idx < index)
      )
        return curr;
      else return ""
    });
    console.log("ambiguous_C2_vs_C2_A " )
    this.writeDalaset(ambiguous_C2_vs_C2_A)

    if (ambiguous_C2_vs_C2_A.length > 0)
      return ("ambiguous 3-3 rule: earlier: ["
        + ambiguous_C2_vs_C2_A[0].modifier_key + " "
        + ambiguous_C2_vs_C2_A[0].key + "]  >  \'"
        + new TextDecoder().decode(ambiguous_C2_vs_C2_A[0].output)
        + ") here: [")*/




    // 4-4
    // find ambiguous C3 rules vs. C3 ....................................................................................................................
    // e.g. CAPS K_A > dk(B1)   <->  CAPS K_A > dk(B2) ...................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C3_vs_C3: rule_object[] = rule.filter((curr, idx) => {
      if (((rule[index].rule_type === "C3"))
        && ((curr.rule_type === "C3"))
        && curr.modifier_prev_deadkey === rule[index].modifier_prev_deadkey
        && curr.prev_deadkey === rule[index].prev_deadkey
        && curr.dk_prev
        === rule[index].dk_prev
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C2_vs_C2.length > 0)
      return ("duplicate 4-4 rule: earlier: ["
        + ambiguous_C3_vs_C3[0].modifier_prev_deadkey + " "
        + ambiguous_C3_vs_C3[0].prev_deadkey + "]  >  dk( C'"
        + ambiguous_C3_vs_C3[0].dk_prev
        + ") here: [")


    //6-6
    // find ambiguous C3 rules vs. C3  ...................................................................................................................
    // e.g.  dk(C1) + NCAPS K_Z > 'Z''   <->   dk(C1) + NCAPS K_Z > 'X'  .................................................................................
    // ...................................................................................................................................................
    const ambiguous_C3_vs_C3_B: rule_object[] = rule.filter((curr, idx) => {
      if (((rule[index].rule_type === "C3"))
        && ((curr.rule_type === "C3"))
        && curr.modifier_prev_deadkey === rule[index].modifier_prev_deadkey
        && curr.prev_deadkey === rule[index].prev_deadkey
        && curr.dk_prev === rule[index].dk_prev
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C3_vs_C3_B.length > 0)
      return ("duplicate 6-6 rule: earlier: ["
        + ambiguous_C3_vs_C3_B[0].modifier_key + " "
        + ambiguous_C3_vs_C3_B[0].key + "]  >  \'"
        + new TextDecoder().decode(ambiguous_C3_vs_C3_B[0].output)
        + ") here: [")

    // 1-2
    // find ambiguous C0 or C1 rules vs. C2 ..............................................................................................................
    // e.g. CAPS K_A > 'A''   <->  CAPS K_A > dk(A2) .....................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C1C1_vs_C2: rule_object[] = rule.filter((curr) => {
      if ((rule[index].rule_type === "C2")
        && ((curr.rule_type === "C0") || (curr.rule_type === "C1"))
        && curr.modifier_prev_deadkey === ""
        && curr.prev_deadkey === ""
        && rule[index].modifier_deadkey === curr.modifier_key
        && rule[index].deadkey === curr.key
        && (rule[index].uniqueB === curr.uniqueB)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C1C1_vs_C2.length > 0)
      return ("duplicate 1-2 rule: earlier: ["
        + ambiguous_C1C1_vs_C2[0].modifier_key + " "
        + ambiguous_C1C1_vs_C2[0].key + "]  >  \'"
        + new TextDecoder().decode(ambiguous_C1C1_vs_C2[0].output)
        + "\' here: [")

    // 1-4
    // find ambiguous C0 or C1 rules vs. C3 ..............................................................................................................
    // e.g. CAPS K_A > 'A''   <->  CAPS K_A > dk(B2) .....................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C0C1_vs_C3: rule_object[] = rule.filter((curr, idx) => {
      if ((rule[index].rule_type === "C3")
        && ((curr.rule_type === "C0") || (curr.rule_type === "C1"))
        && rule[index].modifier_prev_deadkey === curr.modifier_key
        && rule[index].prev_deadkey === curr.key
        && (rule[index].uniqueA === curr.uniqueA)
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C0C1_vs_C3.length > 0)
      return ("duplicate 1-4 rule: earlier: ["
        + ambiguous_C0C1_vs_C3[0].modifier_key + " "
        + ambiguous_C0C1_vs_C3[0].key + "]  >  \'"
        + new TextDecoder().decode(ambiguous_C0C1_vs_C3[0].output)
        + "\' here: [")

    // 3-6
    // find ambiguous C2 rules vs. C3 ....................................................................................................................
    // e.g. CAPS K_A > 'A'   <->  CAPS K_A > 'B' .........................................................................................................
    // ...................................................................................................................................................
    const ambiguous_C2_vs_C3: rule_object[] = rule.filter((curr, idx) => {
      if ((rule[index].rule_type === "C3")
        && ((curr.rule_type === "C2"))
        && rule[index].modifier_prev_deadkey === curr.modifier_deadkey
        && rule[index].prev_deadkey === curr.deadkey
        && curr.dk_prev !== rule[index].dk_C2
        && (rule[index].uniqueA === curr.uniqueA)
        && (rule[index].uniqueB === curr.uniqueB)
        && (idx < index)
      )
        return curr;
      else return ""
    });
    if (ambiguous_C2_vs_C3.length > 0)
      return ("duplicate 1-6 rule: earlier: ["
        + ambiguous_C2_vs_C3[0].modifier_key + " "
        + ambiguous_C2_vs_C3[0].key + "]  >  \'"
        + new TextDecoder().decode(ambiguous_C2_vs_C3[0].output)
        + "\' here: [")

    return ""
  }


  public findUnAvailableRule(unique_Rules: rule_object[], index: number): string {
    if (
      (((unique_Rules[index].rule_type === "C0") || (unique_Rules[index].rule_type === "C1") || (unique_Rules[index].rule_type === "C4"))
        && (this.isAcceptableKeymanModifier(unique_Rules[index].modifier_key)))

      || (unique_Rules[index].rule_type === "C2"
        && ((this.isAcceptableKeymanModifier(unique_Rules[index].modifier_deadkey))
          && (this.isAcceptableKeymanModifier(unique_Rules[index].modifier_key)))
      )

      || (unique_Rules[index].rule_type === "C3"
        && ((this.isAcceptableKeymanModifier((unique_Rules[index].modifier_prev_deadkey)))
          && (this.isAcceptableKeymanModifier((unique_Rules[index].modifier_deadkey)))
          && (this.isAcceptableKeymanModifier((unique_Rules[index].modifier_key))))
      )
    )
      return ""
    return ("unavailable modifier in: ")
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
    if (pos === 0x31) return "K_SPACE"      /* \ */
    if (pos === 0x2A) return "K_BKSLASH"    /* \ */   // 42 for ISO  correct??
    // if (pos === 0x30) return "K_?C1"     /* \ */   // 48 for ANSI  correct??
    // TODO numbers OK??

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
    if (pos === 0x2B) return "K_COMMA"      /* , */
    if (pos === 0x2F) return "K_PERIOD"     /* . */
    if (pos === 0x2C) return "K_SLASH"      /* / */

    if (pos === 0x24) return "K_ENTER"
    else return ""
  }
  //----------------------------------------------------------------------------------------------------
  //----------------------------------------------------------------------------------------------------
  //----------------------------------------------------------------------------------------------------

  public createData_Rules(data_ukelele: convert_object): string {

    const maxkey: number = 50
    let data: string = ""
    let keymarker: string = ""

     const data_CAll: rule_object[] = data_ukelele.ArrayOf_Rules.filter((curr) => {
      if ((curr.output !== new TextEncoder().encode("") || curr.output !== undefined)
        && (curr.rule_type === "C0") || (curr.rule_type === "C1") || (curr.rule_type === "C2") || (curr.rule_type === "C3")) {
        return curr;
      }
      else return ""
    });
    const unique_CAll_Rules: rule_object[] = data_CAll.reduce((unique, o) => {
      if (!unique.some((obj: { output: Uint8Array; rule_type: string; modifier_key: string; modifier_deadkey: string; prev_deadkey: string; modifier_prev_deadkey: string; deadkey: string; key: string }) =>
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

    console.log("xx  data_ukelele.ArrayOf_Rules", data_ukelele.ArrayOf_Rules.length)
    //console.log("xx  data_ukelele.ArrayOf_Rules", this.writeDalaset(data_ukelele.ArrayOf_Rules))

    console.log("xx  unique_CAll_Rules", unique_CAll_Rules.length)
    //console.log("xx  unique_CAll_Rules", this.writeDalaset(unique_CAll_Rules))

    //................................................ C0 C1 ................................................................
    //................................................ C0 C1 ................................................................
    //................................................ C0 C1 ................................................................

    for (let i = 0; i < unique_CAll_Rules.length; i++) {

      if ((unique_CAll_Rules[i].rule_type === "C0") || (unique_CAll_Rules[i].rule_type === "C1")) {
        // lookup key nr of the key that is being processed
        let keyNr: number = 0;
        for (let j = 0; j < maxkey; j++) {
          if (this.map_UkeleleKC_To_VK(j) === unique_CAll_Rules[i].key)
            keyNr = j
        }

        // skip keyNr 48 ( TAB )
        if (keyNr === 48)
          continue

        // add a line after rules of each key
        if (unique_CAll_Rules[i].key !== keymarker)
          data += '\n'

        let warningtext: string = "c C0 WARNING: "
        warningtext = warningtext + this.findDuplicateRules(unique_CAll_Rules, i, "C01")
        //warningtext = warningtext + this.findDuplicateRules(unique_CAll_Rules, i)
        warningtext = warningtext + this.findAmbiguousRules(unique_CAll_Rules, i)
        warningtext = warningtext + this.findUnAvailableRule(unique_CAll_Rules, i)

        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ((unique_CAll_Rules[i].rule_type === "C0") || (unique_CAll_Rules[i].rule_type === "C1")
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if ((KeylayoutToKmnConverter.print_draft) && (new TextDecoder().decode(unique_CAll_Rules[i].output) !== "")) {
          if (warningtext === "c C0 WARNING: ")
            data += unique_CAll_Rules[i].rule_type + keyNr + keyNr + "-(modif:" + unique_CAll_Rules[i].rule_type + "-" + unique_CAll_Rules[i].dk_prev + `) + [` + (unique_CAll_Rules[i].modifier_key + ' ' + unique_CAll_Rules[i].key).trim() + `] °°-> \'` + new TextDecoder().decode(unique_CAll_Rules[i].output) + '\'\n'
          else
            data += warningtext + keyNr + "-(modif:" + unique_CAll_Rules[i].rule_type + "-" + unique_CAll_Rules[i].dk_prev + `) + [` + (unique_CAll_Rules[i].modifier_key + ' ' + unique_CAll_Rules[i].key).trim() + `] **°°-> \'` + new TextDecoder().decode(unique_CAll_Rules[i].output) + '\'\n'
        }
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~ draft print end ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        else {
          if (warningtext === "c C0 WARNING: ")
            data += "+ [" + (unique_CAll_Rules[i].modifier_key + ' ' + unique_CAll_Rules[i].key).trim() + `]  > \'` + new TextDecoder().decode(unique_CAll_Rules[i].output) + '\'\n'
          else
            data += warningtext + (unique_CAll_Rules[i].modifier_key + ' ' + unique_CAll_Rules[i].key).trim() + `]  > \'` + new TextDecoder().decode(unique_CAll_Rules[i].output) + '\'\n'
        }
        keymarker = unique_CAll_Rules[i].key
      }
    }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    if (KeylayoutToKmnConverter.print_draft) {
      data += "\n########## C2 #################################################################\n"
    }
    else
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // ~~~~ draft print end ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      data += "\n"

    //................................................ C2 ...................................................................
    //................................................ C2 ...................................................................
    //................................................ C2 ...................................................................


    for (let k = 0; k < unique_CAll_Rules.length; k++) {

      if (unique_CAll_Rules[k].rule_type === "C2") {

        let warningtext: string = "c C2 WARNING: "
        warningtext = warningtext + this.findDuplicateRules(unique_CAll_Rules, k, "C2")
        warningtext = warningtext + this.findAmbiguousRules(unique_CAll_Rules, k)
        warningtext = warningtext + this.findUnAvailableRule(unique_CAll_Rules, k)

        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (KeylayoutToKmnConverter.print_draft) {
          if (warningtext === "c C2 WARNING: ") {
            data += "new: [" + unique_CAll_Rules[k].modifier_deadkey + " " + unique_CAll_Rules[k].deadkey + "]  >  XXX" +
              " [" + unique_CAll_Rules[k].modifier_key + " " + unique_CAll_Rules[k].key + "]  °°->  \'" + new TextDecoder().decode(unique_CAll_Rules[k].output) + "\'"
          }
          else {
            data += warningtext + " [" + unique_CAll_Rules[k].modifier_deadkey + " " + unique_CAll_Rules[k].deadkey + "]  >  XXX" +
              "[" + unique_CAll_Rules[k].modifier_key + " " + unique_CAll_Rules[k].key + "]   >  \'" + new TextDecoder().decode(unique_CAll_Rules[k].output) + "\'"
          }
        }
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~ draft print end ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        else {
          // all OK
          if (warningtext === "c C2 WARNING: ") {

            if (unique_CAll_Rules[k].uniqueB !== 0) {
              data += "+ [" + unique_CAll_Rules[k].modifier_deadkey + " " + unique_CAll_Rules[k].deadkey + "]  >  dk(C" + String(unique_CAll_Rules[k].dk_C2) + ")\n"
            }
            //data += "---\n"
            data += "dk(C" + String(unique_CAll_Rules[k].dk_C2) + ") + [" + unique_CAll_Rules[k].modifier_key + " " + unique_CAll_Rules[k].key + "]  >  \'" + new TextDecoder().decode(unique_CAll_Rules[k].output) + "\'\n"
          }
          // warning avail
          else {
            data += warningtext + unique_CAll_Rules[k].modifier_deadkey + " " + unique_CAll_Rules[k].deadkey + "]  >  dk(C" + String(unique_CAll_Rules[k].dk_C2) + ")\n"
            data += " dk(C" + String(unique_CAll_Rules[k].dk_C2) + ") + [" + unique_CAll_Rules[k].modifier_key + " " + unique_CAll_Rules[k].key + "]   >  \'" + new TextDecoder().decode(unique_CAll_Rules[k].output) + "\'\n"
          }

        }
        data += "\n"
      }
    }

    //................................................ C3 ...................................................................
    //................................................ C3 ...................................................................
    //................................................ C3 ...................................................................

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (KeylayoutToKmnConverter.print_draft) {
      data += "\n########## C3 #################################################################\n"
    }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print end ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    console.log("NOW print C§ ",)


    for (let k = 0; k < unique_CAll_Rules.length; k++) {

      if (unique_CAll_Rules[k].rule_type === "C3") {

        let warningtext: string = "c C3 WARNING: "
        warningtext = warningtext + this.findDuplicateRules(unique_CAll_Rules, k, "C3")    // OK
        warningtext = warningtext + this.findAmbiguousRules(unique_CAll_Rules, k)     // OK
        warningtext = warningtext + this.findUnAvailableRule(unique_CAll_Rules, k)    // OK
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (KeylayoutToKmnConverter.print_draft) {
          if (warningtext === "c C3 WARNING: ") {
            data += " [" + unique_CAll_Rules[k].modifier_prev_deadkey + " " + unique_CAll_Rules[k].prev_deadkey + "]   >   XXX  " +
              "[" + unique_CAll_Rules[k].modifier_deadkey + " " + unique_CAll_Rules[k].deadkey + "]  >  YYY " +
              " [" + unique_CAll_Rules[k].modifier_key + " " + unique_CAll_Rules[k].key + "]  °°->  \'" + new TextDecoder().decode(unique_CAll_Rules[k].output) + "\'"
          }
          else {
            data += warningtext + " [" + unique_CAll_Rules[k].modifier_prev_deadkey + " " + unique_CAll_Rules[k].prev_deadkey + "]   >   XXX  " +
              "[" + unique_CAll_Rules[k].modifier_deadkey + " " + unique_CAll_Rules[k].deadkey + "]  >  YYY " +
              " [" + unique_CAll_Rules[k].modifier_key + " " + unique_CAll_Rules[k].key + "]  °°->  \'" + new TextDecoder().decode(unique_CAll_Rules[k].output) + "\'"
          }
        }
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~ draft print end ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        else {
          if (warningtext === "c C3 WARNING: ") {

            if (unique_CAll_Rules[k].uniqueA !== 0) {
              data += "+ [" + unique_CAll_Rules[k].modifier_prev_deadkey + " " + unique_CAll_Rules[k].prev_deadkey + "]   >   dk(A" + String(unique_CAll_Rules[k].dk_prev) + ")\n"
            }

            if ((unique_CAll_Rules[k].uniqueB !== 0) && (unique_CAll_Rules[k].uniqueA !== 0)) {
              data += "dk(A" + String(unique_CAll_Rules[k].dk_prev) + ")  + [" + unique_CAll_Rules[k].modifier_deadkey + " " + unique_CAll_Rules[k].deadkey + "]  >  dk(B" + String(unique_CAll_Rules[k].dk) + ")\n"
            }

            data += "dk(B" + String(unique_CAll_Rules[k].dk) + ") + [" + unique_CAll_Rules[k].modifier_key + " " + unique_CAll_Rules[k].key + "]  >  \'" + new TextDecoder().decode(unique_CAll_Rules[k].output) + "\'\n"
            data += "\n"
          }

          else {
            data += warningtext + unique_CAll_Rules[k].modifier_prev_deadkey + " " + unique_CAll_Rules[k].prev_deadkey + "]   >   dk(A" + String(unique_CAll_Rules[k].dk_prev) + ")\n"
            data += "dk(A" + String(unique_CAll_Rules[k].dk_prev) + ") + [" + unique_CAll_Rules[k].modifier_deadkey + " " + unique_CAll_Rules[k].deadkey + "]  >  dk(B" + String(unique_CAll_Rules[k].dk) + ")\n"
            data += "dk(B" + String(unique_CAll_Rules[k].dk) + ") + [" + unique_CAll_Rules[k].modifier_key + " " + unique_CAll_Rules[k].key + "]  >  \'" + new TextDecoder().decode(unique_CAll_Rules[k].output) + "\'\n"
          }
        }
        data += "\n"
      }
    }

    data += '\n'
    return data
  }

  public createData_Stores(data_ukelele: convert_object): string {
    let data: string = ""

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (KeylayoutToKmnConverter.print_draft) {
      data += "c\n"
      data += "c Keyman keyboard generated by kmn-convert\n"
      data += "c\n"
      data += "\n"

      data += '\########## OK #################################################################\n'
      data += "store(&VERSION) \'10.0\'\n"
      data += "store(&TARGETS) \'any\'\n"
      data += "store(&KEYBOARDVERSION) \'1.0\'\n"
      data += "store(&COPYRIGHT) '© 2024 SIL International\'\n"
      // TODO what else ??

      data += '\########## OK #################################################################\n'
      data += "\n"
      data += "begin Unicode > use(main)\n\n"
      data += "group(main) using keys\n\n"

      data += '\########## OK #################################################################\n'
      data += "Tipp: if K_? is replaced by undefined-> no enry in kmn_Key_Name=> add K_? there and it will be shown here\n"
      data += "Tipp: keys that are marked with sction do not aoear in ukelele_Array_output->do not appear in kmn\n"
      data += "\n"
    }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print end ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else {
      data += "c\n"
      data += "c Keyman keyboard generated by kmn-convert\n"
      data += "c\n"
      data += "\n"

      data += "store(&VERSION) \'10.0\'\n"
      data += "store(&TARGETS) \'any\'\n"
      data += "store(&KEYBOARDVERSION) \'1.0\'\n"
      data += "store(&COPYRIGHT) '© 2024 SIL International\'\n"
      // TODO what else ??

      data += "\n"
      data += "begin Unicode > use(main)\n\n"
      data += "group(main) using keys\n\n"

      data += "\n"
    }
    return data
  }

  /// console log all entries C3  TODO remove
  public writeDalaset(dataRules: rule_object[]) {
    for (let i = 0; i < dataRules.length; i++) {

      console.log("dataRules ",

        dataRules[i].rule_type !== "" ? dataRules[i].rule_type : "--".padEnd(4, " ")
        , dataRules.length, i,

        "|  ", (dataRules[i].modifier_prev_deadkey !== "" ? dataRules[i].modifier_prev_deadkey.padEnd(33, " ") : "--".padEnd(33, " ")),
        (dataRules[i].prev_deadkey !== "" ? dataRules[i].prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        (dataRules[i].dk_prev !== 0 ? ("dk(A" + String(dataRules[i].dk_prev) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),
        (dataRules[i].uniqueA !== 0 ? ("unique(A" + String(dataRules[i].uniqueA) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),


        (dataRules[i].modifier_deadkey !== "" ? dataRules[i].modifier_deadkey.padEnd(30, " ") : "--".padEnd(30, " ")),
        (dataRules[i].deadkey !== "" ? dataRules[i].deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        dataRules[i].rule_type === "C2" ? (dataRules[i].dk !== 0 ? ("dk(C" + String(dataRules[i].dk) + ")").padEnd(9, " ") : "--".padEnd(9, " ")) : ((dataRules[i].dk !== 0 ? ("dk(B" + String(dataRules[i].dk) + ")").padEnd(9, " ") : "--".padEnd(9, " "))),
        dataRules[i].rule_type === "C2" ? (dataRules[i].uniqueB !== 0 ? ("unique(C" + String(dataRules[i].uniqueB) + ")").padEnd(9, " ") : "--".padEnd(9, " ")) : ((dataRules[i].uniqueA !== 0 ? ("unique(B" + String(dataRules[i].dk) + ")").padEnd(9, " ") : "--".padEnd(9, " "))),


        "|  ", (dataRules[i].modifier_key !== "" ? dataRules[i].modifier_key.padEnd(40, " ") : "--".padEnd(40, " ")),
        (dataRules[i].key !== "" ? dataRules[i].key.padEnd(8, " ") : "--".padEnd(8, " ")),
        (new TextDecoder().decode(dataRules[i].output) !== "" ? new TextDecoder().decode(dataRules[i].output).padEnd(10, " ") : ("--" + dataRules[i].output) + "--"),

        "| °°")
    }

  }
  // TODO remove
  public writeDalasetSingle(dataRules: rule_object) {

    console.log("dataRules ",
      dataRules.rule_type !== "" ? dataRules.rule_type : "--".padEnd(4, " ")
      ,

      "|  ", (dataRules.modifier_prev_deadkey !== "" ? dataRules.modifier_prev_deadkey.padEnd(33, " ") : "--".padEnd(33, " ")),
      (dataRules.prev_deadkey !== "" ? dataRules.prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
      (dataRules.dk_prev !== 0 ? ("dk(A" + String(dataRules.dk_prev) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),
      (dataRules.uniqueA !== 0 ? ("unique(A" + String(dataRules.uniqueA) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),


      (dataRules.modifier_deadkey !== "" ? dataRules.modifier_deadkey.padEnd(30, " ") : "--".padEnd(30, " ")),
      (dataRules.deadkey !== "" ? dataRules.deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
      dataRules.rule_type === "C2" ? (dataRules.dk !== 0 ? ("dk(C" + String(dataRules.dk) + ")").padEnd(9, " ") : "--".padEnd(9, " ")) : ((dataRules.dk !== 0 ? ("dk(B" + String(dataRules.dk) + ")").padEnd(9, " ") : "--".padEnd(9, " "))),
      dataRules.rule_type === "C2" ? (dataRules.uniqueB !== 0 ? ("unique(C" + String(dataRules.uniqueB) + ")").padEnd(9, " ") : "--".padEnd(9, " ")) : ((dataRules.uniqueA !== 0 ? ("unique(B" + String(dataRules.dk) + ")").padEnd(9, " ") : "--".padEnd(9, " "))),


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
    public dk_prev: number,
    public uniqueA: number,

    public modifier_deadkey: string,      /* second key used by C2,C3 rules*/
    public deadkey: string,
    public dk: number,                    //todo remove one
    public dk_C2: number,
    public uniqueB: number,

    public modifier_key: string,          /* third key used by C0,C1,C2,C3,C4 rules*/
    public key: string,
    public output: Uint8Array,            /* output used by C0,C1,C2,C3,C4 rules*/

  ) { }

}
