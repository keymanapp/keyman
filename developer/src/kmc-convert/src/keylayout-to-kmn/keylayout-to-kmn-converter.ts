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
  //     Several steps action-> action-> action->character ( not only  action->character)
  //     TODO waht about using actions twice in a row???
  //     Usable for all keylayout files
  //     Return conditions
  // OK  Use callbacks as for writeFileSync
  //     Tests throws
  //     Conditions NCAPS,OPT;...
  //     TODO move func outside of class
  //     Functions as object methods? 
  //     objects contain only used stuff READ in: -- out: only read arrays / CONVERT in: only read arrays out: return only to write arrays
  //     Use catch blocks for file read
  // OK  read:  answer :  + [K_A] > 'a'  is OK / TODO which format to use in output ?  + [K_A] > 'a' (character code)  or    + [K_A] > U+0061 (virt Keycode)
  //     read   TODO which stores?
  // OK  one entry vs several entry in tags
  //     false arrangement of tags ?
  // OK  no added NCAPS when first modifier in modifierMap does not contain "caps" or"caps?"
  //     use length to clear array instead of defining evetry time new (modifierMap_ONE_InKeymapSelect.length=0
  //     naming of for-loop var i,j,k?...
  //     warning if contrADICTING RULES E:G:   [ 'modifier_C0', 'SHIFT NCAPS', 'K_C', 'c'],  vs [ 'modifier_C0', 'SHIFT NCAPS', 'K_C', 'C' ],
  //  OK print NCAPS as the first of the modifiers in create_kmn_modifier  
  //     use boxXmlArray from codebase instead of my own
  //     where are rules for action+none ( a, e, u, etc)
  //     order of object member var
  //     readfilesync with paths better way?
  //     rearrange code to use read, convert, write
  //     remove all any
  //     TODO rewrite explanantion for object instead of array
  //     remove all any types
  //     let push_count = 0    // Todo remove later
  //     check code for code styses keyman
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

// todo use from elsewhere
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

export interface rule_object {
  rule_type: string,              /* rule type C0-C4 */
  isTerminator: boolean,          /* is this a terminator rule */

  modifier_prev_deadkey: string,  /* string of modifiers for the first key (e.g. "NCAPS RALT CTRL") */
  prev_deadkey: string,           /* name of the first key (e.g. K_U) */
  prev_deadkeys_Ch: Uint8Array,   /* Todo needed?*/
  dk_prev: number,                /* Todo needed?*/

  modifier_deadkey: string,       /* string of modifiers for the second key (e.g. "NCAPS RALT CTRL") */
  deadkey: string,                /* name of the second key */
  deadkeys_Ch: Uint8Array,        /* Todo needed?*/
  dk: number,                     /* Todo needed?*/

  modifier_key: string,           /* string of modifiers for the third key (e.g. "NCAPS RALT CTRL") */
  key: string,                    /* name of the third key (e.g. K_U) */
  deadkeyed_Ch: Uint8Array,       /* the output character */
};

export interface convert_object {
  ArrayOf_Modifiers: string[][],
  ArrayOf_Rules: rule_object[],
};


export class KeylayoutToKmnConverter {
  static readonly INPUT_FILE_EXTENSION = '.keylayout';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';

  //Todo remove print_draft
  static print_draft = true

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
   *         member function to read filename ( a .keylayout-file) and write contents into Uint8Array keys_all_Layers
   * @param  filename the ukelele .keylayout-file to be converted
   * @return in case of success Uint8Array keys_all_Layers; else null
   */
  public read(filename: string): convert_object {

    const modifierBehavior: string[][] = []          // modifier for each keymapselect
    const RuleObject: rule_object[] = []             // an array of objects which hold data for a kmn rule

    const DataObject: convert_object = {
      ArrayOf_Modifiers: modifierBehavior,   // e.g. 18 modifiers in 8 KeyMapSelect(behaviors)
      ArrayOf_Rules: RuleObject
    };

    console.log("inputFilename read", filename)
    const options = {
      ignoreAttributes: false,
      attributeNamePrefix: '@_'    // to access the attribute
    };

    // Todo files/path !!!
    console.log("xmlFile_name", (process.cwd() + "\\data" + filename.substring(filename.lastIndexOf("\\"))).replace(" ", ""))
    //const xmlFile = readFileSync(`${process.cwd()}/data/MySample.keylayout`, 'utf8')
    const xmlFile = readFileSync((process.cwd() + "\\data" + filename.substring(filename.lastIndexOf("\\"))).replace(" ", ""), 'utf8');
    console.log("xmlFile parsed ")

    // we don`t need file-read with uint8array return
    /*const fullPath = (process.cwd() + "\\data" + filename.substring(filename.lastIndexOf("\\"))).replace(" ", "")
    const xmlFile1 = this.callbacks.loadFile(fullPath)
    console.log("xmlFile1",xmlFile1)*/

    const parser = new XMLParser(options);
    const jsonObj = parser.parse(xmlFile); // get plain Object

    // jsonObj now contains only arrays; no single fields
    boxArrays_S(jsonObj.keyboard);

    // MODIFIER MAP: get behaviours(=MapIndex) and modifiers (e.g. shift? leftShift caps? )
    for (let j = 0; j < jsonObj.keyboard.modifierMap.keyMapSelect.length; j++) {
      const singleModifierSet: string[] = []
      for (let k = 0; k < jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier.length; k++) {
        singleModifierSet.push(jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier[k]['@_keys'])
      }
      modifierBehavior.push(singleModifierSet)
    }

    // Todo ? move to convert?
    // TODO review condition
    return this.createRuleData(DataObject, jsonObj)
  }


  /**
   * @brief  member function to convert data of .keylayout-file to kmn-file This will convert/rename modifiers, position of Keys and deadkeys and save into an array 
   * @param  take data_ukelele and create a mapping from mac Keycodes to key-names and save to data_ukelele object
   * @param  data_ukelele (Uint8Array) data of the ukelele .keylayout-file
   * @return outArray Uint8Array keys_all_Layers, the converted data for kmn-files if all layers have been converted; else null
   */
  public convert(data_ukelele: convert_object): convert_object {
    return data_ukelele
  }


  /**
   * @brief   member function to write data fro object to file
   * @param  data_ukelele the array holding keyboard data
   * @return true if data has been written; false if not
   */
  //TODO need to use export const USVirtualKeyCodes here
  public write(data_ukelele: convert_object): boolean {
    console.log("\n###################################################\n")
    let data = "\n"

    // add top part of kmn file: STORES
    data += this.createData_Stores(data_ukelele)

    // add middle part of kmn file: RULES
    data += this.createData_Rules(data_ukelele)

    // add bottom part of kmn file: DEADKEYS
    data += this.createData_Deadkeys(data_ukelele)

    data += "\n"

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
    const ObjectArray: any[] = []
    let dk_counter_C3_A = 0
    let dk_counter_C3_B = 0
    let dk_counter_C2 = 0

    // start Tests v ToDo remove......................................
    const testArray_Ukelele: string[] = []
    let testArray_Ukelele_count = 0
    const testArray_Ukelele_action: string[] = []
    let testArray_Ukelele_action_count = 0

    const testArray_kmn: string[] = []
    // const testArray_kmn_action: string[] = []
    const testArray_kmn_action1: string[] = []
    let testArray_kmn_count = 0
    // let testArray_kmn_action_count = 0


    // loop behaviors ( in ukelele it is possible to define multiple modifier combinations that behave in the same)
    for (let i = 0; i < jsonObj.keyboard.keyMapSet[0].keyMap.length; i++) {
      // loop keys 0-50 (= all keys we use)
      for (let j = 0; j < 51; j++) {

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
    }
    // End Tests ToDo remove   ^.......................................


    let action_id
    const used_Keys_count = 51

    const isCapsused = this.checkIfCapsUsed(data_ukelele.ArrayOf_Modifiers)

    // loop keys 0-50 (= all keys we use)
    for (let j = 0; j < used_Keys_count; j++) {

      // loop behaviors ( in ukelele it is possible to define multiple modifier combinations that behave in the same)
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[0].keyMap.length; i++) {

        // ......................................................................................................
        // case C0: output ......................................................................................
        // a key is mapped to a character directly ( code-> output) .............................................
        // ...............e. g. <key code="1" output="s"/> ......................................................
        // ......................................................................................................

        let RuleObj

        if ((jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== undefined)) {

          testArray_kmn.push(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'])
          testArray_kmn_count++
          if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== "") {

            // loop modifier combinations
            for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {
              if (this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']))) {

                RuleObj = new Rules(
                  "C0",
                  false,

                  "",
                  "",
                  new TextEncoder().encode(""),
                  0,

                  "",
                  "",
                  new TextEncoder().encode(""),
                  0,
                  0,

                  this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                  this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                  new TextEncoder().encode(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output']),
                )
                ObjectArray.push(RuleObj)
              }
            }
          }
        }

        else if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] !== undefined) {

          // ......................................................................................................
          // case C1: action + state none + output ................................................................
          // a key is mapped to an action and then to an output ...................................................
          // KeyMap:code->KeyMap:action->action:action_state(none)->action_output .................................
          // ...............e. g. <when state="none" output="a" ...................................................
          // ......................................................................................................

          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']
          // loop modifiers
          for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {

            if (this.get_Action2ID_NoneOutput__From__ActionID_Id(jsonObj, action_id) !== undefined) {

              RuleObj = new Rules(
                "C1",
                false,

                "",
                "",
                new TextEncoder().encode(""),
                0,

                "",
                "",
                new TextEncoder().encode(""),
                0,
                0,

                this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                new TextEncoder().encode(this.get_Action2ID_NoneOutput__From__ActionID_Id(jsonObj, action_id))
              )
              ObjectArray.push(RuleObj)
            }
          }
          // ......................................................................................................
          // case C2: action + state Nr + output ..................................................................
          // a key is mapped to an action, then to an state+output ................................................
          // replace state x with all rules that result in x (<when state="none" next="x") e.g. x=2 ...............
          // Action:actionId -> KeyMap:action -> KeyMap:Code -> KeyName............................................
          // Action:next -> Action:state -> action:output .........................................................
          // ...............e. g. <when state="2" output="à"/> ....................................................
          // ......................................................................................................

          /* oldcase C2
          let nextVal: string[] = []

          // find the nth action id e.g.  id a9 ->id nr 20
          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']
          const indexOfActions = this.get_ActionID_Index__From__ActionID_Id(jsonObj, action_id)

          // loop through all actionh/when find state and get action id ( 20-> state = 1)
          for (let jj = 0; jj < jsonObj.keyboard.actions.action[indexOfActions].when.length; jj++) {
            const stateVal = jsonObj.keyboard.actions.action[indexOfActions].when[jj]['@_state']

            if (jsonObj.keyboard.actions.action[indexOfActions].when[jj]['@_output'] !== undefined) {
              testArray_kmn_action.push(jsonObj.keyboard.actions.action[indexOfActions].when[jj]['@_output'])
              testArray_kmn_action_count++
            }
            testArray_kmn_action1 = testArray_kmn_action.filter(function (item, pos, self) {
              return self.indexOf(item) == pos;
            })

            // if there is a state defined, collect all cases which result in that state ( e.g. which action id contains next = 1)
            if (stateVal !== undefined) {

              // get all cases which result in state 3
              nextVal = this.get_ActionID_array__From__ActionID_NoneNext(jsonObj, stateVal)

              // for all modifier combinations
              for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {
                for (let k = 0; k < nextVal.length; k++) {
                  if (jsonObj.keyboard.actions.action[indexOfActions].when[jj]['@_output'] !== undefined) {
                    const KeymapIndex = this.get_KeyMap_Index__From__KeyMap_Action(jsonObj, nextVal[k])

                    for (let zz = 0; zz < data_ukelele.ArrayOf_Modifiers[KeymapIndex].length; zz++) {
                      RuleObj = new Rules(
                        "C2",
                        false,

                        "",
                        "",
                        new TextEncoder().encode(""),
                        0,

                        this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[KeymapIndex][zz], isCapsused),
                        this.map_UkeleleKC_To_VK(Number(this.get_KeyMap_Code__From__KeyMap_Action(jsonObj, nextVal[k]))),
                        new TextEncoder().encode(""),
                        0,
                        dk_counter_C2++,

                        this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                        this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                        new TextEncoder().encode(jsonObj.keyboard.actions.action[indexOfActions].when[jj]['@_output'])
                      )
                      ObjectArray.push(RuleObj)
                    }
                  }
                }
              }
            }
          }
            */

          // ......................................................................................................
          // case C2 NEW : action + state Nr + Next ....................................................................
          // ...............e. g.<when state="14" next="20"/> .....................................................
          // replace state x with all rules that result in 14 (<when state="x" next="14") .........................
          // a key is mapped to an action and then to a terminator ................................................
          // code->action->action(state)->action(next)->terminator(output) ........................................
          // Action:state -> Action:state -> Action:id -> KeyMap:action -> keyMap:code (first dk) .................
          // Action:id -> KeyMap:action -> keyMap:code (second dk) ................................................
          // Action:next -> Action:state -> action:output .........................................................
          // Action:id -> KeyMap:action -> keyMap:code (key) ......................................................          
          // ......................................................................................................         


          // get a9  in behavior/key
          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']
          const actionIdIndexC2 = this.get_ActionID_Index__From__ActionID_Id(jsonObj, action_id)

          // loop all action-when and find state-next-pair
          for (let l = 0; l < jsonObj.keyboard.actions.action[actionIdIndexC2].when.length; l++) {

            // state_none data
            if ((jsonObj.keyboard.actions.action[actionIdIndexC2].when[l]['@_state'] === "none")
              && (jsonObj.keyboard.actions.action[actionIdIndexC2].when[l]['@_next'] !== undefined)) {


              // Data of Block Nr 5 ..........................................................
              /*  eg: state = 3  */                // const b5_value_state = jsonObj.keyboard.actions.action[actionIdIndexC2].when[l]['@_state']
              /*  eg: next  = 1  */                 const b5_value_next = jsonObj.keyboard.actions.action[actionIdIndexC2].when[l]['@_next']
              /*  eg: StateNextID = a16  */         const b5_actionId = jsonObj.keyboard.actions.action[actionIdIndexC2]['@_id']
              // .............................................................................
console.log(" action_id", action_id)

              console.log("b5_value_next ",b5_value_next )
console.log(" b5_actionId", b5_actionId)

              // Data of Block Nr 4 ..........................................................
              /*  eg: [ '6', '31', '32' ]*/         const b4_code_arr = this.get_KeyMap_Code_array__From__KeyMap_Action(jsonObj, b5_actionId)
              /*  eg: 0 */                          const b4_behaviourId = this.get_KeyMap_Index__From__KeyMap_Action(jsonObj, b5_actionId)
              /*  eg: ['CAPS', 'RALT'] */           const b4_modifier_arr = this.get_data_From__KeyMap_Modifier_Index(data_ukelele.ArrayOf_Modifiers, b4_behaviourId)
              // .............................................................................

              // Data of Block Nr 3 ..........................................................
              /*  eg: actioniD = a17  */            //const b3_actionId = this.get_ActionID_Id__From__ActionID_next(jsonObj, b5_value_state)
              // .............................................................................

              // Data of Block Nr 2 ..........................................................
              /*  eg: ['K_8', 'K_M]  */             //const b2_keyname_arr = this.get_KecCode_arr__From__ActionId(jsonObj, b3_actionId)
              /*  eg: index=3 */                   // const b2_keyBehaviour_arr = this.get_KeyMap_Code_array__From__ActionID_Action(jsonObj, String(b3_actionId))
              /* e.g. [[ '0','1shift? caps?']]*/   // const b2_modifier_arr_all = this.get_KeyMap_Modifier_array__From__behaviour_arr(data_ukelele.ArrayOf_Modifiers, b2_keyBehaviour_arr)
              // .............................................................................

              // Data of Block Nr 6 ..........................................................
              /*  eg: [ 'a9','1','â'] */            const b6_actionId_arr = this.get_ActionID_Output_array__From__ActionID_State(jsonObj, b5_value_next)
              // .............................................................................

              // Data of Block Nr 1 ..........................................................
              /*  eg: ['49','K_SPACE','a0','0'] */  const b1_keycode_arr = this.get_KeyMap_Code_array__From__KeyMap_Action_array2D(jsonObj, b6_actionId_arr)
              // .............................................................................

              for (let n1 = 0; n1 < b6_actionId_arr.length; n1++) {
                for (let n2 = 0; n2 < b1_keycode_arr.length; n2++) {
                  if (b6_actionId_arr[n1][0] === b1_keycode_arr[n2][2]) {
                    for (let n3 = 0; n3 < data_ukelele.ArrayOf_Modifiers[Number(b1_keycode_arr[n2][3])].length; n3++) {
                      for (let n4 = 0; n4 < b4_modifier_arr.length; n4++) {
                        //  for (let x5 = 0; x5 < b2_modifier_arr_all.length; x5++) {
                        //    for (let n6 = 0; n6 < b2_modifier_arr_all[x5].length; n6++) {
                        for (let n7 = 0; n7 < b4_code_arr.length; n7++) {
                          //        for (let n8 = 0; n8 < b2_keyname_arr.length; n8++) {

                          RuleObj = new Rules(
                                /* OK rule_type */              "C2",
                                /* OK isTerminator */           false,

                                /* OK  modifier_prev_deadkey*/  "",
                                /* OK  prev_deadkey */          "",
                                /*     prev_deadkeys_Ch*/       new TextEncoder().encode(""),
                                /* OK  dk_prev */               0,

                                /* OK  modifier_deadkey */      b4_modifier_arr[n4],
                                /* OK  deadkey */               this.map_UkeleleKC_To_VK(Number(b4_code_arr[n7])),
                                /*     deadkeys_Ch  */          new TextEncoder().encode(""),
                                /* OK  dk*/                     dk_counter_C2++,
                                /* OK  dk for C2*/              0,

                                /* OK modifier_key*/            data_ukelele.ArrayOf_Modifiers[Number(b1_keycode_arr[n2][3])][n3],
                                /* OK  key */                   b1_keycode_arr[n2][1],
                                /* OK deadkeyed_Ch */           new TextEncoder().encode(b6_actionId_arr[n1][2])
                          )
                          ObjectArray.push(RuleObj)
                        }
                      }
                      //      }
                      //     }
                      //    }
                    }
                  }
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
          // Action:state -> Action:state -> Action:id -> KeyMap:action -> keyMap:code (first dk) .................
          // Action:id -> KeyMap:action -> keyMap:code (second dk) ................................................
          // Action:next -> Action:state -> action:output .........................................................
          // Action:id -> KeyMap:action -> keyMap:code (key) ......................................................          
          // ......................................................................................................         

          // get a9  in behavior/key
          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']
          const actionIdIndex = this.get_ActionID_Index__From__ActionID_Id(jsonObj, action_id)

          // loop all action-when and find state-next-pair
          for (let l = 0; l < jsonObj.keyboard.actions.action[actionIdIndex].when.length; l++) {

            // state_next data
            if ((jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_state'] !== "none")
              && (jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_next'] !== undefined)) {

              // Data of Block Nr 5 ..........................................................
              /*  eg: state = 3  */                 const b5_value_state = jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_state']
              /*  eg: next  = 1  */                 const b5_value_next = jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_next']
              /*  eg: StateNextID = a16  */         const b5_actionId = jsonObj.keyboard.actions.action[actionIdIndex]['@_id']
              // .............................................................................

              // Data of Block Nr 4 ..........................................................
              /*  eg: [ '6', '31', '32' ]*/         const b4_code_arr = this.get_KeyMap_Code_array__From__KeyMap_Action(jsonObj, b5_actionId)
              /*  eg: 0 */                          const b4_behaviourId = this.get_KeyMap_Index__From__KeyMap_Action(jsonObj, b5_actionId)
              /*  eg: ['CAPS', 'RALT'] */           const b4_modifier_arr = this.get_data_From__KeyMap_Modifier_Index(data_ukelele.ArrayOf_Modifiers, b4_behaviourId)
              // .............................................................................

              // Data of Block Nr 3 ..........................................................
              /*  eg: actioniD = a17  */            const b3_actionId = this.get_ActionID_Id__From__ActionID_next(jsonObj, b5_value_state)
              // .............................................................................

              // Data of Block Nr 2 ..........................................................
              /*  eg: ['K_8', 'K_M]  */             const b2_keyname_arr = this.get_KecCode_arr__From__ActionId(jsonObj, b3_actionId)
              /*  eg: index=3 */                    const b2_keyBehaviour_arr = this.get_KeyMap_Code_array__From__ActionID_Action(jsonObj, String(b3_actionId))
              /* e.g. [[ '0','1shift? caps?']]*/    const b2_modifier_arr_all = this.get_KeyMap_Modifier_array__From__behaviour_arr(data_ukelele.ArrayOf_Modifiers, b2_keyBehaviour_arr)
              // .............................................................................

              // Data of Block Nr 6 ..........................................................
              /*  eg: [ 'a9','1','â'] */            const b6_actionId_arr = this.get_ActionID_Output_array__From__ActionID_State(jsonObj, b5_value_next)
              // .............................................................................

              // Data of Block Nr 1 ..........................................................
              /*  eg: ['49','K_SPACE','a0','0'] */  const b1_keycode_arr = this.get_KeyMap_Code_array__From__KeyMap_Action_array2D(jsonObj, b6_actionId_arr)
              // .............................................................................

              for (let n1 = 0; n1 < b6_actionId_arr.length; n1++) {
                for (let n2 = 0; n2 < b1_keycode_arr.length; n2++) {
                  if (b6_actionId_arr[n1][0] === b1_keycode_arr[n2][2]) {
                    for (let n3 = 0; n3 < data_ukelele.ArrayOf_Modifiers[Number(b1_keycode_arr[n2][3])].length; n3++) {
                      for (let n4 = 0; n4 < b4_modifier_arr.length; n4++) {
                        for (let x5 = 0; x5 < b2_modifier_arr_all.length; x5++) {
                          for (let n6 = 0; n6 < b2_modifier_arr_all[x5].length; n6++) {
                            for (let n7 = 0; n7 < b4_code_arr.length; n7++) {
                              for (let n8 = 0; n8 < b2_keyname_arr.length; n8++) {

                                RuleObj = new Rules(
                                /* OK rule_type */              "C3",
                                /* OK isTerminator */           false,

                                /* OK  modifier_prev_deadkey*/  b2_modifier_arr_all[x5][n6],
                                /* OK  prev_deadkey */          b2_keyname_arr[n8],
                                /*     prev_deadkeys_Ch*/       new TextEncoder().encode(""),
                                /* OK  dk_prev */               dk_counter_C3_A++,

                                /* OK  modifier_deadkey */      b4_modifier_arr[n4],
                                /* OK  deadkey */               this.map_UkeleleKC_To_VK(Number(b4_code_arr[n7])),
                                /*     deadkeys_Ch  */          new TextEncoder().encode(""),
                                /* OK  dk*/                     dk_counter_C3_B++,
                                /* OK  dk for C2*/              0,

                                /* OK modifier_key*/            data_ukelele.ArrayOf_Modifiers[Number(b1_keycode_arr[n2][3])][n3],
                                /* OK  key */                   b1_keycode_arr[n2][1],
                                /* OK deadkeyed_Ch */           new TextEncoder().encode(b6_actionId_arr[n1][2])
                                )
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

          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']

          const next_id = this.get_ActionID_Next__From__ActionID_None(jsonObj, action_id)

          for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {

            if (new TextEncoder().encode(this.get_Terminator_Output__From__Terminator_State(jsonObj, next_id)).length !== 0) {

              RuleObj = new Rules(
                "C4",
                true,

                "",
                "",
                new TextEncoder().encode(""),
                0,

                "",
                "",
                new TextEncoder().encode(""),
                0,
                0,

                this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                new TextEncoder().encode(this.get_Terminator_Output__From__Terminator_State(jsonObj, next_id))
              )
            }
            ObjectArray.push(RuleObj)
          }
        }
        else
          console.log("ERROR : some entries are not available")
      }
    }

    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------
    // TODO remove:  test files: checkif kmn gats the same output as ukelele file(except for C3 t works well :))
    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------

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
      console.log("OOOOHHHH DIFFERENT AMOUNTS ")



    //  test modifiers

    if (this.create_kmn_modifier("", true) === "NCAPS") console.log("testNr 1 OK "); else console.log("testNr 1 NOT OK  - ", (this.create_kmn_modifier("", true)))
    if (this.create_kmn_modifier("", false) === "") console.log("testNr 2 OK "); else console.log("testNr 2 NOT OK  - ", (this.create_kmn_modifier("", false)))
    if (this.create_kmn_modifier("caps", true) === "CAPS") console.log("testNr 5 OK "); else console.log("testNr 5 NOT OK  - ", (this.create_kmn_modifier("caps", true)))
    if (this.create_kmn_modifier("6caps", true) === "CAPS") console.log("testNr 6 OK "); else console.log("testNr 6 NOT OK - ", (this.create_kmn_modifier("6caps", true)))
    if (this.create_kmn_modifier("rightControl", false) === "CTRL") console.log("testNr 13 OK "); else console.log("testNr 13 NOT OK - ", (this.create_kmn_modifier("rightControl", false)))
    if (this.create_kmn_modifier("command", false) === "command") console.log("testNr 16 OK "); else console.log("testNr 16 NOT OK  - ", (this.create_kmn_modifier("command", false)))
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
    /*
        if (this.replaceModifier("rightshift") === "SHIFT") console.log("testNr B 1 OK "); else console.log("testNr B 1 NOT OK  - ", this.replaceModifier("rightshift"))
        if (this.replaceModifier("rightshift SHIFT") === "SHIFT") console.log("testNr B 2 OK "); else console.log("testNr B 2 NOT OK  - ", this.replaceModifier("rightshift SHIFT"))
        if (this.replaceModifier("lshift") === "SHIFT") console.log("testNr B 3 OK "); else console.log("testNr B 3 NOT OK  - ", this.replaceModifier("lshift"))
        if (this.replaceModifier("lcontrol") === "CTRL") console.log("testNr B 4 OK "); else console.log("testNr B 4 NOT OK  - ", this.replaceModifier("lcontrol"))
        if (this.replaceModifier("RIGHTOPTION") === "RALT") console.log("testNr B 5 OK "); else console.log("testNr B 5 NOT OK  - ", this.replaceModifier("RIGHTOPTION"))
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
  public get_KeyMap_Code__From__KeyMap_Action(data: any, search: string): number {
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          return data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']
        }
      }
    }
    return -1
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
  public get_KeyMap_Code_array__From__KeyMap_Action_array(data: any, search: string[][]): string[] {
    const returnarray: string[] = []
    for (let k = 0; k < search.length; k++) {
      for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
        for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
          if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search[k][0]) {
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])
          }
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
  public get_ActionID_array__From__ActionID_NoneNext(data: any, search: string): string[] {
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
  public get_Action2ID_NoneOutput__From__ActionID_Id(data: any, search: any): any {
    let OutputValue = ""
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
  public get_Terminator_Output__From__Terminator_State(data: any, search: string): string {
    for (let i = 0; i < data.keyboard.terminators.when.length; i++) {
      if (data.keyboard.terminators.when[i]['@_state'] === search) {
        return data.keyboard.terminators.when[i]['@_output']
      }
    }
    return ""
  }

  // ToDo needed??
  public get_ActionID_Output__From__ActionID_IdState(data: any, search_id: string, search_state: string): Uint8Array {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
        if ((data.keyboard.actions.action[i]['@_id'] === search_id) &&
          (data.keyboard.actions.action[i].when[j]['@_state'] === search_state)) {
          return new TextEncoder().encode(data.keyboard.actions.action[i].when[j]['@_output'])
        }
      }
    }
    return new TextEncoder().encode("")
  }
  // ToDo needed??
  public get_ActionID_Key__From__ActionID_IdState(data: any, search_id: string, search_state: string): Uint8Array {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
        if ((data.keyboard.actions.action[i]['@_id'] === search_id) &&
          (data.keyboard.actions.action[i].when[j]['@_state'] === search_state)) {
          return new TextEncoder().encode(data.keyboard.actions.action[i].when[j]['@_output'])
        }
      }
    }
    return new TextEncoder().encode("")
  }
  // ToDo needed??
  public get_KeyMap_Keymaparray__From__KeyMap_Code_array(data: any, search: string[]): string[][] {
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
  }
  // ToDo needed??
  public get_KeyMap_Keymaparray__From__KeyMap_Action(data: any, search: string): string[] {
    const returnarray: string[] = []
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[i]['@_index'])
        }
      }
    }
    return returnarray
  }
  // ToDo needed??
  public get_ActionID_Id_array__From__ActionID_next(data: any, search: string): string[][] {
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
  }
  // ToDo needed?? get all entries for state= and a given action id
  public get_Action2ID_State_arrayState_Array__From__ActionID_Id(data: any, search: any): any[] {
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
  }
  // ToDo needed?? get all entries for state= and a given action id
  public get_ActionID_Id_arrayNext_Array__From__ActionID_StateNext(data: any, search: any): string[] {
    const returnarray: string[] = []
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let kk = 0; kk < data.keyboard.actions.action[i].when.length; kk++) {
        if ((data.keyboard.actions.action[i].when[kk]['@_next'] === search) && (data.keyboard.actions.action[i].when[kk]['@_state'] !== "none")) {
          returnarray.push(data.keyboard.actions.action[i]['@_id'])
        }
      }
    }
    return returnarray
  }
  public map_UkeleleKC_To_VK_array(search: string[]): string[] {
    const returnarray: string[] = []


    for (let i = 0; i < search.length; i++) {
      returnarray.push(this.map_UkeleleKC_To_VK(Number(search[i])))
    }


    return returnarray
  }
  public get_ActionID_Id__From__ActionID_Next(data: any, search: string) {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
        if ((data.keyboard.actions.action[i].when[j]['@_next'] === search)) {
          return data.keyboard.actions.action[i]['@_id']
        }
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
    for (let j = 0; j < search.length; j++) {
      mapIndexArray_max.push(data[search[j][1]])
    }
    return mapIndexArray_max
  }
  public get_Modifier_Text__From__Modifier_Index(data: any, search: string): string[] {
    const mapIndexArray_max: string[] = []
    const mapIndexArrayperKey: string[] = []
    for (let j = 0; j < data[search].length; j++) {
      mapIndexArrayperKey.push((data[search][j]))
    }
    for (let j = 0; j < data[search].length; j++) {
      mapIndexArray_max.push(data[search][j])
    }
    return mapIndexArray_max
  }
  public get_KeyMap_Code__From__ActionID_Action(data: any, search: string): number {
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
  }
  public get_KeyMap_Code__From__ActionID_Action2(data: any, search: string): number {
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
  }
  public get_ActionID_Next__From__ActionID_Id(data: any, search: string): string {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      if ((data.keyboard.actions.action[i]['@_id'] === search))
        return data.keyboard.actions.action[i].when[1]['@_next']
    }
    return ""
  }
  public get_KeyMap_Index__From__KeyMap_Action(data: any, search: string): number {
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          return data.keyboard.keyMapSet[0].keyMap[i]['@_index']
        }
      }
    }
    return -1
  }
  public get_KeyMap_IndexCodeAction_array__From__KeyMap_Action(data: any, search: string, keycode: number): string[][] {
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
  }
  public get_KeyMap_ModiIndex_array__From__KeyMap_Action(data: any, search_code: string, search_action: string): string[][] {
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
  }
  public get_ActionID_Output__From__ActionID_State(data: any, search: string, nextval: string): string {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
        if ((data.keyboard.actions.action[i].when[j]['@_state'] === search) && (data.keyboard.actions.action[i]['@_id'] === nextval)) {
          return data.keyboard.actions.action[i].when[j]['@_output']
        }
      }
    }
    return ""
  }
  public get_KeyMap_Modifier_aray__From__KeyMap_Index(data: any, search: number): string[] {
    return data.ArrayOf_Modifiers[search]
  }
  public get_data_From__KeyMap_Modifier_Index(data: any, search: number): string[] {
    return data[search]
  }
  public get_dataArray2D__From__KeyMap_Modifier_Index(data: any, search: string[]): string[][] {
    const returnarray: string[][] = []
    for (let i = 0; i < search.length; i++) {
      returnarray.push(data[search[i]])
    }
    return returnarray
  }

  /**
   * @brief  member function to return the unicode value of a character
   * @param  character the value that will converted
   * @return headecimal value of a character
   */
  public getHexFromChar(character: string): string {
    return character.charCodeAt(0).toString(16).slice(-4).toUpperCase().padStart(4, "0")
  }

  /** 
   * @brief  member function to create a string of modifiers in kmn-style from the modifierMap section of .keylayout-file
   * @param  keylayout_modifier the modifier value used in the .keylayout-file
   * @return kmn_modifier the modifier value used in the .kmn-file
   */
  //ToDo review lower part
  public create_kmn_modifier(keylayout_modifier: string, isCAPSused: boolean): string {
    let add_modifier = ""
    let kmn_modifier = ""
    let kmn_ncaps = ""

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

      // we do not use the right or left version of a modifier ( e.g. rightshift, leftshift). If they are used in keylayout files they will not be changed but used as is. 
      // Later when we write out the rules to the kmn file instead of writing those rules we print out a warning 
      else if ((String(modifier_state[i]).toUpperCase() === 'ANYOPTION') || (String(modifier_state[i]).toUpperCase() === 'OPTION')) add_modifier = "RALT ";
      else if ((String(modifier_state[i]).toUpperCase() === 'RIGHTOPTION')) add_modifier = "RALT ";
      else if ((String(modifier_state[i]).toUpperCase() === 'ANYSHIFT') || (String(modifier_state[i]).toUpperCase() === 'SHIFT')) add_modifier = "SHIFT ";
      else if ((String(modifier_state[i]).toUpperCase() === 'ANYCONTROL') || (String(modifier_state[i]).toUpperCase() === 'CONTROL')) add_modifier = "CTRL ";

      else add_modifier = String(modifier_state[i]) + " "
      kmn_modifier += kmn_ncaps + add_modifier
    }

    // remove duplicate and empty entries
    const duplicate_modifier_array: string[] = kmn_modifier.split(" ").filter(item => item)
    const unique_Modifier: string[] = duplicate_modifier_array.filter(function (item, pos, self) {
      return self.indexOf(item) == pos;
    })

    //ToDo review lower part
    const unique_Modifier_string = unique_Modifier.join(" ").replace(/\s+/g, " ").trim()

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
    //return unique_Modifier.join(" ").replace(/\s+/g, " ").trim().toUpperCase()
    //return unique_Modifier.join(" ").replace(/\s+/g, " ").trim()
    //return unique_Modifier_string
  }

  public checkIfCapsUsed(keylayout_modifier: string[][]): boolean {
    return keylayout_modifier.flat().flat().includes("caps")
  }
  /*
    public replaceModifier(keylayout_modifier: string): string {
  
      const modifier_array: string[] = keylayout_modifier.split(" ");
  
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
  */

  public isAcceptableKeymanModifier(keylayout_modifier: string): boolean {
    let iskKeymanModifier = true
    const modifier_single: string[] = keylayout_modifier.split(" ");

    for (let i = 0; i < modifier_single.length; i++) {
      if (
        (modifier_single[i].toUpperCase() === "NCAPS")
        || (modifier_single[i].toUpperCase() === "SHIFT")
        || (modifier_single[i].toUpperCase() === "CAPS")
        || (modifier_single[i].toUpperCase() === "RALT")
        || (modifier_single[i].toUpperCase() === "CTRL")
        || (modifier_single[i].toUpperCase() === "")

      ) { iskKeymanModifier = iskKeymanModifier && true }
      else { iskKeymanModifier = iskKeymanModifier && false }
    }
    return iskKeymanModifier
  }


  public findDuplicateRule_allC(uniqueDeadkeyRules: any[], index: number, rule_type: string, isCapsused: boolean): string {

    for (let i = 0; i < index - 1; i++) {
      if (
        (((rule_type === "C014"))
          //&& this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_key, true) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_key, true)
          && uniqueDeadkeyRules[i].modifier_key === uniqueDeadkeyRules[index].modifier_key
          && uniqueDeadkeyRules[i].key === uniqueDeadkeyRules[index].key
          && new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) === new TextDecoder().decode(uniqueDeadkeyRules[index].deadkeyed_Ch))

        || (rule_type === "C2"
          && this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_key, isCapsused) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_key, isCapsused)
          && uniqueDeadkeyRules[i].key === uniqueDeadkeyRules[index].key
          && this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_deadkey, isCapsused) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_deadkey, isCapsused)
          && uniqueDeadkeyRules[i].deadkey === uniqueDeadkeyRules[index].deadkey
          && new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) === new TextDecoder().decode(uniqueDeadkeyRules[index].deadkeyed_Ch)
        )

        || (rule_type === "C3"
          && this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_key, isCapsused) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_key, isCapsused)
          && uniqueDeadkeyRules[i].key === uniqueDeadkeyRules[index].key
          && this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_deadkey, isCapsused) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_deadkey, isCapsused)
          && uniqueDeadkeyRules[i].deadkey === uniqueDeadkeyRules[index].deadkey
          && this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_prev_deadkey, isCapsused) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_prev_deadkey, isCapsused)
          && uniqueDeadkeyRules[i].prev_deadkey === uniqueDeadkeyRules[index].prev_deadkey
          && new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) === new TextDecoder().decode(uniqueDeadkeyRules[index].deadkeyed_Ch)
        )
      )
        return ("duplicate Rule: [" +

          this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_prev_deadkey, true) + " " + uniqueDeadkeyRules[i].prev_deadkey + "]  >  XXX [" +
          this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_deadkey, true) + " " + uniqueDeadkeyRules[i].deadkey + "]  >  YYY  [" +
          this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_key, true) + " " + uniqueDeadkeyRules[i].key +
          "]  °°->  \'" + new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) + "\'  <--> ")
    }
    return ""
  }


  public findAmbiguousRule_allC(uniqueDeadkeyRules: any[], index: number, rule_type: string, isCapsused: boolean): string {

    for (let i = 0; i < index - 1; i++) {
      if (
        (
          (rule_type === "C014")
          && this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_key, isCapsused) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_key, isCapsused)
          && uniqueDeadkeyRules[i].key === uniqueDeadkeyRules[index].key &&
          new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) !== new TextDecoder().decode(uniqueDeadkeyRules[index].deadkeyed_Ch))

        || (
          rule_type === "C2"
          && this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_key, isCapsused) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_key, isCapsused)
          && uniqueDeadkeyRules[i].key === uniqueDeadkeyRules[index].key
          && this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_deadkey, isCapsused) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_deadkey, isCapsused)
          && uniqueDeadkeyRules[i].deadkey === uniqueDeadkeyRules[index].deadkey
          && new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) !== new TextDecoder().decode(uniqueDeadkeyRules[index].deadkeyed_Ch)
        )

        || (
          rule_type === "C3"
          && this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_key, isCapsused) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_key, isCapsused)
          && uniqueDeadkeyRules[i].key === uniqueDeadkeyRules[index].key
          && this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_deadkey, isCapsused) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_deadkey, isCapsused)
          && uniqueDeadkeyRules[i].deadkey === uniqueDeadkeyRules[index].deadkey
          && this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_prev_deadkey, isCapsused) === this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_prev_deadkey, isCapsused)
          && uniqueDeadkeyRules[i].prev_deadkey === uniqueDeadkeyRules[index].prev_deadkey
          && new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) !== new TextDecoder().decode(uniqueDeadkeyRules[index].deadkeyed_Ch)
        )
      )
        // return ("ambiguous Rule: [" + uniqueDeadkeyRules[i].modifier_key + " " + uniqueDeadkeyRules[i].key + "] > " + new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) + "  <--> ")
        return ("ambiguous Rule: [" +
          this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_prev_deadkey, isCapsused) + " " + uniqueDeadkeyRules[i].prev_deadkey + "] | [" +
          this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_deadkey, isCapsused) + " " + uniqueDeadkeyRules[i].deadkey + "]  >  XXX [" +
          this.create_kmn_modifier(uniqueDeadkeyRules[i].modifier_key, isCapsused) + " " + uniqueDeadkeyRules[i].key +
          "]  °°->  \'" + new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) + "\'  <--> ")
    }
    return ""
  }

  public findUnAvailableRule(uniqueDeadkeyRules: any[], index: number, isCapsused: boolean): string {
    if (
      (((uniqueDeadkeyRules[index].rule_type === "C0") || (uniqueDeadkeyRules[index].rule_type === "C1") || (uniqueDeadkeyRules[index].rule_type === "C4"))
        && (this.isAcceptableKeymanModifier(this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_key, isCapsused))))

      || (uniqueDeadkeyRules[index].rule_type === "C2"
        && ((this.isAcceptableKeymanModifier(this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_deadkey, isCapsused)))
          && (this.isAcceptableKeymanModifier(this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_key, isCapsused))))
      )

      || (uniqueDeadkeyRules[index].rule_type === "C3"
        && ((this.isAcceptableKeymanModifier(this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_prev_deadkey, isCapsused)))
          && (this.isAcceptableKeymanModifier(this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_deadkey, isCapsused)))
          && (this.isAcceptableKeymanModifier(this.create_kmn_modifier(uniqueDeadkeyRules[index].modifier_key, isCapsused))))
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


  public createData_Deadkeys(data_ukelele: convert_object): string {

    let data: string = ""
    // filter for dk values
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (KeylayoutToKmnConverter.print_draft) {
      data += '\########## OK #################################################################\n'
      data += '\nNOW MY C4 RULES **ccc ********* (only to get 02C6 ect) *********************\n\n'
    }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    const DeadkeyRules = data_ukelele.ArrayOf_Rules.filter((curr) => {
      if (curr.isTerminator === true) {
        return curr;
      }
      return ""
    });

    // remove duplicates
    const uniqueDeadkeyRules = DeadkeyRules.reduce((unique, o) => {
      if (!unique.some((obj: { deadkeyed_Ch: any; rule_type: any; modifier_key: any; key: any }) =>
        new TextDecoder().decode(obj.deadkeyed_Ch) === new TextDecoder().decode(o.deadkeyed_Ch)
        && obj.key === o.key
        && obj.rule_type === o.rule_type
        && obj.modifier_key === o.modifier_key)
      ) {
        unique.push(o);
      }
      return unique;
    }, []);

    for (let i = 0; i < uniqueDeadkeyRules.length; i++) {
      let line: string = ""
      line = "+  [" + uniqueDeadkeyRules[i].modifier_key + " " + uniqueDeadkeyRules[i].key + "]  >   dxk(" + this.getHexFromChar(new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch)) + ")"
      data += line + '\n'
    }


    //*************************************************************************************************** */
    /*
    How to create rules from keylayout data from data_ukelele.ArrayOf_processed_RuleData:

    There are 6 different types of data entries (C0-C4) in data_ukelele.ArrayOf_processed_RuleData e.g. that contain all data to retrieve deadkeys, deadkeyables and deadkeyed characters: 

    C0: (size: 4) ['C0', 'RIGHTSHIFT ', 'K_S',     'S' ]
    C1: (size: 4) ['C1', 'CAPS',        'K_A',     'A' ],
    C2: (size: 5) ['C2', 'CAPS',        'K_A',     'K_N',         'Ã' ],                           use 2.(K_A)  and 4.('Ã') to get name and DEADKEYED
    C3: (size: 6) ['C3', 'NCAPS RALT',  'K_8',     'NCAPS RALT',  'K_U',          'ˆ' ],
    C4: (size: 4) ['C4', 'NCAPS 0',     'K_SPACE', '' ],
    C4: (size: 7) ['C4', 'NCAPS RALT',  'K_N',     '˜',           'isTerminator', '˜', '02DC '],   use 2.(K_N) , 5. (isTerminator) and 6.('02DC') to get key name and DEADKEY in hex
    
    To Find a DEADKEY e.g. ^ 
      1) look in modifier_C4 with 'isTerminator' included (n = 7)
      2) get 7th entry ===> DEADKEY (e.g. 02DC)

    To Find a DEADKEYABLE ( a character that can be converted to a deadkey e.g. A (-> Ã) ):
      1) C1    CAPS   K_A   A
      2) C2    CAPS   K_A   K_N   Ã
        - Find C2 rule with 5 entries (Deadkeyed)           (e.g. C2   CAPS   K_A    K_N   Ã)
        - get C1 rule with same second and third entries -> (e.g. C1   CAPS   K_A)
        - get 2nd entry of C1 rule ===> DEADKEYABLE (e.g. A)
    
    To Find a DEADKEYED ( a character that has been converted from another character e.g. Ã ):
      1) look in modifier_C2  CAPS   K_A   K_N   Ã 
      2) get 3rd entry (='K_A')
      3) get 5th entry ===> DEADKEYED (='Ã')
  */


    // select only lines for deadkeyables ( entries that contain C1, are not K_SPACE and create chars e.g. from 'C1', 'CAPS', 'K_A',  'A' ])
    const deadkeyables_raw = data_ukelele.ArrayOf_Rules.filter((curr) => {
      if ((curr.rule_type === "C1") && (curr.key !== "K_SPACE") && (new TextDecoder().decode(curr.deadkeyed_Ch) !== "")) {
        return curr;
      }
      return ""
    });

    // select only lines for deadkeyed ( entries that contain C2, are not K_SPACE and create a deadkeyed) e.g. from ['C2', 'CAPS', 'K_A', 'K_N', 'Ã' ]
    const deadkeyed_raw_multiple = data_ukelele.ArrayOf_Rules.filter((curr) => {
      // if ((curr.rule_type === "C2") && (curr.key !== "K_SPACE") && (new TextDecoder().decode(curr.deadkeyed_Ch) !== "")) {
      // if ((curr.rule_type === "C2") && (curr.key !== "K_SPACE")) {
      if ((curr.rule_type === "C2")) {
        return curr;
      }
      return ""
    });

    // remove duplicates
    const deadkeyed_raw = deadkeyed_raw_multiple.reduce((unique, o) => {
      if (!unique.some((obj: { rule_type: any; modifier_key: any; key: any; deadkeyed_Ch: any }) =>
        new TextDecoder().decode(obj.deadkeyed_Ch) === new TextDecoder().decode(o.deadkeyed_Ch)
        && obj.key === o.key
        && obj.rule_type === o.rule_type
        && obj.modifier_key === o.modifier_key)
      ) {
        unique.push(o);
      }
      return unique;
    }, []);

    const dk_array_comlpete: any[] = []

    // create 2D array with data of both deadkeyables_raw, deadkeyed_raw and remove possible duplicates
    for (let rr = 0; rr < deadkeyables_raw.length; rr++) {
      // loop deadkeyed_raw, take Keyname
      for (let ss = 0; ss < deadkeyed_raw.length; ss++) {
        const dk_array: any[] = []
        //if available ( if both have the same shiftstate and keyname)
        if ((deadkeyables_raw[rr].modifier_key === deadkeyed_raw[ss].modifier_key) &&
          ((deadkeyables_raw[rr].key === deadkeyed_raw[ss].key))) {

          dk_array.push(new TextDecoder().decode(deadkeyables_raw[rr].deadkeyed_Ch))  // deadkeyable e.g.  A
          dk_array.push(deadkeyed_raw[ss].deadkey)      // Keyname dk  e.g.  K_N
          dk_array.push(new TextDecoder().decode(deadkeyed_raw[ss].deadkeyed_Ch))     // deadkeyed   e.g.  Ã
        }
        if (dk_array.length > 0)
          dk_array_comlpete.push(dk_array)
      }
    }

    const unique_dk_array_comlpete = dk_array_comlpete.reduce((acc, curr) => {
      const [uniq, set] = acc;
      if (!set.has(curr.join(','))) {
        set.add(curr.join(','));
        uniq.push(curr);
      }
      return acc;
    }, [[], new Set()],
    );


    const deadkeyedElement: string[][] = Array.from({ length: uniqueDeadkeyRules.length }, () => new Array(2).fill(''));
    const deadkeyablesElement: string[][] = Array.from({ length: uniqueDeadkeyRules.length }, () => new Array(1).fill(''));


    // why is there a set entry???? deadkeyables_raw[0]
    for (let i = 0; i < uniqueDeadkeyRules.length; i++) {
      for (let j = 0; j < unique_dk_array_comlpete[0].length; j++) {

        if (uniqueDeadkeyRules[i].key === unique_dk_array_comlpete[0][j][1]) {
          deadkeyablesElement[i][0] = deadkeyablesElement[i][0] + "\'" + unique_dk_array_comlpete[0][j][0] + "\'  "
          deadkeyedElement[i][0] = deadkeyedElement[i][0] + "\'" + unique_dk_array_comlpete[0][j][2] + "\'  "
        }
        deadkeyedElement[i][1] = this.getHexFromChar(new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch))
      }
    }


    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (KeylayoutToKmnConverter.print_draft) {

      data += "\n"
      data += '\########## OK #################################################################\n'

      data += '\nmatch > use(deadkeys)\n\n'
      data += '\ngroup(deadkeys)\n\n'

      //for (let i = 0; i < newAA.length; i++) {
      for (let i = 0; i < deadkeyablesElement.length; i++) {
        data += "\n\ndk: " + deadkeyedElement[i][1]
        data += "\ndeadkeyablesArray xx" + deadkeyablesElement[i][0]
        data += "\ndeadkeyedArray xx   " + deadkeyedElement[i][0]
        /*data += "\n\ndk: " + newAA[i][0]
        data += "\ndeadkeyablesArray xx" + newAA[i][1]
        data += "\ndeadkeyedArray xx   " + newAA[i][2]*/
      }
    }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else {
      data += '\nmatch > use(deadkeys)\n\n'
      data += '\ngroup(deadkeys)\n\n'

      //for (let i = 0; i < newAA.length; i++) {
      for (let i = 0; i < deadkeyablesElement.length; i++) {
        data += "\n\ndk: " + deadkeyedElement[i][1]
        data += "\ndeadkeyablesArray xx" + deadkeyablesElement[i][0]
        data += "\ndeadkeyedArray xx   " + deadkeyedElement[i][0]
        /*data += "\n\ndk: " + newAA[i][0]
        data += "\ndeadkeyablesArray xx" + newAA[i][1]
        data += "\ndeadkeyedArray xx   " + newAA[i][2]*/
      }
    }
    return data
  }

  public createData_Rules(data_ukelele: convert_object): string {
    const maxkey = 50
    let data: string = ""
    let keymarker = ""


    // select only lines that contain C0 or C1 and create an output.
    const data_C0_C1 = data_ukelele.ArrayOf_Rules.filter((curr) => {
      if ((curr.deadkeyed_Ch !== new TextEncoder().encode("") || curr.deadkeyed_Ch !== undefined)
        && (curr.rule_type === "C0") || (curr.rule_type === "C1")) {
        return curr;
      }
      else return ""
    });
    // Also remove duplicates C014 types
    const uniqueDeadkeyRules = data_C0_C1.reduce((unique, o) => {
      if (!unique.some((obj: { deadkeyed_Ch: any; rule_type: any; modifier_key: any; key: any }) =>
        new TextDecoder().decode(obj.deadkeyed_Ch) === new TextDecoder().decode(o.deadkeyed_Ch)
        && obj.key === o.key
        && obj.rule_type === o.rule_type
        && obj.modifier_key === o.modifier_key)
      ) {
        unique.push(o);
      }
      return unique;
    }, []);


    /*
       const data_C2_C3 = data_ukelele.ArrayOf_Rules.filter((curr) => {
         if ((curr.deadkeyed_Ch !== new TextEncoder().encode("") || curr.deadkeyed_Ch !== undefined)
           && (curr.rule_type === "C2") || (curr.rule_type === "C3")) {
           return curr;
         }
         else return ""
       });
     //  console.log("data_C2_C3 ", data_C2_C3)
   */

    const data_C2 = data_ukelele.ArrayOf_Rules.filter((curr) => {
      if ((curr.deadkeyed_Ch !== new TextEncoder().encode("") || curr.deadkeyed_Ch !== undefined)
        && (curr.rule_type === "C2")) {
        return curr;
      }
      else return ""
    });// Also remove duplicates C014 types

    // ToDo: true or false for create_kmn_modifier ??? where to gt true/false from?
    const uniqueC2Rules = data_C2.reduce((unique, o) => {
      if (!unique.some((obj: { deadkeyed_Ch: any; rule_type: any; modifier_key: any; modifier_deadkey: string; deadkey: any; key: any }) =>
        new TextDecoder().decode(obj.deadkeyed_Ch) === new TextDecoder().decode(o.deadkeyed_Ch)
        && obj.rule_type === o.rule_type

        && obj.rule_type === o.rule_type
        && this.create_kmn_modifier(obj.modifier_key, false) === this.create_kmn_modifier(o.modifier_key, false)

        && this.create_kmn_modifier(obj.modifier_deadkey, false) === this.create_kmn_modifier(o.modifier_deadkey, false)
        && obj.deadkey === o.deadkey
      )
      ) {
        unique.push(o);
      }
      return unique;
    }, []);
    /*
        /// console log all entries
        for(let i=0;i<uniqueC2Rules.length;i++ ) {
          console.log("uniqueC2Rules ", uniqueC2Rules.length, i,
       
           uniqueC2Rules[i].rule_type !== "" ? uniqueC2Rules[i].rule_type : "--".padEnd(4, " "),
       
       
           "|  ", (uniqueC2Rules[i].modifier_prev_deadkey !== "" ? uniqueC2Rules[i].modifier_prev_deadkey.padEnd(33, " ") : "--".padEnd(33, " ")),
           (uniqueC2Rules[i].prev_deadkey !== "" ? uniqueC2Rules[i].prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
           (uniqueC2Rules[i].dk_prev !== 0 ? ("dk(A" + String(uniqueC2Rules[i].dk_prev) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),
       
       
           (uniqueC2Rules[i].modifier_deadkey !== "" ? uniqueC2Rules[i].modifier_deadkey.padEnd(30, " ") : "--".padEnd(30, " ")),
           (uniqueC2Rules[i].deadkey !== "" ? uniqueC2Rules[i].deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
           (uniqueC2Rules[i].dk !== 0 ? ("dk(B" + String(uniqueC2Rules[i].dk) + ")").padEnd(9, " ") : "--".padEnd(9, " ")),
       
       
           "|  ", (uniqueC2Rules[i].modifier_key !== "" ? uniqueC2Rules[i].modifier_key.padEnd(40, " ") : "--".padEnd(40, " ")),
           (uniqueC2Rules[i].key !== "" ? uniqueC2Rules[i].key.padEnd(8, " ") : "--".padEnd(8, " ")),
           (new TextDecoder().decode(uniqueC2Rules[i].deadkeyed_Ch) !== "" ? new TextDecoder().decode(uniqueC2Rules[i].deadkeyed_Ch).padEnd(10, " ") : "--".padEnd(10, " ")),
       
           "| °°")
          }
    */

    /// console log all entries
    for (let i = 0; i < data_C2.length; i++) {
      console.log("data_C2 ", data_C2.length, i,

        data_C2[i].rule_type !== "" ? data_C2[i].rule_type : "--".padEnd(4, " "),


        "|  ", (data_C2[i].modifier_prev_deadkey !== "" ? data_C2[i].modifier_prev_deadkey.padEnd(33, " ") : "--".padEnd(33, " ")),
        (data_C2[i].prev_deadkey !== "" ? data_C2[i].prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        (data_C2[i].dk_prev !== 0 ? ("dk(A" + String(data_C2[i].dk_prev) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),


        (data_C2[i].modifier_deadkey !== "" ? data_C2[i].modifier_deadkey.padEnd(30, " ") : "--".padEnd(30, " ")),
        (data_C2[i].deadkey !== "" ? data_C2[i].deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        (data_C2[i].dk !== 0 ? ("dk(B" + String(data_C2[i].dk) + ")").padEnd(9, " ") : "--".padEnd(9, " ")),


        "|  ", (data_C2[i].modifier_key !== "" ? data_C2[i].modifier_key.padEnd(40, " ") : "--".padEnd(40, " ")),
        (data_C2[i].key !== "" ? data_C2[i].key.padEnd(8, " ") : "--".padEnd(8, " ")),
        (new TextDecoder().decode(data_C2[i].deadkeyed_Ch) !== "" ? new TextDecoder().decode(data_C2[i].deadkeyed_Ch).padEnd(10, " ") : "--".padEnd(10, " ")),

        "| °°")
    }

    const data_C3 = data_ukelele.ArrayOf_Rules.filter((curr) => {
      if ((curr.deadkeyed_Ch !== new TextEncoder().encode("") || curr.deadkeyed_Ch !== undefined)
        && (curr.rule_type === "C3")) {
        return curr;
      }
      else return ""
    });
    // ToDo: true or false for create_kmn_modifier ??? where to gt true/false from?
    const uniqueC3Rules = data_C3.reduce((unique, o) => {
      if (!unique.some((obj: { deadkeyed_Ch: any; rule_type: any; modifier_key: any; modifier_deadkey: string; prev_deadkey: string; modifier_prev_deadkey: string; deadkey: any; key: any }) =>
        new TextDecoder().decode(obj.deadkeyed_Ch) === new TextDecoder().decode(o.deadkeyed_Ch)
        && obj.key === o.key

        && obj.rule_type === o.rule_type
        && this.create_kmn_modifier(obj.modifier_key, false) === this.create_kmn_modifier(o.modifier_key, false)

        && this.create_kmn_modifier(obj.modifier_deadkey, false) === this.create_kmn_modifier(o.modifier_deadkey, false)
        && obj.deadkey === o.deadkey

        && this.create_kmn_modifier(obj.modifier_prev_deadkey, false) === this.create_kmn_modifier(o.modifier_prev_deadkey, false)
        && obj.prev_deadkey === o.prev_deadkey)
      ) {
        unique.push(o);
      }
      return unique;
    }, []);
    // console.log("data_C2_C3 ", data_C3, uniqueC3Rules.length)





    for (let i = 0; i < uniqueC3Rules.length; i++) {
      console.log("uniqueC3Rules ", uniqueC3Rules.length, i,

        uniqueC3Rules[i].rule_type !== "" ? uniqueC3Rules[i].rule_type : "--".padEnd(4, " "),


        "|  ", (uniqueC3Rules[i].modifier_prev_deadkey !== "" ? uniqueC3Rules[i].modifier_prev_deadkey.padEnd(33, " ") : "--".padEnd(33, " ")),
        (uniqueC3Rules[i].prev_deadkey !== "" ? uniqueC3Rules[i].prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        (uniqueC3Rules[i].dk_prev !== 0 ? ("dk(A" + String(uniqueC3Rules[i].dk_prev) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),


        (uniqueC3Rules[i].modifier_deadkey !== "" ? uniqueC3Rules[i].modifier_deadkey.padEnd(30, " ") : "--".padEnd(30, " ")),
        (uniqueC3Rules[i].deadkey !== "" ? uniqueC3Rules[i].deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        (uniqueC3Rules[i].dk !== 0 ? ("dk(B" + String(uniqueC3Rules[i].dk) + ")").padEnd(9, " ") : "--".padEnd(9, " ")),


        "|  ", (uniqueC3Rules[i].modifier_key !== "" ? uniqueC3Rules[i].modifier_key.padEnd(40, " ") : "--".padEnd(40, " ")),
        (uniqueC3Rules[i].key !== "" ? uniqueC3Rules[i].key.padEnd(8, " ") : "--".padEnd(8, " ")),
        (new TextDecoder().decode(uniqueC3Rules[i].deadkeyed_Ch) !== "" ? new TextDecoder().decode(uniqueC3Rules[i].deadkeyed_Ch).padEnd(10, " ") : "--".padEnd(10, " ")),

        "| °°")
    }

    const data_All_C = data_ukelele.ArrayOf_Rules.filter((curr) => {
      if ((curr.deadkeyed_Ch !== new TextEncoder().encode("") || curr.deadkeyed_Ch !== undefined)
        && ((curr.rule_type === "C0") || (curr.rule_type === "C1") || (curr.rule_type === "C2") || (curr.rule_type === "C3") || (curr.rule_type === "C4"))) {
        return curr;
      }
      else return ""
    });
    //  console.log("data_All_C ", data_All_C)


    /* // Also remove duplicates
     const uniqueDeadkeys_obj_C2_C3 = data_C2_C3.reduce((unique, o) => {
       if (!unique.some((obj: { rule_type: any; modifier_prev_deadkey: string; prev_deadkey: string; modifier_deadkey: string; deadkey: string; modifier_key: any; key: any }) =>
 
         obj.rule_type === o.rule_type
         && obj.modifier_prev_deadkey === o.modifier_prev_deadkey
         && obj.prev_deadkey === o.prev_deadkey
 
         && obj.modifier_deadkey === o.modifier_deadkey
         && obj.deadkey === o.deadkey
 
         && obj.modifier_key === o.modifier_key
         && obj.key === o.key
       )
       ) {
         unique.push(o);
       }
       return unique;
     }, []);
     console.log("uniqueDeadkeys_obj_C2_C3 ", uniqueDeadkeys_obj_C2_C3)*/

    // Also remove duplicates
  /*  const uniqueDeadkeys_obj_All_C = data_All_C.reduce((unique, o) => {
      if (!unique.some((obj: { deadkeyed_Ch: Uint8Array; rule_type: any; modifier_prev_deadkey: string; prev_deadkey: string; modifier_deadkey: string; deadkey: string; modifier_key: any; key: any }) =>

        obj.rule_type === o.rule_type
        && obj.modifier_prev_deadkey === o.modifier_prev_deadkey
        && obj.prev_deadkey === o.prev_deadkey

        && obj.modifier_deadkey === o.modifier_deadkey
        && obj.deadkey === o.deadkey

        && obj.modifier_key === o.modifier_key
        && obj.key === o.key

        && new TextDecoder().decode(obj.deadkeyed_Ch) === new TextDecoder().decode(o.deadkeyed_Ch)
      )
      ) {
        unique.push(o);
      }
      return unique;
    }, []);*/

    /*console.log(" uniqueDeadkeys_obj_All_C.leng ", data_All_C.length, "--", uniqueDeadkeys_obj_All_C.length)
    for (let i = 0; i < uniqueDeadkeys_obj_All_C.length; i++) {

      console.log("uniqueDeadkeys_obj_All_C ", uniqueDeadkeys_obj_All_C.length, i,

        uniqueDeadkeys_obj_All_C[i].rule_type !== "" ? uniqueDeadkeys_obj_All_C[i].rule_type : "--".padEnd(4, " "),


        "|  ", (uniqueDeadkeys_obj_All_C[i].modifier_prev_deadkey !== "" ? uniqueDeadkeys_obj_All_C[i].modifier_prev_deadkey.padEnd(33, " ") : "--".padEnd(33, " ")),
        (uniqueDeadkeys_obj_All_C[i].prev_deadkey !== "" ? uniqueDeadkeys_obj_All_C[i].prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        (uniqueDeadkeys_obj_All_C[i].dk_prev !== 0 ? ("dk(A" + String(uniqueDeadkeys_obj_All_C[i].dk_prev) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),


        (uniqueDeadkeys_obj_All_C[i].modifier_deadkey !== "" ? uniqueDeadkeys_obj_All_C[i].modifier_deadkey.padEnd(30, " ") : "--".padEnd(30, " ")),
        (uniqueDeadkeys_obj_All_C[i].deadkey !== "" ? uniqueDeadkeys_obj_All_C[i].deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        (uniqueDeadkeys_obj_All_C[i].dk !== 0 ? ("dk(B" + String(uniqueDeadkeys_obj_All_C[i].dk) + ")").padEnd(9, " ") : "--".padEnd(9, " ")),


        "|  ", (uniqueDeadkeys_obj_All_C[i].modifier_key !== "" ? uniqueDeadkeys_obj_All_C[i].modifier_key.padEnd(40, " ") : "--".padEnd(40, " ")),
        (uniqueDeadkeys_obj_All_C[i].key !== "" ? uniqueDeadkeys_obj_All_C[i].key.padEnd(8, " ") : "--".padEnd(8, " ")),
        (new TextDecoder().decode(uniqueDeadkeys_obj_All_C[i].deadkeyed_Ch) !== "" ? new TextDecoder().decode(uniqueDeadkeys_obj_All_C[i].deadkeyed_Ch).padEnd(10, " ") : "--".padEnd(10, " ")),

        "| °°")

    }**/
/*
    for (let i = 0; i < data_All_C.length; i++) {

      console.log("data_All_C ", data_All_C.length, i,

        data_All_C[i].rule_type !== "" ? data_All_C[i].rule_type : "--".padEnd(4, " "),


        "|  ", (data_All_C[i].modifier_prev_deadkey !== "" ? data_All_C[i].modifier_prev_deadkey.padEnd(33, " ") : "--".padEnd(33, " ")),
        (data_All_C[i].prev_deadkey !== "" ? data_All_C[i].prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        (data_All_C[i].dk_prev !== 0 ? ("dk(A" + String(data_All_C[i].dk_prev) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),


        (data_All_C[i].modifier_deadkey !== "" ? data_All_C[i].modifier_deadkey.padEnd(30, " ") : "--".padEnd(30, " ")),
        (data_All_C[i].deadkey !== "" ? data_All_C[i].deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
        (data_All_C[i].dk !== 0 ? ("dk(B" + String(data_All_C[i].dk) + ")").padEnd(9, " ") : "--".padEnd(9, " ")),


        "|  ", (data_All_C[i].modifier_key !== "" ? data_All_C[i].modifier_key.padEnd(40, " ") : "--".padEnd(40, " ")),
        (data_All_C[i].key !== "" ? data_All_C[i].key.padEnd(8, " ") : "--".padEnd(8, " ")),
        (new TextDecoder().decode(data_All_C[i].deadkeyed_Ch) !== "" ? new TextDecoder().decode(data_All_C[i].deadkeyed_Ch).padEnd(10, " ") : "--".padEnd(10, " ")),

        "| °°")

    }



*/






    const isCapsused = this.checkIfCapsUsed(data_ukelele.ArrayOf_Modifiers)
    for (let i = 0; i < uniqueDeadkeyRules.length; i++) {

      // lookup key nr of the key that is being processed
      let keyNr = 0;
      for (let j = 0; j < maxkey; j++) {
        if (this.map_UkeleleKC_To_VK(j) === uniqueDeadkeyRules[i].key)
          keyNr = j
      }

      // skip keyNr 48 ( TAB )
      if (keyNr === 48)
        continue

      // add a line after rules of each key
      if (uniqueDeadkeyRules[i].key !== keymarker)
        data += '\n'

      let warningtext = "c C0 WARNING: "
      warningtext = warningtext + this.findDuplicateRule_allC(uniqueDeadkeyRules, i, "C014", isCapsused)    // OK
      warningtext = warningtext + this.findAmbiguousRule_allC(uniqueDeadkeyRules, i, "C014", isCapsused)   // OK
      warningtext = warningtext + this.findUnAvailableRule(uniqueDeadkeyRules, i, isCapsused)    // OK

      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (KeylayoutToKmnConverter.print_draft) {
        if (warningtext === "c C0 WARNING: ")
          data += keyNr + "-(modif:" + uniqueDeadkeyRules[i].rule_type + "-" + "i" + `) + [` + (uniqueDeadkeyRules[i].modifier_key + ' ' + uniqueDeadkeyRules[i].key).trim() + `] °°-> \'` + new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) + '\'\n'
        else
          data += warningtext + keyNr + "-(modif:" + uniqueDeadkeyRules[i].rule_type + "-" + "i" + `) + [` + (uniqueDeadkeyRules[i].modifier_key + ' ' + uniqueDeadkeyRules[i].key).trim() + `] **°°-> \'` + new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) + '\'\n'
      }
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      else {
        if (warningtext === "c C0 WARNING: ")
          data += "[" + (uniqueDeadkeyRules[i].modifier_key + ' ' + uniqueDeadkeyRules[i].key).trim() + `]  > \'` + new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) + '\'\n'
        else
          data += warningtext + "[" + (uniqueDeadkeyRules[i].modifier_key + ' ' + uniqueDeadkeyRules[i].key).trim() + `]  > \'` + new TextDecoder().decode(uniqueDeadkeyRules[i].deadkeyed_Ch) + '\'\n'
      }

      keymarker = uniqueDeadkeyRules[i].key
    }


    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (KeylayoutToKmnConverter.print_draft) {
      data += "\n########## C2 #################################################################\n"
    }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   // console.log("XXXXYYYZZc2")
    for (let k = 0; k < uniqueC2Rules.length; k++) {

      if ((uniqueC2Rules[k].rule_type === "C2")) {

        let warningtext = "c C2 WARNING: "//uniqueC3Rules
        warningtext = warningtext + this.findDuplicateRule_allC(uniqueC2Rules, k, "C2", isCapsused)  // OK
        warningtext = warningtext + this.findAmbiguousRule_allC(uniqueC2Rules, k, "C2", isCapsused)  // OK
        warningtext = warningtext + this.findUnAvailableRule(uniqueC2Rules, k, isCapsused)  // OK 

        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (KeylayoutToKmnConverter.print_draft) {
          if (warningtext === "c C2 WARNING: ") {
            /*  data += " [" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC2Rules[k].deadkey + "]  >  dk(C" + String(uniqueC2Rules[k].dk_C2) + ")\n"
              data += " dk(C" + String(uniqueC2Rules[k].dk_C2) + ") [" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_key, isCapsused) + " " + uniqueC2Rules[k].key + "]  °°->  \'" + new TextDecoder().decode(uniqueC2Rules[k].deadkeyed_Ch) + "\'\n"
             */
            data += " [" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC2Rules[k].deadkey + "]  >  XXX" +
              " [" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_key, isCapsused) + " " + uniqueC2Rules[k].key + "]  °°->  \'" + new TextDecoder().decode(uniqueC2Rules[k].deadkeyed_Ch) + "\'"
          }
          else {
            /* data += warningtext + " [" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC2Rules[k].deadkey + "]  >  dk(C" + String(uniqueC2Rules[k].dk_C2) + ")\n"
             data += warningtext + " dk(C" + String(uniqueC2Rules[k].dk_C2) + ") [" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_key, isCapsused) + " " + uniqueC2Rules[k].key + "]   >  \'" + new TextDecoder().decode(uniqueC2Rules[k].deadkeyed_Ch) + "\'\n"
            */
            data += warningtext + " [" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC2Rules[k].deadkey + "]  >  XXX" +
              "[" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_key, isCapsused) + " " + uniqueC2Rules[k].key + "]   >  \'" + new TextDecoder().decode(uniqueC2Rules[k].deadkeyed_Ch) + "\'"
          }
        }
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        else {
          if (warningtext === "c C2 WARNING: ") {
            data += " [" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC2Rules[k].deadkey + "]  >  dk(C" + String(uniqueC2Rules[k].dk_C2) + ")\n"
            data += " dk(C" + String(uniqueC2Rules[k].dk_C2) + ") [" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_key, isCapsused) + " " + uniqueC2Rules[k].key + "]  °°->  \'" + new TextDecoder().decode(uniqueC2Rules[k].deadkeyed_Ch) + "\'\n"
          }
          else {
            data += warningtext + " [" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC2Rules[k].deadkey + "]  >  dk(C" + String(uniqueC2Rules[k].dk_C2) + ")\n"
            data += warningtext + " dk(C" + String(uniqueC2Rules[k].dk_C2) + ") [" + this.create_kmn_modifier(uniqueC2Rules[k].modifier_key, isCapsused) + " " + uniqueC2Rules[k].key + "]   >  \'" + new TextDecoder().decode(uniqueC2Rules[k].deadkeyed_Ch) + "\'\n"
          }
        }
        data += "\n"
      }
    }

    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (KeylayoutToKmnConverter.print_draft) {
      data += "\n########## C3 #################################################################\n"
    }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    for (let k = 0; k < uniqueC3Rules.length; k++) {
      if ((uniqueC3Rules[k].rule_type === "C3")) {

        let warningtext = "c C3 WARNING: "
        warningtext = warningtext + this.findDuplicateRule_allC(uniqueC3Rules, k, "C3", isCapsused)    // OK
        warningtext = warningtext + this.findAmbiguousRule_allC(uniqueC3Rules, k, "C3", isCapsused)     // OK
        warningtext = warningtext + this.findUnAvailableRule(uniqueC3Rules, k, isCapsused)    // OK

        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (KeylayoutToKmnConverter.print_draft) {
          if (warningtext === "c C3 WARNING: ") {
            /* data += " [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_prev_deadkey, isCapsused) + " " + uniqueC3Rules[k].prev_deadkey + "]   >   dk(A" + String(uniqueC3Rules[k].dk_prev) + ")\n"
             data += " dk(A" + String(uniqueC3Rules[k].dk_prev) + ")  + [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC3Rules[k].deadkey + "]  >  dk(B" + String(uniqueC3Rules[k].dk) + ")\n"
             data += " dk(B" + String(uniqueC3Rules[k].dk) + ") [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_key, isCapsused) + " " + uniqueC3Rules[k].key + "]  °°->  \'" + new TextDecoder().decode(uniqueC3Rules[k].deadkeyed_Ch) + "\'\n"
              */
            data += " [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_prev_deadkey, isCapsused) + " " + uniqueC3Rules[k].prev_deadkey + "]   >   XXX  " +
              "[" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC3Rules[k].deadkey + "]  >  YYY " +
              " [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_key, isCapsused) + " " + uniqueC3Rules[k].key + "]  °°->  \'" + new TextDecoder().decode(uniqueC3Rules[k].deadkeyed_Ch) + "\'"

          }
          else {
            /*    data += warningtext + " [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_prev_deadkey, isCapsused) + " " + uniqueC3Rules[k].prev_deadkey + "]   >   dk(A" + String(uniqueC3Rules[k].dk_prev) + ")\n"
                data += warningtext + " dk(A" + String(uniqueC3Rules[k].dk_prev) + ")  + [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC3Rules[k].deadkey + "]  >  dk(B" + String(uniqueC3Rules[k].dk) + ")\n"
                data += warningtext + " dk(B" + String(uniqueC3Rules[k].dk) + ") [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_key, isCapsused) + " " + uniqueC3Rules[k].key + "]  **°°->  \'" + new TextDecoder().decode(uniqueC3Rules[k].deadkeyed_Ch) + "\'\n"
             */
            data += warningtext + " [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_prev_deadkey, isCapsused) + " " + uniqueC3Rules[k].prev_deadkey + "]   >   XXX  " +
              "[" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC3Rules[k].deadkey + "]  >  YYY " +
              " [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_key, isCapsused) + " " + uniqueC3Rules[k].key + "]  °°->  \'" + new TextDecoder().decode(uniqueC3Rules[k].deadkeyed_Ch) + "\'"


          }
        }

        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        else {
          if (warningtext === "c C3 WARNING: ") {
            data += " [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_prev_deadkey, isCapsused) + " " + uniqueC3Rules[k].prev_deadkey + "]   >   dk(A" + String(uniqueC3Rules[k].dk_prev) + ")\n"
            data += " dk(A" + String(uniqueC3Rules[k].dk_prev) + ")  + [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC3Rules[k].deadkey + "]  >  dk(B" + String(uniqueC3Rules[k].dk) + ")\n"
            data += " dk(B" + String(uniqueC3Rules[k].dk) + ") [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_key, isCapsused) + " " + uniqueC3Rules[k].key + "]  °°->  \'" + new TextDecoder().decode(uniqueC3Rules[k].deadkeyed_Ch) + "\'\n"
          }
          else {
            data += warningtext + " [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_prev_deadkey, isCapsused) + " " + uniqueC3Rules[k].prev_deadkey + "]   >   dk(A" + String(uniqueC3Rules[k].dk_prev) + ")\n"
            data += warningtext + " dk(A" + String(uniqueC3Rules[k].dk_prev) + ")  + [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_deadkey, isCapsused) + " " + uniqueC3Rules[k].deadkey + "]  >  dk(B" + String(uniqueC3Rules[k].dk) + ")\n"
            data += warningtext + " dk(B" + String(uniqueC3Rules[k].dk) + ") [" + this.create_kmn_modifier(uniqueC3Rules[k].modifier_key, isCapsused) + " " + uniqueC3Rules[k].key + "]  **°°->  \'" + new TextDecoder().decode(uniqueC3Rules[k].deadkeyed_Ch) + "\'\n"
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
    }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~ draft print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else {
      data += "c\n"
      data += "c Keyman keyboard generated by kmn-convert\n"
      data += "c\n"
      data += "\n"

      data += "store(&VERSION) \'...\'\n"
      data += "store(&TARGETS) \'any\'\n"
      data += "store(&KEYBOARDVERSION) \'...\'\n"
      data += "store(&COPYRIGHT) '© 2024 SIL International\n"
      // TODO what else ??

      data += "\n"
      data += "begin Unicode > use(main)\n\n"
      data += "group(main) using keys\n\n"

      data += "\n"
    }
    return data
  }

}

/* /// console log all entries

  for(let i=0;i<uniqueC3Rules.length;i++ ) {
   console.log("uniqueC3Rules ", uniqueC3Rules.length, i,

    uniqueC3Rules[i].rule_type !== "" ? uniqueC3Rules[i].rule_type : "--".padEnd(4, " "),


    "|  ", (uniqueC3Rules[i].modifier_prev_deadkey !== "" ? uniqueC3Rules[i].modifier_prev_deadkey.padEnd(33, " ") : "--".padEnd(33, " ")),
    (uniqueC3Rules[i].prev_deadkey !== "" ? uniqueC3Rules[i].prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
    (uniqueC3Rules[i].dk_prev !== 0 ? ("dk(A" + String(uniqueC3Rules[i].dk_prev) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),


    (uniqueC3Rules[i].modifier_deadkey !== "" ? uniqueC3Rules[i].modifier_deadkey.padEnd(30, " ") : "--".padEnd(30, " ")),
    (uniqueC3Rules[i].deadkey !== "" ? uniqueC3Rules[i].deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
    (uniqueC3Rules[i].dk !== 0 ? ("dk(B" + String(uniqueC3Rules[i].dk) + ")").padEnd(9, " ") : "--".padEnd(9, " ")),


    "|  ", (uniqueC3Rules[i].modifier_key !== "" ? uniqueC3Rules[i].modifier_key.padEnd(40, " ") : "--".padEnd(40, " ")),
    (uniqueC3Rules[i].key !== "" ? uniqueC3Rules[i].key.padEnd(8, " ") : "--".padEnd(8, " ")),
    (new TextDecoder().decode(uniqueC3Rules[i].deadkeyed_Ch) !== "" ? new TextDecoder().decode(uniqueC3Rules[i].deadkeyed_Ch).padEnd(10, " ") : "--".padEnd(10, " ")),

    "| °°")
   }

*/

class Rules {

  constructor(
    public rule_type: string,
    public isTerminator: boolean,

    public modifier_prev_deadkey: string,
    public prev_deadkey: string,
    public prev_deadkeys_Ch: Uint8Array,
    public dk_prev: number,

    public modifier_deadkey: string,
    public deadkey: string,
    public deadkeys_Ch: Uint8Array,
    public dk: number,
    public dk_C2: number,

    public modifier_key: string,
    public key: string,
    public deadkeyed_Ch: Uint8Array,

  ) { }

}
