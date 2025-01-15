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
  //     print NCAPS as the first of the modifiers in create_kmn_modifier  
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

export interface rule_object {
  type_C: string,             /* rule type C0-C4 */
  modifiers_first: string,    /* string of modifiers for the first key (e.g. "NCAPS RALT CTRL") */
  key_first: string,          /* name of the first key (e.g. K_U) */

  modifiers_second: string,   /* string of modifiers for the second key */
  key_second: string,         /* name of the second key */

  chars: Uint8Array,          /* the output character */
  isTerminator: boolean,      /* is this a terminator rule */
  dk_hex: string,             /* deadkey in hex notation (e.g. 00B4 for "´" )*/
  dk_char: Uint8Array,        //todo needed?/* */

  deadkeys: Uint8Array,       /* deadkeys to add to a character (e.g. ^,´,`,~ ) */
  deadkeyables: Uint8Array,   /* characters that can use a deadkey (e.g. a,e,i,o,u )*/
  deadkeyed: Uint8Array,      // deadkeys+deadkeyables (e.g. â,ê,î,ô,û)


  /*  ****************************************** */

  prev_deadkey: string,
  modifier_prev_deadkey: string,
  prev_deadkeys_Ch: Uint8Array,
  dk_prev: number,

  deadkey: string,
  modifier_deadkey: string,
  deadkeys_Ch: Uint8Array,
  dk: number,

  key: string,
  modifier_key: string,
  deadkeyed_Ch: Uint8Array,

};

export interface convert_object {
  ArrayOf_Modifiers: string[][],
  ArrayOf_Rules: rule_object[],
};


export class KeylayoutToKmnConverter {
  static readonly INPUT_FILE_EXTENSION = '.keylayout';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';

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
    data += this.create_KMN_Data(data_ukelele)
    data += "\n"

    /*writeFileSync("data/MyResult.kmn", data, { flag: "w" })*/
    this.callbacks.fs.writeFileSync("data/MyResult.kmn", new TextEncoder().encode(data)) // not usable here since it takes UInt8array data

    // ToDo conditions?
    if (data.length > 0)
      return true;
    else
      return false

    data += '\n'
  }

  //   TODO move outside of class?
  // ToDo keep only uint8array-version
  // for more info about mapping and cases C0-C4 see https://docs.google.com/document/d/1ISjACTA9aUBueTo1AoKnOsI6QR5kXeeD_maI0XUyXqg/edit?tab=t.0

  public createRuleData(data_ukelele: convert_object, jsonObj: any): convert_object {
    const ObjectArray: any[] = []
    let dk_counter = 0

    // start Tests v ToDo remove
    const testArray_Ukelele: string[] = []
    let testArray_Ukelele_count = 0
    const testArray_Ukelele_action: string[] = []
    let testArray_Ukelele_action_count = 0

    const testArray_kmn: string[] = []
    const testArray_kmn_action: string[] = []
    let testArray_kmn_action1: string[] = []
    let testArray_kmn_count = 0
    let testArray_kmn_action_count = 0


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
    // End Tests ToDo remove     ^


    let action_id
    const used_Keys_count = 51

    const isCapsused = this.checkIfCapsUsed(data_ukelele.ArrayOf_Modifiers)

    // loop keys 0-50 (= all keys we use)
    for (let j = 0; j < used_Keys_count; j++) {

      // loop behaviors ( in ukelele it is possible to define multiple modifier combinations that behave in the same)
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[0].keyMap.length; i++) {

        // ......................................................................................................
        // case C0: output ......................................................................................
        /*// a key is mapped to a character directly ( code-> output) .............................................
        // ...............e. g. <key code="1" output="s"/> ......................................................
        // ......................................................................................................
        // in keys at top for code 1 (K_S) take output ("s") [italian copy]
        // get modifiers [modifer of Keymap index 0]
        // write [modifer-of-Keymap-index-0  K_S] > s

          [ key code="1" -> output="s" ]
            <keyMap index="0">
                <key code="1" output="s"/>
            </keyMap>
            */
        // ......................................................................................................
        let RuleObj
        let out_ch = ""
        // if this is an output tag
        if ((jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== undefined)) {

          testArray_kmn.push(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'])
          testArray_kmn_count++
          if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'] !== "")
            out_ch = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output']

          // loop modifier combinations
          for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {
            if (this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']))) {
              RuleObj = new Rules(
                "C0",
                this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                "",
                "",
                new TextEncoder().encode(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output']),
                false,
                "",
                new TextEncoder().encode(""),
                new TextEncoder().encode(""),
                new TextEncoder().encode(""),
                new TextEncoder().encode(""),

                "",
                "",
                new TextEncoder().encode(""),
                0,

                "",
                "",
                new TextEncoder().encode(""),
                0,

                this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                new TextEncoder().encode(out_ch),
              )
              ObjectArray.push(RuleObj)
            }
          }
        }
        // if this is an action tag
        else if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] !== undefined) {

          // ......................................................................................................
          // case C1: action + state none + output ................................................................
          // a key is mapped to an action and then to an output ...................................................
          // code->action->action(none)->action(output) ...........................................................
          // ...............e. g. <when state="none" output="a" ...................................................
          // ......................................................................................................
          // in keys at top for code 0 (K_A) take actions id (a9) [italian copy]
          // get modifiers [modifer of Keymap index 0]
          // goto id a9
          // in action id a9 find "none"
          // get output "a"
          // write [modifer-of-Keymap-index-0  K_A] > a
          /*

          /* [key code="0" -> action="a9" ->  action id="a9" -> state="none" -> output="a" ]
          <keyMap index="0">
              <key code="0" action="a9"/>
          </keyMap>
          ...
          <action id="a9">
              <when state="none" output="a"/>
          </action>
              */
          // ......................................................................................................

          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']
          // loop modifiers
          for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {

            const none_output = this.lookup_6_ActionNone__To__ActionOutput(jsonObj, action_id)

            if (none_output !== undefined) {
              RuleObj = new Rules(
                "C1",
                this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                "",
                "",
                new TextEncoder().encode(none_output),
                false,
                "",
                new TextEncoder().encode(""),
                new TextEncoder().encode(""),
                new TextEncoder().encode(""),
                new TextEncoder().encode(""),

                /*  ****************************************** */
                "",
                "",
                new TextEncoder().encode(""),
                0,

                "",
                "",
                new TextEncoder().encode(""),
                0,

                this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                new TextEncoder().encode(none_output)
              )
              ObjectArray.push(RuleObj)
            }
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
          // write: [modifer-of-Keymap-index-0  K_A]  > dk(dk1)  ; dk(dk1) + [second modifiers K_9]  > à

          /* [ key code="0" -> action="a9" -> action id="a9" -> state="1" ->  output="â ]
                <keyMap index="0">
                    <key code="0" action="a9"/>
                </keyMap>
                <action id="a9">
                    <when state="1" output="â"/>
                </action>

            [state="1" ≙ next="1" <- state="none" <- action id="a18  <- key code="24" <- keyMap index="3"]
                <action id="a18">
                    <when state="none" next="1"/>
                </action>

                <keyMap index="3">
                    <key code="24" action="a18"/>
                </keyMap>
          */
          // ......................................................................................................

          let result_C2: string
          let nextVal: string[] = []

          // find the nth action id e.g.  id a9 ->id nr 20
          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']
          const indexOfActions = this.lookup_10_ActionId__To__ActioIndex(jsonObj, action_id)

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
              // get output
              result_C2 = jsonObj.keyboard.actions.action[indexOfActions].when[jj]['@_output']
              // console.log(" typeof(result_C2",typeof(result_C2),typeof( new TextEncoder().encode(result_C2)),typeof(new TextDecoder().decode( (new TextEncoder().encode(result_C2)))))

              // get all cases which result in state 3
              nextVal = this.lookup_5_ActionState__To__ActionNext_none(jsonObj, stateVal)

              // for all modifier combinations
              for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {
                for (let k = 0; k < nextVal.length; k++) {
                  if (result_C2 !== undefined) {
                    const KeymapIndey = this.lookup_15_KeyMapAction__To__KeyMapIndex(jsonObj, nextVal[k])

                    for (let zz = 0; zz < data_ukelele.ArrayOf_Modifiers[KeymapIndey].length; zz++) {
                      RuleObj = new Rules(
                        "C2",
                        this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                        this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                        this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[KeymapIndey][zz], isCapsused),
                        this.map_UkeleleKC_To_VK(Number(this.lookup_11_KeyMapAction__To__KeyMapCode(jsonObj, nextVal[k]))),
                        new TextEncoder().encode(""),
                        false,
                        "",
                        new TextEncoder().encode(""),
                        new TextEncoder().encode(""),
                        new TextEncoder().encode(""),
                        new TextEncoder().encode(result_C2),

                        "",
                        "",
                        new TextEncoder().encode(""),
                        0,

                        this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[KeymapIndey][zz], isCapsused),
                        this.map_UkeleleKC_To_VK(Number(this.lookup_11_KeyMapAction__To__KeyMapCode(jsonObj, nextVal[k]))),
                        new TextEncoder().encode(""),
                        dk_counter++,

                        this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                        this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                        new TextEncoder().encode(result_C2)
                      )

                      ObjectArray.push(RuleObj)
                      //  console.log("RuleObj", RuleObj)
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
          // ......................................................................................................
          // in keys at top for code 10 (=K_BACKQUOTE) take actions id (a57)  [German standard copy]
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
          /*

        [ key code="10" -> action="a57" -> action id="a57" -> state="14" -> next="20 -> state="20" ->  output="̭" ]
            <keyMap index="2">
              <key code="10" action="a57"/>
            </keyMap>

            <action id="a57">
              <when state="14" next="20"/>
            </action>

          <terminators>
              <when state="20" output="̭"/>
          </terminators>


        [ action="a57" -> action id="a57" -> state="14" -> next="20 -> state="20" -> action id="a40" -> key code="37" ]
            <keyMap index="1">
              <key code="37" action="a40"/>
            </keyMap>


        [state="14" ≙ next="14" <- action id="a80  <- key code="40" <- keyMap index="3"]

            <action id="a80">
              <when state="none" next="14"/>
            </action>

            <keyMap index="3">
              <key code="40" action="a80"/>
            </keyMap>

            <terminators>
                <when state="20" output="̭"/>
            </terminators>
          */
          // ......................................................................................................

          let value_state
          let value_next
          let Key_Block4
          let KeyCode_Block2
          let codeIndexPair_Block4: any[][]
          //let keymapIndexForactionID
          let modifier_Block4_fixed: string[]

          // get a9  in behavior/key
          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']
          const actionIdIndex = this.lookup_10_ActionId__To__ActioIndex(jsonObj, action_id)

          // loop all action-when and find state-next-pair
          for (let l = 0; l < jsonObj.keyboard.actions.action[actionIdIndex].when.length; l++) {

            // state_next data
            if ((jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_state'] !== "none")
              && (jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_next'] !== undefined)) {

              // Data of Block Nr 5 ..........................................................
              /*  5: state =3  */           value_state = jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_state']
              /*  5: next  =1  */           value_next = jsonObj.keyboard.actions.action[actionIdIndex].when[l]['@_next']
              /*  5: StateNextID = a16  */  const StateNextID_Block5 = jsonObj.keyboard.actions.action[actionIdIndex]['@_id'] // get actionId of that state/next pair (14,20) e.g. actionId a57   //my a16   NNNRRR 5 
              // ........................................................................

              // Data of Block Nr 4 ..........................................................
              /*  4: code = 32   */          Key_Block4 = this.lookup_11_KeyMapAction__To__KeyMapCode(jsonObj, StateNextID_Block5) // find keyNr of that actionID a57> code=37   // my 32 ( K_U)
              /*  4: K_U  */                 //first_key_C3_U8_deadkey = new TextEncoder().encode(this.map_UkeleleKC_To_VK(Number(ContextKeyNr_deadkey)))
              /*  4: codeIndexPair=[32 3] */ codeIndexPair_Block4 = this.lookup_14_ActionName__To__MapIndex(jsonObj, String(StateNextID_Block5))
              /*  4: MapIndex 3 */           //keymapIndexForactionID = this.lookup_14_ActionName__To__MapIndexSingle(jsonObj, String(theContextID))
              /*     [32 3] FIX Arr[1] */    modifier_Block4_fixed = this.lookup_11_KeyMapIndex__To__Modifier(data_ukelele.ArrayOf_Modifiers, 1) // !! FIXED VALUE HERE!!!! needs to be loop 
              // ........................................................................
              console.log("modifier_Block4_fixed ", modifier_Block4_fixed)


              // Data of Block Nr 3 ..........................................................
              //must be array ???
              /*  3: actioniD = a17  */      const NoneNextId_Block3 = this.lookup_5_ActionState__To__ActionNext_str(jsonObj, value_state)
              // ........................................................................

              // Data of Block Nr 2 ..........................................................
              /*  2: code = 28  */           KeyCode_Block2 = this.lookup_11_KeyMapAction__To__KeyMapCode(jsonObj, NoneNextId_Block3)
              /*  2: K_8   */                const KeyName_Block2 = this.map_UkeleleKC_To_VK(Number(KeyCode_Block2))
              /*  2 index=3 */               const modifier_Block2 = this.lookup_14_ActionName__To__MapIndex(jsonObj, String(NoneNextId_Block3))
              /*  2 RALT   */                const ModifierElementBlock2 = this.lookup_11_KeyMapIndex__To__Modifier(data_ukelele.ArrayOf_Modifiers, modifier_Block2[0][1])
              // ........................................................................

              // Data of Block Nr 6 ..........................................................
              /*  6: [ 'a9', '1', 'â' ] */   const state_Block6_all = this.lookup_13_ActionNext__To__AllState(jsonObj, value_next)
              /*  6: output =  â  FIX */     const output_Block6_fixed = this.lookup_17_ActionState__To__ActionOutput(jsonObj, value_next, state_Block6_all[1][0])
              // ........................................................................

              for (let a = 0; a < state_Block6_all.length; a++) {

                // Data of Block Nr 1 ..........................................................
                // todo get all state1´s
                /*  2: code = 0  */          const keycode_Block1 = this.lookup_11_KeyMapAction__To__KeyMapCode(jsonObj, state_Block6_all[a][0])
                /*  2: K_A  */               const keyname_Block1 = this.map_UkeleleKC_To_VK(Number(keycode_Block1))
                // ........................................................................

                // a16 has state
                // field 1: which code has action a0 or a9 or...
                const modif = this.lookup_15_KeyMapAction__To__KeyMapIndex_Arr(jsonObj, state_Block6_all[a][0], keycode_Block1)
                //console.log("modif ", modif)

                for (let jj = 0; jj < modif.length; jj++) {

                  for (let kk = 0; kk < ModifierElementBlock2.length; kk++) {

                    const modifierText = this.lookup_11_KeyMapIndex__To__Modifier(data_ukelele.ArrayOf_Modifiers, Number(modif[jj][0]))
                    const modiflat = modifierText.flat()
                    const modi = this.create_kmn_modifier(modiflat[0], isCapsused)
                    console.log("modifierText : array of all modifiers", modifierText)
                    console.log("modiflat ", modiflat)
                    console.log("modi only the first mod !!! HERE!!", modi)
                    console.log(" modifier ", modifier_Block4_fixed)


                    RuleObj = new Rules(
                      "CX",

                      modi,
                      keyname_Block1,

                      //this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                      "modifier",
                      this.map_UkeleleKC_To_VK(Number(Key_Block4)),

                      new TextEncoder().encode(""),
                      false,
                      "",
                      new TextEncoder().encode(""),
                      new TextEncoder().encode(""),
                      new TextEncoder().encode(""),
                      new TextEncoder().encode(""),
                      /*  ****************************************** */

                      this.create_kmn_modifier(ModifierElementBlock2[kk], isCapsused),
                      KeyName_Block2,
                      new TextEncoder().encode(""),
                      dk_counter++,

                      this.create_kmn_modifier(modifier_Block4_fixed[0], isCapsused),
                      this.map_UkeleleKC_To_VK(Number(Key_Block4)),
                      new TextEncoder().encode(""),
                      dk_counter++,

                      modi,
                      keyname_Block1,
                      new TextEncoder().encode(output_Block6_fixed),
                    )
                    ObjectArray.push(RuleObj)
                    console.log("qqqqq C3 RuleObj", RuleObj)


                  }
                }
              }
            }
            const result_C3_U8 = new TextEncoder().encode(this.lookup_9_TerminatorState__To__TerminatorOutput_str(jsonObj, value_next))

            for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {

              if (Key_Block4 !== undefined) {

                for (let kk = 0; kk < data_ukelele.ArrayOf_Modifiers[codeIndexPair_Block4[0][1]].length; kk++) {
                  //console.log("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ")

                  for (let jj = 0; jj < modifier_Block4_fixed.length; jj++) {

                    const first_modifier_text_C3_U8_deadkey = data_ukelele.ArrayOf_Modifiers[codeIndexPair_Block4[0][1]][kk]

                    if (this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])) !== "") {
                      console.log("(this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])) ",
                        (this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']))))

                      console.log(" ",)
                      console.log(" ",)
                      console.log("this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])) ", this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])))

                      // ToDo is C3 this really neccessary? - I think no! )
                      RuleObj = new Rules(
                        "C3",

                        this.create_kmn_modifier(first_modifier_text_C3_U8_deadkey, isCapsused),
                        this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),

                        modifier_Block4_fixed[jj],
                        this.map_UkeleleKC_To_VK(Number(Key_Block4)),

                        result_C3_U8,
                        false,
                        "",
                        new TextEncoder().encode(""),
                        result_C3_U8,
                        new TextEncoder().encode(""),
                        new TextEncoder().encode(""),

                        this.create_kmn_modifier(modifier_Block4_fixed[jj], isCapsused),
                        this.map_UkeleleKC_To_VK(Number(Key_Block4)),
                        new TextEncoder().encode(""),
                        0,

                        this.create_kmn_modifier(first_modifier_text_C3_U8_deadkey, isCapsused),
                        this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                        new TextEncoder().encode(""),
                        0,

                        this.create_kmn_modifier(modifier_Block4_fixed[jj], isCapsused),
                        this.map_UkeleleKC_To_VK(Number(Key_Block4)),
                        result_C3_U8
                      )
                      ObjectArray.push(RuleObj)
                      console.log("qqqqq C3 RuleObj", RuleObj)
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
          // code->action->action(none)->action(next)->terminator(output) .........................................
          // ......................................................................................................
          // in keys for code 32 (K_U) at top find actions id a16   [italian copy]
          // get modifiers [modifer of Keymap index 3]
          // in actions a16 (<when state="none" next="4"/>) find state "none" and get next (=4)
          // in terminators ( <when state="4" output="¨"/>) find the state (=4) and get output ("¨")
          // write  [modifer of Keymap index 3  +  K_U] -> "¨"
          /*
          [ action="a16" -> action id="a16" -> state="none" -> next="4 -> state="4" ->  output="¨" ]
              <keyMap index="3">
                <key code="32" action="a16"/>
              </keyMap>

              <action id="a16">
                <when state="none" next="4"/>
              </action>

              <terminators>
                <when state="4" output="¨"/>
              </terminators>
          */


          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']

          const next_id = this.lookup_3_ActionNone__To__ActionNext(jsonObj, action_id)
          const result_C4 = new TextEncoder().encode(this.lookup_9_TerminatorState__To__TerminatorOutput_str(jsonObj, next_id))

          for (let l = 0; l < data_ukelele.ArrayOf_Modifiers[i].length; l++) {

            if (result_C4.length !== 0) {

              RuleObj = new Rules(
                "C4",
                this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                "",
                "",
                new TextEncoder().encode(this.lookup_9_TerminatorState__To__TerminatorOutput_str(jsonObj, next_id)),
                true,
                this.getHexFromChar(new TextDecoder().decode(result_C4)),
                new TextEncoder().encode(this.getHexFromChar(new TextDecoder().decode(result_C4))),
                result_C4,  /* todo which one to use???*/
                result_C4,
                result_C4,

                "",
                "",
                new TextEncoder().encode(""),
                0,

                "",
                "",
                new TextEncoder().encode(""),
                0,

                this.create_kmn_modifier(data_ukelele.ArrayOf_Modifiers[i][l], isCapsused),
                this.map_UkeleleKC_To_VK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
                result_C4
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

let old_counter=0
    console.log("start check ", ObjectArray.length)

console.log("ObjectArray ",ObjectArray.length )
    for (let k = 0; k < ObjectArray.length; k++) {

      if (/*(ObjectArray[k].type_C === "C3")||*/(ObjectArray[k].type_C === "C2")) {
old_counter++
        console.log("Obj: k ", k,
          "\tType: ", (ObjectArray[k].type_C !== "" ? ObjectArray[k].type_C : "--".padEnd(4, " ")),

          /* previous dk*/
          "| modi_prev_dk: ", (ObjectArray[k].modifier_prev_deadkey !== "" ? ObjectArray[k].modifier_prev_deadkey.padEnd(23, " ") : "--".padEnd(23, " ")),
          "key_prev_dk: ", (ObjectArray[k].prev_deadkey !== "" ? ObjectArray[k].prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
          "is_prev dk(): ", (ObjectArray[k].dk_prev !== 0 ? ("dk_p(" + String(ObjectArray[k].dk_prev) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),

          /* dk*/
          "| modi_dk: ", (ObjectArray[k].modifier_deadkey !== "" ? ObjectArray[k].modifier_deadkey.padEnd(23, " ") : "--".padEnd(23, " ")),
          "key_dk: ", (ObjectArray[k].deadkey !== "" ? ObjectArray[k].deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
          "is dk(): ", (ObjectArray[k].dk !== 0 ? ("dk(" + String(ObjectArray[k].dk) + ")").padEnd(9, " ") : "--".padEnd(9, " ")),

          /* key*/
          "| modi: ", (ObjectArray[k].modifier_key !== "" ? ObjectArray[k].modifier_key.padEnd(20, " ") : "--".padEnd(20, " ")),
          "key: ", (ObjectArray[k].key !== "" ? ObjectArray[k].key.padEnd(8, " ") : "--".padEnd(8, " ")),
          "ch: ", (new TextDecoder().decode(ObjectArray[k].deadkeyed_Ch) !== "" ? new TextDecoder().decode(ObjectArray[k].deadkeyed_Ch).padEnd(10, " ") : "--".padEnd(10, " ")),

          "| °°"
        )
      }
    }
    console.log("old_counter ",old_counter )


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

    //****************************************************************************************** */
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
  public lookup_5_ActionState__To__ActionNext_str(data: any, action_idName: string): string {
    // e.g. action_idName = 3 
    if (action_idName !== "none") {
      // loop all action/when
      for (let k = 0; k < data.keyboard.actions.action.length; k++) {
        for (let j = 0; j < data.keyboard.actions.action[k].when.length; j++) {
          // find attribute next === 3
          if (data.keyboard.actions.action[k].when[j]['@_next'] === action_idName) {
            return data.keyboard.actions.action[k]['@_id']
          }
        }
      }
    }
    return ""
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
  // get all entries for state= and a given action id
  public lookup_6_Action__To__State_Array(data: any, search: any): any[] {
    const returnarray: string[] = []
    for (let jj = 0; jj < data.keyboard.actions.action.length; jj++) {
      if (data.keyboard.actions.action[jj]['@_id'] === search) {
        for (let kk = 0; kk < data.keyboard.actions.action[jj].when.length; kk++) {
          if (data.keyboard.actions.action[jj].when[kk]['@_state'] !== "none") {
            returnarray.push(data.keyboard.actions.action[jj].when[kk]['@_state'])
          }
        }
      }
    }
    return returnarray
  }

  // get all entries for state= and a given action id
  public lookup_6_State__From__Next_Array(data: any, search: any): string[] {
    const returnarray: string[] = []
    for (let jj = 0; jj < data.keyboard.actions.action.length; jj++) {
      for (let kk = 0; kk < data.keyboard.actions.action[jj].when.length; kk++) {
        if ((data.keyboard.actions.action[jj].when[kk]['@_next'] === search) && (data.keyboard.actions.action[jj].when[kk]['@_state'] !== "none")) {
          returnarray.push(data.keyboard.actions.action[jj]['@_id'])
        }
      }
    }
    return returnarray
  }

  public lookup_13_ActionNext__To__AllState(data: any, search: string) {
    const returnarray2D: string[][] = []
    for (let ll = 0; ll < data.keyboard.actions.action.length; ll++) {
      for (let mm = 0; mm < data.keyboard.actions.action[ll].when.length; mm++) {
        const returnarray: string[] = []
        if ((data.keyboard.actions.action[ll].when[mm]['@_state'] === search)) {
          returnarray.push(data.keyboard.actions.action[ll]['@_id'])
          returnarray.push(data.keyboard.actions.action[ll].when[mm]['@_state'])
          returnarray.push(data.keyboard.actions.action[ll].when[mm]['@_output'])
        }
        if (returnarray.length > 0)
          returnarray2D.push(returnarray)
      }
    }
    return returnarray2D
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
    return -1
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
  public lookup_14_ActionName__To__MapIndexSingle(data: any, search: string): number {
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
    return mapIndexArray_max[0][1]
  }
  public lookup_15_KeyMapAction__To__State(data: any, search: string): string {
    // todo loop
    for (let k = 0; k < data.keyboard.actions.action.length; k++) {
      if ((data.keyboard.actions.action[k]['@_id'] === search))
        // for (let l = 0; l < data.keyboard.actions.action[l].length; l++) {
        for (let j = 0; j < data.keyboard.actions.action.when.length; j++) {
          return ""
          //return data.keyboard.actions.action[k].when[j]['@_state']
        }
      //}
    }
    return ""
  }
  public lookup_15_KeyMapAction__To__Next(data: any, search: string): string {
    for (let k = 0; k < data.keyboard.actions.action.length; k++) {
      if ((data.keyboard.actions.action[k]['@_id'] === search))
        return data.keyboard.actions.action[k].when[1]['@_next']
    }
    return ""
  }

  public lookup_15_KeyMapAction__To__KeyMapIndex(data: any, search: string): number {
    for (let kk = 0; kk < data.keyboard.keyMapSet[0].keyMap.length; kk++) {
      for (let jj = 0; jj < data.keyboard.keyMapSet[0].keyMap[kk].key.length; jj++) {
        if (data.keyboard.keyMapSet[0].keyMap[kk].key[jj]['@_action'] === search) {
          return data.keyboard.keyMapSet[0].keyMap[kk]['@_index']
        }
      }
    }
    return -1
  }
  public lookup_15_KeyMapAction__To__KeyMapIndex_Arr(data: any, search: string, keycode: number): string[][] {
    const returnarray2D: string[][] = []
    for (let kk = 0; kk < data.keyboard.keyMapSet[0].keyMap.length; kk++) {
      for (let jj = 0; jj < data.keyboard.keyMapSet[0].keyMap[kk].key.length; jj++) {
        const returnarray: string[] = []
        if (data.keyboard.keyMapSet[0].keyMap[kk].key[jj]['@_action'] === search) {
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[kk]['@_index'])
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[kk].key[jj]['@_code'])
          returnarray.push(data.keyboard.keyMapSet[0].keyMap[kk].key[jj]['@_action'])
        }
        if (returnarray.length > 0) {
          returnarray2D.push(returnarray)
        }
      }
    }

    return returnarray2D
  }

  public lookup_11_KeyMapIndex__To__Modifier(data: any, search: number): string[] {
    return data[search]
  }
  public lookup_11_KeyMapIndex__To__Modifier2(data: any, search: number): string[] {
    return data.ArrayOf_Modifiers[search]
    //return data[search]
  }

  public lookup_17_ActionState__To__ActionOutput(data: any, search: string, nextval: string): string {
    for (let k = 0; k < data.keyboard.actions.action.length; k++) {
      for (let j = 0; j < data.keyboard.actions.action[k].when.length; j++) {
        if ((data.keyboard.actions.action[k].when[j]['@_state'] === search) && (data.keyboard.actions.action[k]['@_id'] === nextval)) {
          return data.keyboard.actions.action[k].when[j]['@_output']
        }
      }
    }
    return ""
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

      // TODO review, find more conditions & simplify
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
    return keylayout_modifier.flat().flat().includes("caps")
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


  public createData_Deadkeys(data_ukelele: convert_object, workArray2D: any[]): string {

    let data: string = ""
    // filter for dk values
    data += '\########## OK #################################################################\n'
    data += '\nNOW MY C4 RULES **ccc ********* (only to get 02C6 ect) *********************\n\n'

    // ToDo in 1 func
    // select only lines twhere isTerminator=true
    const uniqueDeadkeys_all = workArray2D.filter((curr) => {
      if (curr.isTerminator === true) {
        return curr;
      }
    });

    // remove duplicates
    const uniqueDeadkeys = uniqueDeadkeys_all.reduce((unique, o) => {
      if (!unique.some((obj: { chars: any; type_C: any; modifier_key: any; key: any }) =>
        new TextDecoder().decode(obj.chars) === new TextDecoder().decode(o.chars)
        && obj.key === o.key
        && obj.type_C === o.type_C
        && obj.modifier_key === o.modifier_key)
      ) {
        unique.push(o);
      }
      return unique;
    }, []);

    const uniqueDeadkeys_all_obj = data_ukelele.ArrayOf_Rules.filter((curr) => {
      if (curr.isTerminator === true) {
        return curr;
      }
      return ""
    });

    // remove duplicates
    const uniqueDeadkeys_obj = uniqueDeadkeys_all_obj.reduce((unique, o) => {
      if (!unique.some((obj: { chars: any; type_C: any; modifier_key: any; key: any }) =>
        new TextDecoder().decode(obj.chars) === new TextDecoder().decode(o.chars)
        && obj.key === o.key
        && obj.type_C === o.type_C
        && obj.modifier_key === o.modifier_key)
      ) {
        unique.push(o);
      }
      return unique;
    }, []);

    for (let kkk = 0; kkk < uniqueDeadkeys_obj.length; kkk++) {
      let line: string = ""
     // line = "+  [" + uniqueDeadkeys[kkk].modifier_key + " " + uniqueDeadkeys[kkk].key + "]  >   dk(" + uniqueDeadkeys[kkk].dk_hex + ")"
      line = "+  [" + uniqueDeadkeys_obj[kkk].modifier_key + " " + uniqueDeadkeys_obj[kkk].key + "]  >   dxk(" + uniqueDeadkeys_obj[kkk].dk_hex + ")"
      data += line + '\n'
    }

    /*
    How to create rules from keylayout data ffrom data_ukelele.ArrayOf_processed_RuleData:
 
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
   /* const deadkeyables_raw = workArray2D.filter((curr) => {
      if ((curr.type_C === "C1") && (curr.key !== "K_SPACE") && (new TextDecoder().decode(curr.chars) !== "")) {
        return curr;
      }
    });

    // select only lines for deadkeyed ( entries that contain C2, are not K_SPACE and create a deadkeyed) e.g. from ['C2', 'CAPS', 'K_A', 'K_N', 'Ã' ]
    const deadkeyed_raw_multiple = workArray2D.filter((curr) => {
      if ((curr.type_C === "C2") && (curr.key !== "K_SPACE") && (curr.deadkeyed !== "")) {
        return curr;
      }
    });*/

    // select only lines for deadkeyables ( entries that contain C1, are not K_SPACE and create chars e.g. from 'C1', 'CAPS', 'K_A',  'A' ])
    const deadkeyables_raw = data_ukelele.ArrayOf_Rules.filter((curr) => {
      if ((curr.type_C === "C1") && (curr.key !== "K_SPACE") && (new TextDecoder().decode(curr.chars) !== "")) {
        return curr;
      }
      return ""
    });

    // select only lines for deadkeyed ( entries that contain C2, are not K_SPACE and create a deadkeyed) e.g. from ['C2', 'CAPS', 'K_A', 'K_N', 'Ã' ]
    const deadkeyed_raw_multiple = data_ukelele.ArrayOf_Rules.filter((curr) => {
     // if ((curr.type_C === "C2") && (curr.key !== "K_SPACE") && (curr.deadkeyed_Ch !== "")) {
        if ((curr.type_C === "C2") && (curr.key !== "K_SPACE") ) {
        return curr;
      }
      return ""
    });

    // remove duplicate s
    const deadkeyed_raw = deadkeyed_raw_multiple.reduce((unique, o) => {
      if (!unique.some((obj: { chars: any; type_C: any; modifier_key: any; key: any; deadkeyed: any }) =>
        new TextDecoder().decode(obj.deadkeyed) === new TextDecoder().decode(o.deadkeyed)
        && obj.key === o.key
        && obj.type_C === o.type_C
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

          dk_array.push(new TextDecoder().decode(deadkeyables_raw[rr].chars))  // deadkeyable e.g.  A
          dk_array.push(deadkeyed_raw[ss].key_second)      // Keyname dk  e.g.  K_N
          dk_array.push(new TextDecoder().decode(deadkeyed_raw[ss].deadkeyed))     // deadkeyed   e.g.  Ã
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

    const deadkeyedElement: string[][] = Array.from({ length: uniqueDeadkeys.length }, () => new Array(2).fill(''));
    const deadkeyablesElement: string[][] = Array.from({ length: uniqueDeadkeys.length }, () => new Array(1).fill(''));


    // why is there a set entry???? deadkeyables_raw[0]
    for (let i = 0; i < uniqueDeadkeys.length; i++) {
      for (let j = 0; j < unique_dk_array_comlpete[0].length; j++) {

        if (uniqueDeadkeys[i].key === unique_dk_array_comlpete[0][j][1]) {
          deadkeyablesElement[i][0] = deadkeyablesElement[i][0] + "\'" + unique_dk_array_comlpete[0][j][0] + "\'  "
          deadkeyedElement[i][0] = deadkeyedElement[i][0] + "\'" + unique_dk_array_comlpete[0][j][2] + "\'  "
        }
        deadkeyedElement[i][1] = uniqueDeadkeys[i].dk_hex
      }
    }

    data += "\n"
    data += '\########## OK #################################################################\n'

    data += '\nmatch > use(deadkeys)\n\n'
    data += '\ngroup(deadkeys)\n\n'

    for (let i = 0; i < deadkeyablesElement.length; i++) {
      data += "\n\ndk: " + deadkeyedElement[i][1]
      data += "\ndeadkeyablesArray xx" + deadkeyablesElement[i][0]
      data += "\ndeadkeyedArray xx  " + deadkeyedElement[i][0]
    }
    return data
  }

  public createData_Rules(data_ukelele: convert_object, workArray2D: any[]): string {
    const maxkey = 50
    let data: string = ""
    let keymarker = ""

console.log("data_ukelele.ArrayOf_Rules ", data_ukelele.ArrayOf_Rules.length,data_ukelele.ArrayOf_Rules[155].key)

    // can I use dataUkeklele /ruledata to get uniqueDeadkeys_all
    // select only lines that contain C0 or C1 and create an output.
/*
    const uniqueDeadkeys_New = data_ukelele.ArrayOf_Rules.reduce((unique, o) => {
      if (!unique.some((obj: { chars: any; type_C: any; modifier_key: any; key: any }) =>
        new TextDecoder().decode(obj.chars) === new TextDecoder().decode(o.chars)
        && obj.key === o.key
        && obj.type_C === o.type_C
        && obj.modifier_key === o.modifier_key)
      ) {
        unique.push(o);
      }
      return unique;
    }, []);
let unique_counter=0
    console.log("uniqueDeadkeys_New ",uniqueDeadkeys_New.length )
console.log("uniqueDeadkeys_New ",uniqueDeadkeys_New.length ,uniqueDeadkeys_New)
for (let k = 0; k < uniqueDeadkeys_New.length; k++) {

  if ((ObjectArray[k].type_C === "C3")||(uniqueDeadkeys_New[k].type_C === "C2")) {
    unique_counter++
console.log("uniqueDeadkeys_New: k ", k,
  "\tType: ", (uniqueDeadkeys_New[k].type_C !== "" ? uniqueDeadkeys_New[k].type_C : "--".padEnd(4, " ")),

  "| modi_prev_dk: ", (uniqueDeadkeys_New[k].modifier_prev_deadkey !== "" ? uniqueDeadkeys_New[k].modifier_prev_deadkey.padEnd(23, " ") : "--".padEnd(23, " ")),
  "key_prev_dk: ", (uniqueDeadkeys_New[k].prev_deadkey !== "" ? uniqueDeadkeys_New[k].prev_deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
  "is_prev dk(): ", (uniqueDeadkeys_New[k].dk_prev !== 0 ? ("dk_p(" + String(uniqueDeadkeys_New[k].dk_prev) + ")").padEnd(11, " ") : "--".padEnd(11, " ")),

  "| modi_dk: ", (uniqueDeadkeys_New[k].modifier_deadkey !== "" ? uniqueDeadkeys_New[k].modifier_deadkey.padEnd(23, " ") : "--".padEnd(23, " ")),
  "key_dk: ", (uniqueDeadkeys_New[k].deadkey !== "" ? uniqueDeadkeys_New[k].deadkey.padEnd(8, " ") : "--".padEnd(8, " ")),
  "is dk(): ", (uniqueDeadkeys_New[k].dk !== 0 ? ("dk(" + String(uniqueDeadkeys_New[k].dk) + ")").padEnd(9, " ") : "--".padEnd(9, " ")),

  "| modi: ", (uniqueDeadkeys_New[k].modifier_key !== "" ? uniqueDeadkeys_New[k].modifier_key.padEnd(20, " ") : "--".padEnd(20, " ")),
  "key: ", (uniqueDeadkeys_New[k].key !== "" ? uniqueDeadkeys_New[k].key.padEnd(8, " ") : "--".padEnd(8, " ")),
  "ch: ", (new TextDecoder().decode(uniqueDeadkeys_New[k].deadkeyed_Ch) !== "" ? new TextDecoder().decode(uniqueDeadkeys_New[k].deadkeyed_Ch).padEnd(10, " ") : "--".padEnd(10, " ")),

  "| °°"
)

  }}
console.log("unique_counter ",unique_counter )

*/
    // can I use dataUkeklele /ruledata to get uniqueDeadkeys_all
    // select only lines that contain C0 or C1 and create an output.
    const uniqueDeadkeys_all = workArray2D.filter((curr) => {
      if ((curr.chars !== new TextEncoder().encode("") || curr.chars !== undefined)
        && (curr.type_C === "C0") || (curr.type_C === "C1")) {
        return curr;
      }
    });

    // Also remove duplicates
    const uniqueDeadkeys = uniqueDeadkeys_all.reduce((unique, o) => {
      if (!unique.some((obj: { chars: any; type_C: any; modifier_key: any; key: any }) =>
        new TextDecoder().decode(obj.chars) === new TextDecoder().decode(o.chars)
        && obj.key === o.key
        && obj.type_C === o.type_C
        && obj.modifier_key === o.modifier_key)
      ) {
        unique.push(o);
      }
      return unique;
    }, []);

    const uniqueDeadkeys_all_obj = data_ukelele.ArrayOf_Rules.filter((curr) => {
      if ((curr.chars !== new TextEncoder().encode("") || curr.chars !== undefined)
        && (curr.type_C === "C0") || (curr.type_C === "C1")) {
        return curr;
      }
      else return ""
    });

    // Also remove duplicates
    const uniqueDeadkeys_obj = uniqueDeadkeys_all_obj.reduce((unique, o) => {
      if (!unique.some((obj: { chars: any; type_C: any; modifier_key: any; key: any }) =>
        new TextDecoder().decode(obj.chars) === new TextDecoder().decode(o.chars)
        && obj.key === o.key
        && obj.type_C === o.type_C
        && obj.modifier_key === o.modifier_key)
      ) {
        unique.push(o);
      }
      return unique;
    }, []);

console.log("uniqueDeadkeys_obj ", uniqueDeadkeys_obj)


    for (let kkk = 0; kkk < uniqueDeadkeys.length; kkk++) {
      // lookup key nr of the key that is being processed

      let keyNr = 0;
      for (let pp = 0; pp < maxkey; pp++) {
        if (this.map_UkeleleKC_To_VK(pp) === uniqueDeadkeys[kkk].key)
          keyNr = pp
      }

      // todo wrong output: writes 49 with no K_   and 0 with space ????
      // skip keyNr 48 ( TAB )
      if (keyNr === 48)
        continue

      // add a line after rules of each key
      if (uniqueDeadkeys[kkk].key !== keymarker)
        data += '\n'

      // write rule

        //data += keyNr + "-(modif:" + uniqueDeadkeys[kkk].type_C + "-" + "i" + `) + [` + (uniqueDeadkeys[kkk].modifier_key + ' ' + uniqueDeadkeys[kkk].key).trim() + `] > \'` + new TextDecoder().decode(uniqueDeadkeys[kkk].chars) + '\'\n'

      data += keyNr + "-(modif:"+uniqueDeadkeys_obj[kkk].type_C + "-" + "i" + `) + [` + (uniqueDeadkeys_obj[kkk].modifier_key + ' ' + uniqueDeadkeys_obj[kkk].key).trim() + `] > \'` + new TextDecoder().decode(uniqueDeadkeys_obj[kkk].chars) + '\'\n'
      keymarker = uniqueDeadkeys[kkk].key
    }

    data += '\n'
    // console.log("data ", data)

    return data
  }

  public createData_Stores(data_ukelele: convert_object): string {
    let data: string = ""

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

    return data
  }

  public create_KMN_Data(data_ukelele: convert_object): string {

    // create an array to work with ................................................
    const workArray2D: any[] = []
    for (let k = 0; k < data_ukelele.ArrayOf_Rules.length; k++) {
      workArray2D.push(data_ukelele.ArrayOf_Rules[k])
    }

    let data: string = ""
    // add top part of kmn file: STORES
    data += this.createData_Stores(data_ukelele)
    // add middle part of kmn file: Rules
    data += this.createData_Rules(data_ukelele, workArray2D)
    // add bottom part of kmn file: DEADKEYS
    data += this.createData_Deadkeys(data_ukelele, workArray2D)

    return data
  }
}


class Rules {

  constructor(
    public type_C: string,
    public modifiers_first: string,
    public key_first: string,

    public modifiers_second: string,
    public key_second: string,

    public chars: Uint8Array,
    public isTerminator: boolean,
    public dk_hex: string,
    public dk_char: Uint8Array,   //todo needed?

    public deadkeys: Uint8Array,
    public deadkeyables: Uint8Array,
    public deadkeyed: Uint8Array,
    /*  ****************************************** */

    public modifier_prev_deadkey: string,
    public prev_deadkey: string,
    public prev_deadkeys_Ch: Uint8Array,
    public dk_prev: number,

    public modifier_deadkey: string,
    public deadkey: string,
    public deadkeys_Ch: Uint8Array,
    public dk: number,

    public modifier_key: string,
    public key: string,
    public deadkeyed_Ch: Uint8Array,

  ) { }
/*
  public get_key() {    return this.key  }

  public getval() {
    return `blabla ${this.isTerminator} blub: ${this.type_C}...`
  }9*/
}
