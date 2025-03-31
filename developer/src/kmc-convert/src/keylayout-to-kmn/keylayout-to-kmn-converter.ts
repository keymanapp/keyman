/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Converts macOS/Ukelele .keylayout files to Keyman .kmn
 */
import { CompilerCallbacks, CompilerOptions } from "@keymanapp/developer-utils";
import { ConverterToKmnArtifacts } from "../converter-artifacts.js";
import path from 'path';
import { XMLParser } from 'fast-xml-parser';  // for reading an xml file
import { readFileSync } from 'fs';
import { util } from '@keymanapp/common-types';
import boxXmlArray = util.boxXmlArray;

function boxArray(source: any) {
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
  kmn_filename: string,
  arrayOf_Modifiers: string[][],
  arrayOf_Rules: rule_object[],
};

export class KeylayoutToKmnConverter {

  static readonly INPUT_FILE_EXTENSION = '.keylayout';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';
  static readonly USED_KEYS_COUNT = 51;
  static readonly MAX_CTRL_CHARACTER = 32;
  static readonly SKIP_COMMENTED_LINES = false;

  constructor(private callbacks: CompilerCallbacks, options: CompilerOptions) {
  };

  /**
   * @brief  member function to run read/convert/write
   * @param  inputFilename the ukelele .keylayout-file to be converted
   * @param  outputFilename the resulting keyman .kmn-file
   * @return null on success
   */
  async run(inputFilename: string, outputFilename: string = ""): Promise<ConverterToKmnArtifacts> {

    if (!inputFilename) {
      throw new Error('Invalid parameters');
    }
    const jsonO: object = this.read(inputFilename);

    if (!jsonO) {
      throw new Error('Error while processing read()');
      return null;
    }

    const outArray: convert_object = await this.convert(jsonO, outputFilename);

    if (!outArray) {
      throw new Error('Error while processing convert()');
      return null;
    }

    const out_text_ok: boolean = this.write(outArray);

    if (!out_text_ok) {
      throw new Error('Error while processing write()');
      return null;
    }
    return null;
  }

  /**
   * @brief  member function to parse data from a .keylayout-file and store to a json object
   * @param  absolutefilename the ukelele .keylayout-file to be parsed
   * @return in case of success: json object containing data of the .keylayout file; else null
   */
  public read(absolutefilename: string): Object {
    let xmlFile;
    let jsonObj = [];

    const options = {
      ignoreAttributes: false,
      trimValues: false,           // preserve spaces
      attributeNamePrefix: '@_'    // to access the attribute
    };

    try {
      xmlFile = readFileSync(path.join(process.cwd(), "data", absolutefilename.replace(/^.*[\\/]/, '')), 'utf8');
      const parser = new XMLParser(options);
      jsonObj = parser.parse(xmlFile);  // get plain Object
      boxArray(jsonObj.keyboard);       // jsonObj now contains arrays; no single fields
    }
    catch (err) {
      console.log(err.message);
    }
    return jsonObj;
  }

  /**
   * @brief  member function to read filename and behaviour of a json object into a convert_object
   * @param  jsonObj containing filename, behaviour and rules of a json object
   * @return an convert_object containing all data ready to print out
   */
  public convert(jsonObj: any, outputfilename: string = ""): convert_object {

    const modifierBehavior: string[][] = [];           // modifier for each behaviour
    const rule_object: rule_object[] = [];             // an array of data for a kmn rule
    const jsonObj_any: any = jsonObj;

    const data_object: convert_object = {
      keylayout_filename: "",
      kmn_filename: "",
      arrayOf_Modifiers: [],
      arrayOf_Rules: []
    };

    // ToDo in a new PR: check tags  ( issue # 13599)
    if (jsonObj_any.hasOwnProperty("keyboard")) {

      data_object.keylayout_filename = jsonObj_any.keyboard['@_name'] + ".keylayout";

      if ((outputfilename === "") || (outputfilename === null)) {
        data_object.kmn_filename = (process.cwd() + "\\data\\" + data_object.keylayout_filename.substring(0, data_object.keylayout_filename.lastIndexOf(".")) + ".kmn");
      }
      else
        data_object.kmn_filename = outputfilename;

      console.log("RUN kmc convert - input file: ", (process.cwd() + "\\data\\" + data_object.keylayout_filename), " -->  output file: ", data_object.kmn_filename);

      data_object.arrayOf_Modifiers = modifierBehavior;  // ukelele uses behaviours e.g. 18 modifiersCombinations in 8 KeyMapSelect(behaviors)
      data_object.arrayOf_Rules = rule_object;

      // create an array of modifier combinations and store in data_object
      for (let j = 0; j < jsonObj.keyboard.modifierMap.keyMapSelect.length; j++) {
        const singleModifierSet: string[] = [];
        for (let k = 0; k < jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier.length; k++) {
          singleModifierSet.push(jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier[k]['@_keys']);
        }
        modifierBehavior.push(singleModifierSet);
      }
      // fill rules into arrayOf_Rules of data_object
      return this.createRuleData(data_object, jsonObj);
    }
    return data_object;
  }

  /**
    * @brief  member function to read the rules contained in a json object and add array of Rules[] to an convert_object
    * @param  data_ukelele: an object containing the name of the input file, an array of behaviours and an (empty) array of Rules
    * @param  jsonObj: json Object containing all data read from a keylayout file
    * @return an object containing the name of the input file, an array of behaviours and a populated array of Rules[]
    */
  public createRuleData(data_ukelele: convert_object, jsonObj: any): convert_object {

    const object_array: rule_object[] = [];
    let dk_counter_C3: number = 0;
    let dk_counter_C2: number = 0;
    let action_id: string;

    // check if we use CAPS in a modifier throughout the .keylayout file. In this case we need to add NCAPS
    const isCapsused: boolean = this.checkIfCapsIsUsed(data_ukelele.arrayOf_Modifiers);

    // loop keys 0-50 (= all keys we use)
    for (let j = 0; j < KeylayoutToKmnConverter.USED_KEYS_COUNT; j++) {

      // loop behaviors (in ukelele it is possible to define multiple modifier combinations that behave in the same way)
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[0].keyMap.length; i++) {

        let rule_obj: rule_object;

        // ...............................................................................................................................
        // case C0: output ...............................................................................................................
        // C0 see: https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.g7jwx3lx0ydd ...
        // a key is mapped to a character directly ( code -> output) .....................................................................
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
              );
              object_array.push(rule_obj);
            }
          }

        }
        else if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] !== undefined) {

          action_id = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'];
          // ...............................................................................................................................
          // case C1: action + state none + output .........................................................................................
          // C1 see: https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.g7jwx3lx0ydd ...
          // a key is mapped to an action and then to an output ............................................................................
          // KeyMap:code -> KeyMap:action->action:action_state(none) -> action_output ......................................................
          // ...............e. g. <when state="none" output="a" ............................................................................
          // ...............................................................................................................................

          for (let l = 0; l < data_ukelele.arrayOf_Modifiers[i].length; l++) {

            if ((this.get_Output__From__ActionId_None(jsonObj, action_id) !== undefined)
              && (this.get_Output__From__ActionId_None(jsonObj, action_id) !== "")) {

              const outputchar: string = this.get_Output__From__ActionId_None(jsonObj, action_id);
              const b1_modifierKey_arr: string[][] =
                this.get_ActionOutputBehaviourKeyModi_From__ActionIDStateOutput(jsonObj, data_ukelele.arrayOf_Modifiers, action_id, outputchar, isCapsused);

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
                );
                if ((outputchar !== undefined) && (outputchar !== "undefined") && (outputchar !== "")) {
                  object_array.push(rule_obj);
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

            const b1_actionIndex: number = this.get_ActionIndex__From__ActionId(jsonObj, action_id);

            // with action_id from above loop all 'action' and search for a state(none)-next-pair ............................................................................................................
            // e.g. in Block 5: find <when state="none" next="1"/> for action id a18 ......................................................................................................................
            for (let l = 0; l < jsonObj.keyboard.actions.action[b1_actionIndex].when.length; l++) {
              if ((jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_state'] === "none")         // find "none"
                && (jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_next'] !== undefined)) {   // find "next"

              // Data of Block Nr 5 .....................................................................................................................................................................
              // of this state(none)-next-pair get value of next (next="1") .............................................................................................................................
              /* eg: 1  */                            const b5_value_next: string = jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_next'];
              // ........................................................................................................................................................................................


              // Data of Block Nr 4 .....................................................................................................................................................................
              // with present action_id (a18) find all keycode-behaviour-pairs that use this action (a18) => (keymapIndex 0/keycode 24 and keymapIndex 3/keycode 24) ....................................
              // from these create an array of modifier combinations  e.g. [['','caps?'], ['Caps']] .....................................................................................................
              /* eg: [['24', 0], ['24', 3]] */        const b4_deadkey_arr: number[][] = this.get_KeyModifier_array__From__ActionID(jsonObj, action_id);
              /* e.g. [['','caps?'], ['Caps']]*/      const b4_deadkeyModifier_arr: string[] = this.get_Modifier_array__From__KeyModifier_array(data_ukelele.arrayOf_Modifiers, b4_deadkey_arr);
              // ........................................................................................................................................................................................


              // Data of Block Nr 6 .....................................................................................................................................................................
              // create an array[action id,state,output] from all state-output-pairs that use state = b5_value_next (e.g. use 1 in  <when state="1" output="â"/> ) ......................................
              /*  eg: [ 'a9','1','â'] */              const b6_actionId_arr: string[][] = this.get_ActionStateOutput_array__From__ActionState(jsonObj, b5_value_next);
              // ........................................................................................................................................................................................


              // Data of Block Nr 1  ....................................................................................................................................................................
              // create array[Keycode,Keyname,action id,actionIndex,output] and array[Keyname,action id,behaviour,modifier,output] ......................................................................
              /*  eg: ['0','K_A','a9','0','â'] */     const b1_keycode_arr: string[][] = this.get_KeyActionOutput_array__From__ActionStateOutput_array(jsonObj, b6_actionId_arr);
              /*  eg: ['K_A','a9','0','NCAPS','â']*/  const b1_modifierKey_arr: string[][] = this.get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(jsonObj, b1_keycode_arr, isCapsused);
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
                        );
                        if ((b1_modifierKey_arr[n4][4] !== undefined)
                          && (b1_modifierKey_arr[n4][4] !== "undefined")
                          && (b1_modifierKey_arr[n4][4] !== "")) {
                          object_array.push(rule_obj);
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
              /* e.g. state = 3  */                       const b5_value_state: string = jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_state'];
              /* e.g. next  = 1  */                       const b5_value_next: string = jsonObj.keyboard.actions.action[b1_actionIndex].when[l]['@_next'];
              // ...........................................................................................................................................................................................

              // Data of Block Nr 4 ........................................................................................................................................................................
              // with present action_id (a16) find all keycode-behaviour-pairs that use this action (a16) => (keymapIndex 3/keycode 32) ....................................................................
              // from these create an array of modifier combinations  e.g. [ [ 'anyOption', 'Caps' ] ] .....................................................................................................
              /* e.g. [['32', 3]] */                      const b4_deadkey_arr: number[][] = this.get_KeyModifier_array__From__ActionID(jsonObj, action_id);
              /* e.g. [ [ 'anyOption', 'Caps' ] ]*/       const b4_deadkeyModifier_arr: string[] = this.get_Modifier_array__From__KeyModifier_array(data_ukelele.arrayOf_Modifiers, b4_deadkey_arr);
              // ...........................................................................................................................................................................................

              // Data of Block Nr 3 ........................................................................................................................................................................
              // get an action id from a state-output-pair that use state = b5_value_state (e.g. use 3 in  <when state="none" next="3"/> ) .................................................................
              /* e.g. actioniD = a17  */                  const b3_actionId: string = this.get_ActionID__From__ActionNext(jsonObj, b5_value_state);
              // ...........................................................................................................................................................................................

              // Data of Block Nr 2  .......................................................................................................................................................................
              // with present action_id (a17) find all key names and behaviours that use this action (a17) => (keymapIndex 3/keycode 28) ....................................................................
              // from these create an array of modifier combinations  e.g. [ [ 'anyOption', 'Caps' ] ] .....................................................................................................
              /* eg: index=3 */                           const b2_prev_deadkey_arr: number[][] = this.get_KeyModifier_array__From__ActionID(jsonObj, String(b3_actionId));   // conversion not necessary
              /* e.g. [ [ 'anyOption', 'Caps' ] ] */      const b2_prev_deadkeyModifier_arr: string[] = this.get_Modifier_array__From__KeyModifier_array(data_ukelele.arrayOf_Modifiers, b2_prev_deadkey_arr);
              // ...........................................................................................................................................................................................
              // Data of Block Nr 6 ........................................................................................................................................................................
              // create an array[action id,state,output] from all state-output-pairs that use state = b5_value_next (e.g. use 1 in  <when state="1" output="â"/> ) .........................................
              /*  eg:[ [ 'a9','1','â'] ]*/                const b6_actionId_arr: string[][] = this.get_ActionStateOutput_array__From__ActionState(jsonObj, b5_value_next);
              // ...........................................................................................................................................................................................

              // Data of Block Nr 1  .......................................................................................................................................................................
              // create array[Keycode,Keyname,action id,actionIndex,output] and array[Keyname,action id,behaviour,modifier,output] .........................................................................
              /*  eg: ['49','K_SPACE','a0','0','Â'] */    const b1_keycode_arr: string[][] = this.get_KeyActionOutput_array__From__ActionStateOutput_array(jsonObj, b6_actionId_arr);
              /*  eg: ['K_SPACE','a0','0','NCAPS','Â'] */ const b1_modifierKey_arr: string[][] = this.get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(jsonObj, b1_keycode_arr, isCapsused);
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
                              /*   id_prev_deadkey */        dk_counter_C3++,
                              /*   unique A */              0,

                              /*   modifier_deadkey */      this.create_kmn_modifier(b4_deadkeyModifier_arr[n4][n5], isCapsused),
                              /*   deadkey */               this.map_UkeleleKC_To_VK(Number(b4_deadkey_arr[n6][0])),
                              /*   dk for C2*/              0,
                              /*   unique B */              0,

                              /*   modifier_key*/           b1_modifierKey_arr[n7][3],
                              /*   key */                   b1_modifierKey_arr[n7][0],
                              /*   output */                new TextEncoder().encode(b1_modifierKey_arr[n7][4]),
                              );

                              if ((b1_modifierKey_arr[n7][4] !== undefined)
                                && (b1_modifierKey_arr[n7][4] !== "undefined")
                                && (b1_modifierKey_arr[n7][4] !== "")) {
                                object_array.push(rule_obj);
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
        } else {
          console.log("ERROR : some output characters can not be used in keyman \"",
            (jsonObj.keyboard['@_name'] + ".keylayout\""),
            "\"<keyMap index=\"", jsonObj.keyboard.keyMapSet[0].keyMap[i]['@_index'], "\">\" :",
            jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]);
          continue;
        }
      }
    }
    data_ukelele.arrayOf_Rules = object_array;
    return this.reviewRuleInputData(data_ukelele);
  }

  /**
    * @brief  member function to review data in array of Rules[] of data_ukelele: remove duplicate rules and mark first occurance of a rule in object_array
    * @param  data_ukelele: an object containing the name of the input file, an array of behaviours and an array of Rules
    * @return an object containing the name of the input file, an array of behaviours and the revised array of Rules[]
    */
  public reviewRuleInputData(data_ukelele: convert_object): convert_object {

    // check for duplicate C2 and C3 rules in object_array (e.g. [NCAPS RALT K_8]  >  dk(C12) ): create a separate array of unique rules,
    // then compare to object_array and mark first occurrence  of a rule in object_array

    let unique_dkB_count = 0;
    const list_of_unique_Text2_rules: string[][] = [];

    const object_array: rule_object[] = data_ukelele.arrayOf_Rules;

    //------------------------------------ C2: dk ----------------------------------
    // first rule is always unique
    object_array[0].unique_deadkey = unique_dkB_count;
    object_array[0].id_deadkey = unique_dkB_count;
    unique_dkB_count++;

    for (let i = 0; i < object_array.length; i++) {
      if ((object_array[i].modifier_deadkey !== "") && (object_array[i].deadkey !== "")) {
        let isFirstUsedHere_dk: boolean = true;

        // check if not used before
        for (let j = 0; j < i; j++) {
          if ((object_array[i].modifier_deadkey === object_array[j].modifier_deadkey)
            && (object_array[i].deadkey === object_array[j].deadkey)) {
            isFirstUsedHere_dk = isFirstUsedHere_dk && false;
          }
        }

        if (isFirstUsedHere_dk) {
          const ruleArray: string[] = [];
          object_array[i].unique_deadkey = unique_dkB_count;
          ruleArray.push(object_array[i].modifier_deadkey);
          ruleArray.push(object_array[i].deadkey);
          ruleArray.push(String(unique_dkB_count));
          unique_dkB_count++;
          list_of_unique_Text2_rules.push(ruleArray);
        }

      }
    }

    //----------------------------------- C3: prev-dk ----------------------------------
    let unique_dkA_count = 0;

    // first rule is always unique
    object_array[0].unique_prev_deadkey = unique_dkA_count;
    unique_dkA_count++;

    for (let i = 0; i < object_array.length; i++) {
      if ((object_array[i].modifier_prev_deadkey !== "") && (object_array[i].prev_deadkey !== "")) {
        let isFirstUsedHere_prev_dk: boolean = true;

        // check if not used before
        for (let j = 0; j < i; j++) {
          if ((object_array[i].modifier_prev_deadkey === object_array[j].modifier_prev_deadkey)
            && (object_array[i].prev_deadkey === object_array[j].prev_deadkey)) {
            isFirstUsedHere_prev_dk = isFirstUsedHere_prev_dk && false;
          }
        }

        // check if first part of C3 rule contains a rule that is already defined in C2
        if (isFirstUsedHere_prev_dk) {
          object_array[i].unique_prev_deadkey = unique_dkA_count;
          unique_dkA_count++;
          for (let k = 0; k < list_of_unique_Text2_rules.length; k++) {
            if ((list_of_unique_Text2_rules[k][0] === object_array[i].modifier_deadkey) && ((list_of_unique_Text2_rules[k][1] === object_array[i].deadkey))) {
              object_array[i].unique_deadkey = Number(list_of_unique_Text2_rules[k][2]);
            }
          }
        }

        if (isFirstUsedHere_prev_dk) {
          const ruleArray: string[] = [];
          object_array[i].unique_deadkey = unique_dkB_count;
          ruleArray.push(object_array[i].modifier_prev_deadkey);
          ruleArray.push(object_array[i].prev_deadkey);
          ruleArray.push(String(unique_dkB_count));
          unique_dkB_count++;
          list_of_unique_Text2_rules.push(ruleArray);

        }
      }
    }

    // loop through object_array and mark first occurence each rule of list_of_unique_Text2_rules
    for (let i = 0; i < object_array.length; i++) {
      for (let j = 0; j < list_of_unique_Text2_rules.length; j++) {
        if ((object_array[i].modifier_prev_deadkey === list_of_unique_Text2_rules[j][0]) && (object_array[i].prev_deadkey === list_of_unique_Text2_rules[j][1])) {
          object_array[i].id_prev_deadkey = Number(list_of_unique_Text2_rules[j][2]);
        }
        if ((object_array[i].modifier_deadkey === list_of_unique_Text2_rules[j][0]) && (object_array[i].deadkey === list_of_unique_Text2_rules[j][1])) {
          object_array[i].id_deadkey = Number(list_of_unique_Text2_rules[j][2]);
        }
      }
    }
    data_ukelele.arrayOf_Rules = object_array;
    return data_ukelele;
  }

  /**
   * @brief  member function to write data from object to a kmn file
   * @param  data_ukelele the array holding all keyboard data
   * @param  outputfilename the file that will be written; if no outputfilename is given an outputfilename will be created from data_ukelele.keylayout_filename
   * @return true if data has been written; false if not
   */
  public write(data_ukelele: convert_object): boolean {

    let data: string = "\n";

    // add top part of kmn file: STORES
    data += this.writeData_Stores(data_ukelele);

    // add bottom part of kmn file: RULES
    data += this.writeData_Rules(data_ukelele);

    try {
      this.callbacks.fs.writeFileSync(data_ukelele.kmn_filename, new TextEncoder().encode(data));
      return true;
    } catch (err) {
      console.log('ERROR writing kmn file:' + err.message);
      return false;
    }
  }

  /**
   * @brief  member function to create a kmn modifier from a keylayout modifier
   * @param  keylayout_modifier :string - modifier used in a .keylayout file
   * @param  isCAPSused  : boolean flag to indicate if CAPS is used in a keylayout file or not
   * @return string - a modifier value suitable for use in a .kmn-file
   */
  public create_kmn_modifier(keylayout_modifier: string, isCAPSused: boolean): string {
    let add_modifier: string = "";
    let kmn_modifier: string = "";
    let kmn_ncaps: string = "";

    const modifier_state: string[] = keylayout_modifier.split(" ");

    for (let i = 0; i < modifier_state.length; i++) {

      if (isCAPSused && (keylayout_modifier).toUpperCase().indexOf("CAPS?") > 0) {
        kmn_ncaps = " NCAPS ";
      }

      if (isCAPSused && (keylayout_modifier).toUpperCase().indexOf("CAPS") === -1) {
        kmn_ncaps = " NCAPS ";
      }

      // if we find a modifier containing a '?' e.g. SHIFT? => SHIFT occurs 0 or 1 times so it is not necessary.
      // If it is not necessary we don't write this modifier
      if (modifier_state[i].toUpperCase().includes('?') && (!modifier_state[i].toUpperCase().includes('CAPS?'))) {
        add_modifier = "";
      }

      // if we find caps? => caps is not necessary.
      // If caps is not necessary and isCAPSused we need to write out NCAPS.
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
        add_modifier = "CTRL ";
      }
      else if ((modifier_state[i].toUpperCase() === "LEFTCONTROL") || (modifier_state[i].toUpperCase() === "LCONTROL")) {
        add_modifier = "LCTRL ";
      }
      else if ((modifier_state[i].toUpperCase() === "RIGHTCONTROL") || (modifier_state[i].toUpperCase() === "RCONTROL")) {
        add_modifier = "RCTRL ";
      }

      else if ((modifier_state[i].toUpperCase() === "LEFTOPTION") || (modifier_state[i].toUpperCase() === "LOPTION")) {
        add_modifier = "LALT ";
      }
      else if ((modifier_state[i].toUpperCase() === "RIGHTOPTION") || (modifier_state[i].toUpperCase() === "ROPTION")) {
        add_modifier = "RALT ";
      }
      else if ((modifier_state[i].toUpperCase() === 'ANYOPTION') || (modifier_state[i].toUpperCase() === 'OPTION')) {
        add_modifier = "RALT ";
      }

      else {
        add_modifier = String(modifier_state[i]) + " ";
      }
      kmn_modifier += kmn_ncaps + add_modifier;
    }
    // remove duplicate and empty entries and make sure NCAPS is at the beginning
    const duplicate_modifier_array: string[] = kmn_modifier.split(" ").filter(item => item);

    const unique_modifier: string[] = duplicate_modifier_array.filter(function (item, pos, self) {
      return self.indexOf(item) === pos;
    });

    return unique_modifier.flat().toString().replace(/,/g, " ");
  }

  /**
   * @brief  member function to check if CAPS is used throughout a keylayout file or not
   * @param  keylayout_modifier the modifier string used in the .keylayout-file
   * @return kmn_modifier the modifier string used in the .kmn-file
   */
  public checkIfCapsIsUsed(keylayout_modifier: string[][]): boolean {
    return JSON.stringify(keylayout_modifier).toUpperCase().includes("CAPS");
  }

  /**
  * @brief  member function to check if a modifier can be used in keyman
  * @param  keylayout_modifier the modifier string used in the .keylayout-file
  * @return true if the modifier can be used in keyman; false if not
  */
  public isAcceptableKeymanModifier(keylayout_modifier: string): boolean {
    if (keylayout_modifier === null)
      return false;

    let iskKeymanModifier: boolean = true;
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
        iskKeymanModifier &&= true;
      } else {
        iskKeymanModifier &&= false;
      }
    }
    return iskKeymanModifier;
  }

  /**
    * @brief  member function to review rules for acceptable modifiers, duplicate or ambiguous rules and return an array containing possible warnings
    *         definition of comparisons e.g. 1-1, 2-4, 6-6
    *         see https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.pcz8rjyrl5ug
    * @param  rule : rule_object[] - an array of all rules
    * @param  index the index of a rule in array[rule]
    * @return a string[] containing possible warnings for a rule
    */
  public reviewRules(rule: rule_object[], index: number): string[] {

    const warningTextArray: string[] = Array(3).fill("");

    // ------------------------- check unavailable modifiers -------------------------

    if ((rule[index].rule_type === "C0") || (rule[index].rule_type === "C1")) {
      if (!this.isAcceptableKeymanModifier(rule[index].modifier_key)) {
        warningTextArray[2] = "unavailable modifier : ";
      }
    }

    else if (rule[index].rule_type === "C2") {
      if (!this.isAcceptableKeymanModifier(rule[index].modifier_deadkey)) {
        warningTextArray[1] = "unavailable modifier : ";
        warningTextArray[2] = "unavailable superior rule ( ["
          + rule[index].modifier_deadkey + " "
          + rule[index].deadkey
          + "]  >  dk(A"
          + rule[index].id_deadkey
          + ") ) : ";
      }
      if (!this.isAcceptableKeymanModifier(rule[index].modifier_key)) {
        warningTextArray[2] = "unavailable modifier : ";
      }
    }

    else if (rule[index].rule_type === "C3") {

      if (!this.isAcceptableKeymanModifier(rule[index].modifier_prev_deadkey)) {
        warningTextArray[0] = "unavailable modifier : ";
        warningTextArray[1] = "unavailable superior rule ( ["
          + rule[index].modifier_prev_deadkey + " "
          + rule[index].prev_deadkey
          + "]  >  dk(A"
          + rule[index].id_prev_deadkey
          + ") ) : ";

        warningTextArray[2] = "unavailable superior rule ( ["
          + rule[index].modifier_deadkey + " "
          + rule[index].deadkey
          + "]  >  dk(A"
          + rule[index].id_deadkey
          + ") ) : ";
      }

      if (!this.isAcceptableKeymanModifier(rule[index].modifier_deadkey)) {
        warningTextArray[1] = "unavailable modifier : ";
        warningTextArray[2] = "unavailable superior rule ( ["
          + rule[index].modifier_deadkey + " "
          + rule[index].deadkey
          + "]  >  dk(A"
          + rule[index].id_deadkey
          + ") ) : ";
      }

      if (!this.isAcceptableKeymanModifier(rule[index].modifier_key)) {
        warningTextArray[2] = "unavailable modifier : ";
      }
    }

    // ------------------------- check ambiguous/duplicate rules -------------------------

    if ((rule[index].rule_type === "C0") || (rule[index].rule_type === "C1")) {

      // 1-1: + [CAPS K_N]  > 'N' <-> + [CAPS K_N]  >  'A'
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

      // 1-1: + [CAPS K_N]  > 'N' <-> + [CAPS K_N]  >  'N'
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

      // 4-1: + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  'Ñ'
      const amb_4_1 = rule.filter((curr, idx) =>
        ((curr.rule_type === "C3"))
        && curr.modifier_prev_deadkey === rule[index].modifier_key
        && curr.prev_deadkey === rule[index].key
      );

      // 2-1: + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  'Ñ'
      const amb_2_1 = rule.filter((curr, idx) =>
        ((curr.rule_type === "C2"))
        && curr.modifier_deadkey === rule[index].modifier_key
        && curr.deadkey === rule[index].key
      );

      if (amb_4_1.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("ambiguous 4-1 rule: later: ["
            + amb_4_1[0].modifier_prev_deadkey
            + " "
            + amb_4_1[0].prev_deadkey
            + "]  >  dk(C"
            + amb_2_1[0].id_deadkey
            + ") ");
      }

      if (amb_2_1.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("ambiguous 2-1 rule: later: ["
            + amb_2_1[0].modifier_deadkey
            + " "
            + amb_2_1[0].deadkey
            + "]  >  dk(A"
            + amb_2_1[0].id_deadkey
            + ") ");
      }

      if (amb_1_1.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("ambiguous rule: earlier: ["
            + amb_1_1[0].modifier_key
            + " "
            + amb_1_1[0].key
            + "]  >  \'"
            + new TextDecoder().decode(amb_1_1[0].output)
            + "\' ");
      }

      if (dup_1_1.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("duplicate rule: earlier: ["
            + dup_1_1[0].modifier_key
            + " "
            + dup_1_1[0].key
            + "]  >  \'"
            + new TextDecoder().decode(dup_1_1[0].output)
            + "\' ");
      }
    }

    if (rule[index].rule_type === "C2") {
      // 2-2: + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(C3)
      const amb_2_2 = rule.filter((curr, idx) =>
        curr.rule_type === "C2"
        && curr.modifier_deadkey === rule[index].modifier_deadkey
        && curr.deadkey === rule[index].deadkey
        && curr.id_deadkey !== rule[index].id_deadkey
        && idx < index
      );

      // 2-2: + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(C11)
      const dup_2_2 = rule.filter((curr, idx) =>
        curr.rule_type === "C2"
        && curr.modifier_deadkey === rule[index].modifier_deadkey
        && curr.deadkey === rule[index].deadkey
        && curr.id_deadkey === rule[index].id_deadkey
        && idx < index
      );

      //3-3: dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'B'
      const amb_3_3 = rule.filter((curr, idx) =>
        (curr.rule_type === "C2")
        && curr.id_deadkey === rule[index].id_deadkey
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output)
        && idx < index
      );

      //3-3: dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'Ã'
      const dup_3_3 = rule.filter((curr, idx) =>
        (curr.rule_type === "C2")
        && curr.id_deadkey === rule[index].id_deadkey
        && rule[index].unique_deadkey === 0
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output)
        && idx < index
      );

      // 4-2: + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(B11)
      const amb_4_2 = rule.filter((curr, idx) =>
        ((curr.rule_type === "C3"))
        && curr.modifier_prev_deadkey === rule[index].modifier_deadkey
        && curr.prev_deadkey === rule[index].deadkey
        && curr.id_prev_deadkey === rule[index].id_deadkey
      );

      if (amb_2_2.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("ambiguous rule: earlier: ["
            + amb_2_2[0].modifier_deadkey
            + " "
            + amb_2_2[0].deadkey
            + "]  >  dk(C"
            + amb_2_2[0].id_deadkey
            + ") ");
      }

      if (dup_2_2.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("duplicate rule: earlier: ["
            + dup_2_2[0].modifier_deadkey
            + " "
            + dup_2_2[0].deadkey
            + "]  >  dk(C"
            + dup_2_2[0].id_deadkey
            + ") ");
      }

      if (amb_3_3.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("ambiguous rule: earlier: dk(A"
            + amb_3_3[0].id_deadkey
            + ") + ["
            + amb_3_3[0].modifier_key
            + " "
            + amb_3_3[0].key
            + "]  >  \'"
            + new TextDecoder().decode(amb_3_3[0].output)
            + "\' ");
      }

      if (dup_3_3.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("duplicate rule: earlier: dk(A"
            + dup_3_3[0].id_deadkey
            + ") + ["
            + dup_3_3[0].modifier_key
            + " "
            + dup_3_3[0].key
            + "]  >  \'"
            + new TextDecoder().decode(dup_3_3[0].output)
            + "\' ");
      }

      if (amb_4_2.length > 0) {
        warningTextArray[0] = warningTextArray[0]
          + ("ambiguous rule: later: ["
            + amb_4_2[0].modifier_prev_deadkey
            + " "
            + amb_4_2[0].prev_deadkey
            + "]  >  dk(C"
            + amb_4_2[0].id_prev_deadkey
            + ") ");
      }
    }

    if (rule[index].rule_type === "C3") {

      // 2-4 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(B11)
      const amb_2_4 = rule.filter((curr, idx) =>
        ((curr.rule_type === "C2"))
        && curr.modifier_deadkey === rule[index].modifier_prev_deadkey
        && curr.deadkey === rule[index].prev_deadkey
        && curr.id_deadkey === rule[index].id_prev_deadkey
      );

      // 6-3  dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'B'
      const amb_6_3 = rule.filter((curr, idx) =>
        (curr.rule_type === "C2")
        && curr.id_prev_deadkey === rule[index].id_prev_deadkey
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && (new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output))
      );

      // 6-3 dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'Ã'
      const dup_6_3 = rule.filter((curr, idx) =>
        (curr.rule_type === "C2")
        && curr.id_prev_deadkey === rule[index].id_prev_deadkey
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output)
      );

      // 4-4 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(C1)
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
      );

      // 5-5
      const amb_5_5 = rule.filter((curr, idx) =>
        (curr.rule_type === "C3")
        && curr.modifier_key === rule[index].modifier_key
        && curr.key === rule[index].key
        && curr.id_deadkey === rule[index].id_deadkey
        && (new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output))
        && idx < index
      );

      // 5-5
      const dup_5_5 = rule.filter((curr, idx) =>
        (curr.rule_type === "C3")
        && curr.modifier_deadkey === rule[index].modifier_deadkey
        && curr.deadkey === rule[index].deadkey
        && curr.id_prev_deadkey === rule[index].id_prev_deadkey
        && curr.id_deadkey === rule[index].id_deadkey
        && rule[index].unique_deadkey === 0
      );

      // 6-6 dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'B'
      const amb_6_6 = rule.filter((curr, idx) =>
        (curr.rule_type === "C3")
        && curr.id_prev_deadkey === rule[index].id_prev_deadkey
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

      if (amb_2_4.length > 0) {
        warningTextArray[0] = warningTextArray[0]
          + ("ambiguous rule: earlier: ["
            + amb_2_4[0].modifier_deadkey
            + " "
            + amb_2_4[0].deadkey
            + "]  >  dk(A"
            + amb_2_4[0].id_deadkey
            + ") ");
      }

      if (amb_6_3.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("ambiguous rule: earlier: dk(C"
            + amb_6_3[0].id_deadkey
            + ") + ["
            + amb_6_3[0].modifier_key
            + " "
            + amb_6_3[0].key
            + "]  >  \'"
            + new TextDecoder().decode(amb_6_3[0].output)
            + "\' ");
      }

      if (dup_6_3.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("duplicate rule: earlier: dk(C"
            + dup_6_3[0].id_deadkey
            + ") + ["
            + dup_6_3[0].modifier_key
            + " "
            + dup_6_3[0].key
            + "]  >  \'"
            + new TextDecoder().decode(dup_6_3[0].output)
            + "\' ");
      }

      if (amb_4_4.length > 0) {
        warningTextArray[0] = warningTextArray[0]
          + ("ambiguous rule: earlier: ["
            + amb_4_4[0].modifier_prev_deadkey
            + " "
            + amb_4_4[0].prev_deadkey
            + "]  >  dk(C"
            + amb_4_4[0].id_prev_deadkey
            + ") ");
      }

      if (dup_4_4.length > 0) {
        warningTextArray[0] = warningTextArray[0]
          + ("duplicate rule: earlier: ["
            + dup_4_4[0].modifier_prev_deadkey
            + " "
            + dup_4_4[0].prev_deadkey
            + "]  >  dk(C"
            + dup_4_4[0].id_prev_deadkey
            + ") ");
      }

      if (amb_5_5.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("ambiguous rule: earlier: dk(B"
            + amb_5_5[0].id_prev_deadkey
            + ") + ["
            + amb_5_5[0].modifier_deadkey
            + " "
            + amb_5_5[0].deadkey
            + "]  >  dk(B"
            + amb_5_5[0].id_deadkey
            + ") ");
      }

      if (dup_5_5.length > 0) {
        warningTextArray[1] = warningTextArray[1]
          + ("duplicate rule: earlier: dk(B"
            + dup_5_5[0].id_prev_deadkey
            + ") + ["
            + dup_5_5[0].modifier_deadkey
            + " "
            + dup_5_5[0].deadkey
            + "]  >  dk(B"
            + dup_5_5[0].id_deadkey
            + ") ");
      }

      if (amb_6_6.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("ambiguous rule: earlier: dk(B"
            + amb_6_6[0].id_deadkey
            + ") + ["
            + amb_6_6[0].modifier_key
            + " "
            + amb_6_6[0].key
            + "]  >  \'"
            + new TextDecoder().decode(amb_6_6[0].output)
            + "\' ");
      }

      if (dup_6_6.length > 0) {
        warningTextArray[2] = warningTextArray[2]
          + ("duplicate rule: earlier: dk(B"
            + dup_6_6[0].id_deadkey
            + ") + ["
            + dup_6_6[0].modifier_key
            + " "
            + dup_6_6[0].key
            + "]  >  \'"
            + new TextDecoder().decode(dup_6_6[0].output)
            + "\' ");
      }
    }
    // In rare cases a rule might not be written out therefore we need to inform the user
    const extra_warning = "PLEASE CHECK THE FOLLOWING RULE AS IT WILL NOT BE WRITTEN !  ";

    if (warningTextArray[0] !== "") {
      warningTextArray[0] = "c WARNING: " + warningTextArray[0] + "here: ";

      if ((warningTextArray[0].indexOf("earlier:") > 0) && (warningTextArray[0].indexOf("later:") > 0)) {
        warningTextArray[0] = warningTextArray[0] + extra_warning;
      }
    }
    if (warningTextArray[1] !== "") {
      warningTextArray[1] = "c WARNING: " + warningTextArray[1] + "here: ";

      if ((warningTextArray[1].indexOf("earlier:") > 0) && (warningTextArray[1].indexOf("later:") > 0)) {
        warningTextArray[1] = warningTextArray[1] + extra_warning;
      }
    }

    if (warningTextArray[2] !== "") {
      warningTextArray[2] = "c WARNING: " + warningTextArray[2] + "here: ";

      if ((warningTextArray[2].indexOf("earlier:") > 0) && (warningTextArray[2].indexOf("later:") > 0)) {
        warningTextArray[2] = warningTextArray[2] + extra_warning;
      }
    }

    return warningTextArray;
  }


  /**
   * @brief  member function to create data for stores that will be printed to the resulting kmn file
   * @param  data_ukelele an object containing all data read from a .keylayout file
   * @return string -  all stores to be printed
   */
  public writeData_Stores(data_ukelele: convert_object): string {

    let data: string = "";
    data += "c ......................................................................\n";
    data += "c ......................................................................\n";
    data += "c Keyman keyboard generated by kmn-convert\n";
    data += "c from Ukelele file: " + data_ukelele.keylayout_filename + "\n";
    data += "c ......................................................................\n";
    data += "c ......................................................................\n";
    data += "\n";

    data += "store(&VERSION) \'10.0\'\n";
    data += "store(&TARGETS) \'any\'\n";
    data += "store(&KEYBOARDVERSION) \'1.0\'\n";
    data += "store(&COPYRIGHT) '© 2024 SIL International\'\n";

    data += "\n";
    data += "begin Unicode > use(main)\n\n";
    data += "group(main) using keys\n\n";

    data += "\n";
    return data;
  }

  /**
   * @brief  member function to create data from rules that will be printed to the resulting kmn file
   * @param  data_ukelele an object containing all data read from a .keylayout file
   * @return string -  all rules to be printed
   */
  public writeData_Rules(data_ukelele: convert_object): string {

    let data: string = "";

    // filter array of all rules and remove duplicates
    const unique_data_Rules: rule_object[] = data_ukelele.arrayOf_Rules.filter((curr) => {
      return (!(curr.output === new TextEncoder().encode("") || curr.output === undefined)
        && (curr.key !== "")
        && ((curr.rule_type === "C0")
          || (curr.rule_type === "C1")
          || (curr.rule_type === "C2" && (curr.deadkey !== ""))
          || (curr.rule_type === "C3" && (curr.deadkey !== "") && (curr.prev_deadkey !== "")))
      );
    }).reduce((unique, o) => {
      if (!unique.some((obj: {
        modifier_prev_deadkey: string; prev_deadkey: string;
        modifier_deadkey: string; deadkey: string;
        modifier_key: string; key: string;
        rule_type: string;
        output: Uint8Array;
      }) =>
        new TextDecoder().decode(obj.output) === new TextDecoder().decode(o.output)

        && obj.output !== new TextEncoder().encode("")

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

    //................................................ C0 C1 ................................................................

    for (let k = 0; k < unique_data_Rules.length; k++) {

      if ((unique_data_Rules[k].rule_type === "C0") || (unique_data_Rules[k].rule_type === "C1")) {

        // lookup key nr of the key which is being processed
        let keyNr: number = 0;
        for (let j = 0; j < KeylayoutToKmnConverter.USED_KEYS_COUNT; j++) {
          if (this.map_UkeleleKC_To_VK(j) === unique_data_Rules[k].key) {
            keyNr = j;
            break;
          }
        }

        // skip keyNr 48 (K_TAB) and 36 (K_ENTER)
        if ((keyNr === 48) || (keyNr === 36)) {
          continue;
        }

        //---------------------------------------------------------------------------------------------

        // add a line after rules of each key
        if ((k > 1) && (unique_data_Rules[k - 1].key !== unique_data_Rules[k].key) && (unique_data_Rules[k - 1].rule_type === unique_data_Rules[k].rule_type)) {
          data += '\n';
        }

        const warn_text = this.reviewRules(unique_data_Rules, k);

        const output_character = new TextDecoder().decode(unique_data_Rules[k].output);
        const output_character_unicode = this.convertToUnicodeCodePoint(output_character);

        // if we are about to print a unicode codepoint instead of a single character we need to check if it is a control character
        if ((output_character_unicode.length > 1)
          && (Number("0x" + output_character_unicode.substring(2, output_character_unicode.length)) < KeylayoutToKmnConverter.MAX_CTRL_CHARACTER)) {
          if (warn_text[2] == "")
            warn_text[2] = warn_text[2] + "c WARNING: use of a control character ";
          else
            warn_text[2] = warn_text[2] + " Use of a control character ";
        }

        // add a warning in front of rules in case unavailable modifiers or ambiguous rules are used
        // if warning contains duplicate rules we do not write out the entire rule
        // (even if there are other warnings for the same rule) since that rule had been written before
        if ((warn_text[2].indexOf("duplicate") < 0)) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warn_text[2].length > 0)) {
            warningTextToWrite = warn_text[2];
          }

          if (!((warn_text[2].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            data += warningTextToWrite
              + "+ ["
              + (unique_data_Rules[k].modifier_key + ' ' + unique_data_Rules[k].key).trim()
              + `]  > \'`
              + output_character_unicode
              + '\'\n';
          }
        }
      }
    }

    //................................................ C2 ...................................................................
    for (let k = 0; k < unique_data_Rules.length; k++) {

      if (unique_data_Rules[k].rule_type === "C2") {

        const warn_text = this.reviewRules(unique_data_Rules, k);

        const output_character = new TextDecoder().decode(unique_data_Rules[k].output);
        const output_character_unicode = this.convertToUnicodeCodePoint(output_character);

        // if we are about to print a unicode codepoint instead of a single character we need to check if it is a control character
        if ((output_character_unicode.length > 1)
          && (Number("0x" + output_character_unicode.substring(2, output_character_unicode.length)) < KeylayoutToKmnConverter.MAX_CTRL_CHARACTER)) {
          if (warn_text[2] == "")
            warn_text[2] = warn_text[2] + "c WARNING: use of a control character ";
          else
            warn_text[2] = warn_text[2] + "; Use of a control character ";
        }

        // add a warning in front of rules in case unavailable modifiers or ambiguous rules are used
        // if warning contains duplicate rules we do not write out the entire rule
        // (even if there are other warnings for the same rule) since that rule had been written before
        if ((warn_text[1].indexOf("duplicate") < 0)) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warn_text[1].length > 0)) {
            warningTextToWrite = warn_text[1];
          }

          if (!((warn_text[1].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            data += warningTextToWrite
              + "+ [" + (unique_data_Rules[k].modifier_deadkey + " "
                + unique_data_Rules[k].deadkey).trim()
              + "]  >  dk(A" + String(unique_data_Rules[k].id_deadkey)
              + ")\n";
          }
        }

        if ((warn_text[2].indexOf("duplicate") < 0)) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warn_text[2].length > 0)) {
            warningTextToWrite = warn_text[2];
          }

          if (!((warn_text[2].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            data += warningTextToWrite
              + "dk(A"
              + (String(unique_data_Rules[k].id_deadkey) + ") + ["
                + unique_data_Rules[k].modifier_key).trim()
              + " "
              + unique_data_Rules[k].key + "]  >  \'"
              + output_character_unicode
              + "\'\n";
          }
          data += "\n";
        }
      }
    }

    //................................................ C3 ...................................................................

    for (let k = 0; k < unique_data_Rules.length; k++) {
      if (unique_data_Rules[k].rule_type === "C3") {

        const warn_text = this.reviewRules(unique_data_Rules, k);

        const output_character = new TextDecoder().decode(unique_data_Rules[k].output);
        const output_character_unicode = this.convertToUnicodeCodePoint(output_character);

        // if we are about to print a unicode codepoint instead of a single character we need to check if a control character is to be used
        if ((output_character_unicode.length > 1)
          && (Number("0x" + output_character_unicode.substring(2, output_character_unicode.length)) < KeylayoutToKmnConverter.MAX_CTRL_CHARACTER)) {
          if (warn_text[2] == "")
            warn_text[2] = warn_text[2] + "c WARNING: use of a control character ";
          else
            warn_text[2] = warn_text[2] + "; Use of a control character ";
        }

        // add a warning in front of rules in case unavailable modifiers or ambiguous rules are used
        // if warning contains duplicate rules we do not write out the entire rule
        // (even if there are other warnings for the same rule) since that rule had been written before
        if ((warn_text[0].indexOf("duplicate") < 0)) {


          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warn_text[0].length > 0)) {
            warningTextToWrite = warn_text[0];
          }

          if (!((warn_text[0].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            data += warningTextToWrite
              + "+ ["
              + (unique_data_Rules[k].modifier_prev_deadkey + " "
                + unique_data_Rules[k].prev_deadkey).trim()
              + "]   >   dk(A"
              + String(unique_data_Rules[k].id_prev_deadkey) + ")\n";
          }
        }




        if ((warn_text[1].indexOf("duplicate") < 0)) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warn_text[1].length > 0)) {
            warningTextToWrite = warn_text[1];
          }

          if (!((warn_text[1].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            data += warningTextToWrite
              + "dk(A" + (String(unique_data_Rules[k].id_prev_deadkey) + ")  + ["
                + unique_data_Rules[k].modifier_deadkey).trim()
              + " "
              + unique_data_Rules[k].deadkey
              + "]  >  dk(B"
              + String(unique_data_Rules[k].id_deadkey)
              + ")\n";
          }
        }

        if ((warn_text[2].indexOf("duplicate") < 0)) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warn_text[2].length > 0)) {
            warningTextToWrite = warn_text[2];
          }

          if (!((warn_text[2].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            data += warningTextToWrite + "dk(B"
              + (String(unique_data_Rules[k].id_deadkey)
                + ") + ["
                + unique_data_Rules[k].modifier_key).trim()
              + " "
              + unique_data_Rules[k].key
              + "]  >  \'"
              + output_character_unicode
              + "\'\n";
          }
        }

        if ((warn_text[0].indexOf("duplicate") < 0) || (warn_text[1].indexOf("duplicate") < 0) || (warn_text[2].indexOf("duplicate") < 0)) {
          data += "\n";
        }
      }
    }
    return data;
  }

  /**
   * @brief  member function to map Ukelele keycodes to Windows Keycodes
   * @param  pos Ukelele (=mac) keycodes
   * @return keycode on a Windows Keyboard
   */
  public map_UkeleleKC_To_VK(pos: number): string {

    // ukelele KC  -->  VK_US
    if (pos === 0x0A) return "K_BKQUOTE";         /* ^ */
    else if (pos === 0x12) return "K_1";          /* 1 */
    else if (pos === 0x13) return "K_2";          /* 2 */
    else if (pos === 0x14) return "K_3";          /* 3 */
    else if (pos === 0x15) return "K_4";          /* 4 */
    else if (pos === 0x17) return "K_5";          /* 5 */
    else if (pos === 0x16) return "K_6";          /* 6 */
    else if (pos === 0x1A) return "K_7";          /* 7 */
    else if (pos === 0x1C) return "K_8";          /* 8 */
    else if (pos === 0x19) return "K_9";          /* 9 */
    else if (pos === 0x1D) return "K_0";          /* 0 */
    else if (pos === 0x1B) return "K_HYPHEN";     /* ß */
    else if (pos === 0x18) return "K_EQUAL";      /* ´ */

    else if (pos === 0x0C) return "K_Q";          /* Q */
    else if (pos === 0x0D) return "K_W";          /* W */
    else if (pos === 0x0E) return "K_E";          /* E */
    else if (pos === 0x0F) return "K_R";          /* R */
    else if (pos === 0x11) return "K_T";          /* T */
    else if (pos === 0x10) return "K_Y";          /* Y */
    else if (pos === 0x20) return "K_U";          /* U */
    else if (pos === 0x22) return "K_I";          /* I */
    else if (pos === 0x1F) return "K_O";          /* O */
    else if (pos === 0x23) return "K_P";          /* P */
    else if (pos === 0x21) return "K_LBRKT";      /* [ */
    else if (pos === 0x1E) return "K_RBRKT";      /* ] */
    else if (pos === 0x31) return "K_SPACE";      /* \ */
    else if (pos === 0x2A) return "K_BKSLASH";    /* \ */   // 42 for ISO  correct??
    // else if (pos === 0x30) return "K_?C1"     /* \ */   // 48 for ANSI  correct??

    else if (pos === 0x00) return "K_A";          /* A */
    else if (pos === 0x01) return "K_S";          /* S */
    else if (pos === 0x02) return "K_D";          /* D */
    else if (pos === 0x03) return "K_F";          /* F */
    else if (pos === 0x05) return "K_G";          /* G */
    else if (pos === 0x04) return "K_H";          /* H */
    else if (pos === 0x26) return "K_J";          /* J */
    else if (pos === 0x28) return "K_K";          /* K */
    else if (pos === 0x25) return "K_L";          /* L */
    else if (pos === 0x29) return "K_COLON";      /* : */
    else if (pos === 0x27) return "K_QUOTE";      /* " */

    else if (pos === 0x23) return "K_oE2";        /* | */
    else if (pos === 0x06) return "K_Z";          /* Z */
    else if (pos === 0x07) return "K_X";          /* X */
    else if (pos === 0x08) return "K_C";          /* C */
    else if (pos === 0x09) return "K_V";          /* V */
    else if (pos === 0x0B) return "K_B";          /* B */
    else if (pos === 0x2D) return "K_N";          /* N */
    else if (pos === 0x2E) return "K_M";          /* M */
    else if (pos === 0x2B) return "K_COMMA";      /* , */
    else if (pos === 0x2F) return "K_PERIOD";     /* . */
    else if (pos === 0x2C) return "K_SLASH";      /* / */

    else if (pos === 0x24) return "K_ENTER";
    else return "";
  }

  /**
     * @brief  member function to return the unicode value
     * @param  instr the string that will converted
     * @return hexadecimal value of a character
     */
  public getHexFromString(instr: string): string {
    return Number(instr).toString(16).slice(-6).toUpperCase().padStart(4, "0");
  }

  /**
      * @brief  member function to convert a numeric character reference to a unicode codepoint
      * @param  instr the value that will converted
      * @return a unicode codepoint if instr is a numeric character reference
      *         instr if instr is not a numeric character reference
      */
  public convertToUnicodeCodePoint(instr: string): string {

    if (instr.substring(0, 3) === "&#x") {
      const num_length = instr.length - instr.indexOf("x") - 1;
      const num_str = instr.substring(instr.indexOf("x") + 1, instr.length - 1);
      return ("U+" + num_str.slice(-num_length).padStart(4, "0"));
    }

    // if not hex: convert to hex
    if ((instr.substring(0, 2) === "&#")) {
      const num_length = instr.length - instr.indexOf("#") - 1;
      const num_str = instr.substring(instr.indexOf("#") + 1, instr.length - 1);
      return ("U+" + this.getHexFromString(num_str.slice(-num_length)).padStart(4, "0"));
    }
    else
      return instr;
  }

  /**
  * @brief  member function to return an index for a given actionID
  * @param  data   :any - an object containing all data read from a .keylayout file
  * @param  search :string - value 'id' to be found
  * @return a number specifying the index of an actionId
  */
  public get_ActionIndex__From__ActionId(data: any, search: string): number {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      if (data.keyboard.actions.action[i]['@_id'] === search) {
        return i;
      }
    }
    return 0;
  }

  /**
  * @brief  member function to  find the actionID of a certain state-next pair
  * @param  data   :any an object containing all data read from a .keylayout file
  * @param  search :string value 'next' to be found
  * @return a string containing the actionId of a certain state-next pair
  */
  public get_ActionID__From__ActionNext(data: any, search: string): string {
    if (search !== "none") {
      for (let i = 0; i < data.keyboard.actions.action.length; i++) {
        for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
          if (data.keyboard.actions.action[i].when[j]['@_next'] === search) {
            return data.keyboard.actions.action[i]['@_id'];
          }
        }
      }
    }
    return "";
  }

  /**
   * @brief  member function to create an array of modifier behaviours for a given keycode in [keycode,modifier]
   * @param  data    : any - an object containing all data read from a .keylayout file
   * @param  search  : (string | number)[][] - an array[keycode,modifier]  to be found
   * @return an array: string[] containing modifiers
   */
  public get_Modifier_array__From__KeyModifier_array(data: any, search: (string | number)[][]): string[] {
    const mapIndexArray_2D: string[] = [];
    for (let i = 0; i < search.length; i++) {
      mapIndexArray_2D.push(data[search[i][1]]);
    }
    return mapIndexArray_2D;
  }

  /**
   * @brief  member function to find the output for a certain actionID for state 'none'
   * @param  data   :any an object containing all data read from a .keylayout file
   * @param  search :string an actionId to be found
   * @return a string containing the output character
   */
  public get_Output__From__ActionId_None(data: any, search: string): string {
    let OutputValue: string = "";

    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      if (data.keyboard.actions.action[i]['@_id'] === search) {
        for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
          if (data.keyboard.actions.action[i].when[j]['@_state'] === "none") {
            OutputValue = data.keyboard.actions.action[i].when[j]['@_output'];
          }
        }
      }
    }
    return OutputValue;
  }

  /**
  * @brief  member function to return array of [Keycode,Keyname,actionId,actionIDIndex, output] for a given actionID in of [ actionID,state,output]
  * @param  data   :any - an object containing all data read from a .keylayout file
  * @param  search :string[][] - array of [ actionID,state,output]
  * @return a string[][] containing [Keycode,Keyname,actionId,actionIDIndex, output]
  */
  public get_KeyActionOutput_array__From__ActionStateOutput_array(data: any, search: string[][]): string[][] {

    if ((search === undefined) || (search === null))
      return [];

    const returnarray2D: string[][] = [];
    for (let k = 0; k < search.length; k++) {
      for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
        for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
          const returnarray: string[] = [];
          if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search[k][0] &&
            data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'] < KeylayoutToKmnConverter.USED_KEYS_COUNT) {
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']);
            returnarray.push(this.map_UkeleleKC_To_VK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])));
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']);
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i]['@_index']);
            returnarray.push(search[k][2]);
          }
          if (returnarray.length > 0) {
            returnarray2D.push(returnarray);
          }
        }
      }
    }
    return returnarray2D;
  }

  /**
   * @brief  member function to get an array of all actionId-output pairs for a certain state
   * @param  data    : any an object containing all data read from a .keylayout file
   * @param  search  : string a 'state' to be found
   * @return an array: string[][] containing all [actionId, state, output] for a certain state
   */
  public get_ActionStateOutput_array__From__ActionState(data: any, search: string): string[][] {
    const returnarray2D: string[][] = [];

    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
        const returnarray: string[] = [];
        if ((data.keyboard.actions.action[i].when[j]['@_state'] === search)) {
          returnarray.push(data.keyboard.actions.action[i]['@_id']);
          returnarray.push(data.keyboard.actions.action[i].when[j]['@_state']);
          returnarray.push(data.keyboard.actions.action[i].when[j]['@_output']);
        }
        if (returnarray.length > 0) {
          returnarray2D.push(returnarray);
        }
      }
    }
    return returnarray2D;
  }

  /**
   * @brief  member function to create an 2D array of [KeyName,actionId,behaviour,modifier,output]
   * @param  data    : any an object containing all data read from a .keylayout file
   * @param  search  : array of [keycode,keyname,actionId,behaviour,output] to be found
   * @param  isCAPSused  : boolean flag to indicate if CAPS is used in a keylayout file or not
   * @return an array: string[][] containing [KeyName,actionId,behaviour,modifier,output]
   */
  public get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(data: any, search: (boolean | string)[][], isCAPSused: boolean): string[][] {
    const returnarray: string[][] = [];

    if (!((search === undefined) || (search === null) || (search.length === 0))) {
      for (let i = 0; i < search.length; i++) {
        const behaviour: number = Number(search[i][3]);

        for (let j = 0; j < data.keyboard.modifierMap.keyMapSelect[behaviour].modifier.length; j++) {
          const returnarray1D: string[] = [];
          // KeyName
          returnarray1D.push(String(search[i][1]));
          // actionId
          returnarray1D.push(String(search[i][2]));
          // behaviour
          returnarray1D.push(String(search[i][3]));
          // modifier
          returnarray1D.push(String(this.create_kmn_modifier(data.keyboard.modifierMap.keyMapSelect[behaviour].modifier[j]['@_keys'], isCAPSused)));
          // output
          returnarray1D.push(String(search[i][4]));

          if (returnarray1D.length > 0) {
            returnarray.push(returnarray1D);
          }
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
    return unique_returnarray;
  }

  /**
   * @brief  member function to create an array of [actionID, output, behaviour,keyname,modifier] for a given actionId
   * @param  data    : any - an object containing all data read from a .keylayout file
   * @param  modi    : any - an array of modifiers
   * @param  search  : string - an actionId to be found
   * @param  outchar  : string - the output character
   * @param  isCAPSused  : boolean - flag to indicate if CAPS is used in a keylayout file or not
   * @return an array: string[][] containing [actionID,output,actionID, behaviour,keyname,modifier]
   */
  public get_ActionOutputBehaviourKeyModi_From__ActionIDStateOutput(data: any, modi: any, search: string, outchar: string, isCapsused: boolean): string[][] {
    const returnarray2D: string[][] = [];

    if ((search === "") || (search === undefined) || !((isCapsused === true) || (isCapsused === false))) {
      return [];
    }

    // loop behaviors (in ukelele it is possible to define multiple modifier combinations that behave in the same)
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < KeylayoutToKmnConverter.USED_KEYS_COUNT; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          for (let k = 0; k < modi[data.keyboard.keyMapSet[0].keyMap[i]['@_index']].length; k++) {
            const returnarray: string[] = [];
            const behaviour: string = data.keyboard.keyMapSet[0].keyMap[i]['@_index'];
            const modifierkmn: string = this.create_kmn_modifier(modi[behaviour][k], isCapsused);
            const keyName: string = this.map_UkeleleKC_To_VK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']));

            returnarray.push(search);
            returnarray.push(outchar);
            returnarray.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action']);
            returnarray.push(behaviour);
            returnarray.push(keyName);
            returnarray.push(modifierkmn);

            if (returnarray.length > 0) {
              returnarray2D.push(returnarray);
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

    return unique_returnarray;
  }

  /**
   * @brief  member function to create an array of [keycode,behaviour] for a given actionId
   * @param  data    : any - an object containing all data read from a .keylayout file
   * @param  search  : string - an actionId to be found
   * @return an array: number[][] containing [keycode,behaviour]
   */
  public get_KeyModifier_array__From__ActionID(data: any, search: string): number[][] {
    const mapIndexArray_2D: number[][] = [];
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < KeylayoutToKmnConverter.USED_KEYS_COUNT; j++) {
        const mapIndexArrayperKey: number[] = [];
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          mapIndexArrayperKey.push(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']);
          mapIndexArrayperKey.push(i);
        }
        if (mapIndexArrayperKey.length > 0) {
          mapIndexArray_2D.push(mapIndexArrayperKey);
        }
      }
    }
    return mapIndexArray_2D;
  }
}

/**
 * @brief  class for all storing a rule containing data for key, deadkey, previous deadkey, output)
 */
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
