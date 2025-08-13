/*
 * Keyman is 2025 copyright (C) SIL International. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Convert macOS/Ukelele .keylayout files to Keyman .kmn
 *
 */

import { CompilerCallbacks, CompilerOptions, KeymanCompilerResult, } from "@keymanapp/developer-utils";
import { KmnFileWriter } from './kmn-file-writer.js';
import { KeylayoutFileReader } from './keylayout-file-reader.js';
import { ConverterMessages } from '../converter-messages.js';
import { ConverterArtifacts } from "../converter-artifacts.js";
import { ConverterToKmnArtifacts } from "../converter-artifacts.js";

export interface ConverterResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link Converter.write}
   */
  artifacts: ConverterArtifacts;
};

export interface ConverterToKmnResult extends ConverterResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link Converter.write}
   */
  artifacts: ConverterToKmnArtifacts;
};


export interface convert_object {
  keylayout_filename: string,
  kmn_filename: string,
  arrayOf_Modifiers: string[][],
  arrayOf_Rules: Rule[],
};

export class KeylayoutToKmnConverter {

  static readonly INPUT_FILE_EXTENSION = '.keylayout';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';
  static readonly USED_KEYS_COUNT = 49;               // we use key Nr 0 (A) -> key Nr 49 (Space)
  static readonly MAX_CTRL_CHARACTER = 32;
  static readonly SKIP_COMMENTED_LINES = false;

  private options: CompilerOptions;

  async init(callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {
    this.options = { ...options };
    this.callbacks = callbacks;
    return true;
  }

  constructor(private callbacks: CompilerCallbacks, options: CompilerOptions) { };

  /**
   * @brief  member function to run read/convert/write
   * @param  inputFilename the ukelele .keylayout-file to be converted
   * @param  outputFilename the resulting keyman .kmn-file
   * @return null on success
   */
  async run(inputFilename: string, outputFilename?: string): Promise<ConverterToKmnResult> {

    if (!inputFilename) {
      this.callbacks.reportMessage(ConverterMessages.Error_FileNotFound({ inputFilename }));
      return null;
    }

    outputFilename = outputFilename ?? inputFilename.replace(/\.keylayout$/, '.kmn');

    const KeylayoutReader = new KeylayoutFileReader(this.callbacks/*, this.options*/);
    const jsonO: KeylayoutXMLSourceFile = KeylayoutReader.read(inputFilename);

    try {
      if (!KeylayoutReader.validate(jsonO)) {
        return null;
      }
    } catch (e) {
      this.callbacks.reportMessage(ConverterMessages.Error_InvalidFile({ errorText: e.toString() }));
      return null;
    }

    if (!jsonO) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToRead({ inputFilename }));
      return null;
    }

    const outArray: convert_object = await this.convert(jsonO, outputFilename);
    if (!outArray) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToConvert({ inputFilename }));
      return null;
    }

    const kmnFileWriter = new KmnFileWriter(this.callbacks, this.options);

    // _S2 still write to file - will be removed later
    const out_text_ok: boolean = kmnFileWriter.writeToFile(outArray);
    if (!out_text_ok) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToWrite({ outputFilename }));
      return null;
    }

    // write to object/ConverterToKmnResult
    const out_Uint8: Uint8Array = kmnFileWriter.write(outArray);
    const Result_toBeReturned: ConverterToKmnResult = {
      artifacts: {
        kmn: { data: out_Uint8, filename: outputFilename }
      }
    };

    if (!out_Uint8) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToWrite({ outputFilename }));
      return null;
    }
    return Result_toBeReturned;
  }

  /**
   * @brief  member function to read filename and behaviour of a json object into a convert_object
   * @param  jsonObj containing filename, behaviour and rules of a json object
   * @return an convert_object containing all data ready to print out
   */
  private convert(jsonObj: any, outputfilename: string): convert_object {
    const modifierBehavior: string[][] = [];           // modifier for each behaviour
    const rules: Rule[] = [];                          // an array of data for a kmn rule

    const data_object: convert_object = {
      keylayout_filename: "",
      kmn_filename: "",
      arrayOf_Modifiers: [],
      arrayOf_Rules: []
    };

    if ((jsonObj !== null) && (jsonObj.hasOwnProperty("keyboard"))) {

      data_object.keylayout_filename = outputfilename.replace(/\.kmn$/, '.keylayout');
      data_object.kmn_filename = outputfilename;
      data_object.arrayOf_Modifiers = modifierBehavior;  // ukelele uses behaviours e.g. 18 modifiersCombinations in 8 KeyMapSelect(behaviors)
      data_object.arrayOf_Rules = rules;

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

    const object_array: Rule[] = [];
    let dk_counter_C3: number = 0;
    let dk_counter_C2: number = 0;
    let action_id: string;

    // check if we use CAPS in a modifier throughout the .keylayout file. In this case we need to add NCAPS
    const isCapsused: boolean = this.checkIfCapsIsUsed(data_ukelele.arrayOf_Modifiers);

    // loop keys 0-50 (= all keys we use)
    for (let j = 0; j <= KeylayoutToKmnConverter.USED_KEYS_COUNT; j++) {

      // loop behaviors (in ukelele it is possible to define multiple modifier combinations that behave in the same way)
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[0].keyMap.length; i++) {

        let rule_obj: Rule;

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

              rule_obj = new Rule(
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

                rule_obj = new Rule(
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

                        rule_obj = new Rule(
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

                              rule_obj = new Rule(
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

          /* QUESTION
          // _S2 why does this not work here? Correct data in this.callback.messages is available !?!?!
           this.callbacks.reportMessage(ConverterMessages.Error_UnsupportedCharactersDetected({
               inputFilename: jsonObj.keyboard['@_name'] + ".keylayout",
               keymap_index: jsonObj.keyboard.keyMapSet[0].keyMap[i]['@_index'],
               output: jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_output'],
               key: jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']
             }));
           return null;*/

          console.log("ERROR : some output characters can not be used in Keyman \"",
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

    const object_array: Rule[] = data_ukelele.arrayOf_Rules;

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
          object_array[i].unique_deadkey = unique_dkB_count;
          list_of_unique_Text2_rules.push([
            object_array[i].modifier_deadkey,
            object_array[i].deadkey,
            String(unique_dkB_count)]);
          unique_dkB_count++;
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
          object_array[i].unique_deadkey = unique_dkB_count;
          list_of_unique_Text2_rules.push([
            object_array[i].modifier_prev_deadkey,
            object_array[i].prev_deadkey,
            String(unique_dkB_count)
          ]);
          unique_dkB_count++;
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
   * @brief  member function to map Ukelele keycodes to Windows Keycodes
   * @param  pos Ukelele (=mac) keycodes
   * @return keycode on a Windows Keyboard
   */
  public map_UkeleleKC_To_VK(pos: number): string {
    const vk = [
      "K_A"          /* A */,
      "K_S"          /* S */,
      "K_D"          /* D */,
      "K_F"          /* F */,
      "K_H"          /* H */,
      "K_G"          /* G */,
      "K_Z"          /* Z */,
      "K_X"          /* X */,
      "K_C"          /* C */,
      "K_V"          /* V */,
      "K_BKQUOTE"    /* ^ */,
      "K_B"          /* B */,
      "K_Q"          /* Q */,
      "K_W"          /* W */,
      "K_E"          /* E */,
      "K_R"          /* R */,
      "K_Y"          /* Y */,
      "K_T"          /* T */,
      "K_1"          /* 1 */,
      "K_2"          /* 2 */,
      "K_3"          /* 3 */,
      "K_4"          /* 4 */,
      "K_6"          /* 6 */,
      "K_5"          /* 5 */,
      "K_EQUAL"      /* ´ */,
      "K_9"          /* 9 */,
      "K_7"          /* 7 */,
      "K_HYPHEN"     /* ß */,
      "K_8"          /* 8 */,
      "K_0"          /* 0 */,
      "K_RBRKT"      /* ] */,
      "K_O"          /* O */,
      "K_U"          /* U */,
      "K_LBRKT"      /* [ */,
      "K_I"          /* I */,
      "K_P"          /* P */,
      "K_ENTER",
      "K_L"          /* L */,
      "K_J"          /* J */,
      "K_QUOTE"      /* " */,
      "K_K"          /* K */,
      "K_COLON"      /* : */,
      "K_BKSLASH"    /* \ */,   // 42 for ISO  correct??
      "K_COMMA"      /* , */,
      "K_SLASH"      /* / */,
      "K_N"          /* N */,
      "K_M"          /* M */,
      "K_PERIOD"     /* . */,
      "K_?C1"        /* \ */,   // 48 for ANSI  correct??
      "K_SPACE"      /* \ */
    ];

    if (!(pos >= 0 && pos <= 0x31) || (pos === null) || (pos === undefined)) {
      return "";
    } else {
      return vk[pos];
    }
  }

  /* @brief  member function to return an index for a given actionID
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
          if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search[k][0] &&
            data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'] <= KeylayoutToKmnConverter.USED_KEYS_COUNT) {
            returnarray2D.push([
              data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'],
              this.map_UkeleleKC_To_VK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'])),
              data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'],
              data.keyboard.keyMapSet[0].keyMap[i]['@_index'],
              search[k][2]
            ]);
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
        if ((data.keyboard.actions.action[i].when[j]['@_state'] === search)) {
          returnarray2D.push([
            data.keyboard.actions.action[i]['@_id'],
            data.keyboard.actions.action[i].when[j]['@_state'],
            data.keyboard.actions.action[i].when[j]['@_output']
          ]);
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
          returnarray.push([
            // KeyName
            String(search[i][1]),
            // actionId
            String(search[i][2]),
            // behaviour
            String(search[i][3]),
            // modifier
            String(this.create_kmn_modifier(data.keyboard.modifierMap.keyMapSelect[behaviour].modifier[j]['@_keys'], isCAPSused)),
            // output
            String(search[i][4])
          ]);
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
      for (let j = 0; j <= KeylayoutToKmnConverter.USED_KEYS_COUNT; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          for (let k = 0; k < modi[data.keyboard.keyMapSet[0].keyMap[i]['@_index']].length; k++) {
            const behaviour: string = data.keyboard.keyMapSet[0].keyMap[i]['@_index'];
            const modifierkmn: string = this.create_kmn_modifier(modi[behaviour][k], isCapsused);
            const keyName: string = this.map_UkeleleKC_To_VK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code']));
            returnarray2D.push([
              search,
              outchar,
              data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'],
              behaviour,
              keyName,
              modifierkmn]);
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
      for (let j = 0; j <= KeylayoutToKmnConverter.USED_KEYS_COUNT; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_action'] === search) {
          mapIndexArray_2D.push([data.keyboard.keyMapSet[0].keyMap[i].key[j]['@_code'], i]);
        }
      }
    }
    return mapIndexArray_2D;
  }

  /** @internal */
  public convert_bound = {
    convert: this.convert.bind(this),
  };
}

/**
 * @brief  class for all storing a rule containing data for key, deadkey, previous deadkey, output)
 */
export class Rule {
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
