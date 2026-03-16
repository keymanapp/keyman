/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Convert macOS/Ukelele .keylayout files to Keyman .kmn
 *
 */

import { CompilerCallbacks, CompilerOptions, KeymanCompilerResult, Keylayout } from "@keymanapp/developer-utils";
import { KmnFileWriter } from './kmn-file-writer.js';
import { KeylayoutFileReader } from './keylayout-file-reader.js';
import { ConverterMessages } from '../converter-messages.js';
import { ConverterArtifacts, ConverterToKmnArtifacts } from "../converter-artifacts.js";


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

export interface ProcessedData {
  /**
  * Interface for all data read from a .keylayout file. Also contains all rules processed from input data.
  * Data will be used for writing to a .kmn file (e.g. filename, modifier combinations, rules)
  */
  keylayoutFilename: string;
  kmnFilename: string;
  modifiers: string[][];
  rules: Rule[];
};

export interface KeylayoutFileData {
  /**
   * Interface for storing data read from a .keylayout file and used for processing rules.
   * These are used for obtaining one entity form the other (e.g. from action id to output, from keycode to modifier, etc.)
   */
  actionId?: string;
  keyCode?: string;
  key?: string;
  behavior?: string;
  modifier?: string;
  outchar?: string;
};

export interface ActionStateOutput {
  /**
  * Interface for storing data read from a .keylayout file and used for processing rules.
  * These are used for obtaining the triplet [action id, state, output]
  * e.g. ['a9','1','â'] from <when state="1" output="â"/> for action id a9
  */
  id: string;
  state: string;
  output: string;
};

/**
 * @brief  class for all storing a rule containing data for key, deadkey, previous deadkey, output)
 */
export class Rule {
  constructor(
    public readonly ruleType: string,             /* C0, C1, C2, C3, or C4 */

    public readonly modifierPrevDeadkey: string, /* first key used by C3 rules*/
    public readonly prevDeadkey: string,
    public idPrevDeadkey: number,
    public uniquePrevDeadkey: number,

    public readonly modifierDeadkey: string,      /* second key used by C2,C3 rules*/
    public readonly deadkey: string,
    public idDeadkey: number,
    public uniqueDeadkey: number,

    public readonly modifierKey: string,          /* third key used by C0,C1,C2,C3,C4 rules*/
    public readonly key: string,
    public readonly output: Uint8Array,            /* output used by C0,C1,C2,C3,C4 rules*/
  ) { }

}

/**
 * @brief  member function to find the number of keys defined in a .keykayout file.
 *         We process 'MAX_KEY_COUNT' keys at maximum. In case a keylayout has fewer keys defined, we use that smaller number of keys (USE_KEY_COUNT)
 * @param  data data read from keylayout file 
 * @return usedKeyCount holding the number of keys of a certain keyMap used in a .keykayout file.
 */
export function findUsedKeysCount(data: any): number {

  let usedKeyCount = KeylayoutToKmnConverter.MAX_KEY_COUNT;
  if (data.keyboard.keyMapSet[0].keyMap[0].key.length < usedKeyCount) {
    // set max to n-1 (keys are zero indexed )
    usedKeyCount = data.keyboard.keyMapSet[0].keyMap[0].key.length - 1;
  }
  return usedKeyCount;
}

export class KeylayoutToKmnConverter {
  static readonly INPUT_FILE_EXTENSION = '.keylayout';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';
  static readonly SKIP_COMMENTED_LINES = false;
  static readonly MAX_CTRL_CHARACTER = 0x20;                      // the hightest control character we print out as a Unicode CodePoint
  static readonly MAX_KEY_COUNT = 49;                             // At most we use key Nr 0 (A) -> key Nr 49 (Space)
  static USE_KEY_COUNT = KeylayoutToKmnConverter.MAX_KEY_COUNT;   // we use key Nr 0 (A) -> highest available key of .keylayout file

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

    const KeylayoutReader = new KeylayoutFileReader(this.callbacks/*, this.options*/);
    const jsonO: Keylayout.KeylayoutXMLSourceFile = KeylayoutReader.read(inputFilename);

    try {
      if (!KeylayoutReader.validate(jsonO)) {
        return null;
      }
    } catch (e) {
      this.callbacks.reportMessage(ConverterMessages.Error_InvalidFile({ errorText: e.toString() }));
      return null;
    }

    const processedData = await this.convert(jsonO, inputFilename);

    const kmnFileWriter = new KmnFileWriter(this.callbacks, this.options);

    // write to object/ConverterToKmnResult
    const outputKmn = kmnFileWriter.write(processedData);
    const result: ConverterToKmnResult = {
      artifacts: {
        kmn: { data: outputKmn, filename: processedData.kmnFilename }
      }
    };
    return result;
  }

  /**
   * @brief  member function to read filename and behaviorof a json object into a ProcessedData
   * @param  jsonObj containing filename, behaviorand rules of a json object
   * @return an ProcessedData containing all data ready to print out
   */
  private convert(jsonObj: any, inputfilename: string): ProcessedData {
    // modifiers for each behavior
    const modifierBehavior: string[][] = [];

    // an array of data for a kmn rule
    const rules: Rule[] = [];

    // dataObject for all relevant data
    const dataObject: ProcessedData = {
      keylayoutFilename: "",
      kmnFilename: "",
      modifiers: [],
      rules: []
    };

    if ((jsonObj === null) || (!jsonObj.hasOwnProperty("keyboard"))) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToRead({ inputFilename: inputfilename }));
      return dataObject;
    } else {

      // create an array of modifier combinations and store in dataObject
      for (let j = 0; j < jsonObj.keyboard.modifierMap.keyMapSelect.length; j++) {
        const singleModifierSet: string[] = [];
        for (let k = 0; k < jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier.length; k++) {
          singleModifierSet.push(jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier[k]['@__keys']);
        }
        modifierBehavior.push(singleModifierSet);
      }

      // fill dataObject with filenames, behaviors and (initialized) rules
      dataObject.keylayoutFilename = inputfilename;
      dataObject.kmnFilename = inputfilename.replace(/\.keylayout$/, '.kmn');
      dataObject.modifiers = modifierBehavior;  // ukelele uses behaviors e.g. 18 modifiersCombinations in 8 KeyMapSelect(behaviors)
      dataObject.rules = rules;

      // fix the amount of processable keys to the maximun nr of keys of a keyMap to avoid processing more keys than defined
      KeylayoutToKmnConverter.USE_KEY_COUNT = findUsedKeysCount(jsonObj);

      // fill rules into 'rules' of dataObject
      return this.createRuleData(dataObject, jsonObj);
    }
  }

  /**
    * @brief  member function to read the rules contained in a json object and add array of Rules[] to an ProcessedData
    * @param  dataUkelele: an object containing the name of the in/output file, an array of behaviors and an (empty) array of Rules
    * @param  jsonObj: json Object containing all data read from a keylayout file
    * @return an object containing the name of the input file, an array of behaviors and a populated array of Rules[]
    */
  public createRuleData(dataUkelele: ProcessedData, jsonObj: any): ProcessedData {

    const rules: Rule[] = [];
    let dkCounterC3: number = 0;
    let dkCounterC2: number = 0;
    let actionId: string;

    // check if we use CAPS in a modifier throughout the .keylayout file. In this case we need to add NCAPS
    const isCapsused = (this.checkIfCapsIsUsed(dataUkelele.modifiers) !== undefined);

    // if there are different amounts of keyMapSelect vs keyMap
    if (jsonObj.keyboard.modifierMap?.keyMapSelect.length !== jsonObj.keyboard.keyMapSet[0].keyMap.length) {
      const errorText = dataUkelele.keylayoutFilename;
      this.callbacks.reportMessage(ConverterMessages.Error_InvalidFile({ errorText }));
    }

    for (let j = 0; j <= KeylayoutToKmnConverter.MAX_KEY_COUNT; j++) {

      // loop behaviors (in ukelele it is possible to define multiple modifier combinations that behave in the same way)
      for (let i = 0; i < jsonObj.keyboard.keyMapSet[0].keyMap.length; i++) {

        // if index of keys and behaviors exist
        const isItAvailable = ((j < jsonObj.keyboard.keyMapSet[0].keyMap[i].key.length) && (i < jsonObj.keyboard.keyMapSet[0].keyMap.length));
        if (!isItAvailable) {
          continue;
        }

        let ruleObj: Rule;

        if ((j < jsonObj.keyboard.keyMapSet[0].keyMap[i].key.length)) {
          // ...............................................................................................................................
          // case C0: output ...............................................................................................................
          // C0 see: https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.g7jwx3lx0ydd ...
          // a key is mapped to a character directly ( code -> output) .....................................................................
          // ...............e. g. <key code="1" output="s"/> ...............................................................................
          // ...............................................................................................................................

          if ((jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@__output'] !== undefined)
            && (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@__output'] !== "")) {

            // loop modifiers
            for (let l = 0; l < dataUkelele.modifiers[i].length; l++) {

              if (this.mapUkeleleKeycodeToVK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@__code']))) {

                ruleObj = new Rule(
                  /*   ruleType */                "C0",

                  /*   modifierPrevDeadkey*/      "",
                  /*   prevDeadkey */             "",
                  /*   idPrevDeadkey */           0,
                  /*   unique A */                0,

                  /*   modifierDeadkey */         "",
                  /*   deadkey */                 "",
                  /*   dk for C2*/                0,
                  /*   unique B */                0,

                  /*   modifierKey*/             this.createKmnModifier(dataUkelele.modifiers[i][l], isCapsused),
                  /*   key */                     this.mapUkeleleKeycodeToVK(Number(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@__code'])),
                  /*   output */                  new TextEncoder().encode(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@__output']),
                );
                rules.push(ruleObj);
              }
            }
            // }

          }
          else if (jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@__action'] !== undefined) {

            actionId = jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@__action'];
            // ...............................................................................................................................
            // case C1: action + state none + output .........................................................................................
            // C1 see: https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.g7jwx3lx0ydd ...
            // a key is mapped to an action and then to an output ............................................................................
            // KeyMap:code -> KeyMap:action->action:actionState(none) -> actionOutput ......................................................
            // ...............e. g. <when state="none" output="a" ............................................................................
            // ...............................................................................................................................

            for (let l = 0; l < dataUkelele.modifiers[i].length; l++) {

              if ((this.getOutputFromActionIdNone(jsonObj, actionId) !== undefined)
                && (this.getOutputFromActionIdNone(jsonObj, actionId) !== "")) {

                const outputchar: string = this.getOutputFromActionIdNone(jsonObj, actionId);

                const b1ModifierKeyObj: KeylayoutFileData[] =
                  this.getActionOutputBehaviorKeyModiFromActionIDStateOutput(jsonObj, dataUkelele.modifiers, actionId, outputchar, isCapsused);

                for (let m = 0; m < b1ModifierKeyObj.length; m++) {
                  ruleObj = new Rule(
                    /*   ruleType */                "C1",

                    /*   modifierPrevDeadkey*/      "",
                    /*   prevDeadkey */             "",
                    /*   idPrevDeadkey */           0,
                    /*   unique A */                0,

                    /*   modifierDeadkey */         "",
                    /*   deadkey */                 "",
                    /*   dk for C2*/                0,
                    /*   unique B */                0,

                    /*   modifierKey*/              b1ModifierKeyObj[m].modifier,
                    /*   key */                     b1ModifierKeyObj[m].key,
                    /*   output */                  new TextEncoder().encode(outputchar)
                  );
                  if ((outputchar !== undefined) && (outputchar !== "undefined") && (outputchar !== "")) {
                    rules.push(ruleObj);
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

              const b1ActionIndex: number = this.getActionIndexFromActionId(jsonObj, actionId);

              // with actionId from above loop all 'action' and search for a state(none)-next-pair ............................................................................................................
              // e.g. in Block 5: find <when state="none" next="1"/> for action id a18 ......................................................................................................................
              for (let l = 0; l < jsonObj.keyboard.actions.action[b1ActionIndex].when.length; l++) {
                if ((jsonObj.keyboard.actions.action[b1ActionIndex].when[l]['@__state'] === "none")         // find "none"
                  && (jsonObj.keyboard.actions.action[b1ActionIndex].when[l]['@__next'] !== undefined)) {   // find "next"

              // Data of Block Nr 5 .....................................................................................................................................................................
              // of this state(none)-next-pair get value of next (next="1") .............................................................................................................................
              /* eg: 1  */                            const b5ValueNext: string = jsonObj.keyboard.actions.action[b1ActionIndex].when[l]['@__next'];
              // ........................................................................................................................................................................................


              // Data of Block Nr 4 .....................................................................................................................................................................
              // with present actionId (a18) find all keycode-behavior-pairs that use this action (a18) => (keymapIndex 0/keycode 24 and keymapIndex 3/keycode 24) ....................................
              // from these create an array of modifier combinations  e.g. [['','caps?'], ['Caps']] .....................................................................................................
              /* eg: [['24', 0], ['24', 3]] */        const b4DeadkeyObj: KeylayoutFileData[] = this.getKeyModifierArrayFromActionID(jsonObj, actionId);
              /* e.g. [['','caps?'], ['Caps']]*/      const b4DeadkeyModifierObj: string[] = this.getModifierArrayFromKeyModifierArray(dataUkelele.modifiers, b4DeadkeyObj);
              // ........................................................................................................................................................................................


              // Data of Block Nr 6 .....................................................................................................................................................................
              // create an array[action id,state,output] from all state-output-pairs that use state = b5ValueNext (e.g. use 1 in  <when state="1" output="â"/> ) ......................................
              /*  eg: [ 'a9','1','â'] */              const b6ActionIdObj: ActionStateOutput[] = this.getActionStateOutputArrayFromActionState(jsonObj, b5ValueNext);
              // ........................................................................................................................................................................................


              // Data of Block Nr 1  ....................................................................................................................................................................
              // create array[Keycode,Keyname,action id,actionIndex,output] and array[Keyname,action id,behavior,modifier,output] ......................................................................
              /*  eg: ['0','K_A','a9','0','â'] */    const b1KeycodeObj: KeylayoutFileData[] = this.getKeyActionOutputArrayFromActionStateOutputArray(jsonObj, b6ActionIdObj);
              /*  eg: ['K_A','a9','0','NCAPS','â']*/  const b1ModifierKeyObj: KeylayoutFileData[] = this.getKeyBehaviorModOutputArrayFromKeyActionBehaviorOutputArray(jsonObj, b1KeycodeObj, isCapsused);
                  // .......................................................................................................................................................................................

                  for (let n1 = 0; n1 < b4DeadkeyModifierObj.length; n1++) {
                    for (let n2 = 0; n2 < b4DeadkeyModifierObj[n1].length; n2++) {
                      for (let n3 = 0; n3 < b4DeadkeyObj.length; n3++) {
                        for (let n4 = 0; n4 < b1ModifierKeyObj.length; n4++) {

                          ruleObj = new Rule(
                            /*   ruleType */              "C2",

                            /*   modifierPrevDeadkey*/    "",
                            /*   prevDeadkey */           "",
                            /*   idPrevDeadkey */         0,
                            /*   unique A */              0,

                            /*   modifierDeadkey */       this.createKmnModifier(b4DeadkeyModifierObj[n1][n2], isCapsused),
                            /*   deadkey */               this.mapUkeleleKeycodeToVK(Number(b4DeadkeyObj[n3].key)),
                            /*   dk for C2*/              dkCounterC2++,
                            /*   unique B */              0,

                            /*   modifierKey*/            b1ModifierKeyObj[n4].modifier,
                            /*   key */                   b1ModifierKeyObj[n4].key,
                            /*   output */                new TextEncoder().encode(b1ModifierKeyObj[n4].outchar),
                          );
                          if ((b1ModifierKeyObj[n4].outchar !== undefined)
                            && (b1ModifierKeyObj[n4].outchar !== "undefined")
                            && (b1ModifierKeyObj[n4].outchar !== "")) {
                            rules.push(ruleObj);
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

              // with actionId from above loop all 'action' and search for a state-next-pair ...................................................................................................................
              // e.g. in Block 5: find <when state="3" next="1"/> for action id a16 .............................................................................................................................
              for (let l = 0; l < jsonObj.keyboard.actions.action[b1ActionIndex].when.length; l++) {
                if ((jsonObj.keyboard.actions.action[b1ActionIndex].when[l]['@__state'] !== "none")
                  && (jsonObj.keyboard.actions.action[b1ActionIndex].when[l]['@__next'] !== undefined)) {

              // Data of Block Nr 5 ........................................................................................................................................................................
              // of this state-next-pair get value of next (next="1") and state="3" ........................................................................................................................
              /* e.g. state = 3  */                       const b5ValueState: string = jsonObj.keyboard.actions.action[b1ActionIndex].when[l]['@__state'];
              /* e.g. next  = 1  */                       const b5ValueNext: string = jsonObj.keyboard.actions.action[b1ActionIndex].when[l]['@__next'];
              // ...........................................................................................................................................................................................

              // Data of Block Nr 4 ........................................................................................................................................................................
              // with present actionId (a16) find all keycode-behavior-pairs that use this action (a16) => (keymapIndex 3/keycode 32) ....................................................................
              // from these create an array of modifier combinations  e.g. [ [ 'anyOption', 'Caps' ] ] .....................................................................................................
              /* e.g. [['32', 3]] */                      const b4DeadkeyObj: KeylayoutFileData[] = this.getKeyModifierArrayFromActionID(jsonObj, actionId);
              /* e.g. [ [ 'anyOption', 'Caps' ] ]*/       const b4DeadkeyModifierObj: string[] = this.getModifierArrayFromKeyModifierArray(dataUkelele.modifiers, b4DeadkeyObj);
              // ...........................................................................................................................................................................................

              // Data of Block Nr 3 ........................................................................................................................................................................
              // get an action id from a state-output-pair that use state = b5ValueState (e.g. use 3 in  <when state="none" next="3"/> ) .................................................................
              /* e.g. actioniD = a17  */                  const b3ActionId: string = this.getActionIdFromActionNext(jsonObj, b5ValueState);
              // ...........................................................................................................................................................................................

              // Data of Block Nr 2  .......................................................................................................................................................................
              // with present actionId (a17) find all key names and behaviors that use this action (a17) => (keymapIndex 3/keycode 28) ....................................................................
              // from these create an array of modifier combinations  e.g. [ [ 'anyOption', 'Caps' ] ] .....................................................................................................
              /* eg: index=3 */                           const b2PrevDeadkeyObj: KeylayoutFileData[] = this.getKeyModifierArrayFromActionID(jsonObj, b3ActionId);
              /* e.g. [ [ 'anyOption', 'Caps' ] ] */      const b2PrevDeadkeyModifierObj: string[] = this.getModifierArrayFromKeyModifierArray(dataUkelele.modifiers, b2PrevDeadkeyObj);
              // ...........................................................................................................................................................................................
              // Data of Block Nr 6 ........................................................................................................................................................................
              // create an array[action id,state,output] from all state-output-pairs that use state = b5ValueNext (e.g. use 1 in  <when state="1" output="â"/> ) .........................................
              /*  eg:[ [ 'a9','1','â'] ]*/                const b6ActionIdObj: ActionStateOutput[] = this.getActionStateOutputArrayFromActionState(jsonObj, b5ValueNext); /*  eg:[ [ 'a9','1','â'] ]*/
              // ...........................................................................................................................................................................................

              // Data of Block Nr 1  .......................................................................................................................................................................
              // create array[Keycode,Keyname,action id,actionIndex,output] and array[Keyname,action id,behavior,modifier,output] .........................................................................
              /*  eg: ['49','K_SPACE','a0','0','Â'] */    const b1KeycodeObj: KeylayoutFileData[] = this.getKeyActionOutputArrayFromActionStateOutputArray(jsonObj, b6ActionIdObj);
              /*  eg: ['K_SPACE','a0','0','NCAPS','Â'] */ const b1ModifierKeyObj: KeylayoutFileData[] = this.getKeyBehaviorModOutputArrayFromKeyActionBehaviorOutputArray(jsonObj, b1KeycodeObj, isCapsused);
                  // ...........................................................................................................................................................................................

                  for (let n1 = 0; n1 < b2PrevDeadkeyModifierObj.length; n1++) {
                    for (let n2 = 0; n2 < b2PrevDeadkeyModifierObj[n1].length; n2++) {
                      for (let n3 = 0; n3 < b2PrevDeadkeyObj.length; n3++) {
                        for (let n4 = 0; n4 < b4DeadkeyModifierObj.length; n4++) {
                          for (let n5 = 0; n5 < b4DeadkeyModifierObj[n4].length; n5++) {
                            for (let n6 = 0; n6 < b4DeadkeyObj.length; n6++) {
                              for (let n7 = 0; n7 < b1ModifierKeyObj.length; n7++) {

                                ruleObj = new Rule(
                                  /*   ruleType */              "C3",
                                  /*   modifierPrevDeadkey*/    this.createKmnModifier(b2PrevDeadkeyModifierObj[n1][n2], isCapsused),
                                  /*   prevDeadkey */           this.mapUkeleleKeycodeToVK(Number(b2PrevDeadkeyObj[n3].key)),
                                  /*   idPrevDeadkey */         dkCounterC3++,
                                  /*   unique A */              0,

                                  /*   modifierDeadkey */       this.createKmnModifier(b4DeadkeyModifierObj[n4][n5], isCapsused),
                                  /*   deadkey */               this.mapUkeleleKeycodeToVK(Number(b4DeadkeyObj[n6].key)),
                                  /*   dk for C2*/              0,
                                  /*   unique B */              0,

                                  /*   modifierKey*/            b1ModifierKeyObj[n7].modifier,
                                  /*   key */                   b1ModifierKeyObj[n7].key,
                                  /*   output */                new TextEncoder().encode(b1ModifierKeyObj[n7].outchar),
                                );
                                if ((b1ModifierKeyObj[n7].outchar !== undefined)
                                  && (b1ModifierKeyObj[n7].outchar !== "undefined")
                                  && (b1ModifierKeyObj[n7].outchar !== "")) {
                                  rules.push(ruleObj);
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
            this.callbacks.reportMessage(ConverterMessages.Info_UnsupportedCharactersDetected({
              inputFilename: jsonObj.keyboard['@__name'] + ".keylayout",
              keymapIndex: jsonObj.keyboard.keyMapSet[0].keyMap[i]['@__index'],
              output: jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@__output'],
              key: jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@__code'],
              KeyName: this.mapUkeleleKeycodeToVK(jsonObj.keyboard.keyMapSet[0].keyMap[i].key[j]['@__code'])
            }));
          }
        }
      }
    }
    dataUkelele.rules = rules;
    return this.reviewRuleInputData(dataUkelele);
  }

  /**
    * @brief  member function to review data in array of rules of dataUkelele: remove duplicate rules and mark first occurance of a rule in rules
    * @param  dataUkelele: an object containing the name of the in/output file, an array of behaviors and an array of Rules
    * @return an object containing the name of the input file, an array of behaviors and the revised array of Rule[]
    */
  public reviewRuleInputData(dataUkelele: ProcessedData): ProcessedData {

    // check for duplicate C2 and C3 rules in rules (e.g. [NCAPS RALT K_8]  >  dk(C12) ): create a separate array of unique rules,
    // then compare to rules and mark first occurrence  of a rule in rules

    let uniqueCountDkB = 0;
    const uniqueTextRules: string[][] = [];

    const rules: Rule[] = dataUkelele.rules;

    //------------------------------------ C2: dk ----------------------------------
    // first rule is always unique
    rules[0].uniqueDeadkey = uniqueCountDkB;
    rules[0].idDeadkey = uniqueCountDkB;
    uniqueCountDkB++;

    for (let i = 0; i < rules.length; i++) {


      if (
        ((rules[i].modifierDeadkey !== undefined) && (rules[i].modifierDeadkey !== "")) &&
        ((rules[i].deadkey !== undefined) && (rules[i].deadkey !== ""))
      ) {
        let IsFirstUsedHereDk: boolean = true;

        // check if not used before
        for (let j = 0; j < i; j++) {
          if ((rules[i].modifierDeadkey === rules[j].modifierDeadkey)
            && (rules[i].deadkey === rules[j].deadkey)) {
            IsFirstUsedHereDk = IsFirstUsedHereDk && false;
          }
        }

        if (IsFirstUsedHereDk) {
          rules[i].uniqueDeadkey = uniqueCountDkB;
          uniqueTextRules.push([
            rules[i].modifierDeadkey,
            rules[i].deadkey,
            String(uniqueCountDkB)]);
          uniqueCountDkB++;
        }
      }
    }

    //----------------------------------- C3: prev-dk ----------------------------------
    let uniqueCountDkA = 0;

    // first rule is always unique
    rules[0].uniquePrevDeadkey = uniqueCountDkA;
    uniqueCountDkA++;

    for (let i = 0; i < rules.length; i++) {
      if ((rules[i].modifierPrevDeadkey !== "") && (rules[i].prevDeadkey !== "")) {
        let isFirstUsedHerePrevDk: boolean = true;

        // check if not used before
        for (let j = 0; j < i; j++) {
          if ((rules[i].modifierPrevDeadkey === rules[j].modifierPrevDeadkey)
            && (rules[i].prevDeadkey === rules[j].prevDeadkey)) {
            isFirstUsedHerePrevDk = isFirstUsedHerePrevDk && false;
          }
        }

        // check if first part of C3 rule contains a rule that is already defined in C2
        if (isFirstUsedHerePrevDk) {
          rules[i].uniquePrevDeadkey = uniqueCountDkA;
          uniqueCountDkA++;
          for (let k = 0; k < uniqueTextRules.length; k++) {
            if ((uniqueTextRules[k][0] === rules[i].modifierDeadkey) && ((uniqueTextRules[k][1] === rules[i].deadkey))) {
              rules[i].uniqueDeadkey = Number(uniqueTextRules[k][2]);
            }
          }
        }

        if (isFirstUsedHerePrevDk) {
          rules[i].uniqueDeadkey = uniqueCountDkB;
          uniqueTextRules.push([
            rules[i].modifierPrevDeadkey,
            rules[i].prevDeadkey,
            String(uniqueCountDkB)
          ]);
          uniqueCountDkB++;
        }
      }
    }

    // loop through rules and mark first occurence each rule of uniqueTextRules
    for (let i = 0; i < rules.length; i++) {
      for (let j = 0; j < uniqueTextRules.length; j++) {
        if ((rules[i].modifierPrevDeadkey === uniqueTextRules[j][0]) && (rules[i].prevDeadkey === uniqueTextRules[j][1])) {
          rules[i].idPrevDeadkey = Number(uniqueTextRules[j][2]);
        }
        if ((rules[i].modifierDeadkey === uniqueTextRules[j][0]) && (rules[i].deadkey === uniqueTextRules[j][1])) {
          rules[i].idDeadkey = Number(uniqueTextRules[j][2]);
        }
      }
    }
    dataUkelele.rules = rules;
    return dataUkelele;
  }

  /**
   * @brief  member function to create a kmn modifier from a keylayout modifier
   * @param  keylayoutModifier :string - modifier used in a .keylayout file
   * @param  isCAPSused  : boolean flag to indicate if CAPS is used in a keylayout file or not
   * @return string - a modifier value suitable for use in a .kmn-file
   */
  public createKmnModifier(keylayoutModifier: string, isCAPSused: boolean): string {

    const kmnModifier: string[] = [];
    const modifierState = keylayoutModifier.split(" ");

    for (const modifier of modifierState) {
      const modifierUppercase = modifier.toUpperCase();

      if (isCAPSused && (keylayoutModifier).toUpperCase().indexOf("CAPS?") > 0) {
        kmnModifier.push("NCAPS");
      }
      if (isCAPSused && (keylayoutModifier).toUpperCase().indexOf("CAPS") === -1) {
        kmnModifier.push("NCAPS");
      }

      // if we find a modifier containing a '?' e.g. SHIFT? it means the modifier is not necessary.
      // If it is not necessary we don't write this modifier
      if (modifierUppercase.includes('?') && modifierUppercase !== 'CAPS?') {
        kmnModifier.push("");
      }
      // if we find 'caps?' => caps is not necessary.
      // If caps is not necessary and isCAPSused we need to write out NCAPS.
      else if (isCAPSused && modifierUppercase === 'CAPS?') {
        kmnModifier.push("NCAPS");
      }
      else if (!isCAPSused && modifierUppercase === 'CAPS?') {
        kmnModifier.push("");
      }
      else if (modifierUppercase === 'CAPS' || modifierUppercase === 'CAPS?') {
        kmnModifier.push("CAPS");
      }
      else if (isCAPSused && (modifierUppercase === 'NCAPS')) {
        kmnModifier.push("NCAPS");
      }
      else if (modifierUppercase === 'ANYSHIFT' || modifierUppercase === 'SHIFT') {
        kmnModifier.push("SHIFT");
      }
      else if (modifierUppercase === "LEFTSHIFT" || modifierUppercase === "LSHIFT") {
        kmnModifier.push("SHIFT");
      }
      else if (modifierUppercase === "RIGHTSHIFT" || modifierUppercase === "RSHIFT") {
        kmnModifier.push("SHIFT");
      }
      else if (modifierUppercase === 'ANYCONTROL' || modifierUppercase === 'CONTROL') {
        kmnModifier.push("CTRL");
      }
      else if (modifierUppercase === "LEFTCONTROL" || modifierUppercase === "LCONTROL") {
        kmnModifier.push("LCTRL");
      }
      else if (modifierUppercase === "RIGHTCONTROL" || modifierUppercase === "RCONTROL") {
        kmnModifier.push("RCTRL");
      }
      else if (modifierUppercase === "LEFTOPTION" || modifierUppercase === "LOPTION") {
        kmnModifier.push("LALT");
      }
      else if (modifierUppercase === "RIGHTOPTION" || modifierUppercase === "ROPTION") {
        kmnModifier.push("RALT");
      }
      else if (modifierUppercase === 'ANYOPTION' || modifierUppercase === 'OPTION') {
        kmnModifier.push("RALT");
      }
      // to enable the use of other modifiers (leave upper/lowecase as in .keylayout)
      // e.g. 'shift command' -> 'NCAPS SHIFT command'; 'wrongModifierName' -> 'wrongModifierName'
      else {
        if (!this.isAcceptableKeymanModifier(modifier))
          kmnModifier.push(modifier);
      }
    }

    // remove duplicate and empty entries and make sure NCAPS is at the beginning
    const uniqueModifier: string[] = kmnModifier.filter(function (item, pos, self) {
      return ((self.indexOf(item) === pos) && (item !== ""));
    });

    return uniqueModifier.flat().toString().replace(/,/g, " ");
  }

  /**
   * @brief  member function to check if CAPS is used throughout a keylayout file or not
   * @param  keylayoutModifier the modifier string used in the .keylayout-file
   * @return "caps" or undefined if "caps" is not found
   */
  public checkIfCapsIsUsed(keylayoutModifier: string[][]): string {
    if (!keylayoutModifier)
      return undefined;
    return keylayoutModifier.flat().find(e => e.toUpperCase() === 'CAPS');
  }

  /**
  * @brief  member function to check if a modifier can be used in Keyman
  * @param  keylayoutModifier the modifier string used in the .keylayout-file
  * @return true if the modifier can be used in keyman; false if not
  */
  public isAcceptableKeymanModifier(keylayoutModifier: string): boolean {
    if (keylayoutModifier === null)
      return false;
    const modifierSingle = keylayoutModifier.toUpperCase().split(" ");
    for (const mod of modifierSingle) {
      if (!mod.match(/^(NCAPS|CAPS|SHIFT|ALT|RALT|LALT|CTRL|LCTRL|RCTRL|)$/)) {
        return false;
      }
    }
    return true;
  }

  /**
   * @brief  member function to map Ukelele keycodes to Windows Keycodes
   * @param  pos Ukelele (=mac) keycodes
   * @return VK
   */
  public mapUkeleleKeycodeToVK(pos: number): string {
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

  /**
  * @brief  member function to return an index for a given actionID
  * @param  data   :any - an object containing all data read from a .keylayout file
  * @param  search :string - value 'id' to be found
  * @return a number specifying the index of an actionId
  */
  public getActionIndexFromActionId(data: any, search: string): number {
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      if (data.keyboard.actions.action[i]['@__id'] === search) {
        return i;
      }
    }
    return 0;
  }

  /**
  * @brief  member function to  find the actionID of a certain state-next pair
  * @param  data   :any an object containing all data read from a .keylayout file
  * @param  search :string value 'next' to be found
  * @return a string containing the actionId of a certain state(none)-next pair
  */
  public getActionIdFromActionNext(data: any, search: string): string {
    if (search !== "none") {
      for (let i = 0; i < data.keyboard.actions.action.length; i++) {
        for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
          if (data.keyboard.actions.action[i].when[j]['@__next'] === search) {
            return data.keyboard.actions.action[i]['@__id'];
          }
        }
      }
    }
    return "";
  }

  /**
   * @brief  member function to create an array of (modifier) behaviors for a given keycode in [{keycode,modifier}]
   * @param  data    : any - an object containing all data read from a .keylayout file
   * @param  search  : KeylayoutFileData[] - an array[{keycode,modifier}]  to be found
   * @return a string[] containing modifiers
   */
  public getModifierArrayFromKeyModifierArray(data: any, search: KeylayoutFileData[]): string[] {
    const returnString1D: string[] = [];
    for (let i = 0; i < search.length; i++) {
      returnString1D.push(data[search[i].behavior]);
    }
    return returnString1D;
  }


  /**
   * @brief  member function to find the output for a certain actionID for state 'none'
   * @param  data   :any an object containing all data read from a .keylayout file
   * @param  search :string an actionId to be found
   * @return a string containing the output character
   */
  public getOutputFromActionIdNone(data: any, search: string): string {
    let OutputValue: string = "";
    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      if (data.keyboard.actions.action[i]['@__id'] === search) {
        for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
          if (data.keyboard.actions.action[i].when[j]['@__state'] === "none") {
            if (data.keyboard.actions.action[i].when[j]['@__output'] !== undefined) {
              OutputValue = data.keyboard.actions.action[i].when[j]['@__output'];
            }
          }
        }
      }
    }
    return OutputValue;
  }

  /**
  * @brief  member function to return array of [Keycode,Keyname,actionId,actionIDIndex, output] for a given actionID in of [ actionID,state,output]
  * @param  data   :any - an object containing all data read from a .keylayout file
  * @param  search :idStateOutputObject[] - array of [{ actionID,state,output }]
  * @return a KeylayoutFileData[] containing [{Keycode,Keyname,actionId,actionID, output}]
  */
  public getKeyActionOutputArrayFromActionStateOutputArray(data: any, search: ActionStateOutput[]): KeylayoutFileData[] {

    if ((search === undefined) || (search === null))
      return [];

    const keyActionOutput = [];

    for (let k = 0; k < search.length; k++) {
      for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
        for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
          if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@__action'] === search[k].id &&
            data.keyboard.keyMapSet[0].keyMap[i].key[j]['@__code'] <= KeylayoutToKmnConverter.MAX_KEY_COUNT) {
            const singleDataSet = {
              keyCode: data.keyboard.keyMapSet[0].keyMap[i].key[j]['@__code'],
              key: this.mapUkeleleKeycodeToVK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@__code'])),
              actionId: data.keyboard.keyMapSet[0].keyMap[i].key[j]['@__action'],
              behavior: data.keyboard.keyMapSet[0].keyMap[i]['@__index'],
              outchar: search[k].output
            };
            keyActionOutput.push(singleDataSet);
          }
        }
      }
    }
    return keyActionOutput;
  }

  /**
   * @brief  member function to get an array of all actionId-output pairs for a certain state
   * @param  data    : any an object containing all data read from a .keylayout file
   * @param  search  : string a 'state' to be found
   * @return an array: idStateOutputObject[] containing all [{actionId, state, output}] for a certain state
   */
  public getActionStateOutputArrayFromActionState(data: any, search: string): ActionStateOutput[] {
    const actionStateOutput: ActionStateOutput[] = [];

    for (let i = 0; i < data.keyboard.actions.action.length; i++) {
      for (let j = 0; j < data.keyboard.actions.action[i].when.length; j++) {
        if ((data.keyboard.actions.action[i].when[j]['@__state'] === search)) {
          if (data.keyboard.actions.action[i].when[j]['@__output'] !== undefined) {
            const singleDataSet = {
              id: data.keyboard.actions.action[i]['@__id'],
              state: data.keyboard.actions.action[i].when[j]['@__state'],
              output: data.keyboard.actions.action[i].when[j]['@__output']
            };
            actionStateOutput.push(singleDataSet);
          }
        }
      }
    }
    return actionStateOutput;
  }

  /**
   * @brief  member function to create an 2D array of [KeyName,actionId,behavior,modifier,output]
   * @param  data    : any an object containing all data read from a .keylayout file
   * @param  search  : array of [{keycode,keyname,actionId,behavior,output}] to be found
   * @param  isCAPSused  : boolean flag to indicate if CAPS is used in a keylayout file or not
   * @return an array: KeylayoutFileData[] containing [{KeyName,actionId,behavior,modifier,output}]
   */
  public getKeyBehaviorModOutputArrayFromKeyActionBehaviorOutputArray(data: any, search: KeylayoutFileData[], isCAPSused: boolean): KeylayoutFileData[] {
    const keyBehaviorModOutput = [];

    if (!((search === undefined) || (search === null) || (search.length === 0))) {
      for (let i = 0; i < search.length; i++) {
        const behaviorIdx: number = Number(search[i].behavior);
        for (let j = 0; j < data.keyboard.modifierMap.keyMapSelect[behaviorIdx].modifier.length; j++) {
          const singleDataSet = {
            actionId: search[i].actionId,
            key: search[i].key,
            behavior: search[i].behavior,
            modifier: this.createKmnModifier(data.keyboard.modifierMap.keyMapSelect[behaviorIdx].modifier[j]['@__keys'], isCAPSused),
            outchar: search[i].outchar,
          };
          keyBehaviorModOutput.push(singleDataSet);
        }
      }
    }
    // remove duplicates
    const uniquekeyBehaviorModOutput = keyBehaviorModOutput.reduce((unique, o) => {
      if (!unique.some(obj =>
        obj.actionId === o.actionId &&
        obj.key === o.key &&
        obj.behavior === o.behavior &&
        obj.modifier === o.modifier &&
        obj.outchar === o.outchar
      )) {
        unique.push(o);
      }
      return unique;
    }, []);
    return uniquekeyBehaviorModOutput;
  }

  /**
   * @brief  member function to create an array of [actionID, output, behavior,keyname,modifier] for a given actionId
   * @param  data    : any - an object containing all data read from a .keylayout file
   * @param  modi    : any - an array of modifiers
   * @param  search  : string - an actionId to be found
   * @param  outchar  : string - the output character
   * @param  isCAPSused  : boolean - flag to indicate if CAPS is used in a keylayout file or not
   * @return an array: KeylayoutFileData[] containing [{actionID,output, behavior,keyname,modifier}]
   */
  public getActionOutputBehaviorKeyModiFromActionIDStateOutput(data: any, modi: string[][], search: string, outchar: string, isCapsused: boolean): KeylayoutFileData[] {
    const actionOutputBehaviorKeyModi = [];

    if ((search === "") || (search === undefined) || !((isCapsused === true) || (isCapsused === false))) {
      return [];
    }
    // loop behaviors (in ukelele it is possible to define multiple modifier combinations that behave in the same way)
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@__action'] === search) {
          for (let k = 0; k < modi[data.keyboard.keyMapSet[0].keyMap[i]['@__index']].length; k++) {
            const behaviorIdx: number = data.keyboard.keyMapSet[0].keyMap[i]['@__index'];
            const singleDataSet = {
              outchar: outchar,
              actionId: data.keyboard.keyMapSet[0].keyMap[i].key[j]['@__action'],
              behavior: data.keyboard.keyMapSet[0].keyMap[i]['@__index'],
              key: this.mapUkeleleKeycodeToVK(Number(data.keyboard.keyMapSet[0].keyMap[i].key[j]['@__code'])),
              modifier: this.createKmnModifier(modi[behaviorIdx][k], isCapsused),
            };
            actionOutputBehaviorKeyModi.push(singleDataSet);
          }
        }
      }
    }

    //.............................................................................

    // remove duplicates
    const uniqueactionOutputBehaviorKey = actionOutputBehaviorKeyModi.reduce((unique, o) => {
      if (!unique.some(obj =>
        obj.outchar === o.outchar &&
        obj.actionId === o.actionId &&
        obj.behavior === o.behavior &&
        obj.key === o.key &&
        obj.modifier === o.modifier
      )) {
        unique.push(o);
      }
      return unique;
    }, []);

    return uniqueactionOutputBehaviorKey;
  }

  /**
   * @brief  member function to create an array of [{keycode,behavior}] for a given actionId
   * @param  data    : any - an object containing all data read from a .keylayout file
   * @param  search  : string - an actionId to be found
   * @return an array: KeylayoutFileData[] containing [{keycode,behavior}]
   */
  public getKeyModifierArrayFromActionID(data: any, search: string): KeylayoutFileData[] {
    const mapIndexObject1D: KeylayoutFileData[] = [];
    for (let i = 0; i < data.keyboard.keyMapSet[0].keyMap.length; i++) {
      for (let j = 0; j < data.keyboard.keyMapSet[0].keyMap[i].key.length; j++) {
        if (data.keyboard.keyMapSet[0].keyMap[i].key[j]['@__action'] === search) {
          const singleDataSet = {
            key: data.keyboard.keyMapSet[0].keyMap[i].key[j]['@__code'],
            behavior: String(i),
          };
          mapIndexObject1D.push(singleDataSet);
        }
      }
    }
    return mapIndexObject1D;
  }

  /** @internal */
  public convertBound = {
    convert: this.convert.bind(this),
  };
}

