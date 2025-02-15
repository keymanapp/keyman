import { JSKeyboardProcessor } from "./jsKeyboardProcessor.js";
import { VariableStoreDictionary } from "keyman/engine/keyboard";
import OutputTarget, { type Transcription } from './outputTarget.js';
import { Mock } from "./mock.js";
import { type VariableStore } from "./systemStores.js";
import { LexicalModelTypes } from '@keymanapp/common-types';

/**
 * Represents the commands and state changes that result from a matched keyboard rule.
 */
export default class RuleBehavior {
  /**
   * The before-and-after Transform from matching a keyboard rule.  May be `null`
   * if no keyboard rules were matched for the keystroke.
   */
  transcription: Transcription = null;

  /**
   * Indicates whether or not a BEEP command was issued by the matched keyboard rule.
   */
  beep?: boolean;

  /**
   * A set of changed store values triggered by the matched keyboard rule.
   */
  setStore: {[id: number]: string} = {};

  /**
   * A set of variable stores with save requests triggered by the matched keyboard rule
   */
  saveStore: {[name: string]: VariableStore} = {};

  /**
   * A set of variable stores with possible changes to be applied during finalization.
   */
    variableStores: VariableStoreDictionary = {};

  /**
   * Denotes a non-output default behavior; this should be evaluated later, against the true keystroke.
   */
  triggersDefaultCommand: boolean = false;

  /**
   * Denotes error log messages generated when attempting to generate this behavior.
   */
  errorLog?: string;

  /**
   * Denotes warning log messages generated when attempting to generate this behavior.
   */
  warningLog?: string;

  /**
   * If predictive text is active, contains a Promise returning predictive Suggestions.
   */
  predictionPromise?: Promise<LexicalModelTypes.Suggestion[]>;

  /**
   * In reference to https://github.com/keymanapp/keyman/pull/4350#issuecomment-768753852:
   *
   * If the final group processed is a context and keystroke group (using keys),
   * and there is no nomatch rule, and the keystroke is not matched in the group,
   * the keystroke's default behavior should trigger, regardless of whether or not any
   * rules in prior groups matched.
   */
  triggerKeyDefault?: boolean;

  finalize(processor: JSKeyboardProcessor, outputTarget: OutputTarget, readonly: boolean) {
    if(!this.transcription) {
      throw "Cannot finalize a RuleBehavior with no transcription.";
    }

    if(processor.beepHandler && this.beep) {
      processor.beepHandler(outputTarget);
    }

    for(let storeID in this.setStore) {
      let sysStore = processor.keyboardInterface.systemStores[storeID];
      if(sysStore) {
        try {
          sysStore.set(this.setStore[storeID]);
        } catch (error) {
          if(processor.errorLogger) {
            processor.errorLogger("Rule attempted to perform illegal operation - 'platform' may not be changed.");
          }
        }
      } else if(processor.warningLogger) {
        processor.warningLogger("Unknown store affected by keyboard rule: " + storeID);
      }
    }

    processor.keyboardInterface.applyVariableStores(this.variableStores);

    if(processor.keyboardInterface.variableStoreSerializer) {
      for(let storeID in this.saveStore) {
        processor.keyboardInterface.variableStoreSerializer.saveStore(processor.activeKeyboard.id, storeID, this.saveStore[storeID]);
      }
    }

    if(this.triggersDefaultCommand) {
      let keyEvent = this.transcription.keystroke;
      processor.defaultRules.applyCommand(keyEvent, outputTarget);
    }

    if(processor.warningLogger && this.warningLog) {
      processor.warningLogger(this.warningLog);
    } else if(processor.errorLogger && this.errorLog) {
      processor.errorLogger(this.errorLog);
    }
  }

  /**
   * Merges default-related behaviors from another RuleBehavior into this one.  Assumes that the current instance
   * "came first" chronologically.  Both RuleBehaviors must be sourced from the same keystroke.
   *
   * Intended use:  merging rule-based behavior with default key behavior during scenarios like those described
   * at https://github.com/keymanapp/keyman/pull/4350#issuecomment-768753852.
   *
   * This function does not attempt a "complete" merge for two fully-constructed RuleBehaviors!  Things
   * WILL break for unintended uses.
   * @param other
   */
  mergeInDefaults(other: RuleBehavior) {
    let keystroke = this.transcription.keystroke;
    let keyFromOther = other.transcription.keystroke;
    if(keystroke.Lcode != keyFromOther.Lcode || keystroke.Lmodifiers != keyFromOther.Lmodifiers) {
      throw "RuleBehavior default-merge not supported unless keystrokes are identical!";
    }

    this.triggersDefaultCommand = this.triggersDefaultCommand || other.triggersDefaultCommand;

    let mergingMock = Mock.from(this.transcription.preInput, false);
    mergingMock.apply(this.transcription.transform);
    mergingMock.apply(other.transcription.transform);

    this.transcription = mergingMock.buildTranscriptionFrom(this.transcription.preInput, keystroke, false, this.transcription.alternates);
  }
}