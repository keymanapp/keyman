/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { LexicalModelTypes } from '@keymanapp/common-types';
import { Transcription } from './transcription.js';
import { VariableStore, VariableStoreDictionary } from '../variableStore.js';
import { SystemStoreDictionary } from '../systemStore.js';

/**
 * Represents the commands and state changes that result from a matched keyboard rule.
 */
export class ProcessorAction {
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
  setStore: SystemStoreDictionary = {};

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
}