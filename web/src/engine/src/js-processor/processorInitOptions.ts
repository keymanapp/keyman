/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { DefaultOutputRules } from "keyman/engine/keyboard";
import { JSKeyboardInterface } from './jsKeyboardInterface.js';

export interface ProcessorInitOptions {
  baseLayout: string;
  keyboardInterface: JSKeyboardInterface;
  defaultOutputRules: DefaultOutputRules;
}
