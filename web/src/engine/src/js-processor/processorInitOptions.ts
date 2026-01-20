/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { DefaultOutputRules } from "keyman/engine/keyboard";
import { JSKeyboardInterface } from './jsKeyboardInterface.js';

/**
 * Configuration options for initializing a keyboard processor.
 */
export interface ProcessorInitOptions {
  /**
   * The base layout identifier for the keyboard.
   */
  baseLayout: string;

  /**
   * The keyboard interface instance to use for processing.
   */
  keyboardInterface: JSKeyboardInterface;

  /**
   * The default output rules configuration for key processing.
   */
  defaultOutputRules: DefaultOutputRules;
}
