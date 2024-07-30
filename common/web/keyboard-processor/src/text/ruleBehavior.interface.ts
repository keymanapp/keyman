import { type Transcription } from './outputTarget.js';

/**
 * Represents the commands and state changes that result from a matched keyboard rule.
 */
export interface RuleBehaviorInterface {
  /**
   * The before-and-after Transform from matching a keyboard rule.  May be `null`
   * if no keyboard rules were matched for the keystroke.
   */
  transcription: Transcription;
}
