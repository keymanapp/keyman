import { EventEmitter } from "eventemitter3";
import { type KeyEvent } from 'keyman/engine/keyboard';
import { type RuleBehavior } from 'keyman/engine/js-processor';

export type KeyEventResultCallback = (result: RuleBehavior, error?: Error) => void;
export type KeyEventHandler = (event: KeyEvent, callback?: KeyEventResultCallback) => void;

interface EventMap {
  /**
   * Designed to pass key events off to any consuming modules/libraries.
   */
  'keyevent': KeyEventHandler;
}

export interface KeyEventSourceInterface<Map extends EventMap> extends EventEmitter<Map> { }