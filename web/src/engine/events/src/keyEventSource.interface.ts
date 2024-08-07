import { EventEmitter } from "eventemitter3";
import { type KeyEvent, type RuleBehaviorInterface } from "keyman/engine/keyboard";

export type KeyEventResultCallback = (result: RuleBehaviorInterface, error?: Error) => void;
export type KeyEventHandler = (event: KeyEvent, callback?: KeyEventResultCallback) => void;

interface EventMap {
  /**
   * Designed to pass key events off to any consuming modules/libraries.
   */
  'keyevent': KeyEventHandler;
}

export interface KeyEventSourceInterface<Map extends EventMap> extends EventEmitter<Map> { }