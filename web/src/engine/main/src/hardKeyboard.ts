import EventEmitter from "eventemitter3";
import { type KeyEvent, type RuleBehavior } from "@keymanapp/keyboard-processor";
import KeyEventSourceInterface from './keyEventSource.interface.js';

interface EventMap {
  /**
   * Designed to pass key events off to any consuming modules/libraries.
   */
  'keyevent': (event: KeyEvent, callback?: (result: RuleBehavior, error?: Error) => void) => void
}

export default class HardKeyboard extends EventEmitter<EventMap> implements KeyEventSourceInterface { }

// Intended design:
// - KeyEventKeyboard:  website-integrated handler for hardware-keystroke input; interprets DOM events.
//   - app/web
// - AppPassthroughKeyboard:  WebView-hosted forwarding of hardware key events through to the Web engine.
//   - app/embed