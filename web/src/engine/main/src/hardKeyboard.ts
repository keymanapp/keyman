import EventEmitter from "eventemitter3";
import { type KeyEvent } from "@keymanapp/keyboard-processor";

interface EventMap {
  /**
   * Designed to pass key events off to any consuming modules/libraries.
   */
  'keyEvent': (event: KeyEvent) => void
}

export default class HardKeyboard extends EventEmitter<EventMap> { }

// Intended design:
// - KeyEventKeyboard:  website-integrated handler for hardware-keystroke input; interprets DOM events.
//   - app/web
// - AppPassthroughKeyboard:  WebView-hosted forwarding of hardware key events through to the Web engine.
//   - app/embed