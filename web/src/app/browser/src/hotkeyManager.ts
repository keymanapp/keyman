/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { _GetEventKeyCode } from './hardwareEventKeyboard.js';

class Hotkey {
  code: number;
  shift: number;
  handler: () => void

  constructor(code: number, shift: number, handler: () => void) {
    this.code = code;
    this.shift = shift;
    this.handler = handler;
  }

  matches(keyCode: number, shiftState: number): boolean {
    return (this.code == keyCode && this.shift == shiftState);
  }
}

export class HotkeyManager {
  hotkeys: Hotkey[] = [];

  /**
   * Add hot key handler to array of document-level hotkeys triggered by key up event
   *
   * @param       {number}            keyCode
   * @param       {number}            shiftState
   * @param       {function(Object)}  handler
   */
  public addHotKey(keyCode: number, shiftState: number, handler: () => void) {
    // Test if existing handler for this code and replace it if so
    for(let i=0; i<this.hotkeys.length; i++) {
      if(this.hotkeys[i].code == keyCode && this.hotkeys[i].shift == shiftState) {
        this.hotkeys[i].handler = handler;
        return;
      }
    }

    // Otherwise add it to the array
    this.hotkeys.push(new Hotkey(keyCode, shiftState, handler));
  }

  /**
   * Remove a hot key handler from array of document-level hotkeys triggered
   * by key up event
   *
   * @param       {number}        keyCode
   * @param       {number}        shiftState
   */
  public removeHotkey(keyCode: number, shiftState: number) {
    for(let i=0; i<this.hotkeys.length; i++) {
      if(this.hotkeys[i].matches(keyCode, shiftState)) {
        this.hotkeys.splice(i,1);
        return;
      }
    }
  }

  /**
   * Passes control to handlers according to the hotkey pressed
   *
   * @param       {Event}       e       event
   */
  public _Process: (e: KeyboardEvent) => boolean = (e: KeyboardEvent) => {
    if(!e) {
      e = window.event as KeyboardEvent;
    }

    console.log(`HotkeyManager._Process: ${e.type} keyCode=${e.keyCode} code=${e.code} key=${e.key} shift=${e.shiftKey} ctrl=${e.ctrlKey} alt=${e.altKey} meta=${e.metaKey}`);
    const _Lcode = _GetEventKeyCode(e);
    if(_Lcode == null) {
      return false;
    }

    // Removed testing of e.shiftKey==null  I3363 (Build 301)
    const _Lmodifiers =
      (e.shiftKey ? 0x10 : 0) |
      (e.ctrlKey ? 0x20 : 0) |
      (e.altKey ? 0x40 : 0);

    for(let i=0; i<this.hotkeys.length; i++) {
      if(this.hotkeys[i].matches(_Lcode, _Lmodifiers)) {
        this.hotkeys[i].handler();
        e.returnValue = false;
        if(e && e.preventDefault) {
          e.preventDefault();
        }
        e.cancelBubble = true;
        return false;
      }
    }
    return true;
  }
}