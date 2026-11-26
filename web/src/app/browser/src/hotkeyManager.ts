/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { _GetEventKeyCode } from './hardwareEventKeyboard.js';
import { KeyEventFullResultCallback } from 'keyman/engine/main';
import { KeyEvent } from 'keyman/engine/keyboard';
import { ModifierKeyConstants } from '@keymanapp/common-types';

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
      (e.shiftKey ? ModifierKeyConstants.K_SHIFTFLAG : 0) |
      (e.ctrlKey ? ModifierKeyConstants.K_CTRLFLAG : 0) |
      (e.altKey ? ModifierKeyConstants.K_ALTFLAG : 0);

    for(let i=0; i<this.hotkeys.length; i++) {
      if(this.hotkeys[i].matches(_Lcode, _Lmodifiers)) {
        this.hotkeys[i].handler();
        e.returnValue = false;
        e?.preventDefault();
        e.cancelBubble = true;
        e.stopImmediatePropagation();
        return false;
      }
    }
    return true;
  }

  public _ProcessKeyEvent: (event: KeyEvent, callback: KeyEventFullResultCallback) => void = (event: KeyEvent, callback: KeyEventFullResultCallback): void => {
    const keyboardEvent = new KeyboardEvent('keydown', {
      keyCode: event.Lcode,
      key: event.kName,
      altKey: (event.Lmodifiers & (ModifierKeyConstants.K_ALTFLAG | ModifierKeyConstants.LALTFLAG | ModifierKeyConstants.RALTFLAG)) != 0,
      ctrlKey: (event.Lmodifiers & (ModifierKeyConstants.K_CTRLFLAG | ModifierKeyConstants.LCTRLFLAG | ModifierKeyConstants.RCTRLFLAG)) != 0,
      metaKey: (event.Lmodifiers & ModifierKeyConstants.K_METAFLAG) != 0,
      shiftKey: (event.Lmodifiers & ModifierKeyConstants.K_SHIFTFLAG) != 0,
      isComposing: false
    });

    this._Process(keyboardEvent);
  }

}
