import {
  Keyboard,
  KeyboardInterface as KeyboardInterfaceBase,
  KeyboardKeymanGlobal,
} from "@keymanapp/keyboard-processor";
import { KeyboardStub, StubAndKeyboardCache } from '../../../../build/engine/keyboard-cache/obj/index.js';

import ContextManager from './contextManager.js';
import { VariableStoreCookieSerializer } from "./variableStoreCookieSerializer.js";

export default class KeyboardInterface extends KeyboardInterfaceBase {
  private readonly contextManager: ContextManager;
  private readonly stubAndKeyboardCache: StubAndKeyboardCache;

  constructor(
    _jsGlobal: any,
    keymanGlobal: KeyboardKeymanGlobal,
    cache: StubAndKeyboardCache,
  ) {
    super(_jsGlobal, keymanGlobal, new VariableStoreCookieSerializer());
    this.stubAndKeyboardCache = cache;
  }

  registerKeyboard(Pk): void {
    const priorActiveKeyboard = this.activeKeyboard;

    // Among other things, sets Pk as a newly-active Keyboard.
    super.registerKeyboard(Pk);
    const registeredKeyboard = this.activeKeyboard;

    const cacheEntry = this.stubAndKeyboardCache.getKeyboard(registeredKeyboard.id);
    if(!(cacheEntry instanceof Promise)) {
      // Deliberate keyboard pre-loading via direct script-tag link on the page.
      // Just load the keyboard and set our field back in place.
      this.stubAndKeyboardCache.addKeyboard(new Keyboard(Pk));
      this.activeKeyboard = priorActiveKeyboard;
    }
  }

  /**
   * Add the basic keyboard parameters (keyboard stub) to the array of keyboard stubs
   * If no language code is specified in a keyboard it cannot be registered,
   * and a keyboard stub must be registered before the keyboard is loaded
   * for the keyboard to be usable.
   *
   * @param       {Object}      Pstub     Keyboard stub object
   * @return      {?number}               1 if already registered, else null
   */
  registerStub = (Pstub): number => {
    const stub = new KeyboardStub(Pstub);
    if(this.stubAndKeyboardCache.findMatchingStub(stub)) {
      return 1;
    }

    return null;
  }

  insertText = (Ptext: string, PdeadKey:number): void => {
    this.resetContextCache();
    this.contextManager.insertText(this, Ptext, PdeadKey);
  }

  // *** The following are quite useful for website-integrating KMW, but not needed for the embedded form. ***
  //
  // /**
  //  * Function     KSF
  //  * Scope        Public
  //  * Description  Save keyboard focus
  //  */
  // saveFocus(): void {
  //   dom.DOMEventHandlers.states._IgnoreNextSelChange = 1;
  // }

  // /**
  //  * Legacy entry points (non-standard names)- included only to allow existing IME keyboards to continue to be used
  //  */
  // getLastActiveElement(): OutputTarget {
  //   return dom.Utils.getOutputTarget();
  // }

  // focusLastActiveElement(): void {
  //   let keyman = com.keyman.singleton;
  //   keyman.domManager.focusLastActiveElement();
  // }

  // //The following entry points are defined but should not normally be used in a keyboard, as OSK display is no longer determined by the keyboard
  // hideHelp(): void {
  //   let keyman = com.keyman.singleton;
  //   keyman.osk.startHide(true);
  // }

  // showHelp(Px: number, Py: number): void {
  //   let keyman = com.keyman.singleton;
  //   if(keyman.osk instanceof osk.FloatingOSKView) {
  //     keyman.osk.presentAtPosition(Px,Py);
  //   } else {
  //     keyman.osk.present();
  //   }
  // }

  // showPinnedHelp(): void {
  //   let keyman = com.keyman.singleton;
  //   if(keyman.osk instanceof osk.FloatingOSKView) {
  //     // An old KMW bug previously auto-unset the affected field when this function was
  //     // used by CJK keyboards during rule processing.  As a result, we need to condition
  //     // on whether or not:
  //     // 1.  The active keyboard is CJK
  //     // 2.  A keyboard rule is actively processing.
  //     //
  //     // If BOTH are true, we do NOT mutate keyman.osk.userPositioned.
  //     // Otherwise, not all conditions are met, so we still allow OSK pinning.
  //     if(!keyman.core.activeKeyboard.isCJK || !this.ruleBehavior) {
  //       keyman.osk.userPositioned=true;
  //     }
  //   }
  //   // Automatically reuses previously-set positioning.
  //   // Other OSK API functions must have previously been used to set the
  //   // pinned position.
  //   keyman.osk.present();
  // }

  // // Also needed for some legacy CJK keyboards.
  // readonly GetLastActiveElement = this.getLastActiveElement;
  // readonly FocusLastActiveElement = this.focusLastActiveElement;
  // readonly HideHelp = this.hideHelp;
  // readonly ShowHelp = this.showHelp;
  // readonly ShowPinnedHelp = this.showPinnedHelp;
}

(function() {
  // This will be the only call within the keyboard-processor module.
  KeyboardInterface.__publishShorthandAPI();
}());