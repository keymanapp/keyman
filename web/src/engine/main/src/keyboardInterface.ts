import {
  Keyboard,
  KeyboardInterface as KeyboardInterfaceBase,
  KeyboardKeymanGlobal,
} from "@keymanapp/keyboard-processor";
import { KeyboardStub, RawKeyboardStub, StubAndKeyboardCache } from 'keyman/engine/keyboard-cache';

import { ContextManagerBase } from './contextManagerBase.js';
import { VariableStoreCookieSerializer } from "./variableStoreCookieSerializer.js";

export default class KeyboardInterface extends KeyboardInterfaceBase {
  private readonly contextManager: ContextManagerBase;
  private stubAndKeyboardCache: StubAndKeyboardCache;
  private stubNamespacer?: (stub: RawKeyboardStub) => void;

  constructor(
    _jsGlobal: any,
    keymanGlobal: KeyboardKeymanGlobal,
    contextManager: ContextManagerBase,
    stubNamespacer?: (stub: RawKeyboardStub) => void
  ) {
    super(_jsGlobal, keymanGlobal, new VariableStoreCookieSerializer());
    this.contextManager = contextManager;
    this.stubNamespacer = stubNamespacer;
  }

  setKeyboardCache(cache: StubAndKeyboardCache) {
    this.stubAndKeyboardCache = cache;
  }

  registerKeyboard(Pk): void {
    // Among other things, sets Pk as a newly-active Keyboard.
    super.registerKeyboard(Pk);
    const registeredKeyboard = this.loadedKeyboard;

    const cacheEntry = this.stubAndKeyboardCache.getKeyboard(registeredKeyboard.id);
    if(!(cacheEntry instanceof Promise)) {
      // Deliberate keyboard pre-loading via direct script-tag link on the page.
      // Just load the keyboard and reset the harness's keyboard-receiver field.
      this.stubAndKeyboardCache.addKeyboard(new Keyboard(Pk));
      this.loadedKeyboard = null;
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
  registerStub(Pstub: RawKeyboardStub): number {
    if(this.stubNamespacer) {
      this.stubNamespacer(Pstub);
    }

    // Other notes:  this is where app-hosted KeymanWeb receives pre-formed stubs.
    // They're specified in the "internal" format (KI, KN, KLC...)
    // (SHIFT-CTRL-F @ repo-level:  `setKeymanLanguage`)
    //
    // It may also be used by documented legacy API:
    // https://help.keyman.com/DEVELOPER/ENGINE/WEB/2.0/guide/examples/manual-control
    // (See: referenced laokeys_load.js)
    const stub = new KeyboardStub(Pstub);
    if(this.stubAndKeyboardCache.findMatchingStub(stub)) {
      return 1;
    }

    this.stubAndKeyboardCache.addStub(stub);
    return null;
  }

  insertText = (Ptext: string, PdeadKey:number): void => {
    this.resetContextCache();
    // As this function isn't provided a handle to an active outputTarget, we rely on
    // the context manager to resolve said issue.
    this.contextManager.insertText(this, Ptext, PdeadKey);
  }
}

(function() {
  KeyboardInterface.__publishShorthandAPI();
}());