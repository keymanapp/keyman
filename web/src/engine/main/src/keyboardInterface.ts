import { KeymanWebKeyboard } from '@keymanapp/common-types';
import { KeyboardInterface as KeyboardInterfaceBase } from 'keyman/engine/js-processor';
import { KeyboardStub, RawKeyboardStub, toUnprefixedKeyboardId as unprefixed } from 'keyman/engine/keyboard-storage';

import KeyboardObject = KeymanWebKeyboard.KeyboardObject;

import { ContextManagerBase } from './contextManagerBase.js';
import { VariableStoreCookieSerializer } from "./variableStoreCookieSerializer.js";
import KeymanEngine from "./keymanEngine.js";
import { EngineConfiguration } from "./engineConfiguration.js";

export default class KeyboardInterface<ContextManagerType extends ContextManagerBase<any>> extends KeyboardInterfaceBase {
  protected readonly engine: KeymanEngine<EngineConfiguration, ContextManagerType, any>;
  private stubNamespacer?: (stub: RawKeyboardStub) => void;

  constructor(
    _jsGlobal: any,
    engine: KeymanEngine<any, ContextManagerType, any>,
    stubNamespacer?: (stub: RawKeyboardStub) => void
  ) {
    super(_jsGlobal, engine, new VariableStoreCookieSerializer());
    this.engine = engine;
    this.stubNamespacer = stubNamespacer;
  }

  // Preserves a keyboard's ID, even if namespaced, via script tag tagging.
  preserveID(Pk: any /** a `Keyboard`'s `scriptObject` entry */) {
    var trueID;

    // Find the currently-executing script tag; KR is called directly from each keyboard's definition script.
    if(document.currentScript) {
      trueID = document.currentScript.id;
    } else {
      var scripts = document.getElementsByTagName('script');
      var currentScript = scripts[scripts.length-1];

      trueID = currentScript.id;
    }

    // Final check that the script tag is valid and appropriate for the loading keyboard.
    if(!trueID) {
      return;
    } else if(trueID.indexOf(unprefixed(Pk['KI'])) != -1) {
      Pk['KI'] = trueID;  // Take the script's version of the ID, which may include package namespacing.
    } else {
      console.error("Error when registering keyboard:  current SCRIPT tag's ID does not match!");
    }
  }

  registerKeyboard(Pk: KeyboardObject): void {
    // Among other things, sets Pk as a newly-active Keyboard.
    super.registerKeyboard(Pk);
    const registeredKeyboard = this.loadedKeyboard;

    this.preserveID(Pk);

    this.engine.config.deferForInitialization.then(() => {
      if(!this.engine.keyboardRequisitioner.cache.isFetchingKeyboard(registeredKeyboard.id)) {
        // Deliberate keyboard pre-loading via direct script-tag link on the page.
        // Just load the keyboard and reset the harness's keyboard-receiver field.
        this.engine.keyboardRequisitioner.cache.addKeyboard(registeredKeyboard);
        this.loadedKeyboard = null;
      }
    });
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

    // This is where app-hosted KeymanWeb receives pre-formed stubs.
    // They're specified in the "internal" format (KI, KN, KLC...)
    // (SHIFT-CTRL-F @ repo-level for the mobile apps:  `setKeymanLanguage`)
    // Keyman Developer may also use this method directly for its test-host page.
    //
    // It may also be used by documented legacy API:
    // https://help.keyman.com/developer/engine/web/2.0/guide/examples/manual-control
    // (See: referenced laokeys_load.js)
    //
    // The mobile apps typically have fully-preconfigured paths, but Developer's
    // test-host page does not.

    const buildStub = () => {
      const pathConfig = this.engine.config.paths;
      return new KeyboardStub(Pstub, pathConfig.keyboards, pathConfig.fonts);
    };

    if(!this.engine.config.deferForInitialization.isResolved) {
      // pathConfig is not ready until KMW initializes, which prevents proper stub-building.
      this.engine.config.deferForInitialization.then(() => this.engine.keyboardRequisitioner.cache.addStub(buildStub()));
    } else {
      const stub = buildStub();

      if(this.engine.keyboardRequisitioner?.cache.findMatchingStub(stub)) {
        return 1;
      }
      this.engine.keyboardRequisitioner.cache.addStub(stub);
    }

    return null;
  }

  insertText = (Ptext: string, PdeadKey:number): void => {
    this.resetContextCache();
    // As this function isn't provided a handle to an active outputTarget, we rely on
    // the context manager to resolve said issue.
    this.engine.contextManager.insertText(this, Ptext, PdeadKey);
  }

  // Short-hand name: necessary to do it this way due to assignment style.
  KT = this.insertText;
}

(function() {
  KeyboardInterface.__publishShorthandAPI();
}());