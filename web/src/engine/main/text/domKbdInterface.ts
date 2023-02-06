/*
 * This file defines DOM-specific keyboard API that is not available in headless mode, extending web-core's
 * base KeyboardInterface offerings.
 */
namespace com.keyman.text {
  /**
   * Function     registerKeyboard  KR
   * Scope        Public
   * @param       {Object}      Pk      Keyboard  object
   * Description  Register and load the keyboard.  This implementation overwrites web-core's intentionally,
   *              as web-core lacks access to the `KeyboardManager` object and class.
   */
  KeyboardInterface.prototype.registerKeyboard = function(Pk): void {
    let keyman = com.keyman.singleton;
    keyman.keyboardManager._registerKeyboard(Pk);
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
  KeyboardInterface.prototype.registerStub = function(Pstub): number {
    let keyboardManager = com.keyman.singleton.keyboardManager;
    if (keyboardManager.keymanweb.initialized) {
      // If language code already defined (or not specified in stub), check to see if stub already registered
      for(let Lk=0; Lk < keyboardManager.keyboardStubs.length; Lk++) {
        if(keyboardManager.keyboardStubs[Lk]['KI'] == Pstub['KI']) {
          if(Pstub['KLC'] == '' || (keyboardManager.keyboardStubs[Lk]['KLC'] == Pstub['KLC'])) {
            return 1; // no need to register
          }
        }
      }
    }
    keyboardManager._registerStub(Pstub);
    return null;
  }

  /**
   * Function     KT
   * Scope        Public
   * @param       {string}      Ptext     Text to insert
   * @param       {?number}     PdeadKey  Dead key number, if any (???)
   * @return      {boolean}               true if inserted
   * Description  Insert text into active control.  Is utilized by keyboards with custom help HTML and requires
   *              special DOM handling.
   */
  KeyboardInterface.prototype.insertText = function(Ptext: string, PdeadKey:number): boolean {
    let keyman = com.keyman.singleton;
    this.resetContextCache();

    // Find the correct output target to manipulate.
    let outputTarget: OutputTarget = this.activeTargetOutput ? this.activeTargetOutput : dom.Utils.getOutputTarget();

    if(outputTarget != null) {
      // Required for the `sil_euro_latin` keyboard's desktop OSK/table to function properly.
      keyman.uiManager.setActivatingUI(true);
      dom.DOMEventHandlers.states._IgnoreNextSelChange = 100;
      keyman.domManager.focusLastActiveElement();
      dom.DOMEventHandlers.states._IgnoreNextSelChange = 0;

      if(Ptext!=null) {
        this.output(0, outputTarget, Ptext);
      }

      if((typeof(PdeadKey)!=='undefined') && (PdeadKey !== null)) {
        this.deadkeyOutput(0, outputTarget, PdeadKey);
      }

      outputTarget.invalidateSelection();
      return true;
    }
    return false;
  }

  /**
   * Function     KSF
   * Scope        Public
   * Description  Save keyboard focus
   */
  KeyboardInterface.prototype.saveFocus = function(): void {
    dom.DOMEventHandlers.states._IgnoreNextSelChange = 1;
  }

  /**
   * Legacy entry points (non-standard names)- included only to allow existing IME keyboards to continue to be used
   */
  KeyboardInterface.prototype['getLastActiveElement'] = function(): OutputTarget {
    return dom.Utils.getOutputTarget();
  }

  KeyboardInterface.prototype['focusLastActiveElement'] = function(): void {
    let keyman = com.keyman.singleton;
    keyman.domManager.focusLastActiveElement();
  }

  //The following entry points are defined but should not normally be used in a keyboard, as OSK display is no longer determined by the keyboard
  KeyboardInterface.prototype['hideHelp'] = function(): void {
    let keyman = com.keyman.singleton;
    keyman.osk.startHide(true);
  }

  KeyboardInterface.prototype['showHelp'] = function(Px: number, Py: number): void {
    let keyman = com.keyman.singleton;
    if(keyman.osk instanceof osk.FloatingOSKView) {
      keyman.osk.presentAtPosition(Px,Py);
    } else {
      keyman.osk.present();
    }
  }

  KeyboardInterface.prototype['showPinnedHelp'] = function(): void {
    let keyman = com.keyman.singleton;
    if(keyman.osk instanceof osk.FloatingOSKView) {
      // An old KMW bug previously auto-unset the affected field when this function was
      // used by CJK keyboards during rule processing.  As a result, we need to condition
      // on whether or not:
      // 1.  The active keyboard is CJK
      // 2.  A keyboard rule is actively processing.
      //
      // If BOTH are true, we do NOT mutate keyman.osk.userPositioned.
      // Otherwise, not all conditions are met, so we still allow OSK pinning.
      if(!keyman.core.activeKeyboard.isCJK || !this.ruleBehavior) {
        keyman.osk.userPositioned=true;
      }
    }
    // Automatically reuses previously-set positioning.
    // Other OSK API functions must have previously been used to set the
    // pinned position.
    keyman.osk.present();
  }

  // Also needed for some legacy CJK keyboards.
  KeyboardInterface.prototype['GetLastActiveElement'] = KeyboardInterface.prototype['getLastActiveElement'];
  KeyboardInterface.prototype['FocusLastActiveElement'] = KeyboardInterface.prototype['focusLastActiveElement'];
  KeyboardInterface.prototype['HideHelp'] = KeyboardInterface.prototype['hideHelp'];
  KeyboardInterface.prototype['ShowHelp'] = KeyboardInterface.prototype['showHelp'];
  KeyboardInterface.prototype['ShowPinnedHelp'] = KeyboardInterface.prototype['showPinnedHelp'];
}