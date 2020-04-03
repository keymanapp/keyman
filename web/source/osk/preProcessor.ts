namespace com.keyman.osk {
  export class PreProcessor {
    static _GetClickEventProperties(e: keyboards.ActiveKey, Lelem: HTMLElement): text.KeyEvent {
      let keyman = com.keyman.singleton;
      let processor = keyman.textProcessor;

      var activeKeyboard = processor.activeKeyboard;
      let formFactor = keyman.util.device.formFactor;

      // Get key name and keyboard shift state (needed only for default layouts and physical keyboard handling)
      // Note - virtual keys should be treated case-insensitive, so we force uppercasing here.
      var layer = e.layer || e.displayLayer || '', keyName=e.id.toUpperCase();

      // Start:  mirrors _GetKeyEventProperties

      // Override key shift state if specified for key in layout (corrected for popup keys KMEW-93)
      var keyShiftState = processor.core.getModifierState(layer);

      // First check the virtual key, and process shift, control, alt or function keys
      var Lkc: text.KeyEvent = {
        Ltarg: dom.Utils.getOutputTarget(Lelem),
        Lmodifiers: keyShiftState,
        Lstates: 0,
        Lcode: text.Codes.keyCodes[keyName],
        LisVirtualKey: true,
        vkCode: 0,
        kName: keyName,
        kLayer: layer,
        kbdLayer: e.displayLayer,
        kNextLayer: e.nextlayer,
        device: keyman.util.device.coreSpec,  // The OSK's events always use the 'true' device.
        isSynthetic: true
      };

      // If it's actually a state key modifier, trigger its effects immediately, as KeyboardEvents would do the same.
      switch(keyName) {
        case 'K_CAPS':
        case 'K_NUMLOCK':
        case 'K_SCROLL':
          processor.core.stateKeys[keyName] = ! processor.core.stateKeys[keyName];
      }

      // Performs common pre-analysis for both 'native' and 'embedded' OSK key & subkey input events.
      processor.core.setSyntheticEventDefaults(Lkc);

      // End - mirrors _GetKeyEventProperties

      // Include *limited* support for mnemonic keyboards (Sept 2012)
      // If a touch layout has been defined for a mnemonic keyout, do not perform mnemonic mapping for rules on touch devices.
      if(activeKeyboard && activeKeyboard.isMnemonic && !(activeKeyboard.layout(formFactor as text.FormFactor).isDefault && formFactor != 'desktop')) {
        if(Lkc.Lcode != text.Codes.keyCodes['K_SPACE']) { // exception required, March 2013
          // Jan 2019 - interesting that 'K_SPACE' also affects the caps-state check...
          Lkc.vkCode = Lkc.Lcode;
          text.KeyboardProcessor.setMnemonicCode(Lkc, layer.indexOf('shift') != -1, processor.core.stateKeys['K_CAPS']);
        }
      } else {
        Lkc.vkCode=Lkc.Lcode;
      }

      // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
      if(!activeKeyboard.definesPositionalOrMnemonic) {
        Lkc.Lcode = KeyMapping._USKeyCodeToCharCode(Lkc);
        Lkc.LisVirtualKey=false;
      }

      return Lkc;
    }

    /**
     * Simulate a keystroke according to the touched keyboard button element
     *
     * Note that the test-case oriented 'recorder' stubs this method to facilitate OSK-based input
     * recording for use in test cases.  If changing this function, please ensure the recorder is
     * not affected.
     * 
     * @param       {Object}      e      element touched (or clicked)
     */
    static clickKey(e: osk.KeyElement, touch?: Touch, layerId?: string, keyDistribution?: text.KeyDistribution) {
      let keyman = com.keyman.singleton;
      var Lelem = keyman.domManager.getLastActiveElement();

      if(Lelem != null) {
        // Handle any DOM state management related to click inputs.
        let outputTarget = dom.Utils.getOutputTarget(Lelem);
        keyman.domManager.initActiveElement(Lelem);
  
        // Turn off key highlighting (or preview)
        keyman['osk'].vkbd.highlightKey(e,false);
        
        // Clear any cached codepoint data; we can rebuild it if it's unchanged.
        outputTarget.invalidateSelection();
        // Deadkey matching continues to be troublesome.
        // Deleting matched deadkeys here seems to correct some of the issues.   (JD 6/6/14)
        outputTarget.deadkeys().deleteMatched();      // Delete any matched deadkeys before continuing
  
        let Lkc = PreProcessor._GetClickEventProperties(e['key'].spec as keyboards.ActiveKey, Lelem);
        if(keyman.modelManager.enabled) {
          Lkc.source = touch;
          Lkc.keyDistribution = keyDistribution;
        }

        if(!keyman.isEmbedded) {
          keyman.uiManager.setActivatingUI(true);
          com.keyman.dom.DOMEventHandlers.states._IgnoreNextSelChange = 100;
          keyman.domManager.focusLastActiveElement();
          com.keyman.dom.DOMEventHandlers.states._IgnoreNextSelChange = 0;
        }

        let retVal = PreProcessor.handleClick(Lkc, e);

        // Now that processing is done, we can do a bit of post-processing, too.
        keyman.uiManager.setActivatingUI(false);	// I2498 - KeymanWeb OSK does not accept clicks in FF when using automatic UI
        return retVal;
      } else {
        return true;
      }
    }

    // Serves to hold DOM-dependent code that affects both 'native' and 'embedded' mode OSK use 
    // after the KeyEvent object has been properly instantiated.  This should help catch any 
    // mutual last-minute DOM-side interactions before passing control to the processor... such as
    // the UI-control command keys as seen below.
    static handleClick(Lkc: text.KeyEvent, e: KeyElement) {
      let keyman = com.keyman.singleton;
        // Exclude menu and OSK hide keys from normal click processing
      if(Lkc.kName == 'K_LOPT' || Lkc.kName == 'K_ROPT') {
        keyman['osk'].vkbd.optionKey(e, Lkc.kName, true);
        return true;
      }

      let retVal = keyman.textProcessor.processKeyEvent(Lkc);

      return retVal;
    }
  }
}