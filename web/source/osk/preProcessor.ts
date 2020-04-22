namespace com.keyman.osk {
  export class PreProcessor {
    static _GetClickEventProperties(e: keyboards.ActiveKey, Lelem: HTMLElement): text.KeyEvent {
      let keyman = com.keyman.singleton;
      let core = keyman.core;

      // Start:  mirrors _GetKeyEventProperties

      // First check the virtual key, and process shift, control, alt or function keys
      let Lkc = e.constructKeyEvent(core.keyboardProcessor, dom.Utils.getOutputTarget(Lelem), keyman.util.device.coreSpec);

      // If it's actually a state key modifier, trigger its effects immediately, as KeyboardEvents would do the same.
      switch(Lkc.kName) {
        case 'K_CAPS':
        case 'K_NUMLOCK':
        case 'K_SCROLL':
          core.keyboardProcessor.stateKeys[Lkc.kName] = ! core.keyboardProcessor.stateKeys[Lkc.kName];
      }

      // End - mirrors _GetKeyEventProperties
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
        if(keyman.core.languageProcessor.isActive) {
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

      let retVal = (keyman.core.processKeyEvent(Lkc) != null);

      return retVal;
    }
  }
}