namespace com.keyman.osk {
  export class PreProcessor {
    /**
     * Simulate a keystroke according to the touched keyboard button element
     *
     * Note that the test-case oriented 'recorder' stubs this method to facilitate OSK-based input
     * recording for use in test cases.  If changing this function, please ensure the recorder is
     * not affected.
     * 
     * @param       {Object}      e      element touched (or clicked)
     */
    static clickKey(e: osk.KeyElement, input?: InputEventCoordinate) {
      let keyman = com.keyman.singleton;
      let Lkc = keyman['osk'].vkbd.initKeyEvent(e, input);
      if(!Lkc) {
        return true;
      }

      return this.raiseKeyEvent(Lkc);
    }

    static raiseKeyEvent(keyEvent: text.KeyEvent) {
      let keyman = com.keyman.singleton;
      var Lelem = keyman.domManager.lastActiveElement;

      if(Lelem != null) {
        // Handle any DOM state management related to click inputs.
        let outputTarget = dom.Utils.getOutputTarget(Lelem);
        keyman.domManager.initActiveElement(Lelem);
        
        // Clear any cached codepoint data; we can rebuild it if it's unchanged.
        outputTarget.invalidateSelection();
        // Deadkey matching continues to be troublesome.
        // Deleting matched deadkeys here seems to correct some of the issues.   (JD 6/6/14)
        outputTarget.deadkeys().deleteMatched();      // Delete any matched deadkeys before continuing

        if(!keyman.isEmbedded) {
          keyman.uiManager.setActivatingUI(true);
          com.keyman.dom.DOMEventHandlers.states._IgnoreNextSelChange = 100;
          keyman.domManager.focusLastActiveElement();
          com.keyman.dom.DOMEventHandlers.states._IgnoreNextSelChange = 0;
        }

        let retVal = PreProcessor.handleClick(keyEvent, outputTarget, null);

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
    static handleClick(Lkc: text.KeyEvent, outputTarget: text.OutputTarget, e: osk.KeyElement) {
      let keyman = com.keyman.singleton;
        // Exclude menu and OSK hide keys from normal click processing
      if(Lkc.kName == 'K_LOPT' || Lkc.kName == 'K_ROPT') {
        keyman['osk'].vkbd.optionKey(e, Lkc.kName, true);
        return true;
      }

      let retVal = !!keyman.core.processKeyEvent(Lkc, outputTarget);

      return retVal;
    }
  }
}