namespace com.keyman.osk {
  export class PreProcessor {
    static _GetClickEventProperties(e: osk.ActiveKey, Lelem: HTMLElement): text.KeyEvent {
      let keyman = com.keyman.singleton;
      let processor = keyman.textProcessor;

      var activeKeyboard = processor.activeKeyboard;
      let formFactor = keyman.util.device.formFactor;

      // Get key name and keyboard shift state (needed only for default layouts and physical keyboard handling)
      // Note - virtual keys should be treated case-insensitive, so we force uppercasing here.
      var layer = e.layer || e.displayLayer || '', keyName=e.id.toUpperCase();

      // Start:  mirrors _GetKeyEventProperties

      // Override key shift state if specified for key in layout (corrected for popup keys KMEW-93)
      var keyShiftState = processor.getModifierState(layer);

      // First check the virtual key, and process shift, control, alt or function keys
      var Lkc: text.KeyEvent = {
        Ltarg: text.Processor.getOutputTarget(Lelem),
        Lmodifiers: keyShiftState,
        Lstates: 0,
        Lcode: text.Codes.keyCodes[keyName],
        LisVirtualKey: true,
        vkCode: 0,
        kName: keyName,
        kLayer: layer,
        kbdLayer: e.displayLayer,
        kNextLayer: e.nextlayer,
        device: keyman.util.device.headlessSpec,  // The OSK's events always use the 'true' device.
        isSynthetic: true
      };

      // If it's actually a state key modifier, trigger its effects immediately, as KeyboardEvents would do the same.
      switch(keyName) {
        case 'K_CAPS':
        case 'K_NUMLOCK':
        case 'K_SCROLL':
          processor.stateKeys[keyName] = ! processor.stateKeys[keyName];
      }

      // Performs common pre-analysis for both 'native' and 'embedded' OSK key & subkey input events.
      processor.setSyntheticEventDefaults(Lkc);

      // End - mirrors _GetKeyEventProperties

      // Include *limited* support for mnemonic keyboards (Sept 2012)
      // If a touch layout has been defined for a mnemonic keyout, do not perform mnemonic mapping for rules on touch devices.
      if(activeKeyboard && activeKeyboard.isMnemonic && !(activeKeyboard.layouts && formFactor != 'desktop')) {
        if(Lkc.Lcode != text.Codes.keyCodes['K_SPACE']) { // exception required, March 2013
          // Jan 2019 - interesting that 'K_SPACE' also affects the caps-state check...
          Lkc.vkCode = Lkc.Lcode;
          text.Processor.setMnemonicCode(Lkc, layer.indexOf('shift') != -1, processor.stateKeys['K_CAPS']);
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
        let outputTarget = text.Processor.getOutputTarget(Lelem);
        keyman.domManager.initActiveElement(Lelem);
  
        // Turn off key highlighting (or preview)
        keyman['osk'].vkbd.highlightKey(e,false);
        
        // Clear any cached codepoint data; we can rebuild it if it's unchanged.
        outputTarget.invalidateSelection();
        // Deadkey matching continues to be troublesome.
        // Deleting matched deadkeys here seems to correct some of the issues.   (JD 6/6/14)
        outputTarget.deadkeys().deleteMatched();      // Delete any matched deadkeys before continuing
  
        let Lkc = PreProcessor._GetClickEventProperties(e['key'].spec as osk.ActiveKey, Lelem);
        if(keyman.modelManager.enabled) {
          Lkc.source = touch;
          Lkc.keyDistribution = keyDistribution;
        }
        return keyman.textProcessor.processKeyEvent(Lkc, e);
      } else {
        return true;
      }
    }
  }
}