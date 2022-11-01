// Since 'web' compilation is the path recognized by VSCode, we need to make references here to prevent TS errors.
// References the base Keyman object (and consequently, the rest of the core objects).
/// <reference path="kmwbase.ts" />
/// <reference path="osk/embedded/keytip.ts" />
/// <reference path="osk/embedded/pendingLongpress.ts" />

// KeymanWeb 11.0
// Copyright 2019 SIL International

/*****************************************/
/*                                       */
/*   Embedded application-specific code  */
/*                                       */
/*****************************************/

namespace com.keyman.osk {
  VisualKeyboard.prototype.optionKey = function(this: VisualKeyboard, e: KeyElement, keyName: string, keyDown: boolean) {
    let keyman = com.keyman.singleton;

    if(keyName.indexOf('K_LOPT') >= 0) {
      if(keyDown) {
        this.menuEvent = e;
        if(typeof keyman['showKeyboardList'] == 'function') {
          keyman['showKeyboardList']();
        }
      } else {
        if(this.menuEvent) {
          this.highlightKey(this.menuEvent, false);
        }
        if(typeof(window['menuKeyUp']) == 'function') {
          window['menuKeyUp']();
        }
        this.menuEvent = null;
      }
    } else if(keyName.indexOf('K_ROPT') >= 0) {
      if(keyDown) {
        this.highlightKey(e,false);
        if(typeof keyman['hideKeyboard'] == 'function') {
          keyman['hideKeyboard']();
        }
      }
    }
  };

  // iOS now relies upon native-mode popup key management, so we only implement these hybrid-targeted
  // methods when embedding in Android.
  let device = com.keyman.singleton.util.device;

  if(device.OS == 'Android') { // assumption - if this file is being loaded, keyman.isEmbedded == true.
    // Send the subkey array to iOS, with centre,top of base key position
    /**
     * Create a popup key array natively
     *
     * @param {Object}  key   base key element
     */
    VisualKeyboard.prototype.startLongpress = function(this: VisualKeyboard, key: KeyElement): PendingGesture {
      if(typeof(window['oskCreatePopup']) == 'function') {
        var xBase = dom.Utils.getAbsoluteX(key) - dom.Utils.getAbsoluteX(this.kbdDiv) + key.offsetWidth/2,
            yBase = dom.Utils.getAbsoluteY(key);

        // #3718: No longer prepend base key to subkey array
        window['oskCreatePopup'](key['subKeys'], xBase, yBase, key.offsetWidth, key.offsetHeight);

        return new embedded.PendingLongpress(this, key);
      } else {
        // When embedded within our Android app, we expect the `oskCreatePopup` function to
        // exist; all subkey control is delegated to the app.
        //
        // No function = big problem.
        console.error("Missing `oskCreatePopup` function for engine integration.");
        return null;
      }
    };

    // Create a keytip (dummy call - actual keytip handled by native code)
    VisualKeyboard.prototype.createKeyTip = function(this: VisualKeyboard) {
      if(com.keyman.singleton.util.device.formFactor == 'phone') {
        this.keytip = new osk.embedded.KeyTip(window['oskCreateKeyPreview'], window['oskClearKeyPreview']);
      }
    };
  }

  VisualKeyboard.prototype.waitForFonts = function(this: VisualKeyboard, kfd, ofd) {
    // a dummy function; it's only really used for 'native' KMW.
    return true;
  }

  SuggestionManager.prototype.platformHold = function(this: SuggestionManager, suggestionObj: BannerSuggestion, isCustom: boolean) {
    // Parallels VisualKeyboard.prototype.touchHold, but for predictive suggestions instead of keystrokes.
    let suggestionEle = suggestionObj.div;

    // Need to know if it's a <keep> option or not!

    var xBase = dom.Utils.getAbsoluteX(suggestionEle) - dom.Utils.getAbsoluteX(suggestionEle.parentElement) + suggestionEle.offsetWidth/2,
        yBase = dom.Utils.getAbsoluteY(suggestionEle) - dom.Utils.getAbsoluteY(suggestionEle.parentElement);

    window['suggestionPopup'](suggestionObj.suggestion, isCustom, xBase, yBase, suggestionEle.offsetWidth, suggestionEle.offsetHeight);
  }
}

namespace com.keyman.text {
  let nativeForBaseKeys = DefaultOutput.forBaseKeys;

  // Overrides the 'native'-mode implementation with in-app friendly defaults prioritized over 'native' defaults.
  DefaultOutput.forBaseKeys = function(Lkc: KeyEvent): string {
    let Codes = com.keyman.text.Codes;
    let code = Lkc.Lcode;

    // Intentionally not assigning K_TAB or K_ENTER so KMW will pass them back
    // to the mobile apps to handle (insert characters or navigate forms).
    if (code == Codes.keyCodes.K_oE2) {
      // Using defaults of English US layout for the 102nd key
      if (Lkc.Lmodifiers == Codes.modifierCodes['SHIFT']) {
        return '|';
      } else {
        return '\\';
      }
    }

    // Use 'native'-mode defaults, determining the character from the OSK
    return nativeForBaseKeys(Lkc);
  }
}

(function() {
  // Declare KeymanWeb and related objects
  var keymanweb=window['keyman'], util=keymanweb['util'],device=util.device;
  var dom = com.keyman.dom;

  // Allow definition of application name
  keymanweb.options['app']='';

  // Flag to control refreshing of a keyboard that is already loaded
  keymanweb.mustReloadKeyboard = true;

  // Skip full page initialization - skips native-mode only code
  keymanweb.isEmbedded = true;

  // Set default device options
  keymanweb.setDefaultDeviceOptions = function(opt: com.keyman.OptionType) {
    opt['attachType'] = 'manual';
    device.app=opt['app'];
    device.touchable=true;
    device.formFactor = device.app.indexOf('Tablet') >= 0 ? 'tablet' : 'phone';
    device.browser='native';
  };

  // Get default style sheet path
  keymanweb.getStyleSheetPath = function(ssName) {
    return keymanweb.rootPath+ssName;
  };

  // Get KMEI, KMEA keyboard path (overrides default function, allows direct app control of paths)
  keymanweb.getKeyboardPath = function(Lfilename, packageID) {
    return Lfilename + "?v=" + (new Date()).getTime(); /*cache buster*/
  };

  // Establishes keyboard namespacing.
  keymanweb.namespaceID = function(Pstub) {
    if(typeof(Pstub['KP']) != 'undefined') {
      // An embedded use case wants to utilize package-namespacing.
      Pstub['KI'] = Pstub['KP'] + "::" + Pstub['KI'];
    }
  }

  // In conjunction with the KeyboardManager's installKeyboard method and script IDs, preserves a keyboard's
  // namespaced ID.
  keymanweb.preserveID = function(Pk) {
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
    if(trueID.indexOf(Pk['KI']) != -1) {
      Pk['KI'] = trueID;  // Take the script's version of the ID, which may include package namespacing.
    } else {
      console.error("Error when registering keyboard:  current SCRIPT tag's ID does not match!");
    }
  }

    /**
   * Force reload of resource
   *
   *  @param  {string}  s unmodified URL
   *  @return {string}    modified URL
   */
  util.unCached = function(s) {
    var t=(new Date().getTime());
    s = s + '?v=' + t;
    return s;
  };

  util.wait = function() {
    // Empty stub - this function should not be implemented or used within embedded code routes.
    console.warn("util.wait() call attempted in embedded mode!");  // Sends log message to embedding app.
  };

  util.alert = function() {
    // Empty stub - this function should not be implemented or used within embedded code routes.
    console.warn("util.alert() call attempted in embedded mode!");  // Sends log message to embedding app.
  };

  /**
   * Refresh element content after change of text (if required)
   *
   *  @param  {Object}  Pelem   input element
   */
  keymanweb.refreshElementContent = function(Pelem)
  {
    if('ontextchange' in keymanweb) keymanweb['ontextchange'](Pelem);
  };

  /**
   * Set target element text direction (LTR or RTL): not functional for KMEI, KMEA
   *
   * @param       {Object}      Ptarg      Target element
   */
  keymanweb.domManager._SetTargDir = function(Ptarg){};

  /**
   * Align input fields (should not be needed with KMEI, KMEA)
   *
   *  @param  {object}   eleList    A list of specific elements to align.  If nil, selects all elements.
   *
   **/
  keymanweb.alignInputs = function(eleList: HTMLElement[]) {};

  /**
   * Use rotation events to adjust OSK element positions and scaling if necessary
   */
  keymanweb.handleRotationEvents = function() {};

  /**
   * Caret position always determined from the active (but hidden) element
   *
   * @return  {boolean}
   **/
  keymanweb.isPositionSynthesized = function()
  {
    return false;
  };

  /**
   * correctOSKTextSize handles rotation event -- currently rebuilds keyboard and adjusts font sizes
   */
  keymanweb['correctOSKTextSize']=function() {
    keymanweb.osk?.refreshLayout();
  };

  /**
   * Register a lexical model
   *
   * @param {com.keyman.text.prediction.ModelSpec} model  Spec of the lexical model
   */
  keymanweb['registerModel']=function(model: com.keyman.text.prediction.ModelSpec) {
    keymanweb.modelManager.register(model);
  };

  /**
   * Function called by Android and iOS when a device-implemented keyboard popup
   * is displayed or hidden.  As this is controlled by the app, we use it as a
   * trigger for 'embedded'-mode gesture state management.
   *
   *  @param  {boolean}  isVisible
   *
   **/
  keymanweb['popupVisible'] = function(isVisible) {
    let osk = keymanweb.osk;
    let gesture = osk.vkbd.subkeyGesture as com.keyman.osk.embedded.SubkeyDelegator;
    let pendingLongpress = osk.vkbd.pendingSubkey;

    /*
     * If a longpress popup was visible, but is no longer, this means that the
     * associated longpress gesture was cancelled.  It is possible for the base
     * key to emit if selected at this time; detection of this is managed by
     * the `SubkeyDelegator` class.
     */
    if(!isVisible) {
      if(gesture) {
        gesture.resolve(null);
        osk.vkbd.subkeyGesture = null;
      } else if(pendingLongpress) {
        pendingLongpress.cancel();
        osk.vkbd.pendingSubkey = null;
      }
    }

    /*
     * If the popup was not visible, but now is, that means our previously-pending
     * longpress is now 'realized' (complete).  The OSK relies upon this state
     * information, which will be properly updated by `resolve`.
     *
     * Prominent uses of such state info helps prevent change of base key, key
     * previews, and key output from occurring while a subkey popup remains active.
     */
    if(isVisible && pendingLongpress) {
      // Fulfills the first-stage promise.
      pendingLongpress.resolve();
    }
  };

  /**
   *  Return position of language menu key to KeymanTouch
   *
   *  @return  {string}      comma-separated x,y,w,h of language menu key
   *
   **/
  keymanweb['touchMenuPos'] = function() {
    let osk = keymanweb.osk;
    if(osk == null || osk.vkbd == null || osk.vkbd.lgKey == null) {
      return '';
    }

    var key: HTMLElement = osk.vkbd.lgKey;
    // A CSS change of kmd-key-square from position:fixed to position:static was needed
    // for Android 4.3 to display the OSK correctly, but resulted in the position of
    // the menu key not being returned correctly.  The following line gets the
    // key element, instead of the key-square element, fixes this.  It should be
    // removed again when the key-square elements are all removed as planned.
    if(typeof key.firstChild != 'undefined' && key.firstChild != null && util.hasClass(key.firstChild,'kmw-key')) {
      key = <HTMLElement> key.firstChild;
    }

    var w=key.offsetWidth,
        h=key.offsetHeight,
        // Since the full OSKManager '_Box' is displayed within the keyboards' WebViews,
        // these calculations should be performed with respect to that, rather than osk.vkbd.kbdDiv.
        x=dom.Utils.getAbsoluteX(key) - dom.Utils.getAbsoluteX(osk._Box) + w/2,
        y=dom.Utils.getAbsoluteY(key) - dom.Utils.getAbsoluteY(osk._Box);

    return x+','+y+','+w+','+h;
  };

 /**
   *  Accept an external key ID (from KeymanTouch) and pass to the keyboard mapping
   *
   *  @param  {string}  keyName   key identifier which could contain a display layer and a "functional" layer
   *                              e.g: 'shift-K_E+rightalt-shift'
   **/
  keymanweb['executePopupKey'] = function(keyName: string) {
      let osk = keymanweb.osk;
      var origArg = keyName;
      if(!keymanweb.core.activeKeyboard || !osk.vkbd) {
        return false;
      }

      /* Clear any pending (non-popup) key */
      osk.vkbd.keyPending = null;

      // Changes for Build 353 to resolve KMEI popup key issues
      keyName=keyName.replace('popup-',''); //remove popup prefix if present (unlikely)

      // Regex for 'display layer'-'virtual key name'+'optional functional layer'
      // Can't just split on '-' because some layers like ctrl-shift contain it.
      // Virtual key name starts with T_, K_, or U_
      // matches[1]: displayLayer (not used)
      // matches[2]: keyId
      // matches[3]: optional functionalLayer
      let matches = keyName.match(/^(.+)-([TKU]_[^+]+)\+?(.+)?$/);
      if (matches == null) {
        return false;
      }
      keyName = matches[2] + (matches[3] ? '+' + matches[3] : '');

      // Note:  this assumes Lelem is properly attached and has an element interface.
      // Currently true in the Android and iOS apps.
      var Lelem=keymanweb.domManager.lastActiveElement;
      keymanweb.domManager.initActiveElement(Lelem);

      // This should be set if we're within this method... but it's best to guard against nulls here, just in case.
      if(osk.vkbd.subkeyGesture) {
        let gesture = osk.vkbd.subkeyGesture as com.keyman.osk.embedded.SubkeyDelegator;
        gesture.resolve(keyName);
        osk.vkbd.subkeyGesture = null;
      } else {
        console.warn("No base key exists for the subkey being executed: '" + origArg + "'");
      }
  };

  /**
   *  API endpoint for hardware keystroke events from Android external keyboards
   *
   *  @param  {number}  code   key identifier
   *  @param  {number}  shift  shift state (0x01=left ctrl 0x02=right ctrl 0x04=left alt 0x08=right alt
   *                                        0x10=shift 0x20=ctrl 0x40=alt)
   *  @param  {number}  lstates lock state (0x0200=no caps 0x0400=num 0x0800=no num 0x1000=scroll 0x2000=no scroll locks)
   *  @return {boolean} false when KMW _has_ fully handled the event and true when not.
   **/
  keymanweb['executeHardwareKeystroke'] = function(code, shift, lstates = 0): boolean {
    if(!keymanweb.core.activeKeyboard || code == 0) {
      return false;
    }

    // Clear any pending (non-popup) key
    keymanweb.osk.vkbd.keyPending = null;

    // Note:  this assumes Lelem is properly attached and has an element interface.
    // Currently true in the Android and iOS apps.
    var Lelem = keymanweb.domManager.lastActiveElement;

    keymanweb.domManager.initActiveElement(Lelem);

    // Check the virtual key
    var Lkc: com.keyman.text.KeyEvent = {
      Lmodifiers: shift,
      vkCode: code,
      Lcode: code,
      Lstates: lstates,
      LisVirtualKey: true,
      kName: '',
      device: keymanweb.util.physicalDevice.coreSpec, // As we're executing a hardware keystroke.
      isSynthetic: false
    };

    try {
      // Now that we've manually constructed a proper keystroke-sourced KeyEvent, pass control
      // off to the processor for its actual execution.

      // Should return 'false' when KMW _has_ fully handled the event and 'true' when not.
      const ruleBehavior: com.keyman.text.RuleBehavior = keymanweb.core.processKeyEvent(Lkc, com.keyman.dom.Utils.getOutputTarget(Lelem));

      return !ruleBehavior || ruleBehavior.triggerKeyDefault;
    } catch (err) {
      console.error(err.message, err);
      return false;
    }
  };
})();
