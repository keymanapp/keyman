// Since 'web' compilation is the path recognized by VSCode, we need to make references here to prevent TS errors.
// Includes KMW string extension declarations.
/// <reference path="text/kmwstring.ts" />
// References the base Keyman object (and consequently, the rest of the core objects).
/// <reference path="kmwbase.ts" />

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

  // iOS now relies upon native-mode popup key management, so we only implement these hybrid-targetted
  // methods when embedding in Android.
  let device = com.keyman.singleton.util.device;

  if(device.OS == 'Android') { // assumption - if this file is being loaded, keyman.isEmbedded == true.
    // Send the subkey array to iOS, with centre,top of base key position
    /**
     * Create a popup key array natively 
     * 
     * @param {Object}  key   base key element
     */            
    VisualKeyboard.prototype.touchHold = function(this: VisualKeyboard, key: KeyElement) {
      let util = com.keyman.singleton.util;
      if(key['subKeys'] && (typeof(window['oskCreatePopup']) == 'function')) {
        var xBase = dom.Utils.getAbsoluteX(key) - dom.Utils.getAbsoluteX(this.kbdDiv) + key.offsetWidth/2,
            yBase = dom.Utils.getAbsoluteY(key);
        
        if(util.device.formFactor == 'phone') {
          this.prependBaseKey(key);
        }

        this.popupBaseKey = key;
        this.popupPending=true;
        window['oskCreatePopup'](key['subKeys'], xBase, yBase, key.offsetWidth, key.offsetHeight);
      }
    };

    // Send the key details to KMEI or KMEA for showing or hiding the native-code keytip
    VisualKeyboard.prototype.showKeyTip = function(this: VisualKeyboard, key: KeyElement, on: boolean) {
      let util = com.keyman.singleton.util;
      var tip = this.keytip,
          showPreview = window['oskCreateKeyPreview'],
          clearPreview = window['oskClearKeyPreview'];

      if(tip == null || (key == tip.key && on == tip.state)) {
        return;
      }

      if(on && (typeof showPreview == 'function')) {
        let bannerHeight : number = com.keyman.singleton.osk.getBannerHeight();
        var xBase = dom.Utils.getAbsoluteX(key) - dom.Utils.getAbsoluteX(this.kbdDiv) + key.offsetWidth/2,
            yBase = dom.Utils.getAbsoluteY(key) /*- dom.Utils.getAbsoluteY(this.kbdDiv) + bannerHeight*/,
            kc;

        // Find key text element
        for(var i=0; i<key.childNodes.length; i++) {
          kc = key.childNodes[i];
          if(util.hasClass(kc,'kmw-key-text')) {
            break;
          }
        }
          
        if(key.className.indexOf('kmw-key-default') >= 0 && key.id.indexOf('K_SPACE') < 0) {
          showPreview(xBase, yBase, key.offsetWidth, key.offsetHeight, kc.innerHTML);
        }
      } else if(!on && (typeof clearPreview == 'function')) {
        if(this.touchCount == 0 || key == null) {
          clearPreview();
        }
      }

      tip.key = key;
      tip.state = on;
    };

    // Create a keytip (dummy call - actual keytip handled by native code)
    VisualKeyboard.prototype.createKeyTip = function(this: VisualKeyboard) {
      if(com.keyman.singleton.util.device.formFactor == 'phone') {
        this.keytip = {key:null, state:false};
      }
    };

    VisualKeyboard.prototype.highlightSubKeys = function(this: VisualKeyboard, k, x, y) {
      // a dummy function; it's only really used for 'native' KMW.
    }

    VisualKeyboard.prototype.drawPreview = function(this: VisualKeyboard, w, h, edge) {
      // a dummy function; it's only really used for 'native' KMW.
    }

    VisualKeyboard.prototype.addCallout = function(this: VisualKeyboard, key) {
      // a dummy function; it's only really used for 'native' KMW.
      return null;
    }

    VisualKeyboard.prototype.waitForFonts = function(this: VisualKeyboard, kfd, ofd) {
      // a dummy function; it's only really used for 'native' KMW.
      return true;
    }
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
  var keymanweb=window['keyman'], osk: com.keyman.osk.OSKManager = keymanweb['osk'],util=keymanweb['util'],device=util.device;
  var dom = com.keyman.dom;

  // Allow definition of application name
  keymanweb.options['app']='';
  
  // Flag to control refreshing of a keyboard that is already loaded
  keymanweb.mustReloadKeyboard = true;
  
  // Skip full page initialization - skips native-mode only code
  keymanweb.isEmbedded = true;

  com.keyman.osk.VisualKeyboard.prototype.popupDelay = 400;  // Delay must be less than native touch-hold delay 
  
  // Set default device options
  keymanweb.setDefaultDeviceOptions = function(opt) {
    opt['attachType'] = 'manual';
    device.app=opt['app'];
    device.touchable=true; 
    device.formFactor='phone'; 
    if(navigator && navigator.userAgent && navigator.userAgent.indexOf('iPad') >= 0) device.formFactor='tablet';
    if(device.app.indexOf('Mobile') >= 0) device.formFactor='phone';
    if(device.app.indexOf('Tablet') >= 0) device.formFactor='tablet';
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
    if(osk && osk.vkbd && osk.vkbd.adjustHeights()) {
      osk._Load();
    }
  };

  /**
   * Register a lexical model
   * 
   * @param {com.keyman.text.prediction.ModelSpec} model  Spec of the lexical model
   */
  keymanweb['registerModel']=function(model: com.keyman.text.prediction.ModelSpec) {
    keymanweb.modelManager.register(model);
  };

  keymanweb['showNewSuggestions'] = function() {
    let keyman = com.keyman.singleton;

    if(keyman['osk'].banner['activeBanner'] instanceof com.keyman.osk.SuggestionBanner) {
      let banner = keyman['osk'].banner['activeBanner'];
      banner.rotateSuggestions();
    }
  }

  /**
   * Function called by Android and iOS when a device-implemented keyboard popup is displayed or hidden
   * 
   *  @param  {boolean}  isVisible
   *     
   **/
  keymanweb['popupVisible'] = function(isVisible)
  {
    osk.vkbd.popupVisible = isVisible;
  };

  /**
   *  Return position of language menu key to KeymanTouch
   *  
   *  @return  {string}      comma-separated x,y,w,h of language menu key
   *  
   **/
  keymanweb['touchMenuPos'] = function()
  {
    if(osk.vkbd.lgKey == null) {
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
   *  @param  {string}  keyName   key identifier
   **/            
  keymanweb['executePopupKey'] = function(keyName: string) {
      let core = (<KeymanBase> keymanweb).core;

      var origArg = keyName;
      if(!keymanweb.core.activeKeyboard || !osk.vkbd) {
        return false;
      }

      /* Clear any pending (non-popup) key */
      osk.vkbd.keyPending = null;

      // Changes for Build 353 to resolve KMEI popup key issues      
      keyName=keyName.replace('popup-',''); //remove popup prefix if present (unlikely)      
      
      var t=keyName.split('-'),layer=(t.length>1?t[0]:core.keyboardProcessor.layerId);
      keyName=t[t.length-1];
      if(layer == 'undefined') {
        layer=core.keyboardProcessor.layerId;
      }
      
      // Note:  this assumes Lelem is properly attached and has an element interface.
      // Currently true in the Android and iOS apps.
      var Lelem=keymanweb.domManager.getLastActiveElement(),keyShiftState=com.keyman.text.KeyboardProcessor.getModifierState(layer);
      
      keymanweb.domManager.initActiveElement(Lelem);

      var nextLayer: string;

      // This should be set if we're within this method... but it's best to guard against nulls here, just in case.
      if(osk.vkbd.popupBaseKey && osk.vkbd.popupBaseKey['key']) {
        // This is set with the base key of our current subkey elsewhere within the engine.
        var baseKey: com.keyman.osk.OSKKeySpec = osk.vkbd.popupBaseKey['key'].spec;
        var found = false;

        if(baseKey.id == keyName) {
          nextLayer = baseKey.nextlayer;
          found = true;
        } else {
          // Search for the specified subkey so we can retrieve its useful properties.
          // It should be within the popupBaseKey's subkey list.
          for(let subKey of baseKey.sk) {
            if(subKey.id == keyName) {
              // ... to consider:  why are we not just taking the keyspec wholesale right here?
              nextLayer = subKey.nextlayer;
              found = true;
              break;
            }
          }
        }

        if(!found) {
          console.warn("Could not find subkey '" + origArg + "' under the current base key '" + baseKey.id + "'!");
        }
      } else {
        console.warn("No base key exists for the subkey being executed: '" + origArg + "'");
      }

      let Codes = com.keyman.text.Codes;
      
      // Check the virtual key 
      let Lkc: com.keyman.text.KeyEvent = {
        Ltarg: com.keyman.dom.Utils.getOutputTarget(Lelem),
        Lmodifiers: keyShiftState,
        Lstates: 0,
        Lcode: Codes.keyCodes[keyName],
        LisVirtualKey: true,
        kName: keyName,
        kNextLayer: nextLayer,
        vkCode: null, // was originally undefined
        isSynthetic: true,
        device: keymanweb.util.device.coreSpec
      };

      // Process modifier key action
      if(core.keyboardProcessor.selectLayer(Lkc, true)) { // ignores key's 'nextLayer' property for this check
        return true;      
      }

      // While we can't source the base KeyEvent properties for embedded subkeys the same way as native,
      // we can handle many other pre-processing steps the same way with this common method.
      core.keyboardProcessor.setSyntheticEventDefaults(Lkc);

      //if(!Lkc.Lcode) return false;  // Value is now zero if not known (Build 347)
      //Build 353: revert to prior test to try to fix lack of KMEI output, May 1, 2014      
      if(isNaN(Lkc.Lcode) || !Lkc.Lcode) { 
        // Addresses modifier SHIFT keys.
        if(nextLayer) {
          core.keyboardProcessor.selectLayer(Lkc);
        }
        return false;
      }

      Lkc.vkCode=Lkc.Lcode;

      // Now that we have a valid key event, hand it off to the Processor for execution.
      // This allows the Processor to also handle any predictive-text tasks necessary.
      let retVal = com.keyman.osk.PreProcessor.handleClick(Lkc, null);

      // Special case for embedded to pass K_TAB back to device to process
      if(Lkc.Lcode == Codes.keyCodes["K_TAB"] || Lkc.Lcode == Codes.keyCodes["K_TABBACK"] 
          || Lkc.Lcode == Codes.keyCodes["K_TABFWD"]) {
        return false;
      }

      return retVal;
  };

  /**
   *  API endpoint for hardware keystroke events from Android external keyboards
   *  
   *  @param  {number}  code   key identifier
   *  @param  {number}  shift  shift state (0x01=left ctrl 0x02=right ctrl 0x04=left alt 0x08=right alt
   *                                        0x10=shift 0x20=ctrl 0x40=alt)
   *  @param  {number}  lstates lock state (0x0200=no caps 0x0400=num 0x0800=no num 0x1000=scroll 0x2000=no scroll locks)
   **/            
  keymanweb['executeHardwareKeystroke'] = function(code, shift, lstates = 0) {
    let keyman = com.keyman.singleton;
    if(!keyman.core.activeKeyboard || code == 0) {
      return false;
    }

    // Clear any pending (non-popup) key
    osk.vkbd.keyPending = null;
    
    // Note:  this assumes Lelem is properly attached and has an element interface.
    // Currently true in the Android and iOS apps.
    var Lelem = keymanweb.domManager.getLastActiveElement();
    
    keyman.domManager.initActiveElement(Lelem);

    // Check the virtual key 
    var Lkc: com.keyman.text.KeyEvent = {
      Ltarg: com.keyman.dom.Utils.getOutputTarget(keyman.domManager.getActiveElement()),
      Lmodifiers: shift,
      vkCode: code,
      Lcode: code,
      Lstates: lstates,
      LisVirtualKey: true,
      kName: '',
      device: keyman.util.physicalDevice.coreSpec, // As we're executing a hardware keystroke.
      isSynthetic: false
    }; 

    try {
      // Now that we've manually constructed a proper keystroke-sourced KeyEvent, pass control
      // off to the processor for its actual execution.
      return keyman.core.processKeyEvent(Lkc) != null;
    } catch (err) {
      console.error(err.message, err);
      return false;
    }
  };
})();
