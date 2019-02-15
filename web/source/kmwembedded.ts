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
    // Send the subkey array to iOS, with centre,top of base key position
  /**
   * Create a popup key array natively 
   * 
   * @param {Object}  key   base key element
   */            
  VisualKeyboard.prototype.touchHold = function(this: VisualKeyboard, key: KeyElement) { 
    let util = com.keyman.singleton.util;       
    if(key['subKeys'] && (typeof(window['oskCreatePopup']) == 'function')) {
      var xBase=dom.Utils.getAbsoluteX(key)-dom.Utils.getAbsoluteX(this.kbdDiv)+key.offsetWidth/2,
          yBase=dom.Utils.getAbsoluteY(key)-dom.Utils.getAbsoluteY(this.kbdDiv);      
      
      if(util.device.formFactor == 'phone') {
        this.prependBaseKey(key);
      }

      this.popupBaseKey = key;
      this.popupPending=true;      
      window['oskCreatePopup'](key['subKeys'], xBase, yBase, key.offsetWidth, key.offsetHeight);
    }
  };

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
      var xBase=dom.Utils.getAbsoluteX(key)-dom.Utils.getAbsoluteX(this.kbdDiv)+key.offsetWidth/2,
          yBase=dom.Utils.getAbsoluteY(key)-dom.Utils.getAbsoluteY(this.kbdDiv), kc;

      // Find key text element
      for(var i=0; i<key.childNodes.length; i++) {
        kc = key.childNodes[i];
        if(util.hasClass(kc,'kmw-key-text')) {
          break;
        }
      }
        
      if(key.className.indexOf('kmw-key-default') >= 0 && key.id.indexOf('K_SPACE') < 0) {
        showPreview(xBase,yBase,key.offsetWidth,key.offsetHeight,kc.innerHTML);
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

  /**
   * Adjust the absolute height of each keyboard element after a rotation - modified for KMEI, 13/11/14
   *    
   **/      
  VisualKeyboard.prototype.adjustHeights=function(this: VisualKeyboard): boolean {
    let keyman = com.keyman.singleton;
    let osk = keyman.osk;
    let _Box = osk._Box;
    let util = keyman.util;
    let device = util.device;

    if(!_Box || !this.kbdDiv || !this.kbdDiv.firstChild || !this.kbdDiv.firstChild.firstChild.childNodes) {
      return false;
    }

    var layers=_Box.firstChild.firstChild.childNodes,
        nRows=layers[0].childNodes.length,
        oskHeight=osk.getHeight(),
        rowHeight=Math.floor(oskHeight/nRows),
        nLayer,nRow,rs,keys,nKeys,nKey,key,ks,j,pad=4,fs=1.0;

    if(device.OS == 'Android' && 'devicePixelRatio' in window) {
      rowHeight = rowHeight/window.devicePixelRatio;
    }

    oskHeight=nRows*rowHeight;

    var b: HTMLElement = osk._Box, bs=b.style;
    bs.height=bs.maxHeight=(oskHeight+3)+'px';
    b = <HTMLElement> b.firstChild.firstChild;
    bs=b.style;
    bs.height=bs.maxHeight=(oskHeight+3)+'px';
    pad = Math.round(0.15*rowHeight);

    var resizeLabels=(device.OS == 'iOS' && device.formFactor == 'phone' && util.landscapeView());
 
    for(nLayer=0;nLayer<layers.length; nLayer++) {
      // Check the heights of each row, in case different layers have different row counts.
      nRows=layers[nLayer].childNodes.length;
      (<HTMLElement> layers[nLayer]).style.height=(oskHeight+3)+'px';

      for(nRow=0; nRow<nRows; nRow++) {
        rs=(<HTMLElement> layers[nLayer].childNodes[nRow]).style;
        rs.bottom=(nRows-nRow-1)*rowHeight+'px';
        rs.maxHeight=rs.height=rowHeight+'px';
        keys=layers[nLayer].childNodes[nRow].childNodes;
        nKeys=keys.length;
        for(nKey=0;nKey<nKeys;nKey++) {
          key=keys[nKey];
          // Must set the height of the text DIV, not the label (if any)
          for(j=0;j<key.childNodes.length;j++) {
            if(util.hasClass(key.childNodes[j],'kmw-key')) {
              break;
            }
          }
          ks=key.childNodes[j].style;
          ks.bottom=rs.bottom;
          ks.height=ks.minHeight=(rowHeight-pad)+'px';
                          
          // Rescale keycap labels on iPhone (iOS 7)
          if(resizeLabels && (j > 0)) key.childNodes[0].style.fontSize='6px';
        }
      }
    }

    return true;
  };
}

(function() {
  // Declare KeymanWeb and related objects
  var keymanweb=window['keyman'], osk: com.keyman.osk.OSKManager = keymanweb['osk'],util=keymanweb['util'],device=util.device;
  var dom = com.keyman.dom;
  var Layouts = com.keyman.osk.Layouts;
  var kbdInterface=keymanweb['interface'];

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
    if(osk.vkbd.adjustHeights()) {
      osk._Load();
    }
  };

  /**
   * Function called by iOS when a device-implemented keyboard popup is displayed or hidden
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
        x=dom.Utils.getAbsoluteX(key) - dom.Utils.getAbsoluteX(osk.vkbd.kbdDiv) + w/2,
        y=dom.Utils.getAbsoluteY(key) - dom.Utils.getAbsoluteY(osk.vkbd.kbdDiv);

    return x+','+y+','+w+','+h;
  };
  
 /**
   *  Accept an external key ID (from KeymanTouch) and pass to the keyboard mapping
   *  
   *  @param  {string}  keyName   key identifier
   **/            
  keymanweb['executePopupKey'] = function(keyName: string) {
      let Processor = (<KeymanBase> keymanweb).textProcessor;

      var origArg = keyName;
      if(!keymanweb.keyboardManager.activeKeyboard || !osk.vkbd) {
        return false;
      }

      /* Clear any pending (non-popup) key */
      osk.vkbd.keyPending = null;

      // Changes for Build 353 to resolve KMEI popup key issues      
      keyName=keyName.replace('popup-',''); //remove popup prefix if present (unlikely)      
      
      var t=keyName.split('-'),layer=(t.length>1?t[0]:osk.vkbd.layerId);
      keyName=t[t.length-1];
      if(layer == 'undefined') layer=osk.vkbd.layerId;
      
      
      // Note:  this assumes Lelem is properly attached and has an element interface.
      // Currently true in the Android and iOS apps.
      var Lelem=keymanweb.domManager.getLastActiveElement(),Lkc,keyShiftState=Processor.getModifierState(layer);
      
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
      
      // Process modifier key action
      if(Processor.selectLayer(keyName)) {
        return true;      
      }

      let Codes = com.keyman.text.Codes;
      let modifierCodes = Codes.modifierCodes;
      
      // Check the virtual key 
      Lkc = {Ltarg:Lelem,Lmodifiers:0,Lstates:0, Lcode: Codes.keyCodes[keyName],LisVirtualKey:true};

      // Set the flags for the state keys.
      Lkc.Lstates |= Processor.stateKeys['K_CAPS']    ? modifierCodes['CAPS'] : modifierCodes['NO_CAPS'];
      Lkc.Lstates |= Processor.stateKeys['K_NUMLOCK'] ? modifierCodes['NUM_LOCK'] : modifierCodes['NO_NUM_LOCK'];
      Lkc.Lstates |= Processor.stateKeys['K_SCROLL']  ? modifierCodes['SCROLL_LOCK'] : modifierCodes['NO_SCROLL_LOCK'];

      // Set LisVirtualKey to false to ensure that nomatch rule does fire for U_xxxx keys
      if(keyName.substr(0,2) == 'U_') Lkc.isVirtualKey=false;

      // Get code for non-physical keys
      if(typeof Lkc.Lcode == 'undefined') {
          Lkc.Lcode = keymanweb.textProcessor.getVKDictionaryCode(keyName);
          if (!Lkc.Lcode) {
              // Special case for U_xxxx keys
              Lkc.Lcode = 1;
          }
      }

      //if(!Lkc.Lcode) return false;  // Value is now zero if not known (Build 347)
      //Build 353: revert to prior test to try to fix lack of KMEI output, May 1, 2014      
      if(isNaN(Lkc.Lcode) || !Lkc.Lcode) { 
        // Addresses modifier SHIFT keys.
        if(nextLayer) {
          Processor.selectLayer(keyName, nextLayer);
        }
        return false;
      }

      // Define modifiers value for sending to keyboard mapping function
      Lkc.Lmodifiers = keyShiftState;
      let modifierBitmasks = Codes.modifierBitmasks;

      // Handles modifier states when the OSK is emulating rightalt through the leftctrl-leftalt layer.
      if((Lkc.Lmodifiers & modifierBitmasks['ALT_GR_SIM']) == modifierBitmasks['ALT_GR_SIM'] && Layouts.emulatesAltGr()) {
          Lkc.Lmodifiers &= ~modifierBitmasks['ALT_GR_SIM'];
          Lkc.Lmodifiers |= modifierCodes['RALT'];
      }

      Lkc.vkCode=Lkc.Lcode;

      // Pass this key code and state to the keyboard program
      if(!keymanweb.keyboardManager.activeKeyboard ||  Lkc.Lcode == 0) return false;
      
      // If key is mapped, return true
      if(kbdInterface.processKeystroke(util.device, Lelem, Lkc)) {
        // Make sure we don't affect the current layer until the keystroke has been processed!
        if(nextLayer) {
          Processor.selectLayer(keyName, nextLayer);
        }

        return true;
      }

      keymanweb.processDefaultMapping(Lkc.Lcode, keyShiftState, Lelem, keyName);

      if(nextLayer) {
        // Final nextLayer check.
        Processor.selectLayer(keyName, nextLayer);
      }

      return true;
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
    if(!keymanweb.keyboardManager.activeKeyboard || code == 0) {
      return false;
    }

    // Clear any pending (non-popup) key
    osk.vkbd.keyPending = null;
    
    // Note:  this assumes Lelem is properly attached and has an element interface.
    // Currently true in the Android and iOS apps.
    var Lelem = keymanweb.domManager.getLastActiveElement();
    
    keymanweb.domManager.initActiveElement(Lelem);

    // Check the virtual key 
    var Lkc: com.keyman.text.KeyEvent = {
      Ltarg: keymanweb.domManager.getActiveElement(),
      Lmodifiers: shift,
      vkCode: code,
      Lcode: code,
      Lstates: lstates,
      LisVirtualKey: true,
      LisVirtualKeyCode: false
    }; 

    let Processor = com.keyman.text.Processor;
    let outputTarget = Processor.getOutputTarget(Lelem);

    try {
      // Pass this key code and state to the keyboard program
      // If key is mapped, return true
      if((kbdInterface as com.keyman.text.KeyboardInterface).processKeystroke(util.physicalDevice, outputTarget, Lkc)) {
        return true;
      }

      return keymanweb.processDefaultMapping(Lkc.Lcode, shift, outputTarget, '');
    } catch (err) {
      console.error(err.message, err);
      return false;
    }
  };

  /**
   * Process default mapping only if necessary (last resort)
   *  @param  {number}  code   key identifier
   *  @param  {number}  shift  shift state (0x01=left ctrl 0x02=right ctrl 0x04=left alt 0x08=right alt
   *                                        0x10=shift 0x20=ctrl 0x40=alt)
   *  @param  {Object}  Lelem   element to output to
   *  @param  {string}  keyName
   *  @return {boolean}         true if key code successfully processed
   */
  keymanweb.processDefaultMapping = function(code, shift, Lelem, keyName) {
    // Note:  this assumes Lelem is properly attached and has an element interface.
    // Currently true in the Android and iOS apps.
    let Codes = com.keyman.text.Codes;

    // Default handling for external keys.
    // Intentionally not assigning K_TAB or K_ENTER so KMW will pass them back
    // to the mobile apps to handle (insert characters or navigate forms).
    if (code == Codes.keyCodes.K_SPACE) {
      kbdInterface.output(0, Lelem, ' ');
      return true;
    } else if (code == Codes.keyCodes.K_BKSP) {
      kbdInterface.defaultBackspace();
      return true;
    } else if (code == Codes.keyCodes.K_oE2) {
      // Using defaults of English US layout for the 102nd key
      if (shift == Codes.modifierCodes['SHIFT']) {
        kbdInterface.output(0, Lelem, '|');
      } else {
        kbdInterface.output(0, Lelem, '\\');
      }
      return true;
    }

    // Determine the character from the OSK
    var ch = keymanweb.textProcessor.defaultKeyOutput(keyName, code, shift, false, undefined);
    if(ch) {
      kbdInterface.output(0, Lelem, ch);
      return true;
    }

    return false;
  }
})();
