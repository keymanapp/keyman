// KeymanWeb 10.0
// Copyright 2017 SIL International

/*****************************************/
/*                                       */
/*   Embedded application-specific code  */
/*                                       */
/*****************************************/

(function() {
  // Declare KeymanWeb and related objects
  var keymanweb=window['tavultesoft']['keymanweb'],osk=keymanweb['osk'],util=keymanweb['util'],device=util.device;

  // Allow definition of application name
  keymanweb.options['app']='';
  
  // Flag to control refreshing of a keyboard that is already loaded
  keymanweb.mustReloadKeyboard = true;
  
  // Skip full page initialization - skips native-mode only code
  keymanweb.isEmbedded = true;

  osk.popupDelay = 400;  // Delay must be less than native touch-hold delay 
  
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
  }
  
  // Get default style sheet path
  keymanweb.getStyleSheetPath = function(ssName) {
    return keymanweb.rootPath+ssName;
  }

  // Get KMEI, KMEA keyboard path (overrides default function)
  keymanweb.getKeyboardPath = function(Lfilename) {
    return keymanweb.rootPath+'languages/' + Lfilename + "?v=" + (new Date()).getTime(); /*cache buster*/  
  }

    /**
   * Force reload of resource
   * 
   *  @param  {string}  s unmodified URL
   *  @return             modified URL         
   */
  util.unCached = function(s) {
    var t=(new Date().getTime());
    s = s + '?v=' + t;
    return s;
  }  
  
  util.wait = function() {
    // Empty stub - this function should not be implemented or used within embedded code routes.
    console.warn("util.wait() call attempted in embedded mode!");  // Sends log message to embedding app.
  }

  util.alert = function() {
    // Empty stub - this function should not be implemented or used within embedded code routes.
    console.warn("util.alert() call attempted in embedded mode!");  // Sends log message to embedding app.
  }
 
  // TODO: This needs to be discussed with Serkan - can possibly get context without any reference to Pelem?? 
  
  /**
   * Get (uncached) keyboard context for a specified range, relative to caret
   * 
   * @param       {number}      n       Number of characters to move back from caret
   * @param       {number}      ln      Number of characters to return
   * @param       {Object}      Pelem   Element to work with (must be currently focused element)
   * @return      {string}              Context string 
   * 
   * Example     [abcdef|ghi] as INPUT, with the caret position marked by |:
   *             KC(2,1,Pelem) == "e"
   *             KC(3,3,Pelem) == "def"
   *             KC(10,10,Pelem) == "abcdef"  i.e. return as much as possible of the requested string
   */    
  keymanweb.KC_ = function(n, ln, Pelem) 
  {
    var Ldv;
    if(Pelem.body) var Ldoc=Pelem; else var Ldoc=Pelem.ownerDocument; // I1481 - use Ldoc to get the ownerDocument when no selection is found
   
    if(keymanweb.legacy)
    {
      return Pelem.value._kmwSubstr(Pelem.length-n, ln); //I3319
    }
    else if(Ldoc  &&  (Ldv=Ldoc.defaultView)  &&  Ldv.getSelection  &&
      (Ldoc.designMode.toLowerCase() == 'on' || Pelem.contentEditable == 'true' || Pelem.contentEditable == 'plaintext-only' || Pelem.contentEditable === '')) //  &&  Pelem.tagName == 'HTML')  &&  Pelem.tagName == 'HTML')
		  // I2457 - support contentEditable elements in mozilla, webkit
    {
      /* Mozilla midas html editor and editable elements */
      var Lsel = Ldv.getSelection();
      if(Lsel.focusNode.nodeType == 3)
      {
        if(Lsel.focusOffset > 2*n)  // I3319 SMP extension
          return Lsel.focusNode.substringData(Lsel.focusOffset - 2*n, 2*n)._kmwSubstr(-n)._kmwSubstr(0,ln); // I3319
        else
          return Lsel.focusNode.substringData(0, Lsel.focusOffset)._kmwSubstr(-n)._kmwSubstr(0,ln);         // I3319
      }
      else
        return "";
    }
    else if (Pelem.setSelectionRange)
    {
      /* Mozilla other controls */
      var LselectionStart, LselectionEnd;
      if(Pelem._KeymanWebSelectionStart) 
      {
        LselectionStart = Pelem._KeymanWebSelectionStart;
        LselectionEnd = Pelem._KeymanWebSelectionEnd;
        //KeymanWeb._Debug('KeymanWeb.KC: _KeymanWebSelectionStart=TRUE LselectionStart='+LselectionStart+'; LselectionEnd='+LselectionEnd);
      }
      else
      {
        if(keymanweb._CachedSelectionStart === null || Pelem.selectionStart !== keymanweb._LastCachedSelection) // I3319, KMW-1
        {
          keymanweb._LastCachedSelection = Pelem.selectionStart; // KMW-1
          keymanweb._CachedSelectionStart = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionStart); // I3319
          keymanweb._CachedSelectionEnd = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionEnd);     // I3319
        }
        LselectionStart = keymanweb._CachedSelectionStart; // I3319
        LselectionEnd = keymanweb._CachedSelectionEnd;     // I3319           
      }
      if(LselectionStart < n)
      {
        // Looking for context before start of text buffer so return non-characters to pad result
        var tempContext = Array(n-LselectionStart+1).join("\uFFFE") + Pelem.value._kmwSubstr(0,LselectionStart);
        return tempContext._kmwSubstr(0,ln); 
      }
//dbg(n+' '+ln+' '+Pelem.value._kmwSubstring(LselectionStart-n,LselectionStart-n+ln));
      return Pelem.value._kmwSubstring(LselectionStart-n,LselectionStart-n+ln); //I3319, KMW-1
    }
    
    else if(Ldoc  &&  (Ldv=Ldoc.selection)) // build 77 - use elem.ownerDocument instead of document
                                            // I1481 - use Ldoc to get the ownerDocument when no selection is found
    {  
      /* IE */
      var Lrange = Ldv.createRange();
      //if (Lrange.parentElement() == Pelem) {  // build 77 - ignore parent of selection
      Lrange.moveStart('character',-2*n);                     //I3319

      return Lrange.text._kmwSubstr(-n)._kmwSubstring(0,ln);  //I3319
      //}
    }

    return "";
  }  

  /**
   * Refresh element content after change of text (if required)
   * 
   *  @param  {Object}  Pelem   input element
   */         
  keymanweb.refreshElementContent = function(Pelem) 
  {
    if('ontextchange' in keymanweb) keymanweb['ontextchange'](Pelem);
  }

  /**
   * Set target element text direction (LTR or RTL): not functional for KMEI, KMEA
   *    
   * @param       {Object}      Ptarg      Target element
   */    
  keymanweb._SetTargDir = function(Ptarg){}
  
/**
   * Align input fields (should not be needed with KMEI, KMEA)
   * 
   *  @param  {boolean}   align    align and make visible, else hide
   * 
   **/
  keymanweb.alignInputs = function(align) {}

  /**
   * Use rotation events to adjust OSK element positions and scaling if necessary
   */     
  keymanweb.handleRotationEvents = function() {}

  /**
   * Adjust the absolute height of each keyboard element after a rotation - modified for KMEI, 13/11/14
   *    
   **/      
  osk.adjustHeights=function()
  {        
    if(!osk._Box || !osk._Box.firstChild || !osk._Box.firstChild.firstChild || !osk._Box.firstChild.firstChild.childNodes)
      return false;
    
    var layers=osk._Box.firstChild.firstChild.childNodes,
        nRows=layers[0].childNodes.length,
        oskHeight=osk.getHeight(),
        rowHeight=Math.floor(oskHeight/nRows),
        nLayer,nRow,rs,keys,nKeys,nKey,key,ks,j,pad=4,fs=1.0;
        
    if(device.OS == 'Android' && 'devicePixelRatio' in window) 
      rowHeight = rowHeight/window.devicePixelRatio;
    
    oskHeight=nRows*rowHeight;

    var b=osk._Box,bs=b.style;
    bs.height=bs.maxHeight=(oskHeight+3)+'px';
    b=b.firstChild.firstChild; bs=b.style;
    bs.height=bs.maxHeight=(oskHeight+3)+'px';
    if(device.formFactor == 'phone') fs = 0.65;
    pad = Math.round(0.15*rowHeight);

    bs.fontSize=fs+'em';  
    var resizeLabels=(device.OS == 'iOS' && device.formFactor == 'phone' && util.landscapeView());
 
    for(nLayer=0;nLayer<layers.length; nLayer++)
    {
      layers[nLayer].style.height=(oskHeight+3)+'px';       
      for(nRow=0; nRow<nRows; nRow++)
      {                                  
        rs=layers[nLayer].childNodes[nRow].style;
        rs.bottom=(nRows-nRow-1)*rowHeight+'px';       
        rs.maxHeight=rs.height=rowHeight+'px';      
        keys=layers[nLayer].childNodes[nRow].childNodes;
        nKeys=keys.length;     
        for(nKey=0;nKey<nKeys;nKey++)
        {                      
          key=keys[nKey];
          // Must set the height of the text DIV, not the label (if any)
          for(j=0;j<key.childNodes.length;j++)
            if(osk.hasClass(key.childNodes[j],'kmw-key')) break;
          ks=key.childNodes[j].style;
          ks.bottom=rs.bottom; 
          ks.height=ks.minHeight=(rowHeight-pad)+'px'; 
                          
          // Rescale keycap labels on iPhone (iOS 7)
          if(resizeLabels && (j > 0)) key.childNodes[0].style.fontSize='6px'; 
        }
      }    
    } 
    
    return true;
  }
  
  /**
   * Caret position always determined from the active (but hidden) element
   * 
   * @return  {boolean}
   **/          
  keymanweb.isPositionSynthesized = function()
  {
    return false;
  }

  /**
   * correctOSKTextSize handles rotation event -- currently rebuilds keyboard and adjusts font sizes
   */             
  keymanweb['correctOSKTextSize']=function() {
    if(osk.adjustHeights()) {
      osk._Load();
    }
  }
   
  // Send the subkey array to iOS, with centre,top of base key position
  /**
   * Create a popup key array natively 
   * 
   * @param {Object}  key   base key element
   */            
  osk.touchHold = function(key)
  {        
    if(key.subKeys && (typeof(window['oskCreatePopup']) == 'function'))
    {                           
      var xBase=util._GetAbsoluteX(key)-util._GetAbsoluteX(osk._DivVKbd)+key.offsetWidth/2,
          yBase=util._GetAbsoluteY(key)-util._GetAbsoluteY(osk._DivVKbd);      
      
      if(device.formFactor == 'phone') osk.prependBaseKey(key);
      osk.popupBaseKey = key; osk.popupPending=true;      
      window['oskCreatePopup'](key.subKeys,xBase,yBase,key.offsetWidth,key.offsetHeight);
    }      
  }

  /**
   * Function called by iOS when a device-implemented keyboard popup is displayed or hidden
   * 
   *  @param  {boolean}  isVisible
   *     
   **/                
  keymanweb['popupVisible'] = function(isVisible)
  {
    osk.popupVisible = isVisible;
  }

  // Popup key highlighting (managed by device, dummy call)
  osk.highlightSubKeys = function(k,x,y){}

  // Create a keytip (dummy call - actual keytip handled by native code)
  osk.createKeyTip = function()
  {
      if(device.formFactor == 'phone') osk.keytip = {key:null,state:false};
  }    
    
  // Send the key details to KMEI or KMEA for showing or hiding the native-code keytip
  osk.showKeyTip = function(key,on) 
  {  
    var tip = osk.keytip, 
        showPreview = window['oskCreateKeyPreview'],
        clearPreview = window['oskClearKeyPreview'];

    if(tip == null || (key == tip.key && on == tip.state)) return;  

    if(on && (typeof showPreview == 'function'))
    {
      var xBase=util._GetAbsoluteX(key)-util._GetAbsoluteX(osk._DivVKbd)+key.offsetWidth/2,
          yBase=util._GetAbsoluteY(key)-util._GetAbsoluteY(osk._DivVKbd), kc;  

      // Find key text element
      for(var i=0; i<key.childNodes.length; i++)
      {
        kc = key.childNodes[i];
        if(osk.hasClass(kc,'kmw-key-text')) break;    
      }
        
      if(key.className.indexOf('kmw-key-default') >= 0 && key.id.indexOf('K_SPACE') < 0)
        showPreview(xBase,yBase,key.offsetWidth,key.offsetHeight,kc.innerHTML);
    }
    else if(!on && (typeof clearPreview == 'function')) 
    {           
      if(osk.touchCount == 0 || key == null) clearPreview();
    }
    tip.key = key; tip.state = on;
  }

  osk.menuEvent = null;
  osk.optionKey = function(e,keyName,keyDown)
  {
    if(keyName.indexOf('K_LOPT') >= 0)
    {
      if(keyDown)
      {
        osk.menuEvent = e;
        if('showKeyboardList' in keymanweb) keymanweb['showKeyboardList']();
      }
      else
      {
        if(osk.menuEvent) osk.highlightKey(osk.menuEvent,false);
        if(typeof(window['menuKeyUp']) == 'function') window['menuKeyUp']();
        osk.menuEvent = null;
      }
    }
    else if(keyName.indexOf('K_ROPT') >= 0)
    {
      if(keyDown)
      {
        osk.highlightKey(e,false);            
        if('hideKeyboard' in keymanweb) keymanweb['hideKeyboard']();
      }
    }
  }

  // For KMEI and KMEA will always assume fonts are already installed
  osk.waitForFonts = function(kfd,ofd){return true;}

  /**
   *  Return position of language menu key to KeymanTouch
   *  
   *  @return  {string}      comma-separated x,y,w,h of language menu key
   *  
   **/            
  keymanweb['touchMenuPos'] = function()
  {
    if(osk.lgKey == null) return '';  
    var key=osk.lgKey;
    // A CSS change of kmd-key-square from position:fixed to position:static was needed
    // for Android 4.3 to display the OSK correctly, but resulted in the position of
    // the menu key not being returned correctly.  The following line gets the 
    // key element, instead of the key-square element, fixes this.  It should be 
    // removed again when the key-square elements are all removed as planned.
    if(typeof key.firstChild != 'undefined' && key.firstChild != null && osk.hasClass(key.firstChild,'kmw-key')) key = key.firstChild;  
    var w=key.offsetWidth, 
        h=key.offsetHeight,
        x=util._GetAbsoluteX(key) - util._GetAbsoluteX(osk._DivVKbd) + w/2,
        y=util._GetAbsoluteY(key) - util._GetAbsoluteY(osk._DivVKbd);
    return x+','+y+','+w+','+h;
  }
  
 /**
   *  Accept an external key ID (from KeymanTouch) and pass to the keyboard mapping
   *  
   *  @param  {string}  keyName   key identifier
   **/            
  keymanweb['executePopupKey'] = function(keyName)
  {              
      if(!keymanweb._ActiveKeyboard) return false;

      /* Clear any pending (non-popup) key */
      osk.keyPending = null;

      // Changes for Build 353 to resolve KMEI popup key issues      
      keyName=keyName.replace('popup-',''); //remove popup prefix if present (unlikely)      
      
      var t=keyName.split('-'),layer=(t.length>1?t[0]:osk.layerId);
      keyName=t[t.length-1];       
      if(layer == 'undefined') layer=osk.layerId;
              
      var Lelem=keymanweb._LastActiveElement,Lkc,keyShiftState=osk.getModifierState(layer);
      
      if(keymanweb._ActiveElement == null) keymanweb._ActiveElement=Lelem;    
      
      // Process modifier key action
      if(osk.selectLayer(keyName,null)) return true;      
      
      // Check the virtual key 
      Lkc = {Ltarg:Lelem,Lmodifiers:0,Lcode:osk.keyCodes[keyName],LisVirtualKey:true}; 

      if(typeof Lkc.Lcode == 'undefined')
        Lkc.Lcode = osk.getVKDictionaryCode(keyName);

      if(!Lkc.Lcode)
      {
        // Key code will be Unicode value for U_xxxx keys
        if(keyName.substr(0,2) == 'U_')
        {                 
          var tUnicode=parseInt(keyName.substr(2),16);
          if(!isNaN(tUnicode)) Lkc.Lcode=tUnicode;  
        }
      }
     
      //if(!Lkc.Lcode) return false;  // Value is now zero if not known (Build 347)
      //Build 353: revert to prior test to try to fix lack of KMEI output, May 1, 2014      
      if(isNaN(Lkc.Lcode) || !Lkc.Lcode) return false;  
            
      Lkc.vkCode=Lkc.Lcode;

      // Ensure that KIK returns true for U_xxxx keys to avoid firing nomatch
      if(keyName.substr(0,2) == 'U_') Lkc.isVirtualKey=false;
      
      // Define modifiers value for sending to keyboard mapping function
      Lkc.Lmodifiers = keyShiftState*0x10; 

      // Pass this key code and state to the keyboard program
      if(!keymanweb._ActiveKeyboard ||  Lkc.Lcode == 0) return false;
      
      // If key is mapped, return true
      if(keymanweb._ActiveKeyboard['gs'](Lelem, Lkc)) return true;

      // Use default mapping only if necessary (last resort) 
      var ch = osk.defaultKeyOutput(keyName,Lkc.Lcode,keyShiftState);
      if(ch) keymanweb.KO(0, Lelem, ch);           
              
      return true;       
  }

  /**
   *  API endpoint for hardware keystroke events from Android external keyboards
   *  
   *  @param  {number}  code   key identifier
   *  @param  {number}  shift  shift state (0x10=shift 0x20=ctrl 0x40=alt)
   **/            
  keymanweb['executeHardwareKeystroke'] = function(code, shift) {              
    if(!keymanweb._ActiveKeyboard || code == 0) {
      return false;
    }
    var cachedTouchable = device.touchable, cachedFormFactor = device.formFactor;
    var result = false; // Signals if we successfully handled the keystroke.
    device.touchable = false;
    device.formFactor = 'desktop'; 
    try {
      result = keymanweb.executeHardwareKeystrokeInternal(code, shift);
    } catch (err) {
      console.error(err.message, err);
    }
    device.touchable = cachedTouchable; 
    device.formFactor = cachedFormFactor;
    return result;
  };
  
  /**
   *  Process the hardware key to the keyboard mapping
   *  
   *  @param  {number}  code   key identifier
   *  @param  {number}  shift  shift state (0x10=shift 0x20=ctrl 0x40=alt)
   **/            
  keymanweb.executeHardwareKeystrokeInternal = function(code, shift) {
    
      // Clear any pending (non-popup) key
      osk.keyPending = null;
              
      var Lelem = keymanweb._LastActiveElement;
      
      if(keymanweb._ActiveElement == null) {
        keymanweb._ActiveElement = Lelem;
      }

      // Check the virtual key 
      var Lkc = {
        Ltarg: keymanweb._ActiveElement,
        Lmodifiers: shift,
        vkCode: code,
        Lcode: code,
        LisVirtualKey: true,
        LisVirtualKeyCode: false
      }; 
      
      // Pass this key code and state to the keyboard program
      // If key is mapped, return true
      if(keymanweb._ActiveKeyboard['gs'](Lelem, Lkc)) {
        return true;
      }
      
      // Use default mapping only if necessary (last resort)
      if (Lkc.Lcode == osk.keyCodes.K_SPACE) {
        keymanweb.KO(0, Lelem, ' ');
        return true;
      }
      else if (Lkc.Lcode == osk.keyCodes.K_ENTER) {
        keymanweb.KO(0, Lelem, '\n');
        return true;
      }
      var ch = osk.defaultKeyOutput('', Lkc.Lcode, shift / 0x10);
      if(ch) {
        keymanweb.KO(0, Lelem, ch);                     
        return true;
      }

      return false;
  };

})();