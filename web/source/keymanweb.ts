// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwexthtml.ts" />
// Includes KMW string extension declarations.
/// <reference path="kmwstring.ts" />

/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/

// If KMW is already initialized, the KMW script has been loaded more than once. We wish to prevent resetting the 
// KMW system, so we use the fact that 'initialized' is only 1 / true after all scripts are loaded for the initial
// load of KMW.
if(!window['keyman']['initialized']) { 

  /******************************************************************
   *  Main Keyman Web Module    
   *   
   *  Code enclosed as an anonymous function to protect name space                          
   *    
   ******************************************************************/

  // -------------------------------------------------------------------------
  
  (function() 
  {

    // Declare KeymanWeb, OnScreen Keyboard and Util objects
    var keymanweb=window['keyman'],osk=keymanweb['osk'],util=keymanweb['util'], device=util.device;
    var kbdInterface=keymanweb['interface'];

    /**
     * Function     debug
     * Scope        Private
     * @param       {(string|Object)}     s   string (or object) to print
     * Description  Simple debug display (upper right of screen)
     *              Extended to support multiple arguments May 2015   
     */       
    keymanweb['debug']=keymanweb.debug=function(s){
      var p;
      if(keymanweb.debugElement == null)
      {
        var d=document.createElement('DIV'),ds=d.style;
        ds.position='absolute';ds.width='30%';ds.maxHeight='50%';ds.top='0';ds.right='0';
        ds.minHeight='50px'; ds.border='1px solid blue'; ds.whiteSpace='pre-line';ds.overflowY='scroll';
        p=document.createElement('P'); p.id='debug_output';p.style.margin='2px';
        d.appendChild(p);
        document.body.appendChild(d);   
        keymanweb.debugElement=p;  
      } 
      if((p=document.getElementById('debug_output')) == null) return; 

      if(arguments.length == 0)
        if(typeof p.textContent != 'undefined') p.textContent=''; else p.innerHTML='';
      else
      {
        var ts=new Date().toTimeString().substr(3,5),t=ts+' ',t1,k,m,sx;
        for(k=0; k<arguments.length; k++)
        {
          if(k > 0) t = t + '; ';
          sx = arguments[k];
          if(typeof sx == 'object')
          {
            if(sx == null)
            {
              t = t + 'null';
            }
            else
            {
              t1 = '';
              for(m in sx) 
              {
                if(t1.length > 0) t1 = t1 + ', ';
                t1 = t1 + m + ':';              
                switch(typeof sx[m])
                {
                  case 'string':
                  case 'number':
                  case 'boolean':
                    t1 = t1 + sx[m]; break;
                  default:
                    t1 = t1 + typeof sx[m]; break;
                }
                if(t1.length > 1024) 
                {
                  t1 = t1.substr(0,1000)+'...'; break;
                }
              }
              if(t1.length > 0) t = t + '{' + t1 + '}';
            }
          }
          else
          {
            t = t + sx;
          }
        } 
        // Truncate if necessary to avoid memory problems
        if(t.length > 1500) t = t.substr(0,1500) + ' (more)';  
        
        if(typeof p.textContent != 'undefined')
          p.textContent=t+'\n'+p.textContent;
        else
          p.innerHTML=t+'<br />'+p.innerHTML;
        
      }
    }
    keymanweb.debugElement=null;
    var dbg=keymanweb.debug;
        
    /**
     * Function    setUpTouchDevice
     * Scope       Private
     * Description Initialize event handling and duplicate input fields for touch-input devices
     */       
    keymanweb.setupTouchDevice = function() { 
      /**
       * Ideally, OSK behaviour should emulate internal keyboard, but 
       * it is not possible to do that while allowing native scrolling.
       * The compromise adopted is that a touchstart or touchmove event
       * on any part of the page other than an input element or the OSK 
       * itself will temporarily hide the OSK until the touchend or 
       * window.scroll event is fired. Hiding the OSK in this way seems
       * less disturbing than having it move as the page is scrolled.
       * 
       * All of this may be better if we can reliably get the y-offset 
       * from the CSS transform and apply that to repositioning the OSK
       * using a timed event loop.           
       */

      keymanweb.touchAliasing = new TouchHandlers(keymanweb);

    }
    
    /*********************************************************
     *  
     * End of main touch-device initialization.
     *     
     *********************************************************/

    keymanweb.touchAliasing = new AliasStubs();

    /**
     * Get the user-specified (or default) font for the first mapped input or textarea element
     * before applying any keymanweb styles or classes
     * 
     *  @return   {string}
     **/                 
    keymanweb.getBaseFont = function()
    {
      var ipInput = document.getElementsByTagName<'input'>('input'),
          ipTextArea=document.getElementsByTagName<'textarea'>('textarea'),
          n=0,fs,fsDefault='Arial,sans-serif';
      
      if(ipInput.length == 0 && ipTextArea.length == 0) n=0;
      else if(ipInput.length > 0 && ipTextArea.length == 0) n=1;
      else if(ipInput.length == 0 && ipTextArea.length > 0) n=2;
      else {
        var firstInput = ipInput[0];
        var firstTextArea = ipTextArea[0];

        if(firstInput.offsetTop < firstTextArea.offsetTop) {
          n=1;    
        } else if(firstInput.offsetTop > firstTextArea.offsetTop) {
          n=2;
        } else if(firstInput.offsetLeft < firstTextArea.offsetLeft) {
          n=1;    
        } else if(firstInput.offsetLeft > firstTextArea.offsetLeft) {
          n=2;
        }
      }
      
      switch(n)
      {
        case 0:
          fs=fsDefault;
        case 1:     
          fs=util.getStyleValue(ipInput[0],'font-family');
        case 2:       
          fs=util.getStyleValue(ipTextArea[0],'font-family');
      }
      if(typeof(fs) == 'undefined' || fs == 'monospace') fs=fsDefault;
      
      return fs;
    }

    // Probably should relocate this definition somewhere.
    keymanweb.timerID = null;

    /**
     * Function     resetContext
     * Scope        Public
     * Description  Revert OSK to default layer and clear any deadkeys and modifiers
     */
    keymanweb['resetContext'] = keymanweb.resetContext = function() {
      osk.layerId = 'default';

      kbdInterface.clearDeadkeys();
      kbdInterface.resetContextCache();
      keymanweb._ResetVKShift();

      osk._Show();
    };

    /**
     * Exposed function to load keyboards by name. One or more arguments may be used
     * 
     * @param {string|Object} x keyboard name string or keyboard metadata JSON object
     * 
     */  
    keymanweb['addKeyboards'] = function(x) {                       
      if(arguments.length == 0) {
        keymanweb.keyboardManager.keymanCloudRequest('',false);
      } else {
        keymanweb.keyboardManager.addKeyboardArray(arguments);
      }
    }
    
    /**
     *  Add default or all keyboards for a given language
     *  
     *  @param  {string}   arg    Language name (multiple arguments allowed)
     **/           
    keymanweb['addKeyboardsForLanguage'] = function(arg)
    {
      keymanweb.keyboardManager.addLanguageKeyboards(arguments);
    }
    
    /**
     * Call back from cloud for adding keyboard metadata
     * 
     * @param {Object}    x   metadata object
     **/                  
    keymanweb['register'] = function(x) {                     
      keymanweb.keyboardManager.register(x);
    }

    /**
     * Build 362: removeKeyboards() remove keyboard from list of available keyboards
     * 
     * @param {string} x keyboard name string
     * 
     */  
    keymanweb['removeKeyboards'] = function(x) {
      return keymanweb.keyboardManager.removeKeyboards(x);
    }


    
    /**
     * Browser dependent initialization
     */       
    if(document.selection)          // only defined for IE
    {
      var appVer=navigator.appVersion;
    // Legacy support variables
      if(appVer.indexOf('MSIE 6.0') >= 0) keymanweb._IE = 6;
      else if(appVer.indexOf('MSIE 7.0') >= 0) keymanweb._IE = 7;
      else if(appVer.indexOf('MSIE 8.0') >= 0) keymanweb._IE = 8;
      if(keymanweb._IE && document.compatMode=='BackCompat') keymanweb._IE = 6;
    }

    // I732 START - Support for European underlying keyboards #1
    if(typeof(window['KeymanWeb_BaseLayout']) !== 'undefined') 
      osk._BaseLayout = window['KeymanWeb_BaseLayout'];
    else
      osk._BaseLayout = 'us';    
    
    
    keymanweb._BrowserIsSafari = (navigator.userAgent.indexOf('AppleWebKit') >= 0);  // I732 END - Support for European underlying keyboards #1 

    /**
     * Function     _GetEventObject
     * Scope        Private   
     * @param       {Event=}     e     Event object if passed by browser
     * @return      {Event|null}       Event object              
     * Description Gets the event object from the window when using Internet Explorer
     *             and handles getting the event correctly in frames 
     */     
    keymanweb._GetEventObject=function(e)   // I2404 - Attach to controls in IFRAMEs
    {
      if (!e) {
        e = window.event;
        if(!e) {
          var elem = keymanweb._GetLastActiveElement();
          if(elem) {
            elem = elem.ownerDocument;
            if(elem) {
              elem = elem.parentWindow;
            }
            if(!elem) {
              return null;
            }
            e = elem.event;
          }
        }
      }
      return e;    
    }

    /**
     * Function     _push
     * Scope        Private   
     * @param       {Array}     Parray    Array   
     * @param       {*}         Pval      Value to be pushed or appended to array   
     * @return      {Array}               Returns extended array
     * Description  Push (if possible) or append a value to an array 
     */  
    keymanweb._push = function(Parray, Pval)
    {
      if(Parray.push) Parray.push(Pval);
      else Parray=Parray.concat(Pval);
      return Parray;
    }
    
    /**
     * Function     attachToControl
     * Scope        Public
     * @param       {Element}    Pelem       Element to which KMW will be attached
     * Description  Attaches KMW to control (or IFrame) 
     */  
    keymanweb['attachToControl'] = function(Pelem: HTMLElement) {
      keymanweb.domManager.attachToControl(Pelem);
    }

    /**
     * Function     detachFromControl
     * Scope        Public
     * @param       {Element}    Pelem       Element from which KMW will detach
     * Description  Detaches KMW from a control (or IFrame) 
     */  
    keymanweb['detachFromControl'] = function(Pelem: HTMLElement) {
      keymanweb.domManager.detachFromControl(Pelem);
    }
        
    /**
     * Function     _AttachToIframe
     * Scope        Private
     * @param       {Element}      Pelem       IFrame to which KMW will be attached
     * Description  Attaches KeymanWeb to IFrame 
     */  
    keymanweb._AttachToIframe = function(Pelem)
    {      
      try
      {
        var Lelem=Pelem.contentWindow.document;
        /* editable Iframe */
        if(Lelem)
        {
          if(Lelem.parentWindow)
          {
            // Internet Explorer
            if(Lelem.designMode.toLowerCase() == 'on' || Lelem.body.isContentEditable)   // I1295 - fix non-attachment for some forms of IFRAMEs
            {
              // I1480 - Attach to IFRAME instead of document
              util.attachDOMEvent(Pelem,'focus', keymanweb._ControlFocus);
              util.attachDOMEvent(Pelem,'blur', keymanweb._ControlBlur);
              util.attachDOMEvent(Lelem,'keydown', keymanweb._KeyDown);   // I2404 - Update for attaching to elements within IFRAMEs, don't attach to read-only IFRAMEs
              util.attachDOMEvent(Lelem,'keypress', keymanweb._KeyPress);
              util.attachDOMEvent(Lelem,'keyup', keymanweb._KeyUp);
              
              // I1481 - Attach to the selectionchange in the iframe (and do a selchange to get the new selection)
              /* IE: call _SelectionChange when the user changes the selection */
              util.attachDOMEvent(Lelem, 'selectionchange', keymanweb._SelectionChange);
              keymanweb._SelectionChange();
              
            }
            else {
              // Lelem is the IFrame's internal document; set 'er up!
              keymanweb._SetupDocument(Lelem);
            }
          }
          else
          {
            if(Lelem.designMode.toLowerCase() == 'on')
            {
              // Mozilla      // I2404 - Attach to  IFRAMEs child objects, only editable IFRAMEs here
              util.attachDOMEvent(Lelem,'focus', keymanweb._ControlFocus);
              util.attachDOMEvent(Lelem,'blur', keymanweb._ControlBlur);
              util.attachDOMEvent(Lelem,'keydown', keymanweb._KeyDown);
              util.attachDOMEvent(Lelem,'keypress', keymanweb._KeyPress);
              util.attachDOMEvent(Lelem,'keyup', keymanweb._KeyUp);
            }
            else
            {
              // Lelem is the IFrame's internal document; set 'er up!
              keymanweb._SetupDocument(Lelem);	   // I2404 - Manage IE events in IFRAMEs
            }
          }
        }
      }
      catch(err)
      {
        // do not attempt to attach to the iframe as it is from another domain - XSS denied!
      }  
    }

        /**
     * Function     _DetachFromIframe
     * Scope        Private
     * @param       {Element}      Pelem       IFrame to which KMW will be attached
     * Description  Detaches KeymanWeb from an IFrame 
     */  
    keymanweb._DetachFromIframe = function(Pelem)
    {      
      try
      {
        var Lelem=Pelem.contentWindow.document;
        /* editable Iframe */
        if(Lelem)
        {
          if(Lelem.parentWindow)
          {
            // Internet Explorer
            if(Lelem.designMode.toLowerCase() == 'on' || Lelem.body.isContentEditable)   // I1295 - fix non-attachment for some forms of IFRAMEs
            {
              // I1480 - Attach to IFRAME instead of document
              util.detachDOMEvent(Pelem,'focus', keymanweb._ControlFocus);
              util.detachDOMEvent(Pelem,'blur', keymanweb._ControlBlur);
              util.detachDOMEvent(Lelem,'keydown', keymanweb._KeyDown);   // I2404 - Update for attaching to elements within IFRAMEs, don't attach to read-only IFRAMEs
              util.detachDOMEvent(Lelem,'keypress', keymanweb._KeyPress);
              util.detachDOMEvent(Lelem,'keyup', keymanweb._KeyUp);
              
              // I1481 - Attach to the selectionchange in the iframe (and do a selchange to get the new selection)
              /* IE: call _SelectionChange when the user changes the selection */
              util.detachDOMEvent(Lelem, 'selectionchange', keymanweb._SelectionChange);
              keymanweb._SelectionChange();
            }
            else {
              // Lelem is the IFrame's internal document; set 'er up!
              keymanweb._ClearDocument(Lelem);
            }
          }
          else
          {
            if(Lelem.designMode.toLowerCase() == 'on')
            {
              // Mozilla      // I2404 - Attach to  IFRAMEs child objects, only editable IFRAMEs here
              util.detachDOMEvent(Lelem,'focus', keymanweb._ControlFocus);
              util.detachDOMEvent(Lelem,'blur', keymanweb._ControlBlur);
              util.detachDOMEvent(Lelem,'keydown', keymanweb._KeyDown);
              util.detachDOMEvent(Lelem,'keypress', keymanweb._KeyPress);
              util.detachDOMEvent(Lelem,'keyup', keymanweb._KeyUp);
            }
            else
            {
              // Lelem is the IFrame's internal document; set 'er up!
              keymanweb._ClearDocument(Lelem);	   // I2404 - Manage IE events in IFRAMEs
            }
          }
        }
      }
      catch(err)
      {
        // do not attempt to attach to the iframe as it is from another domain - XSS denied!
      }  
    }
      
    /**
     * Function     GetEnabled
     * Scope        Private
     * @return      {boolean}      True if KMW enabled
     * Description Test if KMW enabled 
     */    
    keymanweb.GetEnabled = function()
    {
      return keymanweb._Enabled;
    }
    
    /**
     * Function     SetEnabled
     * Scope        Private
     * @param       {(boolean|number)}     Pvalue   True to enable KMW
     * Description  Enable or disable KMW
     */    
    keymanweb.SetEnabled = function(Pvalue)
    {
      if(Pvalue) Pvalue=1; else Pvalue=0;
      if(keymanweb._Enabled != Pvalue)
      {
        keymanweb._Enabled = Pvalue;
        if((!Pvalue) && keymanweb['HideInterface']) keymanweb['HideInterface'](); //JMD 3/9/10
      }
    }  
    
    /**
     * Function     disableControl
     * Scope        Public
     * @param       {Element}      Pelem       Element to be disabled
     * Description  Disables a KMW control element 
     */    
    keymanweb['disableControl'] = keymanweb.disableControl = function(Pelem) {
      if(!keymanweb.domManager.isAttached(Pelem)) {
        console.warn("KeymanWeb is not attached to element " + Pelem);
      } 

      var cn = Pelem.className;
      if(cn.indexOf('kmw-disabled') < 0) { // if not already explicitly disabled...
        Pelem.className = cn ? cn + ' kmw-disabled' : 'kmw-disabled';
      }

      // The rest is triggered within MutationObserver code.
      // See keymanweb._EnablementMutationObserverCore.
    }

    /**
     * Function     enableControl
     * Scope        Public
     * @param       {Element}      Pelem       Element to be disabled
     * Description  Disables a KMW control element 
     */    
    keymanweb['enableControl'] = keymanweb.enableControl = function(Pelem) {
      if(!keymanweb.domManager.isAttached(Pelem)) {
        console.warn("KeymanWeb is not attached to element " + Pelem);
      } 

      var cn = Pelem.className;
      var tagIndex = cn.indexOf('kmw-disabled');
      if(tagIndex >= 0) { // if already explicitly disabled...
        Pelem.className = cn.replace('kmw-disabled', '').trim();
      }

      // The rest is triggered within MutationObserver code.
      // See keymanweb._EnablementMutationObserverCore.
    }

    /**
     * Function     _DisableControl
     * Scope        Private
     * @param       {Element}      Pelem       Element to be disabled
     * Description  Disable KMW control element 
     */    
    keymanweb._DisableControl = function(Pelem) {
      if(keymanweb.domManager.isAttached(Pelem)) { // Only operate on attached elements!  
        if(keymanweb._LastActiveElement == Pelem || keymanweb._LastActiveElement == Pelem['kmw_ip']) {
          keymanweb._LastActiveElement = null;
          keymanweb.keyboardManager.setActiveKeyboard(keymanweb.globalKeyboard, keymanweb.globalLanguageCode);
          keymanweb['osk']._Hide();
        }
        
        if(device.touchable) {
          keymanweb.domManager.disableTouchElement(Pelem);
          keymanweb.domManager.setupNonKMWTouchElement(Pelem);
        
          // If a touch alias was removed, chances are it's gonna mess up our touch-based layout scheme, so let's update the touch elements.
          window.setTimeout(function() {
            keymanweb.listInputs();

            for(var k = 0; k < keymanweb.sortedInputs.length; k++) {
              if(keymanweb.sortedInputs[k]['kmw_ip']) {
                keymanweb.touchAliasing.updateInput(keymanweb.sortedInputs[k]['kmw_ip']);
              }
            }
          }, 1);
        } else {
          keymanweb.listInputs(); // Fix up our internal input ordering scheme.
        }
        
        keymanweb.domManager.disableInputElement(Pelem);
      }
    }

    /**
     * Function     _EnableControl
     * Scope        Private
     * @param       {Element}    Pelem   Element to be enabled
     * Description  Enable KMW control element 
     */    
    keymanweb._EnableControl = function(Pelem) {
      if(keymanweb.domManager.isAttached(Pelem)) { // Only operate on attached elements!
        if(device.touchable) {
          keymanweb.domManager.enableTouchElement(Pelem);

          // If we just added a new input alias, some languages will mess up our touch-based layout scheme
          // if we don't update the touch elements.
          window.setTimeout(function() {
            keymanweb.listInputs();

            for(var k = 0; k < keymanweb.sortedInputs.length; k++) {
              if(keymanweb.sortedInputs[k]['kmw_ip']) {
                keymanweb.touchAliasing.updateInput(keymanweb.sortedInputs[k]['kmw_ip']);
              }
            }
          }, 1);
        } else {
          keymanweb.domManager.enableInputElement(Pelem);
        }
      }
    }
    
    /**
     * Function     setKeyboardForControl
     * Scope        Public   
     * @param       {Element}    Pelem    Control element 
     * @param       {string|null=}    Pkbd     Keyboard (Clears the set keyboard if set to null.)  
     * @param       {string|null=}     Plc      Language Code
     * Description  Set default keyboard for the control 
     */    
    keymanweb['setKeyboardForControl'] = keymanweb.setKeyboardForControl = function(Pelem, Pkbd, Plc) {
      /* pass null for kbd to specify no default, or '' to specify the default system keyboard. */
      if(Pkbd !== null && Pkbd !== undefined) {
        var index = Pkbd.indexOf("Keyboard_");
        if(index < 0 && Pkbd != '') {
          Pkbd = "Keyboard_" + Pkbd;
        }
      } else {
        Plc = null;
      }

      if(Pelem.tagName.toLowerCase() == "iframe") {
        console.warn("'keymanweb.setKeyboardForControl' cannot set keyboard on iframes.");
        return;
      }

      if(!keymanweb.domManager.isAttached(Pelem)) {
        console.error("KeymanWeb is not attached to element " + Pelem);
        return;
      } else {
        Pelem._kmwAttachment.keyboard = Pkbd;
        Pelem._kmwAttachment.languageCode = Plc;

        // If Pelem is the focused element/active control, we should set the keyboard in place now.
        // 'kmw_ip' is the touch-alias for the original page's control.
        if((keymanweb._LastActiveElement == Pelem || keymanweb._LastActiveElement == Pelem['kmw_ip']) 
            && keymanweb._LastActiveElement) {
            
          if(Pkbd != null && Plc != null) { // Second part necessary for Closure.
            keymanweb.keyboardManager.setActiveKeyboard(Pkbd, Plc);
          } 
        }
      }
    }

    /**
     * Function     getKeyboardForControl
     * Scope        Public   
     * @param       {Element}    Pelem    Control element 
     * @return      {string|null}         The independently-managed keyboard for the control.
     * Description  Returns the keyboard ID of the current independently-managed keyboard for this control.
     *              If it is currently following the global keyboard setting, returns null instead.
     */
    keymanweb['getKeyboardForControl'] = keymanweb.getKeyboardForControl = function(Pelem) {
      if(!keymanweb.domManager.isAttached(Pelem)) {
        console.error("KeymanWeb is not attached to element " + Pelem);
        return null;
      } else {
        return Pelem._kmwAttachment.keyboard;  // Should we have a version for the language code, too?
      }
    }
      
    /**
     * Set focus to last active target element (browser-dependent)
     */    
    keymanweb['focusLastActiveElement'] = keymanweb._FocusLastActiveElement = function()
    {
      if(!keymanweb._LastActiveElement) return;

      keymanweb._JustActivatedKeymanWebUI = 1;
      if(keymanweb._IsMozillaEditableIframe(keymanweb._LastActiveElement,0))
        keymanweb._LastActiveElement.defaultView.focus(); // I3363 (Build 301)
      else if(keymanweb._LastActiveElement.focus) keymanweb._LastActiveElement.focus();
    }
    
    /**
     * Get the last active target element *before* KMW activated (I1297)
     * 
     * @return      {Object}        
     */    
    keymanweb['getLastActiveElement'] = keymanweb._GetLastActiveElement = function()
    {
      return keymanweb._LastActiveElement;
    }

    /**
     *  Set the active input element directly optionally setting focus 
     * 
     *  @param  {Object|string} e         element id or element
     *  @param  {boolean=}      setFocus  optionally set focus  (KMEW-123) 
     **/
    keymanweb['setActiveElement']=keymanweb.setActiveElement=function(e,setFocus)
    {
      if(typeof(e) == 'string') e=document.getElementById(e);
      keymanweb._ActiveElement=keymanweb._LastActiveElement=e; 
      // Allow external focusing KMEW-123
      if(arguments.length > 1 && setFocus)
      {
        if(device.touchable) keymanweb.touchAliasing.setFocus();
        else keymanweb['focusLastActiveElement']();
      }
    }
    
    /**
     * Function     getUIState
     * Scope        Public   
     * @return      {Object.<string,(boolean|number)>}
     * Description  Return object with activation state of UI:
     *                activationPending (bool):   KMW being activated
     *                activated         (bool):   KMW active    
     */    
    keymanweb['getUIState'] = keymanweb.getUIState = function() {
      var p={
        activationPending: keymanweb._IsActivatingKeymanWebUI,
        activated: keymanweb._JustActivatedKeymanWebUI
      };
      p['activationPending'] = p.activationPending;
      p['activated'] = p.activated;
      return p;
    }

    /**
     * Set or clear the IsActivatingKeymanWebUI flag (exposed function)
     * 
     * @param       {(boolean|number)}  state  Activate (true,false)
     */
    keymanweb['activatingUI'] = function(state)
    {
      keymanweb._IsActivatingKeymanWebUI = (state ? 1 : 0);
    }      

  //TODO: add more complete description of what ControlFocus really does

    /**
     * Respond to KeymanWeb-aware input element receiving focus 
     * 
     * @param       {Event}       e       Event object
     * @return      {boolean}             always true  (?) 
     */    
    keymanweb._ControlFocus = function(e) {
      var Ltarg, Ln;
      if(!keymanweb._Enabled) {
        return true;
      }
      e = keymanweb._GetEventObject(e);     // I2404 - Manage IE events in IFRAMEs
      Ltarg = util.eventTarget(e);
      if (Ltarg == null) {
        return true;
      }
    
      // Prevent any action if a protected input field
      if(device.touchable && (Ltarg.className == null || Ltarg.className.indexOf('keymanweb-input') < 0)) {
        return true;
      }

      // Or if not a remappable input field
      var en=Ltarg.nodeName.toLowerCase();
      if(en == 'input') {
        var et=Ltarg.type.toLowerCase();
        if(!(et == 'text' || et == 'search')) {
          return true;
        }
      } else if((device.touchable || !Ltarg.isContentEditable) && en != 'textarea') {
        return true;
      }

      keymanweb._ActiveElement=Ltarg;  // I3363 (Build 301)  

      if (Ltarg.nodeType == 3) { // defeat Safari bug
        Ltarg = Ltarg.parentNode;
      }
        
      var LfocusTarg = Ltarg;

      // Ensure that focussed element is visible above the keyboard
      if(device.touchable && (Ltarg.className == null || Ltarg.className.indexOf('keymanweb-input') < 0)) {
        keymanweb.touchAliasing.scrollBody(Ltarg);
      }
          
      if(Ltarg.tagName=='IFRAME') { //**TODO: check case reference
        keymanweb._AttachToIframe(Ltarg);
        Ltarg=Ltarg.contentWindow.document;
      }
          
      //??keymanweb._Selection = null;

      // We condition on 'priorElement' below as a check to allow KMW to set a default active keyboard.
      var priorElement = keymanweb._LastActiveElement;
      keymanweb._LastActiveElement = Ltarg;

      if(keymanweb._JustActivatedKeymanWebUI) {
        keymanweb._BlurKeyboardSettings();
      } else {
        keymanweb._FocusKeyboardSettings(priorElement ? false : true);
      }

      // Always do the common focus stuff, instantly returning if we're in an editable iframe.
      if(keymanweb._CommonFocusHelper(Ltarg)) {
        return true;
      };

      Ltarg._KeymanWebSelectionStart = Ltarg._KeymanWebSelectionEnd = null; // I3363 (Build 301)

      // Set element directionality (but only if element is empty)
      keymanweb._SetTargDir(Ltarg); 

      //Execute external (UI) code needed on focus if required
      keymanweb.doControlFocused(LfocusTarg,keymanweb._LastActiveElement);
    
      // Force display of OSK for touch input device, or if a CJK keyboard, to ensure visibility of pick list
      if(device.touchable) {
        osk._Enabled=1;
      } else {
        // Conditionally show the OSK when control receives the focus
        if(osk.ready) {
          if(keymanweb.keyboardManager.isCJK()) {
            osk._Enabled=1;
          }
          if(osk._Enabled) {
            osk._Show();
          } else {
            osk._Hide(false);
          }
        }
      }

      return true;
    }  
    
    /**
     * Function             _BlurKeyboardSettings
     * Description          Stores the last active element's keyboard settings.  Should be called
     *                      whenever a KMW-enabled page element loses control.
     */
    keymanweb._BlurKeyboardSettings = function() {
      var keyboardID = keymanweb.keyboardManager.activeKeyboard ? keymanweb.keyboardManager.activeKeyboard['KI'] : '';
      
      if(keymanweb._LastActiveElement && keymanweb._LastActiveElement._kmwAttachment.keyboard != null) {
        keymanweb._LastActiveElement._kmwAttachment.keyboard = keyboardID;
        keymanweb._LastActiveElement._kmwAttachment.languageCode = keymanweb.getActiveLanguage();
      } else {
        keymanweb.globalKeyboard = keyboardID;
        keymanweb.globalLanguageCode = keymanweb.getActiveLanguage();
      }
    }

    /**
     * Function             _FocusKeyboardSettings
     * @param   {boolean}   blockGlobalChange   A flag indicating if the global keyboard setting should be ignored for this call.
     * Description          Restores the newly active element's keyboard settings.  Should be called
     *                      whenever a KMW-enabled page element gains control, but only once the prior
     *                      element's loss of control is guaranteed.
     */ 
    keymanweb._FocusKeyboardSettings = function(blockGlobalChange) {      
      if(keymanweb._LastActiveElement._kmwAttachment.keyboard != null) {      
        keymanweb.keyboardManager.setActiveKeyboard(keymanweb._LastActiveElement._kmwAttachment.keyboard, 
          keymanweb._LastActiveElement._kmwAttachment.languageCode); 
      } else if(!blockGlobalChange) { 
        keymanweb.keyboardManager.setActiveKeyboard(keymanweb.globalKeyboard, keymanweb.globalLanguageCode);
      }
    }

    /**
     * Function             _CommonFocusHelper
     * @param   {Element}   target 
     * @returns {boolean}
     * Description          Performs common state management for the various focus events of KeymanWeb.                      
     *                      The return value indicates whether (true) or not (false) the calling event handler 
     *                      should be terminated immediately after the call.
     */
    keymanweb._CommonFocusHelper = function(target) {
      //TODO: the logic of the following line doesn't look right!!  Both variables are true, but that doesn't make sense!
      //_Debug(keymanweb._IsIEEditableIframe(Ltarg,1) + '...' +keymanweb._IsMozillaEditableIframe(Ltarg,1));
      if(!keymanweb._IsIEEditableIframe(target,1) || !keymanweb._IsMozillaEditableIframe(target,1)) {
        keymanweb._DisableInput = 1; 
        return true;
      }
      keymanweb._DisableInput = 0;
  
      if(!keymanweb._JustActivatedKeymanWebUI) {
        keymanweb._DeadKeys = [];
        keymanweb._NotifyKeyboard(0,target,1);  // I2187
      }
    
      if(!keymanweb._JustActivatedKeymanWebUI  &&  keymanweb._SelectionControl != target) {
        keymanweb._IsActivatingKeymanWebUI = 0;
      }
      keymanweb._JustActivatedKeymanWebUI = 0;
  
      keymanweb._SelectionControl = target;
      return false;
    }
    
    /**
     * Function     _IsIEEditableIframe
     * Scope        Private
     * @param       {Object}          Pelem         Iframe element
     *              {boolean|number}  PtestOn       1 to test if frame content is editable (TODO: unclear exactly what this is doing!)   
     * @return      {boolean}
     * Description  Test if element is an IE editable IFrame 
     */    
    keymanweb._IsIEEditableIframe = function(Pelem,PtestOn)
    {
      var Ldv, Lvalid = Pelem  &&  (Ldv=Pelem.tagName)  &&  Ldv.toLowerCase() == 'body'  &&  (Ldv=Pelem.ownerDocument)  &&  Ldv.parentWindow;
      return (!PtestOn  &&  Lvalid) || (PtestOn  &&  (!Lvalid || Pelem.isContentEditable));
    }

    /**
     * Function     _IsMozillaEditableIframe
     * Scope        Private
     * @param       {Object}           Pelem    Iframe element
     * @param       {boolean|number}   PtestOn  1 to test if 'designMode' is 'ON'    
     * @return      {boolean} 
     * Description  Test if element is a Mozilla editable IFrame 
     */    
    keymanweb._IsMozillaEditableIframe = function(Pelem,PtestOn)
    {
      var Ldv, Lvalid = Pelem  &&  (Ldv=Pelem.defaultView)  &&  Ldv.frameElement;
      return (!PtestOn  &&  Lvalid) || (PtestOn  &&  (!Lvalid || Ldv.document.designMode.toLowerCase()=='on'));
    }
      
    /**
     * Respond to KMW losing focus on event
     * 
     * @param       {Event}       e       Event object
     * @return      {boolean}             Always true  (?) 
     */    
    keymanweb._ControlBlur = function(e) {
      var Ltarg;  

      if(!keymanweb._Enabled) {
        return true;
      }

      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      Ltarg = util.eventTarget(e);
      if (Ltarg == null) {
        return true;
      }

      keymanweb._ActiveElement = null; // I3363 (Build 301)

      // Hide the touch device input caret, if applicable  I3363 (Build 301)
      if(device.touchable) {
        keymanweb.touchAliasing.hideCaret();
      }
          
      if (Ltarg.nodeType == 3) { // defeat Safari bug
        Ltarg = Ltarg.parentNode;
      }

      if(Ltarg.tagName=='IFRAME') {
        Ltarg=Ltarg.contentWindow.document;
      }
        
      if (Ltarg.setSelectionRange) {                                           
        //Ltarg._KeymanWebSelectionStart = Ltarg.selectionStart;
        //Ltarg._KeymanWebSelectionEnd = Ltarg.selectionEnd;
        Ltarg._KeymanWebSelectionStart = Ltarg.value._kmwCodeUnitToCodePoint(Ltarg.selectionStart);  //I3319
        Ltarg._KeymanWebSelectionEnd = Ltarg.value._kmwCodeUnitToCodePoint(Ltarg.selectionEnd);  //I3319
        
      }
      
      ////keymanweb._SelectionControl = null;    
      keymanweb._BlurKeyboardSettings();

      // Now that we've handled all prior-element maintenance, update the 'last active element'.
      keymanweb._LastActiveElement = Ltarg;

      /* If the KeymanWeb UI is active as a user changes controls, all UI-based effects should be restrained to this control in case
      * the user is manually specifying languages on a per-control basis.
      */
      keymanweb._JustActivatedKeymanWebUI = 0;
      
      if(!keymanweb._IsActivatingKeymanWebUI) {
        keymanweb._NotifyKeyboard(0,Ltarg,0);  // I2187
      }

      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs  //TODO: is this really needed again????
      keymanweb.doControlBlurred(Ltarg,e,keymanweb._IsActivatingKeymanWebUI);

      // Hide the OSK when the control is blurred, unless the UI is being temporarily selected
      if(osk.ready && !keymanweb._IsActivatingKeymanWebUI) {
        osk._Hide(false);
      }

      return true;
    }

    /****************************************************************
     *  
     * Provide for external processing on events
     *    
     ***************************************************************/     
    
    /**
     * Function     doControlBlurred
     * Scope        Private
     * @param       {Object}            _target       element losing focus
     * @param       {Event}             _event        event object
     * @param       {(boolean|number)}  _isActivating activation state
     * @return      {boolean}      
     * Description  Execute external (UI) code needed on blur
     */       
    keymanweb.doControlBlurred = function(_target,_event,_isActivating)
    {
      var p={}; p['target']=_target; p['event']=_event; p['isActivating']=_isActivating;
      return util.callEvent('kmw.controlblurred',p);
    }

    /**
     * Function     doControlFocused
     * Scope        Private
     * @param       {Object}            _target         element gaining focus
     * @param       {Object}            _activeControl  currently active control
     * @return      {boolean}   
     * Description  Execute external (UI) code needed on focus
     */       
    keymanweb.doControlFocused = function(_target,_activeControl)
    {
      var p={}; p['target']=_target; p['activeControl']=_activeControl;  
      return util.callEvent('kmw.controlfocused',p);
    }

    /**
     * Function     doUnloadOSK
     * Scope        Private
     * @return      {boolean}   
     * Description  Execute external (UI) code if any needed after unloading OSK (probably not required)
     */       
    keymanweb.doUnloadOSK = function()
    {
      var p={};
      return util.callEvent('kmw.unloadosk',p);
    }

    /**
     * Function     doLoadUI
     * Scope        Private
     * @return      {boolean}   
     * Description  Execute UI initialization code after loading the UI
     */       
    keymanweb.doLoadUI = function()
    {
      var p={};
      return util.callEvent('kmw.loaduserinterface',p);
    }

    /**
     * Function     doUnloadUI
     * Scope        Private
     * @return      {boolean}   
     * Description  Execute UI cleanup code before unloading the UI (may not be required?)
     */       
    keymanweb.doUnloadUI = function()
    {
      var p={};
      return util.callEvent('kmw.unloaduserinterface',p);
    }

    /*****************************************************************************
     *  
     * Provide for handling the initial focus event differently
     * The first focus event can happen before we get the WindowLoad, 
     * e.g. if the page activates a control on WindowLoad itself,
     * so trap that and run it through to the page 
     * 
     *****************************************************************************/

    /**
     * Function     _BubbledFocus
     * Scope        Private
     * @param       {Event}       e         Event object
     * Description  Respond to KMW receiving bubbled focus on event (TODO: may not be needed, not currently doing anything) 
     */    
    keymanweb._BubbledFocus = function(e) { /*KeymanWeb._WindowLoad(e);*/ }
      
    if (window.addEventListener)
      window.addEventListener('focus', keymanweb._BubbledFocus, true);  

    /**
     * Function     _GetEventKeyCode
     * Scope        Private
     * @param       {Event}       e         Event object
     * Description  Finds the key code represented by the event.
     */
    keymanweb._GetEventKeyCode = function(e) {
      if (e.keyCode) {
        return e.keyCode;
      } else if (e.which) {
        return e.which;
      } else {
        return null;
      }
    }

    /**
     * Function     _GetKeyEventProperties
     * Scope        Private
     * @param       {Event}       e         Event object
     * @param       {boolean=}    keyState  true if call results from a keyDown event, false if keyUp, undefined if keyPress
     * @return      {Object.<string,*>}     KMW keyboard event object: 
     * Description  Get object with target element, key code, shift state, virtual key state 
     *                Ltarg=target element
     *                Lcode=keyCode
     *                Lmodifiers=shiftState
     *                LisVirtualKeyCode e.g. ctrl/alt key
     *                LisVirtualKey     e.g. Virtual key or non-keypress event
     */    
    keymanweb._GetKeyEventProperties = function(e, keyState) {
      class KeyEvent {
        Ltarg: any;
        Lcode: any;
        Lstates: any;
        LmodifierChange: any;
        Lmodifiers: any;
        LisVirtualKeyCode: any;
        LisVirtualKey: any;
      };

      var s = new KeyEvent();

      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      s.Ltarg = util.eventTarget(e);
      if (s.Ltarg == null) {
        return null;
      }
      if(e.cancelBubble === true) {
        return null; // I2457 - Facebook meta-event generation mess -- two events generated for a keydown in Facebook contentEditable divs
      }      

      if (s.Ltarg.nodeType == 3) {// defeat Safari bug
        s.Ltarg = s.Ltarg.parentNode;
      }

      s.Lcode = keymanweb._GetEventKeyCode(e);
      if (s.Lcode == null) {
        return null;
      }
      
      // Stage 1 - track the true state of the keyboard's modifiers.
      var osk = keymanweb['osk'], prevModState = keymanweb.modStateFlags, curModState = 0x0000;
      var ctrlEvent = false, altEvent = false;
      
      switch(s.Lcode) {
        case osk.keyCodes['K_CTRL']:      // The 3 shorter "K_*CTRL" entries exist in some legacy keyboards.
        case osk.keyCodes['K_LCTRL']:
        case osk.keyCodes['K_RCTRL']:
        case osk.keyCodes['K_CONTROL']:
        case osk.keyCodes['K_LCONTROL']:
        case osk.keyCodes['K_RCONTROL']:
          ctrlEvent = true;
          break;
        case osk.keyCodes['K_LMENU']:     // The 2 "K_*MENU" entries exist in some legacy keyboards.
        case osk.keyCodes['K_RMENU']:
        case osk.keyCodes['K_ALT']:
        case osk.keyCodes['K_LALT']:
        case osk.keyCodes['K_RALT']:
          altEvent = true;
          break;
      }

      /**
       * Two separate conditions exist that should trigger chiral modifier detection.  Examples below use CTRL but also work for ALT.
       * 
       * 1.  The user literally just pressed CTRL, so the event has a valid `location` property we can utilize.  
       *     Problem: its layer isn't presently activated within the OSK.
       * 
       * 2.  CTRL has been held a while, so the OSK layer is valid, but the key event doesn't tell us the chirality of the active CTRL press.
       *     Bonus issue:  RAlt simulation may cause erasure of this location property, but it should ONLY be empty if pressed in this case.
       *     We default to the 'left' variants since they're more likely to exist and cause less issues with RAlt simulation handling.
       * 
       * In either case, `e.ctrlKey` is set to true, but as a result does nothing to tell us which case is active.
       * 
       * `e.location != 0` if true matches condition 1 and matches condition 2 if false.
       */

      curModState |= (e.shiftKey ? 0x10 : 0);      

      if(e.ctrlKey) {
        curModState |= ((e.location != 0 && ctrlEvent) ? 
          (e.location == 1 ? osk.modifierCodes['LCTRL'] : osk.modifierCodes['RCTRL']) : // Condition 1
          prevModState & 0x0003);                                                       // Condition 2
      }
      if(e.altKey) {
        curModState |= ((e.location != 0 && altEvent) ? 
          (e.location == 1 ? osk.modifierCodes['LALT'] : osk.modifierCodes['RALT']) :   // Condition 1
          prevModState & 0x000C);                                                       // Condition 2
      }

      // Stage 2 - detect state key information.  It can be looked up per keypress with no issue.
      s.Lstates = 0;
      
      if(e.getModifierState("CapsLock")) {
        s.Lstates = osk.modifierCodes['CAPS'];
      } else {
        s.Lstates = osk.modifierCodes['NO_CAPS'];
      }

      if(e.getModifierState("NumLock")) {
        s.Lstates |= osk.modifierCodes['NUM_LOCK'];
      } else {
        s.Lstates |= osk.modifierCodes['NO_NUM_LOCK'];
      }

      if(e.getModifierState("ScrollLock") || e.getModifierState("Scroll")) {  // "Scroll" for IE9.
        s.Lstates |= osk.modifierCodes['SCROLL_LOCK'];
      } else {
        s.Lstates |= osk.modifierCodes['NO_SCROLL_LOCK'];
      }

      // We need these states to be tracked as well for proper OSK updates.
      curModState |= s.Lstates;

      // Stage 3 - Set our modifier state tracking variable and perform basic AltGr-related management.
      s.LmodifierChange = keymanweb.modStateFlags != curModState;
      keymanweb.modStateFlags = curModState;

      // For European keyboards, not all browsers properly send both key-up events for the AltGr combo.
      var altGrMask = osk.modifierCodes['RALT'] | osk.modifierCodes['LCTRL'];
      if((prevModState & altGrMask) == altGrMask && (curModState & altGrMask) != altGrMask) {
        // We just released AltGr - make sure it's all released.
        curModState &= ~ altGrMask;
      }
      // Perform basic filtering for Windows-based ALT_GR emulation on European keyboards.
      if(curModState & osk.modifierCodes['RALT']) {
        curModState &= ~osk.modifierCodes['LCTRL'];
      }

      // Stage 4 - map the modifier set to the appropriate keystroke's modifiers.
      if(keymanweb.isChiral()) {
        s.Lmodifiers = curModState & osk.modifierBitmasks.CHIRAL;

        // Note for future - embedding a kill switch here or in keymanweb.osk.emulatesAltGr would facilitate disabling
        // AltGr / Right-alt simulation.
        if(osk.emulatesAltGr() && (s.Lmodifiers & osk.modifierBitmasks['ALT_GR_SIM']) == osk.modifierBitmasks['ALT_GR_SIM']) {
          s.Lmodifiers ^= osk.modifierBitmasks['ALT_GR_SIM'];
          s.Lmodifiers |= osk.modifierCodes['RALT'];
        }
      } else {
        // No need to sim AltGr here; we don't need chiral ALTs.
        s.Lmodifiers = 
          (e.shiftKey ? 0x10 : 0) |
          ((curModState & (osk.modifierCodes['LCTRL'] | osk.modifierCodes['RCTRL'])) ? 0x20 : 0) | 
          ((curModState & (osk.modifierCodes['LALT'] | osk.modifierCodes['RALT']))   ? 0x40 : 0); 
      }

      // The 0x6F used to be 0x60 - this adjustment now includes the chiral alt and ctrl modifiers in that check.
      s.LisVirtualKeyCode = (typeof e.charCode != 'undefined' && e.charCode != null  &&  (e.charCode == 0 || (s.Lmodifiers & 0x6F) != 0));
      s.LisVirtualKey = s.LisVirtualKeyCode || e.type != 'keypress';
      
      return s;
    }

    /**
     * Function   _SelectionChange
     * Scope      Private
     * @return    {boolean} 
     * Description Respond to selection change event 
     */
    keymanweb._SelectionChange = function()
    {
      if(keymanweb._IgnoreNextSelChange)
      {
        keymanweb._IgnoreNextSelChange--;
      }
      else
      {
        var Ls=document.selection;
        if(Ls.type.toLowerCase()!='control') //  &&  document.selection.createRange().parentElement() == keymanweb._SelectionControl) //  &&  window.event.srcElement == keymanweb._SelectionControl)
        {
          var Lrange=Ls.createRange();
          if(!keymanweb._Selection || !keymanweb._Selection.isEqual(Lrange))
          {
            keymanweb._Selection = Lrange;

            /* Delete deadkeys for IE when certain keys pressed */
            kbdInterface.clearDeadkeys();
          }
        }
      }
      return true;
    }
    
    /**
     * Function     _NotifyKeyboard
     * Scope        Private
     * @param       {number}    _PCommand     event code (16,17,18) or 0
     * @param       {Object}    _PTarget      target element
     * @param       {number}    _PData        1 or 0    
     * Description  Notifies keyboard of keystroke or other event
     */    
    keymanweb._NotifyKeyboard = function(_PCommand,_PTarget,_PData) { // I2187
      var activeKeyboard = keymanweb.keyboardManager.activeKeyboard;

      if(activeKeyboard != null && typeof(activeKeyboard['KNS']) == 'function') {
        activeKeyboard['KNS'](_PCommand,_PTarget,_PData);
      }
    }
    
    
    /**
     * Function     _KeyDown
     * Scope        Private
     * @param       {Event}       e     event
     * @return      {boolean}           
     * Description  Processes keydown event and passes data to keyboard. 
     */ 
    keymanweb._KeyDown = function(e) {
      var Ldv,eClass='';
      var activeKeyboard = keymanweb.keyboardManager.activeKeyboard;

      keymanweb._KeyPressToSwallow = 0;
      if(!keymanweb._Enabled || keymanweb._DisableInput || activeKeyboard == null) {
        return true;
      }

      // Prevent mapping element is readonly or tagged as kmw-disabled
      var el=util.eventTarget(e);
      if(device.touchable) {
        if(el && typeof el.kmwInput != 'undefined' && el.kmwInput == false) {
          return true;
        }
      } else if(el && el.className.indexOf('kmw-disabled') >= 0) {
        return true; 
      }
      
      // Or if OSK not yet ready (for any reason)
      if(!osk.ready) {
        return true;
      }
      
      // Get event properties  
      var Levent = keymanweb._GetKeyEventProperties(e, true);
      if(Levent == null) {
        return true;
      }

      switch(Levent.Lcode) {
        case 8: 
          kbdInterface.clearDeadkeys();
          break; // I3318 (always clear deadkeys after backspace) 
        case 16: //"K_SHIFT":16,"K_CONTROL":17,"K_ALT":18
        case 17: 
        case 18: 
        case 20: //"K_CAPS":20, "K_NUMLOCK":144,"K_SCROLL":145
        case 144:
        case 145:
          // For eventual integration - we bypass an OSK update for physical keystrokes when in touch mode.
          keymanweb._NotifyKeyboard(Levent.Lcode,Levent.Ltarg,1); 
          if(!device.touchable) {
            return osk._UpdateVKShift(Levent, Levent.Lcode-15, 1); // I2187
          } else {
            return true;
          }
      }

      if(Levent.LmodifierChange) {
        keymanweb._NotifyKeyboard(0,Levent.Ltarg,1); 
        osk._UpdateVKShift(Levent, 0, 1);
      }
      
      // I1207
      if((Ldv=Levent.Ltarg.ownerDocument)  &&  (Ldv=Ldv.selection)  &&  (Levent.Lcode<33 || Levent.Lcode>40)) {
        Ldv.createRange().select();
      }

      if(!window.event) {
        // I1466 - Convert the - keycode on mnemonic as well as positional layouts
        // FireFox, Mozilla Suite
        if(keymanweb._VKMap_FF_IE['k'+Levent.Lcode]) {
          Levent.Lcode=keymanweb._VKMap_FF_IE['k'+Levent.Lcode];
        }
      }
      //else 
      //{
      // Safari, IE, Opera?
      //}
      
      if(!activeKeyboard['KM']) {
        // Positional Layout

        var LeventMatched=0;
        /* 13/03/2007 MCD: Swedish: Start mapping of keystroke to US keyboard */
        var Lbase=keymanweb._VKMap[osk._BaseLayout];
        if(Lbase && Lbase['k'+Levent.Lcode]) {
          Levent.Lcode=Lbase['k'+Levent.Lcode];
        }
        /* 13/03/2007 MCD: Swedish: End mapping of keystroke to US keyboard */
        
        if(typeof(activeKeyboard['KM'])=='undefined'  &&  !(Levent.Lmodifiers & 0x60)) {
          // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
          var Levent2={Lcode:keymanweb._USKeyCodeToCharCode(Levent),Ltarg:Levent.Ltarg,Lmodifiers:0,LisVirtualKey:0};
          if(kbdInterface.processKeystroke(util.physicalDevice, Levent2.Ltarg,Levent2)) {
            LeventMatched=1;
          }
        }
        
        LeventMatched = LeventMatched || kbdInterface.processKeystroke(util.physicalDevice,Levent.Ltarg,Levent);
        
        // Support backspace in simulated input DIV from physical keyboard where not matched in rule  I3363 (Build 301)
        if(Levent.Lcode == 8 && !LeventMatched && Levent.Ltarg.className != null && Levent.Ltarg.className.indexOf('keymanweb-input') >= 0) {
          kbdInterface.output(1,keymanweb._LastActiveElement,"");
        }
      } else {
        // Mnemonic layout
        if(Levent.Lcode == 8) { // I1595 - Backspace for mnemonic
          keymanweb._KeyPressToSwallow = 1;
          if(!kbdInterface.processKeystroke(util.physicalDevice,Levent.Ltarg,Levent)) {
            kbdInterface.output(1,keymanweb._LastActiveElement,""); // I3363 (Build 301)
          }
          return false;  //added 16/3/13 to fix double backspace on mnemonic layouts on desktop
        }
        else {
          keymanweb._KeyPressToSwallow = 0;
        }
      }

      if(!LeventMatched  &&  Levent.Lcode >= 96  &&  Levent.Lcode <= 111) {
        // Number pad, numlock on
  //      _Debug('KeyPress NumPad code='+Levent.Lcode+'; Ltarg='+Levent.Ltarg.tagName+'; LisVirtualKey='+Levent.LisVirtualKey+'; _KeyPressToSwallow='+keymanweb._KeyPressToSwallow+'; keyCode='+(e?e.keyCode:'nothing'));

        if(Levent.Lcode < 106) {
          var Lch = Levent.Lcode-48;
        } else {
          Lch = Levent.Lcode-64;
        }
        kbdInterface.output(0, Levent.Ltarg, String._kmwFromCharCode(Lch)); //I3319

        LeventMatched = 1;
      }
    
      if(LeventMatched) {
        if(e  &&  e.preventDefault) {
          e.preventDefault();
          e.stopPropagation();
        }
        keymanweb._KeyPressToSwallow = (e?keymanweb._GetEventKeyCode(e):0);
        return false;
      } else {
        keymanweb._KeyPressToSwallow = 0;
      }
      
      if(Levent.Lcode == 8) {
        /* Backspace - delete deadkeys, also special rule if desired? */
        // This is needed to prevent jumping to previous page, but why???  // I3363 (Build 301)
        if(Levent.Ltarg.className != null && Levent.Ltarg.className.indexOf('keymanweb-input') >= 0) {
          return false;
        }
      }

      if(typeof(Levent.Ltarg.base) != 'undefined') {
        // Simulated touch elements have no default text-processing - we need to rely on a strategy similar to
        // that of the OSK here.
        var ch = osk.defaultKeyOutput('',Levent.Lcode,Levent.Lmodifiers,false,Levent.Ltarg);
        if(ch) {
          kbdInterface.output(0, Levent.Ltarg, ch);
          return false;
        }
      }
      return true;
    }                

    /**
     * Function     _KeyPress
     * Scope        Private
     * @param       {Event}       e     event
     * @return      {boolean}           
     * Description Processes keypress event (does not pass data to keyboard)
     */       
    keymanweb._KeyPress = function(e) {
      if(e._kmw_block) { // A custom event property added for bugfix-oriented simulated keypress events.
        return false;
      }

      var Levent;
      if(!keymanweb._Enabled || keymanweb._DisableInput || keymanweb.keyboardManager.activeKeyboard == null) {
        return true;
      }

      Levent = keymanweb._GetKeyEventProperties(e);
      if(Levent == null || Levent.LisVirtualKey) {
        return true;
      }

  //    _Debug('KeyPress code='+Levent.Lcode+'; Ltarg='+Levent.Ltarg.tagName+'; LisVirtualKey='+Levent.LisVirtualKey+'; _KeyPressToSwallow='+keymanweb._KeyPressToSwallow+'; keyCode='+(e?e.keyCode:'nothing'));

      /* I732 START - 13/03/2007 MCD: Swedish: Start positional keyboard layout code: prevent keystroke */
      if(!keymanweb.keyboardManager.activeKeyboard['KM']) {
        if(!keymanweb._KeyPressToSwallow) {
          return true;
        }
        if(Levent.Lcode < 0x20 || (keymanweb._BrowserIsSafari  &&  (Levent.Lcode > 0xF700  &&  Levent.Lcode < 0xF900))) {
          return true;
        }
        e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
        if(e) {
          e.returnValue = false;
        }
        return false;
      }
      /* I732 END - 13/03/2007 MCD: Swedish: End positional keyboard layout code */
      
      if(keymanweb._KeyPressToSwallow || kbdInterface.processKeystroke(util.physicalDevice,Levent.Ltarg,Levent)) {
        keymanweb._KeyPressToSwallow=0;
        if(e  &&  e.preventDefault) {
          e.preventDefault();
          e.stopPropagation();
        }
        return false;
      }
      keymanweb._KeyPressToSwallow=0;
      return true;
    }

    /**
     * Function     _KeyUp
     * Scope        Private
     * @param       {Event}       e     event
     * @return      {boolean}           
     * Description Processes keyup event and passes event data to keyboard
     */       
    keymanweb._KeyUp = function(e) {
      var Levent = keymanweb._GetKeyEventProperties(e, false);
      if(Levent == null || !osk.ready) {
        return true;
      }

      switch(Levent.Lcode) {
        case 13:  
          if(Levent.Ltarg.nodeName == 'TEXTAREA') {
            break;
          }
        
          if(Levent.Ltarg.base && Levent.Ltarg.base.nodeName == 'TEXTAREA') {
            break;
          }

          // For input fields, move to next input element
          if(Levent.Ltarg.type == 'search' || Levent.Ltarg.type == 'submit') {
            Levent.Ltarg.form.submit();
          } else {
            keymanweb.moveToNext(false);
          }
          return true;        
                  
        case 16: //"K_SHIFT":16,"K_CONTROL":17,"K_ALT":18
        case 17: 
        case 18: 
        case 20: //"K_CAPS":20, "K_NUMLOCK":144,"K_SCROLL":145
        case 144:
        case 145:
          keymanweb._NotifyKeyboard(Levent.Lcode,Levent.Ltarg,0);
          if(!device.touchable) {
            return osk._UpdateVKShift(Levent, Levent.Lcode-15, 1);  // I2187
          } else {
            return true;
          }
      }
      
      if(Levent.LmodifierChange){
        keymanweb._NotifyKeyboard(0,Levent.Ltarg,0); 
        osk._UpdateVKShift(Levent, 0, 1);  // I2187
      }

      // I736 start
      var Ldv;
      if((Ldv=Levent.Ltarg.ownerDocument)  &&  (Ldv=Ldv.selection)  &&  Ldv.type != 'control') { // I1479 - avoid createRange on controls
        Ldv=Ldv.createRange();
        //if(Ldv.parentElement()==Levent.Ltarg) //I1505
        keymanweb._Selection = Ldv;
      }
      // I736 end
      
      return false;
    }
    
  //TODO: check return of _KeyUp - what happens if returning true or false ?? what if null returned?

    /**
    * Move focus to next (or previous) input or text area element on TAB
    *   Uses list of actual input elements
    *     
    *   Note that _ActiveElement on touch devices returns the DIV that overlays
    *   the input element, not the element itself.
    * 
    * @param      {number|boolean}  bBack     Direction to move (0 or 1)
    */
    keymanweb.moveToNext=function(bBack) {
      var i,t=keymanweb.sortedInputs, activeBase=keymanweb._ActiveElement;
      
      if(t.length == 0) {
        return;
      }

      // For touchable devices, get the base element of the DIV
      if(device.touchable) {
        activeBase=activeBase.base;
      }

      // Identify the active element in the list of inputs ordered by position
      for(i=0; i<t.length; i++) {          
        if(t[i] == activeBase) break;
      }   

      // Find the next (or previous) element in the list
      i = bBack ? i-1 : i+1;
      // Treat the list as circular, wrapping the index if necessary.
      i = i >= t.length ? i-t.length : i;
      i = i < 0 ? i+t.length : i;

      // Move to the selected element
      if(device.touchable) {                     
        // Set focusing flag to prevent OSK disappearing 
        keymanweb.focusing=true;
        var target=t[i]['kmw_ip'];

        // Focus if next element is non-mapped
        if(typeof(target) == 'undefined') {
          t[i].focus();
        } else { // Or reposition the caret on the input DIV if mapped
          keymanweb._ActiveElement=keymanweb._LastActiveElement=target;    
          keymanweb.touchAliasing.setTextCaret(target,10000);                            
          keymanweb.touchAliasing.scrollInput(target);   // mousedown check
          target.focus();
        } 
      } else { // Behaviour for desktop browsers
        t[i].focus();
      }    
    }          

    /**
     * Move focus to user-specified element
     * 
     *  @param  {string|Object}   e   element or element id
     *           
     **/
    keymanweb['moveToElement'] = function(e)
    {
      var i;
      
      if(typeof(e) == 'string') e=document.getElementById(e);
      
      if(device.touchable && e['kmw_ip'])
        e['kmw_ip'].focus();
      else
        e.focus();
    }

    /**
     * Function    isCJK
     * Scope       Public
     * @param      {Object=}  k0 
     * @return     {boolean}
     * Description Tests if active keyboard (or optional argument) uses a pick list (Chinese, Japanese, Korean, etc.)
     *             (This function accepts either keyboard structure.)   
     */    
    keymanweb['isCJK'] = function(k0) { 
      return keymanweb.keyboardManager.isCJK(k0);
    }

    /**
     * Function     isChiral
     * Scope        Public
     * @param       {string|Object=}   k0
     * @return      {boolean}
     * Description  Tests if the active keyboard (or optional argument) uses chiral modifiers.
     */
    keymanweb.isChiral = keymanweb['isChiral'] = function(k0) {
      if(typeof(k0) == "string") {
        k0 = keymanweb.keyboardManager.getKeyboardByID(k0);
      }

      return !!(keymanweb.getKeyboardModifierBitmask(k0) & keymanweb['osk'].modifierBitmasks.IS_CHIRAL);
    }

    /**
     * Function     getKeyboardModifierBitmask
     * Scope        Private
     * @param       {Object=}   k0
     * @return      {number}
     * Description  Obtains the currently-active modifier bitmask for the active keyboard.
     */
    keymanweb.getKeyboardModifierBitmask = function(k0) {
      var k=keymanweb.keyboardManager.activeKeyboard;
      
      if(arguments.length > 0 && typeof k0 != 'undefined') {
        k = k0;
      }

      if(!k) {
        return 0x0000;
      }

      if(k['KMBM']) {
        return k['KMBM'];
      }

      return osk.modifierBitmasks['NON_CHIRAL'];
    }
    
    /**
     * Allow to change active keyboard by (internal) keyboard name
     * 
     * @param       {string}    PInternalName   Internal name
     * @param       {string}    PLgCode         Language code
     */    
    keymanweb['setActiveKeyboard'] = function(PInternalName,PLgCode) {
      keymanweb.keyboardManager.setActiveKeyboard(PInternalName,PLgCode);
    }
    
    /**
     * Function     getActiveKeyboard
     * Scope        Public
     * @return      {string}      Name of active keyboard
     * Description  Return internal name of currently active keyboard
     */    
    keymanweb['getActiveKeyboard'] = function() {
      return keymanweb.keyboardManager.getActiveKeyboardName();
    }

    /**
     * Function    getActiveLanguage
     * Scope       Public
     * @return     {string}         language code
     * Description Return language code for currently selected language
     */    
    keymanweb['getActiveLanguage'] = keymanweb.getActiveLanguage = function()
    {
      if((<KeymanBase>keymanweb).keyboardManager.activeStub == null) return '';
      return (<KeymanBase>keymanweb).keyboardManager.activeStub['KLC'];
    }

  //TODO: find all references to next three routines and disambiguate!!
    
    /**
     * Get keyboard meta data for the selected keyboard and language
     * 
     * @param       {string}    PInternalName     Internal name of keyboard
     * @param       {string=}   PlgCode           language code
     * @return      {Object}                      Details of named keyboard 
     *                                            
     **/    
    keymanweb['getKeyboard'] = function(PInternalName, PlgCode?) {
      var Ln, Lrn;

      var kbdList = keymanweb.keyboardManager.getDetailedKeyboards();

      for(Ln=0; Ln < kbdList.length; Ln++) {
        Lrn = kbdList[Ln];

        if(Lrn['InternalName'] == PInternalName || Lrn['InternalName'] == "Keyboard_" + PInternalName) { 
          if(arguments.length < 2) {
            return Lrn;
          }

          if(Lrn['LanguageCode'] == PlgCode) {
            return Lrn;
          }
        } 
      }

      return null;
    }
    
    /**
     * Get array of available keyboard stubs 
     * 
     * @return   {Array}     Array of available keyboards
     * 
     */    
    keymanweb['getKeyboards'] = function() {
      return keymanweb.keyboardManager.getDetailedKeyboards();
    }
    
    /**
     * Function     _WindowUnload
     * Scope        Private
     * Description  Remove handlers before detaching KMW window  
     */    
    keymanweb._WindowUnload = function()
    {
      // Allow the UI to release its own resources
      keymanweb.doUnloadUI();
      
      // Allow the OSK to release its own resources
      if(osk.ready) osk._Unload(); // I3363 (Build 301)
      
      keymanweb._LastActiveElement = 0;
    }
    
      // Complete page initialization only after the page is fully loaded, including any embedded fonts
    // This avoids the need to use a timer to test for the fonts
    
    util.attachDOMEvent(window, 'load',function(e){
      //keymanweb.completeInitialization();
      // Always return to top of page after a page reload
      document.body.scrollTop=0;
      if(typeof document.documentElement != 'undefined') document.documentElement.scrollTop=0;
      },false);
    
    // Attach this handler to window unload event  
    util.attachDOMEvent(window, 'unload', keymanweb._WindowUnload,false);  // added fourth argument (default value)

    /**
     * Function     _GetDocumentEditables
     * Scope        Private
     * @param       {Element}     Pelem     HTML element
     * @return      {Array<Element>}        A list of potentially-editable controls.  Further filtering [as with isKMWInput() and
     *                                      isKMWDisabled()] is required.
     */
    keymanweb._GetDocumentEditables = function(Pelem) {
      var possibleInputs = [];

      if(Pelem.tagName) {
        var tagName = Pelem.tagName.toLowerCase();
        if(tagName == 'input' || tagName == 'textarea' || tagName == 'iframe') {
          possibleInputs.push(Pelem);
        }
      } else if(Pelem.nodeName == "#text") {
        return [];
      }

      // Constructing it like this also allows for individual element filtering for the auto-attach MutationObserver without errors.
      if(Pelem.getElementsByTagName) {
        /**
         * Function     LiTmp
         * Scope        Private
         * @param       {string}    _colon    type of element
         * @return      {Array<Element>}  array of elements of specified type                       
         * Description  Local function to get list of editable controls
         */    
        var LiTmp = function(_colon){
          return util.arrayFromNodeList(Pelem.getElementsByTagName(_colon));
        };

        // Note that isKMWInput() will block IFRAME elements as necessary for touch-based devices.
        possibleInputs = possibleInputs.concat(LiTmp('INPUT'), LiTmp('TEXTAREA'), LiTmp('IFRAME'));
      }
      
      // Not all active browsers may support the method, but only those that do would work with contenteditables anyway.
      if(Pelem.querySelectorAll) {
        possibleInputs = possibleInputs.concat(util.arrayFromNodeList(Pelem.querySelectorAll('[contenteditable]')));
      }
      
      if(Pelem.isContentEditable) {
        possibleInputs.push(Pelem);
      }

      return possibleInputs;
    }

    /**
     * Function     _SetupDocument
     * Scope        Private
     * @param       {Element}     Pelem - the root element of a document, including IFrame documents.
     * Description  Used to automatically attach KMW to editable controls, regardless of control path.
     */
    keymanweb._SetupDocument = function(Pelem) { // I1961
      var possibleInputs = keymanweb._GetDocumentEditables(Pelem);

      for(var Li = 0; Li < possibleInputs.length; Li++) {
        var input = possibleInputs[Li];

        // It knows how to handle pre-loaded iframes appropriately.
        keymanweb.domManager.attachToControl(possibleInputs[Li]);
      }
    }

        /**
     * Function     _ClearDocument
     * Scope        Private
     * @param       {Element}     Pelem - the root element of a document, including IFrame documents.
     * Description  Used to automatically detach KMW from editable controls, regardless of control path.
     *              Mostly used to clear out all controls of a detached IFrame.
     */
    keymanweb._ClearDocument = function(Pelem) { // I1961
      var possibleInputs = keymanweb._GetDocumentEditables(Pelem);

      for(var Li = 0; Li < possibleInputs.length; Li++) {
        var input = possibleInputs[Li];

        // It knows how to handle pre-loaded iframes appropriately.
        keymanweb.domManager.detachFromControl(possibleInputs[Li]);
      }
    }
    
    /**
     * Return a path that has is always terminated by a slash
     *    
     * @param   {string}  p folder path   
     * @return  {string}   
    **/      
    keymanweb.fixPath = function(p)
    {
      if(p.length == 0) return p;
      var q=p.substr(p.length-1,1);
      if(q == '/' || q == '\\') return p;
      return p+'/'; 
    }          


    /**
     * Function     Initialization
     * Scope        Public
     * @param       {Object}  arg     object array of user-defined properties
     * Description  KMW window initialization  
     */    
    keymanweb['init'] = keymanweb.init = function(arg) 
    { 
      var i,j,c,e,p,eTextArea,eInput,opt,dTrailer,ds;

      // Local function to convert relative to absolute URLs
      // with respect to the source path, server root and protocol 
      var fixPath = function(p) {
        if(p.length == 0) return p;
        
        // Add delimiter if missing
        if(p.substr(p.length-1,1) != '/') p = p+'/';

        // Absolute
        if((p.replace(/^(http)s?:.*/,'$1') == 'http') 
            || (p.replace(/^(file):.*/,'$1') == 'file'))
          return p;         
          
        // Absolute (except for protocol)
        if(p.substr(0,2) == '//')
          return keymanweb.protocol+p;
        
        // Relative to server root
        if(p.substr(0,1) == '/')
          return keymanweb.rootPath+p.substr(1);

        // Otherwise, assume relative to source path
        return keymanweb.srcPath+p;
      }            
      
      // Explicit (user-defined) parameter initialization       
      opt=keymanweb.options;
      if(typeof(arg) == 'object' && arg !== null)
      {
        for(p in opt)
        { 
          if(arg.hasOwnProperty(p)) opt[p] = arg[p];
        }
      }
    
      // Get default paths and device options
      if(opt['root'] != '') keymanweb.rootPath = fixPath(opt['root']); 
  
      // Keyboards and fonts are located with respect to the server root by default          
      //if(opt['keyboards'] == '') opt['keyboards'] = keymanweb.rootPath+'keyboard/';
      //if(opt['fonts'] == '') opt['fonts'] = keymanweb.rootPath+'font/';
    
      // Resources are located with respect to the engine by default 
      if(opt['resources'] == '') opt['resources'] = keymanweb.srcPath;
    
      // Convert resource, keyboard and font paths to absolute URLs
      opt['resources'] = fixPath(opt['resources']);
      opt['keyboards'] = fixPath(opt['keyboards']);
      opt['fonts'] = fixPath(opt['fonts']);    

      // Set element attachment type    
      if(opt['attachType'] == '') opt['attachType'] = 'auto';

  /*    

      // Only initialize options the first time init is called   
      if(typeof(keymanweb.options['resources']) == 'undefined') 
      {
        var root='',fontSource='',resources='',keyboards='';
            
        // Get values of global variables if defined 
        if(typeof(window['KeymanWeb_Root']) == 'string') root=window['KeymanWeb_Root'];
        if(typeof(window['KeymanWeb_Resources']) == 'string') resources=window['KeymanWeb_Resources'];
        if(typeof(window['KeymanWeb_Keyboards']) == 'string') keyboards=window['KeymanWeb_Keyboards'];
        if(typeof(window['KeymanWeb_FontUriBasePath']) == 'string') fontSource=window['KeymanWeb_FontUriBasePath'];
    
        var opt={};
        opt['root']=root;
        opt['resources']=resources;
        opt['keyboards']=keyboards;
        opt['fonts'] = fontSource;      

        if(typeof(window['KeymanWeb_AttachType']) == 'string') 
          opt['attachType']=window['KeymanWeb_AttachType'];
        else
        opt['attachType'] = (device.touchable ? 'manual' : 'auto');
  
        if(typeof(window['KeymanWeb_FloatUI']) == 'object') 
          opt['ui'] = window['KeymanWeb_FloatUI'];
        else
          opt['ui'] = '';
        
        if(device.touchable) opt['ui'] = 'none';
  
        keymanweb.options = opt;    
      }
      else
      {
        var opt=keymanweb.options;
      }
      
      // Update the option if required by a subsequent call
      if(arguments.length > 0 && typeof(arg)=='object' && arg != null)
      { 
        for(var p in opt)
        { 
          if(arg.hasOwnProperty(p)) opt[p] = arg[p];
        }
      }

      // Get default root path and device options
      if(opt['root'] == '') opt['root'] = keymanweb.rootPath;

      // Use root-relative paths for resources and keyboards unless set by page source
      if(opt['resources'] == '') opt['resources']=opt['root'];
      if(opt['keyboards'] == '') opt['keyboards']=opt['root']+'kbd/';
      if(opt['fonts'] == '') opt['fonts']=opt['root']+'font/';
  */   
      
      // Set default device options
      keymanweb.setDefaultDeviceOptions(opt);   
      
      // Only do remainder of initialization once!  
      if(keymanweb.initialized) {
        return;
      }

      // Do not initialize until the document has been fully loaded
      if(document.readyState !== 'complete')
      {
        window.setTimeout(function(){keymanweb.init(arg);},50);
        return;
      }

      keymanweb._MasterDocument = window.document;

      /**
       * Initialization of touch devices and browser interfaces must be done 
       * after all resources are loaded, during final stage of initialization
       *        
       */            
      
      // Treat Android devices as phones if either (reported) screen dimension is less than 4" 
      if(device.OS == 'Android')
      {
        // Determine actual device characteristics  I3363 (Build 301)
        // TODO: device.dpi may no longer be needed - if so, get rid of it.
        device.dpi = util.getDPI(); //TODO: this will not work when called from HEAD!!
        device.formFactor=((screen.height < 4.0*device.dpi) || (screen.width < 4.0*device.dpi)) ? 'phone' : 'tablet';
      }

      if (window.removeEventListener)
        window.removeEventListener('focus', keymanweb._BubbledFocus, true);
    
      // Set exposed initialization flag member for UI (and other) code to use 
      keymanweb.setInitialized(1);
  
      // Finish keymanweb and OSK initialization once all necessary resources are available
      osk.prepare();
    
      // Create and save the remote keyboard loading delay indicator
      util.prepareWait();

      // Register deferred keyboard stubs (addKeyboards() format)
      keymanweb.keyboardManager.registerDeferredStubs();
    
      // Initialize the desktop UI
      keymanweb.initializeUI();
    
      // Register deferred keyboards 
      keymanweb.keyboardManager.registerDeferredKeyboards();
    
      // Exit initialization here if we're using an embedded code path.
      if(keymanweb.isEmbedded) {
        if(!keymanweb.keyboardManager.setDefaultKeyboard()) {
          console.error("No keyboard stubs exist - cannot initialize keyboard!");
        }
        return;
      }

      // Determine the default font for mapped elements
      keymanweb.appliedFont=keymanweb.baseFont=keymanweb.getBaseFont();

      // Add orientationchange event handler to manage orientation changes on mobile devices
      // Initialize touch-screen device interface  I3363 (Build 301)
      if(device.touchable) {
        keymanweb.handleRotationEvents();
        keymanweb.setupTouchDevice();
      }    
      // Initialize browser interface

      if(keymanweb.options['attachType'] != 'manual') {
        keymanweb._SetupDocument(document.documentElement);
      }

      // Create an ordered list of all input and textarea fields
      keymanweb.listInputs();
    
      // Initialize the OSK and set default OSK styles
      // Note that this should *never* be called before the OSK has been initialized.
      // However, it possibly may be called before the OSK has been fully defined with the current keyboard, need to check.    
      //osk._Load(); 
      
      //document.body.appendChild(osk._Box); 

      //osk._Load(false);
      
      // I3363 (Build 301)
      if(device.touchable)
      {
        // Handle OSK touchend events (prevent propagation)
        osk._Box.addEventListener('touchend',function(e){e.stopPropagation();},false);

        // Add a blank DIV to the bottom of the page to allow the bottom of the page to be shown
        dTrailer=document.createElement('DIV'); ds=dTrailer.style;
        ds.width='100%';ds.height=(screen.width/2)+'px';
        document.body.appendChild(dTrailer);  
        
        // On Chrome, scrolling up or down causes the URL bar to be shown or hidden 
        // according to whether or not the document is at the top of the screen.
        // But when doing that, each OSK row top and height gets modified by Chrome
        // looking very ugly.  Itwould be best to hide the OSK then show it again 
        // when the user scroll finishes, but Chrome has no way to reliably report
        // the touch end event after a move. c.f. http://code.google.com/p/chromium/issues/detail?id=152913
        // The best compromise behaviour is simply to hide the OSK whenever any 
        // non-input and non-OSK element is touched.
        if(device.OS == 'Android' && navigator.userAgent.indexOf('Chrome') > 0)
        {
          keymanweb.hideOskWhileScrolling=function(e)
          {           
            if(typeof(osk._Box) == 'undefined') return;
            if(typeof(osk._Box.style) == 'undefined') return;

            // The following tests are needed to prevent the OSK from being hidden during normal input!
            p=e.target.parentNode;
            if(typeof(p) != 'undefined' && p != null)
            {
              if(p.className.indexOf('keymanweb-input') >= 0) return; 
              if(p.className.indexOf('kmw-key-') >= 0) return; 
              if(typeof(p.parentNode) != 'undefined')
              {
                p=p.parentNode;
                if(p.className.indexOf('keymanweb-input') >= 0) return; 
                if(p.className.indexOf('kmw-key-') >= 0) return; 
              }
            }          
            osk.hideNow(); 
          }        
          document.body.addEventListener('touchstart',keymanweb.hideOskWhileScrolling,false);
        } 
        else
        {          
          keymanweb.conditionallyHideOsk = function()
          {
            // Should not hide OSK if simply closing the language menu (30/4/15)
            if(keymanweb.hideOnRelease && !osk.lgList) osk.hideNow();
            keymanweb.hideOnRelease=false;
          }
          keymanweb.hideOskIfOnBody = function(e)
          {
            keymanweb.touchY=e.touches[0].screenY;
            keymanweb.hideOnRelease=true;
          }
          keymanweb.cancelHideIfScrolling = function(e)
          {
            var y=e.touches[0].screenY,y0=keymanweb.touchY;    
            if(y-y0 > 5 || y0-y < 5) keymanweb.hideOnRelease = false;
          }
          document.body.addEventListener('touchstart',keymanweb.hideOskIfOnBody,false);      
          document.body.addEventListener('touchmove',keymanweb.cancelHideIfScrolling,false);      
          document.body.addEventListener('touchend',keymanweb.conditionallyHideOsk,false);      
        } 
      }

      //document.body.appendChild(keymanweb._StyleBlock);

      // IE: call _SelectionChange when the user changes the selection 
      if(document.selection)
        util.attachDOMEvent(document, 'selectionchange', keymanweb._SelectionChange);
    
      // Restore and reload the currently selected keyboard, selecting a default keyboard if necessary.
      keymanweb.keyboardManager.restoreCurrentKeyboard(); 

      /* Setup of handlers for dynamically-added and (eventually) dynamically-removed elements.
        * Reference: https://developer.mozilla.org/en/docs/Web/API/MutationObserver
        * 
        * We place it here so that it loads after most of the other UI loads, reducing the MutationObserver's overhead.
        * Of course, we only want to dynamically add elements if the user hasn't enabled the manual attachment option.
        */

      var observationTarget = document.querySelector('body'), observationConfig;
      if(keymanweb.options['attachType'] != 'manual') { //I1961
        keymanweb.mutationObserver = new MutationObserver(keymanweb._AutoAttachObserverCore);

        observationConfig = { childList: true, subtree: true};
        keymanweb.mutationObserver.observe(observationTarget, observationConfig);
      }

      /**
       * Setup of handlers for dynamic detection of the kmw-disabled class tag that controls enablement.
       */
      observationConfig = { subtree: true, attributes: true, attributeOldValue: true, attributeFilter: ['class', 'readonly']};
      new MutationObserver(keymanweb._EnablementMutationObserverCore).observe(observationTarget, observationConfig);

      // Set exposed initialization flag to 2 to indicate deferred initialization also complete
      keymanweb.setInitialized(2);
    }

    keymanweb._EnablementMutationObserverCore = function(mutations) {
      for(var i=0; i < mutations.length; i++) {
        var mutation = mutations[i];

        // ( ? : ) needed as a null check.
        var disabledBefore = mutation.oldValue ? mutation.oldValue.indexOf('kmw-disabled') >= 0 : false;
        var disabledAfter = mutation.target.className.indexOf('kmw-disabled') >= 0;
        
        if(disabledBefore && !disabledAfter) {
          keymanweb._EnableControl(mutation.target);
        } else if(!disabledBefore && disabledAfter) {
          keymanweb._DisableControl(mutation.target);
        }

        // 'readonly' triggers on whether or not the attribute exists, not its value.
        if(!disabledAfter && mutation.attributeName == "readonly") {
          var readonlyBefore = mutation.oldValue ? mutation.oldValue != null : false;
          var readonlyAfter = mutation.target.readOnly;

          if(readonlyBefore && !readonlyAfter) {
            keymanweb._EnableControl(mutation.target);
          } else if(!readonlyBefore && readonlyAfter) {
            keymanweb._DisableControl(mutation.target);
          }
        }
      }
    }

    keymanweb._AutoAttachObserverCore = function(mutations) {
      var inputElementAdditions = [];
      var inputElementRemovals = [];

      for(var i=0; i < mutations.length; i++) {
        var mutation = mutations[i];
        
        for(var j=0; j < mutation.addedNodes.length; j++) {
          inputElementAdditions = inputElementAdditions.concat(keymanweb._GetDocumentEditables(mutation.addedNodes[j]));
        }          

        for(j = 0; j < mutation.removedNodes.length; j++) {
          inputElementRemovals = inputElementRemovals.concat(keymanweb._GetDocumentEditables(mutation.removedNodes[j]));
        }
      }

      for(var k = 0; k < inputElementAdditions.length; k++) {
        if(keymanweb.domManager.isKMWInput(inputElementAdditions[k])) { // Apply standard element filtering!
          keymanweb._MutationAdditionObserved(inputElementAdditions[k]);
        }
      }

      for(k = 0; k < inputElementRemovals.length; k++) {
        if(keymanweb.domManager.isKMWInput(inputElementRemovals[k])) { // Apply standard element filtering!
          keymanweb._MutationRemovalObserved(inputElementRemovals[k]);
        }
      }

      /* After all mutations have been handled, we need to recompile our .sortedInputs array, but only
        * if any have actually occurred.
        */
      if(inputElementAdditions.length || inputElementRemovals.length) {
        if(!device.touchable) {
          keymanweb.listInputs();
        } else if(device.touchable) {   // If something was added or removed, chances are it's gonna mess up our touch-based layout scheme, so let's update the touch elements.
          window.setTimeout(function() {
            keymanweb.listInputs();

            for(var k = 0; k < keymanweb.sortedInputs.length; k++) {
              if(keymanweb.sortedInputs[k]['kmw_ip']) {
                keymanweb.touchAliasing.updateInput(keymanweb.sortedInputs[k]['kmw_ip']);
              }
            }
          }, 1);
        }
      }
    }

    /** 
     * Function     _MutationAdditionObserved
     * Scope        Private
     * @param       {Element}  Pelem     A page input, textarea, or iframe element.
     * Description  Used by the MutationObserver event handler to properly setup any elements dynamically added to the document post-initialization.
     * 
     */
    keymanweb._MutationAdditionObserved = function(Pelem) {
      if(Pelem.tagName.toLowerCase() == 'iframe' && !device.touchable) {
        //Problem:  the iframe is loaded asynchronously, and we must wait for it to load fully before hooking in.

        var attachFunctor = function() {  // Triggers at the same time as iframe's onload property, after its internal document loads.
          keymanweb.domManager.attachToControl(Pelem);
        };

        Pelem.addEventListener('load', attachFunctor);

        /* If the iframe has somehow already loaded, we can't expect the onload event to be raised.  We ought just
        * go ahead and perform our callback's contents.
        * 
        * keymanweb.domManager.attachToControl() is now idempotent, so even if our call 'whiffs', it won't cause long-lasting
        * problems.
        */
        if(Pelem.contentDocument.readyState == 'complete') {
          attachFunctor();
        }
      } else {
        keymanweb.domManager.attachToControl(Pelem);
      }  
    }

    // Used by the mutation event handler to properly decouple any elements dynamically removed from the document.
    keymanweb._MutationRemovalObserved = function(Pelem) {
      var element = Pelem;
      if(device.touchable) {
        keymanweb.domManager.disableTouchElement(Pelem);
      }

      keymanweb.domManager.disableInputElement(Pelem); // Remove all KMW event hooks, styling.
      keymanweb.domManager.clearElementAttachment(element);  // Memory management & auto de-attachment upon removal.
    }

    // Create an ordered list of all text and search input elements and textarea elements
    // except any tagged with class 'kmw-disabled'
    // TODO: email and url types should perhaps use default keyboard only
    keymanweb.listInputs = function() {
      var i,eList=[],
        t1=document.getElementsByTagName<'input'>('input'),
        t2=document.getElementsByTagName<'textarea'>('textarea');

      for(i=0; i<t1.length; i++) {
        switch(t1[i].type) {
          case 'text':
          case 'search':
          case 'email':
          case 'url':
            if(t1[i].className.indexOf('kmw-disabled') < 0) {
              eList.push({ip:t1[i],x:util._GetAbsoluteX(t1[i]),y:util._GetAbsoluteY(t1[i])});
            }
            break;    
        }
      }

      for(i=0; i<t2.length; i++) { 
        if(t2[i].className.indexOf('kmw-disabled') < 0)
          eList.push({ip:t2[i],x:util._GetAbsoluteX(t2[i]),y:util._GetAbsoluteY(t2[i])});
      }
      
      /**
       * Local function to sort by screen position
       * 
       * @param       {Object}     e1     first object
       * @param       {Object}     e2     second object
       * @return      {number}            y-difference between object positions, or x-difference if y values the same
       */       
      var xySort=function(e1,e2)
      {
        if(e1.y != e2.y) return e1.y-e2.y;
        return e1.x-e2.x;    
      }
      
      // Sort elements by Y then X
      eList.sort(xySort);
      
      // Create a new list of sorted elements
      var tList=[];
      for(i=0;i<eList.length;i++)
        tList.push(eList[i].ip);
    
      // Return the sorted element list
      keymanweb.sortedInputs=tList;
    }
    
    /**
     * Initialize the desktop user interface as soon as it is ready
    **/       
    keymanweb.initializeUI = function()
    {
      if(typeof(keymanweb['ui']['initialize'])=='function')
      {
        keymanweb['ui']['initialize']();
        // Display the OSK (again) if enabled, in order to set its position correctly after
        // adding the UI to the page 
        osk._Show();     
      }
      else
        window.setTimeout(keymanweb.initializeUI,1000);
    }      
    /**
     * Test if caret position is determined from the active element, or 
     * from the synthesized overlay element (touch devices)
     * 
     * @return  {boolean}
     **/          
    keymanweb.isPositionSynthesized = function()
    {
      return device.touchable;
    }
    
    /**
     * Function     _SelPos
     * Scope        Private
     * @param       {Object}  Pelem   Element
     * @return      {number}          Selection start
     * Description  Get start of selection (with supplementary plane modifications)
     */   
    keymanweb._SelPos = function(Pelem)
    {
      var Ldoc, Ldv, isMSIE=(util._GetIEVersion()<999); // I3363 (Build 301)

      if(keymanweb.isPositionSynthesized())
        return keymanweb.getTextCaret(Pelem);

      if(Pelem._KeymanWebSelectionStart) 
        return Pelem._KeymanWebSelectionStart;
      
      // Mozilla, IE9 
      else if (Pelem.setSelectionRange)  
        return Pelem.value.substr(0,Pelem.selectionStart)._kmwLength();        
    
      // contentEditable elements, Mozilla midas
      else if((Ldv=Pelem.ownerDocument)  &&  (Ldv=Ldv.defaultView)  &&  Ldv.getSelection
        &&  Pelem.ownerDocument.designMode.toLowerCase() == 'on') {
        var Lsel = Ldv.getSelection();
        if(Lsel.focusNode.nodeType == 3) 
          return Lsel.focusNode.substringData(0,Lsel.focusOffset)._kmwLength(); 
      }
      
      // IE8 and earlier
      else if(isMSIE)
      { 
        // Get position within input or textarea element       
        if(typeof(Pelem.value) == 'string') {
          var ss=keymanweb.getInputSelection(Pelem);               
          return Pelem.value.substr(0,ss.start)._kmwLength();        
        }
        
        // Get position within content-editable region
        if(Pelem.body) Ldoc=Pelem; else Ldoc=Pelem.ownerDocument;	// I1481 - integration with rich editors not working 100%

        if(Ldoc) Ldv=Ldoc.selection; else return 0;
            
        var Lrange = Ldv.createRange();
        Lrange.moveStart('textedit',-1);
        return Lrange.text._kmwLength();    
      }
      return 0;
    }  

    /*    Old code without SMP mods
    
    keymanweb._SelPos = function(Pelem)
    {
      var Ldv;
      if(Pelem._KeymanWebSelectionStart) return Pelem._KeymanWebSelectionStart;
      else if (Pelem.setSelectionRange)
        return Pelem.selectionStart;
      else if((Ldv=Pelem.ownerDocument)  &&  (Ldv=Ldv.defaultView)  &&  Ldv.getSelection  &&  Pelem.ownerDocument.designMode.toLowerCase() == 'on') //  &&  Pelem.tagName == 'HTML')
      {
        var Lsel = Ldv.getSelection();
        if(Lsel.focusNode.nodeType == 3) return Lsel.focusOffset;
      }
      return 0;
    }*/   
    
    /**
     * Function     getInputSelection
     * Scope        Private
     * @param       {Object}      el          element
     * @return      {Object.<string,number>}  selection start
     * Description Get input selection for all(?) browsers, per Tim Down
     *            http://stackoverflow.com/questions/3053542/how-to-get-the-start-and-end-points-of-selection-in-text-area/3053640#3053640 
     *            But only works for input fields, not for content editable fields!!!  
     **/            
    keymanweb.getInputSelection = function(el)
    { 
      var start = 0, end = 0, normalizedValue = '', range, textInputRange, len = 0, endRange; 
  
      if(typeof el.selectionStart == "number" && typeof el.selectionEnd == "number") { 
        start = el.selectionStart; end = el.selectionEnd; 
      } else { 
        range = document.selection.createRange(); 
  
        if(range && range.parentElement() == el) { 
          len = el.value.length; 
          normalizedValue = el.value.replace(/\r\n/g, "\n"); 
              
          // Create a working TextRange that lives only in the input 
          textInputRange = el.createTextRange(); 
          textInputRange.moveToBookmark(range.getBookmark()); 
  
          // Check if the start and end of the selection are at the very end of the input,
          // since moveStart/moveEnd doesn't return what we want in those cases 
          endRange = el.createTextRange(); 
          endRange.collapse(false); 
  
          if(textInputRange.compareEndPoints("StartToEnd", endRange) > -1) { 
            start = end = len; 
          } else { 
            start = -textInputRange.moveStart("character", -len); 
            start += normalizedValue.slice(0, start).split("\n").length - 1; 
  
            if(textInputRange.compareEndPoints("EndToEnd", endRange) > -1) { 
              end = len; 
            } else { 
              end = -textInputRange.moveEnd("character", -len); 
              end += normalizedValue.slice(0, end).split("\n").length - 1; 
            } 
          } 
        } 
      } 
      return {start: start, end: end}; 
    }
    // *** I3319 Supplementary Plane modifications - end new code
  
    /**
     * Set target element text direction (LTR or RTL), but only if the element is empty
     *    
     * If the element base directionality is changed after it contains content, unless all the text
     * has the same directionality, text runs will be re-ordered which is confusing and causes
     * incorrect caret positioning
     *    
     * @param       {Object}      Ptarg      Target element
     */    
    keymanweb._SetTargDir = function(Ptarg) {  
      var elDir=((keymanweb.keyboardManager.activeKeyboard != null) && (keymanweb.keyboardManager.activeKeyboard['KRTL'])) ? 'rtl' : 'ltr';  
      if(Ptarg) {
        if(device.touchable) {
          if(Ptarg.textContent.length == 0) {
            Ptarg.base.dir=Ptarg.dir=elDir;
            keymanweb.touchAliasing.setTextCaret(Ptarg,10000);
          }
        } else {
          if(typeof Ptarg.value == "string") {
            if(Ptarg.value.length == 0) {
              Ptarg.dir=elDir;
            }
          } else if(typeof Ptarg.textContent == "string" && Ptarg.textContent.length == 0) { // As with contenteditable DIVs, for example.
            Ptarg.dir=elDir;
          }
        }
      }
    }
    
    /**
     * Reset OSK shift states when entering or exiting the active element
     **/    
    keymanweb._ResetVKShift = function()
    {
      if(!keymanweb._IsActivatingKeymanWebUI) 
      {
        if(osk._UpdateVKShift) osk._UpdateVKShift(null,15,0);  //this should be enabled !!!!! TODO
      }
    }

    /**
     * Function     addHotKey
     * Scope        Public
     * @param       {number}            keyCode
     * @param       {number}            shiftState
     * @param       {function(Object)}  handler
     * Description  Add hot key handler to array of document-level hotkeys triggered by key up event
     */
    keymanweb['addHotKey'] = keymanweb.addHotKey = function(keyCode,shiftState,handler)
    {
      // Test if existing handler for this code and replace it if so
      for(var i=0; i<keymanweb._HotKeys.length; i++)
      {
        if(keymanweb._HotKeys[i].Code == keyCode && keymanweb._HotKeys[i].Shift == shiftState)
        {
          keymanweb._HotKeys[i].Handler = handler; return;
        }
      }
      // Otherwise add it to the array
      keymanweb._HotKeys.push({Code:keyCode,Shift:shiftState,Handler:handler});
    }              

    /**
     * Function     removeHotKey
     * Scope        Public
     * @param       {number}        keyCode
     * @param       {number}        shiftState
     * Description  Remove a hot key handler from array of document-level hotkeys triggered by key up event
     */
    keymanweb['removeHotKey'] = keymanweb.removeHotKey = function(keyCode,shiftState)
    {
      for(var i=0; i<keymanweb._HotKeys.length; i++)
      {
        if(keymanweb._HotKeys[i].Code == keyCode && keymanweb._HotKeys[i].Shift == shiftState)
        {
          keymanweb._HotKeys.splice(i,1); return;
        }
      }
    }
                  
    /**
     * Function     _ProcessHotKeys
     * Scope        Private
     * @param       {Event}       e       event
     * Description  Passes control to handlers according to the hotkey pressed
     */
    keymanweb._ProcessHotKeys = function(e) {
      if(!e) {
        e = window.event;
      }

      var _Lcode = keymanweb._GetEventKeyCode(e);
      if(_Lcode == null) {
        return null;
      }
      
      // Removed testing of e.shiftKey==null  I3363 (Build 301)
      var _Lmodifiers = 
        (e.shiftKey ? 0x10 : 0) |
        (e.ctrlKey ? (e.ctrlLeft ? 0x20 : 0x20) : 0) | 
        (e.altKey ? (e.altLeft ? 0x40 : 0x40) : 0);

      for(var i=0; i<keymanweb._HotKeys.length; i++) {  
        if(_Lcode == keymanweb._HotKeys[i].Code) { 
          if(_Lmodifiers == keymanweb._HotKeys[i].Shift) { 
            keymanweb._HotKeys[i].Handler(); 
            e.returnValue = 0; 
            if(e && e.preventDefault) e.preventDefault(); 
            e.cancelBubble = true; 
            return false; 
          }
        }
      }
      return true;
    }

    util.attachDOMEvent(document, 'keyup', keymanweb._ProcessHotKeys,false);  

    /**
     * Gets the cookie for the name and language code of the most recently active keyboard
     * 
     *  Defaults to US English, but this needs to be user-set in later revision (TODO)      
     * 
     * @return      {string}          InternalName:LanguageCode 
     **/    
    keymanweb['getSavedKeyboard'] = function() {
      return keymanweb.keyboardManager.getSavedKeyboard();  
    }

    util.attachDOMEvent(window, 'focus', keymanweb._ResetVKShift,false);  // I775
    util.attachDOMEvent(window, 'blur', keymanweb._ResetVKShift,false);   // I775
    
    // Initialize supplementary plane string extensions
    String.kmwEnableSupplementaryPlane(false);    

  })();
}

