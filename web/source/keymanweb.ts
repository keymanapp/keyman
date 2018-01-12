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

      keymanweb.touchAliasing = keymanweb.domManager.touchHandlers;

    }
    
    /*********************************************************
     *  
     * End of main touch-device initialization.
     *     
     *********************************************************/

    keymanweb.touchAliasing = keymanweb.domManager.nonTouchHandlers;

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

      if(Pelem instanceof HTMLIFrameElement) {
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
        if(keymanweb._LastActiveElement && (keymanweb._LastActiveElement == Pelem || keymanweb._LastActiveElement == Pelem['kmw_ip'])) {

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

  //TODO: find all references to next three routines and disambiguate!!
    
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
    
    util.attachDOMEvent(window, 'load', function(e){
      //keymanweb.completeInitialization();
      // Always return to top of page after a page reload
      document.body.scrollTop=0;
      if(typeof document.documentElement != 'undefined') document.documentElement.scrollTop=0;
      },false);
    
    // Attach this handler to window unload event  
    util.attachDOMEvent(window, 'unload', keymanweb._WindowUnload,false);  // added fourth argument (default value)
    
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

      var _Lcode = keymanweb.domManager.nonTouchHandlers._GetEventKeyCode(e);
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

