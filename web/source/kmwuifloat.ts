/// <reference path="../node_modules/@keymanapp/web-utils/src/kmwstring.ts" />

/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/

// If a UI module has been loaded, we can rely on the publically-published 'name' property
// having been set as a way to short-out a UI reload.  Its parent object always exists by
// this point in the build process.
if(!window['keyman']['ui']['name']) { 
  /********************************/
  /*                              */
  /* Floating User Interface      */
  /*                              */
  /********************************/

  /**
   * Do not enclose in an anonymous function, as the compiler may create
   * global scope variables to replace true, false, null, which can collide
   * with other variables.
   * Instead, use the --output-wrapper command during optimization, which will
   * add the anonymous function to enclose all code, including those optimized
   * variables which would otherwise have global scope.
   **/  

  try {

    // Declare KeymanWeb, OnScreen keyboard and Util objects
    var keymanweb=window['keyman'];
    var util=keymanweb['util'];
    var osk=keymanweb['osk'];
    var dbg=keymanweb['debug'];

    // Disable UI for touch devices
    if(util['isTouchDevice']()) throw '';
    
    // User interface global and local variables
    keymanweb['ui'] = {};
    var ui=keymanweb['ui'];
    ui['name'] = 'float';
    
    ui.KeyboardSelector = null; 
    
    ui.outerDiv = null;     // replaces DivKeymanWeb
    ui.innerDiv = null;     // replaces Lkdiv
    ui.oskButton = null;    // toggles OSK on or off  
    ui.kbdIcon = null;      // keyboard icon within OSK button
    ui.selecting = false;   // control focus behaviour during selection
    ui.updateList = true;   // control keyboard list updating
    ui.updateTimer = null;  // prevent unnecessary list refreshing
    ui.floatRight = false;  // align left by default
    ui.initialized = false; // initialization flag 

    /**
     * Display or hide the OSK from the OSK icon link
     */     
    ui.toggleOSK = function()
    {
      keymanweb['focusLastActiveElement']();
      if(osk['show']) 
      {
        if(osk['isEnabled']()) osk['hide'](); else osk['show'](true);
      }
      if(window.event) window.event.returnValue=false;
      return false;
    }
    
    /**
     * Function     Initialize
     * Scope        Private   
     * Description  UI Initialization
     **/
    ui.Initialize = function()
    { 
      // Must always initialize after keymanWeb itself, otherwise options are undefined
      if(!keymanweb['initialized'])
      {
        window.setTimeout(ui.Initialize,50); return;
      }
      
      if(ui.initialized || util['isTouchDevice']()) return;
          
      var imgPath=util['getOption']('resources')+"ui/float/";
      
      ui.outerDiv = util['createElement']('DIV');         // Container for UI (visible when KeymanWeb is active)
      ui.innerDiv = util['createElement']('DIV');         // inner div for UI
      ui.kbdIcon = util['createElement']('IMG');  
      ui.outerDiv.innerHTML = "<a href='http://keyman.com/web/' target='KeymanWebHelp'>"
        + "<img src='"+imgPath+"kmicon.gif' border='0' style='padding: 0px 2px 0 1px; margin:0px;' title='KeymanWeb' alt='KeymanWeb' /></a>"; /* I2081 */

      var s=ui.outerDiv.style;                                                                              
      s.backgroundColor='white'; s.border='solid 1px black'; s.position='absolute'; s.height='18px';
      s.font='bold 8pt sans-serif'; s.display='none'; s.textAlign='left';s.overflow='hidden'; 
      
      util['attachDOMEvent'](ui.outerDiv,'mousedown',ui._SelectorMouseDown);
      util['attachDOMEvent'](ui.outerDiv,'mouseover',ui._SelectorMouseOver);
      util['attachDOMEvent'](ui.outerDiv,'mouseout',ui._SelectorMouseOut);
          
      // Register keymanweb events
      ui.registerEvents();        

      ui.kbdIcon.src = imgPath+'kbdicon.gif';
      ui.kbdIcon.title = 'Display visual keyboard';

      // Set initial OSK button style (off by default)
      ui.oskButtonState(false);
      
      var Lhdiv = util['createElement']('DIV'); 
      ui.oskButton = Lhdiv;
      Lhdiv.onclick = ui.toggleOSK;    
      Lhdiv.appendChild(ui.kbdIcon);
      ui.innerDiv.appendChild(Lhdiv);
      ui.outerDiv.appendChild(ui.innerDiv);
      document.body.appendChild(ui.outerDiv);

      ui.KeyboardSelector =  util['createElement']('SELECT'); // ControlSelector - KeymanWeb keyboard selector
      
      s=ui.KeyboardSelector.style;
      s.font='8pt sans-serif'; s.backgroundColor='#f3e5de'; s.border='solid 1px #7B9EBD'; s.height='16px';s.margin='1px 2px 0px 2px';
      s.left='20px'; s.top='0px'; s.position='absolute'; s.maxWidth='150px';

      util['attachDOMEvent'](ui.KeyboardSelector,'change',ui.SelectKeyboardChange);
      util['attachDOMEvent'](ui.KeyboardSelector,'blur',ui.SelectBlur);
      
      ui.innerDiv.appendChild(ui.KeyboardSelector);  //this may need to be moved up.... 

      // Check required interface alignment and default keyboard
      var opt=util['getOption']('ui'),dfltKeyboard='(System keyboard)';
      if(opt && typeof(opt) == 'object')
      {
        if(typeof(opt['position']) == 'string' && opt['position'] == 'right') 
          ui.floatRight = true;
        if(typeof(opt['default']) == 'string') 
          dfltKeyboard = opt['default']; 
      }

      var Lopt = util['createElement']('OPTION');
      Lopt.value = '-';
      Lopt.innerHTML = dfltKeyboard;
      ui.KeyboardSelector.appendChild(Lopt);  
      Lopt = null;

      // This must be set before updating the keyboard list to prevent recursion!
      ui.initialized = true;

      // Update the keyboard list if required
      ui.updateKeyboardList();
      
      //may also want to initialize style sheet here ??
    }

    ui._UnloadUserInterface = function() {
      ui.KeyboardSelector = ui.innerDiv = ui.outerDiv = ui.kbdIcon = null; 
    };
    /**
     * UI removal - resource cleanup
     */    
    keymanweb['addEventListener']('unloaduserinterface', ui._UnloadUserInterface);
        

    ui.shutdown = function() {
      var root = ui.outerDiv;
      if(root) {
        root.parentNode.removeChild(root);
      }

      ui._UnloadUserInterface();

      if(window.removeEventListener) {
        window.removeEventListener('resize', ui._Resize, false);
      }
    }
  
    /**
     * Update list of keyboards shown by UI
     */     
    ui.updateKeyboardList = function()
    {
      // Do nothing unless list requires updating
      if(ui.updateList) 
      { 
        if(!ui.initialized) ui.Initialize();
                                                      
        // Remove current list (except for default element)
        var i,opts=ui.KeyboardSelector.getElementsByTagName('OPTION');
        for(i=opts.length; i>1; i--)
        {
          ui.KeyboardSelector.removeChild(opts[i-1]);
        }    
        
        // Loop over installed keyboards and add to selection list     
        var Ln,Lopt,Lkbds=keymanweb['getKeyboards']();  
            
        for(Ln=0; Ln<Lkbds.length; Ln++) 
        {
          Lopt = util['createElement']('OPTION');
          Lopt.value = Lkbds[Ln]['InternalName']+':'+Lkbds[Ln]['LanguageCode'];;        
          Lopt.innerHTML = Lkbds[Ln]['Name'].replace(/\s?keyboard/i,'');      
          if(Lkbds[Ln]['LanguageName'])
          {
            var lg=Lkbds[Ln]['LanguageName'];
            // Only show the main language name if variants indicated (this is tentative)
            // e.g. Chinese rather than Chinese, Mandarin, which is in the keyboard name
            lg = lg.split(',')[0];
            if(Lkbds[Ln]['Name'].search(lg) == -1)        
              Lopt.innerHTML = lg+' ('+Lopt.innerHTML+')';  // I1300 - Language support
          }
          
          ui.KeyboardSelector.appendChild(Lopt);
          Lopt = null;
        }
      }
      ui.updateList = false;    
      
      // Set the menu selector to the currently saved keyboard
      var sk = keymanweb['getSavedKeyboard']().split(':');
      if(sk.length < 2) sk[1] = ''; 
      ui.updateMenu(sk[0],sk[1]); 
      
      // Redisplay the UI to correct width for any new language entries
      if(keymanweb['getLastActiveElement']())
      {
        ui.HideInterface();
        ui.ShowInterface();
      }
    }

    /**
     * Function     updateMenu
     * Scope        Private   
     * @param       {string}     kbd
     * @param       {string}     lg 
     * Description  Update the UI menu selection
     *              Separate arguments passed to allow better control of selection when a keyboard 
     *              is listed more than once for different language codes       
     */   
    ui.updateMenu = function(kbd,lg)
    {
      var i=0,value='English';
      
      // This can be called during startup before fully initialized - ignore if so
      if(!ui.initialized) return;  
      
      var match = kbd;
      if(lg != '') match += ':' + lg;
      
      if(kbd == '')
        ui.KeyboardSelector.selectedIndex=0;
      else
      {  
        var opt=ui.KeyboardSelector.getElementsByTagName('OPTION');
        for(i=0; i<opt.length; i++)
        {
          var t=opt[i].value; 
          if(lg == '') t = t.split(':')[0];
          if(t == match) 
          {
            ui.KeyboardSelector.selectedIndex=i;
            break; 
          }   
        }
      }
    }  

    /**
     * Function     oskButtonState
     * Scope        Private   
     * @param       {boolean}     oskEnabled
     * Description  Update kbd icon border style to indicate whether OSK is enabled for display or not
     **/   
    ui.oskButtonState = function(oskEnabled)
    {
      var s = ui.kbdIcon.style;
      s.width='24px'; s.height='13px'; s.top='1px'; 
      s.verticalAlign='bottom'; s.padding='2px 2px 1px 2px'; s.position='absolute';
      s.border=oskEnabled ? 'inset 1px #808080' : 'none'; 
      s.background=oskEnabled ? '#f7e7de' : 'white';
      s.display = 'block';     
      if(ui.initialized) ui.oskButton.style.display = 'block';
    }
    
    /**
     * Register all keymanweb and OSK events only after keymanweb is fully initialized
     *    
     **/      
    ui.registerEvents = function()
    {
      /**
       * Keyboard registration event handler
       *    
       * Set a timer to update the UI keyboard list on timeout after each keyboard is registered, 
       * thus updating only once when only if multiple keyboards are registered together
       */   
      keymanweb['addEventListener']('keyboardregistered', 
        function(p)
        {  
            ui.updateList = true;
            if(ui.updateTimer) clearTimeout(ui.updateTimer);
            ui.updateTimer = setTimeout(ui.updateKeyboardList,200);
        });       
      
      /**
       * Keyboard load event handler
       * 
       * Set the menu selector to the currently saved keyboard when a keyboard is loaded
       *    
       * Note: Cannot simply set it to the loaded keyboard, 
       *       as more than one language may be supported by that keyboard.
       */   
      keymanweb['addEventListener']('keyboardloaded',
        function(p) {
          var sk = keymanweb['getSavedKeyboard']().split(':');
          if(sk.length > 1) ui.updateMenu(sk[0],sk[1]);
        });
        
      /**
       * Keyboard change event handler
       *        
       * Update menu selection and control OSK display appropriately
       */
      keymanweb['addEventListener']('keyboardchange',  
        function(p)
        {
          // Update the keyboard selector whenever a keyboard is loaded  
          ui.updateMenu(p['internalName'],p['languageCode']);    
      
          // (Conditionally) display the OSK button, and set the style 
          ui.addButtonOSK();        
        });
                      
      /**
       * Show OSK event handler: show or hide the OSK button (never display if a CJK keyboard)
       */                
      osk['addEventListener']('show',
        function(oskPosition)
        {          
          ui.addButtonOSK();    
          return oskPosition; 
        }); 

      /**
       * Hide OSK event handler
       */ 
      
      osk['addEventListener']('hide',
        function(hiddenByUser)
        {
          if(ui.initialized) ui.oskButtonState(false);
        }); 
    }
      
    /**
     * Function     _SelectorMouseDown
     * Scope        Private   
     * @param       {Object}     e      event
     * Description  Set KMW UI activation state on mouse click
     */    
    ui._SelectorMouseDown = function(e)
    { 
      if(keymanweb['activatingUI']) keymanweb['activatingUI'](1);
    }

    /**
     * Function     _SelectorMouseOver
     * Scope        Private
     * @param       {Object}    e       event
     * Description  Set KMW UI activation state on mouse over
     */    
    ui._SelectorMouseOver = function(e)
    {
      if(keymanweb['activatingUI']) keymanweb['activatingUI'](1);
    }

    /**
     * Function     _SelectorMouseOut
     * Scope        Private
     * @param       {Object}    e       event
     * Description Clear KMW UI activation state on mouse out
     */    
    ui._SelectorMouseOut = function(e)
    {
      if(keymanweb['activatingUI']) keymanweb['activatingUI'](0);
    }

    /**
     * Function     _SelectKeyboardChange
     * Scope        Private
     * @param       {Object}    e       event
     * Description  Change active keyboard in response to user selection event
     */    
    ui.SelectKeyboardChange = function(e)
    { 
      if(ui.KeyboardSelector.value != '-')
      {
        var i=ui.KeyboardSelector.selectedIndex;
        var t=ui.KeyboardSelector.options[i].value.split(':');
        keymanweb['setActiveKeyboard'](t[0],t[1]);
      }
      else
        keymanweb['setActiveKeyboard'](''); 
      
      //if(osk['show']) osk['show'](osk['isEnabled']()); handled by keyboard change event???
      keymanweb['focusLastActiveElement']();
      ui.selecting = true;
    }

    /**
     * Function     _SelectBlur
     * Scope        Private
     * @param       {Object}    e       event
     * Description  Ensure OSK is hidden when moving focus after reselecting a keyboard
     */    
    ui.SelectBlur = function(e)
    {
      if(!ui.selecting) keymanweb['focusLastActiveElement']();
      ui.selecting = false;                                   
    }
    
    /**
     * Function     ShowInterface
     * Scope        Private
     * @param       {number=}    Px    x-position for KMW window
     * @param       {number=}    Py    y-position for KMW window
     * Description  Display KMW window at specified position
     */    
    ui.ShowInterface = function(Px, Py)
    {
      if(!ui.initialized) return;
    
      var Ls = ui.outerDiv.style;   

      if(Px  &&  Py) {
        Ls.left = Px + 'px';
        Ls.top = Py + 'px';
      }
      Ls.display = 'block';
      
      ui.kbdIcon.style.left = ui.KeyboardSelector.offsetWidth + 24 + 'px';
      
      // (Conditionally) display the OSK button
      ui.addButtonOSK();
          
      // Set the language selection to the currently active keyboard, if listed
      ui.updateMenu(keymanweb['getActiveKeyboard'](),keymanweb['getActiveLanguage']());
    }
      
    /**
     * Function     HideInterface
     * Scope        Private
     * Description  Completely hide KMW window 
     */    
    ui.HideInterface = function()
    {
      if(!ui.initialized) return;
      
      ui.outerDiv.style.display = 'none';
    }


    /**
     * Function     addButtonOSK
     * Scope        Private
     * Description  Display the OSK button unless a CJK keyboard (or English)
     */            
    ui.addButtonOSK = function()
    {
      if(ui.oskButton != null)
      {
        if(keymanweb['isCJK']() || (ui.KeyboardSelector.selectedIndex==0)) 
        {
          ui.oskButton.style.display = 'none';
          ui.outerDiv.style.width = ui.KeyboardSelector.offsetWidth+30+'px';    
        } 
        else 
        {
          ui.oskButton.style.display = 'block';
          ui.oskButtonState(osk['isEnabled']());
          ui.outerDiv.style.width = ui.KeyboardSelector.offsetWidth+56+'px';
        }      
      }
    }
    //TODO:  had to expose properties of params - what does that do? (focus event doesn't normally include these properties?)  
    keymanweb['addEventListener']('controlfocused',
      function(params)
      {
        if(params['activeControl'] == null || params['activeControl']['_kmwAttachment'])
        {
          /*if(keymanweb._IsIEEditableIframe(Ltarg))
            Ltarg = Ltarg.ownerDocument.parentWindow.frameElement;
          else if(keymanweb.domManager._IsMozillaEditableIframe(Ltarg))
            Ltarg = Ltarg.defaultView.frameElement;*/
          if(ui.floatRight)   // I1296
            ui.ShowInterface(util['getAbsoluteX'](params.target) + params.target.offsetWidth + 1, util['getAbsoluteY'](params.target) + 1);
          else
            ui.ShowInterface(util['getAbsoluteX'](params.target), util['getAbsoluteY'](params.target) 
              - parseInt(util['getStyleValue'](ui.outerDiv,'height'),10) - 2);
        }
        return true;
      });               

    keymanweb['addEventListener']('controlblurred',
      function(params)
      {  
        if(!params['event']) return true;   // I2404 - Manage IE events in IFRAMEs
    
        if(!params['isActivating']) ui.HideInterface();

        return true;
      });
    
    /**
     * Function     _Resize
     * Scope        Private   
     * @param       {Object}     e      event object
     * @return      {boolean}      
     * Description  Display KMW OSK at specified position (returns nothing) 
     */    
    ui._Resize = function(e)
    {
      if(ui.outerDiv.style.display =='block') 
      {
        var elem = keymanweb['getLastActiveElement']();
        if(ui.floatRight)   // I1296
          ui.ShowInterface(util['getAbsoluteX'](elem) + elem.offsetWidth + 1, util['getAbsoluteY'](elem) + 1);
        else
          ui.ShowInterface(util['getAbsoluteX'](elem) + 1, util['getAbsoluteY'](elem) + elem.offsetHeight + 1);
      }
      return true;
    }  
    
    if(window.addEventListener)
      window.addEventListener('resize', ui._Resize, false);

    // Initialize after KMW is fully initialized, if UI already loaded
    keymanweb['addEventListener']('loaduserinterface',ui.Initialize);
    
    // but also call initialization when script loaded, which is after KMW initialization for asynchronous script loading
    ui.Initialize();
    
  } catch(err){}

  // Do not wrap in an anonymous function - let the closure compiler do that, but
  // use try...catch to avoid script errors and only execute if a desktop browser
}