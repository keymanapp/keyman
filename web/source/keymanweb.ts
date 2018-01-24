// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwexthtml.ts" />
// Includes KMW string extension declarations.
/// <reference path="kmwstring.ts" />
// Includes type definitions for basic KMW types.
/// <reference path="kmwtypedefs.ts" />

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
    keymanweb.delayedInit();

    // I732 START - Support for European underlying keyboards #1
    if(typeof(window['KeymanWeb_BaseLayout']) !== 'undefined') 
      osk._BaseLayout = window['KeymanWeb_BaseLayout'];
    else
      osk._BaseLayout = 'us';    
    
    
    keymanweb._BrowserIsSafari = (navigator.userAgent.indexOf('AppleWebKit') >= 0);  // I732 END - Support for European underlying keyboards #1 
    
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
    keymanweb['activatingUI'] = function(state) {
      keymanweb._IsActivatingKeymanWebUI = (state ? 1 : 0);
    }      

  //TODO: add more complete description of what ControlFocus really does

    /**
     * Function     doUnloadOSK
     * Scope        Private
     * @return      {boolean}   
     * Description  Execute external (UI) code if any needed after unloading OSK (probably not required)
     */       
    keymanweb.doUnloadOSK = function() {
      var p={};
      return util.callEvent('kmw.unloadosk',p);
    }

    /**
     * Function     doLoadUI
     * Scope        Private
     * @return      {boolean}   
     * Description  Execute UI initialization code after loading the UI
     */       
    keymanweb.doLoadUI = function() {
      var p={};
      return util.callEvent('kmw.loaduserinterface',p);
    }

    /**
     * Function     doUnloadUI
     * Scope        Private
     * @return      {boolean}   
     * Description  Execute UI cleanup code before unloading the UI (may not be required?)
     */       
    keymanweb.doUnloadUI = function() {
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
    
  //TODO: check return of _KeyUp - what happens if returning true or false ?? what if null returned?       

  //TODO: find all references to next three routines and disambiguate!!
    
    // Complete page initialization only after the page is fully loaded, including any embedded fonts
    // This avoids the need to use a timer to test for the fonts
    
    util.attachDOMEvent(window, 'load', keyman.domManager._WindowLoad,false);
    util.attachDOMEvent(window, 'unload', keymanweb.domManager._WindowUnload,false);  // added fourth argument (default value)       
        
    /**
     * Test if caret position is determined from the active element, or 
     * from the synthesized overlay element (touch devices)
     * 
     * @return  {boolean}
     **/          
    keymanweb.isPositionSynthesized = function() {
      return device.touchable;
    }
    
    // *** I3319 Supplementary Plane modifications - end new code
    
    /**
     * Reset OSK shift states when entering or exiting the active element
     **/    
    keymanweb._ResetVKShift = function() {
      if(!keymanweb._IsActivatingKeymanWebUI) 
      {
        if(osk._UpdateVKShift) osk._UpdateVKShift(null,15,0);  //this should be enabled !!!!! TODO
      }
    }

    util.attachDOMEvent(document, 'keyup', keymanweb.hotkeyProcessor._Process, false);  

    util.attachDOMEvent(window, 'focus', keymanweb._ResetVKShift,false);  // I775
    util.attachDOMEvent(window, 'blur', keymanweb._ResetVKShift,false);   // I775
    
    // Initialize supplementary plane string extensions
    String.kmwEnableSupplementaryPlane(false);    

  })();
}

