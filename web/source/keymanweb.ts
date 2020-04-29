// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwexthtml.ts" />
// Includes type definitions for basic KMW types.
/// <reference path="kmwtypedefs.ts" />

/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/

// If KMW is already initialized, the KMW script has been loaded more than once. We wish to prevent resetting the 
// KMW system, so we use the fact that 'initialized' is only 1 / true after all scripts are loaded for the initial
// load of KMW.
if(!window['keyman']['initialized']) { 

  // Continued KeymanWeb initialization.
  (function() 
  {

    // Declare KeymanWeb, OnScreen Keyboard and Util object variables
    var keymanweb=window['keyman'],util=keymanweb['util'];

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

    /*
     * The following code existed here as part of the original pre-conversion JavaScript source, performing some inline initialization.
     * Ideally, this will be refactored once proper object-orientation of the codebase within TypeScript is complete.
     */
    keymanweb.debugElement=null;
    var dbg=keymanweb.debug;
        
    keymanweb.delayedInit();

  //TODO: find all references to next three routines and disambiguate!!
    
    // Complete page initialization only after the page is fully loaded, including any embedded fonts
    // This avoids the need to use a timer to test for the fonts
    
    util.attachDOMEvent(window, 'load', keymanweb.domManager._WindowLoad,false);
    util.attachDOMEvent(window, 'unload', keymanweb.domManager._WindowUnload,false);  // added fourth argument (default value)       
    
    // *** I3319 Supplementary Plane modifications - end new code

    util.attachDOMEvent(document, 'keyup', keymanweb.hotkeyManager._Process, false);

    /**
     * Reset OSK shift states when entering or exiting the active element
     **/    
    function resetVKShift() {
      let keyman = com.keyman.singleton;
      if(!keyman.uiManager.isActivating && keyman.osk.vkbd) {
        keyman.core.keyboardProcessor._UpdateVKShift(null, 15, 0);  //this should be enabled !!!!! TODO
      }
    }

    // We need to track this handler, as it causes... interesting... interactions during testing in certain browsers.
    keymanweb['pageFocusHandler'] = resetVKShift;
    util.attachDOMEvent(window, 'focus', keymanweb['pageFocusHandler'], false);  // I775
    util.attachDOMEvent(window, 'blur', keymanweb['pageFocusHandler'], false);   // I775
    
    // Initialize supplementary plane string extensions
    String.kmwEnableSupplementaryPlane(false);    

  })();
}