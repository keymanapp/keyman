// Contains event management for mobile device rotation events.
/// <reference path="kmwrotation.ts" />

/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/

// If KMW is already initialized, the KMW script has been loaded more than once. We wish to prevent resetting the 
// KMW system, so we use the fact that 'initialized' is only 1 / true after all scripts are loaded for the initial
// load of KMW.
if(!window['keyman']['initialized']) { 
  /*****************************************/
  /*                                       */
  /*   On-Screen (Visual) Keyboard Code    */
  /*                                       */
  /*****************************************/
  (function() {
    // Declare KeymanWeb object
    var keymanweb=window['keyman'],osk=keymanweb['osk'],util=keymanweb['util'],device=util.device;
    var dbg=keymanweb.debug;
    var dom = com.keyman.dom;

    // Force full initialization
    keymanweb.isEmbedded = false;

    /**
     * Set default device options
     * @param {Object}  opt device options object
     */
    keymanweb.setDefaultDeviceOptions=function(opt) {
      // Element attachment type
      if(opt['attachType'] == '') opt['attachType'] = (device.touchable ? 'manual' : 'auto');
    }

  /**
     * Customized wait display
     * 
     * @param   {string|boolean}   s       displayed text (or false)
     */
    util.wait = function(s) {
      // Keyboards loaded with page are initialized before the page is ready,
      // so cannot use the wait indicater (and don't need it, anyway)
      // Do not display if a blocking cloud server error has occurred (to prevent multiple errors)
      var bg=this.waiting;
      if(typeof(bg) == 'undefined' || bg == null || keymanweb.warned) {
        return;
      }
      
      var nn=bg.firstChild.childNodes;
      if(s) {
        bg.pending=true;
        window.setTimeout(function() {
            if(bg.pending) {
              window.scrollTo(0,0);
              nn[0].style.display='none';
              nn[1].className='kmw-wait-text'; nn[1].innerHTML=s;
              nn[2].style.display='block';
              bg.style.display='block';
            }
          },1000);
      } else {
        if(bg.pending) {
          nn[1].innerHTML='';
          bg.pending=false; bg.style.display='none';
        }
      }
    }
      
    // Get default style sheet path
    keymanweb.getStyleSheetPath=function(ssName) {
      var ssPath = util['getOption']('resources')+'osk/'+ssName;
      return ssPath;
    }

    /**
     * Get keyboard path (relative or absolute)
     * KeymanWeb 2 revised keyboard location specification:
     *  (a) absolute URL (includes ':') - load from specified URL
     *  (b) relative URL (starts with /, ./, ../) - load with respect to current page
     *  (c) filename only (anything else) - prepend keyboards option to URL 
     *      (e.g. default keyboards option will be set by Cloud)
     *           
     * @param {string}  Lfilename  keyboard file name with optional prefix                     
     */   
    keymanweb.getKeyboardPath=function(Lfilename) {           
      var rx=RegExp('^(([\\.]/)|([\\.][\\.]/)|(/))|(:)');   
      return (rx.test(Lfilename) ? '' : keymanweb.options['keyboards']) + Lfilename;
    }

    /**
     * Align input fields (should not be needed with KMEI, KMEA), making them visible if previously hidden.
     * 
     *  @param  {object}   eleList    A list of specific elements to align.  If nil, selects all elements.
     * 
     **/
    keymanweb.alignInputs = function(eleList: HTMLElement[]) {
      if(device.touchable) {
        var domManager = keymanweb.domManager;
        var processList: HTMLElement[] = [];

        if(eleList) {
          // Did the user specify the actual element or the touch-alias?
          eleList.forEach(function(element: HTMLElement){
            if(element.base) {
              // It's a touch-alias element, which is what we wish to perform alignment on.
              processList.push(element);
            } else {
              // This retrieves an element's touch-alias, should it exist.
              let touchAlias = element['kmw_ip'] as HTMLDivElement;
              if(touchAlias) {
                processList.push(element['kmw_ip']);
              }
            }
          });
        } else {
          processList = domManager.inputList;
        }

        // Supported by IE 9 and all modern browsers.
        processList.forEach(function(element: HTMLElement) {
          if(dom.Utils.instanceof(element, "TouchAliasElement")) {
            (element as com.keyman.dom.TouchAliasElement).updateInput();
          }
          element.style.visibility = 'visible';
          if(element.base.textContent.length > 0) {
            element.base.style.visibility = 'hidden';
          }
        })      
      }
    }

    /**
     * Programatically hides all input fields with underlying elements.  Restore with .alignInputs.    
     * 
     *  @param  {boolean}   align    align and make visible, else hide
     * 
     **/
    keymanweb.hideInputs = function() {
      var domManager = keymanweb.domManager;
      if(device.touchable) {
        for(var i=0; i<domManager.inputList.length; i++) {
          domManager.inputList[i].style.visibility='hidden';
          domManager.inputList[i].base.style.visibility='visible';
        }        
      }
    }

    /**
     * Test if caret position is determined from the active element, or 
     * from the synthesized overlay element (touch devices)
     * 
     * @return  {boolean}
     **/          
    keymanweb.isPositionSynthesized = function() {
      return device.touchable;
    }

    /**
     * Use rotation events to adjust OSK and input element positions and scaling as necessary
     */     
    keymanweb.handleRotationEvents=function() {
      var rotationManager = new com.keyman.RotationManager(keymanweb);

      rotationManager.init();
    }

    /**
     * Possible way to detect the start of a rotation and hide the OSK before it is adjusted in size
     * 
     *  @param  {Object}    e   accelerometer rotation event      
     *      
    keymanweb.testRotation = function(e)
    {
      var r=e.rotationRate;
      if(typeof(r) != 'undefined')
      {
        dbg(r.alpha+' '+r.beta+' '+r.gamma);
      }
    }
    */ 
  })();
}