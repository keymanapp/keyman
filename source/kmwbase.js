/***
   KeymanWeb 2.0
   Copyright 2014 Tavultesoft Pty Ltd

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
***/

 
/**  
 * Base code: Declare tavultesoft, major component namespaces and instances, utility functions 
 */  

/** @define {number} */
var __BUILD__ = 300;

(function() 
{
  // Define exposed base object
  var tavultesoft = window['tavultesoft'] = {}; 

  var keymanweb = tavultesoft['keymanweb'] = {
    _TitleElement: null,      // I1972 - KeymanWeb Titlebar should not be a link
    _Enabled: 1,              // Is KeymanWeb running?
    _IE: 0,                   // browser version identification
    legacy: 0,               // set true for IE 3,4,5 (I2186 - multiple IE tweaks needed)
    _IsActivatingKeymanWebUI: 0,    // ActivatingKeymanWebUI - is the KeymanWeb DIV in process of being clicked on?
    _JustActivatedKeymanWebUI: 0,   // JustActivatedKeymanWebUI - focussing back to control after KeymanWeb UI interaction  
    _IgnoreNextSelChange: 0,  // when a visual keyboard key is mouse-down, ignore the next sel change because this stuffs up our history  
    _Selection: null,
    _SelectionControl: null,
    _KeyboardStubs: [],       // KeyboardStubs - array of available keyboards
    dfltStub: null,           // First keyboard stub loaded - default for touch-screen devices, ignored on desktops
    _Keyboards: [],           // Keyboards - array of loaded keyboards
    _ActiveKeyboard: null,    // ActiveKeyboard - points to active keyboard in Keyboards array
    _ActiveStub: null,        // ActiveStub - points to active stub in KeyboardStubs  
    _LoadingInternalName: null,
    _AnyIndices: [],          // AnyIndex - array of any/index match indices
    _DeadKeys: [],            // DeadKeys - array of matched deadkeys
    _ActiveControl: null,     // Currently active control in _Controls array
    _Controls: [],            // Array of controls with specific states: .Control, .Enabled, .DefaultInternalName  
    _AttachedElements: [],    // I1596 - attach to controls dynamically
    _ActiveElement: null,     // Currently active (focused) element  I3363 (Build 301)
    _LastActiveElement: null, // LastElem - Last active element
    _DfltStyle: '',           // Default styles
    _MasterDocument: null,    // Document with controller (to allow iframes to distinguish local/master control)
    _HotKeys: [],             // Array of document-level hotkey objects
    focusTimer: null,         // Timer to manage loss of focus to unmapped input
    focusing: false,          // Flag to manage movement of focus
    resizing: false,          // Flag to control resize events when resetting viewport parameters
    sortedInputs: [],         // List of all INPUT and TEXTAREA elements ordered top to bottom, left to right
    inputList: [],            // List of simulated input divisions for touch-devices   I3363 (Build 301)
    languageList: null,       // List of keyboard languages available for KeymanCloud
    languagesPending: [],     // Array of languages waiting to be registered
    deferredStubs: [],        // Array of pending keyboard stubs from addKeyboard(), to register after initialization
    deferredKRS: [],          // Array of pending keyboard stubs from KRS, to register afterf initialization
    deferredKR: [],           // Array of pending keyboards, to be installed at end of initialization
    loadTimer:null,           // Keyboard loading timer, to manage load failure
    waiting:null,             // Element displayed during keyboard load time
    warned: false,            // Warning flag (to prevent multiple warnings)
    baseFont: 'sans-serif',   // Default font for mapped input elements
    appliedFont: '',          // Chain of fonts to be applied to mapped input elements
    embeddedFonts: [],        // Array of currently embedded fonts
    fontCheckTimer: null,     // Timer for testing loading of embedded fonts
    srcPath: '',              // Path to folder containing executing keymanweb script
    rootPath: '',             // Path to server root
    mustReloadKeyboard: false,// Force keyboard refreshing even if already loaded
    fullInitialization: true  // Force full page initialization unless overridden
  };

  keymanweb['initialized'] = 0;
  keymanweb['build'] = __BUILD__;
  keymanweb['version'] = '2.0';
  keymanweb['helpURL'] = 'http://help.keyman.com/go'; 

  // Expose (old) KeymanWeb object for use by compiled keyboards (replaces legacy object in earlier versions)
  window['KeymanWeb'] = keymanweb;
  
  // Define public OSK, user interface and utility function objects 
  var util = keymanweb['util'] = {};
  var osk = keymanweb['osk'] = {ready:false};
  var ui = keymanweb['ui'] = {};

  // Stub functions (defined later in code only if required)
  keymanweb.setDefaultDeviceOptions = function(opt){}     
  keymanweb.getStyleSheetPath = function(s){return s;}
  keymanweb.getKeyboardPath=function(f){return f;}  
  keymanweb.KC_ = function(n, ln, Pelem){return '';} 
  keymanweb.handleRotationEvents = function(){}
  keymanweb.alignInputs = function(b){}
  keymanweb.refreshElementContent = null; 
  
  osk.highlightSubKeys = function(k,x,y){}
  osk.createKeyTip = function(){}
  osk.optionKey = function(e,keyName,keyDown){}
  osk.showKeyTip = function(key,on){}  
  osk.waitForFonts = function(kfd,ofd){return true;}
 
  // Define private options object with empty members
  keymanweb.options = {
    'root':'',
    'resources':'',
    'keyboards':'',
    'fonts':'',
    'attachType':'',
    'ui':null
    };

  // Generalized component event registration
  util.events = [];  
  util.currentEvents = [];

  /**
   * Determine path and protocol of executing script
   *    
   * This can only be done during load when the active script will be the  
   * last script loaded.  Otherwise the script must be identified by name.
  */  
  var scripts = document.getElementsByTagName('script');
  var ss = scripts[scripts.length-1].src,
    sPath = ss.substr(0,ss.lastIndexOf('/')+1);  
  keymanweb.srcPath = sPath;
  keymanweb.rootPath = sPath.replace(/(https?:\/\/)([^\/]*)(.*)/,'$1$2/');    
  keymanweb.protocol = sPath.replace(/(.{3,5}:)(.*)/,'$1');                      
/*  

 osk.positionChanged = function(newPosition)
  {
    return util.callEvent('osk.positionChanged', newPosition);
  }
  
  osk['setPosition'] = function(newPosition)
  {
    divOsk.left = newPosition.left;
    ...
    osk.positionChanged({left: divOsk.left, top: divOsk.top, ...});
  }
  
  ui.oskPositionChanged = function(newPosition)
  {
    // do something with the osk
  }
  
  ui.init = function()
  {
    osk.addEventListener('positionChanged', ui.oskPositionChanged);
  }
  

*/

  /**
   * Function    addEventListener
   * Scope       Private
   * @param      {string}     event     name of event prefixed by module, e.g. osk.touchmove
   * @param      {function(Object)}   func      event handler
   * @return     {boolean}         
   * Description Add (or replace) an event listener for this component 
   */    
  util.addEventListener = function(event, func)
  {
    util.removeEventListener(event, func);
    util.events[event].push(func);
    return true;
  }    
  
  /**
   * Function    removeEventListener
   * Scope       Private
   * @param      {string}     event     name of event prefixed by module, e.g. osk.touchmove
   * @param      {function(Object)}   func      event handler
   * @return     {boolean}         
   * Description Remove the specified function from the listeners for this event 
   */    
  util.removeEventListener = function(event, func)
  {
    if(typeof util.events[event] == 'undefined') util.events[event] = [];
    for(var i=0; i<util.events[event].length; i++)
    {
      if(util.events[event][i] == func)
      { 
        util.events[event].splice(i, 1);
        return true;
      }
    }
    return false;
  }
  
  /**
   * Function    callEvent
   * Scope       Private
   * @param      {string}     event     name of event prefixed by module, e.g. osk.touchmove
   * @param      {Array}      params    parameter array for function
   * @return     {boolean}         
   * Description Invoke an event using any function with up to four arguments 
   */    
  util.callEvent = function(event,params)
  {
    if(typeof util.events[event] == 'undefined') return true;
    if(typeof util.currentEvents[event] != 'undefined') return false;
    util.currentEvents.push(event);    
    for(var i=0; i<util.events[event].length; i++)
    {
      var func=util.events[event][i],result=false;
      try { result=func(params); }
      catch(strExcept) { result=false; }//don't know whether to use true or false here      
      if(!result) { util.currentEvents.pop(); return false; }
    }    
    util.currentEvents.pop();
    return true;
  }
  
  /**
   * Function     attachDOMEvent: Note for most browsers, adds an event to a chain, doesn't stop existing events  
   * Scope        Public
   * @param       {Object}    Pelem       Element to which event is being attached
   * @param       {string}    Peventname  Name of event without 'on' prefix
   * @param       {function(Object)}  Phandler    Event handler for event
   * @param       {boolean=}  PuseCapture True only if event to be handled on way to target element      
   * Description  Attaches event handler to element DOM event
   */  
  util['attachDOMEvent'] = util.attachDOMEvent = function(Pelem, Peventname, Phandler, PuseCapture) {
    util.detachDOMEvent(Pelem, Peventname, Phandler, PuseCapture);
    if(Pelem.attachEvent) {
      // IE
      Pelem.attachEvent('on'+Peventname, Phandler);
    } else if(Pelem.addEventListener) {
      // Firefox + standards
      Pelem.addEventListener(Peventname, Phandler, PuseCapture?true:false);
    }
  }    
 
  /**
   * Function     detachDOMEvent
   * Scope        Public
   * @param       {Object}    Pelem       Element from which event is being detached
   * @param       {string}    Peventname  Name of event without 'on' prefix
   * @param       {function(Object)}  Phandler    Event handler for event
   * @param       {boolean=}  PuseCapture True if event was being handled on way to target element      
   * Description Detaches event handler from element [to prevent memory leaks]
   */  
  util['detachDOMEvent'] = util.detachDOMEvent = function(Pelem, Peventname, Phandler, PuseCapture) {
    if(Pelem.detachEvent) {
      Pelem.detachEvent('on'+Peventname, Phandler);
    } else if(Pelem.removeEventListener) {
      Pelem.removeEventListener(Peventname, Phandler, PuseCapture);      
    }
  }    
 
  /**
   * Function     getOption
   * Scope        Public
   * @param       {string}    option      Name of option
   * @param       {*=}        dflt        Default value of option
   * @return      {*}               
   * Description  Returns value of named option
   */  
  util['getOption'] = util.getOption = function(optionName,dflt)
  {
    if(optionName in keymanweb.options) return keymanweb.options[optionName]; 
    else if(arguments.length > 1) return dflt;
    else return '';
  }

  /**
   * Function     setOption
   * Scope        Public
   * @param       {string}    option      Name of option
   * @param       {*=}        value       Value of option
   * Description  Sets value of named option
   */  
  util['setOption'] = util.setOption = function(optionName,value)
  {
    keymanweb.options[optionName] = value;
  }
  
  /**
   * Function     getAbsoluteX
   * Scope        Public
   * @param       {Object}    Pobj        HTML element
   * @return      {number}               
   * Description  Returns x-coordinate of Pobj element absolute position with respect to page
   */  
  util['getAbsoluteX'] = util._GetAbsoluteX = function(Pobj)  // I1476 - Handle SELECT overlapping END

  {
    if(!Pobj) return 0;
    
    var Lcurleft = Pobj.offsetLeft ? Pobj.offsetLeft : 0, Lobj = Pobj;   	// I2404 - Support for IFRAMEs
    if (Lobj.offsetParent)
    {
      while (Lobj.offsetParent)
      {
        Lobj = Lobj.offsetParent;
        Lcurleft += Lobj.offsetLeft;
      }
    }
    // Correct position if element is within a frame (but not if the controller is in document within that frame)
    if(Lobj && Lobj.ownerDocument && (Pobj.ownerDocument != keymanweb._MasterDocument)) Lobj=Lobj.ownerDocument;   // I2404 - Support for IFRAMEs    
    
    // The following two lines are old code and may or may not still be needed - possibly should be conditioned similalry to above    
    if(Lobj && Lobj.parentWindow && Lobj.parentWindow.frameElement) return Lcurleft + util._GetAbsoluteX(Lobj.parentWindow.frameElement) - Lobj.documentElement.scrollLeft;
    if(Lobj && Lobj.defaultView && Lobj.defaultView.frameElement) return Lcurleft + util._GetAbsoluteX(Lobj.defaultView.frameElement) - Lobj.documentElement.scrollLeft;
    return Lcurleft;
  }

  /**
   * Function     getAbsoluteY
   * Scope        Public
   * @param       {Object}    Pobj        HTML element
   * @return      {number}               
   * Description  Returns y-coordinate of Pobj element absolute position with respect to page
   */  
  util['getAbsoluteY'] = util._GetAbsoluteY = function(Pobj)
  {
    if(!Pobj) return 0;
    var Lcurtop = Pobj.offsetTop ? Pobj.offsetTop : 0, Lobj = Pobj;  // I2404 - Support for IFRAMEs
    if (Lobj.offsetParent)
    {
      while (Lobj.offsetParent)
      {
        Lobj = Lobj.offsetParent;
        Lcurtop += Lobj.offsetTop;
      }
    }
    // Correct position if element is within a frame (but not if the controller is in document within that frame)
    if(Lobj && Lobj.ownerDocument && (Pobj.ownerDocument != keymanweb._MasterDocument)) Lobj=Lobj.ownerDocument;   // I2404 - Support for IFRAMEs    
    
    // The following two lines are old code and may or may not still be needed - possibly should be conditioned similalry to above    
    if(Lobj && Lobj.parentWindow && Lobj.parentWindow.frameElement) return Lcurtop + util._GetAbsoluteY(Lobj.parentWindow.frameElement) - Lobj.documentElement.scrollTop;
    if(Lobj && Lobj.defaultView && Lobj.defaultView.frameElement) return Lcurtop + util._GetAbsoluteY(Lobj.defaultView.frameElement) - Lobj.documentElement.scrollTop;
    return Lcurtop;
  }

  /**
   * Function     getAbsolute
   * Scope        Public
   * @param       {Object}    Pobj        HTML element
   * @return      {Object.<string,number>}               
   * Description  Returns absolute position of Pobj element with respect to page
   */  
  util['getAbsolute'] = util._GetAbsolute = function(Pobj)
  {
    var p={};
    p['x'] = util._GetAbsoluteX(Pobj);
    p['y'] = util._GetAbsoluteY(Pobj);
    return p;
  }
  
  /**
   * Select start handler (to replace multiple inline handlers) (Build 360)  
   */
  util.selectStartHandler = function() 
  {
    return false;
  }

  /**
   * Default mouse down event handler (to replace multiple inline handlers) (Build 360)   
   */
  util.mouseDownPreventDefaultHandler = function(e) 
  {
    if(e) e.preventDefault();
  }
  
  util.mouseDownPreventDefaultHandler = function(e) {
    if(e) e.preventDefault();
  }
  
  util['createElement'] =  util._CreateElement = function(nodeName) 
  { 
    var e = document.createElement(nodeName);
    // Make element unselectable (Internet Explorer)
    if (typeof e.onselectstart != 'undefined') //IE route
	  {
      e.unSelectable='on'; e.onselectstart=util.selectStartHandler; // Build 360
    }
    // And for well-behaved browsers (may also work for IE9+, but not necessary)
    else  
    {
      e.style.MozUserSelect="none"; e.style.KhtmlUserSelect="none";  
      e.style.UserSelect="none"; e.style.WebkitUserSelect="none";
    }    
    return e;
  }
              
  /**
   * Function     getIEVersion
   * Scope        Public
   * @return      {number}               
   * Description  Return IE version number (or 999 if browser not IE)
   */       
  util['getIEVersion'] = util._GetIEVersion = function()
  {                                  
    var n,agent='';
    
    if('userAgent' in navigator) agent=navigator.userAgent;
    
    // Test first for old versions
    if('selection' in document)          // only defined for IE and not for IE 11!!!
    {                         
      var appVer=navigator.appVersion;        
      n=appVer.indexOf('MSIE ');
      if(n >= 0) 
      {
        // Check for quirks mode page, always return 6 if so
        if(document.compatMode == 'BackCompat') return 6;
        
        appVer=appVer.substr(n+5);
        n=appVer.indexOf('.');
        if(n > 0) return parseInt(appVer.substr(0,n),10);
      }              
    }
       
    // Finally test for IE 11 (and later?)
    n=agent.indexOf('Trident/');
    if(n < 0) return 999;  
    
    agent=agent.substr(n+8);
    n=agent.indexOf('.');
    if(n > 0) return parseInt(agent.substr(0,n),10)+4;
  
    return 999;
  }

  /**
   * Get device horizontal DPI for touch devices, to set actual size of active regions
   * Note that the actual physical DPI may be somewhat different.
   * 
   * @return      {number}               
   */       
  util.getDPI = function()
  {
    var t=document.createElement('DIV'),s=t.style,dpi=96;
    if(document.readyState !== 'complete') return dpi;
    
    t.id='calculateDPI';
    s.position='absolute'; s.display='block';s.visibility='hidden';
    s.left='10px'; s.top='10px'; s.width='1in'; s.height='10px';
    document.body.appendChild(t);
    dpi=(typeof window.devicePixelRatio == 'undefined') ? t.offsetWidth : t.offsetWidth * window.devicePixelRatio;
    document.body.removeChild(t);
    return dpi;    
  }

  /**
   * Get browser-independent computed style value for element
   * 
   * @param       {Element}     e             HTML element
   * @param       {string}      s             CSS style name 
   * @return      {*}               
   */       
  util.getStyleValue = function(e,s)
  { 
    // Build 349: error trap added, since on iOS, getPropertyValue may fail 
    // and crash in some cases, possibly if passed a text node 
    try  
    {
      if(e && (typeof(window.getComputedStyle) != 'undefined'))
          return window.getComputedStyle(e,'').getPropertyValue(s);
      else if(e && (typeof(e.currentStyle) != 'undefined'))    //IE 8 and earlier
        return e.currentStyle[s];
  }    
    catch(ex){}
    
    // Return empty string if unable to get style value
    return '';
  }    

  /**
   * Get browser-independent computed style integer value for element  (Build 349)
   * 
   * @param       {Element}     e             HTML element
   * @param       {string}      s             CSS style name 
   * @param       {number=}     d             default value if NaN   
   * @return      {number}                    integer value of style
   */       
  util.getStyleInt = function(e,s,d)
  {
    var x=parseInt(util.getStyleValue(e,s),10);
    if(!isNaN(x)) return x;
    
    // Return the default value if numeric, else 0 
    if(typeof(d) == 'number') return d; else return 0;
  }
    
  /**
   * Set device parameters according to platform 
   *    TODO: put into separate function (?) 
   */
  var device=util.device={
    touchable:!!('ontouchstart' in window),OS:'',
    formFactor:'desktop',
    dyPortrait:0,
    dyLandscape:0,
    version:'0',
    orientation:window.orientation
  };
  
  if(navigator && navigator.userAgent)
  {
    var agent=navigator.userAgent;
    if(agent.indexOf('iPad') >= 0) 
    {
      device.OS='iOS';device.formFactor='tablet';
      device.dyPortrait=device.dyLandscape=0;
    }
    if(agent.indexOf('iPhone') >= 0)
    { 
      device.OS='iOS';device.formFactor='phone';
      device.dyPortrait=device.dyLandscape=25;
    }
    if(agent.indexOf('Android') >= 0) 
    {
      device.OS='Android';device.formFactor='phone';    // form factor may be redefined on initialization
      device.dyPortrait=75; device.dyLandscape=25;
      try
      {
        var rx=new RegExp("(?:Android\\s+)(\\d+\\.\\d+\\.\\d+)");
        device.version=agent.match(rx)[1];
      } catch(ex){}
      
    }  
    if(agent.indexOf('Windows NT') >= 0)
    {
      device.OS='Windows';
      if(agent.indexOf('Touch') >= 0) device.formFactor='phone';   // will be redefined as tablet if resolution high enough
      
      // Windows Phone and Tablet PC
      if(typeof navigator.msMaxTouchPoints == 'number' && navigator.msMaxTouchPoints > 0) device.touchable=true; 
    } 
  }  
  // var sxx=device.formFactor;
  // Check and possibly revise form factor according to actual screen size (adjusted for Galaxy S, maybe OK generally?)
  if(device.formFactor == 'tablet' && Math.min(screen.width,screen.height) < 400) device.formFactor='phone';
  if(device.formFactor == 'phone'  && Math.max(screen.width,screen.height) > 720) device.formFactor='tablet';
  //                           alert(sxx+'->'+device.formFactor);
  // Check for phony iOS devices (Win32 test excludes Chrome touch emulation on Windows)!
  if(device.OS == 'iOS' && !('ongesturestart' in window) && navigator.platform != 'Win32') device.OS='Android';
 
  // Determine application or browser
  device.browser='web';
  if(util._GetIEVersion() < 999) device.browser='ie';
  else
  {
    if(device.OS == 'iOS' || device.OS.toLowerCase() == 'macosx') device.browser='safari';    
    var bMatch=/Firefox|Chrome|OPR/;
    if(bMatch.test(navigator.userAgent))
    {
      if((navigator.userAgent.indexOf('Firefox') >= 0) && ('onmozorientationchange' in screen)) 
        device.browser='firefox';
      else if(navigator.userAgent.indexOf('OPR') >= 0) 
        device.browser='opera';
      else if(navigator.userAgent.indexOf('Chrome') >= 0) 
        device.browser='chrome';
    } 
  }
 
  /* DEBUG: Force touch device   (Build 360)
  
  device.touchable = true;
  device.browser = 'safari';
  device.formFactor = 'tablet';
  device.OS = 'iOS';
  
   END DEBUG */
                       
 
                       
  /**
   * Expose the touchable state for UIs - will disable external UIs entirely
   **/      
  util['isTouchDevice'] = function()
  {
    return device.touchable;
  }
  
  /**
   * Get orientation of tablet or phone  display
   *    
   * @return      {boolean}               
   */       
  util.portraitView = function()	// new for I3363 (Build 301)
  {
    return !util.landscapeView();
  }
  
  /**
   * Get orientation of tablet or phone  display
   *    
   * @return      {boolean}               
   */       
  util.landscapeView = function()	// new for I3363 (Build 301)
  { 
    // Assume portrait mode if orientation undefined
    if(typeof(window.orientation) == 'undefined') return false;
    
    // Else landscape for +/-90, portrait for 0, +/-180   
    return (Math.abs(window.orientation/90) == 1); 
  }
  
  /**
   * Get viewport scale factor for this document
   *    
   * @return      {number}               
   */       
  util.getViewportScale = function() 
  { 
    // This can sometimes fail with some browsers if called before document defined,
    // so catch the exception
    try {       
      // Get viewport width 
      var viewportWidth = document.documentElement.clientWidth; 
  
      // Return a default value if screen width is greater than the viewport width (not fullscreen). 
      if(screen.width > viewportWidth) return 1; 
      
      // Get the orientation corrected screen width 
      var screenWidth = screen.width; 
      if(util.landscapeView()) 
      {                     
        // Take larger of the two dimensions 
        if(screen.width < screen.height) screenWidth = screen.height; 
      } 
      else 
      {                               
        // Take smaller of the two dimensions 
        if(screen.width > screen.height) screenWidth = screen.height; 
      } 
      // Calculate viewport scale 
      return Math.round(100*screenWidth / window.innerWidth)/100; 
      }
    catch(ex) {
      return 1;
      }  
  }
  
  /**
   * Return height of URL bar on mobile devices, if visible
   * TODO: This does not seem to be right, so is not currently used   
   *    
   * @return      {number}               
   */       
  util.barHeight = function()
  {
    var dy=0;
    if(device.formFactor == 'phone')
    {
      dy=screen.height/2-window.innerHeight-(util.landscapeView()?device.dyLandscape:device.dyPortrait);
    }
    return dy;    
  }
  
  /**
   * Function     _EncodeEntities
   * Scope        Private
   * @param       {string}      P_txt         string to be encoded
   * @return      {string}                    encoded (html-safe) string               
   * Description Encode angle brackets and ampersand in text string
   */       
  util._EncodeEntities = function(P_txt)
  {
    return P_txt.replace('&','&amp;').replace('<','&lt;').replace('>','&gt;');  // I1452 part 2
  }  


  /**
   * Function     createShim
   * Scope        Public
   * @return      {Object}      IFRAME element           
   * Description  Create an IFRAME element to go between KMW and drop down (to fix IE6 bug)
   */    
  util['createShim'] = util.createShim = function()     // I1476 - Handle SELECT overlapping BEGIN
  {
    var e = util._CreateElement('IFRAME');
    e.src = '';
    e.style.display = 'none';
    e.style.position = 'absolute';
    e.style.filter = 'progid:DXImageTransform.Microsoft.Alpha(style=0,opacity=0)';
    e.frameBorder = '0';
    e.scrolling = 'no';
    return e;
  }

  // I1476 - Handle SELECT overlapping BEGIN
  
  /**
   * Function     showShim
   * Scope        Public
   * @param       {Object}      Pvkbd         Visual keyboard DIV element 
   * @param       {Object}      Pframe        IFRAME shim element
   * @param       {Object}      Phelp         OSK Help DIV element               
   * Description  Display iFrame under OSK at its currently defined position, to allow OSK to overlap SELECT elements (IE6 fix)  
   */    
  util['showShim'] = util.showShim = function(Pvkbd,Pframe,Phelp)     
  {
    if(Pframe)
      try
      {
        Pframe.style.left = util._GetAbsoluteX(Pvkbd)+'px';
        Pframe.style.top = util._GetAbsoluteY(Pvkbd)+'px';
        if(Phelp)
        {
          Pframe.style.width = (util._GetAbsoluteX(Phelp)-util._GetAbsoluteX(Pvkbd)+Phelp.offsetWidth)+"px";
          Pframe.style.height = (util._GetAbsoluteY(Phelp)-util._GetAbsoluteY(Pvkbd)+Phelp.offsetHeight)+"px";
        }
        else
        {
          Pframe.style.width = Pvkbd.offsetWidth+"px";
          Pframe.style.height = Pvkbd.offsetHeight+"px";
        }
        Pframe.style.zindex = '9999';
        Pframe.style.display = 'block';
      } catch (Lerr) {} 
  }

  /**
   * Function     hideShim
   * Scope        Public
   * @param       {Object}      Pframe        IFRAME shim element
   * Description  Hide iFrame shim containing OSK 
   */    
  util['hideShim'] = function(Pframe)
  {
    try {
      if(Pframe) Pframe.style.display = 'none';
    } catch (err) {}      
  }
  // I1476 - Handle SELECT overlapping END

  /**
   * Function     rgba
   * Scope        Public
   * @param       {Object}      s           element style object
   * @param       {number}      r           red value, 0-255
   * @param       {number}      g           green value, 0-255
   * @param       {number}      b           blue value, 0-255
   * @param       {number}      a           opacity value, 0-1.0
   * @return      {string}                  background colour style string
   * Description  Browser-independent alpha-channel management
   */       
  util['rgba'] = util.rgba = function(s,r,g,b,a)
  { var bgColor='transparent';
    if(util._GetIEVersion() < 9)
    {
      var pcOpacity=Math.floor(100*a),rs=r.toString(16),gs=g.toString(16),bs=b.toString(16),hexColor;
      rs=('00'+rs).substr(-2); gs=('00'+gs).substr(-2); bs=('00'+bs).substr(-2);
      hexColor=pcOpacity+rs+gs+bs; 
      s.filter='progid:DXImageTransform.Microsoft.gradient(startColorstr=#'+hexColor+',endColorstr=#'+hexColor+')';
      s.zoom='1';
    }
    else
    {   
      try
      { 
        bgColor='rgba('+r+','+g+','+b+','+a+')';
      }
      catch(ex)
      {
        bgColor='rgb('+r+','+g+','+b+')';
      }
    } 
    return bgColor;
  }
  
  /**
   * Add a stylesheet to a page programmatically, for use by the OSK, the UI or the page creator 
   * 
   * @param       {string}        s             style string
   * @return      {Object}                      returns the object reference
   **/      
  util['addStyleSheet'] = util.addStyleSheet = function(s)
  {        
    var _ElemStyle = document.createElement('STYLE'); 

    _ElemStyle.type = 'text/css';
    if(_ElemStyle.styleSheet) // IE only
    {
      _ElemStyle.styleSheet.cssText = s;
    }
    else                      // all other browsers
    {
      _ElemStyle.appendChild(document.createTextNode(s));
    }
    var _ElemHead=document.getElementsByTagName('HEAD'); 
    if(_ElemHead.length > 0)
    {
      _ElemHead[0].appendChild(_ElemStyle);
    }
    else
    {
      document.body.appendChild(_ElemStyle); // Won't work on Chrome, ah well
    }
    return _ElemStyle;
  }

  /**
   * Remove a stylesheet element
   * 
   * @param       {Object}        s             style sheet reference
   * @return      {boolean}                     false if element is not a style sheet   
   **/      
  util['removeStyleSheet'] = util.removeStyleSheet = function(s)
  {
    if(s == null || typeof(s) != 'object') return false;
    if(s.nodeName != 'STYLE') return false;
    if(typeof(s.parentNode) == 'undefined' || s.parentNode == null) return false;
    s.parentNode.removeChild(s);
    return true;
  }

  /**
   * Add a reference to an external stylesheet file
   * 
   * @param   {string}  s   path to stylesheet file      
   */      
  util['linkStyleSheet']=util.linkStyleSheet = function(s)
  {
    var headElements=document.getElementsByTagName('head');
    if(headElements.length > 0)
    {
      var linkElement=document.createElement('link');
      linkElement.type='text/css';
      linkElement.rel='stylesheet';
      linkElement.href=s;  
      headElements[0].appendChild(linkElement);
    }
  }    

  /**
   *  Add a stylesheet with a font-face CSS descriptor for the embedded font appropriate 
   *  for the browser being used
   *      
   *  @param    {Object}  fd  keymanweb font descriptor
   **/
  util.addFontFaceStyleSheet = function(fd)
  {
    // Test if a valid font descriptor
    if(typeof(fd) == 'undefined') return;

    if(typeof(fd['files']) == 'undefined') fd['files']=fd['source'];                                       
    if(typeof(fd['files']) == 'undefined') return;

    var i,ttf='',woff='',eot='',svg='',fList=[];
    
// TODO: 22 Aug 2014: check that font path passed from cloud is actually used!
     
    // Do not add a new font-face style sheet if already added for this font
    for(i=0; i<keymanweb.embeddedFonts.length; i++)
      if(keymanweb.embeddedFonts[i] == fd['family']) return;
    
    if(typeof(fd['files']) == 'string') fList[0]=fd['files']; else fList=fd['files'];
    
    for(i=0;i<fList.length;i++)
    {
      if(fList[i].indexOf('.ttf') > 0) ttf=fList[i];
      if(fList[i].indexOf('.woff') > 0) woff=fList[i];
      if(fList[i].indexOf('.eot') > 0) eot=fList[i];
      if(fList[i].indexOf('.svg') > 0) svg=fList[i];
    }

    // Font path qualified to support page-relative fonts (build 347)
    if(ttf != '' && (ttf.indexOf('/') < 0))  ttf=keymanweb.options['fonts']+ttf;
    if(woff != '' && (woff.indexOf('/') < 0)) woff=keymanweb.options['fonts']+woff;
    if(eot != '' && (eot.indexOf('/') < 0))  eot=keymanweb.options['fonts']+eot;   
    if(svg != '' && (svg.indexOf('/') < 0))  svg=keymanweb.options['fonts']+svg;

    // Build the font-face definition according to the browser being used
    var s='@font-face {\nfont-family:'
      +fd['family']+';\nfont-style:normal;\nfont-weight:normal;\n';
    
    // Detect if Internet Explorer and version if so
    var IE=util._GetIEVersion(); 
    
    // Build the font source string according to the browser, 
    // but return without adding the style sheet if the required font type is unavailable
    
    // Modern browsers: use WOFF, TTF and fallback finally to SVG. Don't provide EOT
    if(IE >= 9)
    {
      if(device.OS == 'iOS')
      {       
        if(ttf != '')
        { 
          // Modify the url if required to prevent caching  
          ttf = util.unCached(ttf);  
          s=s+'src:url(\''+ttf+'\') format(\'truetype\');';
        }
        else return; 
      }
      else 
      {
        var s0 = [];
        
        if(device.OS == 'Android') {
          // Android 4.2 and 4.3 have bugs in their rendering for some scripts 
          // with embedded ttf or woff.  svg mostly works so is a better initial
          // choice on the Android browser.
          if(svg != '') s0.push("url('"+svg+"') format('svg')");
          if(woff != '') s0.push("url('"+woff+"') format('woff')");
          if(ttf != '') s0.push("url('"+ttf+"') format('truetype')");
        } else {
          if(woff != '') s0.push("url('"+woff+"') format('woff')");
          if(ttf != '') s0.push("url('"+ttf+"') format('truetype')");
          if(svg != '') s0.push("url('"+svg+"') format('svg')");
        }        
        if(s0.length == 0) return;
        
        s += 'src:'+s0.join(',')+';';
      }
    }    
    // IE 6-8
    else
    {
      if(eot != '') 
        s=s+'src:url(\''+eot+'\');';
      else return;  
    }
    s=s+'\n}\n';    
  
    util.addStyleSheet(s);
    keymanweb.embeddedFonts.push(fd['family']);
  }     

  /**
   * Allow forced reload if necessary (stub only here)
   *    
   *  @param  {string}  s unmodified URL
   *  @return             modified URL         
   */
  util.unCached = function(s)
  {
    // var t=(new Date().getTime());
    // s = s + '?v=' + t;
    return s;
  }       
  
  /**
   * Document cookie parsing for use by kernel, OSK, UI etc.
   * 
   * @param       {string=}       cn        cookie name (optional)
   * @return      {Object}                  array of names and strings, or array of variables and values       
   */      
  util['loadCookie'] = util.loadCookie = function(cn)
  {
    var v={}; 
    if(arguments.length > 0)
    {
      var cx = util['loadCookie']();
      for(var t in cx) 
      { 
        if(t == cn) 
        {
          var d = unescape(cx[t]).split(';');
          for(var i=0; i<d.length; i++)
          {
            var xc = d[i].split('=');
            if(xc.length > 1) v[xc[0]] = xc[1]; else v[xc[0]] = '';              
          }
        }
      } 
    }
    else
    { 
      if(typeof(document.cookie) != 'undefined' && document.cookie != '')
      {     
        var c = document.cookie.split(/;\s*/);
        for(var i = 0; i < c.length; i++)
        {
          var d = c[i].split('=');
          if(d.length == 2) v[d[0]] = d[1];
        }
      }
    }
    return v;
  }
  
  /**
   * Standard cookie saving for use by kernel, OSK, UI etc.
   * 
   * @param       {string}      cn            name of cookie
   * @param       {Object}      cv            object with array of named arguments and values   
   */      
  
  util['saveCookie'] = util.saveCookie = function(cn,cv)
  {
    var s='';
    for(var v in cv)
      s = s + v+'='+cv[v]+";";    

    var d = new Date(new Date().valueOf() + 1000 * 60 * 60 * 24 * 30).toGMTString();
    document.cookie = cn+'='+escape(s)+'; path=/; expires='+d;//Fri, 31 Dec 2099 23:59:59 GMT;';
  }
  
  /**
   * Function     toNumber
   * Scope        Public
   * @param       {string}      s            numeric string
   * @param       {number}      v            default value
   * @return      {number}               
   * Description  Return string converted to integer or default value
   */       
  util['toNumber'] = util.toNumber = function(s,dflt)
  {
    var x = parseInt(s,10);
    return isNaN(x) ? dflt : x;
  }

  /**
   * Function     toNumber
   * Scope        Public
   * @param       {string}      s            numeric string
   * @param       {number}      v            default value
   * @return      {number}               
   * Description  Return string converted to real value or default value
   */       
  util['toFloat'] = util.toFloat = function(s,dflt)
  {
    var x = parseFloat(s);
    return isNaN(x) ? dflt : x;
  }
  
  /**
   * Function     toNzString
   * Scope        Public
   * @param       {*}           item         variable to test
   * @param       {?*=}         dflt         default value
   * @return      {*}               
   * Description  Test if a variable is null, false, empty string, or undefined, and return as string
   */       
  util['toNzString'] = util.nzString = function(item,dflt)
  {
    var dfltValue = '';
    if(arguments.length > 1) dfltValue = dflt; 
    if(typeof(item) == 'undefined') return dfltValue;
    if(item == null) return dfltValue;
    if(item == 0 || item == '') return dfltValue;
    return ''+item;
  }

  /**
   * Function     deepCopy
   * Scope        Private
   * @param       {Object}      p           object to copy
   * @param       {Array=}      c0          array member being copied
   * @return      {Object}                  clone ('deep copy') of object
   * Description  Makes an actual copy (not a reference) of an object, copying simple members, 
   *              arrays and member objects but not functions, so use with care!
   */              
  util.deepCopy = function(p,c0) 
  {
    var c = c0 || {};
    for (var i in p) 
    {
      if(typeof p[i] === 'object') 
      {
        c[i] = (p[i].constructor === Array )? [] : {};
        util.deepCopy(p[i],c[i]);
      } 
      else c[i] = p[i];
    }
    return c;
  }

  /**
   * Extend Array function by adding indexOf array member if undefined (IE < IE9)
  */
  if(! ('indexOf' in Array)) 
  {
    Array.prototype.indexOf = function(obj, start) 
    {
        for(var i=(start || 0); i<this.length; i++) 
        {
          if(this[i] == obj) return i;
        }
      return -1;
    }
  }
 
  /**
   * Return the event target for any browser
   *    
   * @param       {Event}      e        event
   * @return      {Object}              HTML element
   */       
  util.eventTarget = function(e)
  {
    if(e && e.target)        // most browsers
      return e.target;
    else if(window.event)     //IE 8 (and earlier)
      return window.event.srcElement;
    else
      return null;            // shouldn't happen!
  }

  /**
   * Return the event type for any browser
   *    
   * @param       {Event}      e        event
   * @return      {string}              type of event
   */       
  util.eventType = function(e)
  {
    if(e && e.type)        // most browsers
      return e.type;
    else if(window.event)     //IE 8 (and earlier)
      return window.event.type;
    else
      return '';            // shouldn't happen!
  }
  
  /**
   * Customized alert 
   *    
   * @param     {string}        s       alert text
   * @param     {function()=}   fn      function to call when alert dismissed   
   */       
  util['alert'] = util.alert = function(s,fn)
  {
    var bg=keymanweb.waiting,nn=bg.firstChild.childNodes;
    nn[0].style.display='block';
    nn[1].className='kmw-alert-text'; nn[1].innerHTML=s;
    nn[2].style.display='none';
    bg.style.display='block';
    if(arguments.length > 1) bg.dismiss=fn; else bg.dismiss=null;
  }
  
  /**
   *  Prepare the background and keyboard loading wait message box
   *  Should not be called before options are defined during initialization
   **/           
  util.prepareWait = function()
  { 
    var bg=document.createElement('DIV'),
        lb=document.createElement('DIV'),
        lt=document.createElement('DIV'),    
        gr=document.createElement('DIV'),
        bx=document.createElement('DIV');

    bg.className='kmw-wait-background'; lb.className='kmw-wait-box'; bg.dismiss=null;
    lt.className='kmw-wait-text'; gr.className='kmw-wait-graphic';
    bx.className='kmw-alert-close';
    // Close alert if anywhere in box is touched, since close box is too small on mobiles 
    lb.onmousedown=lb.onclick=function(e)
    {
      // Ignore if waiting, only handle for alert
      if(bx.style.display == 'block')
      {
        bg.style.display='none';
        if(bg.dismiss)bg.dismiss();
      }
    };
    lb.addEventListener('touchstart', lb.onclick, false);
    bg.onmousedown=bg.onclick=function(e){e.preventDefault();e.stopPropagation();}
    bg.addEventListener('touchstart', bg.onclick, false);
    lb.appendChild(bx); lb.appendChild(lt); lb.appendChild(gr);
    bg.appendChild(lb); document.body.appendChild(bg);
    keymanweb.waiting=bg;    
  }
  
  /**
   * Get path of keymanweb script, for relative references
   * 
   * *** This is not currently used, but may possibly be needed if ***
   * *** script identification during loading proves unreliable.   ***         
   *
   *  @param    {string}      sName   filename prefix  
   *@return   {string}      path to source, with trailing slash
   **/           
  util.myPath = function(sName)
  {
    var i,scripts=document.getElementsByTagName('script'),ss;
    
    for(i=0; i<scripts.length; i++)
    {
      ss=scripts[i];
      if(ss.src.indexOf(sName) >= 0) 
        return ss.src.substr(0,ss.src.lastIndexOf('/')+1);      
    }
    return '';
  }

  // Prepend the appropriate protocol if not included in path
  util.prependProtocol = function(path)
  {
    var pattern = new RegExp('^https?:');
  
    if(pattern.test(path)) 
      return path;
    else if(path.substr(0,2) == '//')
      return keymanweb.protocol+path;
    else if(path.substr(0,1) == '/')
      return keymanweb.protocol+'/'+path;
    else
      return keymanweb.protocol+'//'+path;    
  }
   
  /**
   * Return the appropriate test string for a given font
   * 
   * TODO: Tidy up and remove arrays once 'sample' included in font metadata
   * 
   *  @param  {Object}    fd    font meta-data object
   *  @return {string}          string to compare width
   *
   */                   
  util.testString = function(fd)
  {
    var fontName=fd['family'], 
    i,s='BESbswy';
    if('sample' in fd && typeof(fd['sample']) == 'string')
      return s+fd['sample'];

    var f=['TamilWeb','TibetanWeb','LatinWeb','CherokeeWeb',    
          'EgyptianWeb','SinhalaWeb','KhmerWeb','ArabicWeb',
          'BurmeseWeb','LaoWeb','OriyaWeb','GeezWeb'],
        t=['\u0BBE\u0BF5','\u0F7F\u0FD0','\u02B0\u02A4','\u13D0\u13C9',
          '\uA723\uF7D3','\u0DD8\u0DA3','\u17D6\u178E','\u0639\u06B3',
          '\u1038\u1024','\u0EC0\u0EDD','\u0B03\u0B06','\u1361\u132C'];
    for(i=0; i<f.length; i++)
    {
      if(fontName == f[i]) return s+t[i];
    }
    return s; 
  }

  /**
   * Test if a font is installed (or available) on the target platform
   *    
   * @param       {Object}        fd    font structure
   * @return      {boolean}             true if font available
   */       
  util.checkFont = function(fd)
  {
    var fontReady=false,fontName=fd['family'];
    
  	// Create an absolute positioned div and two paragraph elements with spans for the test string.
  	// The paragraph elements ensure that the spans are measured from the same point, otherwise
  	// pixel rounding can result in different widths for the same string and styles.
  	// Using a separate invisible DIV is more reliable than other positioning.
    var d=document.createElement('DIV'),ds=d.style,
        p1=document.createElement('P'),
        p2=document.createElement('P'),
        t1=document.createElement('SPAN'),s1=t1.style,
        t2=document.createElement('SPAN'),s2=t2.style;
 
    ds.position='absolute';ds.top='10px';ds.left='10px';
    ds.visibility='hidden';
    document.body.appendChild(d);
  	d.appendChild(p1); d.appendChild(p2); 
  	p1.appendChild(t1); p2.appendChild(t2); 

  	// Firefox fails without the !important prefix on the fallback font, 
  	// apparently applying the same font to both elements.
    // But it also fails to distinguish the two if !important is added to the test font!  
  	// *** TODO: See if still true after changes Dec 2013 *** 
    // Must apply !important tag to font-family, but must apply it to the CSS style, not the JS object member
    // c.f. http://stackoverflow.com/questions/462537/overriding-important-style-using-javascript 
    t1.setAttribute('style','font-family:monospace !important');
    s2.fontFamily=fontName+',monospace'; 
  	s1.fontSize=s2.fontSize='24px';      // Not too large, to avoid wrapping or overflow 
    
    // Include narrow and wide characters from each unique script
  	t1.innerHTML=t2.innerHTML=util.testString(fd); 
    
  	// Compare the actual width of each span. Checking monospace, serif, 
    // and sans-serif helps to avoid falsely reporting the font as ready
    // The width must be different for all three tests.
  	if(t1.offsetWidth != t2.offsetWidth)
    { 
      t1.setAttribute('style','font-family:sans-serif !important');
      s2.fontFamily=fontName+',sans-serif';
      if(t1.offsetWidth != t2.offsetWidth) 
      {
        t1.setAttribute('style','font-family:serif !important');
        s2.fontFamily=fontName+',serif';        
      }
  	}    
    fontReady=(t1.offsetWidth != t2.offsetWidth);
    
    // Delete test elements
	  p1.removeChild(t1);p2.removeChild(t2);
	  d.removeChild(p1);d.removeChild(p2);
	  document.body.removeChild(d);

    return fontReady;
  }

  /**
   * Expose font testing to allow checking that SpecialOSK or custom font has
   * been correctly loaded by browser
   * 
   *  @param  {string}  fName   font-family name
   *  @return {boolean}         true if available
   **/         
  keymanweb['isFontAvailable'] = function(fName)
  {
    return util.checkFont({'family':fName});
  }
   
  /**
   * Check a font descriptor for font availability, returning true if undefined
   * 
   *  @param  {Object}  fd  font descriptor member of keyboard stub
   *  @return {boolean}           
   **/
  util.checkFontDescriptor = function(fd)
  {  
    if(typeof(fd) == 'undefined' || typeof(fd['family']) != 'string') return true;
    return util.checkFont(fd);
  }
      
})();

