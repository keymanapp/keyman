/// <reference path="kmwexthtml.ts" />  // Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="Util.ts" />  // Includes KMW-added property declaration extensions for HTML elements.
// Allows proper minification handling.
/// <reference path="../node_modules/google-closure-library/closure/goog/base.js" />

/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/

declare var keyman: any
var keyman = window['keyman'] || {};

goog.exportSymbol("keyman", keyman);


/** @define {number} build counter that gets set by the build environment */
keyman.__BUILD__ = 299;

/**  
 * Base code: Declare major component namespaces, instances, and utility functions 
 */  

// If a copy of the script is already loaded, detect this and prevent re-initialization / data reset.
if(!window['keyman']['loaded']) {

  (function() {

    var keymanweb: any;
    keymanweb = keyman = window['keyman'] = {
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
      waiting:null,             // Element displayed during keyboard load time
      warned: false,            // Warning flag (to prevent multiple warnings)
      baseFont: 'sans-serif',   // Default font for mapped input elements
      appliedFont: '',          // Chain of fonts to be applied to mapped input elements
      fontCheckTimer: null,     // Timer for testing loading of embedded fonts
      srcPath: '',              // Path to folder containing executing keymanweb script
      rootPath: '',             // Path to server root
      mustReloadKeyboard: false,// Force keyboard refreshing even if already loaded
      globalKeyboard: null,     // Indicates the currently-active keyboard for controls without independent keyboard settings.
      globalLanguageCode: null, // Indicates the language code corresponding to `globalKeyboard`.
      isEmbedded: false,        // Indicates if the KeymanWeb instance is embedded within a mobile app.
                                // Blocks full page initialization when set to `true`.
      refocusTimer: 0,          // Tracks a timeout event that aids of OSK modifier/state key tracking when the document loses focus.
      modStateFlags: 0          // Tracks the present state of the physical keyboard's active modifier flags.  Needed for AltGr simulation.
    };

    // Signals that a KMW load has occurred in order to prevent double-loading.
    keymanweb['loaded'] = true;

    keymanweb['initialized'] = 0;
    keymanweb['build'] = keyman.__BUILD__;
    keymanweb['version'] = '10.0';
    keymanweb['helpURL'] = 'http://help.keyman.com/go'; 
    
    // Define public OSK, user interface and utility function objects 

    var util: Keyman.Util;
    util = keymanweb['util'] = new Keyman.Util(keymanweb);
    var osk: any;
    osk = keymanweb['osk'] = {ready:false};
    var ui: any;
    ui = keymanweb['ui'] = {};
    
    var kbdInterface = keymanweb['interface'] = {
      // Cross-reference with /windows/src/global/inc/Compiler.h - these are the Developer codes for the respective system stores.
      // They're named here identically to their use in that header file.
      TSS_LAYER: 33,
      TSS_PLATFORM: 31,

      _AnyIndices: [],        // AnyIndex - array of any/index match indices
      _BeepObjects: [],       // BeepObjects - maintains a list of active 'beep' visual feedback elements
      _BeepTimeout: 0,        // BeepTimeout - a flag indicating if there is an active 'beep'. 
                              // Set to 1 if there is an active 'beep', otherwise leave as '0'.
      _DeadKeys: []           // DeadKeys - array of matched deadkeys
    };

    // Stub functions (defined later in code only if required)
    keymanweb.setDefaultDeviceOptions = function(opt){}     
    keymanweb.getStyleSheetPath = function(s){return s;}
    keymanweb.getKeyboardPath=function(f, p){return f;}
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
    * Extend Array function by adding indexOf array member if undefined (IE < IE9)
    */
    if(!('indexOf' in Array)) {
      Array.prototype.indexOf = function(obj, start) {
        for(var i=(start || 0); i<this.length; i++) {
          if(this[i] == obj) {
            return i;
          }
        }
        return -1;
      }
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
  })();
}