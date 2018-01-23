// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwexthtml.ts" />
// Defines the web-page interface object.
/// <reference path="kmwdom.ts" />
// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwutils.ts" />
// Defines the keyboard callback object.
/// <reference path="kmwcallback.ts" />
// Defines keyboard data & management classes.
/// <reference path="kmwkeyboards.ts" />

/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/

declare var keyman: KeymanBase;
declare var KeymanWeb: KeyboardInterface;
var keyman: KeymanBase = window['keyman'] || {};
window['keyman'] = keyman; // To preserve the name _here_ in case of minification.

class KeymanBase {
  _TitleElement = null;      // I1972 - KeymanWeb Titlebar should not be a link
  _DisableInput = 0;         // Should input be disabled?
  _IE = 0;                   // browser version identification
  _IsActivatingKeymanWebUI = 0;    // ActivatingKeymanWebUI - is the KeymanWeb DIV in process of being clicked on?
  _JustActivatedKeymanWebUI = 0;   // JustActivatedKeymanWebUI - focussing back to control after KeymanWeb UI interaction  
  _IgnoreNextSelChange = 0;  // when a visual keyboard key is mouse-down, ignore the next sel change because this stuffs up our history  
  _Selection = null;
  _SelectionControl = null;
  _AttachedElements = [];    // I1596 - attach to controls dynamically
  _ActiveElement = null;     // Currently active (focused) element  I3363 (Build 301)
  _LastActiveElement = null; // LastElem - Last active element
  _DfltStyle = '';           // Default styles
  _MasterDocument = null;    // Document with controller (to allow iframes to distinguish local/master control)
  _HotKeys = [];             // Array of document-level hotkey objects
  focusTimer = null;         // Timer to manage loss of focus to unmapped input
  focusing = false;          // Flag to manage movement of focus
  resizing = false;          // Flag to control resize events when resetting viewport parameters
  sortedInputs = [];         // List of all INPUT and TEXTAREA elements ordered top to bottom, left to right
  inputList = [];            // List of simulated input divisions for touch-devices   I3363 (Build 301)
  waiting = null;            // Element displayed during keyboard load time
  warned = false;            // Warning flag (to prevent multiple warnings)
  baseFont = 'sans-serif';   // Default font for mapped input elements
  appliedFont = '';          // Chain of fonts to be applied to mapped input elements
  fontCheckTimer = null;     // Timer for testing loading of embedded fonts
  srcPath = '';              // Path to folder containing executing keymanweb script
  rootPath = '';             // Path to server root
  protocol = '';             // Protocol used for the KMW script.
  mustReloadKeyboard = false;// Force keyboard refreshing even if already loaded
  globalKeyboard = null;     // Indicates the currently-active keyboard for controls without independent keyboard settings.
  globalLanguageCode = null; // Indicates the language code corresponding to `globalKeyboard`.
  isEmbedded = false;        // Indicates if the KeymanWeb instance is embedded within a mobile app.
                              // Blocks full page initialization when set to `true`.
  refocusTimer = 0;          // Tracks a timeout event that aids of OSK modifier/state key tracking when the document loses focus.
  modStateFlags = 0;         // Tracks the present state of the physical keyboard's active modifier flags.  Needed for AltGr simulation.

  initialized: number;       // Signals the initialization state of the KeymanWeb system.
  'build' = 300;           // TS needs this to be defined within the class.

  // Used as placeholders during initialization.
  // The corresponding class properties should be dropped after a refactor;
  // this is an intermediate solution while doing the big conversion.
  static _srcPath: string;
  static _rootPath: string;
  static _protocol: string;
  static __BUILD__: number;

  // Internal objects
  util: Util;
  osk: any;
  ui: any;
  interface: KeyboardInterface;
  keyboardManager: KeyboardManager;
  domManager: DOMManager;

  // Defines option-tracking object as a string map.
  options: { [name: string]: string; } = {
    'root':'',
    'resources':'',
    'keyboards':'',
    'fonts':'',
    'attachType':'',
    'ui':null
  };;


  // Stub functions (defined later in code only if required)
  setDefaultDeviceOptions(opt){}     
  getStyleSheetPath(s){return s;}
  getKeyboardPath(f, p?){return f;}
  KC_(n, ln, Pelem){return '';} 
  handleRotationEvents(){}
  alignInputs(b){}
  namespaceID(Pstub) {};
  preserveID(Pk) {};

  setInitialized(val: number) {
    this.initialized = this['initialized'] = val;
  }

  refreshElementContent = null;

  // -------------

  constructor() {
    this.util = this['util'] = new Util(this);
    window['KeymanWeb'] = this.interface = this['interface'] = new KeyboardInterface(this);

    this.osk = this['osk'] = {ready:false};
    this.keyboardManager = new KeyboardManager(this);
    this.domManager = new DOMManager(this);

    // Load properties from their static variants.
    this['build'] = KeymanBase.__BUILD__;
    this.srcPath = KeymanBase._srcPath;
    this.rootPath = KeymanBase._rootPath;
    this.protocol = KeymanBase._protocol;

    this['version'] = "10.0";
    this['helpURL'] = 'http://help.keyman.com/go';
    this.setInitialized(0);

    // Signals that a KMW load has occurred in order to prevent double-loading.
    this['loaded'] = true;
  }

  /**
   * Expose font testing to allow checking that SpecialOSK or custom font has
   * been correctly loaded by browser
   * 
   *  @param  {string}  fName   font-family name
   *  @return {boolean}         true if available
   **/         
  isFontAvailable(fName: string): boolean {
    return this.util.checkFont({'family':fName});
  }

  /**
   * Function     addEventListener
   * Scope        Public
   * @param       {string}            event     event to handle
   * @param       {function(Event)}   func      event handler function
   * @return      {boolean}                     value returned by util.addEventListener
   * Description  Wrapper function to add and identify KeymanWeb-specific event handlers
   */       
  addEventListener(event: string, func): boolean {
    return this.util.addEventListener('kmw.'+event, func);
  }

    /**
   * Function     _GetEventObject
   * Scope        Private   
   * @param       {Event=}     e     Event object if passed by browser
   * @return      {Event|null}       Event object              
   * Description Gets the event object from the window when using Internet Explorer
   *             and handles getting the event correctly in frames 
   */     
  _GetEventObject<E extends Event>(e: E) {  // I2404 - Attach to controls in IFRAMEs
    if (!e) {
      e = window.event as E;
      if(!e) {
        var elem: HTMLElement|Document = (<any>this)._GetLastActiveElement();
        if(elem) {
          elem = elem.ownerDocument;
          var win: Window;
          if(elem) {
            win = elem.parentWindow;
          }
          if(!win) {
            return null;
          }
          
          e = win.event as E;
        }
      }
    }
    
    return e;    
  }

  // Base object API definitions

  /**
   * Function     attachToControl
   * Scope        Public
   * @param       {Element}    Pelem       Element to which KMW will be attached
   * Description  Attaches KMW to control (or IFrame) 
   */  
  ['attachToControl'](Pelem: HTMLElement) {
    this.domManager.attachToControl(Pelem);
  }

  /**
   * Function     detachFromControl
   * Scope        Public
   * @param       {Element}    Pelem       Element from which KMW will detach
   * Description  Detaches KMW from a control (or IFrame) 
   */  
  ['detachFromControl'](Pelem: HTMLElement) {
    this.domManager.detachFromControl(Pelem);
  }

  /**
   * Exposed function to load keyboards by name. One or more arguments may be used
   * 
   * @param {string|Object} x keyboard name string or keyboard metadata JSON object
   * 
   */  
  ['addKeyboards'](x) {                       
    if(arguments.length == 0) {
      this.keyboardManager.keymanCloudRequest('',false);
    } else {
      this.keyboardManager.addKeyboardArray(arguments);
    }
  }
  
  /**
   *  Add default or all keyboards for a given language
   *  
   *  @param  {string}   arg    Language name (multiple arguments allowed)
   **/           
  ['addKeyboardsForLanguage'](arg) {
    this.keyboardManager.addLanguageKeyboards(arguments);
  }
  
  /**
   * Call back from cloud for adding keyboard metadata
   * 
   * @param {Object}    x   metadata object
   **/                  
  ['register'](x) {                     
    this.keyboardManager.register(x);
  }

  /**
   * Build 362: removeKeyboards() remove keyboard from list of available keyboards
   * 
   * @param {string} x keyboard name string
   * 
   */  
  ['removeKeyboards'](x) {
    return this.keyboardManager.removeKeyboards(x);
  }

  /**
   * Allow to change active keyboard by (internal) keyboard name
   * 
   * @param       {string}    PInternalName   Internal name
   * @param       {string}    PLgCode         Language code
   */    
  ['setActiveKeyboard'](PInternalName: string, PLgCode: string) {
    this.keyboardManager.setActiveKeyboard(PInternalName,PLgCode);
  }
  
  /**
   * Function     getActiveKeyboard
   * Scope        Public
   * @return      {string}      Name of active keyboard
   * Description  Return internal name of currently active keyboard
   */    
  ['getActiveKeyboard'](): string {
    return this.keyboardManager.getActiveKeyboardName();
  }

  /**
   * Function    getActiveLanguage
   * Scope       Public
   * @return     {string}         language code
   * Description Return language code for currently selected language
   */    
  ['getActiveLanguage'](): string {
    return this.keyboardManager.getActiveLanguage();
  }

  ['isAttached'](x: HTMLElement): boolean {
    return this.domManager.isAttached(x);
  }

  /**
   * Function    isCJK
   * Scope       Public
   * @param      {Object=}  k0 
   * @return     {boolean}
   * Description Tests if active keyboard (or optional argument) uses a pick list (Chinese, Japanese, Korean, etc.)
   *             (This function accepts either keyboard structure.)   
   */    
  ['isCJK'](k0) { 
    return this.keyboardManager.isCJK(k0);
  }

  /**
   * Function     isChiral
   * Scope        Public
   * @param       {string|Object=}   k0
   * @return      {boolean}
   * Description  Tests if the active keyboard (or optional argument) uses chiral modifiers.
   */
  ['isChiral'](k0?) {
    return this.keyboardManager.isChiral(k0);
  }

  /**
   * Get keyboard meta data for the selected keyboard and language
   * 
   * @param       {string}    PInternalName     Internal name of keyboard
   * @param       {string=}   PlgCode           language code
   * @return      {Object}                      Details of named keyboard 
   *                                            
   **/    
  ['getKeyboard'](PInternalName: string, PlgCode?: string) {
    var Ln, Lrn;

    var kbdList = this.keyboardManager.getDetailedKeyboards();

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
  ['getKeyboards']() {
    return this.keyboardManager.getDetailedKeyboards();
  }

  /**
   * Function     Initialization
   * Scope        Public
   * @param       {Object}  arg     object array of user-defined properties
   * Description  KMW window initialization  
   */    
  ['init'](arg) {
    this.domManager.init(arg);
  }
  
  /*
   * Legacy entry points (non-standard names)- included only to allow existing IME keyboards to continue to be used
   */

  getLastActiveElement(): HTMLElement {
    return this._LastActiveElement; 
  }

  focusLastActiveElement(): void {
    (<any>this)._FocusLastActiveElement(); 
  }

  //The following entry points are defined but should not normally be used in a keyboard, as OSK display is no longer determined by the keyboard
  hideHelp(): void {
    this.osk._Hide(true);
  }

  showHelp(Px: number, Py: number): void {
    this.osk._Show(Px,Py);
  }

  showPinnedHelp(): void {
    this.osk.userPositioned=true; 
    this.osk._Show(-1,-1);
  }

  /**
   * Function     _push
   * Scope        Private   
   * @param       {Array}     Parray    Array   
   * @param       {*}         Pval      Value to be pushed or appended to array   
   * @return      {Array}               Returns extended array
   * Description  Push (if possible) or append a value to an array 
   */  
  _push<T>(Parray: T[], Pval: T) {
    if(Parray.push) {
      Parray.push(Pval);
    } else {
      Parray=Parray.concat(Pval);
    }
    return Parray;
  }
}

/**
 * Determine path and protocol of executing script, setting them as
 * construction defaults.
 *    
 * This can only be done during load when the active script will be the  
 * last script loaded.  Otherwise the script must be identified by name.
*/  
var scripts = document.getElementsByTagName('script');
var ss = scripts[scripts.length-1].src;
var sPath = ss.substr(0,ss.lastIndexOf('/')+1);

KeymanBase._srcPath = sPath;
KeymanBase._rootPath = sPath.replace(/(https?:\/\/)([^\/]*)(.*)/,'$1$2/');
KeymanBase._protocol = sPath.replace(/(.{3,5}:)(.*)/,'$1');

/** @define {number} build counter that gets set by the build environment */
KeymanBase.__BUILD__ = 299;

/**  
 * Base code: Declare major component namespaces, instances, and utility functions 
 */  

// If a copy of the script is already loaded, detect this and prevent re-initialization / data reset.
if(!window['keyman']['loaded']) {

  (function() {
    /* The base object call may need to be moved into a separate, later file eventually.
     * It will be necessary to override methods with kmwnative.ts and kmwembedded.ts before the
     * affected objects are initialized.
     * 
     * We only recreate the 'keyman' object if it's not been loaded.
     * As this is the base object, not creating it prevents a KMW system reset.
     */
    var keymanweb = window['keyman'] = new KeymanBase();
    
    // Define public OSK, user interface and utility function objects 

    var osk: any = keymanweb['osk'];
    var ui: any = keymanweb['ui'] = {};
    
    osk.highlightSubKeys = function(k,x,y){}
    osk.createKeyTip = function(){}
    osk.optionKey = function(e,keyName,keyDown){}
    osk.showKeyTip = function(key,on){}  
    osk.waitForFonts = function(kfd,ofd){return true;}
                 
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
  })();
}