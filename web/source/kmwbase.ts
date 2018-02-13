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
// Defines built-in keymapping.
/// <reference path="kmwkeymaps.ts" />
// Defines KMW's hotkey management object.
/// <reference path="kmwhotkeys.ts" />
// Defines the ui management code that tracks UI activation and such.
/// <reference path="kmwuimanager.ts" />

/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/

class KeymanBase {
  _TitleElement = null;      // I1972 - KeymanWeb Titlebar should not be a link
  _IE = 0;                   // browser version identification
  _MasterDocument = null;    // Document with controller (to allow iframes to distinguish local/master control)
  _HotKeys = [];             // Array of document-level hotkey objects
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
  ['util']: Util;
  ['osk']: any;
  ['ui']: any;
  ['interface']: KeyboardInterface;
  keyboardManager: KeyboardManager;
  domManager: DOMManager;
  hotkeyManager: HotkeyManager;
  uiManager: UIManager;
  keyMapManager: KeyMapManager;

  touchAliasing: DOMEventHandlers;

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
    // Allow internal minification of the public modules.
    this.util = this['util'] = new Util(this);
    window['KeymanWeb'] = this.interface = this['interface'] = new KeyboardInterface(this);
    this.osk = this['osk'] = {ready:false};
    this.ui = this['ui'] = null;

    this.keyboardManager = new KeyboardManager(this);
    this.domManager = new DOMManager(this);
    this.hotkeyManager = new HotkeyManager(this);
    this.uiManager = new UIManager(this);
    this.keyMapManager = new KeyMapManager();

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

  delayedInit() {
    // Track the selected Event-handling object.
    this.touchAliasing = this.util.device.touchable ? this.domManager.touchHandlers : this.domManager.nonTouchHandlers;
  }

  /**
   * Triggers a KeymanWeb engine shutdown to facilitate a full system reset.
   * This function is designed for use with KMW unit-testing, which reloads KMW
   * multiple times to test the different initialization paths.
   */
  ['shutdown']() {
    this.domManager.shutdown();
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
        var elem: HTMLElement|Document = this.domManager.getLastActiveElement();
        if(elem) {
          elem = elem.ownerDocument;
          var win: Window;
          if(elem) {
            win = elem.defaultView;
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
   * Gets the cookie for the name and language code of the most recently active keyboard
   * 
   *  Defaults to US English, but this needs to be user-set in later revision (TODO)      
   * 
   * @return      {string}          InternalName:LanguageCode 
   */    
  ['getSavedKeyboard']() {
    return this.keyboardManager.getSavedKeyboard();  
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

  /**
   * Function     resetContext
   * Scope        Public
   * Description  Revert OSK to default layer and clear any deadkeys and modifiers
   */
  ['resetContext']() {
    this.interface.resetContext();
  };

  /**
   * Function     disableControl
   * Scope        Public
   * @param       {Element}      Pelem       Element to be disabled
   * Description  Disables a KMW control element 
   */    
  ['disableControl'](Pelem: HTMLElement) {
    this.domManager.disableControl(Pelem);
  }

  /**
   * Function     enableControl
   * Scope        Public
   * @param       {Element}      Pelem       Element to be disabled
   * Description  Disables a KMW control element 
   */    
  ['enableControl'](Pelem: HTMLMapElement) {
    this.domManager.enableControl(Pelem);
  }
  
  /**
   * Function     setKeyboardForControl
   * Scope        Public   
   * @param       {Element}    Pelem    Control element 
   * @param       {string|null=}    Pkbd     Keyboard (Clears the set keyboard if set to null.)  
   * @param       {string|null=}     Plc      Language Code
   * Description  Set default keyboard for the control 
   */    
  ['setKeyboardForControl'](Pelem: HTMLElement, Pkbd?: string, Plc?: string) {
    this.domManager.setKeyboardForControl(Pelem, Pkbd, Plc);
  }

  /**
   * Function     getKeyboardForControl
   * Scope        Public   
   * @param       {Element}    Pelem    Control element 
   * @return      {string|null}         The independently-managed keyboard for the control.
   * Description  Returns the keyboard ID of the current independently-managed keyboard for this control.
   *              If it is currently following the global keyboard setting, returns null instead.
   */
  ['getKeyboardForControl'](Pelem) {
    this.domManager.getKeyboardForControl(Pelem);
  }

  /**
   * Function     getLanguageForControl
   * Scope        Public   
   * @param       {Element}    Pelem    Control element 
   * @return      {string|null}         The independently-managed keyboard for the control.
   * Description  Returns the language code used with the current independently-managed keyboard for this control.
   *              If it is currently following the global keyboard setting, returns null instead.
   */
  ['getLanguageForControl'](Pelem) {
    this.domManager.getLanguageForControl(Pelem);
  }

  /**
   * Set focus to last active target element (browser-dependent)
   */    
  ['focusLastActiveElement']() {
    this.domManager.focusLastActiveElement();
  }
  
  /**
   * Get the last active target element *before* KMW activated (I1297)
   * 
   * @return      {Object}        
   */    
  ['getLastActiveElement']() {
    return this.domManager.getLastActiveElement();
  }

  /**
   *  Set the active input element directly optionally setting focus 
   * 
   *  @param  {Object|string} e         element id or element
   *  @param  {boolean=}      setFocus  optionally set focus  (KMEW-123) 
   **/
  ['setActiveElement'](e: string|HTMLElement, setFocus: boolean) {
    return this.domManager.setActiveElement(e, setFocus);
  }

  /**
   * Move focus to user-specified element
   * 
   *  @param  {string|Object}   e   element or element id
   *           
   **/
  ['moveToElement'](e: string|HTMLElement) {
    this.domManager.moveToElement(e);
  }

  /**
   * Function     addHotkey
   * Scope        Public
   * @param       {number}            keyCode
   * @param       {number}            shiftState
   * @param       {function(Object)}  handler
   * Description  Add hot key handler to array of document-level hotkeys triggered by key up event
   */
  ['addHotKey'](keyCode: number, shiftState: number, handler: () => void) {
    this.hotkeyManager.addHotKey(keyCode, shiftState, handler);
  }

  /**
   * Function     removeHotkey
   * Scope        Public
   * @param       {number}        keyCode
   * @param       {number}        shiftState
   * Description  Remove a hot key handler from array of document-level hotkeys triggered by key up event
   */
  ['removeHotKey'](keyCode: number, shiftState: number) {
    this.hotkeyManager.removeHotkey(keyCode, shiftState);
  }

  /**
   * Function     getUIState
   * Scope        Public   
   * @return      {Object.<string,(boolean|number)>}
   * Description  Return object with activation state of UI:
   *                activationPending (bool):   KMW being activated
   *                activated         (bool):   KMW active    
   */    
  ['getUIState'](): UIState {
    return this.uiManager.getUIState();
  }

  /**
   * Set or clear the IsActivatingKeymanWebUI flag (exposed function)
   * 
   * @param       {(boolean|number)}  state  Activate (true,false)
   */
  ['activatingUI'](state: boolean) {
    this.uiManager.setActivatingUI(state);
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
if(!window['keyman'] || !window['keyman']['loaded']) {

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
  })();
}