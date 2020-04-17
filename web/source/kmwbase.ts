// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwexthtml.ts" />
// Includes a promise polyfill (needed for IE)
/// <reference path="../node_modules/es6-shim/es6-shim.min.js" />
// Defines build-environment includes, since `tsc` doesn't provide a compile-time define.
/// <reference path="environment.inc.ts" />
// Defines the web-page interface object.
/// <reference path="singleton.ts" />
// Defines the core text processor.
/// <reference path="text/inputProcessor.ts" />
// Extends KeyboardInterface with DOM-oriented offerings.
/// <reference path="text/domKbdInterface.ts" />
// Defines the web-page interface object.
/// <reference path="dom/domManager.ts" />
// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwutils.ts" />
// Defines keyboard management classes.
/// <reference path="keyboards/kmwkeyboards.ts" />
// Defines KMW's hotkey management object.
/// <reference path="kmwhotkeys.ts" />
// Defines the ui management code that tracks UI activation and such.
/// <reference path="kmwuimanager.ts" />
// Defines OSK management code.
/// <reference path="osk/oskManager.ts" />
// Defines the language modeling layer (for use in autocorrect and text prediction)
/// <reference path="includes/lmlayer.ts" />
// Defines the model manager.
/// <reference path="text/prediction/modelManager.ts" />

/***
   KeymanWeb 11.0
   Copyright 2017-2019 SIL International
***/
namespace com.keyman {
  export class KeymanBase {
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
    isHeadless = false;        // Indicates that KMW lacks any access to the DOM.  Nothing yet implemented for '= true'.
    'build' = 300;             // TS needs this to be defined within the class.
    _BrowserIsSafari: boolean; // A legacy browser-check variable.

    // Used as placeholders during initialization.
    // The corresponding class properties should be dropped after a refactor;
    // this is an intermediate solution while doing the big conversion.
    static _srcPath: string;
    static _rootPath: string;
    static _protocol: string;

    // Internal objects
    ['util']: Util;
    ['osk']: com.keyman.osk.OSKManager;
    ['ui']: any;
    keyboardManager: keyboards.KeyboardManager;
    domManager: dom.DOMManager;
    hotkeyManager: HotkeyManager;
    uiManager: UIManager;
    core: text.InputProcessor;
    modelManager: text.prediction.ModelManager;

    touchAliasing: dom.DOMEventHandlers;

    // Defines option-tracking object as a string map.
    options: { [name: string]: string; } = {
      'root':'',
      'resources':'',
      'keyboards':'',
      'fonts':'',
      'attachType':'',
      'ui':null
    };


    // Stub functions (defined later in code only if required)
    setDefaultDeviceOptions(opt){}     
    getStyleSheetPath(s){return s;}
    getKeyboardPath(f, p?){return f;}
    KC_(n, ln, Pelem){return '';} 
    handleRotationEvents(){}
    // Will serve as an API function for a workaround, in case of future touch-alignment issues.
    ['alignInputs'](eleList?: HTMLElement[]){}
    hideInputs() {};
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
      this.ui = this['ui'] = {};

      this.keyboardManager = new keyboards.KeyboardManager(this);
      this.domManager = new dom.DOMManager(this);
      this.hotkeyManager = new HotkeyManager(this);
      this.uiManager = new UIManager(this);

      // I732 START - Support for European underlying keyboards #1
      var baseLayout: string;
      if(typeof(window['KeymanWeb_BaseLayout']) !== 'undefined') {
        baseLayout = window['KeymanWeb_BaseLayout'];
      } else {
        baseLayout = 'us';
      }
      this._BrowserIsSafari = (navigator.userAgent.indexOf('AppleWebKit') >= 0);  // I732 END - Support for European underlying keyboards #1      

      this.core = new text.InputProcessor({
        baseLayout: baseLayout,
        variableStoreSerializer: new dom.VariableStoreCookieSerializer()
      });

      // Used by the embedded apps.
      this['interface'] = this.core.keyboardInterface;
      
      this.modelManager = new text.prediction.ModelManager();
      this.osk = this['osk'] = new com.keyman.osk.OSKManager();

      // Load properties from their static variants.
      this['build'] = com.keyman.environment.BUILD;
      this.srcPath = KeymanBase._srcPath;
      this.rootPath = KeymanBase._rootPath;
      this.protocol = KeymanBase._protocol;

      this['version'] = com.keyman.environment.VERSION;
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
      // Disable page focus/blur events, which can sometimes trigger and cause parallel KMW instances in testing.
      this.util.detachDOMEvent(window, 'focus', this['pageFocusHandler'], false);
      this.util.detachDOMEvent(window, 'blur', this['pageFocusHandler'], false);

      this.domManager.shutdown();
      this.osk.shutdown();
      this.util.shutdown();
      this.keyboardManager.shutdown();
      this.core.languageProcessor.shutdown();

      if(this.ui && this.ui.shutdown) {
        this.ui.shutdown();
      }

      dom.DOMEventHandlers.states = new dom.CommonDOMStates();
    }

    /**
     * Expose font testing to allow checking that SpecialOSK or custom font has
     * been correctly loaded by browser
     * 
     *  @param  {string}  fName   font-family name
     *  @return {boolean}         true if available
     **/         
    ['isFontAvailable'](fName: string): boolean {
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
    ['addEventListener'](event: string, func): boolean {
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
          var elem: HTMLElement = this.domManager.getLastActiveElement();
          if(elem) {
            let doc = elem.ownerDocument;
            var win: Window;
            if(doc) {
              win = doc.defaultView;
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
     * @param {string}  x      keyboard name string
     * @param {boolean} force  When true, also drops the cached keyboard object
     * 
     */  
    ['removeKeyboards'](x, force?) {
      return this.keyboardManager.removeKeyboards(x);
    }

    /**
     * Allow to change active keyboard by (internal) keyboard name
     * 
     * @param       {string}    PInternalName   Internal name
     * @param       {string}    PLgCode         Language code
     */    
    ['setActiveKeyboard'](PInternalName: string, PLgCode: string): Promise<void> {
      return this.keyboardManager.setActiveKeyboard(PInternalName,PLgCode);
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
     * @param      {boolean=}        true to retrieve full language name, false/undefined to retrieve code.
     * @return     {string}         language code
     * Description Return language code for currently selected language
     */    
    ['getActiveLanguage'](fullName?: boolean): string {
      return this.keyboardManager.getActiveLanguage(fullName);
    }

    ['isAttached'](x: HTMLElement): boolean {
      return this.domManager.isAttached(x);
    }

    /**
     * Function    isCJK
     * Scope       Public
     * @param      {Object=}  k0 
     * @return     {boolean}
     * Description Tests if active keyboard (or specified keyboard script object, as optional argument) 
     *             uses a pick list (Chinese, Japanese, Korean, etc.)
     *             (This function accepts either keyboard structure.)   
     */    
    ['isCJK'](k0?) { 
      var kbd: keyboards.Keyboard;
      if(k0) {
        kbd = new keyboards.Keyboard(k0);
      } else {
        kbd = this.core.activeKeyboard;
      }

      return kbd && kbd.isCJK;
    }

    /**
     * Function     isChiral
     * Scope        Public
     * @param       {string|Object=}   k0
     * @return      {boolean}
     * Description  Tests if the active keyboard (or optional argument) uses chiral modifiers.
     */
    ['isChiral'](k0?) {
      var kbd: keyboards.Keyboard;
      if(k0) {
        kbd = new keyboards.Keyboard(k0);
      } else {
        kbd = this.core.activeKeyboard;
      }
      return kbd.isChiral;
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
    ['init'](arg): Promise<any> {
      return this.domManager.init(arg);
    }

    /**
     * Function     resetContext
     * Scope        Public
     * @param       {Object} e      The element whose context should be cleared.  If null, the currently-active element will be chosen.
     * Description  Reverts the OSK to the default layer, clears any processing caches and modifier states, 
     *              and clears deadkeys and prediction-processing states on the active element (if it exists)
     */
    ['resetContext'](e?: HTMLElement) {
      let elem = e;
      if(!elem) {
        elem = dom.DOMEventHandlers.states.activeElement;
      }
      let outputTarget = dom.Utils.getOutputTarget(elem);
      if(outputTarget) {
        outputTarget.resetContext();
      }
      this.core.resetContext();
    };

    /**
     * Function     setNumericLayer
     * Scope        Public
     * Description  Set OSK to numeric layer if it exists
     */
    ['setNumericLayer']() {
      this.core.keyboardProcessor.setNumericLayer(this.util.device.coreSpec);
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

    // Functions that might be added later
    ['beepKeyboard']: () => void;
    /**
     * @param {number}  dn  Number of pre-caret characters to delete
     * @param {string}  s   Text to insert
     * @param {number=}  dr  Number of post-caret characters to delete
     */
    ['oninserttext']: (dn: number, s: string, dr?: number) => void;

    /**
     * Create copy of the OSK that can be used for embedding in documentation or help
     * The currently active keyboard will be returned if PInternalName is null
     *
     *  @param  {string}          PInternalName   internal name of keyboard, with or without Keyboard_ prefix
     *  @param  {number}          Pstatic         static keyboard flag  (unselectable elements)
     *  @param  {string=}         argFormFactor   layout form factor, defaulting to 'desktop'
     *  @param  {(string|number)=}  argLayerId    name or index of layer to show, defaulting to 'default'
     *  @return {Object}                          DIV object with filled keyboard layer content
     */
    ['BuildVisualKeyboard'](PInternalName, Pstatic, argFormFactor, argLayerId): HTMLElement {
      return com.keyman.osk.VisualKeyboard.buildDocumentationKeyboard(PInternalName, Pstatic, argFormFactor, argLayerId);
    }
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

import KeymanBase = com.keyman.KeymanBase;

KeymanBase._srcPath = sPath;
KeymanBase._rootPath = sPath.replace(/(https?:\/\/)([^\/]*)(.*)/,'$1$2/');
KeymanBase._protocol = sPath.replace(/(.{3,5}:)(.*)/,'$1');

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
    window['keyman'] = com.keyman['singleton'] = com.keyman.singleton = new KeymanBase();

    // TODO:  Eliminate the need for this.  Will require more refactoring & redesign to drop.
    window['keyman'].core.languageProcessor.init();
  })();
}