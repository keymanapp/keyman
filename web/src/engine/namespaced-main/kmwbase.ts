// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwexthtml.ts" />
// Defines the web-page interface object.
/// <reference path="singleton.ts" />
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
/// <reference path="osk/anchoredOskView.ts" />
/// <reference path="osk/floatingOskView.ts" />
/// <reference path="osk/inlinedOskView.ts" />
// Defines the model manager.
/// <reference path="text/prediction/modelManager.ts" />

/***
   KeymanWeb 14.0
   Copyright 2017-2021 SIL International
***/
namespace com.keyman {

  export enum SpacebarText {
    KEYBOARD = 'keyboard',
    LANGUAGE = 'language',
    LANGUAGE_KEYBOARD = 'languageKeyboard',
    BLANK = 'blank'
  };

  export interface OptionType {
    root?: string;
    resources?: string;
    keyboards?: string;
    fonts?: string;
    // attachType and ui are 100% ignored for embedded (app-WebView hosted) contexts.
    // They should only be expected for website-based KMW use.
    attachType?: 'auto' | 'manual' | ''; // If blank or undefined, attachType will be assigned to "auto" or "manual"
    ui?: string;
    setActiveOnRegister?: string; // TODO: Convert to boolean. Option loader needs to be able to receive this as a string or boolean

    // Determines the default text shown on the spacebar, if undefined, LANGUAGE_KEYBOARD
    spacebarText?: SpacebarText;

    // Determines whether or not KeymanWeb should display its own alert messages
    // Only relevant for website-based KMW use.
    useAlerts?: boolean;
  }

  export class KeymanBase {
    _MasterDocument = null;    // Document with controller (to allow iframes to distinguish local/master control)
    _HotKeys = [];             // Array of document-level hotkey objects
    warned = false;            // Warning flag (to prevent multiple warnings)
    baseFont = 'sans-serif';   // Default page font (utilized by the OSK)
    appliedFont = '';          // Chain of fonts to be applied to OSK elements
    srcPath = '';              // Path to folder containing executing keymanweb script
    rootPath = '';             // Path to server root
    protocol = '';             // Protocol used for the KMW script.
    mustReloadKeyboard = false;// Force keyboard refreshing even if already loaded
    globalKeyboard = null;     // Indicates the currently-active keyboard for controls without independent keyboard settings.
    globalLanguageCode = null; // Indicates the language code corresponding to `globalKeyboard`.
    isEmbedded = false;        // Indicates if the KeymanWeb instance is embedded within a mobile app.
                               // Blocks full page initialization when set to `true`.

    initialized: number;       // Signals the initialization state of the KeymanWeb system.
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
    ['osk']: com.keyman.osk.OSKView;
    ['ui']: any;
    keyboardManager: keyboards.KeyboardManager;
    domManager: dom.DOMManager;
    hotkeyManager: HotkeyManager;
    uiManager: UIManager; // half has been modularized as `focusAssistant`.
    core: text.InputProcessor;
    modelManager: text.prediction.ModelManager;

    touchAliasing: dom.DOMEventHandlers;

    // Defines default option values
    options: OptionType = {
      root: '',
      resources: '',
      keyboards: '',
      fonts: '',
      attachType: '',
      ui: null,
      setActiveOnRegister: 'true', // TODO: convert to boolean
      spacebarText: SpacebarText.LANGUAGE_KEYBOARD,

      // Determines whether or not KeymanWeb should display its own alert messages
      useAlerts: true
    };

    // Stub functions (defined later in code only if required)
    setDefaultDeviceOptions(opt: OptionType){}
    getStyleSheetPath(s){return s;}
    linkStylesheetResources(){}
    getKeyboardPath(f, p?){return f;}
    KC_(n, ln, Pelem){return '';}
    handleRotationEvents(){}
    /**
     * Legacy API function for touch-alias issue workarounds
     * Touch-aliases have been eliminated, though.
     *
     * This function is deprecated in 16.0, with plans for removal in 17.0.
     */
    ['alignInputs'](eleList?: HTMLElement[]){}
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

      this.core = new text.InputProcessor(this.util.device.coreSpec, {
        baseLayout: baseLayout,
        variableStoreSerializer: new dom.VariableStoreCookieSerializer()
      });

      // Used by the embedded apps.
      this['interface'] = this.core.keyboardInterface;

      this.modelManager = new text.prediction.ModelManager();
      this.osk = this['osk'] = null;

      // Load properties from their static variants.
      this['build'] = Number.parseInt(com.keyman.KEYMAN_VERSION.VERSION_PATCH, 10);
      this.srcPath = KeymanBase._srcPath;
      this.rootPath = KeymanBase._rootPath;
      this.protocol = KeymanBase._protocol;

      this['version'] = com.keyman.KEYMAN_VERSION.VERSION_RELEASE;
      this['helpURL'] = 'http://help.keyman.com/go';
      this.setInitialized(0);

      // Signals that a KMW load has occurred in order to prevent double-loading.
      this['loaded'] = true;
    }

    /**
     * Triggers a KeymanWeb engine shutdown to facilitate a full system reset.
     * This function is designed for use with KMW unit-testing, which reloads KMW
     * multiple times to test the different initialization paths.
     */
    ['shutdown']() {
      // Disable page focus/blur events, which can sometimes trigger and cause parallel KMW instances in testing.

      if(this.ui && this.ui.shutdown) {
        this.ui.shutdown();
      }

      dom.DOMEventHandlers.states = new dom.CommonDOMStates();
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
     * Exposed function to load keyboards by name. One or more arguments may be used
     *
     * @param {any[]} args keyboard name string or keyboard metadata JSON object
     * @returns {Promise<(KeyboardStub|ErrorStub)[]>} Promise of added keyboard/error stubs
     *
     */
    ['addKeyboards'](...args: any[]) :
        Promise<(com.keyman.keyboards.KeyboardStub|com.keyman.keyboards.ErrorStub)[]> {
      if (!args || !args[0] || args[0].length == 0) {
        // Get the cloud keyboard catalog
        return this.keyboardManager.keymanCloudRequest('',false).catch(error => {
          console.error(error);
          return Promise.reject([{error: error}]);
        });
      } else {
        let x: (string|com.keyman.keyboards.KeyboardStub)[] = [];
        if (Array.isArray(args[0])) {
          args[0].forEach(a =>
            x.push(a));
        } else if (Array.isArray(args)) {
          args.forEach(a =>
            x.push(a));
        } else {
          x.push(args);
        }
        return this.keyboardManager.addKeyboardArray(x);
      }
    }

    /**
     *  Add default keyboards for given language(s)
     *
     *  @param  {string|string[]}   arg    Language name (multiple arguments allowed)
     *  @returns {Promise<(KeyboardStub|ErrorStub)[]>} Promise of added keyboard/error stubs
     **/
    ['addKeyboardsForLanguage'](arg: string[]|string) : Promise<(com.keyman.keyboards.KeyboardStub|com.keyman.keyboards.ErrorStub)[]> {
      if (typeof arg === 'string') {
        return this.keyboardManager.addLanguageKeyboards(arg.split(',').map(item => item.trim()));
      } else {
        return this.keyboardManager.addLanguageKeyboards(arg);
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
     * Move focus to user-specified element
     *
     *  @param  {string|Object}   e   element or element id
     *
     **/
    ['moveToElement'](e: string|HTMLElement) {
      this.domManager.moveToElement(e);
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
      let PKbd: com.keyman.keyboards.Keyboard = null;

      if(PInternalName != null) {
        var p=PInternalName.toLowerCase().replace('keyboard_','');
        var keyboardsList = this.keyboardManager.keyboards;

        for(let Ln=0; Ln<keyboardsList.length; Ln++) {
          if(p == keyboardsList[Ln]['KI'].toLowerCase().replace('keyboard_','')) {
            // Requires the Keyboard wrapping object now.
            PKbd = new com.keyman.keyboards.Keyboard(keyboardsList[Ln]);
            break;
          }
        }
      }

      PKbd = PKbd || this.core.activeKeyboard;

      // help.keyman.com will (lkudingly) set this function in place to specify the desired
      // dimensions for the documentation-keyboards, so we'll give it priority.  One of those
      // "temporary" (but actually permanent) solutions from yesteryear.
      //
      // Note that the main intended use of that function is for embedded KMW on the mobile apps...
      // but they never call `BuildVisualKeyboard`, so it's all good.
      const getOskHeight = this['getOskHeight'];
      let targetHeight = (typeof getOskHeight == 'function' ? getOskHeight() : null) || this.osk.computedHeight || 200;

      return com.keyman.osk.VisualKeyboard.buildDocumentationKeyboard(PKbd, argFormFactor, argLayerId, targetHeight);
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
    window['keyman'].core.languageProcessor.init(); // is shifting to the constructor
  })();
}
