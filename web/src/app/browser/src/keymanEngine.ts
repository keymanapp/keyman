import { KeymanEngine as KeymanEngineBase } from 'keyman/engine/main';
import { Device as DeviceDetector } from 'keyman/engine/device-detect';
import { getAbsoluteY } from 'keyman/engine/dom-utils';
import { OutputTarget } from 'keyman/engine/element-wrappers';
import { AnchoredOSKView, FloatingOSKView, FloatingOSKViewConfiguration, OSKView, TwoStateActivator } from 'keyman/engine/osk';
import { ErrorStub, KeyboardStub, CloudQueryResult, toPrefixedKeyboardId as prefixed } from 'keyman/engine/package-cache';
import { DeviceSpec, Keyboard, ProcessorInitOptions, extendString } from "@keymanapp/keyboard-processor";

import { BrowserConfiguration, BrowserInitOptionDefaults, BrowserInitOptionSpec } from './configuration.js';
import { default as ContextManager } from './contextManager.js';
import DefaultBrowserRules from './defaultBrowserRules.js';
import HardwareEventKeyboard from './hardwareEventKeyboard.js';
import { FocusStateAPIObject } from './context/focusAssistant.js';
import { PageIntegrationHandlers } from './context/pageIntegrationHandlers.js';
import { LanguageMenu } from './languageMenu.js';
import { setupOskListeners } from './oskConfiguration.js';
import { whenDocumentReady } from './utils/documentReady.js';
import { outputTargetForElement } from '../../../../build/engine/attachment/obj/outputTargetForElement.js';

import { UtilApiEndpoint} from './utilApiEndpoint.js';
import { UIModule } from './uiModuleInterface.js';
import { HotkeyManager } from './hotkeyManager.js';

export default class KeymanEngine extends KeymanEngineBase<BrowserConfiguration, ContextManager, HardwareEventKeyboard> {
  touchLanguageMenu?: LanguageMenu;
  private pageIntegration: PageIntegrationHandlers;

  private _initialized: number = 0;
  readonly _util: UtilApiEndpoint;

  private _ui: UIModule;
  hotkeyManager: HotkeyManager = new HotkeyManager();

  keyEventRefocus = () => {
    this.contextManager.restoreLastActiveTarget();
  }

  constructor(worker: Worker, sourceUri: string) {
    const config = new BrowserConfiguration(sourceUri);  // currently set to perform device auto-detect.

    super(worker, config, new ContextManager(config, () => this.legacyAPIEvents));
    this._util = new UtilApiEndpoint(config);

    this.hardKeyboard = new HardwareEventKeyboard(config.hardDevice, this.core.keyboardProcessor, this.contextManager);

    // Scrolls the document-body to ensure that a focused element remains visible after the OSK appears.
    this.contextManager.on('targetchange', (target: OutputTarget<any>) => {
      const e = target?.getElement();
      (this.osk.activationModel as TwoStateActivator<HTMLElement>).activationTrigger = e;

      if(this.config.hostDevice.touchable) {
        if(!target || !this.osk) {
          return;
        }

        const e = target?.getElement();

        // Get the absolute position of the caret
        const y = getAbsoluteY(e);
        const t = window.pageYOffset;
        let dy = 0;
        if(y < t) {
          dy=y-t;
        } else {
          dy=y-t-(window.innerHeight - this.osk._Box.offsetHeight-e.offsetHeight-2);
          if(dy < 0) {
            dy=0;
          }
        }
        // Hide OSK, then scroll, then re-anchor OSK with absolute position (on end of scroll event)
        if(dy != 0) {
          window.scrollTo(0, dy + t);
        }
      }
    });
  }

  public get util() {
    return this._util;
  }

  public get initialized() {
    return this._initialized;
  }

  public get ui() {
    return this._ui;
  }

  public set ui(module: UIModule) {
    if(this._ui) {
      this._ui.shutdown();
    }

    this._ui = module;
    if(this.config.deferForInitialization.hasResolved) {
      module.initialize();
    }
  }

  protected processorConfiguration(): ProcessorInitOptions {
    return {
      keyboardInterface: this.interface,
      defaultOutputRules: new DefaultBrowserRules(this.contextManager)
    };
  };

  async init(options: Required<BrowserInitOptionSpec>) {
    let deviceDetector = new DeviceDetector();
    let device = deviceDetector.detect();

    const totalOptions = {...BrowserInitOptionDefaults, ...options};

    // // Possible condition we can add:  no change of init options after a  ***
    // // prior finalized init.

    this.config.hostDevice = device;
    // Set any incoming options, overriding previous entries.
    this.config.initialize(totalOptions);

    this._initialized = 1;

    // Must wait for document load for further initialization.
    await whenDocumentReady();

    // Deferred keyboard loading + shortcutting if a different init call on the engine has
    // already fully resolved.
    if(this.config.deferForInitialization.hasFinalized) {
      // abort!  Maybe throw an error, too.
      return Promise.resolve();
    }

    // There may be some valid mutations possible even on repeated calls?
    // The original seems to allow it.

    await super.init(totalOptions);

    this.contextManager.initialize();  // will seek to attach to the page, which requires document.body

    const oskConfig: FloatingOSKViewConfiguration = {
      hostDevice: this.config.hostDevice,
      pathConfig: this.config.paths,
      predictionContextManager: this.contextManager.predictionContext,
      isEmbedded: false
    };

    let osk: OSKView;
    if(device.touchable) {
      this.osk = new AnchoredOSKView(oskConfig);
    } else {
      this.osk = new FloatingOSKView(oskConfig);
    }

    setupOskListeners(this, this.osk, this.contextManager);

    // Automatically performs related handler setup & maintains references
    // needed for related cleanup / shutdown.
    this.pageIntegration = new PageIntegrationHandlers(window, this);

    // Initialize supplementary plane string extensions
    String.kmwEnableSupplementaryPlane(true);
    this.config.finalizeInit();

    if(this.ui) {
      this.ui.initialize();
    }

    this._initialized = 2;
  }

  get register(): (x: CloudQueryResult) => void {
    return this.keyboardRequisitioner.cloudQueryEngine.registerFromCloud;
  }

  /**
   * Function     getUIState
   * Scope        Public
   * @return      {Object.<string,(boolean|number)>}
   * Description  Return object with activation state of UI:
   *                activationPending (bool):   KMW being activated
   *                activated         (bool):   KMW active
   *
   * See https://help.keyman.com/DEVELOPER/ENGINE/WEB/16.0/reference/core/getUIState
   */
  getUIState(): FocusStateAPIObject {
    return this.contextManager.focusAssistant.getUIState();
  }

  /**
   * Set or clear the IsActivatingKeymanWebUI flag (exposed function)
   *
   * See https://help.keyman.com/DEVELOPER/ENGINE/WEB/16.0/reference/core/activatingUI
   *
   * @param       {(boolean|number)}  state  Activate (true,false)
   */
  activatingUI(state: boolean | number) {
    this.contextManager.focusAssistant.setMaintainingFocus(!!state);
  }

  /**
   * Function     setKeyboardForControl
   * Scope        Public
   * @param       {Element}    Pelem    Control element
   * @param       {string|null=}    Pkbd     Keyboard (Clears the set keyboard if set to null.)
   * @param       {string|null=}     Plc      Language Code
   * Description  Set default keyboard for the control
   */
  setKeyboardForControl(Pelem: HTMLElement, Pkbd?: string, Plc?: string) {
    if(Pelem instanceof Pelem.ownerDocument.defaultView.HTMLIFrameElement) {
      console.warn("'keymanweb.setKeyboardForControl' cannot set keyboard on iframes.");
      return;
    }

    if(!this.isAttached(Pelem)) {
      console.error("KeymanWeb is not attached to element " + Pelem);
      return;
    }

    let stub = null;
    if(Pkbd) {
      stub = this.keyboardRequisitioner.cache.getStub(Pkbd, Plc);
      if(!stub) {
        throw new Error(`No keyboard has been registered with id ${Pkbd} and language code ${Plc}.`);
      }
    }

    this.contextManager.setKeyboardForTarget(Pelem._kmwAttachment.interface, Pkbd, Plc);
  }

  isAttached(x: HTMLElement): boolean {
    return this.contextManager.page.isAttached(x);
  }

  /**
   * Exposed function to load keyboards by name. One or more arguments may be used
   *
   * @param {any[]} args keyboard name string or keyboard metadata JSON object
   * @returns {Promise<(KeyboardStub|ErrorStub)[]>} Promise of added keyboard/error stubs
   *
   */
  addKeyboards(...args: any[]): Promise<(KeyboardStub|ErrorStub)[]> {
    return this.config.deferForInitialization.then(() => {
      if (!args || !args[0] || args[0].length == 0) {
        // Get the cloud keyboard catalog
        return this.keyboardRequisitioner.fetchCloudCatalog().catch((errVal) => {
          console.error(errVal[0].error);
          return errVal;
        });
      } else {
        let x: (string|KeyboardStub)[] = [];
        if (Array.isArray(args[0])) {
          args[0].forEach(a =>
            x.push(a));
        } else if (Array.isArray(args)) {
          args.forEach(a =>
            x.push(a));
        } else {
          x.push(args);
        }
        return this.keyboardRequisitioner.addKeyboardArray(x);
      }
    })
  }

  /**
   *  Add default keyboards for given language(s)
   *
   *  @param  {string|string[]}   arg    Language name (multiple arguments allowed)
   *  @returns {Promise<(KeyboardStub|ErrorStub)[]>} Promise of added keyboard/error stubs
   **/
  ['addKeyboardsForLanguage'](arg: string[]|string) : Promise<(KeyboardStub|ErrorStub)[]> {
    if (typeof arg === 'string') {
      return this.keyboardRequisitioner.addLanguageKeyboards(arg.split(',').map(item => item.trim()));
    } else {
      return this.keyboardRequisitioner.addLanguageKeyboards(arg);
    }
  }

  /**
   * Get an associative array of keyboard identification strings
   *   This was defined as an array, so is kept that way, but
   *   Javascript treats it as an object anyway
   *
   * @param       {Object}    Lstub      Keyboard stub object
   * @param       {Object}    Lkbd       Keyboard script object
   * @return      {Object}               Copy of keyboard identification strings
   *
   */
  private _GetKeyboardDetail = function(Lstub: KeyboardStub, Lkbd: Keyboard) { // I2078 - Full keyboard detail
    let Lr = {
      Name: Lstub.KN,
      InternalName: Lstub.KI,
      LanguageName: Lstub.KL,  // I1300 - Add support for language names
      LanguageCode: Lstub.KLC, // I1702 - Add support for language codes, region names, region codes, country names and country codes
      RegionName: Lstub.KR,
      RegionCode: Lstub.KRC,
      CountryName: Lstub['KC'] as string,
      CountryCode: Lstub['KCC'] as string,
      KeyboardID: Lstub['KD'] as string,
      Font: Lstub.KFont,
      OskFont: Lstub.KOskFont,
      HasLoaded: !!Lkbd,
      IsRTL: Lkbd ? Lkbd.isRTL : null
    };

    return Lr;
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
  isCJK(k0? /* keyboard script object | return-type of _GetKeyboardDetail [b/c Toolbar UI]*/) {
    let kbd: Keyboard;
    if(k0) {
      let kbdDetail = k0 as ReturnType<KeymanEngine['_GetKeyboardDetail']>;
      if(kbdDetail.KeyboardID){
        kbd = this.keyboardRequisitioner.cache.getKeyboard(k0.KeyboardID);
      } else {
        kbd = new Keyboard(k0);
      }
    } else {
      kbd = this.core.activeKeyboard;
    }

    return kbd && kbd.isCJK;
  }

  /**
   * Get keyboard meta data for the selected keyboard and language
   *
   * @param       {string}    PInternalName     Internal name of keyboard
   * @param       {string=}   PlgCode           language code
   * @return      {Object}                      Details of named keyboard
   *
   **/
  getKeyboard(PInternalName: string, PlgCode?: string) {
    const stub = this.keyboardRequisitioner.cache.getStub(PInternalName, PlgCode);
    const keyboard = this.keyboardRequisitioner.cache.getKeyboardForStub(stub);

    return this._GetKeyboardDetail(stub, keyboard);
  }

  /**
   * Get API-friendly array of available keyboard stubs
   *
   * @return   {Array}     Array of available keyboards
   *
   */
  getKeyboards() {
    const Lr: ReturnType<KeymanEngine['_GetKeyboardDetail']>[] = [];

    const cache = this.keyboardRequisitioner.cache;
    const keyboardStubs = cache.getStubList()
    for(let Ln=0; Ln < keyboardStubs.length; Ln++) { // I1511 - array prototype extended
      const Lstub = keyboardStubs[Ln];

      // In Chrome, (including on Android), Array.prototype.find() requires Chrome 45.
      // This is a later version than the default on our oldest-supported Android devices.
      const Lkbd = cache.getKeyboardForStub(Lstub);
      const Lrn = this._GetKeyboardDetail(Lstub, Lkbd);  // I2078 - Full keyboard detail
      Lr.push(Lrn);
    }
    return Lr;
  }

  /**
   * Build 362: removeKeyboards() remove keyboard from list of available keyboards
   *
   * @param {string}  x      keyboard name string
   *
   */
  removeKeyboards(...x: string[]) {
    for(let i=0; i < x.length; i++) {
      // This will completely forget the keyboard, requiring an async load operation to restore it again.
      // `true` is responsible for this & is required to pass a variable-store unit test.
      this.keyboardRequisitioner.cache.forgetKeyboard(x[i], true);

      if(this.contextManager.activeKeyboard?.metadata.id == prefixed(x[i])) {
        this.contextManager.activateKeyboard('', '');
      }
    }

    return true;
  }

  /**
   * Gets the cookie for the name and language code of the most recently active keyboard
   *
   *  Defaults to US English, but this needs to be user-set in later revision (TODO)
   *
   * @return      {string}          InternalName:LanguageCode
   **/
  getSavedKeyboard(): string {
    return this.contextManager.getSavedKeyboard();
  }

  /**
   * Set focus to last active target element (browser-dependent)
   */
  focusLastActiveElement() {
    this.contextManager.lastActiveTarget.focus();
  }

  /**
   * Get the last active target element *before* KMW activated (I1297)
   *
   * @return      {Object}
   */
  getLastActiveElement() {
    return this.contextManager.lastActiveTarget.getElement();
  }

  /**
   *  Set the active input element directly optionally setting focus
   *
   *  @param  {Object|string} e         element id or element
   *  @param  {boolean=}      setFocus  optionally set focus  (KMEW-123)
   **/
  setActiveElement(e: string|HTMLElement, setFocus: boolean) {
    if(typeof e == 'string') {
      const id = e;
      e = document.getElementById(e);

      if(!e) {
        throw new Error(`Could not find the specified element (id: ${id}`);
      }
    }

    const target = outputTargetForElement(e);
    if(!target) {
      throw new Error(`KMW is not attached to the specified element (id: ${e.id}).`);
    }
    this.contextManager.setActiveTarget(target, setFocus);
  }


  /**
   * Function     addHotkey
   * Scope        Public
   * @param       {number}            keyCode
   * @param       {number}            shiftState
   * @param       {function(Object)}  handler
   * Description  Add hot key handler to array of document-level hotkeys triggered by key up event
   */
  addHotKey(keyCode: number, shiftState: number, handler: () => void) {
    this.hotkeyManager.addHotKey(keyCode, shiftState, handler);
  }

  /**
   * Function     removeHotkey
   * Scope        Public
   * @param       {number}        keyCode
   * @param       {number}        shiftState
   * Description  Remove a hot key handler from array of document-level hotkeys triggered by key up event
   */
  removeHotKey(keyCode: number, shiftState: number) {
    this.hotkeyManager.removeHotkey(keyCode, shiftState);
  }

  /**
   * Detaches all KMW event handlers attached by this instance of the engine and releases
   * other related resources as appropriate.
   *
   * The primary use of this method is to facilitate a clean transition between engine
   * instances during integration testing.  The goal is to prevent interactions intended
   * for the 'current' instance from being accidentally intercepted by a discarded one.
   */
  shutdown() {
    this.pageIntegration.shutdown();
    this.contextManager.shutdown();
    this.osk?.shutdown();
    this.core.languageProcessor.shutdown();
    this.hardKeyboard.shutdown();
  }
}
