import { KeymanEngine as KeymanEngineBase } from 'keyman/engine/main';
import { Device as DeviceDetector } from 'keyman/engine/device-detect';
import { getAbsoluteY } from 'keyman/engine/dom-utils';
import { OutputTarget } from 'keyman/engine/element-wrappers';
import {
  TwoStateActivator,
  VisualKeyboard
} from 'keyman/engine/osk';
import { ErrorStub, KeyboardStub, CloudQueryResult, toPrefixedKeyboardId as prefixed } from 'keyman/engine/package-cache';
import { DeviceSpec, Keyboard, KeyboardObject } from "@keymanapp/keyboard-processor";

import * as views from './viewsAnchorpoint.js';
import { BrowserConfiguration, BrowserInitOptionDefaults, BrowserInitOptionSpec } from './configuration.js';
import { default as ContextManager } from './contextManager.js';
import DefaultBrowserRules from './defaultBrowserRules.js';
import HardwareEventKeyboard from './hardwareEventKeyboard.js';
import { FocusStateAPIObject } from './context/focusAssistant.js';
import { PageIntegrationHandlers } from './context/pageIntegrationHandlers.js';
import { LanguageMenu } from './languageMenu.js';
import { setupOskListeners } from './oskConfiguration.js';
import { whenDocumentReady } from './utils/documentReady.js';
import { outputTargetForElement } from 'keyman/engine/attachment';

import { UtilApiEndpoint} from './utilApiEndpoint.js';
import { UIModule } from './uiModuleInterface.js';
import { HotkeyManager } from './hotkeyManager.js';
import { BeepHandler } from './beepHandler.js';
import KeyboardInterface from './keyboardInterface.js';

export default class KeymanEngine extends KeymanEngineBase<BrowserConfiguration, ContextManager, HardwareEventKeyboard> {
  touchLanguageMenu?: LanguageMenu;
  private pageIntegration: PageIntegrationHandlers;

  private _initialized: number = 0;
  readonly _util: UtilApiEndpoint;

  private _ui: UIModule;
  hotkeyManager: HotkeyManager = new HotkeyManager();
  private readonly beepHandler: BeepHandler;


  // Properties sometimes set up by a hosting page
  getOskHeight?: () => number = null;
  getOskWidth?: () => number = null;

  /**
   * Provides a quick link to the base help page for Keyman keyboards.
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/helpURL
   */
  public readonly helpURL = 'https://help.keyman.com/go';

  keyEventRefocus = () => {
    this.contextManager.restoreLastActiveTarget();
  }

  constructor(worker: Worker, sourceUri: string) {
    const config = new BrowserConfiguration(sourceUri);  // currently set to perform device auto-detect.

    super(worker, config, new ContextManager(config, () => this.legacyAPIEvents), (engine) => {
      return {
        // The `engine` parameter cannot be supplied with the constructing instance before calling
        // `super`, hence the 'fun' rigging to supply it _from_ `super` via this closure.
        keyboardInterface: new KeyboardInterface(window, engine as KeymanEngine),
        defaultOutputRules: new DefaultBrowserRules(engine.contextManager)
      };
    });

    this._util = new UtilApiEndpoint(config);
    this.beepHandler = new BeepHandler(this.core.keyboardInterface);
    this.core.keyboardProcessor.beepHandler = () => this.beepHandler.beep(this.contextManager.activeTarget);

    this.hardKeyboard = new HardwareEventKeyboard(config.hardDevice, this.core.keyboardProcessor, this.contextManager);

    // Scrolls the document-body to ensure that a focused element remains visible after the OSK appears.
    this.contextManager.on('targetchange', (target) => {
      const e = (target as OutputTarget<any>)?.getElement();
      if(this.osk) {
        (this.osk.activationModel as TwoStateActivator<HTMLElement>).activationTrigger = e;
      }

      if(this.config.hostDevice.touchable && target) {
        this.ensureElementVisibility(e);
      }
    });
  }

  public ensureElementVisibility(e: HTMLElement) {
    if(!e || !this.osk) {
      return;
    }

    // Get the absolute position of the caret
    const y = getAbsoluteY(e);
    const t = window.pageYOffset;
    let dy = y-t;
    if(y >= t) {
      dy -= (window.innerHeight - this.osk._Box.offsetHeight - e.offsetHeight - 2);
      if(dy < 0) {
        dy=0;
      }
    }
    // Hide OSK, then scroll, then re-anchor OSK with absolute position (on end of scroll event)
    if(dy != 0) {
      window.scrollTo(0, dy + t);
    }
  }

  public get util() {
    return this._util;
  }

  public get views() {
    // NOT this.views.  Just... `views`, the import of viewsAnchorpoint.ts
    return views;
  }

  /**
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/initialized
   */
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
    if(this.config.deferForInitialization.isFulfilled) {
      module.initialize();
    }
  }

  /**
   * Function     Initialization
   * Scope        Public
   * @param       {com.keyman.OptionType}  arg     object specifying configuration properties for KeymanEngine + its resources
   *
   * Performs initialization of the KeymanEngine for Web, including:
   * - device-detection
   * - option configuration
   * - integration with the active page
   * - OSK-selection
   * - finalization for pre-loaded keyboard + stub registrations.
   *
   * It also self-defers if the page is not yet fully loaded; it will automatically await page
   * load and resume once page-load is complete.  (Certain initialization behaviors will only
   * proceed properly with a fully-loaded page.)
   *
   * @returns A Promise that only resolves once the engine is fully initialized.
   */
  public async init(options: Required<BrowserInitOptionSpec>) {
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
    if(this.config.deferForInitialization.isResolved) {
      // abort!  Maybe throw an error, too.
      return Promise.resolve();
    }

    // There may be some valid mutations possible even on repeated calls?
    // The original seems to allow it.

    await super.init(totalOptions);

    // Used by keymanweb.com; if no keyboard-cookie exists, we need this to trigger
    // default-stub selection on mobile devices so that a keyboard - and thus, the
    // globe key - are accessible.
    //
    // The `super` call above initializes `keyboardRequisitioner`, as needed here.
    this.keyboardRequisitioner.cloudQueryEngine.once('unboundregister', () => {
      if(!this.contextManager.activeKeyboard?.keyboard) {
        // Autoselects this.keyboardRequisitioner.cache.defaultStub, which will be
        // set to an actual keyboard on mobile devices.
        this.setActiveKeyboard('', '');
      }
    });

    this.contextManager.initialize();  // will seek to attach to the page, which requires document.body

    // Capture the saved-keyboard string now, before we load any keyboards/stubs
    // or do anything that would mutate the value.
    const savedKeyboardStr = this.contextManager.getSavedKeyboardRaw();

    // Automatically performs related handler setup & maintains references
    // needed for related cleanup / shutdown.
    this.pageIntegration = new PageIntegrationHandlers(window, this);
    this.config.finalizeInit();

    if(this.ui) {
      this.ui.initialize();
      this.legacyAPIEvents.callEvent('loaduserinterface', {});
    }

    this._initialized = 2;

    // Let any deferred, pre-init stubs complete registration
    await this.config.deferForInitialization;

    /*
      Attempt to restore the user's last-used keyboard from their previous session.
      The method auto-loads the default stub if one is available and the last-used keyboard
      has no registered stub.

      Note:  any cloud stubs will probably not be available yet.
      If we tracked cloud requests and awaited a Promise.all on pending queries,
      we could handle that too.
    */
    const loadingKbd: Promise<any> = this.contextManager.restoreSavedKeyboard(savedKeyboardStr);

    // Wait for the initial keyboard to load before setting the OSK; this will avoid building an
    // empty OSK that we'll instantly discard after.
    try {
      await loadingKbd;
    } catch { /* in case of failed fetch due to network error or bad URI; we must still let the OSK init. */ };

    const firstKbdConfig = {
      keyboardToActivate: this.contextManager.activeKeyboard
    };
    const osk = device.touchable ? new views.AnchoredOSKView(this, firstKbdConfig) : new views.FloatingOSKView(this, firstKbdConfig);

    setupOskListeners(this, osk, this.contextManager);
    // And, now that we have our loaded active keyboard - or failed, thus must use that default...
    // Now we set the OSK in place, an act which triggers VisualKeyboard construction.
    this.osk = osk;
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
  public getUIState(): FocusStateAPIObject {
    return this.contextManager.focusAssistant.getUIState();
  }

  /**
   * Set or clear the IsActivatingKeymanWebUI flag (exposed function)
   *
   * See https://help.keyman.com/DEVELOPER/ENGINE/WEB/16.0/reference/core/activatingUI
   *
   * @param       {(boolean|number)}  state  Activate (true,false)
   */
  public activatingUI(state: boolean | number) {
    this.contextManager.focusAssistant.setMaintainingFocus(!!state);
  }

  /**
   * Function     setKeyboardForControl
   * Scope        Public
   * @param       {Element}    Pelem    Control element
   * @param       {string|null=}    Pkbd     Keyboard (Clears the set keyboard if set to null.)
   * @param       {string|null=}     Plc      Language Code
   * Description  Set default keyboard for the control
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/setKeyboardForControl
   */
  public setKeyboardForControl(Pelem: HTMLElement, Pkbd?: string, Plc?: string) {
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
  /**
   * Function     getKeyboardForControl
   * Scope        Public
   * @param       {Element}    Pelem    Control element
   * @return      {string|null}         The independently-managed keyboard for the control.
   * Description  Returns the keyboard ID of the current independently-managed keyboard for this control.
   *              If it is currently following the global keyboard setting, returns null instead.
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/getKeyboardForControl
   */
  public getKeyboardForControl(Pelem: HTMLElement) {
    const target = outputTargetForElement(Pelem);
    return this.contextManager.getKeyboardStubForTarget(target).id;
  }

  // Is not currently published API... but it exists.
  /**
   * Function     getLanguageForControl
   * Scope        Public
   * @param       {Element}    Pelem    Control element
   * @return      {string|null}         The independently-managed keyboard for the control.
   * Description  Returns the language code used with the current independently-managed keyboard for this control.
   *              If it is currently following the global keyboard setting, returns null instead.
   */
  getLanguageForControl(Pelem: HTMLElement) {
    const target = outputTargetForElement(Pelem);
    return this.contextManager.getKeyboardStubForTarget(target).langId;
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
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/addKeyboards
   */
  public addKeyboards(...args: any[]): Promise<(KeyboardStub|ErrorStub)[]> {
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
          x = x.concat(args[0]);
        } else if (Array.isArray(args)) {
          x = x.concat(args);
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
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/addKeyboardsForLanguage
   **/
  public addKeyboardsForLanguage(arg: string[]|string) : Promise<(KeyboardStub|ErrorStub)[]> {
    return this.config.deferForInitialization.then(() => {
      if (typeof arg === 'string') {
        return this.keyboardRequisitioner.addLanguageKeyboards(arg.split(',').map(item => item.trim()));
      } else {
        return this.keyboardRequisitioner.addLanguageKeyboards(arg);
      }
    });
  }

  /**
   * Get an associative array of keyboard identification strings
   *   This was defined as an array, so is kept that way, but
   *   Javascript treats it as an object anyway
   *
   * This is a public API function documented at
   * https://help.keyman.com/developer/engine/web/current-version/reference/core/getKeyboard.
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
      // @ts-ignore
      CountryName: Lstub['KC'] as string,
      // @ts-ignore
      CountryCode: Lstub['KCC'] as string,
      // @ts-ignore
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
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/isCJK
   */
  public isCJK(k0?: KeyboardObject | ReturnType<KeymanEngine['_GetKeyboardDetail']> /* [b/c Toolbar UI]*/) {
    let kbd: Keyboard;
    if(k0) {
      let kbdDetail = k0 as ReturnType<KeymanEngine['_GetKeyboardDetail']>;
      if(kbdDetail.KeyboardID){
        kbd = this.keyboardRequisitioner.cache.getKeyboard(kbdDetail.KeyboardID);
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
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/getKeyboard
   **/
  public getKeyboard(PInternalName: string, PlgCode?: string) {
    const stub = this.keyboardRequisitioner.cache.getStub(PInternalName, PlgCode);
    const keyboard = this.keyboardRequisitioner.cache.getKeyboardForStub(stub);

    return stub && this._GetKeyboardDetail(stub, keyboard);
  }

  /**
   * Get API-friendly array of available keyboard stubs
   *
   * Refer to https://help.keyman.com/developer/engine/web/current-version/reference/core/getKeyboards.
   *
   * The type of each entry of the array corresponds to that of `getKeyboard`.
   *
   * @return   {Array}     Array of available keyboards
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/getKeyboards
   */
  public getKeyboards(): ReturnType<KeymanEngine['_GetKeyboardDetail']>[] {
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
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/removeKeyboards
   */
  public removeKeyboards(...x: string[]) {
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
   * Defaults to US English, but this needs to be user-set in later revision (TODO)
   *
   * @return      {string}          InternalName:LanguageCode
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/getSavedKeyboard
   **/
  public getSavedKeyboard(): string {
    return this.contextManager.getSavedKeyboard();
  }

  /**
   * Set focus to last active target element (browser-dependent)
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/focusLastActiveElement
   */
  public focusLastActiveElement() {
    this.contextManager.lastActiveTarget?.focus();
  }

  /**
   * Get the last active target element *before* KMW activated (I1297)
   *
   * @return      {Object}
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/getLastActiveElement
   */
  public getLastActiveElement() {
    return this.contextManager.lastActiveTarget?.getElement();
  }

  /**
   *  Set the active input element directly optionally setting focus
   *
   *  @param  {Object|string} e         element id or element
   *  @param  {boolean=}      setFocus  optionally set focus  (KMEW-123)
   **/
  setActiveElement(e: string|HTMLElement, setFocus?: boolean) {
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
   * Move focus to user-specified element
   *
   *  @param  {string|Object}   e   element or element id
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/moveToElement
   **/
  public moveToElement(e: string|HTMLElement) {
    if(typeof(e) == "string") { // Can't instanceof string, and String is a different type.
      e=document.getElementById(e);
    }

    e.focus();
  }

  /**
   * Function     addHotkey
   * Scope        Public
   * @param       {number}            keyCode
   * @param       {number}            shiftState
   * @param       {function(Object)}  handler
   * Description  Add hot key handler to array of document-level hotkeys triggered by key up event
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/addHotKey
   */
  public addHotKey(keyCode: number, shiftState: number, handler: () => void) {
    this.hotkeyManager.addHotKey(keyCode, shiftState, handler);
  }

  /**
   * Function     removeHotkey
   * Scope        Public
   * @param       {number}        keyCode
   * @param       {number}        shiftState
   * Description  Remove a hot key handler from array of document-level hotkeys triggered by key up event
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/removeHotKey
   */
  public removeHotKey(keyCode: number, shiftState: number) {
    this.hotkeyManager.removeHotkey(keyCode, shiftState);
  }

  /**
   * Function     attachToControl
   * Scope        Public
   * @param       {Element}    Pelem       Element to which KMW will be attached
   * Description  Attaches KMW to control (or IFrame)
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/attachToControl
   */
  public attachToControl(Pelem: HTMLElement) {
    this.contextManager.page.attachToControl(Pelem);
  }

  /**
   * Function     detachFromControl
   * Scope        Public
   * @param       {Element}    Pelem       Element from which KMW will detach
   * Description  Detaches KMW from a control (or IFrame)
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/detachFromControl
   */
  public detachFromControl(Pelem: HTMLElement) {
    this.contextManager.page.detachFromControl(Pelem);
  }

  /**
   * Function     disableControl
   * Scope        Public
   * @param       {Element}      Pelem       Element to be disabled
   * Description  Disables a KMW control element
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/disableControl
   */
  public disableControl(Pelem: HTMLElement) {
    this.contextManager.page.disableControl(Pelem);
  }

  /**
   * Function     enableControl
   * Scope        Public
   * @param       {Element}      Pelem       Element to be disabled
   * Description  Disables a KMW control element
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/enableControl
   */
  public enableControl(Pelem: HTMLMapElement) {
    this.contextManager.page.enableControl(Pelem);
  }

  /**
   * Create copy of the OSK that can be used for embedding in documentation or help
   * The currently active keyboard will be returned if PInternalName is null
   *
   *  @param  {string}          PInternalName   internal name of keyboard, with or without Keyboard_ prefix
   *  @param  {number}          Pstatic         static keyboard flag  (unselectable elements)
   *  @param  {string=}         argFormFactor   layout form factor, defaulting to 'desktop'
   *  @param  {(string)=}  argLayerId    name or index of layer to show, defaulting to 'default'
   *  @return {Object}                          DIV object with filled keyboard layer content
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/osk/BuildVisualKeyboard
   */
  public BuildVisualKeyboard(
    PInternalName: string,
    Pstatic: number,
    argFormFactor?: DeviceSpec.FormFactor,
    argLayerId?: string
  ): HTMLElement {
    let PKbd: Keyboard = null;

    if(PInternalName != null) {
      PKbd = this.keyboardRequisitioner.cache.getKeyboard(PInternalName);
    }

    PKbd = PKbd || this.core.activeKeyboard;
    let Pstub = this.keyboardRequisitioner.cache.getStub(PKbd);

    // help.keyman.com will set this function in place to specify the desired
    // dimensions for the documentation-keyboards, so we'll give it priority.  One of those
    // "temporary" (but actually permanent) solutions from yesteryear.
    //
    // Note that the main intended use of that function is for embedded KMW on the mobile apps...
    // but they never call `BuildVisualKeyboard`, so it's all good.
    const getOskHeight = this['getOskHeight'];
    let targetHeight = (typeof getOskHeight == 'function' ? getOskHeight() : null) || this.osk.computedHeight || 200;

    return VisualKeyboard.buildDocumentationKeyboard(
      PKbd,
      Pstub,
      this.config.paths,
      argFormFactor,
      argLayerId,
      targetHeight
      );
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
    this.util.shutdown(); // For tracked dom events, stylesheets.

    this.legacyAPIEvents.callEvent('unloaduserinterface', {});
    this.ui?.shutdown();
  }
}
