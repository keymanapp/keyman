import { KeymanEngine as KeymanEngineBase } from 'keyman/engine/main';
import { Device as DeviceDetector } from 'keyman/engine/device-detect';
import { getAbsoluteY } from 'keyman/engine/dom-utils';
import { OutputTarget } from 'keyman/engine/element-wrappers';
import { AnchoredOSKView, FloatingOSKView, FloatingOSKViewConfiguration, OSKView, TwoStateActivator } from 'keyman/engine/osk';
import { ErrorStub, KeyboardStub, CloudQueryResult } from 'keyman/engine/package-cache';
import { DeviceSpec, ProcessorInitOptions, extendString } from "@keymanapp/keyboard-processor";

import { BrowserConfiguration, BrowserInitOptionDefaults, BrowserInitOptionSpec } from './configuration.js';
import ContextManager from './contextManager.js';
import DefaultBrowserRules from './defaultBrowserRules.js';
import HardwareEventKeyboard from './hardwareEventKeyboard.js';
import { FocusStateAPIObject } from './context/focusAssistant.js';
import { PageIntegrationHandlers } from './context/pageIntegrationHandlers.js';
import { LanguageMenu } from './languageMenu.js';
import { setupOskListeners } from './oskConfiguration.js';
import { whenDocumentReady } from './utils/documentReady.js';

export default class KeymanEngine extends KeymanEngineBase<BrowserConfiguration, ContextManager, HardwareEventKeyboard> {
  touchLanguageMenu?: LanguageMenu;
  private pageIntegration: PageIntegrationHandlers;

  private _initialized: number = 0;

  keyEventRefocus = () => {
    this.contextManager.restoreLastActiveTarget();
  }

  constructor(worker: Worker, sourceUri: string) {
    const config = new BrowserConfiguration(sourceUri);  // currently set to perform device auto-detect.

    super(worker, config, new ContextManager(config, () => this.legacyAPIEvents));
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

  public get initialized() {
    return this._initialized;
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

    // // if an initialization has already completed.
    // if(!this.config.deferForInitialization.hasFinalized) {

    this.config.hostDevice = device;
    // Set any incoming options, overriding previous entries.
    this.config.initialize(totalOptions);

    // }                                                                     ***

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

    // Should use `isAttached` method once available.
    if(!Pelem._kmwAttachment) {
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
  private _GetKeyboardDetail = function(Lstub: KeyboardStub, Lkbd: any /* KeyboardScriptObject */) { // I2078 - Full keyboard detail
    var Lr={};
    Lr['Name'] = Lstub['KN'];
    Lr['InternalName'] =  Lstub['KI'];
    Lr['LanguageName'] = Lstub['KL'];  // I1300 - Add support for language names
    Lr['LanguageCode'] = Lstub['KLC']; // I1702 - Add support for language codes, region names, region codes, country names and country codes
    Lr['RegionName'] = Lstub['KR'];
    Lr['RegionCode'] = Lstub['KRC'];
    Lr['CountryName'] = Lstub['KC'];
    Lr['CountryCode'] = Lstub['KCC'];
    Lr['KeyboardID'] = Lstub['KD'];
    Lr['Font'] = Lstub['KFont'];
    Lr['OskFont'] = Lstub['KOskFont'];
    Lr['HasLoaded'] = !!Lkbd;

    Lr['IsRTL'] = Lkbd ? !!Lkbd['KRTL'] : null;
    return Lr;
  }

  /**
   * Get API-friendly array of available keyboard stubs
   *
   * @return   {Array}     Array of available keyboards
   *
   */
  getKeyboards() {
    const Lr = [];

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
