import { KeymanEngine as KeymanEngineBase } from 'keyman/engine/main';
import { Device as DeviceDetector } from 'keyman/engine/device-detect';
import { getAbsoluteY } from 'keyman/engine/dom-utils';
import { OutputTarget } from 'keyman/engine/element-wrappers';
import { AnchoredOSKView, FloatingOSKView, FloatingOSKViewConfiguration, OSKView } from 'keyman/engine/osk';
import { DeviceSpec, ProcessorInitOptions, extendString } from "@keymanapp/keyboard-processor";

import { BrowserConfiguration, BrowserInitOptionDefaults, BrowserInitOptionSpec } from './configuration.js';
import ContextManager from './contextManager.js';
import DefaultBrowserRules from './defaultBrowserRules.js';
import HardwareEventKeyboard from './hardwareEventKeyboard.js';
import { FocusStateAPIObject } from './context/focusAssistant.js';
import { PageIntegrationHandlers } from './context/pageIntegrationHandlers.js';
import { LanguageMenu } from './languageMenu.js';
import { setupOskListeners } from './oskConfiguration.js';

export class KeymanEngine extends KeymanEngineBase<BrowserConfiguration, ContextManager, HardwareEventKeyboard> {
  touchLanguageMenu?: LanguageMenu;
  private pageIntegration: PageIntegrationHandlers;

  keyEventRefocus = () => {
    this.contextManager.restoreLastActiveTarget();
  }

  constructor(worker: Worker, sourceUri: string) {
    const config = new BrowserConfiguration(sourceUri);  // currently set to perform device auto-detect.

    super(worker, config, new ContextManager(config, () => this.legacyAPIEvents));
    this.hardKeyboard = new HardwareEventKeyboard(config.hardDevice, this.core.keyboardProcessor, this.contextManager);

    // Scrolls the document-body to ensure that a focused element remains visible after the OSK appears.
    this.contextManager.on('targetchange', (target: OutputTarget<any>) => {
      if(this.config.hostDevice.touchable) {
        if(!target || !this.osk) {
          return;
        }

        const e = target.getElement();

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

  protected processorConfiguration(): ProcessorInitOptions {
    return {
      keyboardInterface: this.interface,
      defaultOutputRules: new DefaultBrowserRules(this.contextManager)
    };
  };

  async init(options: Required<BrowserInitOptionSpec>) {
    let deviceDetector = new DeviceDetector();
    let device = deviceDetector.detect();

    this.config.hostDevice = device;

    const totalOptions = {...BrowserInitOptionDefaults, ...options};
    super.init(totalOptions);
    this.config.initialize(totalOptions);

    // There may be some valid mutations possible even on repeated calls?
    // The original seems to allow it.

    if(this.config.deferForInitialization.hasFinalized) {
      // abort!  Maybe throw an error, too.
      return Promise.resolve();
    }

    this.contextManager.initialize();
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
