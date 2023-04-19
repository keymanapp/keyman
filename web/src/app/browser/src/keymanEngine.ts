import { KeymanEngine as KeymanEngineBase } from 'keyman/engine/main';
import { Device as DeviceDetector } from 'keyman/engine/device-detect';
import { AnchoredOSKView, FloatingOSKView, FloatingOSKViewConfiguration } from 'keyman/engine/osk';
import { DeviceSpec, ProcessorInitOptions } from "@keymanapp/keyboard-processor";

import { BrowserConfiguration, BrowserInitOptionDefaults, BrowserInitOptionSpec } from './configuration.js';
import ContextManager from './contextManager.js';
import DefaultBrowserRules from './defaultBrowserRules.js';
import KeyEventKeyboard from './keyEventKeyboard.js';
import { FocusStateAPIObject } from './context/focusAssistant.js';
import { setupOskListeners } from './oskConfiguration.js';

export class KeymanEngine extends KeymanEngineBase<ContextManager, KeyEventKeyboard> {
  constructor(worker: Worker, config: BrowserConfiguration) {
    super(worker, config, new ContextManager(config));
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

    if(device.touchable) {
      this.osk = new AnchoredOSKView(oskConfig);
    } else {
      this.osk = new FloatingOSKView(oskConfig);
    }

    setupOskListeners(this, this.osk, this.contextManager);

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
}
