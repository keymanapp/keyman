import { KeymanEngine as KeymanEngineBase } from 'keyman/engine/main';
import { Configuration } from "keyman/engine/configuration";
import { AnchoredOSKView, ViewConfiguration, StaticActivator } from 'keyman/engine/osk';

import ContextManager from './contextManager.js';
import PassthroughKeyboard from './passthroughKeyboard.js';
import { buildEmbeddedGestureConfig, setupEmbeddedListeners } from './oskConfiguration.js';

export class KeymanEngine extends KeymanEngineBase<ContextManager, PassthroughKeyboard> {
  constructor(config: Configuration, worker: Worker) {
    // TODO:  set the old `namespacedID` function in a new Configuration property
    // for use within `KeyboardInterface.registerStub` when available.
    //
    // Or... just build it in and configure via boolean flag?
    super(config, worker, new ContextManager());

    this.hardKeyboard = new PassthroughKeyboard(config.hardDevice);
  }

  initialize() {
    super.initialize();

    const oskConfig: ViewConfiguration = {
      hostDevice: this.config.hostDevice,
      pathConfig: this.config.paths,
      // When hosted in a WebView, we never hide the Web OSK without hiding the hosting WebView.
      activator: new StaticActivator(),
      embeddedGestureConfig: buildEmbeddedGestureConfig(this.config.softDevice)
    }

    this.osk = new AnchoredOSKView(oskConfig);
    setupEmbeddedListeners(this, this.osk);
  }

  // Properties set by the WebView hosting page
  hideKeyboard?: () => void = null;
  menuKeyUp?: () => void = null;
}
