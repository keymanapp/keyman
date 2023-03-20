import { DeviceSpec } from '@keymanapp/keyboard-processor'
import { KeymanEngine as KeymanEngineBase } from 'keyman/engine/main';
import { AnchoredOSKView, ViewConfiguration, StaticActivator } from 'keyman/engine/osk';
import { toPrefixedKeyboardId, toUnprefixedKeyboardId } from 'keyman/engine/package-cache';

import { WebviewConfiguration, WebviewInitOptionDefaults, WebviewInitOptionSpec } from './configuration.js';
import ContextManager from './contextManager.js';
import PassthroughKeyboard from './passthroughKeyboard.js';
import { buildEmbeddedGestureConfig, setupEmbeddedListeners } from './oskConfiguration.js';

export class KeymanEngine extends KeymanEngineBase<ContextManager, PassthroughKeyboard> {
  // Ideally, we would be able to auto-detect `sourceUri`: https://stackoverflow.com/a/60244278.
  // But it's too new of a feature to utilize... and also expects to be in a module, when this may
  // be compiled down to an IIFE.
  constructor(worker: Worker, sourceUri: string) {
    const config = new WebviewConfiguration(sourceUri);  // currently set to perform device auto-detect.
    config.stubNamespacer = (stub) => {
      // If the package has not yet been applied as namespacing...
      if(stub.KP && stub.KI.indexOf(`${stub.KP}::`) == -1) {
        // Apply namespacing.  To make 100% sure that we don't muck up internal prefixing,
        // we ensure it is applied consistently in the manner specified below.
        stub.KI = toPrefixedKeyboardId(`${stub.KP}::${toUnprefixedKeyboardId(stub.KI)}`);
      }
    }

    super(worker, config, new ContextManager(config));

    this.hardKeyboard = new PassthroughKeyboard(config.hardDevice);
  }

  init(options: Required<WebviewInitOptionSpec>) {
    let device = new DeviceSpec('native', options.embeddingApp.indexOf('Tablet') >= 0 ? 'tablet' : 'phone', this.config.hostDevice.OS, true);
    this.config.hostDevice = device;

    super.init({...WebviewInitOptionDefaults, ...options});

    this.contextManager.initialize();

    const oskConfig: ViewConfiguration = {
      hostDevice: this.config.hostDevice,
      pathConfig: this.config.paths,
      // When hosted in a WebView, we never hide the Web OSK without hiding the hosting WebView.
      activator: new StaticActivator(),
      embeddedGestureConfig: buildEmbeddedGestureConfig(this.config.softDevice),
      doCacheBusting: true,
      predictionContextManager: this.contextManager.predictionContext
    };

    this.osk = new AnchoredOSKView(oskConfig);
    setupEmbeddedListeners(this, this.osk);
  }

  // Properties set by the WebView hosting page
  hideKeyboard?: () => void = null;
  menuKeyUp?: () => void = null;
}
