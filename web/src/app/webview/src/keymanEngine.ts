import { DeviceSpec, DefaultRules } from 'keyman/engine/keyboard';
import { RuleBehavior } from 'keyman/engine/js-processor';
import { KeymanEngine as KeymanEngineBase, KeyboardInterface } from 'keyman/engine/main';
import { AnchoredOSKView, ViewConfiguration, StaticActivator } from 'keyman/engine/osk';
import { getAbsoluteX, getAbsoluteY } from 'keyman/engine/dom-utils';
import { toPrefixedKeyboardId, toUnprefixedKeyboardId } from 'keyman/engine/keyboard-storage';

import { WebviewConfiguration, WebviewInitOptionDefaults, WebviewInitOptionSpec } from './configuration.js';
import ContextManager, { ContextHost } from './contextManager.js';
import PassthroughKeyboard from './passthroughKeyboard.js';
import { buildEmbeddedGestureConfig, setupEmbeddedListeners } from './oskConfiguration.js';

export default class KeymanEngine extends KeymanEngineBase<WebviewConfiguration, ContextManager, PassthroughKeyboard> {
  // Ideally, we would be able to auto-detect `sourceUri`: https://stackoverflow.com/a/60244278.
  // But it's too new of a feature to utilize... and also expects to be in a module, when this may
  // be compiled down to an IIFE.
  constructor(worker: Worker, sourceUri: string) {
    const config = new WebviewConfiguration(sourceUri);  // currently set to perform device auto-detect.

    config.onRuleFinalization = (ruleBehavior: RuleBehavior) => {
      (this.context as ContextHost).updateHost(ruleBehavior.transcription);
    }

    config.stubNamespacer = (stub) => {
      // If the package has not yet been applied as namespacing...
      if(stub.KP && stub.KI.indexOf(`${stub.KP}::`) == -1) {
        // Apply namespacing.  To make 100% sure that we don't muck up internal prefixing,
        // we ensure it is applied consistently in the manner specified below.
        stub.KI = toPrefixedKeyboardId(`${stub.KP}::${toUnprefixedKeyboardId(stub.KI)}`);
      }
    }

    super(worker, config, new ContextManager(config), (engine) => {
      return {
        // The `engine` parameter cannot be supplied with the constructing instance before calling
        // `super`, hence the 'fun' rigging to supply it _from_ `super` via this closure.
        keyboardInterface: new KeyboardInterface(window, engine, config.stubNamespacer),
        defaultOutputRules: new DefaultRules()
      };
    });

    this.hardKeyboard = new PassthroughKeyboard(config.hardDevice);
  }

  async init(options: Required<WebviewInitOptionSpec>) {
    let device = new DeviceSpec(
      'native',
      options.embeddingApp.indexOf('Tablet') >= 0 ? 'tablet' : 'phone',
      this.config.hostDevice.OS,
      true
    );

    this.config.hostDevice = device;

    const totalOptions = {...WebviewInitOptionDefaults, ...options};
    await super.init(totalOptions);

    // There may be some valid mutations possible even on repeated calls?
    // The original seems to allow it.

    if(this.config.deferForInitialization.isResolved) {
      // abort!  Maybe throw an error, too.
      return Promise.resolve();
    }

    if(this.beepKeyboard) {
      this.core.keyboardProcessor.beepHandler = this.beepKeyboard;
    }

    this.contextManager.on('keyboardchange', (kbd) => {
      this.hardKeyboard.activeKeyboard = kbd?.keyboard;
    });

    this.contextManager.initialize();

    const oskConfig: ViewConfiguration = {
      hostDevice: this.config.hostDevice,
      pathConfig: this.config.paths,
      // When hosted in a WebView, we never hide the Web OSK without hiding the hosting WebView.
      activator: new StaticActivator(),
      embeddedGestureConfig: buildEmbeddedGestureConfig(this.config.softDevice),
      doCacheBusting: true,
      predictionContextManager: this.contextManager.predictionContext,
      heightOverride: this.getOskHeight,
      widthOverride: this.getOskWidth,
      isEmbedded: true
    };

    this.osk = new AnchoredOSKView(oskConfig);
    setupEmbeddedListeners(this, this.osk);

    this.config.finalizeInit();
  }

  // Functions that the old 'app/webview' equivalent had always provided to the WebView

  /**
   * refreshOskLayout handles rotation event -- currently rebuilds keyboard and adjusts font sizes
   */
  refreshOskLayout() {
    this.osk?.refreshLayout();
  };

  /**
   *  Return position of language menu key to KeymanTouch
   *
   *  @return  {string}      comma-separated x,y,w,h of language menu key
   *
   **/
  touchMenuPos() {
    const osk = this.osk;
    if(osk == null || osk.vkbd == null || osk.vkbd.lgKey == null) {
      return '';
    }

    let key: HTMLElement = osk.vkbd.lgKey;
    // A CSS change of kmd-key-square from position:fixed to position:static was needed
    // for Android 4.3 to display the OSK correctly, but resulted in the position of
    // the menu key not being returned correctly.  The following line gets the
    // key element, instead of the key-square element, fixes this.  It should be
    // removed again when the key-square elements are all removed as planned.
    const child = key.firstChild as HTMLElement;
    if(typeof child != 'undefined' && child != null && child.classList.contains('kmw-key')) {
      key = child;
    }

    var w=key.offsetWidth,
        h=key.offsetHeight,
        // Since the full OSKManager '_Box' is displayed within the keyboards' WebViews,
        // these calculations should be performed with respect to that, rather than osk.vkbd.kbdDiv.
        x = getAbsoluteX(key) - getAbsoluteX(osk._Box) + w/2,
        y = getAbsoluteY(key) - getAbsoluteY(osk._Box);

    return x+','+y+','+w+','+h;
  };

  showGlobeHint(text: string, onAutodismissal?: () => void) {
    const globeHint = this.osk?.vkbd?.globeHint;

    if(globeHint) {
      // Ensure localized text is properly in-place.
      globeHint.text = text;
      globeHint.show(this.osk.vkbd.currentLayer.globeKey.btn, onAutodismissal);
    }
  }

  hideGlobeHint() {
    this.osk?.vkbd?.globeHint?.hide(this.osk.vkbd.currentLayer.globeKey.btn);
  }

  // Properties set by the WebView hosting page
  beepKeyboard?: () => void = null;
  hideKeyboard?: () => void = null;
  menuKeyUp?: () => void = null;
  showKeyboardList?: () => void = null;
  getOskHeight?: () => number = null;
  getOskWidth?: () => number = null;

  get context() {
    return this.contextManager.activeTarget;
  }
}
