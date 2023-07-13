import { DefaultRules, DeviceSpec } from '@keymanapp/keyboard-processor'
import { KeymanEngine as KeymanEngineBase, KeyboardInterface } from 'keyman/engine/main';
import { AnchoredOSKView, ViewConfiguration, StaticActivator } from 'keyman/engine/osk';
import { getAbsoluteX, getAbsoluteY } from 'keyman/engine/dom-utils';
import { type KeyboardStub, toPrefixedKeyboardId, toUnprefixedKeyboardId } from 'keyman/engine/package-cache';

import { WebviewConfiguration, WebviewInitOptionDefaults, WebviewInitOptionSpec } from './configuration.js';
import ContextManager from './contextManager.js';
import PassthroughKeyboard from './passthroughKeyboard.js';
import { buildEmbeddedGestureConfig, setupEmbeddedListeners } from './oskConfiguration.js';
import { SubkeyDelegator } from './osk/subkeyDelegator.js';

export default class KeymanEngine extends KeymanEngineBase<WebviewConfiguration, ContextManager, PassthroughKeyboard> {
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
   * Function called by Android and iOS when a device-implemented keyboard popup
   * is displayed or hidden.  As this is controlled by the app, we use it as a
   * trigger for 'embedded'-mode gesture state management.
   *
   *  @param  {boolean}  isVisible
   *
   **/
  popupVisible(isVisible) {
    const osk = this.osk;
    if(!osk || !osk.vkbd) {
      return;
    }

    let gesture = osk.vkbd.subkeyGesture as SubkeyDelegator;
    let pendingLongpress = osk.vkbd.pendingSubkey;

    /*
    * If a longpress popup was visible, but is no longer, this means that the
    * associated longpress gesture was cancelled.  It is possible for the base
    * key to emit if selected at this time; detection of this is managed by
    * the `SubkeyDelegator` class.
    */
    if(!isVisible) {
      if(gesture) {
        gesture.resolve(null);
        osk.vkbd.subkeyGesture = null;
      } else if(pendingLongpress) {
        pendingLongpress.cancel();
        osk.vkbd.pendingSubkey = null;
      }
    }

    /*
    * If the popup was not visible, but now is, that means our previously-pending
    * longpress is now 'realized' (complete).  The OSK relies upon this state
    * information, which will be properly updated by `resolve`.
    *
    * Prominent uses of such state info helps prevent change of base key, key
    * previews, and key output from occurring while a subkey popup remains active.
    */
    if(isVisible && pendingLongpress) {
      // Fulfills the first-stage promise.
      pendingLongpress.resolve();
    }
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

  /**
   *  Accept an external key ID (from KeymanTouch) and pass to the keyboard mapping
   *
   *  @param  {string}  keyName   key identifier which could contain a display layer and a "functional" layer
   *                              e.g: 'shift-K_E+rightalt-shift'
   **/
  executePopupKey(keyName: string) {
    const vkbd = this.osk?.vkbd;
    let origArg = keyName;
    if(!this.contextManager.activeKeyboard || !vkbd) {
      return false;
    }

    /* Clear any pending (non-popup) key */
    vkbd.keyPending = null;

    // Changes for Build 353 to resolve KMEI popup key issues
    keyName=keyName.replace('popup-',''); //remove popup prefix if present (unlikely)

    // Regex for 'display layer'-'virtual key name'+'optional functional layer'
    // Can't just split on '-' because some layers like ctrl-shift contain it.
    // Virtual key name starts with T_, K_, or U_
    // matches[1]: displayLayer (not used)
    // matches[2]: keyId
    // matches[3]: optional functionalLayer
    let matches = keyName.match(/^(.+)-([TKU]_[^+]+)\+?(.+)?$/);
    if (matches == null) {
      return false;
    }
    keyName = matches[2] + (matches[3] ? '+' + matches[3] : '');

    // This should be set if we're within this method... but it's best to guard against nulls here, just in case.
    if(vkbd.subkeyGesture) {
      let gesture = vkbd.subkeyGesture as SubkeyDelegator;
      gesture.resolve(keyName);
      vkbd.subkeyGesture = null;
    } else {
      console.warn("No base key exists for the subkey being executed: '" + origArg + "'");
    }
  };

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
