import { DefaultRules, type Keyboard, KeyboardKeymanGlobal, ProcessorInitOptions } from "@keymanapp/keyboard-processor";
import { DOMKeyboardLoader as KeyboardLoader } from "@keymanapp/keyboard-processor/dom-keyboard-loader";
import { InputProcessor, PredictionContext } from "@keymanapp/input-processor";
import { OSKView } from "keyman/engine/osk";
import { KeyboardRequisitioner, type KeyboardStub, ModelCache, ModelSpec } from "keyman/engine/package-cache";

import { EngineConfiguration, InitOptionSpec } from "./engineConfiguration.js";
import KeyboardInterface from "./keyboardInterface.js";
import { ContextManagerBase } from "./contextManagerBase.js";
import { KeyEventHandler } from './keyEventSource.interface.js';
import HardKeyboardBase from "./hardKeyboard.js";
import { LegacyAPIEventEngine } from "./legacyAPIEvents.js";
import DOMCloudRequester from "keyman/engine/package-cache/dom-requester";

export default class KeymanEngine<
  ContextManager extends ContextManagerBase,
  HardKeyboard extends HardKeyboardBase
> implements KeyboardKeymanGlobal {
  readonly config: EngineConfiguration;
  readonly contextManager: ContextManager;
  readonly interface: KeyboardInterface;
  readonly processor: InputProcessor;
  readonly keyboardRequisitioner: KeyboardRequisitioner;
  readonly modelCache: ModelCache;

  private legacyAPIEvents = new LegacyAPIEventEngine();
  private _hardKeyboard: HardKeyboard;
  private _osk: OSKView;

  private keyEventListener: KeyEventHandler = (event, callback) => {
    const outputTarget = this.contextManager.activeTarget;

    if(!this.contextManager.activeKeyboard || !outputTarget) {
      if(callback) {
        callback(null, null);
      }
    }

    //... probably only applies for physical keystrokes.
    if(!event.isSynthetic) {
      if(this.osk?.vkbd?.keyPending) {
        this.osk.vkbd.keyPending = null;
      }
    } else {
      // Do anything needed to guarantee that the outputTarget stays active (`browser`: maintains focus).
    }

    try {
      // Clear any cached codepoint data; we can rebuild it if it's unchanged.
      outputTarget.invalidateSelection();
      // Deadkey matching continues to be troublesome.
      // Deleting matched deadkeys here seems to correct some of the issues.   (JD 6/6/14)
      outputTarget.deadkeys().deleteMatched();      // Delete any matched deadkeys before continuing

      const result = this.processor.processKeyEvent(event, outputTarget);
      if(callback) {
        callback(result, null);
      }
    } catch (err) {
      if(callback) {
        callback(null, err);
      }
    }
  };

  // Should be overwritten as needed by engine subclasses; `browser` should set its DefaultOutput subclass in place.
  protected processorConfiguration(): ProcessorInitOptions {
    return {
      keyboardInterface: this.interface,
      defaultOutputRules: new DefaultRules()
    };
  };

  //

  /**
   * @param worker  A configured WebWorker to serve as the predictive-text engine's main thread.
   *                Available in the following variants:
   *                - sourcemapped, unminified (debug)
   *                - non-sourcemapped + minified (release)
   * @param config
   * @param contextManager
   */
  constructor(worker: Worker, config: EngineConfiguration, contextManager: ContextManager) {
    this.config = config;
    this.contextManager = contextManager;

    // Since we're not sandboxing keyboard loads yet, we just use `window` as the jsGlobal object.
    this.interface = new KeyboardInterface(window, this, this.contextManager, config.stubNamespacer);
    const keyboardLoader = new KeyboardLoader(this.interface, config.applyCacheBusting);
    this.keyboardRequisitioner = new KeyboardRequisitioner(keyboardLoader, new DOMCloudRequester(), this.config.paths);
    this.modelCache = new ModelCache();

    const kbdCache = this.keyboardRequisitioner.cache;
    this.interface.setKeyboardCache(this.keyboardRequisitioner.cache);

    this.processor = new InputProcessor(config.hostDevice, worker, this.processorConfiguration());

    this.contextManager.configure({
      resetKeyState: (target) => {
        this.processor.keyboardProcessor.resetContext(target);
      },
      predictionContext: new PredictionContext(this.processor.languageProcessor, this.processor.keyboardProcessor)
    });

    // TODO:  configure that context-manager!

    // #region Event handler wiring
    kbdCache.on('stubAdded', (stub) => {
      let eventRaiser = () => {
        // The corresponding event is needed in order to update UI modules as new keyboard stubs "come online".
        this.legacyAPIEvents.emit('kmw.keyboardregistered', {
          internalName: stub.KI,
          language: stub.KL,
          keyboardName: stub.KN,
          languageCode: stub.KLC,
          package: stub.KP
        });
      }

      if(this.config.deferForInitialization.hasFinalized) {
        eventRaiser();
      } else {
        this.config.deferForInitialization.then(eventRaiser);
      }
    });

    kbdCache.on('keyboardAdded', (keyboard) => {
      let eventRaiser = () => {
        // Execute any external (UI) code needed after loading keyboard
        this.legacyAPIEvents.emit('kmw.keyboardloaded', {
          keyboardName: keyboard.id
        });
      }

      if(this.config.deferForInitialization.hasFinalized) {
        eventRaiser();
      } else {
        this.config.deferForInitialization.then(eventRaiser);
      }
    });

    contextManager.on('keyboardchange', (kbd) => {
      this.refreshModel();

      if(this.osk) {
        this.osk.setNeedsLayout();
        this.osk.activeKeyboard = kbd;
      }
    });
    // #endregion
  }

  init(optionSpec: Required<InitOptionSpec>): void {
    // There may be some valid mutations possible even on repeated calls?
    // The original seems to allow it.

    if(this.config.deferForInitialization.hasFinalized) {
      // abort!  Maybe throw an error, too.
      return;
    }

    this.config.initialize(optionSpec);
  }

  public get hardKeyboard(): HardKeyboard {
    return this._hardKeyboard;
  }

  protected set hardKeyboard(keyboard: HardKeyboard) {
    if(this._hardKeyboard) {
      this._hardKeyboard.off('keyEvent', this.keyEventListener);
    }
    this._hardKeyboard = keyboard;
    keyboard.on('keyEvent', this.keyEventListener);
  }

  public get osk(): OSKView {
    return this._osk;
  }

  public set osk(value: OSKView) {
    if(this._osk) {
      this._osk.off('keyEvent', this.keyEventListener);
    }
    this._osk = value;
    this._osk.on('keyEvent', this.keyEventListener);
  }

  public getDebugInfo(): Record<string, any> {
    const report = {
      configReport: this.config.debugReport()
      // oskType / oskReport?
      // - mode
      // - dimensions
      // other possible entries?
    };

    return report;
  }

  private refreshModel() {
    const kbd = this.contextManager.activeKeyboard;
    const model = this.modelCache.modelForLanguage(kbd.metadata.langId);

    if(this.processor.activeModel != model) {
      if(this.processor.activeModel) {
        this.processor.languageProcessor.unloadModel();
      }

      if(model) {
        this.processor.languageProcessor.loadModel(model);
      }
    }
  }

  // API methods

  // 17.0: new!  Only used by apps utilizing app/webview and one app/browser test page.
  addModel(model: ModelSpec) {
    this.modelCache.register(model);

    if(model.languages.indexOf(this.contextManager.activeKeyboard.metadata.langId) != -1) {
      this.refreshModel();
    }
  }

  // 17.0: new!  Only used by apps utilizing app/webview and one app/browser test page.
  removeModel(modelId: string) {
    this.modelCache.deregister(modelId);

    // Is it the active model?
    if(this.processor.activeModel && this.processor.activeModel.id == modelId) {
      this.processor.languageProcessor.unloadModel();
    }
  }

  async setActiveKeyboard(keyboardId: string, languageCode?: string): Promise<void> {
    return this.activateKeyboard(keyboardId, languageCode, true);
  }

  protected async activateKeyboard(keyboardId: string, languageCode?: string, saveCookie?: boolean): Promise<void> {
    saveCookie ||= false;

    // TODO:  beforeKeyboardChange
    // - this.osk.startHide(false), when needed, could be called via handler here...
    //                                           and on 'onAsyncKeyboardLoad'
    // Also include an 'abort' check based upon it.

    this.contextManager.activeKeyboard = await this.prepareKeyboardForActivation(keyboardId, languageCode);

    // TODO: keyboardChange
    // - this.osk.present() could totally be part of the handler for the event.

    this.osk.present();
  }

  /**
   * Based on the provided keyboard id and language code, selects and (if necessary) loads the
   * corresponding keyboard but does not activate it.
   *
   * This acts as a helper to `activateKeyboard`, helping to centralize and DRY out the actual
   * activation of the requested keyboard.
   * @param keyboardId
   * @param languageCode
   * @returns
   */
  protected async prepareKeyboardForActivation(
    keyboardId: string,
    languageCode?: string
  ): Promise<{keyboard: Keyboard, metadata: KeyboardStub}> {
    // Set default language code
    languageCode ||= '';

    // Check that the saved keyboard is currently registered
    let requestedStub = this.keyboardRequisitioner.cache.getStub(keyboardId, languageCode);

    // Mobile device addition: force selection of the first keyboard if none set
    if(this.config.softDevice.touchable && !requestedStub) {
      // Pick the oldest-registered stub as default.
      requestedStub = this.keyboardRequisitioner.cache.defaultStub;
    } else if(!requestedStub) {
      // Hide OSK and do not update keyboard list if using internal keyboard (desktops)
      this.osk?.startHide(false);

      return Promise.resolve({
        keyboard: null,
        metadata: null
      });
    }

    // Check if current keyboard matches requested keyboard, but not (necessarily) stub
    if(keyboardId == this.contextManager.activeKeyboard.metadata.id) {
      const keyboard = this.contextManager.activeKeyboard.keyboard;
      // In this case, the keyboard is loaded; just update the stub.

      return Promise.resolve({
        keyboard: keyboard,
        metadata: requestedStub
      });
    }

    // Determine if the keyboard was previously loaded but is not active and use the prior load if so.
    let keyboard: Keyboard;
    if(keyboard = this.keyboardRequisitioner.cache.getKeyboardForStub(requestedStub)) {
      return Promise.resolve({
        keyboard: keyboard,
        metadata: requestedStub
      });
    } else {
      // async time - the keyboard has not yet been loaded.

      // Original implementation:  also checked for CJK and kept OSKs activated pre-load,
      // before the picker could ever show.  We should be fine without it so long as
      // a picker keyboard's OSK is kept activated post-load.
      if(this.config.hostDevice.touchable && this.osk?.activationModel) {
        this.osk.activationModel.enabled = true;
      }

      this.osk?.startHide(false);

      // TODO: Maybe make this an event of sorts?  ... which means extending EventEmitter, etc.
      // We aren't adding a 'new' event API set quite yet, though.
      this.onKeyboardAsyncLoadStart(requestedStub);

      let keyboardPromise = this.keyboardRequisitioner.cache.fetchKeyboardForStub(requestedStub);

      let promise = this.contextManager.setActiveKeyboardAsync(keyboardPromise, requestedStub);
      return promise.then(async (stillValid) => {
        if(!stillValid) {
          return Promise.resolve(null);
        }

        return {
          keyboard: await keyboardPromise,
          metadata: requestedStub
        };
      });
    }
  }

  protected onKeyboardAsyncLoadStart(requestedStub: KeyboardStub) { }
}

// Intent:  define common behaviors for both primary app types; each then subclasses & extends where needed.