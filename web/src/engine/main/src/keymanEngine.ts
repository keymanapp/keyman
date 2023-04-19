import { DefaultRules, type Keyboard, KeyboardKeymanGlobal, ProcessorInitOptions, OutputTarget } from "@keymanapp/keyboard-processor";
import { DOMKeyboardLoader as KeyboardLoader } from "@keymanapp/keyboard-processor/dom-keyboard-loader";
import { InputProcessor, PredictionContext } from "@keymanapp/input-processor";
import { OSKView } from "keyman/engine/osk";
import { KeyboardRequisitioner, type KeyboardStub, ModelCache, ModelSpec } from "keyman/engine/package-cache";

import { EngineConfiguration, InitOptionSpec } from "./engineConfiguration.js";
import KeyboardInterface from "./keyboardInterface.js";
import { ContextManagerBase } from "./contextManagerBase.js";
import { KeyEventHandler } from './keyEventSource.interface.js';
import HardKeyboardBase from "./hardKeyboard.js";
import { LegacyAPIEvents } from "./legacyAPIEvents.js";
import { EventNames, EventListener, LegacyEventEmitter } from "keyman/engine/events";
import DOMCloudRequester from "keyman/engine/package-cache/dom-requester";

export default class KeymanEngine<
  ContextManager extends ContextManagerBase<any>,
  HardKeyboard extends HardKeyboardBase
> implements KeyboardKeymanGlobal {
  readonly config: EngineConfiguration;
  readonly contextManager: ContextManager;
  readonly interface: KeyboardInterface;
  readonly core: InputProcessor;
  readonly keyboardRequisitioner: KeyboardRequisitioner;
  readonly modelCache: ModelCache;

  private legacyAPIEvents = new LegacyEventEmitter<LegacyAPIEvents>();
  private _hardKeyboard: HardKeyboard;
  private _osk: OSKView;

  protected keyEventRefocus?: () => void;

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
    } else if(this.keyEventRefocus) { // && event.isSynthetic // as in, is from the OSK.
      // Do anything needed to guarantee that the outputTarget stays active (`app/browser`: maintains focus).
      // (Interaction with the OSK may have de-focused the element providing active context;
      // we want to restore it in case the user swaps back to the hardware keyboard afterward.)
      this.keyEventRefocus();
    }

    try {
      // Clear any cached codepoint data; we can rebuild it if it's unchanged.
      outputTarget.invalidateSelection();
      // Deadkey matching continues to be troublesome.
      // Deleting matched deadkeys here seems to correct some of the issues.   (JD 6/6/14)
      outputTarget.deadkeys().deleteMatched();      // Delete any matched deadkeys before continuing

      const result = this.core.processKeyEvent(event, outputTarget);

      if(result && result.transcription?.transform) {
        this.config.onRuleFinalization(result, this.contextManager.activeTarget);
      }

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

    this.core = new InputProcessor(config.hostDevice, worker, this.processorConfiguration());

    this.contextManager.configure({
      resetContext: (target) => {
        this.core.resetContext(target);
      },
      predictionContext: new PredictionContext(this.core.languageProcessor, this.core.keyboardProcessor),
      keyboardCache: this.keyboardRequisitioner.cache
    });

    this.core.keyboardProcessor.beepHandler = (target) => {
      if(this.doBeep) {
        this.doBeep(target);
      }
    }

    // #region Event handler wiring
    this.config.on('spacebartext', () => {
      // On change of spacebar-text mode, we currently need a layout refresh to update the
      // spacebar's text.
      this.osk?.refreshLayout();
    });

    kbdCache.on('stubAdded', (stub) => {
      let eventRaiser = () => {
        // The corresponding event is needed in order to update UI modules as new keyboard stubs "come online".
        this.legacyAPIEvents.callEvent('keyboardregistered', {
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
        this.legacyAPIEvents.callEvent('keyboardloaded', {
          keyboardName: keyboard.id
        });
      }

      if(this.config.deferForInitialization.hasFinalized) {
        eventRaiser();
      } else {
        this.config.deferForInitialization.then(eventRaiser);
      }
    });

    contextManager.on('beforekeyboardchange', (metadata) => {
      this.legacyAPIEvents.callEvent('beforekeyboardchange', {
        internalName: metadata.id,
        languageCode: metadata.langId
      });
    });

    contextManager.on('keyboardchange', (kbd) => {
      this.refreshModel();
      this.core.activeKeyboard = kbd.keyboard;

      this.legacyAPIEvents.callEvent('keyboardchange', {
        internalName: kbd.metadata.id,
        languageCode: kbd.metadata.langId
      });

      // Hide OSK and do not update keyboard list if using internal keyboard (desktops).
      // Condition will not be met for touch form-factors; they force selection of a
      // default keyboard.
      if(kbd.keyboard == null && kbd.metadata == null) {
        this.osk.startHide(false);
      }

      if(this.osk) {
        this.osk.setNeedsLayout();
        this.osk.activeKeyboard = kbd;
        this.osk.present();
      }
    });

    contextManager.on('keyboardasyncload', (metadata) => {
      /* Original implementation pre-modularization:
       *
       * > Force OSK display for CJK keyboards (keyboards using a pick list)
       *
       * A matching subcondition in the block below will ensure that the OSK activates pre-load
       * for CJK keyboards.  Yes, even before a CJK picker could ever show.  We should be fine
       * without the CJK check so long as a picker keyboard's OSK is kept activated post-load,
       * when the picker actually needs to be kept persistently-active.
       * `metadata` would be relevant a the CJK-check, which was based on language codes.
       *
       * Of course, as mobile devices don't have guaranteed physical keyboards... we need to
       * keep the OSK visible for them, hence the actual block below.
       */
      if(this.config.hostDevice.touchable && this.osk?.activationModel) {
        this.osk.activationModel.enabled = true;
        // Also note:  the OSKView.mayDisable method returns false when hostDevice.touchable = false.
        // The .startHide() call below will check that method before actually starting an OSK hide.
      }

      // Always (temporarily) hide the OSK when loading a new keyboard, to ensure
      // that a failure to load doesn't leave the current OSK displayed
      this.osk?.startHide(false);
    });

    this.keyboardRequisitioner.cache.on('keyboardAdded', (keyboard) => {
      this.legacyAPIEvents.callEvent('keyboardloaded', { keyboardName: keyboard.id });
    });
    //
    // #endregion
  }

  async init(optionSpec: Required<InitOptionSpec>){
    // There may be some valid mutations possible even on repeated calls?
    // The original seems to allow it.

    if(this.config.deferForInitialization.hasFinalized) {
      // abort!  Maybe throw an error, too.
      return Promise.resolve();
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
      this.core.keyboardProcessor.layerStore.handler = this.osk.layerChangeHandler;
    }
    this._osk = value;
    this._osk.on('keyEvent', this.keyEventListener);
    this.core.keyboardProcessor.layerStore.handler = this.osk.layerChangeHandler;
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

    let bannerDisplayed: boolean = false;

    if(this.core.activeModel != model) {
      if(this.core.activeModel) {
        bannerDisplayed = true;
        this.core.languageProcessor.unloadModel();
      }

      // Semi-hacky management of banner display state.
      if(model) {
        this.core.languageProcessor.loadModel(model).then(() => {
          if(!bannerDisplayed) {
            // Because this is what the original implementation expects to see (over time).
            this.osk.bannerController.selectBanner('active');
            this.osk.bannerController.selectBanner('configured');
          }
        });
      } else if(bannerDisplayed) {
        this.osk.bannerController.selectBanner('inactive');
      }
    }
  }

  addEventListener<Name extends EventNames<LegacyAPIEvents>>(event: Name, listener: EventListener<LegacyAPIEvents, Name>) {
    this.legacyAPIEvents.addEventListener(event, listener);
  }

  removeEventListener<Name extends EventNames<LegacyAPIEvents>>(event: Name, listener: EventListener<LegacyAPIEvents, Name>) {
    this.legacyAPIEvents.removeEventListener(event, listener);
  }

  // Also technically TODO, even if not here:  similar two methods as above for the OSK.

  shutdown() {
    this.legacyAPIEvents.shutdown();
    this.osk?.shutdown();
  }

  // API methods

  // 17.0: new!  Only used by apps utilizing app/webview and one app/browser test page.

  /**
   * Registers the specified lexical model within Keyman Engine.  If a keyboard with a
   * matching language code is currently activated, it will also activate the model.
   *
   * @param model  An object defining model ID, associated language IDs, and either the
   *               model's definition or a path to a file containing it.
   */
  addModel(model: ModelSpec) {
    this.modelCache.register(model);

    const activeStub = this.contextManager.activeKeyboard?.metadata;
    if(activeStub && model.languages.indexOf(activeStub.langId) != -1) {
      this.refreshModel();
    }
  }

  // 17.0: new!  Only used by apps utilizing app/webview and one app/browser test page.

  /**
   * Unregisters any previously-registered lexical model with a matching ID from Keyman Engine.
   * If a keyboard with a matching language code is currently activated, it will also
   * deactivate the model.
   *
   * @param modelId  The ID for the model to be deregistered and forgotten by Keyman Engine.
   */
  removeModel(modelId: string) {
    this.modelCache.unregister(modelId);

    // Is it the active model?
    if(this.core.activeModel && this.core.activeModel.id == modelId) {
      this.core.languageProcessor.unloadModel();
    }
  }

  async setActiveKeyboard(keyboardId: string, languageCode?: string): Promise<boolean> {
    return this.contextManager.activateKeyboard(keyboardId, languageCode, true);
  }

  /**
   * Function     isChiral
   * Scope        Public
   * @param       {string|Object=}   k0
   * @return      {boolean}
   * Description  Tests if the active keyboard (or optional argument) uses chiral modifiers.
   */
  isChiral(k0?: string | Keyboard) {
    let kbd: Keyboard;
    if(k0) {
      if(typeof k0 == 'string') {
        const kbdObj = this.keyboardRequisitioner.cache.getKeyboard(k0);
        if(!kbdObj) {
          throw new Error(`Keyboard '${k0}' has not been loaded.`);
        } else {
          k0 = kbdObj;
        }
      }

      kbd = k0;
    } else {
      kbd = this.core.activeKeyboard;
    }
    return kbd.isChiral;
  }

  /**
   * Function     resetContext
   * Scope        Public
   * Description  Reverts the OSK to the default layer, clears any processing caches and modifier states,
   *              and clears deadkeys and prediction-processing states on the active element (if it exists)
   */
  resetContext() {
    // NOTE:  16.0 KMW had a `element: HTMLElement` parameter.  It was undocumented on help.keyman.com, though,
    //        and I'm not sure it makes sense to attempt to maintain.
    //
    //        I cannot find any calls that utilized the parameter within in-repo code.
    this.contextManager.resetContext();
  };

  /**
   * Function     setNumericLayer
   * Scope        Public
   * Description  Set OSK to numeric layer if it exists
   */
  setNumericLayer() {
    this.core.keyboardProcessor.setNumericLayer(this.config.softDevice);
  };

  doBeep?: (target: OutputTarget) => void;
}

// Intent:  define common behaviors for both primary app types; each then subclasses & extends where needed.