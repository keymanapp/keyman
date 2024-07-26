import { type Keyboard, KeyboardKeymanGlobal, ProcessorInitOptions } from "@keymanapp/keyboard-processor";
import { DOMKeyboardLoader as KeyboardLoader } from "@keymanapp/keyboard-processor/dom-keyboard-loader";
import { InputProcessor, PredictionContext } from "@keymanapp/input-processor";
import { OSKView } from "keyman/engine/osk";
import { KeyboardRequisitioner, ModelCache, ModelSpec, toUnprefixedKeyboardId as unprefixed } from "keyman/engine/package-cache";

import { EngineConfiguration, InitOptionSpec } from "./engineConfiguration.js";
import KeyboardInterface from "./keyboardInterface.js";
import { ContextManagerBase } from "./contextManagerBase.js";
import { KeyEventHandler } from 'keyman/engine/events';
import HardKeyboardBase from "./hardKeyboard.js";
import { LegacyAPIEvents } from "./legacyAPIEvents.js";
import { EventNames, EventListener, LegacyEventEmitter } from "keyman/engine/events";
import DOMCloudRequester from "keyman/engine/package-cache/dom-requester";
import KEYMAN_VERSION from "@keymanapp/keyman-version";

// From https://stackoverflow.com/a/69328045
type WithRequired<T, K extends keyof T> = T & { [P in K]-?: T[P] };
// Sets two parts non-optional at this level, while they were at lower levels.
type ProcessorConfiguration = WithRequired<WithRequired<ProcessorInitOptions, 'keyboardInterface'>, 'defaultOutputRules'>;

function determineBaseLayout(): string {
  // @ts-ignore
  if(typeof(window['KeymanWeb_BaseLayout']) !== 'undefined') {
    // @ts-ignore
    return window['KeymanWeb_BaseLayout'];
  } else {
    return 'us';
  }
}

export default class KeymanEngine<
  Configuration extends EngineConfiguration,
  ContextManager extends ContextManagerBase<any>,
  HardKeyboard extends HardKeyboardBase
> implements KeyboardKeymanGlobal {
  readonly config: Configuration;
  contextManager: ContextManager;
  interface: KeyboardInterface<ContextManager>;
  readonly core: InputProcessor;
  keyboardRequisitioner: KeyboardRequisitioner;
  modelCache: ModelCache;

  protected legacyAPIEvents = new LegacyEventEmitter<LegacyAPIEvents>();
  private _hardKeyboard: HardKeyboard;
  private _osk: OSKView;

  protected keyEventRefocus?: () => void;

  private keyEventListener: KeyEventHandler = (event, callback) => {
    const outputTarget = this.contextManager.activeTarget;

    if(!this.contextManager.activeKeyboard || !outputTarget) {
      if(callback) {
        callback(null, null);
      }
      return;
    }

    if(this.keyEventRefocus) {
      // Do anything needed to guarantee that the outputTarget stays active (`app/browser`: maintains focus).
      // (Interaction with the OSK may have de-focused the element providing active context;
      // we want to restore it in case the user swaps back to the hardware keyboard afterward.)
      this.keyEventRefocus();
    }

    // Clear any cached codepoint data; we can rebuild it if it's unchanged.
    outputTarget.invalidateSelection();
    // Deadkey matching continues to be troublesome.
    // Deleting matched deadkeys here seems to correct some of the issues.   (JD 6/6/14)
    outputTarget.deadkeys().deleteMatched();      // Delete any matched deadkeys before continuing

    if(event.isSynthetic) {
      const oskLayer = this.osk.vkbd.layerId;

      // In case of modipresses.
      if(oskLayer && oskLayer != this.core.keyboardProcessor.layerId) {
        this.core.keyboardProcessor.layerId = oskLayer;
      }
    }
    const result = this.core.processKeyEvent(event, outputTarget);

    if(result && result.transcription?.transform) {
      this.config.onRuleFinalization(result, this.contextManager.activeTarget);
    }

    if(callback) {
      callback(result, null);
    }

    // No try-catch here because we don't want to mask any errors that occur during keystroke
    // processing - silent failures are far harder to diagnose.
  };

  /**
   * @param worker  A configured WebWorker to serve as the predictive-text engine's main thread.
   *                Available in the following variants:
   *                - sourcemapped, unminified (debug)
   *                - non-sourcemapped + minified (release)
   * @param config
   * @param contextManager
   * @param processorConfigInitializer A one-time use closure used to initialize certain critical components reliant
   *                                   upon the class instance, configured by the derived class, but needed during
   *                                   the superclass constructor.
   */
  constructor(
    worker: Worker,
    config: Configuration,
    contextManager: ContextManager,
    processorConfigInitializer: (engine: KeymanEngine<Configuration, ContextManager, HardKeyboard>) => ProcessorConfiguration
  ) {
    this.config = config;
    this.contextManager = contextManager;

    const processorConfiguration = processorConfigInitializer(this);
    processorConfiguration.baseLayout = determineBaseLayout();
    this.interface = processorConfiguration.keyboardInterface as KeyboardInterface<ContextManager>;
    this.core = new InputProcessor(config.hostDevice, worker, processorConfiguration);

    this.core.languageProcessor.on('statechange', (state) => {
      // The banner controller cannot directly trigger a layout-refresh at this time,
      // so we handle that here.
      this.osk?.bannerController.selectBanner(state);
      this.osk?.refreshLayout();

      // If `this.osk` is not yet initialized, the OSK itself will check if the model is loaded
    });

    // The OSK does not possess a direct connection to the KeyboardProcessor's state-key
    // management object; this event + handler allow us to keep the OSK's related states
    // in sync.
    this.core.keyboardProcessor.on('statekeychange', (stateKeys) => {
      this.osk?.vkbd?.updateStateKeys(stateKeys);
    })

    this.contextManager.on('beforekeyboardchange', (metadata) => {
      this.legacyAPIEvents.callEvent('beforekeyboardchange', {
        internalName: metadata?.id,
        languageCode: metadata?.langId
      });
    });

    this.contextManager.on('keyboardchange', (kbd) => {
      // Hide OSK and do not update keyboard list if using internal keyboard (desktops).
      // Condition will not be met for touch form-factors; they force selection of a
      // default keyboard.
      if(!kbd) {
        this.osk.startHide(false);
      }

      const prepareKeyboardSwap = () => {
        this.refreshModel();
        // Triggers context resets that can trigger layout stuff.
        // It's not the final such context-reset, though.
        this.core.activeKeyboard = kbd?.keyboard;

        this.legacyAPIEvents.callEvent('keyboardchange', {
          internalName: kbd?.metadata.id ?? '',
          languageCode: kbd?.metadata.langId ?? ''
        });
      }

      /*
        This pattern is designed to minimize layout reflow during the keyboard-swap process.
        The 'default' layer is loaded by default, but some keyboards will start on different
        layers depending on the current state of the context.

        If possible, we want to only perform layout operations once the correct layer is
        set to active.
      */
      if(this.osk) {
        this.osk.batchLayoutAfter(() => {
          prepareKeyboardSwap();
          this.osk.activeKeyboard = kbd;
          // Note:  when embedded within the mobile apps, the keyboard will still be visible
          // at this time.

          /*
            Needed to ensure the correct layer is displayed AND that deadkeys from
            the old keyboard have been wiped.

            Needs to be after the OSK has loaded for the keyboard in case the default
            layer should be something other than "default" for the current context.
          */
          this.contextManager.resetContext();
          this.osk.present();
        });
      } else {
        prepareKeyboardSwap();
        this.contextManager.resetContext();
      }
    });

    this.contextManager.on('keyboardasyncload', (metadata) => {
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
  }

  public async init(optionSpec: Required<InitOptionSpec>){
    // There may be some valid mutations possible even on repeated calls?
    // The original seems to allow it.

    const config = this.config;
    if(config.deferForInitialization.isResolved) {
      // abort!  Maybe throw an error, too.
      return Promise.resolve();
    }

    config.initialize(optionSpec);

    // Initialize supplementary plane string extensions
    String.kmwEnableSupplementaryPlane(true);

    // Since we're not sandboxing keyboard loads yet, we just use `window` as the jsGlobal object.
    // All components initialized below require a properly-configured `config.paths` or similar.
    const keyboardLoader = new KeyboardLoader(this.interface, config.applyCacheBusting);
    this.keyboardRequisitioner = new KeyboardRequisitioner(keyboardLoader, new DOMCloudRequester(), this.config.paths);
    this.modelCache = new ModelCache();
    const kbdCache = this.keyboardRequisitioner.cache;

    this.contextManager.configure({
      resetContext: (target) => {
        // Could reset the target's deadkeys here, but it's really more of a 'core' task.
        // So we delegate that to keyboard-processor.
        if(this.osk) {
          this.osk.batchLayoutAfter(() => {
            this.core.resetContext(target);
          })
        } else {
          this.core.resetContext(target);
        }
      },
      predictionContext: new PredictionContext(this.core.languageProcessor, this.core.keyboardProcessor),
      keyboardCache: this.keyboardRequisitioner.cache
    });

    // #region Event handler wiring
    this.config.on('spacebartext', () => {
      // On change of spacebar-text mode, we currently need a layout refresh to update the
      // spacebar's text.
      this.osk?.refreshLayout();
    });

    kbdCache.on('stubadded', (stub) => {
      let eventRaiser = () => {
        // The corresponding event is needed in order to update UI modules as new keyboard stubs "come online".
        this.legacyAPIEvents.callEvent('keyboardregistered', {
          internalName: stub.KI,
          language: stub.KL,
          keyboardName: stub.KN,
          languageCode: stub.KLC,
          package: stub.KP
        });

        // If this is the first stub loaded, set it as active.
        if(this.config.activateFirstKeyboard && this.keyboardRequisitioner.cache.defaultStub == stub) {
          // Note:  leaving this out is super-useful for debugging issues that occur when no keyboard is active.
          this.contextManager.activateKeyboard(stub.id, stub.langId, true);
        }
      }

      if(this.config.deferForInitialization.isResolved) {
        eventRaiser();
      } else {
        this.config.deferForInitialization.then(eventRaiser);
      }
    });

    kbdCache.on('keyboardadded', (keyboard) => {
      let eventRaiser = () => {
        // Execute any external (UI) code needed after loading keyboard
        this.legacyAPIEvents.callEvent('keyboardloaded', {
          keyboardName: keyboard.id
        });
      }

      if(this.config.deferForInitialization.isResolved) {
        eventRaiser();
      } else {
        this.config.deferForInitialization.then(eventRaiser);
      }
    });

    this.keyboardRequisitioner.cache.on('keyboardadded', (keyboard) => {
      this.legacyAPIEvents.callEvent('keyboardloaded', { keyboardName: keyboard.id });
    });
    //
    // #endregion
  }

  /**
   * Public API:  Denotes the 'patch' component of the version of the current engine.
   *
   * https://help.keyman.com/developer/engine/web/current-version/reference/core/build
   */
  public get build(): number {
    return Number.parseInt(KEYMAN_VERSION.VERSION_PATCH, 10);
  }

  /**
   * Public API:  Denotes the major & minor components of the version of the current engine.
   *
   * https://help.keyman.com/developer/engine/web/current-version/reference/core/version
   */
  public get version(): string {
    return KEYMAN_VERSION.VERSION_RELEASE;
  }

  public get hardKeyboard(): HardKeyboard {
    return this._hardKeyboard;
  }

  protected set hardKeyboard(keyboard: HardKeyboard) {
    if(this._hardKeyboard) {
      this._hardKeyboard.off('keyevent', this.keyEventListener);
    }
    this._hardKeyboard = keyboard;
    keyboard.on('keyevent', this.keyEventListener);
  }

  public get osk(): OSKView {
    return this._osk;
  }

  public set osk(value: OSKView) {
    if(this._osk) {
      this._osk.off('keyevent', this.keyEventListener);
      this.core.keyboardProcessor.layerStore.handler = this.osk.layerChangeHandler;
    }
    this._osk = value;
    // As the `new context` ruleset is designed to facilitate OSK layer-change updates
    // based on the context being entered, we want the keyboard processor's current
    // contextDevice to match that of the active OSK.  See #11740.
    this.core.keyboardProcessor.contextDevice = value?.targetDevice ?? this.config.softDevice;
    if(value) {
      // Don't build an OSK if no keyboard is available yet; avoid the extra flash.
      if(this.contextManager.activeKeyboard) {
        value.activeKeyboard = this.contextManager.activeKeyboard;
      }
      value.on('keyevent', this.keyEventListener);
      this.core.keyboardProcessor.layerStore.handler = value.layerChangeHandler;
    }
  }

  public getDebugInfo(): Record<string, any> {
    const activeKbd = this.contextManager?.activeKeyboard;

    const report = {
      configReport: this.config?.debugReport(),
      keyboard: {
        id: unprefixed(activeKbd?.metadata?.id ?? ''),
        langId: activeKbd?.metadata?.langId || '',
        version: activeKbd?.keyboard?.version ?? ''
      },
      model: {
        id: this.core?.activeModel?.id || ''
      },
      osk: {
        banner: this.osk?.banner?.banner.type ?? '',
        layer: this.osk?.vkbd?.layerId || ''
      }
    };

    return report;
  }

  // Returned Promise:  gives the model-spec object.  Only resolves when any model loading or unloading
  // is fully complete.
  private refreshModel(): Promise<ModelSpec> {
    const kbd = this.contextManager.activeKeyboard;
    const model = this.modelCache.modelForLanguage(kbd?.metadata.langId);

    if(this.core.activeModel != model) {
      if(this.core.activeModel) {
        this.core.languageProcessor.unloadModel();
      }

      // Semi-hacky management of banner display state.
      if(model) {
        return this.core.languageProcessor.loadModel(model).then(() => {
          return model;
        });
      }
    }

    return Promise.resolve(model);
  }

  /**
   * Subscribe to Keyman Engine events documented at
   * https://help.keyman.com/developer/engine/web/current-version/reference/events.  Note that any OSK-related
   * events should instead register on `keyman.osk.addEventListener`, not on this method.
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/addEventListener
   */
  public addEventListener<Name extends EventNames<LegacyAPIEvents>>(event: Name, listener: EventListener<LegacyAPIEvents, Name>) {
    this.legacyAPIEvents.addEventListener(event, listener);
  }

  /**
   * Public API: Unsubscribe from Keyman Engine events documented at
   * https://help.keyman.com/developer/engine/web/current-version/reference/events.
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/removeEventListener
   */
  public removeEventListener<Name extends EventNames<LegacyAPIEvents>>(event: Name, listener: EventListener<LegacyAPIEvents, Name>) {
    this.legacyAPIEvents.removeEventListener(event, listener);
  }

  shutdown() {
    this.legacyAPIEvents.shutdown();
    this.osk?.shutdown();
  }

  // API methods

  // 17.0: new!  Only used by apps utilizing app/webview and one app/browser test page.
  // Is not part of our 'published' API.

  /**
   * Registers the specified lexical model within Keyman Engine.  If a keyboard with a
   * matching language code is currently activated, it will also activate the model.
   *
   * @param model  An object defining model ID, associated language IDs, and either the
   *               model's definition or a path to a file containing it.
   */
  addModel(model: ModelSpec): Promise<void> {
    this.modelCache.register(model);

    const activeStub = this.contextManager.activeKeyboard?.metadata;
    if(activeStub && model.languages.indexOf(activeStub.langId) != -1) {
      return this.refreshModel().then(() => { return; });
    } else {
      return Promise.resolve();
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

  /**
   * Allow to change active keyboard by (internal) keyboard name
   *
   * @param       {string}    PInternalName   Internal name
   * @param       {string}    PLgCode         Language code
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/setActiveKeyboard
   */
  public async setActiveKeyboard(keyboardId: string, languageCode?: string): Promise<boolean> {
    return this.contextManager.activateKeyboard(keyboardId, languageCode, true);
  }

  /**
   * Function     getActiveKeyboard
   * Scope        Public
   * @return      {string}      Name of active keyboard
   * Description  Return internal name of currently active keyboard
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/getActiveKeyboard
   */
  public getActiveKeyboard(): string {
    return this.contextManager.activeKeyboard?.metadata.id ?? '';
  }

  /**
   * Function    getActiveLanguage
   * Scope       Public
   * @param      {boolean=}        true to retrieve full language name, false/undefined to retrieve code.
   * @return     {string}         language code
   * Description Return language code for currently selected language
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/getActiveLanguage
   */
  public getActiveLanguage(fullName?: boolean): string {
    // In short... the activeStub.
    const metadata = this.contextManager.activeKeyboard?.metadata;

    if(!fullName) {
      return metadata?.langId ?? '';
    } else {
      return metadata?.langName ?? '';
    }
  }

  /**
   * Function     isChiral
   * Scope        Public
   * @param       {string|Object=}   k0
   * @return      {boolean}
   * Description  Tests if the active keyboard (or optional argument) uses chiral modifiers.
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/isChiral
   */
  public isChiral(k0?: string | Keyboard) {
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
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/core/resetContext
   */
  public resetContext() {
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
}

// Intent:  define common behaviors for both primary app types; each then subclasses & extends where needed.