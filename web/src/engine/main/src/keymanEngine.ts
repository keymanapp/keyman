import { DefaultRules, KeyboardKeymanGlobal, ProcessorInitOptions } from "@keymanapp/keyboard-processor";
import { DOMKeyboardLoader as KeyboardLoader } from "@keymanapp/keyboard-processor/dom-keyboard-loader";
import { InputProcessor, PredictionContext } from "@keymanapp/input-processor";
import { OSKView } from "keyman/engine/osk";
import { KeyboardRequisitioner, ModelCache, ModelSpec } from "keyman/engine/package-cache";

import { EngineConfiguration, InitOptionSpec } from "./engineConfiguration.js";
import KeyboardInterface from "./keyboardInterface.js";
import { ContextManagerBase } from "./contextManagerBase.js";
import { KeyEventHandler } from './keyEventSource.interface.js';
import HardKeyboardBase from "./hardKeyboard.js";
import { LegacyAPIEventEngine } from "./legacyAPIEvents.js";
import DOMCloudRequester from "keyman/engine/package-cache/dom-requester";

export default class KeymanEngine<ContextManager extends ContextManagerBase, HardKeyboard extends HardKeyboardBase> implements KeyboardKeymanGlobal {
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

  /**
   * Registers the specified lexical model within Keyman Engine.  If a keyboard with a
   * matching language code is currently activated, it will also activate the model.
   *
   * @param model  An object defining model ID, associated language IDs, and either the
   *               model's definition or a path to a file containing it.
   */
  addModel(model: ModelSpec) {
    this.modelCache.register(model);

    if(model.languages.indexOf(this.contextManager.activeKeyboard.metadata.langId) != -1) {
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
    if(this.processor.activeModel && this.processor.activeModel.id == modelId) {
      this.processor.languageProcessor.unloadModel();
    }
  }
}

// Intent:  define common behaviors for both primary app types; each then subclasses & extends where needed.