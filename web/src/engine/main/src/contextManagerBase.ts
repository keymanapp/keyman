import { EventEmitter } from 'eventemitter3';
import { ManagedPromise, type Keyboard, type KeyboardInterface, type OutputTarget } from '@keymanapp/keyboard-processor';
import { StubAndKeyboardCache, type KeyboardStub } from 'keyman/engine/package-cache';
import { PredictionContext } from '@keymanapp/input-processor';
import { EngineConfiguration } from './engineConfiguration.js';

interface EventMap {
  // target, then keyboard.
  'targetchange': (target: OutputTarget) => boolean;

  /**
   * This event is raised whenever a keyboard change is requested.
   *
   * Note that if the keyboard has not been previously loaded, this event will be raised twice.
   * 1. Before the keyboard is loaded into Keyman Engine for Web.
   * 2. Once the keyboard is loaded, but before it is activated.
   * @param metadata     The to-be-activated keyboard's properties
   * @returns
   */
  'beforekeyboardchange': (metadata: KeyboardStub) => void;

  /**
   * This event is raised whenever an activating keyboard is being loaded into Keyman Engine for
   * the first time in the user's current session, which is an asynchronous operation.  It is called
   * once the async request is initiated.
   * @param metadata  The registered properties for the keyboard being asynchronously loaded
   * @param onload    A Promise that resolves with `null` when loading successfully completes or
   *                  with an `error` if it fails.
   * @returns
   */
  'keyboardasyncload': (metadata: KeyboardStub, onload: Promise<Error>) => void;

  /**
   * This event is raised whenever a keyboard is fully activated and set as the current active
   * keyboard within Keyman Engine for Web.
   * @param kbd
   * @returns
   */
  'keyboardchange': (kbd: {keyboard: Keyboard, metadata: KeyboardStub}) => void;
}

export interface ContextManagerConfiguration {
  /**
   * A function that resets any state-dependent keyboard key-state information such as
   * emulated modifier state and layer id.  Also purges the context cache.
   * If an `outputTarget` is specified, it will also trigger new-context rule processing.
   *
   * Does not reset option-stores, variable-stores, etc.
   */
  readonly resetContext: (outputTarget?: OutputTarget) => void;

  /**
   * A predictive-state management object that interfaces the predictive-text banner
   * with the active context.
   */
  readonly predictionContext: PredictionContext;

  /**
   * The stub & keyboard curation cache holding preloaded keyboards and metadata useable
   * to load those not yet loaded.
   */
  readonly keyboardCache: StubAndKeyboardCache;
}

interface PendingActivation {
  target: OutputTarget,
  keyboard: Promise<Keyboard>,
  stub: KeyboardStub;
}

export abstract class ContextManagerBase<MainConfig extends EngineConfiguration> extends EventEmitter<EventMap> {
  public static readonly TIMEOUT_THRESHOLD = 10000;

  abstract initialize(): void;

  abstract get activeTarget(): OutputTarget;

  private _predictionContext: PredictionContext;
  protected keyboardCache: StubAndKeyboardCache;
  private _resetContext: (outputTarget?: OutputTarget) => void;

  private pendingActivations: PendingActivation[] = [];
  protected engineConfig: MainConfig;

  get predictionContext(): PredictionContext {
    return this._predictionContext;
  }

  constructor(engineConfig: MainConfig) {
    super();

    this.engineConfig = engineConfig;
  }

  configure(config: ContextManagerConfiguration) {
    this._resetContext = config.resetContext;
    this._predictionContext = config.predictionContext;
    this.keyboardCache = config.keyboardCache;
  }

  insertText(kbdInterface: KeyboardInterface, Ptext: string, PdeadKey: number) {
    // Find the correct output target to manipulate.
    const outputTarget = this.activeTarget;

    if(outputTarget != null) {
      if(Ptext != null) {
        kbdInterface.output(0, outputTarget, Ptext);
      }

      if((typeof(PdeadKey)!=='undefined') && (PdeadKey !== null)) {
        kbdInterface.deadkeyOutput(0, outputTarget, PdeadKey);
      }

      outputTarget.invalidateSelection();

      return true;
    }
    return false;
  }

  resetContext() {
    this._resetContext(this.activeTarget);
    this.predictionContext.resetContext();
  }

  abstract get activeKeyboard(): {keyboard: Keyboard, metadata: KeyboardStub};

  /**
   * Determines the 'target' currently used to determine which keyboard should be active.
   * When `null`, keyboard-activation operations will affect the global default; otherwise,
   * such operations affect only the specified `target`.
   *
   * This method exists to facilitate independent-keyboard mode operations for specific
   * attached elements within the app/browser target.  For `app/webview`, this should
   * always return a consistent value - likely, `null`.
   */
  protected abstract currentKeyboardSrcTarget(): OutputTarget;

  /**
   * Ensures that newly activated keyboards are set correctly within managed context, possibly
   * against inactive output targets.
   * @param kbd
   * @param target
   */
  protected abstract activateKeyboardForTarget(kbd: {keyboard: Keyboard, metadata: KeyboardStub}, target: OutputTarget): void;

  /**
   * Checks the pending keyboard-activation array for an entry corresponding to the specified
   * OutputTarget.  If found, also removes the entry for bookkeeping purposes.
   * @param target  The specific OutputTarget affected by the pending Keyboard activation.
   *                May be `null`, which corresponds to the global default Keyboard.
   * @returns `true` if pending activation is still valid, `false` otherwise.
   */
  private findAndPopActivation(target: OutputTarget): PendingActivation {
    // Array.findIndex requires Chrome 45+. :(
    let activationIndex;
    for(activationIndex = 0; activationIndex < this.pendingActivations.length; activationIndex++) {
      if(this.pendingActivations[activationIndex].target == target) {
        break;
      }
    }

    if(activationIndex == this.pendingActivations.length) {
      return null;
    }

    return this.pendingActivations.splice(activationIndex, 1)[0];
  }

  /**
   * Internally registers a pending keyboard-activation's properties, only resolving to a non-null
   * activation if it is still the most recent keyboard-activation request that would affect the
   * corresponding context.
   * @param kbdPromise
   * @param metadata
   * @param target
   * @returns
   */
  protected async deferredKeyboardActivation(
    kbdPromise: Promise<Keyboard>,
    metadata: KeyboardStub,
    target: OutputTarget
  ): Promise<PendingActivation> {
    const activation: PendingActivation = {
      target: target,
      keyboard: kbdPromise,
      stub: metadata
    };

    // Invalidate existing requests for the specified target.
    this.findAndPopActivation(target);
    this.pendingActivations.push(activation);
    await kbdPromise;

    // The keyboard-load is complete; is the activation still desired?
    const activationAfterAwait = this.findAndPopActivation(target);
    if(activationAfterAwait == activation) {
      return activation;
    } else if(activationAfterAwait) {
      // Restore the popped element; it doesn't match the current activation attempt.
      this.pendingActivations.push(activationAfterAwait);
      return null;
    }
  }

  /**
   * Specifies the keyboard id and the language code to use when a 'default' keyboard
   * must be selected by the engine for fallback behaviors.
   */
  protected abstract getFallbackStubKey(): {
    id: string,
    langId: string
  };

  /**
   * Change active keyboard to keyboard selected by (internal) name and language code
   *
   * Test if selected keyboard already loaded, and simply update active stub if so.
   * Otherwise, insert a script to download and insert the keyboard from the repository
   * or user-indicated file location.
   *
   * @param keyboardId
   * @param languageCode
   * @param saveCookie
   * @returns
   */
  public async activateKeyboard(keyboardId: string, languageCode?: string, saveCookie?: boolean): Promise<boolean> {
    // TODO:  relocate default keyboard behavior here once we can also move core error handling for
    // unfound stubs here.
    const wasNull = !this.activeKeyboard;

    // If there was a previous activation attempt set and still active for the specified keyboard target,
    // cancel it.  For exmaple, if the user selects a preloaded keyboard after having tried to select one
    // still async-loading, we should go with the later setting - the preloaded one.
    this.findAndPopActivation(this.currentKeyboardSrcTarget());

    const activatingKeyboard = this.prepareKeyboardForActivation(keyboardId, languageCode);

    const originalKeyboardTarget = this.currentKeyboardSrcTarget();

    const keyboard = await activatingKeyboard.keyboard;
    if(keyboard == null && activatingKeyboard.metadata) {
      // The activation was async and was cancelled - either by `beforeKeyboardChange` first-pass
      // cancellation or because a different keyboard was requested before completion of the async load.
      return false;
    }

    /*
     * Triggers `beforeKeyboardChange` event if the current context at the time when activation is possible
     * would be affected by the requested keyboard change.
     * - if a keyboard was asynchronously loaded for this...
     *   - it is possible for the context (in app/browser) to have changed to a page element in
     *     "independent keyboard" mode (or away from one)
     *   - This is the "second" `beforeKeyboardChange` call - a loaded keyboard may now be activated.
     *
     * If the now-current context would be unaffected by the keyboard change, we do not raise the corresponding
     * event.
     */
    if(this.currentKeyboardSrcTarget() == originalKeyboardTarget) {
      this.emit('beforekeyboardchange', activatingKeyboard.metadata);
    }

    let kbdStubPair: { keyboard: Keyboard, metadata: KeyboardStub } = null;
    if(keyboard) {
      kbdStubPair = {
        keyboard: keyboard,
        metadata: activatingKeyboard.metadata
      };
    }

    this.activateKeyboardForTarget(kbdStubPair, originalKeyboardTarget);

    // Only trigger `keyboardchange` events when they will affect the active context.
    // (!wasNull || !!keyboard) - blocks events for `null` -> `null` transitions.
    // (keyman/keymanweb.com#96)
    if(this.currentKeyboardSrcTarget() == originalKeyboardTarget && (!wasNull || !!keyboard)) {
      // Will trigger KeymanEngine handler that passes keyboard to the OSK, displays it.
      this.emit('keyboardchange', this.activeKeyboard);
    }

    return true;
  }

  /**
   * Based on the provided keyboard id and language code, selects and (if necessary) loads the
   * corresponding keyboard but does not activate it.
   *
   * This acts as a helper to `activateKeyboard`, helping to centralize and DRY out the actual
   * activation of the requested keyboard.  Note that it is a synchronous method and should stay
   * that way, though it should return a `Promise` for the activating keyboard.
   * @param keyboardId
   * @param languageCode
   * @returns
   */
  protected prepareKeyboardForActivation(
    keyboardId: string,
    languageCode?: string
  ): {keyboard: Promise<Keyboard>, metadata: KeyboardStub} {
    // Set default language code
    languageCode ||= '';

    // Check that the saved keyboard is currently registered
    let requestedStub: KeyboardStub = null;
    if(keyboardId) {
      requestedStub = this.keyboardCache.getStub(keyboardId, languageCode);
    } else {
      languageCode == '';
    }

    if(!requestedStub) {
      if(keyboardId) {
        throw new Error("No matching stub has been registered.");
      } else {
        return {
          keyboard: Promise.resolve(null),
          metadata: null
        }
      }
    }

    // Check if current keyboard matches requested keyboard, but not (necessarily) stub
    if(this.activeKeyboard?.metadata && keyboardId == this.activeKeyboard.metadata.id) {
      const keyboard = this.activeKeyboard.keyboard;
      // In this case, the keyboard is loaded; just update the stub.

      return {
        keyboard: Promise.resolve(keyboard),
        metadata: requestedStub
      };
    }

    // Determine if the keyboard was previously loaded but is not active; use the cached, pre-loaded version if so.
    let keyboard: Keyboard;
    if(keyboard = this.keyboardCache.getKeyboardForStub(requestedStub)) {
      return {
        keyboard: Promise.resolve(keyboard),
        metadata: requestedStub
      };
    } else {
      // It's async time - the keyboard is not preloaded within the cache.  Use the stub's data to load it.

      // `beforeKeyboardChange` - first call
      this.emit('beforekeyboardchange', requestedStub);

      const defermentPromise = this.engineConfig.deferForInitialization.then(() => {
        // Provide a Promise for completion of the async load process.
        const completionPromise = new ManagedPromise<Error>();
        this.emit('keyboardasyncload', requestedStub, completionPromise.corePromise);

        let keyboardPromise = this.keyboardCache.fetchKeyboardForStub(requestedStub);
        let timeoutPromise = new Promise<Keyboard>((resolve, reject) => {
          const timeoutMsg = `Sorry, the ${requestedStub.name} keyboard for ${requestedStub.langName} is not currently available.`;
          window.setTimeout(() => reject(new Error(timeoutMsg)), ContextManagerBase.TIMEOUT_THRESHOLD);
        });

        let combinedPromise = Promise.race([keyboardPromise, timeoutPromise]);

        // Ensure the async-load Promise completes properly.
        combinedPromise.then(() => {
          completionPromise.resolve(null);
          // Prevent any 'unhandled Promise rejection' events that may otherwise occur from the timeout promise.
          timeoutPromise.catch(() => {});
        });
        combinedPromise.catch((err) => {
          completionPromise.resolve(err);
          throw err;
        });

        return combinedPromise;
      });

      // Now the fun part:  note the original call's parameters as a pending activation.
      let promise = this.deferredKeyboardActivation(defermentPromise, requestedStub, this.currentKeyboardSrcTarget());
      return {
        keyboard: promise.then(async (activation) => {
          // Is the activation we requested still pending, or was it cancelled in favor of a
          // different activation in some manner?
          if(!activation) {
            // If the user chose to load a different keyboard afterward that would affect the same
            // output target, the activation is no longer valid.
            return Promise.resolve(null);
          } else {
            return defermentPromise;
          }
        }),
        metadata: requestedStub
      }
    }
  }
}