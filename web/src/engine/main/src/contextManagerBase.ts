import EventEmitter from 'eventemitter3';
import { type Keyboard, type KeyboardInterface, type OutputTarget } from '@keymanapp/keyboard-processor';
import { type KeyboardStub } from 'keyman/engine/package-cache';
import { PredictionContext } from '@keymanapp/input-processor';

interface EventMap {
  // target, then keyboard.
  'targetchange': (target: OutputTarget) => boolean;
  'beforekeyboardchange': (metadata: KeyboardStub, abortChange: () => void) => void;
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
  readonly resetKeyState: (outputTarget?: OutputTarget) => void;

  /**
   * A predictive-state management object that interfaces the predictive-text banner
   * with the active context.
   */
  readonly predictionContext: PredictionContext;
}

interface PendingActivation {
  target: OutputTarget,
  keyboard: Promise<Keyboard>,
  stub: KeyboardStub;
}

export abstract class ContextManagerBase extends EventEmitter<EventMap> {
  abstract initialize(): void;

  abstract get activeTarget(): OutputTarget;

  private _predictionContext: PredictionContext;
  private _resetKeyState: (outputTarget?: OutputTarget) => void;

  private pendingActivations: PendingActivation[] = [];

  get predictionContext(): PredictionContext {
    return this._predictionContext;
  }

  protected get resetKeyState(): (outputTarget?: OutputTarget) => void {
    return this._resetKeyState;
  }

  constructor() {
    super();
  }

  configure(config: ContextManagerConfiguration) {
    // TODO: Set in followup configuration method.  Part of initialization?
    this._resetKeyState = config.resetKeyState;
    this._predictionContext = config.predictionContext;
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
    this._resetKeyState(this.activeTarget);
    this.predictionContext.resetContext();
  }

  abstract get activeKeyboard(): {keyboard: Keyboard, metadata: KeyboardStub};

  // TODO:  should `activateKeyboard` (on KeymanEngine) be relocated to within here?
  //        It seems to make sense, and was originally a goal.
  abstract set activeKeyboard(kbd: {keyboard: Keyboard, metadata: KeyboardStub});
  abstract setActiveKeyboardAsync(kbd: Promise<Keyboard>, metadata: KeyboardStub): Promise<boolean>;

  /**
   * Checks the pending keyboard-activation array for an entry corresponding to the specified
   * OutputTarget.  If found, also removes the entry for bookkeeping purposes.
   * @param target  The specific OutputTarget affected by the pending Keyboard activation.
   *                May be `null`, which corresponds to the global default Keyboard.
   * @returns `true` if pending activation is still valid, `false` otherwise.
   */
  private findAndPopActivation(target: OutputTarget): boolean {
    // Array.findIndex requires Chrome 45+. :(
    let activationIndex;
    for(activationIndex = 0; activationIndex < this.pendingActivations.length; activationIndex++) {
      if(this.pendingActivations[activationIndex].target == target) {
        break;
      }
    }

    if(activationIndex == this.pendingActivations.length) {
      return false;
    }

    this.pendingActivations.splice(activationIndex, 1);
    return true;
  }

  protected confirmKeyboardChange(metadata: KeyboardStub): boolean {
    const eventReturn = {
      continue: true
    };

    this.emit('beforekeyboardchange', metadata, () => {eventReturn.continue = false});

    return eventReturn.continue;
  }

  protected async deferredKeyboardActivationValid(kbdPromise: Promise<Keyboard>, metadata: KeyboardStub, target: OutputTarget): Promise<boolean> {
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
    return this.findAndPopActivation(target);
  }
}