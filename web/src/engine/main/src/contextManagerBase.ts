import EventEmitter from 'eventemitter3';
import { type Keyboard, type KeyboardInterface, type OutputTarget } from '@keymanapp/keyboard-processor';
import { type KeyboardStub } from 'keyman/engine/keyboard-cache';
import { PredictionContext } from '@keymanapp/input-processor';

interface EventMap {
  // target, then keyboard.
  'targetchange': (target: OutputTarget) => void;
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

export abstract class ContextManagerBase extends EventEmitter<EventMap> {
  abstract initialize(): void;

  abstract get activeTarget(): OutputTarget;

  private _predictionContext: PredictionContext;
  private _resetKeyState: (outputTarget?: OutputTarget) => void;

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
  abstract set activeKeyboard(kbd: {keyboard: Keyboard, metadata: KeyboardStub});
}

// Intended design:
// - SiteContextManager - for website, document-aware context management
//   - app/embed
// - PassthroughContextManager - for WebView-hosted, app-embedded context management.
//   - app/web