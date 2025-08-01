import { ContextState } from './context-state.js';

import { LexicalModelTypes } from '@keymanapp/common-types';
import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;

export class ContextTransition {
  private states: [ContextState, ContextState];
  private baseIndex = 0;

  inputDistribution?: Distribution<Transform>;
  // The transform ID in play.
  private _transitionId?: number;

  /**
   * Indicates the portion of the incoming keystroke data, if any, that applies to
   * tokens before the last pre-caret token and thus should not be replaced by predictions
   * based upon `state`.  If the provided context state + the incoming transform do not
   * adequately match the current context, the match attempt will fail with a `null` result.
   *
   * Should generally be non-null if the token before the caret did not previously exist.
   *
   * The result may be null if it does not match the prior context state or if bookkeeping
   * based upon it is problematic - say, if wordbreaking effects shift due to new input,
   * causing a mismatch with the prior state's tokenization.
   * (Refer to #12494 for an example case.)
   */
  preservationTransform?: Transform;

  constructor(context: ContextState, transitionId: number);
  constructor(baseTransition: ContextTransition);
  constructor(param: ContextState | ContextTransition, transitionId?: number) {
    if(!(param instanceof ContextTransition)) {
      const contextState = param;
      // We're initializing a ContextTransition from a blank or reset context.
      const baseState = contextState;
      this.states = [baseState, null];
      this._transitionId = transitionId;
    } else {
      const baseTransition = param;
      Object.assign(this, baseTransition);

      // These need to be deep-copied.
      this.states = baseTransition.states.map((entry) => new ContextState(entry)) as [ContextState, ContextState];
    }
  }

  get base(): ContextState {
    return this.states[this.baseIndex];
  }

  get final(): ContextState {
    return this.states[this.finalIndex]
  }

  private get finalIndex(): number {
    return (this.baseIndex + 1) % 2;
  }

  get transitionId(): number {
    return this._transitionId;
  }

  commitTransition(): ContextTransition {
    // Preserve a deep-copy of the current object before proceeding.
    const cloned = new ContextTransition(this);

    // Commit 'final' and make it the new 'base'.
    const finalIndex = this.baseIndex;
    this.baseIndex = this.finalIndex;

    // The old 'base' does not make a valid new 'final' - drop it.
    this.states[finalIndex] = null;

    // And drop the old transition data while we're at it.
    this.inputDistribution = null;
    this._transitionId = null;

    return cloned;
  }

  replaceFinal(state: ContextState, inputDistribution: Distribution<Transform>, preservationTransform?: Transform) {
    this.states[this.finalIndex] = state;
    this.inputDistribution = inputDistribution;
    // Long-term, this should never be null... but we need to allow it at this point
    // in the refactoring process.
    this._transitionId = inputDistribution?.find((entry) => entry.sample.id !== undefined)?.sample.id;
    this.preservationTransform = preservationTransform;
  }
}