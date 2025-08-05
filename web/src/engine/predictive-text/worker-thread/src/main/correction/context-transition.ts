/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * Represents cached data about a single context transition event, as well
 * as the state of the context both before and after the transition.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';

import { ContextState } from './context-state.js';

import Distribution = LexicalModelTypes.Distribution;
import Suggestion = LexicalModelTypes.Suggestion;
import Transform = LexicalModelTypes.Transform;
import { buildMergedTransform } from '@keymanapp/models-templates';
import { ContextTracker } from './context-tracker.js';

/**
 * Represents the transition between two context states as triggered
 * by input keystrokes or applied suggestions.
 */
export class ContextTransition {
  /**
   * Represents the state of the context before the transition event occurred.
   */
  readonly base: ContextState;
  private _final: ContextState;

  /**
   * Indicates the fat-finger distribution for the incoming keystroke related to
   * the context transition event.
   */
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

  /**
   * Constructs a partial context transition object for use during the process
   * of analyzing context transitions or for representing the base state of a
   * reset context.
   * @param context  The base state for the represented context transition
   * @param transitionId  The unique ID corresponding to the transition event
   * or context state.
   */
  constructor(context: ContextState, transitionId: number);
  /**
   * Deep-copies a ContextTransition instance.
   * @param baseTransition
   */
  constructor(baseTransition: ContextTransition);
  constructor(param: ContextState | ContextTransition, transitionId?: number) {
    if(!(param instanceof ContextTransition)) {
      const contextState = param;
      // We're initializing a ContextTransition from a blank or reset context.
      this.base = contextState;
      this._final = null;
      this._transitionId = transitionId;
    } else {
      const baseTransition = param;
      Object.assign(this, baseTransition);

      // These need to be deep-copied.
      this.base = new ContextState(baseTransition.base);
      this._final = new ContextState(baseTransition._final);
    }
  }

  /**
   * Gets the context state resulting from the context transition event,
   * including any generated suggestions and data regarding potential
   * application thereof.
   */
  get final(): ContextState {
    return this._final;
  }

  /**
   * The unique ID corresponding to the transition event or context state.
   */
  get transitionId(): number {
    return this._transitionId;
  }

  /**
   * Records the context state resulting from the context transition generated
   * by a keystroke.
   * @param state  The context state to record as the result of the transition
   * @param inputDistribution Fat-finger data corresponding to the triggering keystroke
   * @param preservationTransform Portions of the most likely input that do not contribute to the final token
   * in the final context's tokenization.
   */
  finalize(state: ContextState, inputDistribution: Distribution<Transform>, preservationTransform?: Transform) {
    this._final = state;
    this.inputDistribution = inputDistribution;
    // Long-term, this should never be null... but we need to allow it at this point
    // in the refactoring process.
    this._transitionId = inputDistribution?.find((entry) => entry.sample.id !== undefined)?.sample.id;
    this.preservationTransform = preservationTransform;
  }

  /**
   * Applies a suggestion generated from this context transition on top of the transition itself,
   * replacing its final context state.  This does _not_, however, replace the original fat-finger
   * distribution or other intermediate data regarding associated keystrokes.
   * @param suggestion
   * @returns
   */
  applySuggestion(suggestion: Suggestion) {
    const fullTransform = suggestion.appendedTransform
      ? buildMergedTransform(suggestion.transform, suggestion.appendedTransform)
      : suggestion.transform;

    // An applied suggestion should replace the original Transition's effects, though keeping
    // the original input around.
    const appliedState = ContextTracker.attemptMatchContext(
      this.base.context,
      this.base.model,
      this.base,
      [{sample: fullTransform, p: 1}],
      true
    ).final;

    const preAppliedState = this.final;
    if(!preAppliedState.suggestions.find((s) => s.id == suggestion?.id)) {
      throw new Error("Could not find matching suggestion to apply");
    }

    // Start from a deep copy, then replace as needed to overwrite with the context
    // state resulting from the suggestion while preserving suggestion + primary
    // keystroke data.
    const resultTransition = new ContextTransition(this);
    resultTransition._final = appliedState;
    resultTransition._transitionId = suggestion.transformId;

    appliedState.appliedSuggestionId = suggestion.id;
    appliedState.appliedInput = preAppliedState.appliedInput;
    appliedState.suggestions = preAppliedState.suggestions;

    return resultTransition;
  }
}