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
import Reversion = LexicalModelTypes.Reversion;
import Suggestion = LexicalModelTypes.Suggestion;
import Transform = LexicalModelTypes.Transform;

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
   * When set, indicates that the text insertion point has returned to the endpoint of a
   * token last edited by application of a Suggestion.  This is not set immediately after
   * it is applied; there must be at least one intermediate edit.
   */
  revertableTransitionId?: number;

  /**
   * Set with the Reversion at the time that a Suggestion was applied, overwriting the
   * original context transition modeled by this instance.
   */
  reversion?: Reversion;

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
   * Deep-copies a ContextTransition instance, optionally assigning a different
   * transition ID to it in the process.
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
      if(baseTransition._final) {
        this._final = new ContextState(baseTransition._final);
      }
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
   */
  finalize(state: ContextState, inputDistribution: Distribution<Transform>) {
    this._final = state;
    this.inputDistribution = inputDistribution;
    // Long-term, this should never be null... but we need to allow it at this point
    // in the refactoring process.
    this._transitionId = inputDistribution?.find((entry) => entry.sample.id !== undefined)?.sample.id;
  }

  /**
   * Applies a suggestion generated from this context transition on top of the transition itself,
   * replacing its final context state.  This does _not_, however, replace the original fat-finger
   * distribution or other intermediate data regarding associated keystrokes.
   * @param suggestion
   * @returns
   */
  applySuggestion(suggestion: Suggestion): {
    base: ContextTransition,
    appended?: ContextTransition
  } {
    const preAppliedState = this.final;
    if(!preAppliedState.suggestions?.find((s) => s.id == suggestion?.id)) {
      throw new Error("Could not find matching suggestion to apply");
    }

    // This closure captures `suggestion`, and definition here / within the class also
    // gives us private access to `._final`.
    const buildAppliedTransition = (
      transition: ContextTransition,
      baseState: ContextState,
      transform: Transform,
      appliedTransitionId: number
    ) => {
      const state = baseState.analyzeTransition(
        baseState.context,
        [{sample: transform, p: 1}],
        appliedTransitionId
      ).final;

      transition._final = state;

      // Applying a suggestion should not forget the original suggestion set.
      state.appliedSuggestionId = suggestion.id;
      state.suggestions = preAppliedState.suggestions;
    }

    // Start from a deep copy, then replace as needed to overwrite with the context
    // state resulting from the suggestion while preserving suggestion + primary
    // keystroke data.

    const resultTransition = new ContextTransition(this);
    buildAppliedTransition(resultTransition, this.base, suggestion.transform, suggestion.transformId);

    // An applied suggestion should replace the original Transition's effects, though keeping
    // the original input around.
    resultTransition._transitionId = suggestion.transformId;
    resultTransition.final.appliedInput = preAppliedState.appliedInput;

    if(!suggestion.appendedTransform) {
      return { base: resultTransition };
    }

    const finalTransition = new ContextTransition(resultTransition.final, suggestion.appendedTransform.id);
    buildAppliedTransition(finalTransition, resultTransition.final, suggestion.appendedTransform, suggestion.transformId);

    // The appended transform is applied with no intermediate input.
    finalTransition.final.appliedInput = { insert: '', deleteLeft: 0 };
    finalTransition.inputDistribution = [];

    return {
      base: resultTransition,
      appended: finalTransition
    };
  }

  /**
   * Recreates the original context transition and its effects from before
   * any application of suggestions based on the transition was applied.
   * @returns
   */
  reproduceOriginal() {
    // By keeping the original keystroke data and effects around even after
    // applying the suggestion, we can easily reconstruct the original .final.
    const original = this.base.analyzeTransition(
      this.base.context,
      this.inputDistribution
    );

    if(this.final.suggestions) {
      original.final.suggestions = this.final.suggestions;
    }

    return original;
  }
}