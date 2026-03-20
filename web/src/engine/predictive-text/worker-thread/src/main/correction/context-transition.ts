/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * Represents cached data about a single context transition event, as well
 * as the state of the context both before and after the transition.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';
import { applyTransform } from '@keymanapp/models-templates';

import { ContextState } from './context-state.js';
import { ContextTokenization } from './context-tokenization.js';
import { precomputeTransitions, transitionTokenizations } from './transition-helpers.js';

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
  applySuggestion(suggestion: Suggestion /*, wasManuallyApplied: boolean */): {
    base: ContextTransition,
    appended?: ContextTransition
  } {
    if(!this.final.suggestions?.find((s) => s.id == suggestion?.id)) {
      throw new Error("Could not find matching suggestion to apply");
    }

    const lexicalModel = this.base.model;

    // Goal:  allow multiple base tokenizations.

    // // Only keep the other context versions - along with a copy of the original
    // // one - if the suggestion was auto-applied; that may not have been
    // // intentional.  If explicit (via manual), the other context versions are
    // // considered to have been wrong and should be discarded.  (They can be
    // // restored via reversion, though.)
    //
    // const preservedVariations = (
    //   wasManuallyApplied ? [] : [this.tokenization].map((t) => {
    //   return t.applyContextSlide(lexicalModel, slideUpdateTransform);
    // });

    const performTransitionStep = (baseState: ContextState, rootTokenization: ContextTokenization, transformToApply: Transform, inputDistribution: Distribution<Transform>) => {
      const appliedDistribution = [{sample: transformToApply, p: 1}];
      const { subsets: applicationSubsets, keyMatchingUserContext } = precomputeTransitions(
        [rootTokenization], appliedDistribution
      );

      // Filter out insert and delete edges here!  ONLY the primary substitution
      // edge should be permitted!
      applicationSubsets.forEach((value, key) => {
        // When applying suggestions, only consider the actual tokenization that would result.
        if(key != keyMatchingUserContext) {
          applicationSubsets.delete(key);
        }

        // TODO:  verify that 'insert' and 'delete' edit-spurs are ignored (once
        // they're supported)
      })

      const resultingTokenization = transitionTokenizations(
        applicationSubsets,
        appliedDistribution
      ).get(keyMatchingUserContext);

      // Tag the result as revertable - but only on the last token.
      //
      // We won't try to partially revert a multi-word suggestion; reversions
      // are only supported at the end of the last word of the main suggestion
      // body and after any appended whitespace.
      resultingTokenization.tail.appliedTransitionId = suggestion.transformId;

      const resultingState = new ContextState(applyTransform(transformToApply, baseState.context), lexicalModel);
      resultingState.tokenization = resultingTokenization; // [resultingTokenization].concat(preservedVariations);
      resultingState.appliedInput = transformToApply;
      resultingState.appliedSuggestionId = suggestion.id;
      resultingState.suggestions = this.final.suggestions;

      const resultingTransition = new ContextTransition(baseState, transformToApply.id);
      resultingTransition.finalize(resultingState, inputDistribution);
      resultingTransition.revertableTransitionId = suggestion.transformId;
      resultingTransition._transitionId = transformToApply.id;

      return {
        transition: resultingTransition,
        tokenization: resultingTokenization
      };
    }

    // Suggestions always apply to the version of context that the user last saw before
    // the input triggering the suggestion..
    //
    // Clone the base state in order to prevent cross-contamination from other operations (?)
    const results = performTransitionStep(
      new ContextState(this.base),
      this.base.displayTokenization,
      suggestion.transform,
      this.inputDistribution
    );

    if(!suggestion.appendedTransform) {
      return {
        base: results.transition,
        appended: null
      };
    }

    // Appended transforms apply to the context resulting from that.
    const appendingTransition = performTransitionStep(results.transition.final, results.tokenization, suggestion.appendedTransform, []).transition;
    appendingTransition.final.appliedInput = { insert: '', deleteLeft: 0 };

    // Ensure the appended tokens all have the transition ID tagged to enable reversion.
    // We allow reversion on any post-suggestion appended components.
    const baseTokenizationLength = results.transition.final.displayTokenization.tokens.length;
    const appliedTokenization = appendingTransition.final.displayTokenization;
    for(let i = baseTokenizationLength; i < appliedTokenization.tokens.length; i++) {
      appliedTokenization.tokens[i].appliedTransitionId = suggestion.transformId;
    }

    return {
      base: results.transition,
      appended: appendingTransition
    }
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