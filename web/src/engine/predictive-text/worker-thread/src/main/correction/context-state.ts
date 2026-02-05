/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * Represents cached data about the state of the sliding context window either
 * before or after a context transition event and related functionality,
 * including logic used to determine the transition from one context state to
 * another.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';
import { applyTransform } from '@keymanapp/models-templates';
import { KMWString } from '@keymanapp/web-utils';

import { ContextToken } from './context-token.js';
import { ContextTokenization } from './context-tokenization.js';
import { ContextTransition } from './context-transition.js';
import { determineModelTokenizer } from '../model-helpers.js';
import { legacySubsetKeyer, TokenizationSubsetBuilder } from './tokenization-subsets.js';
import TransformUtils from '../transformUtils.js';

import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Suggestion = LexicalModelTypes.Suggestion;
import Transform = LexicalModelTypes.Transform;

/**
 * Represents a state of the active context at some point in time along with the
 * results of all related, reusable predictive-text operations.
 */
export class ContextState {
  /**
   * The context window in view for the represented Context state,
   * as passed between the predictive-text worker and its host.
   */
  readonly context: Context;

  /**
   * The active lexical model operating upon the Context.
   */
  readonly model: LexicalModel;

  /**
   * Denotes the most likely tokenization for the represented Context.
   */
  tokenization: ContextTokenization;

  /**
   * Denotes the keystroke-sourced Transform that was last applied to a
   * prior ContextState.
   *
   * Note:  if this specific ContextState resulted from applying a
   * Suggestion, this may not match text seen in the current Context!
   */
  appliedInput?: Transform;

  /**
   * Denotes all keystroke data contributing to ContextTokens seen in
   * .tokenization.  For each contributing context transition, its ID
   * may be used to retrieve the original fat-finger distribution for
   * potential keystroke effects.
   */
  inputTransforms: Map<number, Distribution<Transform>>;

  /**
   * The full set of Suggestions produced for the transition to this context state.
   *
   * May be undefined if no suggestions were generated for this state.
   */
  suggestions?: Suggestion[];

  /**
   * If set, denotes the suggestion ID for the suggestion (from .suggestions) that
   * was applied for the final transition to this context state.
   */
  appliedSuggestionId?: number;

  /**
   * If a suggestion was applied, returns the transition ID associated with the
   * applied suggestion.
   *
   * Otherwise, returns `undefined`.
   */
  get appliedSuggestionTransitionId(): number | undefined {
    if(!this.appliedSuggestionId) {
      return undefined;
    }

    return this.suggestions.find(s => s.id == this.appliedSuggestionId)?.transformId;
  }

  /**
   * Indicates whether or not the applied suggestion (if it exists) was applied
   * directly by the user.
   *
   * - `true` if directly applied via banner interaction or other explicitly-intended
   * behaviors
   * - `false` if indirectly applied (say, by triggering whitespace/punctuation input)
   * - `undefined` if no suggestion has been applied.
   */
  isManuallyApplied?: boolean;

  /**
   * Deep-copies a prior instance.
   * @param stateToClone
   */
  constructor(stateToClone: ContextState);
  /**
   * Initializes a new ContextState instance based on the active model and context.
   *
   * If a precomputed tokenization of the context (with prior correction-search
   * calculation data) is not available, it will be spun up from scratch.
   *
   * @param context The context available within the current sliding context-window
   * @param model The active lexical model.
   * @param tokenization Precomputed tokenization for the context, leveraging previous
   * correction-search progress and results
   */
  constructor(context: Context, model: LexicalModel, tokenization?: ContextTokenization);
  constructor(param1: Context | ContextState, model?: LexicalModel, tokenization?: ContextTokenization) {
    if(!(param1 instanceof ContextState)) {
      this.context = param1;
      this.model = model;
      if(tokenization) {
        this.tokenization = tokenization;
      } else {
        this.initFromReset();
      }
    } else {
      const stateToClone = param1;

      Object.assign(this, stateToClone);
      this.inputTransforms = new Map(stateToClone.inputTransforms);
      this.tokenization = new ContextTokenization(stateToClone.tokenization);

      // A shallow copy of the array is fine, but we'd be best off
      // not aliasing the array itself.
      if(stateToClone.suggestions?.length ?? 0 > 0) {
        this.suggestions = [].concat(stateToClone.suggestions);
      }
    }
  }

  /**
   * Initializes the ContextState instance for use when no valid prior
   * information is available - typically, immediately after engine
   * initialization or a context reset.
   */
  private initFromReset() {
    const tokenizedContext = determineModelTokenizer(this.model)(this.context).left;
    const baseTokens = tokenizedContext.map((entry) => {
      const token = new ContextToken(this.model, entry.text);

      if(entry.isWhitespace) {
        token.isWhitespace = true;
      }

      return token;
    });

    // And now build the final context state object, which includes whitespace 'tokens'.);
    if(baseTokens.length == 0) {
      baseTokens.push(new ContextToken(this.model));
    }
    this.tokenization = new ContextTokenization(baseTokens);
    this.inputTransforms = new Map();
  }

  /**
   * As written, this method attempts to determine the context state and tokenization(s)
   * that result from applying an incoming transform distribution to the incoming context
   * state based upon data from _this_ instance's modeled context state.
   *
   * If the two versions of Context are not alignable, this method will return null.
   *
   * If they are alignable, this method will reuse as much prior cached data (for
   * tokenization and correction-search reuse) as possible when tokenizing and building
   * the final resulting ContextState for the modeled ContextTransition.
   *
   * @param context The incoming Context instance for a prediction or suggestion application,
   * which should be alignable to this instance's .context.  (I.e, should be "the same"
   * context after adjusting for sliding context-window behaviors.)
   * @param transformDistribution A distribution of incoming potential edits to the context -
   * typically from a keystroke's fat-finger distribution.
   *
   * May also contain a single entry for applying Suggestions or when correction behavior
   * is disabled.
   * @param isApplyingSuggestion When true, alters behavior to better model application of suggestions.
   * @returns
   */
  analyzeTransition(
    context: Context,
    transformDistribution: Distribution<Transform>,
    // overrides checks for token substitution that can fail for large applied suggestions.
    isApplyingSuggestion?: boolean
  ): ContextTransition {
    const lexicalModel = this.model;

    const trueInput = transformDistribution[0].sample;
    const transition = new ContextTransition(this, this.appliedInput?.id);

    // From here on, we work toward the common-case - re-using old info when
    // context (and its tokenization) is changed by an input Transform.

    let trueInputSubsetKey: string;
    const slideUpdateTransform = determineContextSlideTransform(this.context, context);

    // Goal:  allow multiple base tokenizations.
    const startTokenizations = [this.tokenization];
    const startTokenizationsAfterSlide = startTokenizations.map(t => t.applyContextSlide(lexicalModel, slideUpdateTransform));

    // Easy case - no net change to the tokenizations whatsoever; the actual request
    // aims to save-state the most recent results.
    //
    // This behavior occurs during context resets & after applying suggestions/reversions.
    if(TransformUtils.isEmpty(trueInput) && transformDistribution.length == 1) {
      // If the tokenizations match, clone the ContextState; we want to preserve a post-application
      // context separately from pre-application contexts for predictions based on empty roots.
      const state = new ContextState(this);
      state.tokenization = startTokenizationsAfterSlide[0];
      transition.finalize(state, transformDistribution);
      return transition;
    }

    const subsetBuilder = new TokenizationSubsetBuilder(legacySubsetKeyer);
    for(let baseTokenization of startTokenizationsAfterSlide) {

      for(let mass of transformDistribution) {
        const tokenizationAnalysis = baseTokenization.mapWhitespacedTokenization(lexicalModel, mass.sample);
        subsetBuilder.addPrecomputation(baseTokenization, tokenizationAnalysis, mass.p);

        if(mass.sample == trueInput) {
          trueInputSubsetKey = subsetBuilder.keyer(tokenizationAnalysis);
        }
      }
    }

    // And now to (partly) detransform from a multiple-tokenization paradigm.
    const trueInputSubset = subsetBuilder.subsets.get(trueInputSubsetKey);
    // Right now, we only have one base tokenization, so we just fetch it.
    const baseTokenization = startTokenizationsAfterSlide[0];
    // For multiple tokenizations, we'd retrieve each, use the "most likely" one as base,
    // and then fold all resulting search spaces (on the final token) into one.
    const tokenizationAnalysis = trueInputSubset.transitionEdges.get(baseTokenization);

    // Determine the best probability from among ALL available inputs, before they're split
    // into subsets.
    const bestProb = transformDistribution.reduce((best, curr) => Math.max(best, curr.p), 0);
    // Should gain one per subsetBuilder.subsets entry.
    const resultTokenization = baseTokenization.evaluateTransition(tokenizationAnalysis, lexicalModel, trueInput, bestProb);

    // ------------

    // So, if we have a suggestion transition ID at the end and didn't just apply...
    // we've just returned to the end of an applied suggestion's token.
    //
    // epic/dict-breaker:  if ANY decently-likely tokenization satisfies this, we still
    // have a reasonable candidate for display of a delayed reversion.  (Not 'all' -
    // 'any'.)
    const tokens = resultTokenization.tokens;
    const lastIndex = tokens.length - 1;
    // Ignore a context-final empty '' token; the interesting one is what comes before.
    const nonEmptyTail = !tokens[lastIndex].isEmptyToken ? tokens[lastIndex] : tokens[lastIndex - 1];
    const appliedSuggestionTransitionId = nonEmptyTail?.appliedTransitionId;

    const state = new ContextState(applyTransform(trueInput, context), lexicalModel);
    state.tokenization =  new ContextTokenization(resultTokenization.tokens, tokenizationAnalysis, resultTokenization.taillessTrueKeystroke);
    state.appliedInput = transformDistribution?.[0].sample;
    transition.finalize(state, transformDistribution);
    transition.revertableTransitionId = appliedSuggestionTransitionId;
    return transition;
  }
}

/**
 * Determines the changes in leading-edge text between two contexts with the
 * same trailing-edge contents.
 *
 * This function assumes that text is only either added to or removed from the
 * second context's data, as if due to shifting the boundaries of the sliding
 * context window.
 * @param srcContext A previous context, which may be computed from the prior
 * transition and its effects.
 * @param dstContext The current context visible through the sliding context
 * window.
 * @returns The substring prepended to the context (if sliding backward) or the
 * number of codepoints removed from its start (if sliding forward)
 */
export function determineContextSlideTransform(srcContext: Context, dstContext: Context): Transform & { deleteRight: number } {
  // Assumption: the current (sliding) context window is alignable.
  // See `matchBaseContextState` in ../predict-helpers.ts.

  // Assertion:  If the assumption above holds and both start-of-buffer flags
  // are true, the contents must then match.
  if(srcContext.startOfBuffer && dstContext.startOfBuffer) {
    // Validate that they actually match.
    // If not, the contexts shouldn't equal.
    if(srcContext.left == dstContext.left) {
      return { insert: '', deleteLeft: 0, deleteRight: 0 };
    } else {
      return null;
    }
  }

  // Assumption:  the right-hand side of the left-context strings WILL match.
  // The only change should be for the contents of the sliding-context window.
  const src = srcContext.left;
  const dst = dstContext.left;

  // Assumption:  the context will always be codepoint-aligned, as the Web engine
  // and worker both do string ops based on codepoints, not code units.

  // Which way did the context window slide, if it did?  This does not
  // vary for different tokenizations; we can determine exactly how
  // much text was prepended (with end text deleted in last edit) or
  // deleted (with end text added in last edit).
  const rawDelta = dst.length - src.length;

  // Validation:  does the part of both strings that should match actually match?
  //
  // Context operations are already code-point aligned; no need to use special
  // non-BMP handling here.
  if(rawDelta > 0 ? dst.slice(rawDelta) != src : src.slice(-rawDelta) != dst) {
    return null;
  }

  return {
    // As we're codepoint-aligned, the part not in common must also be codepoint
    // aligned - no need to incur SMP-aware functionality overhead.
    insert: rawDelta > 0 ? dst.slice(0, rawDelta) : '',
    deleteLeft: 0,
    deleteRight: rawDelta < 0 ? KMWString.length(src.slice(0, -rawDelta)) : 0
  }
}