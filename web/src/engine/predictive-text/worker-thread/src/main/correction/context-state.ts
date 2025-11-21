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
import { ContextTokenization, determineTaillessTrueKeystroke } from './context-tokenization.js';
import { ContextTransition } from './context-transition.js';
import { determineModelTokenizer } from '../model-helpers.js';
import { SearchQuotientCluster } from './search-quotient-cluster.js';
import { SearchQuotientNode } from './search-quotient-node.js';
import { precomputationSubsetKeyer, TokenizationSubsetBuilder } from './tokenization-subsets.js';
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
  tokenizations: ContextTokenization[];

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
  constructor(context: Context, model: LexicalModel, tokenizations?: ContextTokenization[]);
  constructor(param1: Context | ContextState, model?: LexicalModel, tokenizations?: ContextTokenization[]) {
    if(!(param1 instanceof ContextState)) {
      this.context = param1;
      this.model = model;
      if(tokenizations) {
        this.tokenizations = tokenizations;
      } else {
        this.initFromReset();
      }
    } else {
      const stateToClone = param1;

      Object.assign(this, stateToClone);
      this.inputTransforms = new Map(stateToClone.inputTransforms);
      this.tokenizations = stateToClone.tokenizations.map(t => new ContextTokenization(t));

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
    this.tokenizations = [new ContextTokenization(baseTokens)];
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
   * @param appliedSuggestionId When defined, notes the original transition ID corresponding to
   * the applied suggestion.
   * @returns
   */
  analyzeTransition(
    context: Context,
    transformDistribution: Distribution<Transform>,
    // overrides checks for token substitution that can fail for large applied suggestions.
    appliedSuggestionId?: number
  ): ContextTransition {
    const lexicalModel = this.model;
    const trueInput = transformDistribution[0].sample;

    // Determine the best probability from among ALL available inputs, before they're split
    // into subsets.
    const bestProb = transformDistribution.reduce((best, cur) => best < cur.p ? cur.p : best, 0);
    const transition = new ContextTransition(this, this.appliedInput?.id);

    // From here on, we work toward the common-case - re-using old info when
    // context (and its tokenization) is changed by an input Transform.
    const slideUpdateTransform = determineContextSlideTransform(this.context, context);

    // Goal:  allow multiple base tokenizations.
    const startTokenizations: Set<ContextTokenization> = new Set();
    const keyedTokenizations: Map<string, ContextTokenization> = new Map();
    this.tokenizations.forEach(t => {
      const slidTokenization = t.applyContextSlide(lexicalModel, slideUpdateTransform);
      startTokenizations.add(slidTokenization);
      keyedTokenizations.set(t.clusteringKey, slidTokenization)
    });

    // Easy case - no net change to the tokenizations whatsoever; the actual request
    // aims to save-state the most recent results.
    //
    // This behavior occurs during context resets & after applying suggestions/reversions.
    if(TransformUtils.isEmpty(trueInput) && transformDistribution.length == 1) {
      // If the tokenizations match, clone the ContextState; we want to preserve a post-application
      // context separately from pre-application contexts for predictions based on empty roots.
      const state = new ContextState(this);
      state.tokenizations = [...startTokenizations.values()];
      transition.finalize(state, transformDistribution);
      return transition;
    }

    const subsetBuilder = new TokenizationSubsetBuilder(precomputationSubsetKeyer);
    for(let baseTokenization of startTokenizations.values()) {
      for(let mass of transformDistribution) {
        // Handle the splits and merges early, here.
        const tokenizationAnalysis = baseTokenization.mapWhitespacedTokenization(lexicalModel, mass.sample);
        const alignment = tokenizationAnalysis.alignment;

        // Pre-process any splits and merges; the result of these operations may
        // have the same properties as other base tokenizations within the
        // subset if compatible.
        const needsRealignment = (alignment.merges.length > 0 || alignment.splits.length > 0 || alignment.unmappedEdits.length > 0);
        const sourceTokenization = needsRealignment ? baseTokenization.realign(alignment) : baseTokenization;

        subsetBuilder.addPrecomputation(sourceTokenization, tokenizationAnalysis, mass.p);
      }
    }

    // For all target tokenizations - each transition subset...
    const finalTokenizations = [...subsetBuilder.subsets.values()].map((subset) => {
      // Iterate over all _source_ tokenizations and the changes used to transition them
      // to that target tokenization.
      const transitionSets = [...subset.transitionEdges.entries()];
      const isolatedSubsetResults = transitionSets.map((precomp) => {
        const rootTokenization = precomp[0];

        return rootTokenization.evaluateTransition(precomp[1], trueInput.id, bestProb, appliedSuggestionId);
      });

      // Super-easy case:  there's only the one tokenization anyway.
      if(isolatedSubsetResults.length == 1) {
        return isolatedSubsetResults[0];
      }

      // Assumption:  all produced "isolatedSubsetResults" should essentially be
      // the same tokenization. That said, tail entries will likely not be
      // perfect matches; we need to splice them together, without duplicates.
      // We also cannot rely on tokens before the standard tail index having
      // been unmodified; merges and splits may have been applied earlier in the
      // sequence.

      const tokenCount = isolatedSubsetResults[0].tokens.length;
      if(isolatedSubsetResults.find(sr => sr.tokens.length != tokenCount)) {
        throw new Error("Assumption invalidated:  incoming tokenization paths do not converge");
      }

      const finalizedTokenization: ContextToken[] = [];
      for(let i = 0; i < tokenCount; i++) {
        const spaceSet: Set<SearchQuotientNode> = new Set();
        let isWhitespace = true;
        let isPartial = false;

        isolatedSubsetResults.map((sr) => sr.tokens[i]).forEach((token) => {
          const searchSpace = token.searchModule;
          isWhitespace &&= token.isWhitespace;
          isPartial ||= token.isPartial;

          if(searchSpace instanceof SearchQuotientCluster) {
            searchSpace.parents.forEach(p => spaceSet.add(p));
          } else {
            spaceSet.add(searchSpace);
          }
        });

        const setVals = [...spaceSet.values()]
        const finalizedSpace = setVals.length > 1 ? new SearchQuotientCluster(setVals) : setVals[0];

        const token = new ContextToken(finalizedSpace);
        token.isWhitespace = isWhitespace;
        token.isPartial = isPartial;

        finalizedTokenization.push(token)
      }

      return new ContextTokenization(
        finalizedTokenization,
        transitionSets[0][1],
        determineTaillessTrueKeystroke(transitionSets[0][1])
      );
    });

    // ------------

    // So, if we have a suggestion transition ID at the end and didn't just apply...
    // we've just returned to the end of an applied suggestion's token.
    //
    // epic/dict-breaker:  if ANY decently-likely tokenization satisfies this, we still
    // have a reasonable candidate for display of a delayed reversion.  (Not 'all' -
    // 'any'.)

    const state = new ContextState(applyTransform(trueInput, context), lexicalModel);
    // Set tokenizations from above.
    // TODO:
    // - sort by most .tail.searchSpace.bestExample.p?
    // - threshold to the N most likely tokenizations?
    state.tokenizations = finalTokenizations;
    state.appliedInput = transformDistribution?.[0].sample;
    transition.finalize(state, transformDistribution);

    // Maybe sort the tokenizations in some manner, first?
    transition.revertableTransitionId = state.tokenizations.map((tokenization) => {
      const tokens = tokenization.tokens;
      const lastIndex = tokens.length - 1;
      // Ignore a context-final empty '' token; the interesting one is what comes before.
      const nonEmptyTail = !tokens[lastIndex].isEmptyToken ? tokens[lastIndex] : tokens[lastIndex - 1];
      return nonEmptyTail?.appliedTransitionId;
    }).find((transitionId) => {
      return transitionId !== undefined;
    });
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