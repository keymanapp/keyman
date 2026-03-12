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
import { InsertionQuotientSpur } from './insertion-quotient-spur.js';
import { DeletionQuotientSpur } from './deletion-quotient-spur.js';
import { SearchQuotientCluster } from './search-quotient-cluster.js';
import { SearchQuotientNode } from './search-quotient-node.js';
import { generateSubsetId, precomputationSubsetKeyer, TokenizationSubsetBuilder } from './tokenization-subsets.js';
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

    const finalTokenizations = this.transitionTokenizations(startTokenizations, transformDistribution, appliedSuggestionId);

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
    // Also, avoid retaining a transition ID invalidly...
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

  private buildTransitionTargetSets(
    startTokenizations: Set<ContextTokenization>,
    transformDistribution: Distribution<Transform>
  ) {
    // Deletions:
    // - handle the incoming key, but do not extend the token.
    // - is nearly the same as an empty '', deleteLeft: 0 input
    // - technically could use a `null` Transform to indicate "deletion" transition?
    // - would need to overwrite the entry in `tokenizationAnalysis.tokenizedTransform` to facilitate this

    // Insertions:
    // - just... auto-add as appropriate on top of deletions
    // - problem:  how to 'bin' them?  To match the insertion to any pre-existing bucket?

    const subsetBuilder = new TokenizationSubsetBuilder(precomputationSubsetKeyer);
    for(let baseTokenization of startTokenizations.values()) {
      for(let mass of transformDistribution) {
        // Handle the splits and merges early, here.
        const tokenizationAnalysis = baseTokenization.mapWhitespacedTokenization(this.model, mass.sample);
        const alignment = tokenizationAnalysis.alignment;

        // Pre-process any splits and merges; the result of these operations may
        // have the same properties as other base tokenizations within the
        // subset if compatible.
        const needsRealignment = (alignment.merges.length > 0 || alignment.splits.length > 0 || alignment.unmappedEdits.length > 0);
        const sourceTokenization = needsRealignment ? baseTokenization.realign(alignment) : baseTokenization;

        subsetBuilder.addPrecomputation(sourceTokenization, tokenizationAnalysis, mass.p);
      }

      // Also prep for a 'delete' transition:  no change to text, but handles the key anyway.
      const tokenizationAnalysis = baseTokenization.mapWhitespacedTokenization(this.model, {insert: '', deleteLeft: 0});
      // The `null` case indicates the 'delete' transition.
      tokenizationAnalysis.tokenizedTransform = null;
      // TODO:  Though... what if we did it a different way and supported
      // metadata for BOTH `delete` AND `insert`?  We could pre-bin even
      // `insert` cases here, maybe.
      //
      // A tweak to precomputationSubsetKeyer (allowing +1 or +2 inserts) could
      // allow computing the offset key.  This may be doable within
      // `.preComputation`?  How would the markers for that look, though...?
      subsetBuilder.addPrecomputation(baseTokenization, tokenizationAnalysis, 0);
    }

    return subsetBuilder.subsets;
  }

  private transitionTokenizations(
    startTokenizations: Set<ContextTokenization>,
    transformDistribution: Distribution<Transform>,
    // overrides checks for token substitution that can fail for large applied suggestions.
    appliedSuggestionId?: number
  ) {
    const subsets = this.buildTransitionTargetSets(startTokenizations, transformDistribution);

    const trueInput = transformDistribution[0].sample;
    // Determine the best probability from among ALL available inputs, before they're split
    // into subsets.
    const bestProb = transformDistribution.reduce((best, cur) => best < cur.p ? cur.p : best, 0);

    // For all target tokenizations - each transition subset...
    const finalTokenizations: Map<string, ContextTokenization> = new Map();

    [...subsets.entries()].forEach(([key, subset]) => {
      // Iterate over all _source_ tokenizations and the changes used to transition them
      // to that target tokenization.
      const transitionSets = [...subset.transitionEdges.entries()];
      const isolatedSubsetResults = transitionSets.flatMap((precomp) => {
        const rootTokenization = precomp[0];

        // Handle deletions here?  Could always .flatMap above .map call + return an array - one with insertion, one with deletion.
        // Probably:  define new transition method for tokenization:  keeps the same, tags the new input (as deletion only)

        if(precomp[1].viaDeletion) {
          if(appliedSuggestionId !== undefined) {
            return [] as ContextTokenization[];
          }
          const baseTokenization = precomp[0];
          const deletionSpur = new DeletionQuotientSpur(
            baseTokenization.tail.searchModule,
            transformDistribution, {
              segment: {
                transitionId: transformDistribution[0].sample.id,
                start: 0
              },
              subsetId: generateSubsetId(),
              bestProbFromSet: bestProb
            }
          );

          const tokens = baseTokenization.tokens;
          const newTail = new ContextToken(deletionSpur);
          tokens[tokens.length - 1] = newTail;
          return new ContextTokenization(
            tokens,
            null,
            {insert: '', deleteLeft: 0}
          );
        } else {
          // Following call:  is actually designed to build SubstitutionQuotientSpurs.
          return rootTokenization.evaluateTransition(precomp[1], trueInput.id, bestProb, appliedSuggestionId);
        }
      });

      // Super-easy case:  there's only the one tokenization anyway.
      if(isolatedSubsetResults.length == 1) {
        finalTokenizations.set(key, isolatedSubsetResults[0])
        return;
      }

      // A delete-only transition during suggestion application,
      // which should not permit delete edge construction.
      if(isolatedSubsetResults.length == 0) {
        return;
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

      const sourceRangeKey = isolatedSubsetResults[0].tail.sourceRangeKey;
      if(isolatedSubsetResults.find(sr => sr.tail.sourceRangeKey != sourceRangeKey)) {
        throw new Error("Assumption invalidated:  incoming tokenizations do not cover same input range")
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
            // TODO:  detach the cluster's queue, etc for memory reclamation?
            // (The part that receives newly processed nodes from parents.)
            //
            // Then again, that only gets attached in Spurs, right?
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

      // TODO:  process 'insert' edits too.  (Based on number of legal remaining
      // edits?) ALSO NOTE:  root node constructions need initial 'insert'
      // edit-spur constructions + tokenizations, too. ... what if we allow
      // adding new (insert) spurs to the clusters after initial construction?
      // - so, rather than reconstruct from an incomplete cluster, just... add
      //   onto it?
      // - is "less elegant" in terms of how clusters should operate, BUT avoids
      //   nasty cluster-creation ordering logic.

      // Handle insert spurs here & add them to clusters.  Double-check that we
      // aren't pushing too far with 'em.

      finalTokenizations.set(key, new ContextTokenization(
        finalizedTokenization,
        transitionSets[0][1],
        determineTaillessTrueKeystroke(transitionSets[0][1])
      ));
    });

    if(appliedSuggestionId === undefined) {
      // Construct insert edit spurs based on the prior results + link them as needed.
      [...subsets.entries()].forEach(([key, subset]) => {
        for(let i=0; i < subset.insertEdgeKeys.length; i++) {
          const baseTokenization = finalTokenizations.get(i == 0 ? key : subset.insertEdgeKeys[i-1]);
          const tailModule = baseTokenization.tail.searchModule;

          if(tailModule instanceof DeletionQuotientSpur) {
            // No new insertions are allowed immediately after a deletion edge.
            // We also can't extend the disallowed insertion with another;
            // simply break the loop.
            break;
          }

          const insertionSpur = new InsertionQuotientSpur(tailModule);

          let matchedTokenization = finalTokenizations.get(subset.insertEdgeKeys[i]);
          if(matchedTokenization) {
            matchedTokenization.tail.addInboundSpur(insertionSpur);
          } else {
            const tokens = baseTokenization.tokens.slice();
            tokens.pop();
            tokens.push(new ContextToken(insertionSpur));
            matchedTokenization = new ContextTokenization(
              tokens,
              null,
              {insert: '', deleteLeft: 0}
            );
            finalTokenizations.set(subset.insertEdgeKeys[i], matchedTokenization);
          }
        }
      });
    }

    // TODO:  also consider an inserted whitespace (at some sort of low prob)

    return [...finalTokenizations.values()];
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