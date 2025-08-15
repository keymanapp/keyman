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
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Suggestion = LexicalModelTypes.Suggestion;
import Transform = LexicalModelTypes.Transform;
import { ContextToken } from './context-token.js';
import { ContextTokenization } from './context-tokenization.js';
import { ContextTransition } from './context-transition.js';
import { determineModelTokenizer } from '#./model-helpers.js';
import { tokenizeAndFilterDistribution } from './transform-tokenization.js';
import { applyTransform, buildMergedTransform } from '@keymanapp/models-templates';

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

    return this.suggestions[this.appliedSuggestionId]?.transformId;
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
    transformDistribution?: Distribution<Transform>,
    // overrides checks for token substitution that can fail for large applied suggestions.
    isApplyingSuggestion?: boolean
  ): ContextTransition {
    const lexicalModel = this.model;

    // Apply all transforms to the base context state
    const transformSequenceDistribution = tokenizeAndFilterDistribution(context, lexicalModel, transformDistribution);
    const postContext = transformDistribution?.[0] ? applyTransform(transformDistribution[0].sample, context) : context;

    // Note for future:  the next line's pattern asserts that there is only one true tokenization.
    // We may eventually allow for multiple potential tokenizations (per epic-dict-breaker)
    const tokenizedContext = determineModelTokenizer(lexicalModel)(postContext).left;
    if(tokenizedContext.length == 0) {
      tokenizedContext.push({text: ''});
    }
    // In which case we could try need to align for each of them, starting from the most likely.

    // If we're not at the start of the buffer, we're probably a sliding context.
    const isSliding = !this.context.startOfBuffer;

    // It's possible the tokenization will remember more of the initial token than is
    // actually present in the sliding context window, which imposes a need for a wide-band
    // computeDistance 'radius' in the called function.
    const alignmentResults = this.tokenization.computeAlignment(tokenizedContext.map((token) => token.text), isSliding, isApplyingSuggestion);

    if(!alignmentResults.canAlign) {
      return null;
    }

    const resultTokenization = this.tokenization.transitionTo(
      tokenizedContext,
      alignmentResults,
      lexicalModel,
      transformSequenceDistribution
    );

    if(!resultTokenization) {
      return null;
    }

    const transition = new ContextTransition(this, this.appliedInput?.id);
    if(resultTokenization == this.tokenization) {
      // If the tokenizations match, clone the ContextState; we want to preserve a post-application
      // context separately from pre-application contexts for predictions based on empty roots.
      const state = new ContextState(this);
      transition.finalize(state, transformDistribution);
      return transition;
    }

    // Used to construct and represent the part of the incoming transform that
    // does not land as part of the final token in the resulting context.  This
    // component should be preserved by any suggestions that get applied.
    let preservationTransform: Transform;

    // Handling for non-whitespace word boundaries - for example,
    // `the '` => `the 'a` - a fun word boundary shift!
    // We expect such cases to have SOMETHING for a preservation transform here;
    // we need to ensure that any suggestions for the new token believe that
    // the token is starting fresh, without any prior text.
    if(alignmentResults.tailTokenShift > 0) {
      preservationTransform = { insert: '', deleteLeft: 0 };
    }

    // Leave out the final entry!
    for(let i = 0; i < transformSequenceDistribution?.[0].sample.length - 1; i++) {
      const primaryInput = transformSequenceDistribution[0].sample[i];
      preservationTransform = preservationTransform ? buildMergedTransform(preservationTransform, primaryInput): primaryInput;
    }

    const state = new ContextState(postContext, lexicalModel);
    state.tokenization = resultTokenization;
    state.appliedInput = transformDistribution?.[0].sample;
    transition.finalize(state, transformDistribution, preservationTransform);
    return transition;
  }
}