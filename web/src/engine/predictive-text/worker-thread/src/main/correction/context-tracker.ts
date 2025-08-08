import { applyTransform, buildMergedTransform } from '@keymanapp/models-templates';
import { RewindableCache } from '@keymanapp/web-utils';

import { determineModelTokenizer } from '../model-helpers.js';
import { tokenizeAndFilterDistribution } from './transform-tokenization.js';
import { LexicalModelTypes } from '@keymanapp/common-types';
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;
import { ContextState } from './context-state.js';
import { ContextTransition } from './context-transition.js';

export class ContextTracker {
  readonly cache = new RewindableCache<number, ContextTransition>(5);

  /**
   * As written, this method attempts to match an incoming context with a candidate
   * prior ContextState analyzed by the context tracking engine.  Upon success,
   * this method will determine the ContextState properties that best leverage
   * reuse of prior correction-search data based upon the prior state and return
   * the overall results as a ContextTransition.
   *
   * If the two versions of Context match poorly, this method will return null.
   *
   * @param context
   * @param lexicalModel
   * @param matchState
   * @param transformDistribution
   * @param isApplyingSuggestion
   * @returns
   */
  static attemptMatchContext(
    // Matching the context expected for the transition's base state
    context: Context,
    lexicalModel: LexicalModel,
    // As currently written, is a POTENTIAL base state
    matchState: ContextState,
    // the distribution should be tokenized already.
    transformDistribution?: Distribution<Transform>, // transform distribution is needed here.
    // overrides checks for token substitution that can fail for large applied suggestions.
    isApplyingSuggestion?: boolean
  ): ContextTransition /* returns null if matchState is NOT a valid base state */ {
    // Apply all transforms to the base context state
    const transformSequenceDistribution = tokenizeAndFilterDistribution(context, lexicalModel, transformDistribution);

    if(transformDistribution?.[0]) {
      context = applyTransform(transformDistribution[0].sample, context);
    }
    // Note for future:  the next line's pattern asserts that there is only one true tokenization.
    // We may eventually allow for multiple potential tokenizations (per epic-dict-breaker)
    const tokenizedContext = determineModelTokenizer(lexicalModel)(context).left;
    // In which case we could try need to align for each of them, starting from the most likely.
    const alignmentResults = matchState.tokenization.computeAlignment(tokenizedContext.map((token) => token.text), isApplyingSuggestion);

    if(!alignmentResults.canAlign) {
      return null;
    }

    const resultTokenization = matchState.tokenization.transitionTo(
      determineModelTokenizer(lexicalModel)(context).left,
      alignmentResults,
      lexicalModel,
      transformSequenceDistribution
    );

    if(!resultTokenization) {
      return null;
    }

    const transition = new ContextTransition(matchState, matchState.appliedInput?.id);
    if(resultTokenization == matchState.tokenization) {
      // If the tokenizations match, clone the ContextState; we want to preserve a post-application
      // context separately from pre-application contexts for predictions based on empty roots.
      const state = new ContextState(matchState);
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

    const state = new ContextState(context, lexicalModel);
    state.tokenization = resultTokenization;
    state.appliedInput = transformDistribution?.[0].sample;
    transition.finalize(state, transformDistribution, preservationTransform);
    return transition;
  }

  /**
   * Compares the current, post-input context against the most recently-seen contexts from previous prediction calls, returning
   * the most information-rich `TrackedContextState` possible.  If a match is found, the state will be annotated with the
   * input information provided to previous prediction calls and persisted correction-search calculations for re-use.
   *
   * @param model
   * @param context
   * @param transformDistribution
   * @param preserveMatchState  Set to `true` to avoid any edits to the matched context state when they might normally occur.
   */
  analyzeState(
    model: LexicalModel,
    context: Context,
    transformDistribution?: Distribution<Transform>,
    preserveMatchState?: boolean
  ): ContextTransition {
    if(!model.traverseFromRoot) {
      // Assumption:  LexicalModel provides a valid traverseFromRoot function.  (Is technically optional)
      // Without it, no 'corrections' may be made; the model can only be used to predict, not correct.
      throw "This lexical model does not provide adequate data for correction algorithms and context reuse";
    }

    if(transformDistribution?.length == 0) {
      transformDistribution = null;
    }

    const inputTransform = transformDistribution?.[0];
    const postContext = inputTransform ? applyTransform(inputTransform.sample, context) : context;

    const tokenize = determineModelTokenizer(model);
    const tokenizedPostContext = tokenize(postContext);
    const transitionId = inputTransform?.sample.id;

    if(tokenizedPostContext.left.length > 0) {
      for(const id of this.cache.keys()) {
        const priorMatchState = this.cache.peek(id);

        // Skip intermediate multitap-produced contexts.
        // When multitapping, we skip all contexts from prior taps within the same interaction,
        // but not any contexts from before the multitap started.
        const priorTaggedContext = priorMatchState.final.context;
        if(priorTaggedContext && transformDistribution && transformDistribution.length > 0) {
          // Using the potential `matchState` + the incoming transform, do the results line up for
          // our observed context?  If not, skip it.
          //
          // Necessary to properly handle multitaps, as there are context rewinds that the
          // predictive-text engine is not otherwise warned about.
          //
          // `priorTaggedContext` must not be `null`!
          const doublecheckContext = applyTransform(transformDistribution[0].sample, priorTaggedContext);
          if(doublecheckContext.left != postContext.left) {
            continue;
          }
        } else if(priorTaggedContext?.left != postContext.left) {
          continue;
        }

        let result = ContextTracker.attemptMatchContext(context, model, priorMatchState.final, transformDistribution);

        if(result?.final) {
          if(priorMatchState.transitionId !== undefined) {
            // Already has a taggedContext.
            this.cache.get(priorMatchState.transitionId);
          }

          if(transitionId !== undefined) {
            // Special case:  if base and final match, we should use the old Transition instance.
            // This is currently used in some unit tests.
            if(result.final != result.base) {
              this.cache.add(transitionId, result);
            } else {
              return this.cache.peek(priorMatchState.transitionId);
            }
          }

          return result;
        }
      }
    }

    // Else:  either empty OR we've detected a 'new context'.  Initialize from scratch; no prior input information is
    // available.  Only the results of the prior inputs are known.
    //
    // Assumption:  as a caret needs to move to context before any actual transform distributions occur,
    // this state is only reached on caret moves; thus, transformDistribution is actually just a single null transform.
    let state = new ContextState(context, model);
    const transition = new ContextTransition(state, transitionId);
    // Hacky, but holds the course for now.  This should only really happen from context resets, which can
    // then use a different path.
    transition.finalize(state, transformDistribution);
    this.cache.add(transitionId, transition);
    return transition;
  }

  get newest() {
    let key = this.cache.keys()[0];
    if(key === undefined) {
      return undefined;
    } else {
      return this.cache.peek(key);
    }
  }

  clearCache() {
    this.cache.clear();
  }
}
