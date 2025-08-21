import { applyTransform } from '@keymanapp/models-templates';
import { RewindableCache } from '@keymanapp/web-utils';

import { determineModelTokenizer } from '../model-helpers.js';
import { LexicalModelTypes } from '@keymanapp/common-types';
import Configuration = LexicalModelTypes.Configuration;
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;
import { ContextState } from './context-state.js';
import { ContextTransition } from './context-transition.js';

export class ContextTracker {
  private readonly cache = new RewindableCache<ContextTransition>(5);
  /**
   * Tracks the most recent transition handled by the context-tracking engine.
   */
  latest: ContextTransition;

  /**
   * Notes the current configuration of the model and of the sliding context window.
   */
  configuration: Configuration;

  /**
   * The active lexical model.
   */
  readonly model: LexicalModel;

  /** @internal */
  public unitTestEndPoints = {
    cache: () => this.cache
  };

  constructor(model: LexicalModel, context: Context, transitionId: number, config: Configuration) {
    this.model = model;
    this.configuration = config;
    this.reset(context, transitionId);
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
      for(const id of [...this.cache.keys()]) {
        const priorMatchState = this.cache.get(id);

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

        let result = priorMatchState.final.analyzeTransition(context, transformDistribution);

        if(result?.final) {
          if(priorMatchState.transitionId !== undefined) {
            // Already has a taggedContext.
            this.cache.get(priorMatchState.transitionId);
          }

          if(transitionId !== undefined) {
            // Special case:  if base and final match, we should use the old Transition instance.
            // This is currently used in some unit tests.
            if(result.final.context != result.base.context) {
              this.cache.add(transitionId, result);
            } else {
              this.cache.add(priorMatchState.transitionId, priorMatchState);
              return this.cache.get(priorMatchState.transitionId);
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
    //
    // If we couldn't find a good match, we need to directly apply the final transform first.
    if(transformDistribution) {
      context = applyTransform(transformDistribution[0].sample, context);
    }

    let state = new ContextState(context, model);

    const transition = new ContextTransition(state, transitionId);
    // Hacky, but holds the course for now.  This should only really happen from context resets, which can
    // then use a different path.
    transition.finalize(state, transformDistribution);
    this.cache.add(transitionId, transition);
    return transition;
  }

  reset(context: Context, transitionId: number) {
    this.cache.clear();
    this.latest = new ContextTransition(new ContextState(context, this.model), transitionId)
    this.latest.finalize(this.latest.base, [{
      sample: {
        insert: '',
        deleteLeft: 0,
        id: transitionId
      },
      p: 0
    }]);
  }

  findAndRevert(id: number) {
    const transition = this.cache.get(id);
    if(transition) {
      this.cache.rewindTo(id);
      this.cache.get(id);
    }
    return transition;
  }

  saveLatest() {
    this.cache.add(this.latest.transitionId, this.latest);
  }
}
