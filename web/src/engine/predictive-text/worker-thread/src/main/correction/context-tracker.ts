import { applyTransform, buildMergedTransform } from '@keymanapp/models-templates';
import { RewindableCache } from '@keymanapp/web-utils';

import TransformUtils from '../transformUtils.js';
import { determineModelTokenizer } from '../model-helpers.js';
import { tokenizeAndFilterDistribution } from './transform-tokenization.js';
import { LexicalModelTypes } from '@keymanapp/common-types';
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;
import { ContextToken } from './context-token.js';
import { ContextTokenization } from './context-tokenization.js';
import { ContextState } from './context-state.js';
import { ContextTransition } from './context-transition.js';

export class ContextTracker {
  readonly cache = new RewindableCache<ContextTransition>(5);

  // Aim:  relocate to ContextTransition in some form?
  // Or can we split it up in some manner across the different types?
  static attemptMatchContext(
    context: Context,
    lexicalModel: LexicalModel,
    matchState: ContextState,
    // the distribution should be tokenized already.
    transformDistribution?: Distribution<Transform> // transform distribution is needed here.
  ): ContextTransition {
    const baseTransition = new ContextTransition(matchState, matchState.appliedInput?.id);
    const transformSequenceDistribution = tokenizeAndFilterDistribution(context, lexicalModel, transformDistribution);

    if(transformDistribution?.[0]) {
      context = applyTransform(transformDistribution[0].sample, context);
    }
    const tokenizedContext = determineModelTokenizer(lexicalModel)(context).left;
    const alignmentResults = matchState.tokenization.computeAlignment(tokenizedContext.map((token) => token.text));

    if(!alignmentResults.canAlign) {
      return null;
    }

    const {
      leadTokenShift,
      matchLength,
      tailEditLength,
      tailTokenShift
    } = alignmentResults;

    const hasDistribution = transformSequenceDistribution && Array.isArray(transformSequenceDistribution);

    // If we have a perfect match with a pre-existing context, no mutations have
    // happened; just re-use the old context state.
    if(tailEditLength == 0 && leadTokenShift == 0 && tailTokenShift == 0) {
      // Set the 'final' state to match 'base' to signal an intent to reuse the old instance.
      // TODO:  Fix - this is intended to be temporary during refactor work.
      baseTransition.finalize(matchState, transformDistribution);
      return baseTransition;
    } else {
      // If we didn't get any input, we really should perfectly match
      // a previous context state.  If such a state is out of our cache,
      // it should simply be rebuilt.
      if(!hasDistribution) {
        return null;
      }
    }

    // If mutations HAVE happened, we have work to do.
    const tokenization = matchState.tokenization.tokens.map((token) => new ContextToken(token));

    if(leadTokenShift < 0) {
      tokenization.splice(0, -leadTokenShift);
    } else if(leadTokenShift > 0) {
      // TODO:  insert token(s) at the start to match the text that's back within the
      // sliding context window.
      //
      // (was not part of original `attemptContextMatch`)
      return null;
    }

    // If no TAIL mutations have happened, we're safe to return now.
    if(tailEditLength == 0 && tailTokenShift == 0) {
      const state = new ContextState(context, lexicalModel);
      state.tokenization = new ContextTokenization(tokenization, alignmentResults);
      baseTransition.finalize(state, transformDistribution);
      return baseTransition;
    }

    // ***

    // first non-matched tail index within the incoming context
    const incomingTailUpdateIndex = matchLength + (leadTokenShift > 0 ? leadTokenShift : 0);
    // first non-matched tail index in `matchState`, the base context state.
    const matchingTailUpdateIndex = matchLength - (leadTokenShift < 0 ? leadTokenShift : 0);

    // The assumed input from the input distribution is always at index 0.
    const tokenizedPrimaryInput = hasDistribution ? transformSequenceDistribution[0].sample : null;
    // first index:  original sample's tokenization
    // second index:  token index within original sample
    const tokenDistribution = transformSequenceDistribution.map((entry) => {
      return entry.sample.map((sample) => {
        return {
          sample: sample,
          p: entry.p
        }
      });
    });

    // // Gets distribution of token index 1s as excerpted from the sequences' distribution.
    // let a = tokenDistribution.map((sequence) => sequence[1]);

    // Using these as base indices...

    let tailIndex = 0;
    // let lastTailIndex = tailEditLength + (tailTokenShift > 0 ? tailTokenShift : 0);

    // Used to construct and represent the part of the incoming transform that
    // does not land as part of the final token in the resulting context.  This
    // component should be preserved by any suggestions that get applied.
    let preservationTransform: Transform;

    for(let i = 0; i < tailEditLength; i++) {
      // do tail edits
      const incomingIndex = i + incomingTailUpdateIndex;
      const matchingIndex = i + matchingTailUpdateIndex;

      const incomingToken = tokenizedContext[incomingIndex];
      const matchedToken = matchState.tokenization.tokens[matchingIndex];

      let primaryInput = hasDistribution ? tokenizedPrimaryInput[i] : null;
      const isBackspace = primaryInput && TransformUtils.isBackspace(primaryInput);

      const isLastToken = incomingIndex == tokenizedContext.length - 1;

      if(isLastToken) {
        // If this token's transform component is not part of the final token,
        // it's something we'll want to preserve even when applying suggestions
        // for the final token.
        //
        // Note:  will need a either a different approach or more specialized
        // handling if/when supporting phrase-level (multi-token) suggestions.
      } else {
        preservationTransform = preservationTransform && primaryInput ? buildMergedTransform(preservationTransform, primaryInput) : (primaryInput ?? preservationTransform);
      }
      let token: ContextToken;

      if(isBackspace) {
        token = new ContextToken(lexicalModel, incomingToken.text);
        token.searchSpace.inputSequence.forEach((entry) => entry[0].sample.id = primaryInput.id);
      } else {
        // Assumption:  there have been no intervening keystrokes since the last well-aligned context.
        // (May not be valid with epic/dict-breaker or with complex, word-boundary crossing transforms)
        token = new ContextToken(matchedToken);
        token.searchSpace.addInput(tokenDistribution.map((seq) => seq[tailIndex]));
      }

      tokenization[incomingIndex] = token;
      tailIndex++;
    }

    if(tailTokenShift < 0) {
      // delete tail tokens
      for(let i = 0; i > tailTokenShift; i--) {
        // If ALL that remains are deletes, we're good to go.
        //
        // This may not be the token at the index, but since all that remains are deletes,
        // we'll have deleted the correct total number from the end once all iterations
        // are done.
        tokenization.pop();
      }
    } else {
      for(let i = tailEditLength; i < tailEditLength + tailTokenShift; i++) {
        // create tail tokens
        const incomingIndex = i + incomingTailUpdateIndex;
        const incomingToken = tokenizedContext[incomingIndex];
        // // Assertion:  there should be no matching token; this should be a newly-appended token.
        // const matchingIndex = i + tailEditLength + matchingTailUpdateIndex;

        const primaryInput = hasDistribution ? tokenizedPrimaryInput[i] : null;

        if(!preservationTransform) {
          // Allows for consistent handling of "insert" cases; even if there's no edit
          // from a prior token, having a defined transform here indicates that
          // a new token has been produced.  This serves as a useful conditional flag
          // for prediction logic.
          preservationTransform = { insert: '', deleteLeft: 0 };
        }

        const isLastToken = incomingIndex == tokenizedContext.length - 1;
        if(!isLastToken) {
          preservationTransform = preservationTransform && primaryInput ? buildMergedTransform(preservationTransform, primaryInput) : (primaryInput ?? preservationTransform);
        }

        let pushedToken = new ContextToken(lexicalModel);

        // TODO:  assumes that there was no shift in wordbreaking from the
        // prior context to the current one.  This may actually be a major
        // issue for dictionary-based wordbreaking!
        //
        // If there was such a shift, then we may have extra transforms
        // originally on a 'previous' token that got moved into this one!
        //
        // Suppose we're using a dictionary-based wordbreaker and have
        // `butterfl` for our context, which could become butterfly.  If the
        // next keystroke results in `butterfli`, this would likely be
        // tokenized `butter` `fli`.  (e.g: `fli` leads to `flight`.) How do
        // we know to properly relocate the `f` and `l` transforms?
        let tokenDistribComponent = tokenDistribution.map((seq) => {
          const entry = seq[tailIndex];
          if(!entry || TransformUtils.isEmpty(entry.sample)) {
            return null;
          } else {
            return entry;
          }
        }).filter((entry) => !!entry);
        if(primaryInput) {
          let transformDistribution = tokenDistribComponent.length > 0 ? tokenDistribComponent : null;
          if(transformDistribution) {
            pushedToken.searchSpace.addInput(transformDistribution);
          }
        } else if(incomingToken.text) {
          // We have no transform data to match against an inserted token with text; abort!
          // Refer to #12494 for an example case; we currently can't map previously-committed
          // input transforms to a newly split-off token.
          return null;
        }
        pushedToken.isWhitespace = incomingToken.isWhitespace;

        // Auto-replaces the search space to correspond with the new token.
        tokenization.push(pushedToken);

        tailIndex++;
      }
    }

    const state = new ContextState(context, lexicalModel);
    state.tokenization = new ContextTokenization(tokenization, alignmentResults);
    state.appliedInput = transformDistribution?.[0].sample;
    baseTransition.finalize(state, transformDistribution, preservationTransform);
    return baseTransition;
  }

  // Aim:  relocate to ContextState in some form... or ContextTransition?
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

        let result = ContextTracker.attemptMatchContext(context, model, priorMatchState.final, transformDistribution);

        if(result?.final) {
          if(transitionId !== undefined) {
            if(priorMatchState.transitionId != transitionId) {
              // Already has a taggedContext.
              this.cache.add(priorMatchState.transitionId, priorMatchState);
            }
            this.cache.add(transitionId, result);
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
      return this.cache.get(key);
    }
  }

  clearCache() {
    this.cache.clear();
  }
}
