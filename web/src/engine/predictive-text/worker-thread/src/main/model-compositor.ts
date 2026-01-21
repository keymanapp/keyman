import * as models from '@keymanapp/models-templates';
import { LexicalModelTypes } from '@keymanapp/common-types';

import * as correction from './correction/index.js'
import TransformUtils from './transformUtils.js';
import { applySuggestionCasing, correctAndEnumerate, dedupeSuggestions, finalizeSuggestions, predictionAutoSelect, processSimilarity, toAnnotatedSuggestion, tupleDisplayOrderSort } from './predict-helpers.js';
import { detectCurrentCasing, determineModelTokenizer, determineModelWordbreaker, determinePunctuationFromModel } from './model-helpers.js';

import { ContextTracker } from './correction/context-tracker.js';
import { SearchQuotientSpur } from './correction/search-quotient-spur.js';

import CasingForm = LexicalModelTypes.CasingForm;
import Configuration = LexicalModelTypes.Configuration;
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import Keep = LexicalModelTypes.Keep;
import LexicalModel = LexicalModelTypes.LexicalModel;
import LexicalModelPunctuation = LexicalModelTypes.LexicalModelPunctuation;
import Outcome = LexicalModelTypes.Outcome;
import Reversion = LexicalModelTypes.Reversion;
import Suggestion = LexicalModelTypes.Suggestion;
import Transform = LexicalModelTypes.Transform;

export class ModelCompositor {
  private lexicalModel: LexicalModel;
  private _contextTracker?: correction.ContextTracker;

  /**
   * Returns the context-caching store used to store intermediate
   * correction-search and context-tokenization calculations.
   */
  public get contextTracker() {
    return this._contextTracker;
  }

  static readonly MAX_SUGGESTIONS = 12;
  readonly punctuation: LexicalModelPunctuation;

  // Left exposed to facilitate unit tests.
  // See worker-model-compositor, "stops predicting early[...]".
  /**
   * If there is a prediction request currently beign processed, this will
   * hold a reference to its time-management ExecutionTimer, which can
   * be used to have it exit early (once it yields to the task queue).
   */
  activeTimer?: correction.ExecutionTimer;

  /**
   * Controls the strength of anti-corrective measures for single-character scenarios.
   * The base key probability will be raised to this power for this specific case.
   *
   * Current selection's motivation:  (0.5 / 0.4) ^ 16 ~= 35.5.
   * - if the most likely has p=0.5 and second-most has p=0.4 - a highly-inaccurate key
   *   stroke - the net effect will apply a factor of 35.5 to the lexical probability of
   *   the best key's prediction roots, favoring it in this manner.
   * - less extreme edge cases will have a significantly stronger factor, acting as a
   *   "soft threshold".
   * - truly ambiguous, "coin flip" cases will have a lower factor and thus favor the
   *   more likely words from the pair.
   *   - Our OSK key-element borders aren't visible to the user, so the 'spot' where
   *     behavior changes might feel arbitrary to users if we used a hard threshold instead.
   */
  static readonly SINGLE_CHAR_KEY_PROB_EXPONENT = 16;

  private SUGGESTION_ID_SEED = 0;

  private testMode: boolean = false;
  private verbose: boolean = true;
  private configuration: Configuration;

  constructor(
    lexicalModel: LexicalModel,
    testMode?: boolean
  ) {
    this.lexicalModel = lexicalModel;
    this.punctuation = determinePunctuationFromModel(lexicalModel);
    this.testMode = !!testMode;
  }

  setConfiguration(config: Configuration) {
    this.configuration = config;
  }

  initContextTracker(context: Context, transformId: number) {
    if(this.contextTracker || !this.lexicalModel.traverseFromRoot) {
      return;
    }

    this._contextTracker = new ContextTracker(this.lexicalModel, context, transformId, this.configuration);
  }

  async predict(transformDistribution: Transform | Distribution<Transform>, context: Context): Promise<Outcome<Suggestion|Keep>[]> {
    const lexicalModel = this.lexicalModel;

    // If a prior prediction is still processing, signal to terminate it; we have a new
    // prediction request to prioritize.
    this.activeTimer?.terminate();

    if(!(transformDistribution instanceof Array)) {
      transformDistribution = [ {sample: transformDistribution, p: 1.0} ];
    } else if(transformDistribution.length == 0) {
      /*
         Robust stop-gap: if our other filters somehow fail, this fixes the
         zero-length array by making it match the form of the array that
         would result if it were instead called with the other legal
         parameter type - a single Transform.

         Unfortunately, the method will lack all data about even
         the original keystroke that resulted in the call... but this way,
         we can at least get some predictions rather than shortcutting
         and producing none whatsoever.
      */
      transformDistribution.push({
        sample: {
          insert: '',
          deleteLeft: 0
        },
        p: 1.0
      })
    }

    // Only allow new-word suggestions if space was the most likely keypress.
    // const allowSpace = TransformUtils.isWhitespace(inputTransform);
    const inputTransform = transformDistribution[0].sample;
    const transformId = inputTransform.id;
    this.initContextTracker(context, transformId);

    const allowBksp = TransformUtils.isBackspace(inputTransform);
    const allowWhitespace = TransformUtils.isWhitespace(inputTransform);

    const postContext = models.applyTransform(inputTransform, context);

    // TODO:  It would be best for the correctAndEnumerate method to return the
    // suggestion's prefix, as it already has lots of logic oriented to this.
    // The context-tracker used there with v14+ models can determine this more
    // robustly.
    const truePrefix = this.wordbreak(postContext);
    // Only use of `truePrefix`.
    const basePrefix = (allowBksp || allowWhitespace) ? truePrefix : this.wordbreak(context);

    // Used to restore whitespaces if operations would remove them.
    const currentCasing: CasingForm = lexicalModel.languageUsesCasing
      ? detectCurrentCasing(lexicalModel, postContext)
      : null;

    // Section 1:  determine 'prediction roots' - enumerate corrections from most to least likely,
    // searching for results that yield viable predictions from the model.

    const SEARCH_TIMEOUT = SearchQuotientSpur.DEFAULT_ALLOTTED_CORRECTION_TIME_INTERVAL;
    const timer = this.activeTimer = new correction.ExecutionTimer(this.testMode ? Number.MAX_VALUE : SEARCH_TIMEOUT, this.testMode ? Number.MAX_VALUE : SEARCH_TIMEOUT * 1.5);

    const { postContextState, rawPredictions, revertableTransitionId } = await correctAndEnumerate(this.contextTracker, this.lexicalModel, timer, transformDistribution, context);

    if(this.activeTimer == timer) {
      this.activeTimer = null;
    }

    // Section 2 - prediction filtering + post-processing pass 1

    // Properly capitalizes the suggestions based on the existing context casing state.
    // This may result in duplicates if multiple casing options exist within the
    // lexicon for a word.  (Example:  "Apple" the company vs "apple" the fruit.)
    for(let tuple of rawPredictions) {
      if(currentCasing && currentCasing != 'lower') {
        applySuggestionCasing(tuple.prediction.sample, basePrefix, this.lexicalModel, currentCasing);
      }
    }

    // We want to dedupe before trimming the list so that we can present a full set
    // of viable distinct suggestions if available.
    const deduplicatedSuggestionTuples = dedupeSuggestions(this.lexicalModel, rawPredictions, context);

    // Needs "casing" to be applied first.
    //
    // Will also add a 'keep' suggestion (with `.matchesModel = false`) matching
    // the current state of context if there is no such matching prediction.
    processSimilarity(this.lexicalModel, deduplicatedSuggestionTuples, context, transformDistribution[0]);

    // Section 3:  Sort the suggestions in display priority order to determine
    // which are most optimal, then auto-select based on the results.
    deduplicatedSuggestionTuples.sort(tupleDisplayOrderSort);
    predictionAutoSelect(deduplicatedSuggestionTuples);

    // Section 4: Trim down the suggestion list to the N (MAX_SUGGESTIONS) most optimal,
    // then cache and return the set of suggestions

    // Now that we've marked the suggestion to auto-select, we can finalize the suggestions.
    const suggestions = finalizeSuggestions(
      this.lexicalModel,
      deduplicatedSuggestionTuples.splice(0, ModelCompositor.MAX_SUGGESTIONS),
      context,
      inputTransform,
      this.verbose
    );

    suggestions.forEach((suggestion) => {
      suggestion.id = this.SUGGESTION_ID_SEED;
      this.SUGGESTION_ID_SEED++;
    });

    if(revertableTransitionId) {
      const reversion = this.contextTracker.peek(revertableTransitionId)?.reversion;
      if(reversion) {
        if(suggestions[0]?.tag == 'keep') {
          const keep = suggestions.shift();
          suggestions.unshift(reversion);
          suggestions.unshift(keep);
        } else {
          suggestions.unshift(reversion)
        }
      }
    }

    // Store the suggestions on the final token of the current context state (if it exists).
    // Or, once phrase-level suggestions are possible, on whichever token serves as each prediction's root.
    if(postContextState) {
      postContextState.suggestions = suggestions;
    }

    return suggestions;
  }

  /**
   *
   * @param suggestion Suggestion selected, whether automatically or by the user.
   * @param context The context to which the suggestion should be applied.
   * @param originalInput The original transform that is being replaced by the applied suggestion (!)
   * @returns
   */
  acceptSuggestion(suggestion: Suggestion, context: Context, originalInput?: Transform): Reversion {
    // Step 1:  re-use the original input Transform as the reversion's Transform.
    // The Web engine will restore the original state of the context before accepting
    // and before reverting; all we need to do is put the original keystroke back in place.
    let reversionTransform: Transform = originalInput ?? { insert: '', deleteLeft: 0 };

    // Step 2:  building the proper 'displayAs' string for the Reversion
    const postContext = originalInput ? models.applyTransform(originalInput, context) : context;

    let revertedPrefix: string;
    let postContextTokenization = this.tokenize(postContext);
    if(postContextTokenization) {
      // Handles display string for reversions triggered by accepting a suggestion mid-token.
      // In case we ever actually put that back in place.
      const preCaretToken = postContextTokenization.left[postContextTokenization.left.length - 1];
      revertedPrefix = (preCaretToken && !preCaretToken.isWhitespace) ? preCaretToken.text : '';
      revertedPrefix += postContextTokenization.caretSplitsToken ? postContextTokenization.right[0].text : '';
    } else {
      revertedPrefix = this.wordbreak(postContext);
    }

    let firstConversion = models.transformToSuggestion(reversionTransform);
    firstConversion.displayAs = revertedPrefix;

    // Build the actual Reversion, which is technically an annotated Suggestion.
    // Since we're outside of the standard `predict` control path, we'll need to
    // set the Reversion's ID directly.
    let reversion = toAnnotatedSuggestion(this.lexicalModel, firstConversion, 'revert');
    if(suggestion.transformId != null) {
      reversion.transformId = -suggestion.transformId;
    }
    if(suggestion.id != null) {
      // Since a reversion inverts its source suggestion, we set its ID to be the
      // additive inverse of the source suggestion's ID.  Makes easy mapping /
      // verification later.
      reversion.id = -suggestion.id;
    } else {
      reversion.id = -this.SUGGESTION_ID_SEED;
      this.SUGGESTION_ID_SEED++;
    }

    // Step 3:  if we track Contexts, update the tracking data as appropriate.
    if(!this.contextTracker) {
      if(suggestion.appendedTransform) {
        reversion.appendedTransform = {
          insert: '',
          deleteLeft: suggestion.appendedTransform.insert.length,
          id: suggestion.appendedTransform.id
        }
      }
    } else {
      let originalTransition = this.contextTracker.latest;
      if(originalTransition.transitionId != suggestion.transformId) {
        originalTransition = this.contextTracker.findAndRevert(suggestion.transformId);
      }
      if(!originalTransition) {
        this.contextTracker.reset(context, suggestion.transformId);
        originalTransition = this.contextTracker.latest;
      }

      const transitions = originalTransition.applySuggestion(suggestion);
      this.contextTracker.latest = transitions.base;
      this.contextTracker.saveLatest();
      if(transitions.appended) {
        this.contextTracker.latest = transitions.appended;
        this.contextTracker.saveLatest();
        reversion.appendedTransform = {
          insert: '',
          deleteLeft: suggestion.appendedTransform.insert.length,
          id: suggestion.appendedTransform.id
        }
      }

      // Ensure the reversion is annotated on the base, suggested-altered transition that
      // is cached!
      transitions.base.reversion = reversion;
    }

    return reversion;
  }

  /**
   * Reverts a recently applied suggestion.
   * @param reversion The reversion to be applied
   * @param context The original context when the reverted suggestion was applied, including
   * the original keystroke's effects.
   * @returns
   */
  async applyReversion(reversion: Reversion, context: Context, appendedOnly?: boolean): Promise<Suggestion[]> {
    // If we are unable to track context (because the model does not support LexiconTraversal),
    // we need a "fallback" strategy.
    let compositor = this;
    let suggestions: Promise<Suggestion[]>;
    let fallbackSuggestions = async function() {
      const suggestions = await compositor.predict({insert: '', deleteLeft: 0}, context);
      suggestions.forEach(function(suggestion) {
        // A reversion's transform ID is the additive inverse of its original suggestion;
        // we revert to the state of said original suggestion.
        suggestion.transformId = -reversion.transformId;
        // Prevent auto-selection of any suggestion immediately after a reversion.
        // It's fine after at least one keystroke, but not before.
        suggestion.autoAccept = false;
      });

      return suggestions;
    }

    if(!this.contextTracker) {
      return fallbackSuggestions();
    }

    // When the context is tracked, we prefer the tracked information.
    // Note that the base reversion's .transformId will predate the appendedTransform id
    // used to add whitespace (if one existed), so reverting to the base ID's associated
    // context also reverts the appendedTransform.
    let originalTransition = this.contextTracker.findAndRevert(-reversion.transformId);

    if(appendedOnly) {
      this.contextTracker.latest = originalTransition;
      return Promise.resolve([]);
    }

    if(!originalTransition) {
      this.contextTracker.reset(context, -reversion.transformId);
      originalTransition = this.contextTracker.latest;

      suggestions = fallbackSuggestions();
    } else {
      if(suggestions) {
        suggestions = Promise.resolve([]);
      } else {
        // Will need to be modified a bit if/when phrase-level suggestions are implemented.
        // Those will be tracked on the first token of the phrase, which won't be the tail
        // if they cover multiple tokens.
        let suggests = originalTransition.final.suggestions;

        suggests.forEach(function(suggestion) {
          // No need to muck around with the original associated transition ids here.
          // But, we should clear any auto-acceptance flags.
          suggestion.autoAccept = false;
        });

        suggestions = Promise.resolve(suggests);
      }
    }

    // An applied reversion should replace the original Transition's effects.
    const revertedTransition = originalTransition.reproduceOriginal();
    this.contextTracker.latest = revertedTransition;
    this.contextTracker.saveLatest();

    // In case we needed the fallback strategy.
    revertedTransition.final.suggestions = await suggestions;
    return suggestions;
  }

  private wordbreak(context: Context): string {
    const breaker = determineModelWordbreaker(this.lexicalModel);
    return breaker(context);
  }

  private tokenize(context: Context) {
    const tokenizer = determineModelTokenizer(this.lexicalModel);
    return tokenizer(context);
  }

  public resetContext(context: Context, stateId: number) {
    // If we're resetting the context, any active prediction requests are no
    // longer valid.
    this.activeTimer?.terminate();
    // Force-resets the context, throwing out any previous fat-finger data, etc.
    // Designed for use when the caret has been directly moved and/or the context sourced from a different control
    // than before.
    if(this.contextTracker) {
      this.contextTracker.reset(context, stateId);
    }
  }
}

export default ModelCompositor;