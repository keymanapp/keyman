import * as models from '@keymanapp/models-templates';
import * as correction from './correction/index.js'

import TransformUtils from './transformUtils.js';
import { correctAndEnumerate, dedupeSuggestions, finalizeSuggestions, predictionAutoSelect, processSimilarity, toAnnotatedSuggestion, tupleDisplayOrderSort } from './predict-helpers.js';
import { detectCurrentCasing, determineModelTokenizer, determineModelWordbreaker, determinePunctuationFromModel } from './model-helpers.js';

export class ModelCompositor {
  private lexicalModel: LexicalModel;
  private contextTracker?: correction.ContextTracker;

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

  constructor(
    lexicalModel: LexicalModel,
    testMode?: boolean
  ) {
    this.lexicalModel = lexicalModel;
    if(lexicalModel.traverseFromRoot) {
      this.contextTracker = new correction.ContextTracker();
    }
    this.punctuation = determinePunctuationFromModel(lexicalModel);
    this.testMode = !!testMode;
  }

  async predict(transformDistribution: Transform | Distribution<Transform>, context: Context): Promise<Suggestion[]> {
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

    // Fulfill pre-condition:  the transform distribution should be sorted in
    // descending order.
    transformDistribution.sort(function(a, b) {
      return b.p - a.p;
    });

    // Only allow new-word suggestions if space was the most likely keypress.
    // const allowSpace = TransformUtils.isWhitespace(inputTransform);
    const inputTransform = transformDistribution[0].sample;
    const allowBksp = TransformUtils.isBackspace(inputTransform);

    const postContext = models.applyTransform(inputTransform, context);
    const truePrefix = this.wordbreak(postContext);
    const basePrefix = allowBksp ? truePrefix : this.wordbreak(context);

    let currentCasing: CasingForm = null;
    if(lexicalModel.languageUsesCasing) {
      currentCasing = detectCurrentCasing(this.lexicalModel, postContext);
    }

    // Section 1:  determine 'prediction roots' - enumerate corrections from most to least likely,
    // searching for results that yield viable predictions from the model.

    const SEARCH_TIMEOUT = correction.SearchSpace.DEFAULT_ALLOTTED_CORRECTION_TIME_INTERVAL;
    const timer = this.activeTimer = new correction.ExecutionTimer(this.testMode ? Number.MAX_VALUE : SEARCH_TIMEOUT, this.testMode ? Number.MAX_VALUE : SEARCH_TIMEOUT * 1.5);

    const { postContextState, rawPredictions } = await correctAndEnumerate(this.contextTracker, this.lexicalModel, timer, transformDistribution, context);

    if(this.activeTimer == timer) {
      this.activeTimer = null;
    }

    // Section 2 - prediction filtering + post-processing pass 1

    // Properly capitalizes the suggestions based on the existing context casing state.
    // This may result in duplicates if multiple casing options exist within the
    // lexicon for a word.  (Example:  "Apple" the company vs "apple" the fruit.)
    for(let tuple of rawPredictions) {
      if(currentCasing && currentCasing != 'lower') {
        this.applySuggestionCasing(tuple.prediction.sample, basePrefix, currentCasing);
      }
    }

    // We want to dedupe before trimming the list so that we can present a full set
    // of viable distinct suggestions if available.
    let deduplicatedSuggestionTuples = dedupeSuggestions(this.lexicalModel, rawPredictions, context);

    // Needs "casing" to be applied first.
    //
    // Will also add a 'keep' suggestion (with `.matchesModel = false`) matching
    // the current state of context if there is no such matching prediction.
    processSimilarity(this.lexicalModel, deduplicatedSuggestionTuples, context, transformDistribution[0]);

    // Section 3:  Sort the suggestions in display priority order to determine
    // which are most optimal, then auto-select based on the results.
    deduplicatedSuggestionTuples = deduplicatedSuggestionTuples.sort(tupleDisplayOrderSort);
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

    // Store the suggestions on the final token of the current context state (if it exists).
    // Or, once phrase-level suggestions are possible, on whichever token serves as each prediction's root.
    if(postContextState) {
      postContextState.tail.replacements = suggestions.map(function(suggestion) {
        return {
          suggestion: suggestion,
          tokenWidth: 1
        }
      });
    }

    return suggestions;
  }

  // Responsible for applying casing rules to suggestions.
  private applySuggestionCasing(suggestion: Suggestion, baseWord: USVString, casingForm: CasingForm) {
    // Step 1:  does the suggestion replace the whole word?  If not, we should extend the suggestion to do so.
    let unchangedLength  = baseWord.kmwLength() - suggestion.transform.deleteLeft;

    if(unchangedLength > 0) {
      suggestion.transform.deleteLeft += unchangedLength;
      suggestion.transform.insert = baseWord.kmwSubstr(0, unchangedLength) + suggestion.transform.insert;
    }

    // Step 2: Now that the transform affects the whole word, we may safely apply casing rules.
    suggestion.transform.insert = this.lexicalModel.applyCasing(casingForm, suggestion.transform.insert);
    suggestion.displayAs = this.lexicalModel.applyCasing(casingForm, suggestion.displayAs);
  }

  acceptSuggestion(suggestion: Suggestion, context: Context, postTransform?: Transform): Reversion {
    // Step 1:  generate and save the reversion's Transform.
    let sourceTransform = suggestion.transform;
    let deletedLeftChars = context.left.kmwSubstr(-sourceTransform.deleteLeft, sourceTransform.deleteLeft);
    let insertedLength = sourceTransform.insert.kmwLength();

    let reversionTransform: Transform = {
      insert: deletedLeftChars,
      deleteLeft: insertedLength
    };

    // Step 2:  building the proper 'displayAs' string for the Reversion
    let postContext = context;
    if(postTransform) {
      // The code above restores the state to the context at the time the `Suggestion` was created.
      // `postTransform` handles any missing context that came later.
      reversionTransform = models.buildMergedTransform(reversionTransform, postTransform);

      // Now that we've built the reversion based upon the Suggestion's original context,
      // we manipulate it in order to get a proper 'displayAs' string.
      postContext = models.applyTransform(postTransform, postContext);
    }

    let revertedPrefix: string;
    let postContextTokenization = this.tokenize(postContext);
    if(postContextTokenization) {
      // Handles display string for reversions triggered by accepting a suggestion mid-token.
      const preCaretToken = postContextTokenization.left[postContextTokenization.left.length - 1];
      revertedPrefix = (preCaretToken && !preCaretToken.isWhitespace) ? preCaretToken.text : '';
      revertedPrefix += postContextTokenization.caretSplitsToken ? postContextTokenization.right[0] : '';
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
    if(this.contextTracker) {
      let contextState = this.contextTracker.newest;
      if(!contextState) {
        contextState = this.contextTracker.analyzeState(this.lexicalModel, context);
      }

      contextState.tail.activeReplacementId = suggestion.id;
      let acceptedContext = models.applyTransform(suggestion.transform, context);
      this.contextTracker.analyzeState(this.lexicalModel, acceptedContext);
    }

    return reversion;
  }

  async applyReversion(reversion: Reversion, context: Context): Promise<Suggestion[]> {
    // If we are unable to track context (because the model does not support LexiconTraversal),
    // we need a "fallback" strategy.
    let compositor = this;
    let fallbackSuggestions = async function() {
      let revertedContext = models.applyTransform(reversion.transform, context);
      const suggestions = await compositor.predict({insert: '', deleteLeft: 0}, revertedContext);
      suggestions.forEach(function(suggestion) {
        // A reversion's transform ID is the additive inverse of its original suggestion;
        // we revert to the state of said original suggestion.
        suggestion.transformId = -reversion.transformId;
      });

      return suggestions;
    }

    if(!this.contextTracker) {
      return fallbackSuggestions();
    }

    // When the context is tracked, we prefer the tracked information.
    let contextMatchFound = false;
    for(let c = this.contextTracker.count - 1; c >= 0; c--) {
      let contextState = this.contextTracker.item(c);

      if(contextState.tail.activeReplacementId == -reversion.id) {
        contextMatchFound = true;
        break;
      }
    }

    if(!contextMatchFound) {
      return fallbackSuggestions();
    }

    // Remove all contexts more recent than the one we're reverting to.
    while(this.contextTracker.newest.tail.activeReplacementId != -reversion.id) {
      this.contextTracker.popNewest();
    }

    this.contextTracker.newest.tail.revert();

    // Will need to be modified a bit if/when phrase-level suggestions are implemented.
    // Those will be tracked on the first token of the phrase, which won't be the tail
    // if they cover multiple tokens.
    let suggestions = this.contextTracker.newest.tail.replacements.map(function(trackedSuggestion) {
      return trackedSuggestion.suggestion;
    });

    suggestions.forEach(function(suggestion) {
      // A reversion's transform ID is the additive inverse of its original suggestion;
      // we revert to the state of said original suggestion.
      suggestion.transformId = -reversion.transformId;
    });
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

  public resetContext(context: Context) {
    // If we're resetting the context, any active prediction requests are no
    // longer valid.
    this.activeTimer?.terminate();
    // Force-resets the context, throwing out any previous fat-finger data, etc.
    // Designed for use when the caret has been directly moved and/or the context sourced from a different control
    // than before.
    if(this.contextTracker) {
      this.contextTracker.clearCache();
      this.contextTracker.analyzeState(this.lexicalModel, context, null);
    }
  }
}

export default ModelCompositor;