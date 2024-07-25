import * as models from '@keymanapp/models-templates';
import * as wordBreakers from '@keymanapp/models-wordbreakers';
import * as correction from './correction/index.js'

import TransformUtils from './transformUtils.js';

enum SuggestionSimilarity {
  none = 0,
  sameKey = 1,
  sameText = 2,
  exact = 3
}

type CorrectionPredictionTuple = {
  prediction: ProbabilityMass<Suggestion>,
  correction: ProbabilityMass<string>,
  totalProb: number;
  matchLevel: SuggestionSimilarity;
};

function tupleDisplayOrderSort(a: CorrectionPredictionTuple, b: CorrectionPredictionTuple) {
  // Similarity distance
  const simDist = b.matchLevel - a.matchLevel;
  if(simDist != 0) {
    return simDist;
  }

  // Probability distance
  return b.totalProb - a.totalProb;
}

export default class ModelCompositor {
  private lexicalModel: LexicalModel;
  private contextTracker?: correction.ContextTracker;

  private static readonly MAX_SUGGESTIONS = 12;
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
  private static readonly SINGLE_CHAR_KEY_PROB_EXPONENT = 16;

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
    this.punctuation = ModelCompositor.determinePunctuationFromModel(lexicalModel);
    this.testMode = !!testMode;
  }

  private predictFromCorrections(corrections: ProbabilityMass<Transform>[], context: Context): CorrectionPredictionTuple[] {
    let returnedPredictions: CorrectionPredictionTuple[] = [];

    for(let correction of corrections) {
      let predictions = this.lexicalModel.predict(correction.sample, context);

      const { sample: correctionTransform, p: correctionProb } = correction;
      const correctionRoot = this.wordbreak(models.applyTransform(correction.sample, context));

      let predictionSet = predictions.map((pair: ProbabilityMass<Suggestion>) => {
        // Let's not rely on the model to copy transform IDs.
        // Only bother is there IS an ID to copy.
        if(correctionTransform.id !== undefined) {
          pair.sample.transformId = correctionTransform.id;
        }

        let tuple: CorrectionPredictionTuple = {
          prediction: pair,
          correction: {
            sample: correctionRoot,
            p: correctionProb
          },
          totalProb: pair.p * correctionProb,
          matchLevel: SuggestionSimilarity.none
        };
        return tuple;
      }, this);

      returnedPredictions = returnedPredictions.concat(predictionSet);
    }

    return returnedPredictions;
  }

  async predict(transformDistribution: Transform | Distribution<Transform>, context: Context): Promise<Suggestion[]> {
    let lexicalModel = this.lexicalModel;

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

    const { sample: inputTransform, p: inputTransformProb } = transformDistribution.sort(function(a, b) {
      return b.p - a.p;
    })[0];

    // Only allow new-word suggestions if space was the most likely keypress.
    let allowSpace = TransformUtils.isWhitespace(inputTransform);
    let allowBksp = TransformUtils.isBackspace(inputTransform);

    let postContext = models.applyTransform(inputTransform, context);
    const truePrefix = this.wordbreak(postContext);
    const basePrefix = allowBksp ? truePrefix : this.wordbreak(context);
    let keepOption: Outcome<Keep> = null;

    let rawPredictions: CorrectionPredictionTuple[] = [];

    // Used to restore whitespaces if operations would remove them.
    let postContextState: correction.TrackedContextState = null;

    const currentCasing: CasingForm = lexicalModel.languageUsesCasing
      ? this.detectCurrentCasing(postContext)
      : null;

    // Section 1:  determining 'prediction roots'.
    if(!this.contextTracker) {
      let predictionRoots: ProbabilityMass<Transform>[];

      // Generates raw prediction distributions for each valid input.  Can only 'correct'
      // against the final input.
      //
      // This is the old, 12.0-13.0 'correction' style.
      if(allowSpace) {
        // Detect start of new word; prevent whitespace loss here.
        predictionRoots = [{sample: inputTransform, p: 1.0}];
      } else {
        predictionRoots = transformDistribution.map((alt) => {
          let transform = alt.sample;

          // Filter out special keys unless they're expected.
          if(TransformUtils.isWhitespace(transform) && !allowSpace) {
            return null;
          } else if(TransformUtils.isBackspace(transform) && !allowBksp) {
            return null;
          }

          return alt;
        });
      }

      // Remove `null` entries.
      predictionRoots = predictionRoots.filter(tuple => !!tuple);

      // Running in bulk over all suggestions, duplicate entries may be possible.
      rawPredictions = this.predictFromCorrections(predictionRoots, context);
    } else {
      // Token replacement benefits greatly from knowledge of the prior context state.
      let contextState = this.contextTracker.analyzeState(this.lexicalModel, context, null);
      // Corrections and predictions are based upon the post-context state, though.
      postContextState = this.contextTracker.analyzeState(this.lexicalModel,
                                                          postContext,
                                                          !TransformUtils.isEmpty(inputTransform) ?
                                                                          transformDistribution:
                                                                          null
                                                          );

      // TODO:  Should we filter backspaces & whitespaces out of the transform distribution?
      //        Ideally, the answer (in the future) will be no, but leaving it in right now may pose an issue.

      // Rather than go "full hog" and make a priority queue out of the eventual, future competing search spaces...
      // let's just note that right now, there will only ever be one.
      //
      // The 'eventual' logic will be significantly more complex, though still manageable.
      let searchSpace = postContextState.searchSpace[0];

      // No matter the prediction, once we know the root of the prediction, we'll always 'replace' the
      // same amount of text.  We can handle this before the big 'prediction root' loop.
      let deleteLeft = 0;

      // The amount of text to 'replace' depends upon whatever sort of context change occurs
      // from the received input.
      const postContextTokens = postContextState.tokens;
      let postContextLength = postContextTokens.length;
      let contextLengthDelta = postContextTokens.length - contextState.tokens.length;
      // If the context now has more tokens, the token we'll be 'predicting' didn't originally exist.
      if(postContextLength == 0 || contextLengthDelta > 0) {
        // As the word/token being corrected/predicted didn't originally exist, there's no
        // part of it to 'replace'.
        deleteLeft = 0;

        // If the new token is due to whitespace or due to a different input type that would
        // likely imply a tokenization boundary...
        if(TransformUtils.isWhitespace(inputTransform)) {
          /* TODO:  consider/implement:  the second half of the comment above.
           * For example:  on input of a `'`, predict new words instead of replacing the `'`.
           * (since after a letter, the `'` will be ignored, anyway)
           *
           * Idea:  if the model's most likely prediction (with no root) would make a new
           * token if appended to the current token, that's probably a good case.
           * Keeps the check simple & quick.
           *
           * Might need a mixed mode, though:  ';' is close enough that `l` is a reasonable
           * fat-finger guess.   So yeah, we're not addressing this idea right now.
           * - so... consider multiple context behavior angles when building prediction roots?
           *
           * May need something similar to help handle contractions during their construction,
           * but that'd be within `ContextTracker`.
           * can' => [`can`, `'`]
           * can't => [`can't`]  (WB6, 7 of https://unicode.org/reports/tr29/#Word_Boundary_Rules)
           *
           * (Would also helps WB7b+c for Hebrew text)
           */

          // Infer 'new word' mode, even if we received new text when reaching
          // this position.  That new text didn't exist before, so still - nothing
          // to 'replace'.
          context = postContext; // As far as predictions are concerned, the post-context state
                                 // should not be replaced.  Predictions are to be rooted on
                                 // text "up for correction" - so we want a null root for this
                                 // branch.
          contextState = postContextState;
        }
        // If the tokenized context length is shorter... sounds like a backspace (or similar).
      } else if (contextLengthDelta < 0) {
        /* Ooh, we've dropped context here.  Almost certainly from a backspace.
         * Even if we drop multiple tokens... well, we know exactly how many chars
         * were actually deleted - `inputTransform.deleteLeft`.
         * Since we replace a word being corrected/predicted, we take length of the remaining
         * context's tail token in addition to however far was deleted to reach that state.
         */
        deleteLeft = this.wordbreak(postContext).kmwLength() + inputTransform.deleteLeft;
      } else {
        // Suggestions are applied to the pre-input context, so get the token's original length.
        // We're on the same token, so just delete its text for the replacement op.
        deleteLeft = this.wordbreak(context).kmwLength();
      }

      // Is the token under construction newly-constructed / is there no pre-existing root?
      // If so, we want to strongly avoid overcorrection, even for 'nearby' keys.
      // (Strong lexical frequency differences can easily cause overcorrection when only
      // one key's available.)
      //
      // NOTE:  we only want this applied word-initially, when any corrections 'correct'
      // 100% of the word.  Things are generally fine once it's not "all or nothing."
      let tailToken = postContextTokens[postContextTokens.length - 1];
      const isTokenStart = tailToken.transformDistributions.length <= 1;

      // TODO:  whitespace, backspace filtering.  Do it here.
      //        Whitespace is probably fine, actually.  Less sure about backspace.

      const SEARCH_TIMEOUT = correction.SearchSpace.DEFAULT_ALLOTTED_CORRECTION_TIME_INTERVAL;
      const timer = this.activeTimer = new correction.ExecutionTimer(this.testMode ? Number.MAX_VALUE : SEARCH_TIMEOUT, this.testMode ? Number.MAX_VALUE : SEARCH_TIMEOUT * 1.5);

      let bestCorrectionCost: number;
      let correctionPredictionMap: Record<string, Distribution<Suggestion>> = {};

      for await(let match of searchSpace.getBestMatches(timer)) {
        // Corrections obtained:  now to predict from them!
        const correction = match.matchString;

        // Worth considering:  extend Traversal to allow direct prediction lookups?
        // let traversal = match.finalTraversal;

        // Replace the existing context with the correction.
        const correctionTransform: Transform = {
          insert: correction,  // insert correction string
          deleteLeft: deleteLeft,
          id: inputTransform.id // The correction should always be based on the most recent external transform/transcription ID.
        }

        let rootCost = match.totalCost;

        /* If we're dealing with the FIRST keystroke of a new sequence, we'll **dramatically** boost
          * the exponent to ensure only VERY nearby corrections have a chance of winning, and only if
          * there are significantly more likely words.  We only need this to allow very minor fat-finger
          * adjustments for 100% keystroke-sequence corrections in order to prevent finickiness on
          * key borders.
          *
          * Technically, the probabilities this produces won't be normalized as-is... but there's no
          * true NEED to do so for it, even if it'd be 'nice to have'.  Consistently tracking when
          * to apply it could become tricky, so it's simpler to leave out.
          *
          * Worst-case, it's possible to temporarily add normalization if a code deep-dive
          * is needed in the future.
          */
        if(isTokenStart) {
          /* Suppose a key distribution:  most likely with p=0.5, second-most with 0.4 - a pretty
            * ambiguous case that would only arise very near the center of the boundary between two keys.
            * Raising (0.5/0.4)^16 ~= 35.53.  (At time of writing, SINGLE_CHAR_KEY_PROB_EXPONENT = 16.)
            * That seems 'within reason' for correction very near boundaries.
            *
            * So, with the second-most-likely key being that close in probability, its best suggestion
            * must be ~ 35.5x more likely than that of the truly-most-likely key to "win".  So, it's not
            * a HARD cutoff, but more of a 'soft' one.  Keeping the principles in mind documented above,
            * it's possible to tweak this to a more harsh or lenient setting if desired, rather than
            * being totally "all or nothing" on which key is taken for highly-ambiguous keypresses.
            */
          rootCost *= ModelCompositor.SINGLE_CHAR_KEY_PROB_EXPONENT;  // note the `Math.exp` below.
        }

        const predictionRoot = {
          sample: correctionTransform,
          p: Math.exp(-rootCost)
        };

        let predictions = this.predictFromCorrections([predictionRoot], context);

        // Only set 'best correction' cost when a correction ACTUALLY YIELDS predictions.
        if(predictions.length > 0 && bestCorrectionCost === undefined) {
          bestCorrectionCost = -Math.log(predictionRoot.p);
        }

        // If we're getting the same prediction again, it's lower-cost.  Update!
        let oldPredictionSet = correctionPredictionMap[match.matchString];
        if(oldPredictionSet) {
          rawPredictions = rawPredictions.filter((entry) => !oldPredictionSet.find((match) => entry.prediction.sample == match.sample));
        }

        correctionPredictionMap[match.matchString] = predictions.map((entry) => entry.prediction);

        rawPredictions = rawPredictions.concat(predictions);

        let correctionCost = match.totalCost;
        // Searching a bit longer is permitted when no predictions have been found.
        if(correctionCost >= bestCorrectionCost + 8) {
          break;
          // If enough have been found, we're safe to terminate earlier.
        } else if(rawPredictions.length >= ModelCompositor.MAX_SUGGESTIONS) {
            if(correctionCost >= bestCorrectionCost + 4) { // e^-4 = 0.0183156388.  Allows "80%" of an extra edit.
            // Very useful for stopping 'sooner' when words reach a sufficient length.
            break;
          } else {
            // Sort the prediction list; we need them in descending order for the next check.
            rawPredictions.sort(tupleDisplayOrderSort);

            // If the best suggestion from the search's current tier fails to beat the worst
            // pending suggestion from previous tiers, assume all further corrections will
            // similarly fail to win; terminate the search-loop.
            if(rawPredictions[ModelCompositor.MAX_SUGGESTIONS-1].totalProb > Math.exp(-correctionCost)) {
              break;
            }
          }
        }
      }

      // // For debugging / investigation.
      // console.log(`execute: ${timer.executionTime}, deferred: ${timer.deferredTime}`); //, total since start: ${timer.timeSinceConstruction}`);

      if(this.activeTimer == timer) {
        this.activeTimer = null;
      }
    }

    // Section 2 - post-analysis for our generated predictions, managing 'keep'.
    // Assumption:  Duplicated 'displayAs' properties indicate duplicated Suggestions.
    // When true, we can use an 'associative array' to de-duplicate everything.
    let suggestionDistribMap: {[key: string]: CorrectionPredictionTuple} = {};

    const keyed = (text: string) => this.lexicalModel.toKey ? this.lexicalModel.toKey(text) : text;
    const keyCased = (text: string) => this.lexicalModel.applyCasing ? this.lexicalModel.applyCasing('lower', text) : text;
    const keyedPrefix = keyed(truePrefix);
    const lowercasedPrefix = keyCased(truePrefix);

    let deduplicatedSuggestionTuples: CorrectionPredictionTuple[] = [];

    // Deduplicator + annotator of 'keep' suggestions.
    for(let tuple of rawPredictions) {
      if(currentCasing && currentCasing != 'lower') {
        this.applySuggestionCasing(tuple.prediction.sample, basePrefix, currentCasing);
      }

      // Don't set it unnecessarily; this can have side-effects in some automated tests.
      if(inputTransform.id !== undefined) {
        tuple.prediction.sample.transformId = inputTransform.id;
      }

      const predictedWord = this.wordbreak(models.applyTransform(tuple.prediction.sample.transform, context));

      // Is the suggestion an exact match (or, "similar enough") to the
      // actually-typed context?  If so, we wish to note this fact and to
      // prioritize such a suggestion over suggestions that are not.
      if(keyed(tuple.correction.sample) == keyedPrefix) {
        if(predictedWord == truePrefix) {
          // Exact match:  it's a perfect 'keep' suggestion.
          tuple.matchLevel = SuggestionSimilarity.exact;
          keepOption = this.toAnnotatedSuggestion(tuple.prediction.sample, 'keep',  models.QuoteBehavior.noQuotes);

          // Indicates that this suggestion exists directly within the lexical
          // model as a valid suggestion.  (We actively display it if it's an
          // exact match, but hide it if not, only preserving it for reversions
          // if/when needed.)
          keepOption.matchesModel = true;
          Object.assign(tuple.prediction.sample, keepOption);
          keepOption = tuple.prediction.sample as Outcome<Keep>;
        } else if(keyCased(predictedWord) == lowercasedPrefix) {
          // Case-insensitive match.  No diacritic differences; the ONLY difference is casing.
          tuple.matchLevel = SuggestionSimilarity.sameText;
        } else if(keyed(predictedWord) == keyedPrefix) {
          // Diacritic-insensitive / exact-key match.
          tuple.matchLevel = SuggestionSimilarity.sameKey;
        }
      }

      // Assumption:  suggestions that have the same net result should have the
      // same displayAs string.  (We could try to pick the one with highest net
      // probability, but that seems like too much of an edge-case to matter.)
      //
      // Either way, no point in showing multiple suggestions that do the same thing.
      // Merge 'em!
      const existingSuggestion = suggestionDistribMap[predictedWord];
      if(existingSuggestion) {
        existingSuggestion.totalProb += tuple.totalProb;
      } else {
        suggestionDistribMap[predictedWord] = tuple;
      }
    }

    // Generate a default 'keep' option if one was not otherwise produced.
    if(!keepOption && truePrefix != '') {
      // IMPORTANT:  duplicate the original transform.  Causes nasty side-effects
      // for context-tracking otherwise!
      let keepTransform: Transform = { ...inputTransform };

      // 1 is a filler value; goes unused b/c is for a 'keep'.
      let keepSuggestion = models.transformToSuggestion(keepTransform, 1);
      // This is the one case where the transform doesn't insert the full word; we need to override the displayAs param.
      keepSuggestion.displayAs = truePrefix;

      keepOption = this.toAnnotatedSuggestion(keepSuggestion, 'keep');
      keepOption.transformId = inputTransform.id;
      keepOption.matchesModel = false;

      deduplicatedSuggestionTuples.unshift({
        totalProb: keepOption.p,
        prediction: {
          sample: keepOption,
          p: keepOption.p,
        },
        correction: {
          sample: truePrefix,
          p: inputTransformProb // we already sorted + this aligns with inputTransform.
        },
        matchLevel: SuggestionSimilarity.exact,
      });
    }

    // Section 3:  Finalize suggestions, truncate list to the N (MAX_SUGGESTIONS) most optimal, return.

    // Now that we've calculated a unique set of probability masses, time to make them into a proper
    // distribution and prep for return.
    for(let key in suggestionDistribMap) {
      let pair = suggestionDistribMap[key];
      deduplicatedSuggestionTuples.push(pair);
    }

    deduplicatedSuggestionTuples = deduplicatedSuggestionTuples.sort(tupleDisplayOrderSort);
    this.predictionAutoSelect(deduplicatedSuggestionTuples);

    // Section 4: Trim down the suggestion list to the N (MAX_SUGGESTIONS) most optimal,
    // then cache and return the set of suggestions

    // Now that we've marked the suggestion to auto-select, we can finalize the suggestions.
    const suggestions = this.finalizeSuggestions(deduplicatedSuggestionTuples, context, inputTransform);

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

  private predictionAutoSelect(suggestionDistribution: CorrectionPredictionTuple[]) {
    if(suggestionDistribution.length == 0) {
      return;
    }

    const keepOption = suggestionDistribution[0].prediction.sample as Outcome<Keep>;
    if(keepOption.tag == 'keep' && keepOption.matchesModel) {
      // Auto-select it for auto-acceptance; we don't correct away from perfectly-valid
      // lexical entries, even if they are comparatively low-frequency.
      keepOption.autoAccept = true;
      return;
    } else if(suggestionDistribution.length == 1) {
      return;
    }

    suggestionDistribution = suggestionDistribution.slice(1);

    if(suggestionDistribution.length == 1) {
      // Mark for auto-acceptance; there are no alternatives.
      suggestionDistribution[0].prediction.sample.autoAccept = true;
      return;
    }

    // Is it reasonable to auto-accept any of our suggestions?
    const bestSuggestion = suggestionDistribution[0];

    const baseCorrection = bestSuggestion.correction.sample;
    if(baseCorrection.length == 0) {
      // If the correction is rooted on an empty root, there's no basis for
      // auto-correcting to this suggestion.
      return;
    }

    // Find the highest probability for any correction that led to a valid prediction.
    // No need to full-on re-sort everything, though.
    const bestCorrection = suggestionDistribution.reduce((prev, current) => prev?.correction.p > current.correction.p ? prev : current, null).correction;
    if(bestCorrection.p > bestSuggestion.correction.p) {
      // Here, the best suggestion didn't come from the best correction.
      // Is it actually reasonable to auto-correct?  We're probably just very
      // biased toward its frequency.  (Maybe a threshold should be considered?)
      return;
    }

    // compare best vs other probabilities.
    const probSum = suggestionDistribution.reduce((accum, current) => accum + current.totalProb, 0);
    const proportionOfBest = bestSuggestion.totalProb / probSum;
    if(proportionOfBest < .66) {
      return;
    }

    // compare correction-cost aspects?  We disable if the base correction is lower than best,
    // but should we do other comparisons too?

    bestSuggestion.prediction.sample.autoAccept = true;
  }

  /**
   * Given a set of generated + pre-processed suggestions + their associated
   * corrections, this method generates the final, published form for each
   * suggestion.
   *
   * Model-specific punctuation settings are applied during this process, as is
   * verbose logging about the influence of "correction" vs "lexicon-frequency"
   * per suggestion if enabled.
   * @param deduplicatedSuggestionTuples The set of suggestions to finalize.
   * @param context The context to which suggestions will be applied.
   * @param inputTransform The actual text change to `context` that triggered
   * the prediction request (which may be replaced by a Suggestion in the
   * future)
   * @returns
   */
  private finalizeSuggestions(deduplicatedSuggestionTuples: CorrectionPredictionTuple[], context: Context, inputTransform: Transform) {
    const punctuation = this.punctuation;
    const suggestions = deduplicatedSuggestionTuples.splice(0, ModelCompositor.MAX_SUGGESTIONS).map((tuple) => {
      const prediction = tuple.prediction;

      if(!this.verbose) {
        return {
          ...prediction.sample,
          p: tuple.totalProb
        };
      } else {
        const sample: Suggestion & {
          p?: number,
          "lexical-p"?: number,
          "correction-p"?: number
        } = {
          ...prediction.sample,
          p: tuple.totalProb,
          "lexical-p": prediction.p,
          "correction-p": tuple.correction.p
        }

        return sample;
      }
    });

    // Apply 'after word' punctuation and casing (when applicable).  Also, set suggestion IDs.
    // We delay until now so that utility functions relying on the unmodified Transform may execute properly.
    suggestions.forEach((suggestion) => {
      // Valid 'keep' suggestions may have zero length; we still need to evaluate the following code
      // for such cases.

      // If we're mid-word, delete its original post-caret text.
      const tokenization = this.tokenize(context);
      if(tokenization && tokenization.caretSplitsToken) {
        // While we wait on the ability to provide a more 'ideal' solution, let's at least
        // go with a more stable, if slightly less ideal, solution for now.
        //
        // A predictive text default (on iOS, at least) - immediately wordbreak
        // on suggestions accepted mid-word.
        suggestion.transform.insert += punctuation.insertAfterWord;

        // Do we need to manipulate the suggestion's transform based on the current state of the context?
      } else if(!context.right) {
        suggestion.transform.insert += punctuation.insertAfterWord;
      } else if(punctuation.insertAfterWord != '') {
        if(context.right.indexOf(punctuation.insertAfterWord) != 0) {
          suggestion.transform.insert += punctuation.insertAfterWord;
        }
      }

      // If this is a suggestion after wordbreak input, make sure we preserve the wordbreak transform!
      if(TransformUtils.isWhitespace(inputTransform)) {
        let mergedTransform = models.buildMergedTransform(inputTransform, suggestion.transform);
        mergedTransform.id = suggestion.transformId;

        // Temporarily and locally drops 'readonly' semantics so that we can reassign the transform.
        // See https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-8.html#improved-control-over-mapped-type-modifiers
        let mutableSuggestion = suggestion as {-readonly [transform in keyof Suggestion]: Suggestion[transform]};

        // Assignment via by-reference behavior, as suggestion is an object
        mutableSuggestion.transform = mergedTransform;
      }

      suggestion.id = this.SUGGESTION_ID_SEED;
      this.SUGGESTION_ID_SEED++;
    });

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

  private toAnnotatedSuggestion(suggestion: Outcome<Suggestion>,
    annotationType: SuggestionTag,
    quoteBehavior?: models.QuoteBehavior): Outcome<Suggestion>;
  private toAnnotatedSuggestion(suggestion: Outcome<Suggestion>,
    annotationType: 'keep',
    quoteBehavior?: models.QuoteBehavior): Outcome<Keep>;
  private toAnnotatedSuggestion(suggestion: Outcome<Suggestion>,
    annotationType: 'revert',
    quoteBehavior?: models.QuoteBehavior): Outcome<Reversion>;
  private toAnnotatedSuggestion(suggestion: Outcome<Suggestion>,
                                annotationType: SuggestionTag,
                                quoteBehavior: models.QuoteBehavior = models.QuoteBehavior.default): Outcome<Suggestion> {
    // A method-internal 'import' of the enum.
    let QuoteBehavior = models.QuoteBehavior;

    let defaultQuoteBehavior = QuoteBehavior.noQuotes;
    if(annotationType == 'keep' || annotationType == 'revert') {
      defaultQuoteBehavior = QuoteBehavior.useQuotes;
    }

    return {
      transform: suggestion.transform,
      transformId: suggestion.transformId,
      displayAs: QuoteBehavior.apply(quoteBehavior, suggestion.displayAs, this.punctuation, defaultQuoteBehavior),
      tag: annotationType,
      p: suggestion.p
    };
  }

  /**
   * Returns the punctuation used for this model, filling out unspecified fields
   */
  private static determinePunctuationFromModel(model: LexicalModel): LexicalModelPunctuation {
    let defaults = DEFAULT_PUNCTUATION;

    // Use the defaults of the model does not provide any punctuation at all.
    if (!model.punctuation)
      return defaults;

    let specifiedPunctuation = model.punctuation;
    let insertAfterWord = specifiedPunctuation.insertAfterWord;
    if (insertAfterWord !== '' && !insertAfterWord) {
      insertAfterWord = defaults.insertAfterWord;
    }

    let quotesForKeepSuggestion = specifiedPunctuation.quotesForKeepSuggestion;
    if (!quotesForKeepSuggestion) {
      quotesForKeepSuggestion = defaults.quotesForKeepSuggestion;
    }

    let isRTL = specifiedPunctuation.isRTL;
    // Default:  false / undefined, so no need to directly specify it.

    return {
      insertAfterWord, quotesForKeepSuggestion, isRTL
    }
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
      if(postContextTokenization.left.length > 0) {
        revertedPrefix = postContextTokenization.left[postContextTokenization.left.length-1];
      } else {
        revertedPrefix = '';
      }
      revertedPrefix += postContextTokenization.caretSplitsToken ? postContextTokenization.right[0] : '';
    } else {
      revertedPrefix = this.wordbreak(postContext);
    }

    let firstConversion = models.transformToSuggestion(reversionTransform);
    firstConversion.displayAs = revertedPrefix;

    // Build the actual Reversion, which is technically an annotated Suggestion.
    // Since we're outside of the standard `predict` control path, we'll need to
    // set the Reversion's ID directly.
    let reversion = this.toAnnotatedSuggestion(firstConversion, 'revert');
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
      suggestion.autoAccept = false;
    });
    return suggestions;
  }

  private wordbreak(context: Context): string {
    let model = this.lexicalModel;

    if(model.wordbreaker || !model.wordbreak) {
      // We don't need a 12.0 / 13.0 compatibility mode here.
      // We're either relying on defaults or on the 14.0+ wordbreaker spec.
      let wordbreaker = model.wordbreaker || wordBreakers.default;

      return models.wordbreak(wordbreaker, context);
    } else {
      // 1.  This model does not provide a model following the 14.0+ wordbreaking spec
      // 2.  This model DOES define a custom wordbreaker following the 12.0-13.0 spec.

      // Since the model relies on custom wordbreaking behavior, we need to use the
      // old, deprecated wordbreaking pattern.
      return model.wordbreak(context);
    }
  }

  private tokenize(context: Context): models.Tokenization {
    let model = this.lexicalModel;

    if(model.wordbreaker) {
      return models.tokenize(model.wordbreaker, context);
    } else {
      return null;
    }
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

  private detectCurrentCasing(context: Context): CasingForm {
    let model = this.lexicalModel;

    let text = this.wordbreak(context);

    if(!model.languageUsesCasing) {
      throw "Invalid attempt to detect casing: languageUsesCasing is set to false";
    }

    if(!model.applyCasing) {
      // The worker should automatically 'sub in' default behavior during the model's load if that
      // function isn't defined explicitly as part of the model.
      throw "Invalid LMLayer state:  languageUsesCasing is set to true, but no applyCasing function exists";
    }

    // If the user has selected Shift or Caps layer, that overrides our
    // text analysis
    if(context.casingForm == 'upper' || context.casingForm == 'initial') {
      return context.casingForm;
    }
    if(model.applyCasing('lower', text) == text) {
      return 'lower';
    } else if(model.applyCasing('upper', text) == text) {
      // If only a single character has been input, assume we're in 'initial' mode.
      return text.kmwLength() > 1 ? 'upper' : 'initial';
    } else if(model.applyCasing('initial', text) == text) {
      // We check 'initial' last, as upper-case input is indistinguishable.
      return 'initial';
    } else {
      // If we do not have a casing form given to us by the keyboard, then
      // 'null' is returned when no casing pattern matches the input.
      return context.casingForm ?? null;
    }
  }
}

/**
 * The default punctuation and spacing produced by the model.
 */
const DEFAULT_PUNCTUATION: LexicalModelPunctuation = {
  quotesForKeepSuggestion: { open: `“`, close: `”`},
  insertAfterWord: " " ,
};
