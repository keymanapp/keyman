import * as models from '@keymanapp/models-templates';

import TransformUtils from './transformUtils.js';
import { determineModelTokenizer, determineModelWordbreaker, determinePunctuationFromModel } from './model-helpers.js';
import { ContextTracker, TrackedContextState } from './correction/context-tracker.js';
import { ExecutionTimer } from './correction/execution-timer.js';
import ModelCompositor from './model-compositor.js';
import { LexicalModelTypes } from '@keymanapp/common-types';
import { defaultWordbreaker, WordBreakProperty } from '@keymanapp/models-wordbreakers';
const searchForProperty = defaultWordbreaker.searchForProperty;

import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import Keep = LexicalModelTypes.Keep;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Outcome = LexicalModelTypes.Outcome;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Reversion = LexicalModelTypes.Reversion;
import Suggestion = LexicalModelTypes.Suggestion;
import SuggestionTag = LexicalModelTypes.SuggestionTag;
import Transform = LexicalModelTypes.Transform;

/*
 * The functions in this file exist to provide unit-testable stateless components for the
 * correction/prediction process at the core of our predictive-text engine.
 */

export const AUTOSELECT_PROPORTION_THRESHOLD = .66;

/**
 * Defines thresholds used to determine when it is appropriate to stop searching
 * for more prediction-roots.
 *
 * Note that these costs are defined in log-space; a value of 4 corresponds to
 * a factor of `e^-4`, or about 0.0183.
 */
export const CORRECTION_SEARCH_THRESHOLDS = {
  /**
   * Defines the maximum search range used to find additional prediction roots
   * once the first correction yielding a viable prediction has been found.
   *
   * If that "first correction" has an edge cost of 1 in log-space, the search
   * would stop at a total cost of 1 + this value.
   */
  MAX_SEARCH_THRESHOLD: 8 as const,
  /**
   * Defines the maximum search range used to find additional prediction roots
   * once enough viable predictions have been found to return a "full" set.
   * ("Full": enough to meet the count set by `ModelCompositor.MAX_SUGGESTIONS`)
   *
   * It _is_ possible to find 'better' predictions rooted on 'worse'
   * corrections, but the further we search, the less likely it is we'll find
   * strong enough replacements.
   *
   *
   * If the first correction yielding a viable prediction has an edge cost of 1
   * in log-space, the search would stop at a total cost of 1 + this value if
   * a "full" set of suggestions had already been found.
   */
  REPLACEMENT_SEARCH_THRESHOLD: 4 as const // e^-4 = 0.0183156388.  Allows "80%" of an extra edit.
}

export type CorrectionPredictionTuple = {
  prediction: ProbabilityMass<Suggestion>,
  correction: ProbabilityMass<string>,
  totalProb: number;
  matchLevel: SuggestionSimilarity;
  preservationTransform?: Transform;
};

export enum SuggestionSimilarity {
  none = 0,
  sameKey = 1,
  sameText = 2,
  exact = 3
}

export function tupleDisplayOrderSort(a: CorrectionPredictionTuple, b: CorrectionPredictionTuple) {
  // Similarity distance
  const simDist = b.matchLevel - a.matchLevel;
  if(simDist != 0) {
    return simDist;
  }

  // Probability distance
  return b.totalProb - a.totalProb;
}

/**
 * This method performs the correction-search and model-lookup operations for
 * prediction generation by using the user's context state and potential
 * inputs (according to fat-finger distributions).
 * @param transformDistribution
 * @param context
 * @returns
 */
export async function correctAndEnumerate(
  contextTracker: ContextTracker,
  lexicalModel: LexicalModel,
  timer: ExecutionTimer,
  transformDistribution: Distribution<Transform>,
  context: Context
): Promise<{
  /**
   * For models that support correction-search caching, this provides the
   * cached object corresponding to this method's operation.
   *
   * Otherwise, is `null`.
   */
  postContextState?: TrackedContextState;

  /**
   * The suggestions generated based on the user's input state.
   */
  rawPredictions: CorrectionPredictionTuple[];
}> {
  const wordbreak = determineModelWordbreaker(lexicalModel);

  // Assertion / pre-condition:  `transformDistribution` should be sorted!
  const inputTransform = transformDistribution[0].sample;

  const postContext = models.applyTransform(inputTransform, context);
  let rawPredictions: CorrectionPredictionTuple[] = [];

  // If `this.contextTracker` does not exist, we don't have the
  // `LexiconTraversal` pattern available to us.  We're unable to efficiently
  // iterate through the lexicon as a result, so we use a far lazier pattern -
  // only checking corrections for the final keystroke.
  //
  // It's mostly here to support models compiled before Keyman 14.0, which was
  // when the `LexiconTraversal` pattern was established.
  if(!contextTracker) {
    let predictionRoots: ProbabilityMass<Transform>[];

    // Only allow new-word suggestions if space was the most likely keypress.
    const allowSpace = TransformUtils.isWhitespace(inputTransform);
    const allowBksp = TransformUtils.isBackspace(inputTransform);

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
    rawPredictions = predictFromCorrections(lexicalModel, predictionRoots, context);
    if(allowSpace) {
      rawPredictions.forEach((entry) => entry.preservationTransform = inputTransform);
    }

    return {
      postContextState: null,
      rawPredictions: rawPredictions
    };
  }

  // 'else':  the current, 14.0+ pattern, which is able to leverage
  // `LexiconTraversal`s for greater search efficiency.  This pattern
  // facilitates a more thorough correction-search pattern.

  // Token replacement benefits greatly from knowledge of the prior context state.
  let { state: contextState } = contextTracker.analyzeState(
    lexicalModel,
    context,
    null
  );

  // Corrections and predictions are based upon the post-context state, though.
  const contextChangeAnalysis = contextTracker.analyzeState(
    lexicalModel,
    context,
    !TransformUtils.isEmpty(inputTransform)
      ? transformDistribution
      : null
  );
  const postContextState = contextChangeAnalysis.state;

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
  // Only use of `contextState`.
  let contextLengthDelta = postContextTokens.length - contextState.tokens.length;
  // If the context now has more tokens, the token we'll be 'predicting' didn't originally exist.
  if(contextChangeAnalysis.preservationTransform) {
    // As the word/token being corrected/predicted didn't originally exist, there's no
    // part of it to 'replace'.  (Suggestions are applied to the pre-transform state.)
    deleteLeft = 0;

    // If the new token is due to whitespace or due to a different input type that would
    // likely imply a tokenization boundary, infer 'new word' mode.
    // Apply any part of the context change that is not considered
    // to be up for correction.
    context = models.applyTransform(contextChangeAnalysis.preservationTransform, context);
    // If the tokenized context length is shorter... sounds like a backspace (or similar).
  } else if (contextLengthDelta < 0) {
    /* Ooh, we've dropped context here.  Almost certainly from a backspace.
      * Even if we drop multiple tokens... well, we know exactly how many chars
      * were actually deleted - `inputTransform.deleteLeft`.
      * Since we replace a word being corrected/predicted, we take length of the remaining
      * context's tail token in addition to however far was deleted to reach that state.
      */
    deleteLeft = wordbreak(postContext).kmwLength() + inputTransform.deleteLeft;
  } else {
    // Suggestions are applied to the pre-input context, so get the token's original length.
    // We're on the same token, so just delete its text for the replacement op.
    deleteLeft = wordbreak(context).kmwLength();
  }

  // Is the token under construction newly-constructed / is there no pre-existing root?
  // If so, we want to strongly avoid overcorrection, even for 'nearby' keys.
  // (Strong lexical frequency differences can easily cause overcorrection when only
  // one key's available.)
  //
  // NOTE:  we only want this applied word-initially, when any corrections 'correct'
  // 100% of the word.  Things are generally fine once it's not "all or nothing."
  let tailToken = postContextTokens[postContextTokens.length - 1];

  // Did the wordbreaker (or similar) append a blank token before the caret?  If so,
  // preserve that by preventing corrections from triggering left-deletion.
  if(tailToken.raw == '') {
    deleteLeft = 0;
  }

  const isTokenStart = tailToken.transformDistributions.length <= 1;

  // TODO:  whitespace, backspace filtering.  Do it here.
  //        Whitespace is probably fine, actually.  Less sure about backspace.

  let bestCorrectionCost: number;
  let correctionPredictionMap: Record<string, Distribution<Suggestion>> = {};

  // If corrections are not enabled, bypass the correction search aspect entirely.
  // No need to 'search' - just do a direct lookup.
  if(!searchSpace.correctionsEnabled) {
    const predictionRoot = {
      sample: {
        insert: wordbreak(postContext),  // insert correction string
        deleteLeft: deleteLeft,
        id: inputTransform.id // The correction should always be based on the most recent external transform/transcription ID.
      },
      p: 1.0
    };

    let predictions = predictFromCorrections(lexicalModel, [predictionRoot], context);
    predictions.forEach((entry) => entry.preservationTransform = contextChangeAnalysis.preservationTransform);

    // Only one 'correction' / prediction root is allowed - the actual text.
    return {
      postContextState: postContextState,
      rawPredictions: predictions
    }
  }

  // Only run the correction search when corrections are enabled.
  for await(let match of searchSpace.getBestMatches(timer)) {
    // Corrections obtained:  now to predict from them!
    const correction = match.matchString;

    // If our 'match' results in fully deleting the new token, reject it and try again.
    if(match.matchSequence.length == 0 && match.inputSequence.length != 0) {
      continue;
    }

    // If our 'match' fully replaces the token, reject it and try again.
    if(match.matchSequence.length != 0 && match.matchSequence.length == match.knownCost) {
      continue;
    }

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

    let predictions = predictFromCorrections(lexicalModel, [predictionRoot], context);
    predictions.forEach((entry) => entry.preservationTransform = contextChangeAnalysis.preservationTransform);

    // Only set 'best correction' cost when a correction ACTUALLY YIELDS predictions.
    if(predictions.length > 0 && bestCorrectionCost === undefined) {
      bestCorrectionCost = rootCost;
    }

    // If we're getting the same prediction again, it's lower-cost.  Update!
    let oldPredictionSet = correctionPredictionMap[match.matchString];
    if(oldPredictionSet) {
      rawPredictions = rawPredictions.filter((entry) => !oldPredictionSet.find((match) => entry.prediction.sample == match.sample));
    }

    correctionPredictionMap[match.matchString] = predictions.map((entry) => entry.prediction);

    rawPredictions = rawPredictions.concat(predictions);

    if(shouldStopSearchingEarly(bestCorrectionCost, match.totalCost, rawPredictions)) {
      break;
    }
  }

  // // For debugging / investigation.
  // console.log(`execute: ${timer.executionTime}, deferred: ${timer.deferredTime}`); //, total since start: ${timer.timeSinceConstruction}`);

  return {
    postContextState: postContextState,
    rawPredictions: rawPredictions
  };
}

export function shouldStopSearchingEarly(
  bestCorrectionCost: number,
  currentCorrectionCost: number,
  rawPredictions: CorrectionPredictionTuple[]
) {
  if(currentCorrectionCost >= bestCorrectionCost + CORRECTION_SEARCH_THRESHOLDS.MAX_SEARCH_THRESHOLD) {
    return true;
    // If enough have been found, we're safe to terminate earlier.
  } else if(rawPredictions.length >= ModelCompositor.MAX_SUGGESTIONS) {
    if(currentCorrectionCost >= bestCorrectionCost + CORRECTION_SEARCH_THRESHOLDS.REPLACEMENT_SEARCH_THRESHOLD) {
      // Very useful for stopping 'sooner' when words reach a sufficient length.
      return true;
    } else {
      // Sort the prediction list; we need them in descending order for the next check.
      rawPredictions.sort(tupleDisplayOrderSort);

      // If the best suggestion from the search's current tier fails to beat the worst
      // pending suggestion from previous tiers, assume all further corrections will
      // similarly fail to win; terminate the search-loop.
      if(rawPredictions[ModelCompositor.MAX_SUGGESTIONS-1].totalProb > Math.exp(-currentCorrectionCost)) {
        return true;
      }
    }
  }

  return false;
}

/**
 * Given a generated set of corrections from the correction-search process, this
 * function searches the lexical model for valid predictions rooted on each.
 *
 * While doing so, it also associates each prediction with metadata used for
 * to "rank" and select the best predictions once the search is complete.  This
 * is performed at later stages.
 * @param lexicalModel
 * @param corrections
 * @param context
 * @returns
 */
export function predictFromCorrections(
  lexicalModel: LexicalModel,
  corrections: ProbabilityMass<Transform>[],
  context: Context
): CorrectionPredictionTuple[] {
  let returnedPredictions: CorrectionPredictionTuple[] = [];
  const wordbreak = determineModelWordbreaker(lexicalModel);

  for(let correction of corrections) {
    let predictions = lexicalModel.predict(correction.sample, context);

    const { sample: correctionTransform, p: correctionProb } = correction;
    const correctionRoot = wordbreak(models.applyTransform(correction.sample, context));

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
    });

    returnedPredictions = returnedPredictions.concat(predictionSet);
  }

  return returnedPredictions;
}

/**
 * Given an array of suggestions output from the correction and model-lookup processes,
 * this function checks for any duplicate suggestions and merges them.
 *
 * Note that duplicates can arise for a few reasons:
 * - Two or more corrections may have the same net result (due to keyboard rules, etc)
 * - Application of casing may cause previously-distinct suggestions to no longer be distinct.
 * @param rawPredictions
 * @param context The context to which suggestions will be applied.
 * @returns
 */
export function dedupeSuggestions(
  lexicalModel: LexicalModel,
  rawPredictions: CorrectionPredictionTuple[],
  context: Context
) {
  const wordbreak = determineModelWordbreaker(lexicalModel);

  let suggestionDistribMap: {[key: string]: CorrectionPredictionTuple} = {};
  let suggestionDistribution: CorrectionPredictionTuple[] = [];

  // Deduplicator + annotator of 'keep' suggestions.
  for(let tuple of rawPredictions) {
    const predictedWord = wordbreak(models.applyTransform(tuple.prediction.sample.transform, context));

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

  // Now that we've calculated a unique set of probability masses, time to
  // make them into a proper distribution and prep for return.
  for(let key in suggestionDistribMap) {
    let pair = suggestionDistribMap[key];
    suggestionDistribution.push(pair);
  }

  return suggestionDistribution;
}

/**
 * This function checks for any suggestions that directly match the actual
 * context in some manner and ranks suggestions accordingly.  Additionally, if
 * there is no such suggestion, a stand-in is generated and added to the list,
 * though marked as "not matching the model".
 *
 * The suggestion "ranks", from highest to lowest:
 * - the suggestion produces an exact match for the user's current text
 * - the suggestion produces a case-insensitive match for the user's current
 *   text
 * - the suggestion produces a case + diacritic insensitive match for the
 *   user's current text
 * - any other suggestion
 *
 * @param suggestionDistribution
 * @param context
 * @param trueInput inputTransform + its assigned probability
 * @returns
 */
export function processSimilarity(
  lexicalModel: LexicalModel,
  suggestionDistribution: CorrectionPredictionTuple[],
  context: Context,
  trueInput: ProbabilityMass<Transform>
) {
  const { sample: inputTransform, p: inputTransformProb } = trueInput;
  const wordbreak = determineModelWordbreaker(lexicalModel);

  const postContext = models.applyTransform(inputTransform, context);
  const truePrefix = wordbreak(postContext);

  const keyed = (text: string) => lexicalModel.toKey ? lexicalModel.toKey(text) : text;
  const keyCased = (text: string) => lexicalModel.applyCasing ? lexicalModel.applyCasing('lower', text) : text;
  const keyedPrefix = keyed(truePrefix);
  const lowercasedPrefix = keyCased(truePrefix);

  let keepOption: Outcome<Keep>;

  for(let tuple of suggestionDistribution) {
    // Don't set it unnecessarily; this can have side-effects in some automated tests.
    if(inputTransform.id !== undefined) {
      tuple.prediction.sample.transformId = inputTransform.id;
    }

    const predictedWord = wordbreak(models.applyTransform(tuple.prediction.sample.transform, context));

    // Is the suggestion an exact match (or, "similar enough") to the
    // actually-typed context?  If so, we wish to note this fact and to
    // prioritize such a suggestion over suggestions that are not.
    if(keyed(tuple.correction.sample) == keyedPrefix) {
      if(predictedWord == truePrefix) {
        // Exact match:  it's a perfect 'keep' suggestion.
        tuple.matchLevel = SuggestionSimilarity.exact;
        keepOption = toAnnotatedSuggestion(lexicalModel, tuple.prediction.sample, 'keep',  models.QuoteBehavior.noQuotes);

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
      } else {
        tuple.matchLevel = SuggestionSimilarity.none;
      }
    } else {
      tuple.matchLevel = SuggestionSimilarity.none;
    }
  }

  // If we already have a keep option, we're done!  Return and move on.
  if(keepOption || truePrefix == '') {
    return;
  }

  // Generate a full-word 'keep' replacement like other suggestions when one is not otherwise
  // produced; we want to replace the full token in the same manner used for other suggestions.
  const basePrefixLength = truePrefix.kmwLength() - inputTransform.insert.kmwLength() + inputTransform.deleteLeft;
  const keepTransform = {
    insert: truePrefix,
    deleteLeft: basePrefixLength
  };

  // 1 is a filler value; goes unused b/c is for a 'keep'.
  let keepSuggestion = models.transformToSuggestion(keepTransform, 1);
  // This is the one case where the transform doesn't insert the full word; we need to override the displayAs param.
  keepSuggestion.displayAs = truePrefix;

  keepOption = toAnnotatedSuggestion(lexicalModel, keepSuggestion, 'keep');
  if(inputTransform.id !== undefined) {
    keepOption.transformId = inputTransform.id;
  }
  keepOption.matchesModel = false;

  // Insert our synthetic keepOption as a prediction.
  suggestionDistribution.unshift({
    totalProb: keepOption.p,
    prediction: {
      sample: keepOption,
      p: keepOption.p,
    },
    correction: {
      sample: truePrefix,
      p: inputTransformProb
    },
    matchLevel: SuggestionSimilarity.exact
  });
}

/**
 * This function may be used to prevent auto-selection/auto-correct from applying in
 * unexpected ways.  For example, when typing numbers in English, we don't expect
 * '5' to auto-correct to '5th' just because there are no pure-number entries in
 * the lexicon rooted on '5'.
 * @param correction 
 * @returns 
 */
export function correctionValidForAutoSelect(correction: string) {
  let chars = [...correction];
  
  // If the _correction_ - the actual, existing text - does not include any letters,
  // then predictions built upon it should not be considered valid for auto-correction.
  for(let c of chars) {
    // Found even one letter?  We'll consider it valid.
    switch(searchForProperty(c.codePointAt(0))) {
      case WordBreakProperty.ALetter:
      case WordBreakProperty.Hebrew_Letter:
      case WordBreakProperty.Katakana:
        return true;
      default:
    }
  }

  // Only reached when the correction has nothing that passes as a letter in-context.
  // (MidLet and MidNumLet only count when there are adjacent letters.)
  return false;
}

export function predictionAutoSelect(suggestionDistribution: CorrectionPredictionTuple[]) {
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
    // Prevent auto-acceptance when the root doesn't meet validation criteria.
    if(!correctionValidForAutoSelect(suggestionDistribution[0].correction.sample)) {
      return;
    }

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

  // If we allow an option to allow same-key suggestions to replace context automatically
  // - such as replacing `cant` with `can't` if the latter is much more frequent -
  // we may wish to group matchLevel values below by 'mapping' them with an appropriate
  // function.  (Both on the next line and within the reduce functor.)
  const bestSuggestionTier = bestSuggestion.matchLevel;

  // compare best vs other probabilities of compatible tier.
  const probSum = suggestionDistribution.reduce((accum, current) => {
    // If the suggestion is from a different similarity tier, do not count it against
    // the required auto-select probability ratio threshold.  That threshold should
    // only apply within the suggestion's tier.
    return accum + (current.matchLevel == bestSuggestionTier ? current.totalProb : 0)
  }, 0);
  const proportionOfBest = bestSuggestion.totalProb / probSum;
  if(proportionOfBest < AUTOSELECT_PROPORTION_THRESHOLD) {
    return;
  }

  if(!correctionValidForAutoSelect(bestSuggestion.correction.sample)) {
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
 * per suggestion if enabled.  If this is a suggestion presented after
 * whitespace input, this function is also responsible for preserving that
 * whitespace.
 * @param deduplicatedSuggestionTuples The set of suggestions to finalize.
 * @param context The context to which suggestions will be applied.
 * @param inputTransform The actual text change to `context` that triggered the
 * prediction request (which may be replaced by a Suggestion in the future)
 * @returns
 */
export function finalizeSuggestions(
  lexicalModel: LexicalModel,
  deduplicatedSuggestionTuples: CorrectionPredictionTuple[],
  context: Context,
  inputTransform: Transform,
  verbose?: boolean
) {
  const punctuation = determinePunctuationFromModel(lexicalModel);
  const tokenize = determineModelTokenizer(lexicalModel);

  const suggestions = deduplicatedSuggestionTuples.map((tuple) => {
    const prediction = tuple.prediction;

    // If this is a suggestion after any form of wordbreak input, make sure we preserve any components
    // from prior tokens!
    //
    // Note:  may need adjustment if/when supporting phrase-level correction.
    if(tuple.preservationTransform) {
      let mergedTransform = models.buildMergedTransform(tuple.preservationTransform, prediction.sample.transform);
      mergedTransform.id = prediction.sample.transformId;

      // Temporarily and locally drops 'readonly' semantics so that we can reassign the transform.
      // See https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-8.html#improved-control-over-mapped-type-modifiers
      let mutableSuggestion = prediction.sample as {-readonly [transform in keyof Suggestion]: Suggestion[transform]};

      // Assignment via by-reference behavior, as suggestion is an object
      mutableSuggestion.transform = mergedTransform;
    }

    if(!verbose) {
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

  // Apply 'after word' punctuation and other post-processing, setting suggestion IDs.
  // We delay until now so that utility functions relying on the unmodified Transform may execute properly.
  suggestions.forEach((suggestion) => {
    // Valid 'keep' suggestions may have zero length; we still need to evaluate the following code
    // for such cases.

    // If we're mid-word, delete its original post-caret text.
    const tokenization = tokenize(context);
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

  });

  return suggestions;
}

export function toAnnotatedSuggestion(
  lexicalModel: LexicalModel,
  suggestion: Outcome<Suggestion>,
  annotationType: SuggestionTag,
  quoteBehavior?: models.QuoteBehavior
): Outcome<Suggestion>;
export function toAnnotatedSuggestion(
  lexicalModel: LexicalModel,
  suggestion: Outcome<Suggestion>,
  annotationType: 'keep',
  quoteBehavior?: models.QuoteBehavior
): Outcome<Keep>;
export function toAnnotatedSuggestion(
  lexicalModel: LexicalModel,
  suggestion: Outcome<Suggestion>,
  annotationType: 'revert',
  quoteBehavior?: models.QuoteBehavior
): Outcome<Reversion>;
export function toAnnotatedSuggestion(
  lexicalModel: LexicalModel,
  suggestion: Outcome<Suggestion>,
  annotationType: SuggestionTag,
  quoteBehavior: models.QuoteBehavior = models.QuoteBehavior.default
): Outcome<Suggestion> {
  // A method-internal 'import' of the enum.
  let QuoteBehavior = models.QuoteBehavior;
  const punctuation = determinePunctuationFromModel(lexicalModel);

  let defaultQuoteBehavior = QuoteBehavior.noQuotes;
  if(annotationType == 'keep' || annotationType == 'revert') {
    defaultQuoteBehavior = QuoteBehavior.useQuotes;
  }

  const result: Outcome<Suggestion> = {
    transform: suggestion.transform,
    displayAs: QuoteBehavior.apply(quoteBehavior, suggestion.displayAs, punctuation, defaultQuoteBehavior),
    tag: annotationType,
    p: suggestion.p
  };

  if(suggestion.transformId !== undefined) {
    result.transformId = suggestion.transformId;
  }

  return result;
}