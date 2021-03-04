/// <reference path="../node_modules/@keymanapp/models-templates/src/index.ts" />
/// <reference path="correction/context-tracker.ts" />

class ModelCompositor {
  private lexicalModel: LexicalModel;
  private contextTracker?: correction.ContextTracker;
  private static readonly MAX_SUGGESTIONS = 12;
  readonly punctuation: LexicalModelPunctuation;

  private SUGGESTION_ID_SEED = 0;

  constructor(lexicalModel: LexicalModel) {
    this.lexicalModel = lexicalModel;
    if(lexicalModel.traverseFromRoot) {
      this.contextTracker = new correction.ContextTracker();
    }
    this.punctuation = ModelCompositor.determinePunctuationFromModel(lexicalModel);
  }

  protected isWhitespace(transform: Transform): boolean {
    // Matches prefixed text + any instance of a character with Unicode general property Z* or the following: CR, LF, and Tab.
    let whitespaceRemover = /.*[\u0009\u000A\u000D\u0020\u00a0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u200b\u2028\u2029\u202f\u205f\u3000]/i;
    
    // Filter out null-inserts; their high probability can cause issues.
    if(transform.insert == '') { // Can actually register as 'whitespace'.
      return false;
    }

    let insert = transform.insert;

    insert = insert.replace(whitespaceRemover, '');

    return insert == '';
  }

  protected isBackspace(transform: Transform): boolean {
    return transform.insert == "" && transform.deleteLeft > 0;
  }

  protected isEmpty(transform: Transform): boolean {
    return transform.insert == '' && transform.deleteLeft == 0;
  }

  private predictFromCorrections(corrections: ProbabilityMass<Transform>[], context: Context): Distribution<Suggestion> {
    let returnedPredictions: Distribution<Suggestion> = [];

    for(let correction of corrections) {
      let predictions = this.lexicalModel.predict(correction.sample, context);

      let predictionSet = predictions.map(function(pair: ProbabilityMass<Suggestion>) {
        let transform = correction.sample;
        let inputProb = correction.p;
        // Let's not rely on the model to copy transform IDs.
        // Only bother is there IS an ID to copy.
        if(transform.id !== undefined) {
          pair.sample.transformId = transform.id;
        }

        let prediction = {sample: pair.sample, p: pair.p * inputProb};
        return prediction;
      }, this);

      returnedPredictions = returnedPredictions.concat(predictionSet);
    }

    return returnedPredictions;
  }

  predict(transformDistribution: Transform | Distribution<Transform>, context: Context): Suggestion[] {
    let suggestionDistribution: Distribution<Suggestion> = [];
    let lexicalModel = this.lexicalModel;
    let punctuation = this.punctuation;

    if(!(transformDistribution instanceof Array)) {
      transformDistribution = [ {sample: transformDistribution, p: 1.0} ];
    }

    // Find the transform for the actual keypress.
    let inputTransform = transformDistribution.sort(function(a, b) {
      return b.p - a.p;
    })[0].sample;

    // Only allow new-word suggestions if space was the most likely keypress.
    let allowSpace = this.isWhitespace(inputTransform);
    let allowBksp = this.isBackspace(inputTransform);

    let postContext = models.applyTransform(inputTransform, context);
    let keepOptionText = this.wordbreak(postContext);
    let keepOption: Outcome<Keep> = null;

    let rawPredictions: Distribution<Suggestion> = [];

    // Used to restore whitespaces if operations would remove them.
    let prefixTransform: Transform;
    let contextState: correction.TrackedContextState = null;

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
        prefixTransform = inputTransform;
      } else {
        predictionRoots = transformDistribution.map(function(alt) {
          let transform = alt.sample;

          // Filter out special keys unless they're expected.
          if(this.isWhitespace(transform) && !allowSpace) {
            return null;
          } else if(this.isBackspace(transform) && !allowBksp) {
            return null;
          }

          return alt;
        }, this);
      }

      // Remove `null` entries.
      predictionRoots = predictionRoots.filter(tuple => !!tuple);

      // Running in bulk over all suggestions, duplicate entries may be possible.
      rawPredictions = this.predictFromCorrections(predictionRoots, context);
    } else {
      contextState = this.contextTracker.analyzeState(this.lexicalModel, 
                                                      postContext,
                                                      !this.isEmpty(inputTransform) ? 
                                                                    transformDistribution: 
                                                                    null
                                                      );

      // TODO:  Should we filter backspaces & whitespaces out of the transform distribution?
      //        Ideally, the answer (in the future) will be no, but leaving it in right now may pose an issue.
      
      // Rather than go "full hog" and make a priority queue out of the eventual, future competing search spaces...
      // let's just note that right now, there will only ever be one.
      //
      // The 'eventual' logic will be significantly more complex, though still manageable.
      let searchSpace = contextState.searchSpace[0];

      let newEmptyToken = false;
      // Detect if we're starting a new context state.
      let contextTokens = contextState.tokens;
      if(contextTokens.length == 0 || contextTokens[contextTokens.length - 1].isNew) {
        if(this.isEmpty(inputTransform) || this.isWhitespace(inputTransform)) {
          newEmptyToken = true;
          prefixTransform = inputTransform;
          context = postContext; // Ensure the whitespace token is preapplied!
        }
      }

      // TODO:  whitespace, backspace filtering.  Do it here.
      //        Whitespace is probably fine, actually.  Less sure about backspace.

      let bestCorrectionCost: number;
      for(let matches of searchSpace.getBestMatches()) {
        // Corrections obtained:  now to predict from them!
        let predictionRoots = matches.map(function(match) {
          let correction = match.matchString;
          
          // Worth considering:  extend Traversal to allow direct prediction lookups?
          // let traversal = match.finalTraversal;

          // Find a proper Transform ID to map the correction to.
          // Without it, we can't apply the suggestion.
          let finalInput: Transform;
          if(match.inputSequence.length > 0) {
            finalInput = match.inputSequence[match.inputSequence.length - 1].sample;
          } else {
            finalInput = inputTransform;  // A fallback measure.  Greatly matters for empty contexts.
          }

          let deleteLeft = 0;
          // remove actual token string.  If new token, there should be nothing to delete.
          if(!newEmptyToken) {
            // If this is triggered from a backspace, make sure to use its results
            // and also include its left-deletions!  It's the one post-input context case.
            if(allowBksp) {
              deleteLeft = this.wordbreak(postContext).kmwLength() + inputTransform.deleteLeft;
            } else {
              // Normal case - use the pre-input context.
              deleteLeft = this.wordbreak(context).kmwLength();
            }
          }

          // Replace the existing context with the correction.
          let correctionTransform: Transform = {
            insert: correction,  // insert correction string
            deleteLeft: deleteLeft, 
            id: inputTransform.id // The correction should always be based on the most recent external transform/transcription ID.
          }

          if(bestCorrectionCost === undefined) {
            bestCorrectionCost = match.totalCost;
          }

          return {
            sample: correctionTransform,
            p: Math.exp(-match.totalCost)
          };
        }, this);

        // Running in bulk over all suggestions, duplicate entries may be possible.
        let predictions = this.predictFromCorrections(predictionRoots, context);
        rawPredictions = rawPredictions.concat(predictions);
        // TODO:  We don't currently de-duplicate predictions at this point quite yet, so
        // it's technically possible that we return too few.

        let correctionCost = matches[0].totalCost;
        if(correctionCost >= bestCorrectionCost + 4) { // e^-4 = 0.0183156388.  Allows "80%" of an extra edit.
          // Very useful for stopping 'sooner' when words reach a sufficient length.
          break;
        } else if(rawPredictions.length >= ModelCompositor.MAX_SUGGESTIONS) {
          // Sort the prediction list; we need them in descending order for the next check.
          rawPredictions.sort(function(a, b) {
            return b.p - a.p;
          });

          // If the best suggestion from the search's current tier fails to beat the worst
          // pending suggestion from previous tiers, assume all further corrections will 
          // similarly fail to win; terminate the search-loop.
          if(rawPredictions[ModelCompositor.MAX_SUGGESTIONS-1].p > Math.exp(-correctionCost)) {
            break;
          }
        } 
      }
    }

    // Section 2 - post-analysis for our generated predictions, managing 'keep'.
    // Assumption:  Duplicated 'displayAs' properties indicate duplicated Suggestions.
    // When true, we can use an 'associative array' to de-duplicate everything.
    let suggestionDistribMap: {[key: string]: ProbabilityMass<Suggestion>} = {};
    let currentCasing: CasingForm = null;
    if(lexicalModel.languageUsesCasing) {
      currentCasing = this.detectCurrentCasing(postContext);
    }

    let baseWord = this.wordbreak(context);

    // Deduplicator + annotator of 'keep' suggestions.
    for(let prediction of rawPredictions) {
      // Combine duplicate samples.
      let displayText = prediction.sample.displayAs;

      if(displayText == keepOptionText || (lexicalModel.toKey && displayText == lexicalModel.toKey(keepOptionText)) ) {
        // Preserve the original, pre-keyed version of the text.
        if(!keepOption) {
          let baseTransform = prediction.sample.transform;

          let keepTransform = {
            insert: keepOptionText,
            deleteLeft: baseTransform.deleteLeft,
            deleteRight: baseTransform.deleteRight,
            id: baseTransform.id
          }

          let intermediateKeep = models.transformToSuggestion(keepTransform, prediction.p);
          keepOption = this.toAnnotatedSuggestion(intermediateKeep, 'keep',  models.QuoteBehavior.noQuotes);
          keepOption.matchesModel = true;

          // Since we replaced the original Suggestion with a keep-annotated one,
          // we must manually preserve the transform ID.
          keepOption.transformId = prediction.sample.transformId;
        } else if(keepOption.p && prediction.p) {
          keepOption.p += prediction.p;
        }
      } else {
        // Apply capitalization rules now; facilitates de-duplication of suggestions
        // that may be caused as a result.
        //
        // Example:  "apple" and "Apple" are separate when 'lower', but identical for 'initial' and 'upper'.
        if(currentCasing && currentCasing != 'lower') {
          this.applySuggestionCasing(prediction.sample, baseWord, currentCasing);
          // update the mapping string, too.
          displayText = prediction.sample.displayAs;
        }

        let existingSuggestion = suggestionDistribMap[displayText];
        if(existingSuggestion) {
          existingSuggestion.p += prediction.p;
        } else {
          suggestionDistribMap[displayText] = prediction;
        }
      }
    }

    // Generate a default 'keep' option if one was not otherwise produced.
    if(!keepOption && keepOptionText != '') {
      let keepTransform = models.transformToSuggestion(inputTransform, 1);  // 1 is a filler value; goes unused b/c is for a 'keep'.
      // This is the one case where the transform doesn't insert the full word; we need to override the displayAs param.
      keepTransform.displayAs = keepOptionText;

      keepOption = this.toAnnotatedSuggestion(keepTransform, 'keep');
      keepOption.matchesModel = false;
    }

    // Section 3:  Finalize suggestions, truncate list to the N (MAX_SUGGESTIONS) most optimal, return.

    // Now that we've calculated a unique set of probability masses, time to make them into a proper
    // distribution and prep for return.
    for(let key in suggestionDistribMap) {
      let pair = suggestionDistribMap[key];
      suggestionDistribution.push(pair);
    }

    suggestionDistribution = suggestionDistribution.sort(function(a, b) {
      return b.p - a.p; // Use descending order - we want the largest probabilty suggestions first!
    });

    let suggestions = suggestionDistribution.splice(0, ModelCompositor.MAX_SUGGESTIONS).map(function(value) {
      if(value.sample['p']) {
        // For analysis / debugging
        value.sample['lexical-p'] =  value.sample['p'];
        value.sample['correction-p'] = value.p / value.sample['p'];
        // Use of the Trie model always exposed the lexical model's probability for a word to KMW.
        // It's useful for debugging right now, so may as well repurpose it as the posterior.
        //
        // We still condition on 'p' existing so that test cases aren't broken.
        value.sample['p'] = value.p;
      }
      //
      return value.sample;
    });

    if(keepOption) {
      suggestions = [ keepOption as Suggestion ].concat(suggestions);
    }

    // Apply 'after word' punctuation and casing (when applicable).  Also, set suggestion IDs.  
    // We delay until now so that utility functions relying on the unmodified Transform may execute properly.

    let compositor = this;
    suggestions.forEach(function(suggestion) {
      // Valid 'keep' suggestions may have zero length; we still need to evaluate the following code
      // for such cases.

      // Do we need to manipulate the suggestion's transform based on the current state of the context?
      if(!context.right) {
        // Only insert wordbreak characters if we're at the end of the context.  
        suggestion.transform.insert += punctuation.insertAfterWord;
      } else {
        // If we're mid-word, delete its original post-caret text.
        const tokenization = compositor.tokenize(context);
        if(tokenization && tokenization.caretSplitsToken) {
          // While we wait on the ability to provide a more 'ideal' solution, let's at least
          // go with a more stable, if slightly less ideal, solution for now.
          // 
          // A predictive text default (on iOS, at least) - immediately wordbreak 
          // on suggestions accepted mid-word.
          suggestion.transform.insert += punctuation.insertAfterWord;
        }
      }

      // If this is a suggestion after wordbreak input, make sure we preserve the wordbreak transform!
      if(prefixTransform) {
        let mergedTransform = models.buildMergedTransform(prefixTransform, suggestion.transform);
        mergedTransform.id = suggestion.transformId;

        // Temporarily and locally drops 'readonly' semantics so that we can reassign the transform.
        // See https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-8.html#improved-control-over-mapped-type-modifiers
        let mutableSuggestion = suggestion as {-readonly [transform in keyof Suggestion]: Suggestion[transform]};
        
        // Assignment via by-reference behavior, as suggestion is an object
        mutableSuggestion.transform = mergedTransform;
      }

      suggestion.id = compositor.SUGGESTION_ID_SEED;
      compositor.SUGGESTION_ID_SEED++;
    });

    // Store the suggestions on the final token of the current context state (if it exists).
    // Or, once phrase-level suggestions are possible, on whichever token serves as each prediction's root.
    if(contextState) {
      contextState.tail.replacements = suggestions.map(function(suggestion) {
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
      revertedPrefix = postContextTokenization.left[postContextTokenization.left.length-1];
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

  applyReversion(reversion: Reversion, context: Context): Suggestion[] {
    // If we are unable to track context (because the model does not support LexiconTraversal),
    // we need a "fallback" strategy.
    let compositor = this;
    let fallbackSuggestions = function() {
      let revertedContext = models.applyTransform(reversion.transform, context);
      let suggestions = compositor.predict({insert: '', deleteLeft: 0}, revertedContext);
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
    // Force-resets the context, throwing out any previous fat-finger data, etc.
    // Designed for use when the caret has been directly moved and/or the context sourced from a different control
    // than before.
    if(this.contextTracker) {
      let tokenizedContext = models.tokenize(this.lexicalModel.wordbreaker || wordBreakers.default, context);
      let contextState = correction.ContextTracker.modelContextState(tokenizedContext.left, this.lexicalModel);
      this.contextTracker.enqueue(contextState);
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

    if(model.applyCasing('lower', text) == text) {
      return 'lower';
    } else if(model.applyCasing('upper', text) == text) {
      // If only a single character has been input, assume we're in 'initial' mode.
      return text.kmwLength() > 1 ? 'upper' : 'initial';
    } else if(model.applyCasing('initial', text) == text) {
      // We check 'initial' last, as upper-case input is indistinguishable.
      return 'initial';
    } else {
      // 'null' is returned when no casing pattern matches the input.
      return null;
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
