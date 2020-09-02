/// <reference path="../node_modules/@keymanapp/models-templates/src/index.ts" />
/// <reference path="correction/context-tracker.ts" />

class ModelCompositor {
  private lexicalModel: LexicalModel;
  private contextTracker?: correction.ContextTracker;
  private static readonly MAX_SUGGESTIONS = 12;
  private readonly punctuation: LexicalModelPunctuation;

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

  predict(transformDistribution: Transform | Distribution<Transform>, context: Context): Suggestion[] {
    let suggestionDistribution: Distribution<Suggestion> = [];
    let punctuation = this.punctuation;

    // Assumption:  Duplicated 'displayAs' properties indicate duplicated Suggestions.
    // When true, we can use an 'associative array' to de-duplicate everything.
    let suggestionDistribMap: {[key: string]: ProbabilityMass<Suggestion>} = {};

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
    let keepOptionText = this.lexicalModel.wordbreak(postContext);
    let keepOption: Suggestion = null;

    if(!this.contextTracker) {
      // TODO:  base-case:  pure predict, no correct.
    } else { // TODO:  whitespace, backspace filtering.
      let contextState = this.contextTracker.analyzeState(this.lexicalModel, postContext, !this.isEmpty(inputTransform) ? transformDistribution : null);
      let searchSpace = contextState.searchSpace;

      // Now... run that search!
    }

    for(let alt of transformDistribution) {
      let transform = alt.sample;

      // Filter out special keys unless they're expected.
      if(this.isWhitespace(transform) && !allowSpace) {
        continue;
      } else if(this.isBackspace(transform) && !allowBksp) {
        continue;
      }

      let preserveWhitespace: boolean = false;
      if(this.isWhitespace(transform)) {
        // Detect start of new word; prevent whitespace loss here.
        let postContext = models.applyTransform(transform, context);
        preserveWhitespace = (this.lexicalModel.wordbreak(postContext) == '');
      }

      let distribution = this.lexicalModel.predict(transform, context);

      distribution.forEach(function(pair: ProbabilityMass<Suggestion>) {
        // Let's not rely on the model to copy transform IDs.
        // Only bother is there IS an ID to copy.
        if(transform.id !== undefined) {
          pair.sample.transformId = transform.id;
        }

        // Prepends the original whitespace, ensuring it is preserved if
        // the suggestion is accepted.
        if(preserveWhitespace) {
          models.prependTransform(pair.sample.transform, transform);
        }
        
        // The model is trying to add a word; thus, add some custom formatting
        // to that word.
        if (pair.sample.transform.insert.length > 0) {
          pair.sample.transform.insert += punctuation.insertAfterWord;
        }

        // Combine duplicate samples.
        let displayText = pair.sample.displayAs;

        if(displayText == keepOptionText) {
          keepOption = pair.sample;
          // Specifying 'keep' helps uses of the LMLayer find it quickly
          // if/when desired.
          keepOption.tag = 'keep';
        } else {
          let existingSuggestion = suggestionDistribMap[displayText];
          if(existingSuggestion) {
            existingSuggestion.p += pair.p * alt.p;
          } else {
            let compositedPair = {sample: pair.sample, p: pair.p * alt.p};
            suggestionDistribMap[displayText] = compositedPair;
          }
        }
      });
    }

    // Generate a default 'keep' option if one was not otherwise produced.
    if(!keepOption && keepOptionText != '') {
      keepOption = {
        displayAs: keepOptionText,
        transformId: inputTransform.id,
        // Replicate the original transform, modified for appropriate language insertion syntax.
        transform: {
          insert: inputTransform.insert + punctuation.insertAfterWord,
          deleteLeft: inputTransform.deleteLeft,
          deleteRight: inputTransform.deleteRight,
          id: inputTransform.id
        },
        tag: 'keep'
      };
    }

    // Add the surrounding quotes to the "keep" option's display string:
    if (keepOption) {
      let { open, close } = punctuation.quotesForKeepSuggestion;
      
      // Should we also ensure that we're using the default quote marks first?
      // Or is it reasonable to say that the "left" mark is always the one
      // called "open"?
      if(!punctuation.isRTL) {
        keepOption.displayAs = open + keepOption.displayAs + close;
      } else {
        keepOption.displayAs = close + keepOption.displayAs + open;
      }
    }

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
      return value.sample;
    });

    if(keepOption) {
      suggestions = [ keepOption ].concat(suggestions);
    }

    return suggestions;
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
}

/**
 * The default punctuation and spacing produced by the model.
 */
const DEFAULT_PUNCTUATION: LexicalModelPunctuation = {
  quotesForKeepSuggestion: { open: `“`, close: `”`},
  insertAfterWord: " " ,
};
