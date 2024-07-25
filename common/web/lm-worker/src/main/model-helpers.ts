import * as models from '@keymanapp/models-templates';
import * as wordBreakers from '@keymanapp/models-wordbreakers';

/**
 * The default punctuation and spacing produced by the model.
 */
const DEFAULT_PUNCTUATION: LexicalModelPunctuation = {
  quotesForKeepSuggestion: { open: `“`, close: `”`},
  insertAfterWord: " " ,
};

/**
 * Returns the punctuation used for this model, filling out unspecified fields
 */
export function determinePunctuationFromModel(model: LexicalModel): LexicalModelPunctuation {
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
  };
}

export function determineModelWordbreaker(model: LexicalModel): (context: Context) => string {
  return (context: Context) => {
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
  };
}

export function determineModelTokenizer(model: LexicalModel) {
  return (context: Context) => {
    if(model.wordbreaker) {
      return models.tokenize(model.wordbreaker, context);
    } else {
      return null;
    }
  }
}
