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
      /* c8 ignore start */
    } else {
      // 1.  This model does not provide a model following the 14.0+ wordbreaking spec
      // 2.  This model DOES define a custom wordbreaker following the 12.0-13.0 spec.

      // Since the model relies on custom wordbreaking behavior, we need to use the
      // old, deprecated wordbreaking pattern.
      return model.wordbreak(context);
    }
    /* c8 ignore end */
  };
}

export function determineModelTokenizer(model: LexicalModel) {
  return (context: Context) => {
    if(model.wordbreaker) {
      const fullTokenization = models.tokenize(model.wordbreaker, context);

      return {
        left:  fullTokenization.left .filter((entry) => !entry.isWhitespace).map((entry) => entry.text),
        right: fullTokenization.right.filter((entry) => !entry.isWhitespace).map((entry) => entry.text),
        caretSplitsToken: fullTokenization.caretSplitsToken
      }
    } else {
      return null;
    }
  }
}

export function detectCurrentCasing(lexicalModel: LexicalModel, context: Context): CasingForm {
  let model = lexicalModel;
  const wordbreak = determineModelWordbreaker(lexicalModel);

  let text = wordbreak(context);

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