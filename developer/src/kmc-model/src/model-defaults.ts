import { LexicalModelTypes } from '@keymanapp/common-types';
import CasingForm = LexicalModelTypes.CasingForm;
import CasingFunction = LexicalModelTypes.CasingFunction;

/**
 * Converts wordforms into an indexable form. It does this by
 * normalizing the letter case of characters INDIVIDUALLY (to disregard
 * context-sensitive case transformations), normalizing to NFKD form,
 * and removing common diacritical marks.
 *
 * This is a very speculative implementation, that might work with
 * your language. We don't guarantee that this will be perfect for your
 * language, but it's a start.
 *
 * This uses String.prototype.normalize() to convert normalize into NFKD.
 * NFKD neutralizes some funky distinctions, e.g., ꬲ, ｅ, e should all be the
 * same character; plus, it's an easy way to separate a Latin character from
 * its diacritics; Even then, orthographies regularly use code points
 * that, under NFKD normalization, do NOT decompose appropriately for your
 * language (e.g., SENĆOŦEN, Plains Cree in syllabics).
 *
 * Use this in early iterations of the model. For a production lexical model,
 * you will probably write/generate your own key function, tailored to your
 * language. There is a chance the default will work properly out of the box.
 */
export function defaultSearchTermToKey(wordform: string): string {
  return wordform
      .normalize('NFKD')
      // Remove any combining diacritics (if input is in NFKD)
      .replace(/[\u0300-\u036F]/g, '')
      // Replace directional quotation marks with plain apostrophes
      .replace(/[‘’]/g, "'")
      // Also double-quote marks.
      .replace(/[“”]/g, '"');
}

/**
 * Converts wordforms into an indexable form. It does this by
 * normalizing the letter case of characters INDIVIDUALLY (to disregard
 * context-sensitive case transformations), normalizing to NFKD form,
 * and removing common diacritical marks.
 *
 * This is a very speculative implementation, that might work with
 * your language. We don't guarantee that this will be perfect for your
 * language, but it's a start.
 *
 * This uses String.prototype.normalize() to convert normalize into NFKD.
 * NFKD neutralizes some funky distinctions, e.g., ꬲ, ｅ, e should all be the
 * same character; plus, it's an easy way to separate a Latin character from
 * its diacritics; Even then, orthographies regularly use code points
 * that, under NFKD normalization, do NOT decompose appropriately for your
 * language (e.g., SENĆOŦEN, Plains Cree in syllabics).
 *
 * Use this in early iterations of the model. For a production lexical model,
 * you will probably write/generate your own key function, tailored to your
 * language. There is a chance the default will work properly out of the box.
 */
export function defaultCasedSearchTermToKey(wordform: string, applyCasing: CasingFunction): string {
  // While this is a bit WET, as the basic `defaultSearchTermToKey` exists and performs some of
  // the same functions, repetition is the easiest way to allow the function to be safely compiled
  // with ease by use of `.toString()`.
  return Array.from(wordform
        .normalize('NFKD')
        // Remove any combining diacritics (if input is in NFKD)
        .replace(/[\u0300-\u036F]/g, '')
      ) // end of `Array.from`
      .map(function(c) { return applyCasing('lower', c)})
      .join('')
      // Replace directional quotation marks with plain apostrophes
      .replace(/[‘’]/g, "'")
      // Also double-quote marks.
      .replace(/[“”]/g, '"');
}

/**
 * Specifies default casing behavior for lexical models when `languageUsesCasing` is
 * set to true.
 * @param casing One of 'lower' (lowercased), 'upper' (uppercased), or 'initial'.
 *
 * 'initial' is designed to cover cases like sentence-initial & proper noun capitalization in English.
 * This may be overwritten as appropriate in model-specific implementations.
 * @param text The text to be modified.
 */
export function defaultApplyCasing(casing: CasingForm, text: string): string {
  switch(casing) {
    case 'lower':
      return text.toLowerCase();
    case 'upper':
      return text.toUpperCase();
    case 'initial':
      var headCode = text.charCodeAt(0);
      // The length of the first code unit, as measured in code points.
      var headUnitLength = 1;

      // Is the first character a high surrogate, indicating possible use of UTF-16
      // surrogate pairs?  Also, is the string long enough for there to BE a pair?
      if(text.length > 1 && headCode >= 0xD800 && headCode <= 0xDBFF) {
        // It's possible, so now we check for low surrogates.
        var lowSurrogateCode = text.charCodeAt(1);

        if(lowSurrogateCode >= 0xDC00 && lowSurrogateCode <= 0xDFFF) {
          // We have a surrogate pair; this pair is the 'first' character.
          headUnitLength++;
        }
      }

      // Capitalizes the first code unit of the string, leaving the rest intact.
      return text.substring(0, headUnitLength).toUpperCase() // head - uppercased
             .concat(text.substring(headUnitLength));        // tail - lowercased
  }
}
