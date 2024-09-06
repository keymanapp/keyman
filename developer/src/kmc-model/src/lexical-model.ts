/**
 * Interfaces and constants used by the lexical model compiler. These target
 * the LMLayer's internal worker code, so we provide those definitions too.
 */

import { CasingFunction, LexicalModelPunctuation, WordBreakingFunction } from '@keymanapp/common-types';

export interface LexicalModelDeclaration {
  readonly format: 'trie-1.0'|'fst-foma-1.0'|'custom-1.0',
  //... metadata ...
}

/**
 * @public
 * Keyman 14.0+ word breaker specification:
 *
 * Can support all old word breaking specification,
 * but can also be extended with options.
 *
 * @since 14.0
 */
 export interface WordBreakerSpec {
  readonly use: SimpleWordBreakerSpec;
  /**
   * If present, joins words that were split by the word breaker
   * together at the given strings. e.g.,
   *
   *    joinWordsAt: ['-'] // to keep hyphenated items together
   *
   * @since 14.0
   */
  readonly joinWordsAt?: string[];

  /**
   * Overrides word splitting behaviour for certain scripts.
   * For example, specifing that spaces break words in certain South-East
   * Asian scripts that otherwise do not use spaces.
   *
   * @since 14.0
   */
  readonly overrideScriptDefaults?: OverrideScriptDefaults;
}

/**
 * @public
 * Simplified word breaker specification.
 *
 * @since 11.0
 */
 export type SimpleWordBreakerSpec = 'default' | 'ascii' | WordBreakingFunction;

/**
 * @public
 * Simplifies input text to facilitate finding entries within a lexical model's
 * lexicon.
 * @since 11.0
 */
 export type SimpleWordformToKeySpec = (term: string) => string;

/**
 * @public
 * Simplifies input text to facilitate finding entries within a lexical model's
 * lexicon, using the model's `applyCasing` function to assist in the keying process.
 * @since 14.0
 */
export type CasedWordformToKeySpec = (term: string, applyCasing?: CasingFunction) => string;

/**
 * @public
 * Simplifies input text to facilitate finding entries within a lexical model's
 * lexicon.
 */
 export type WordformToKeySpec = SimpleWordformToKeySpec | CasedWordformToKeySpec;

/**
 * Override the default word breaking behaviour for some scripts.
 *
 * There is currently only one option:
 *
 * 'break-words-at-spaces'
 * : some South-East Asian scripts conventionally do not use space or any
 * explicit word boundary character to write word breaks. These scripts are:
 *
 *   * Burmese
 *   * Khmer
 *   * Thai
 *   * Laos
 *
 * (this list may be incomplete and extended in the future)
 *
 * For these scripts, the default word breaker breaks at **every**
 * letter/syllable/ideograph. However, in languages that use these scripts BUT
 * use spaces (or some other delimier) as word breaks, enable
 * 'break-words-at-spaces'; enabling 'break-words-at-spaces' prevents the word
 * breaker from making too many breaks in these scripts.
 *
 * @since 14.0
 */
export type OverrideScriptDefaults = 'break-words-at-spaces';


/**
 * @public
 * Base interface for a lexical model source definition
 */
export interface LexicalModelSource extends LexicalModelDeclaration {
  readonly sources: Array<string>;
  /**
   * The name of the type to instantiate (without parameters) as the base object for a custom predictive model.
   */
  readonly rootClass?: string

  /**
   * When set to `true`, suggestions will attempt to match the case of the input text even if
   * the lexicon entries use a different casing scheme due to search term keying effects.
   * @since 14.0
   */
  readonly languageUsesCasing?: boolean

  /**
   * Specifies the casing rules for a language.  Should implement three casing forms:
   * - 'lower' -- a fully-lowercased version of the text appropriate for the language's
   *   use of the writing system.
   * - 'upper' -- a fully-uppercased version of the text
   * - 'initial' -- a version preserving the input casing aside from the initial character,
   *   which is uppercased (like with proper nouns and sentence-initial words in English
   *   sentences.)
   *
   * This is only utilized if `languageUsesCasing` is defined and set to `true`.
   * @since 14.0
   */
  readonly applyCasing?: CasingFunction

  /**
   * Which word breaker to use. Choose from:
   *
   *  - 'default' -- breaks according to Unicode UAX #29 §4.1 Default Word
   *    Boundary Specification, which works well for *most* languages.
   *  - 'ascii' -- a very simple word breaker, for demonstration purposes only.
   *  - word breaking function -- provide your own function that breaks words.
   *  - class-based word-breaker - may be supported in the future.
   */
  readonly wordBreaker?: WordBreakerSpec | SimpleWordBreakerSpec;

  /**
   * How to simplify words, to convert them into simplified search keys
   * This often involves removing accents, lowercasing, etc.
   */
  readonly searchTermToKey?: WordformToKeySpec;

  /**
   * Punctuation and spacing suggested by the model.
   *
   * @see LexicalModelPunctuation
   */
  readonly punctuation?: LexicalModelPunctuation;
}

export interface LexicalModelCompiled extends LexicalModelDeclaration {
  readonly id: string;
}
