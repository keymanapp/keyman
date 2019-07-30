/**
 * Interfaces and constants used by the lexical model compiler. These target
 * the LMLayer's internal worker code, so we provide those definitions too.
 */
/// <reference path="../../../common/predictive-text/worker/worker-compiler-interfaces.d.ts" />

interface ClassBasedWordBreaker {
  allowedCharacters?: { initials?: string, medials?: string, finals?: string } | string,
  defaultBreakCharacter?: string
  sources?: Array<string>;
  /**
   * The name of the type to instantiate (without parameters) as the base object for a custom word-breaking model.
   */
  rootClass?: string
}

interface LexicalModel {
  // TODO: remove trie-2.0 when https://github.com/keymanapp/lexical-models/pull/39 is merged.
  readonly format: 'trie-1.0'|'trie-2.0'|'fst-foma-1.0'|'custom-1.0',
  //... metadata ...
}

interface LexicalModelPrediction {
  display?: string;
  transform: string;
  delete: number;
}

interface LexicalModelSource extends LexicalModel {
  readonly sources: Array<string>;
  /**
   * The name of the type to instantiate (without parameters) as the base object for a custom predictive model.
   */
  readonly rootClass?: string
  /**
   * What kind of word breaking to use, if any.
   */
  readonly wordBreaking?: 'default' | 'ascii' | 'placeholder' | ClassBasedWordBreaker;
  /**
   * How to simplify words, to convert them into simplifired search keys
   * This often involves removing accents, lowercasing, etc.
   */
  readonly searchTermToKey?: (term: string) => string;

  /**
   * Punctuation and spacing suggested by the model.
   *
   * @see LexicalModelPunctuation
   */
  readonly punctuation?: LexicalModelPunctuation;
}

interface LexicalModelCompiled extends LexicalModel {
  readonly id: string;
}

interface LexicalModelCompiledTrie extends LexicalModelCompiled {
  trie: string;
}

interface LexicalModelCompiledFst extends LexicalModelCompiled {
  fst: string;
}

interface LexicalModelCompiledCustom extends LexicalModelCompiled {
}
