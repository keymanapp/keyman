/*
 * Copyright (c) 2019 National Research Council Canada (author: Eddie A. Santos)
 * Copyright (c) 2019 SIL International
 * Copyright (c) 2015–2017 Conrad Irwin
 * Copyright (c) 2011–2015 Marc Campbell
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

// Worth noting:  we're starting to get quite a 'library' of common model/LMLayer functionality.
// Should probably make a 'lm-utils' submodule.

// Allows the kmwstring bindings to resolve.
import { extendString } from "@keymanapp/web-utils";

import { CasingFunction, Configuration, LexicalModel, LexicalModelPunctuation, WordBreakingFunction } from '@keymanapp/common-types';
import { SearchKey, Wordform2Key } from "./common.js";
import { Node, Trie } from './trie.js';
import { TraversalModel } from './traversal-model.js';

extendString();

/**
 * @file trie-model.ts
 *
 * Defines a simple word list (unigram) model.
 */

/**
 * Additional arguments to pass into the model, in addition to the model
 * parameters themselves.
 */
export interface TrieModelOptions {
  /**
   * How to break words in a phrase.
   */
  wordBreaker?: WordBreakingFunction;
  /**
   *  This should simplify a search term into a key.
   */
  searchTermToKey?: (searchTerm: string) => string;

  /**
   * Indicates that the model's written form has 'casing' behavior.
   */
  languageUsesCasing?: boolean;

  /**
   * Specifies a function used to apply the language's casing rules to a word.
   */
  applyCasing?: CasingFunction;

  /**
   * Any punctuation to expose to the user.
   */
  punctuation?: LexicalModelPunctuation;
}

/**
 * @class TrieModel
 *
 * Defines a trie-backed word list model, or the unigram model.
 * Unigram models throw away all preceding words, and search
 * for the next word exclusively. As such, they can perform simple
 * prefix searches within words, however they are not very good
 * at predicting the next word.
 */
export default class TrieModel extends TraversalModel implements LexicalModel {
  configuration?: Configuration;
  public readonly trie: Trie;
  readonly punctuation?: LexicalModelPunctuation;
  readonly languageUsesCasing?: boolean;

  readonly applyCasing?: CasingFunction;

  constructor(trieData: {root: Node, totalWeight: number}, options: TrieModelOptions = {}) {
    const trie = new Trie(
      trieData['root'],
      trieData['totalWeight'],
      options.searchTermToKey as Wordform2Key || defaultSearchTermToKey
    );

    super(trie.traverseFromRoot(), options);
    this.trie = trie;
  }
};

/**
 * Converts wordforms into an indexable form. It does this by
 * normalizing into NFD, removing diacritics, and then converting
 * the result to lowercase.
 *
 * This is a very naïve implementation, that I only think will work on
 * some languages that use the Latin script. As of 2020-04-08, only
 * 4 out of 11 (36%) of published language models use the Latin script,
 * so this might not actually be a great default.
 *
 * This uses String.prototype.normalize() to convert normalize into NFD.
 * NFD is an easy way to separate a Latin character from its diacritics;
 * Even then, some Latin-based orthographies use code points that,
 * under NFD normalization, do NOT decompose into an ASCII letter and a
 * combining diacritical mark (e.g., SENĆOŦEN).
 *
 * Use this only in early iterations of the model. For a production lexical
 * model, you SHOULD write/generate your own key function, tailored to your
 * language.
 */
function defaultSearchTermToKey(wordform: string): SearchKey {
  /**
   * N.B.: this is (slightly) DIFFERENT than the version in
   * keymanapp/lexical-model-compiler/build-trie
   * as this is for compatibility for models built
   * BEFORE the searchTermToKey function was bundled with
   * all models.
   *
   * This compatibility version lowercases AFTER removing diacritics;
   * the new version (bundled in future models) lowercases,
   * NFD normalizes, THEN removes diacritics.
   */
  return wordform
    .normalize('NFD')
    // Remove all combining diacritics (if input is in NFD)
    // common to Latin-orthographies.
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase() as SearchKey;
}
