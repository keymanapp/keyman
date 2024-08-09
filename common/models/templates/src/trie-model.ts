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
import { extendString, PriorityQueue } from "@keymanapp/web-utils";
import { default as defaultWordBreaker } from "@keymanapp/models-wordbreakers";

import { applyTransform, SearchKey, transformToSuggestion, Wordform2Key } from "./common.js";
import { Node, Trie } from './trie.js';
import { getLastPreCaretToken } from "./tokenization.js";

extendString();

/**
 * @file trie-model.ts
 *
 * Defines a simple word list (unigram) model.
 */

/** Upper bound on the amount of suggestions to generate. */
const MAX_SUGGESTIONS = 12;

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
export default class TrieModel implements LexicalModel {
  configuration?: Configuration;
  private _trie: Trie;
  readonly breakWords: WordBreakingFunction;
  readonly punctuation?: LexicalModelPunctuation;
  readonly languageUsesCasing?: boolean;

  readonly applyCasing?: CasingFunction;

  constructor(trieData: {root: Node, totalWeight: number}, options: TrieModelOptions = {}) {
    this.languageUsesCasing = options.languageUsesCasing;
    this.applyCasing = options.applyCasing;

    this._trie = new Trie(
      trieData['root'],
      trieData['totalWeight'],
      options.searchTermToKey as Wordform2Key || defaultSearchTermToKey
    );
    this.breakWords = options.wordBreaker || defaultWordBreaker;
    this.punctuation = options.punctuation;
  }

  configure(capabilities: Capabilities): Configuration {
    return this.configuration = {
      leftContextCodePoints: capabilities.maxLeftContextCodePoints,
      rightContextCodePoints: capabilities.maxRightContextCodePoints ?? 0
    };
  }

  toKey(text: USVString): USVString {
    return this._trie.toKey(text);
  }

  predict(transform: Transform, context: Context): Distribution<Suggestion> {
    // Special-case the empty buffer/transform: return the top suggestions.
    if (!transform.insert && !context.left && !context.right && context.startOfBuffer && context.endOfBuffer) {
      return makeDistribution(this.firstN(MAX_SUGGESTIONS).map(({text, p}) => ({
        transform: {
          insert: text,
          deleteLeft: 0
        },
        displayAs: text,
        p: p
      })));
    }

    // Compute the results of the keystroke:
    let newContext = applyTransform(transform, context);

    // Computes the different in word length after applying the transform above.
    let leftDelOffset = transform.deleteLeft - transform.insert.kmwLength();

    // All text to the left of the cursor INCLUDING anything that has
    // just been typed.
    let prefix = getLastPreCaretToken(this.breakWords, newContext);

    // Return suggestions from the trie.
    return makeDistribution(this.lookup(prefix).map(({text, p}) =>
      transformToSuggestion({
        insert: text,
        // Delete whatever the prefix that the user wrote.
        deleteLeft: leftDelOffset + prefix.kmwLength()
        // Note: a separate capitalization/orthography engine can take this
        // result and transform it as needed.
      },
      p
    )));

    /* Helper */

    function makeDistribution(suggestions: WithOutcome<Suggestion>[]): Distribution<Suggestion> {
      let distribution: Distribution<Suggestion> = [];

      for(let s of suggestions) {
        distribution.push({sample: s, p: s.p});
      }

      return distribution;
    }
  }

  get wordbreaker(): WordBreakingFunction {
    return this.breakWords;
  }

  public traverseFromRoot(): LexiconTraversal {
    return this._trie.traverseFromRoot();
  }

  /**
   * Returns the top N suggestions from the trie.
   * @param n How many suggestions, maximum, to return.
   */
  firstN(n: number): TextWithProbability[] {
    return getSortedResults(this._trie.traverseFromRoot(), n);
  }

  /**
   * Lookups an arbitrary prefix (a query) in the trie. Returns the top 3
   * results in sorted order.
   *
   * @param prefix
   */
  lookup(prefix: string): TextWithProbability[] {
    const searchKey = this.toKey(prefix);
    const rootTraversal = this.traverseFromRoot().child(searchKey);

    if(!rootTraversal) {
      return [];
    }

    const directEntries = rootTraversal.entries;
    // `Set` requires Chrome 38+, which is more recent than Chrome 35.
    const directSet: Record<string, string> = {};
    for(const entry of directEntries) {
      directSet[entry.text] = entry.text;
    }

    const bestEntries = getSortedResults(rootTraversal);
    const deduplicated = bestEntries.filter((entry) => !directSet[entry.text]);

    // Any entries directly hosted on the current node should get full display
    // priority over anything from its descendants.
    return directEntries.concat(deduplicated);
  }
};

/////////////////////////////////////////////////////////////////////////////////
// What remains in this file is the trie implementation proper. Note: to       //
// reduce bundle size, any functions/methods related to creating the trie have //
// been removed.                                                               //
/////////////////////////////////////////////////////////////////////////////////

/**
 * The priority queue will always pop the most probable item - be it a Traversal
 * state or a lexical entry reached via Traversal.
 */
type TraversableWithProb = TextWithProbability | LexiconTraversal;

/**
 * Returns all entries matching the given prefix, in descending order of
 * weight.
 *
 * @param prefix  the prefix to match.
 * @param results the current results
 * @param queue
 */
function getSortedResults(traversal: LexiconTraversal, limit = MAX_SUGGESTIONS): TextWithProbability[] {
  let queue = new PriorityQueue(function(a: TraversableWithProb, b: TraversableWithProb) {
    // In case of Trie compilation issues that emit `null` or `undefined`
    return (b ? b.p : 0) - (a ? a.p : 0);
  });
  let results: TextWithProbability[] = [];

  queue.enqueue(traversal);

  while(queue.count > 0) {
    const entry = queue.dequeue();

    if((entry as TextWithProbability)!.text !== undefined) {
      const lexicalEntry = entry as TextWithProbability;
      results.push(lexicalEntry);
      if(results.length >= limit) {
        return results;
      }
    } else {
      const traversal = entry as LexiconTraversal;
      queue.enqueueAll(traversal.entries);
      let children: LexiconTraversal[] = []
      for(let child of traversal.children()) {
        children.push(child.traversal());
      }
      queue.enqueueAll(children);
    }
  }

  return results;
}

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
