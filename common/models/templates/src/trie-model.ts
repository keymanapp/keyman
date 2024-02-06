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
import { default as defaultWordBreaker } from "@keymanapp/models-wordbreakers";

import { applyTransform, isHighSurrogate, isSentinel, SENTINEL_CODE_UNIT, transformToSuggestion } from "./common.js";
import { getLastPreCaretToken } from "./tokenization.js";
import PriorityQueue from "./priority-queue.js";

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
 * Used to determine the probability of an entry from the trie.
 */
type TextWithProbability = {
  text: string;
  // TODO: use negative-log scaling instead?
  p: number; // real-number weight, from 0 to 1
}

class Traversal implements LexiconTraversal {
  /**
   * The lexical prefix corresponding to the current traversal state.
   */
  prefix: String;

  /**
   * The current traversal node.  Serves as the 'root' of its own sub-Trie,
   * and we cannot navigate back to its parent.
   */
  root: Node;

  constructor(root: Node, prefix: string) {
    this.root = root;
    this.prefix = prefix;
  }

  *children(): Generator<{char: string, traversal: () => LexiconTraversal}> {
    let root = this.root;

    if(root.type == 'internal') {
      for(let entry of root.values) {
        let entryNode = root.children[entry];

        // UTF-16 astral plane check.
        if(isHighSurrogate(entry)) {
          // First code unit of a UTF-16 code point.
          // For now, we'll just assume the second always completes such a char.
          //
          // Note:  Things get nasty here if this is only sometimes true; in the future,
          // we should compile-time enforce that this assumption is always true if possible.
          if(entryNode.type == 'internal') {
            let internalNode = entryNode;
            for(let lowSurrogate of internalNode.values) {
              let prefix = this.prefix + entry + lowSurrogate;
              yield {
                char: entry + lowSurrogate,
                traversal: function() { return new Traversal(internalNode.children[lowSurrogate], prefix) }
              }
            }
          } else {
            // Determine how much of the 'leaf' entry has no Trie nodes, emulate them.
            let fullText = entryNode.entries[0].key;
            entry = entry + fullText[this.prefix.length + 1]; // The other half of the non-BMP char.
            let prefix = this.prefix + entry;

            yield {
              char: entry,
              traversal: function () {return new Traversal(entryNode, prefix)}
            }
          }
        } else if(isSentinel(entry)) {
          continue;
        } else if(!entry) {
          // Prevent any accidental 'null' or 'undefined' entries from having an effect.
          continue;
        } else {
          let prefix = this.prefix + entry;
          yield {
            char: entry,
            traversal: function() { return new Traversal(entryNode, prefix)}
          }
        }
      }

      return;
    } else { // type == 'leaf'
      let prefix = this.prefix;

      let children = root.entries.filter(function(entry) {
        return entry.key != prefix && prefix.length < entry.key.length;
      })

      for(let {key} of children) {
        let nodeKey = key[prefix.length];

        if(isHighSurrogate(nodeKey)) {
          // Merge the other half of an SMP char in!
          nodeKey = nodeKey + key[prefix.length+1];
        }
        yield {
          char: nodeKey,
          traversal: function() { return new Traversal(root, prefix + nodeKey)}
        }
      };
      return;
    }
  }

  get entries(): string[] {
    if(this.root.type == 'leaf') {
      let prefix = this.prefix;
      let matches = this.root.entries.filter(function(entry) {
        return entry.key == prefix;
      });

      return matches.map(function(value) { return value.content });
    } else {
      let matchingLeaf = this.root.children[SENTINEL_CODE_UNIT];
      if(matchingLeaf && matchingLeaf.type == 'leaf') {
        return matchingLeaf.entries.map(function(value) { return value.content });
      } else {
        return [];
      }
    }
  }
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
      return makeDistribution(this._trie.firstN(MAX_SUGGESTIONS).map(({text, p}) => ({
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
    return makeDistribution(this._trie.lookup(prefix).map(({text, p}) =>
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
    return new Traversal(this._trie['root'], '');
  }
};

/////////////////////////////////////////////////////////////////////////////////
// What remains in this file is the trie implementation proper. Note: to       //
// reduce bundle size, any functions/methods related to creating the trie have //
// been removed.                                                               //
/////////////////////////////////////////////////////////////////////////////////

/**
 * An **opaque** type for a string that is exclusively used as a search key in
 * the trie. There should be a function that converts arbitrary strings
 * (queries) and converts them into a standard search key for a given language
 * model.
 *
 * Fun fact: This opaque type has ALREADY saved my bacon and found a bug!
 */
type SearchKey = string & { _: 'SearchKey'};

/**
 * The priority queue will always pop the most weighted item. There can only
 * be two kinds of items right now: nodes, and entries; both having a weight
 * attribute.
 */
type Weighted = Node | Entry;

/**
 * A function that converts a string (word form or query) into a search key
 * (secretly, this is also a string).
 */
interface Wordform2Key {
  (wordform: string): SearchKey;
}

// The following trie implementation has been (heavily) derived from trie-ing
// by Conrad Irwin.
// trie-ing is copyright (C) 2015–2017 Conrad Irwin.
// Distributed under the terms of the MIT license:
// https://github.com/ConradIrwin/trie-ing/blob/df55d7af7068d357829db9e0a7faa8a38add1d1d/LICENSE

type Node = InternalNode | Leaf;
/**
 * An internal node in the trie. Internal nodes NEVER contain entries; if an
 * internal node should contain an entry, then it has a dummy leaf node (see
 * below), that can be accessed by node.children["\uFDD0"].
 */
interface InternalNode {
  type: 'internal';
  weight: number;
  /** Maintains the keys of children in descending order of weight. */
  values: string[]; // TODO: As an optimization, "values" can be a single string!
  /**
   * Maps a single UTF-16 code unit to a child node in the trie. This child
   * node may be a leaf or an internal node. The keys of this object are kept
   * in sorted order in the .values array.
   */
  children: { [codeunit: string]: Node };
}
/** Only leaf nodes actually contain entries (i.e., the words proper). */
interface Leaf {
  type: 'leaf';
  weight: number;
  entries: Entry[];
}

/**
 * An entry in the prefix trie (stored in leaf nodes exclusively!)
 */
interface Entry {
  /** The actual word form, stored in the trie. */
  content: string;
  /** A search key that usually simplifies the word form, for ease of search. */
  key: SearchKey;
  weight: number;
}

/**
 * Wrapper class for the trie and its nodes.
 */
class Trie {
  private root: Node;
  /** The total weight of the entire trie. */
  private totalWeight: number;
  /**
   * Converts arbitrary strings to a search key. The trie is built up of
   * search keys; not each entry's word form!
   */
  toKey: Wordform2Key;

  constructor(root: Node, totalWeight: number, wordform2key: Wordform2Key) {
    this.root = root;
    this.toKey = wordform2key;
    this.totalWeight = totalWeight;
  }

  /**
   * Lookups an arbitrary prefix (a query) in the trie. Returns the top 3
   * results in sorted order.
   *
   * @param prefix
   */
  lookup(prefix: string): TextWithProbability[] {
    let searchKey = this.toKey(prefix);
    let lowestCommonNode = findPrefix(this.root, searchKey);
    if (lowestCommonNode === null) {
      return [];
    }

    return getSortedResults(lowestCommonNode, searchKey, this.totalWeight);
  }

  /**
   * Returns the top N suggestions from the trie.
   * @param n How many suggestions, maximum, to return.
   */
  firstN(n: number): TextWithProbability[] {
    return getSortedResults(this.root, '' as SearchKey, this.totalWeight, n);
  }
}

/**
 * Finds the deepest descendent in the trie with the given prefix key.
 *
 * This means that a search in the trie for a given prefix has a best-case
 * complexity of O(m) where m is the length of the prefix.
 *
 * @param key The prefix to search for.
 * @param index The index in the prefix. Initially 0.
 */
function findPrefix(node: Node, key: SearchKey, index: number = 0): Node | null {
  // An important note - the Trie itself is built on a per-JS-character basis,
  // not on a UTF-8 character-code basis.
  if (node.type === 'leaf' || index === key.length) {
    return node;
  }

  // So, for SMP models, we need to match each char of the supplementary pair
  // in sequence.  Each has its own node in the Trie.
  let char = key[index];
  if (node.children[char]) {
    return findPrefix(node.children[char], key, index + 1);
  }

  return null;
}

/**
 * Returns all entries matching the given prefix, in descending order of
 * weight.
 *
 * @param prefix  the prefix to match.
 * @param results the current results
 * @param queue
 */
function getSortedResults(node: Node, prefix: SearchKey, N: number, limit = MAX_SUGGESTIONS): TextWithProbability[] {
  let queue = new PriorityQueue(function(a: Weighted, b: Weighted) {
    // In case of Trie compilation issues that emit `null` or `undefined`
    return (b ? b.weight : 0) - (a ? a.weight : 0);
  });
  let results: TextWithProbability[] = [];

  if (node.type === 'leaf') {
    // Assuming the values are sorted, we can just add all of the values in the
    // leaf, until we reach the limit.
    for (let item of node.entries) {
      // String.startsWith is not supported on certain Android (5.0) devices we wish to support.
      // Requires a minimum of Chrome 36, as opposed to 5.0's default of 35.
      if (item.key.indexOf(prefix) == 0) {
        let { content, weight } = item;
        results.push({
          text: content,
          p: weight / N
        });

        if (results.length >= limit) {
          return results;
        }
      }
    }
  } else {
    queue.enqueue(node);
    let next: Weighted | undefined;

    while (next = queue.dequeue()) {
      if (isNode(next)) {
        // When a node is next up in the queue, that means that next least
        // likely suggestion is among its decsendants.
        // So we search all of its descendants!
        if (next.type === 'leaf') {
          queue.enqueueAll(next.entries);
        } else {
          // XXX: alias `next` so that TypeScript can be SURE that internal is
          // in fact an internal node. Because of the callback binding to the
          // original definition of node (i.e., a Node | Entry), this will not
          // type-check otherwise.
          let internal = next;
          queue.enqueueAll(next.values.map(char => {
            return internal.children[char];
          }));
        }
      } else {
        // When an entry is up next in the queue, we just add its contents to
        // the results!
        results.push({
          text: next.content,
          p: next.weight / N
        });
        if (results.length >= limit) {
          return results;
        }
      }
    }
  }
  return results;

}

/** TypeScript type guard that returns whether the thing is a Node. */
function isNode(x: Entry | Node): x is Node {
  return 'type' in x;
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
