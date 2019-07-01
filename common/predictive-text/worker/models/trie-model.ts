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

/// <reference path="../word_breaking/placeholder-word-breaker.ts" />
/// <reference path="common.ts" />

/**
 * @file trie-model.ts
 *
 * Defines a simple word list (unigram) model.
 */

 namespace models {
  /** Upper bound on the amount of suggestions to generate. */
  const MAX_SUGGESTIONS = 12;

  /**
   * Additional arguments to pass into the model, in addition to the model
   * parameters themselves.
   */
  interface TrieModelOptions {
    /**
     * How to break words in a phrase.
     */
    wordBreaker?: WordBreakingFunction;
    /**
     *  This should simplify a search term into a key.
     */
    searchTermToKey?: (searchTerm: string) => string;
  }

  /**
   * Used to determine the probability of an entry from the trie.
   */
  type TextWithProbability = {
    text: string;
    // TODO: use negative-log scaling instead?
    p: number; // real-number weight, from 0 to 1
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
  export class TrieModel implements WorkerInternalModel {
    configuration: Configuration;
    private _trie: Trie;
    readonly breakWords: WordBreakingFunction;

    constructor(trieData: object, options: TrieModelOptions = {}) {
      this._trie = new Trie(
        trieData['root'],
        trieData['totalWeight'],
        options.searchTermToKey as Wordform2Key || defaultWordform2Key
      );
      this.breakWords = options.wordBreaker || wordBreakers.placeholder;
    }

    configure(capabilities: Capabilities): Configuration {
      return this.configuration = {
        leftContextCodeUnits: capabilities.maxLeftContextCodeUnits,
        rightContextCodeUnits: capabilities.maxRightContextCodeUnits
      };
    }

    predict(transform: Transform, context: Context): Distribution<Suggestion> {
      // Special-case the empty buffer/transform: return the top suggestions.
      if (!transform.insert && context.startOfBuffer && context.endOfBuffer) {
        return makeDistribution(this._trie.firstN(MAX_SUGGESTIONS).map(({text, p}) => ({
          transform: {
            insert: text + ' ', // TODO: do NOT add the space here!
            deleteLeft: 0
          },
          displayAs: text,
          p: p
        })));
      }

      // Compute the results of the keystroke:
      let newContext = Common.applyTransform(transform, context);

      // Computes the different in word length after applying the transform above.
      let leftDelOffset = transform.deleteLeft - transform.insert.length;

      // All text to the left of the cursor INCLUDING anything that has
      // just been typed.
      let prefix = this.getLastWord(newContext.left);

      // Return suggestions from the trie.
      return makeDistribution(this._trie.lookup(prefix).map(({text, p}) => ({
        transform: {
          // Insert the suggestion from the Trie, verbatim
          insert: text + ' ',  // TODO: append space at a higher-level
          // Delete whatever the prefix that the user wrote.
          // Note: a separate capitalization/orthography engine can take this
          // result and transform it as needed.
          deleteLeft: leftDelOffset + prefix.length,
        },
        displayAs: text,
        p: p
      })));

      /* Helper */

      function makeDistribution(suggestions: (Suggestion & {p: number})[]): Distribution<Suggestion> {
        let distribution: Distribution<Suggestion> = [];

        for(let s of suggestions) {
          distribution.push({sample: s, p: s.p});
        }

        return distribution;
      }
    }

    /**
     * Get the last word of the phrase, or nothing.
     * @param fullLeftContext the entire left context of the string.
     */
    private getLastWord(fullLeftContext: string): string {
      let words = this.breakWords(fullLeftContext)
      if (words.length > 0) {
        return words.pop().text;
      }

      return '';
    }

    public wordbreak(context: Context): USVString {
      return this.getLastWord(context.left);
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
    if (node.type === 'leaf' || index === key.length) {
      return node;
    }

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
    let queue = new PriorityQueue();
    let results: TextWithProbability[] = [];

    if (node.type === 'leaf') {
      // Assuming the values are sorted, we can just add all of the values in the
      // leaf, until we reach the limit.
      for (let item of node.entries) {
        if (item.key.startsWith(prefix)) {
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
      let next: Weighted;

      while (next = queue.pop()) {
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
   * A priority queue that always pops the highest weighted item.
   */
  class PriorityQueue {
    // TODO: This probable should use a max-heap implementation, but I'm just doing
    // a O(n log n) sort of the array when an item is popped.
    private _storage: Weighted[] = [];
    // TODO: this should have a limit, and ensure small values are not added.

    /**
     * Enqueues a single element to the priority queue.
     */
    enqueue(element: Weighted) {
      this._storage.push(element);
    }

    /**
     * Adds an array of weighted elements to the priority queue.
     */
    enqueueAll(elements: Weighted[]) {
      this._storage = this._storage.concat(elements);
    }

    /**
     * Pops the highest weighted item in the queue.
     */
    pop(): Weighted {
      // Lazily sort only when NEEDED.
      // Sort in descending order of weight, so heaviest weight will be popped
      // first.
      this._storage.sort((a, b) => b.weight - a.weight);
      return this._storage.shift();
    }
  }

  /**
   * Converts word forms in into an indexable form. It does this by converting
   * the string to uppercase and trying to remove diacritical marks.
   *
   * This is a very naïve implementation, that I've only though to work on
   * languages that use the Latin script. Even then, some Latin-based
   * orthographies use code points that, under NFD normalization, do NOT
   * decompose into an ASCII letter and a combining diacritical mark (e.g.,
   * SENĆOŦEN).
   *
   * Use this only in early iterations of the model. For a production lexical
   * model, you SHOULD write/generate your own key function, tailored to your
   * language.
   */
  function defaultWordform2Key(wordform: string): SearchKey {
    return wordform
      .replace(/[\u0100-\u2200]/g, function (c) {
        if (c in PARTIAL_NFD_LOOKUP) {
          return PARTIAL_NFD_LOOKUP[c];
        }
        return c;
      })
      .toLowerCase() as SearchKey;
  }

  /**
   * String.prototype.normalize() is not available on all platforms (*cough* IE
   * 11 *cough cough*). We want to use NFD to take off diacritical marks from
   * characters so that they are not used in key searches.
   *
   * This table is of all characters in the range of U+0100 to U+2200 that
   * have a canonical decomposition in NFD form. For some characters, this
   * translates them into their canonical characters (e.g., K -> K). For
   * characters with combining diacritical marks, this leaves behind the base
   * character, removing its diacritics (e.g., É -> E).
   */
  const PARTIAL_NFD_LOOKUP = {"À":"A","Á":"A","Â":"A","Ã":"A","Ä":"A","Å":"A","Ç":"C","È":"E","É":"E","Ê":"E","Ë":"E","Ì":"I","Í":"I","Î":"I","Ï":"I","Ñ":"N","Ò":"O","Ó":"O","Ô":"O","Õ":"O","Ö":"O","Ù":"U","Ú":"U","Û":"U","Ü":"U","Ý":"Y","à":"a","á":"a","â":"a","ã":"a","ä":"a","å":"a","ç":"c","è":"e","é":"e","ê":"e","ë":"e","ì":"i","í":"i","î":"i","ï":"i","ñ":"n","ò":"o","ó":"o","ô":"o","õ":"o","ö":"o","ù":"u","ú":"u","û":"u","ü":"u","ý":"y","ÿ":"y","Ā":"A","ā":"a","Ă":"A","ă":"a","Ą":"A","ą":"a","Ć":"C","ć":"c","Ĉ":"C","ĉ":"c","Ċ":"C","ċ":"c","Č":"C","č":"c","Ď":"D","ď":"d","Ē":"E","ē":"e","Ĕ":"E","ĕ":"e","Ė":"E","ė":"e","Ę":"E","ę":"e","Ě":"E","ě":"e","Ĝ":"G","ĝ":"g","Ğ":"G","ğ":"g","Ġ":"G","ġ":"g","Ģ":"G","ģ":"g","Ĥ":"H","ĥ":"h","Ĩ":"I","ĩ":"i","Ī":"I","ī":"i","Ĭ":"I","ĭ":"i","Į":"I","į":"i","İ":"I","Ĵ":"J","ĵ":"j","Ķ":"K","ķ":"k","Ĺ":"L","ĺ":"l","Ļ":"L","ļ":"l","Ľ":"L","ľ":"l","Ń":"N","ń":"n","Ņ":"N","ņ":"n","Ň":"N","ň":"n","Ō":"O","ō":"o","Ŏ":"O","ŏ":"o","Ő":"O","ő":"o","Ŕ":"R","ŕ":"r","Ŗ":"R","ŗ":"r","Ř":"R","ř":"r","Ś":"S","ś":"s","Ŝ":"S","ŝ":"s","Ş":"S","ş":"s","Š":"S","š":"s","Ţ":"T","ţ":"t","Ť":"T","ť":"t","Ũ":"U","ũ":"u","Ū":"U","ū":"u","Ŭ":"U","ŭ":"u","Ů":"U","ů":"u","Ű":"U","ű":"u","Ų":"U","ų":"u","Ŵ":"W","ŵ":"w","Ŷ":"Y","ŷ":"y","Ÿ":"Y","Ź":"Z","ź":"z","Ż":"Z","ż":"z","Ž":"Z","ž":"z","Ơ":"O","ơ":"o","Ư":"U","ư":"u","Ǎ":"A","ǎ":"a","Ǐ":"I","ǐ":"i","Ǒ":"O","ǒ":"o","Ǔ":"U","ǔ":"u","Ǖ":"U","ǖ":"u","Ǘ":"U","ǘ":"u","Ǚ":"U","ǚ":"u","Ǜ":"U","ǜ":"u","Ǟ":"A","ǟ":"a","Ǡ":"A","ǡ":"a","Ǣ":"Æ","ǣ":"æ","Ǧ":"G","ǧ":"g","Ǩ":"K","ǩ":"k","Ǫ":"O","ǫ":"o","Ǭ":"O","ǭ":"o","Ǯ":"Ʒ","ǯ":"ʒ","ǰ":"j","Ǵ":"G","ǵ":"g","Ǹ":"N","ǹ":"n","Ǻ":"A","ǻ":"a","Ǽ":"Æ","ǽ":"æ","Ǿ":"Ø","ǿ":"ø","Ȁ":"A","ȁ":"a","Ȃ":"A","ȃ":"a","Ȅ":"E","ȅ":"e","Ȇ":"E","ȇ":"e","Ȉ":"I","ȉ":"i","Ȋ":"I","ȋ":"i","Ȍ":"O","ȍ":"o","Ȏ":"O","ȏ":"o","Ȑ":"R","ȑ":"r","Ȓ":"R","ȓ":"r","Ȕ":"U","ȕ":"u","Ȗ":"U","ȗ":"u","Ș":"S","ș":"s","Ț":"T","ț":"t","Ȟ":"H","ȟ":"h","Ȧ":"A","ȧ":"a","Ȩ":"E","ȩ":"e","Ȫ":"O","ȫ":"o","Ȭ":"O","ȭ":"o","Ȯ":"O","ȯ":"o","Ȱ":"O","ȱ":"o","Ȳ":"Y","ȳ":"y","̀":"̀","́":"́","̓":"̓","̈́":"̈","ʹ":"ʹ",";":";","΅":"¨","Ά":"Α","·":"·","Έ":"Ε","Ή":"Η","Ί":"Ι","Ό":"Ο","Ύ":"Υ","Ώ":"Ω","ΐ":"ι","Ϊ":"Ι","Ϋ":"Υ","ά":"α","έ":"ε","ή":"η","ί":"ι","ΰ":"υ","ϊ":"ι","ϋ":"υ","ό":"ο","ύ":"υ","ώ":"ω","ϓ":"ϒ","ϔ":"ϒ","Ѐ":"Е","Ё":"Е","Ѓ":"Г","Ї":"І","Ќ":"К","Ѝ":"И","Ў":"У","Й":"И","й":"и","ѐ":"е","ё":"е","ѓ":"г","ї":"і","ќ":"к","ѝ":"и","ў":"у","Ѷ":"Ѵ","ѷ":"ѵ","Ӂ":"Ж","ӂ":"ж","Ӑ":"А","ӑ":"а","Ӓ":"А","ӓ":"а","Ӗ":"Е","ӗ":"е","Ӛ":"Ә","ӛ":"ә","Ӝ":"Ж","ӝ":"ж","Ӟ":"З","ӟ":"з","Ӣ":"И","ӣ":"и","Ӥ":"И","ӥ":"и","Ӧ":"О","ӧ":"о","Ӫ":"Ө","ӫ":"ө","Ӭ":"Э","ӭ":"э","Ӯ":"У","ӯ":"у","Ӱ":"У","ӱ":"у","Ӳ":"У","ӳ":"у","Ӵ":"Ч","ӵ":"ч","Ӹ":"Ы","ӹ":"ы","Ḁ":"A","ḁ":"a","Ḃ":"B","ḃ":"b","Ḅ":"B","ḅ":"b","Ḇ":"B","ḇ":"b","Ḉ":"C","ḉ":"c","Ḋ":"D","ḋ":"d","Ḍ":"D","ḍ":"d","Ḏ":"D","ḏ":"d","Ḑ":"D","ḑ":"d","Ḓ":"D","ḓ":"d","Ḕ":"E","ḕ":"e","Ḗ":"E","ḗ":"e","Ḙ":"E","ḙ":"e","Ḛ":"E","ḛ":"e","Ḝ":"E","ḝ":"e","Ḟ":"F","ḟ":"f","Ḡ":"G","ḡ":"g","Ḣ":"H","ḣ":"h","Ḥ":"H","ḥ":"h","Ḧ":"H","ḧ":"h","Ḩ":"H","ḩ":"h","Ḫ":"H","ḫ":"h","Ḭ":"I","ḭ":"i","Ḯ":"I","ḯ":"i","Ḱ":"K","ḱ":"k","Ḳ":"K","ḳ":"k","Ḵ":"K","ḵ":"k","Ḷ":"L","ḷ":"l","Ḹ":"L","ḹ":"l","Ḻ":"L","ḻ":"l","Ḽ":"L","ḽ":"l","Ḿ":"M","ḿ":"m","Ṁ":"M","ṁ":"m","Ṃ":"M","ṃ":"m","Ṅ":"N","ṅ":"n","Ṇ":"N","ṇ":"n","Ṉ":"N","ṉ":"n","Ṋ":"N","ṋ":"n","Ṍ":"O","ṍ":"o","Ṏ":"O","ṏ":"o","Ṑ":"O","ṑ":"o","Ṓ":"O","ṓ":"o","Ṕ":"P","ṕ":"p","Ṗ":"P","ṗ":"p","Ṙ":"R","ṙ":"r","Ṛ":"R","ṛ":"r","Ṝ":"R","ṝ":"r","Ṟ":"R","ṟ":"r","Ṡ":"S","ṡ":"s","Ṣ":"S","ṣ":"s","Ṥ":"S","ṥ":"s","Ṧ":"S","ṧ":"s","Ṩ":"S","ṩ":"s","Ṫ":"T","ṫ":"t","Ṭ":"T","ṭ":"t","Ṯ":"T","ṯ":"t","Ṱ":"T","ṱ":"t","Ṳ":"U","ṳ":"u","Ṵ":"U","ṵ":"u","Ṷ":"U","ṷ":"u","Ṹ":"U","ṹ":"u","Ṻ":"U","ṻ":"u","Ṽ":"V","ṽ":"v","Ṿ":"V","ṿ":"v","Ẁ":"W","ẁ":"w","Ẃ":"W","ẃ":"w","Ẅ":"W","ẅ":"w","Ẇ":"W","ẇ":"w","Ẉ":"W","ẉ":"w","Ẋ":"X","ẋ":"x","Ẍ":"X","ẍ":"x","Ẏ":"Y","ẏ":"y","Ẑ":"Z","ẑ":"z","Ẓ":"Z","ẓ":"z","Ẕ":"Z","ẕ":"z","ẖ":"h","ẗ":"t","ẘ":"w","ẙ":"y","ẛ":"ſ","Ạ":"A","ạ":"a","Ả":"A","ả":"a","Ấ":"A","ấ":"a","Ầ":"A","ầ":"a","Ẩ":"A","ẩ":"a","Ẫ":"A","ẫ":"a","Ậ":"A","ậ":"a","Ắ":"A","ắ":"a","Ằ":"A","ằ":"a","Ẳ":"A","ẳ":"a","Ẵ":"A","ẵ":"a","Ặ":"A","ặ":"a","Ẹ":"E","ẹ":"e","Ẻ":"E","ẻ":"e","Ẽ":"E","ẽ":"e","Ế":"E","ế":"e","Ề":"E","ề":"e","Ể":"E","ể":"e","Ễ":"E","ễ":"e","Ệ":"E","ệ":"e","Ỉ":"I","ỉ":"i","Ị":"I","ị":"i","Ọ":"O","ọ":"o","Ỏ":"O","ỏ":"o","Ố":"O","ố":"o","Ồ":"O","ồ":"o","Ổ":"O","ổ":"o","Ỗ":"O","ỗ":"o","Ộ":"O","ộ":"o","Ớ":"O","ớ":"o","Ờ":"O","ờ":"o","Ở":"O","ở":"o","Ỡ":"O","ỡ":"o","Ợ":"O","ợ":"o","Ụ":"U","ụ":"u","Ủ":"U","ủ":"u","Ứ":"U","ứ":"u","Ừ":"U","ừ":"u","Ử":"U","ử":"u","Ữ":"U","ữ":"u","Ự":"U","ự":"u","Ỳ":"Y","ỳ":"y","Ỵ":"Y","ỵ":"y","Ỷ":"Y","ỷ":"y","Ỹ":"Y","ỹ":"y","ἀ":"α","ἁ":"α","ἂ":"α","ἃ":"α","ἄ":"α","ἅ":"α","ἆ":"α","ἇ":"α","Ἀ":"Α","Ἁ":"Α","Ἂ":"Α","Ἃ":"Α","Ἄ":"Α","Ἅ":"Α","Ἆ":"Α","Ἇ":"Α","ἐ":"ε","ἑ":"ε","ἒ":"ε","ἓ":"ε","ἔ":"ε","ἕ":"ε","Ἐ":"Ε","Ἑ":"Ε","Ἒ":"Ε","Ἓ":"Ε","Ἔ":"Ε","Ἕ":"Ε","ἠ":"η","ἡ":"η","ἢ":"η","ἣ":"η","ἤ":"η","ἥ":"η","ἦ":"η","ἧ":"η","Ἠ":"Η","Ἡ":"Η","Ἢ":"Η","Ἣ":"Η","Ἤ":"Η","Ἥ":"Η","Ἦ":"Η","Ἧ":"Η","ἰ":"ι","ἱ":"ι","ἲ":"ι","ἳ":"ι","ἴ":"ι","ἵ":"ι","ἶ":"ι","ἷ":"ι","Ἰ":"Ι","Ἱ":"Ι","Ἲ":"Ι","Ἳ":"Ι","Ἴ":"Ι","Ἵ":"Ι","Ἶ":"Ι","Ἷ":"Ι","ὀ":"ο","ὁ":"ο","ὂ":"ο","ὃ":"ο","ὄ":"ο","ὅ":"ο","Ὀ":"Ο","Ὁ":"Ο","Ὂ":"Ο","Ὃ":"Ο","Ὄ":"Ο","Ὅ":"Ο","ὐ":"υ","ὑ":"υ","ὒ":"υ","ὓ":"υ","ὔ":"υ","ὕ":"υ","ὖ":"υ","ὗ":"υ","Ὑ":"Υ","Ὓ":"Υ","Ὕ":"Υ","Ὗ":"Υ","ὠ":"ω","ὡ":"ω","ὢ":"ω","ὣ":"ω","ὤ":"ω","ὥ":"ω","ὦ":"ω","ὧ":"ω","Ὠ":"Ω","Ὡ":"Ω","Ὢ":"Ω","Ὣ":"Ω","Ὤ":"Ω","Ὥ":"Ω","Ὦ":"Ω","Ὧ":"Ω","ὰ":"α","ά":"α","ὲ":"ε","έ":"ε","ὴ":"η","ή":"η","ὶ":"ι","ί":"ι","ὸ":"ο","ό":"ο","ὺ":"υ","ύ":"υ","ὼ":"ω","ώ":"ω","ᾀ":"α","ᾁ":"α","ᾂ":"α","ᾃ":"α","ᾄ":"α","ᾅ":"α","ᾆ":"α","ᾇ":"α","ᾈ":"Α","ᾉ":"Α","ᾊ":"Α","ᾋ":"Α","ᾌ":"Α","ᾍ":"Α","ᾎ":"Α","ᾏ":"Α","ᾐ":"η","ᾑ":"η","ᾒ":"η","ᾓ":"η","ᾔ":"η","ᾕ":"η","ᾖ":"η","ᾗ":"η","ᾘ":"Η","ᾙ":"Η","ᾚ":"Η","ᾛ":"Η","ᾜ":"Η","ᾝ":"Η","ᾞ":"Η","ᾟ":"Η","ᾠ":"ω","ᾡ":"ω","ᾢ":"ω","ᾣ":"ω","ᾤ":"ω","ᾥ":"ω","ᾦ":"ω","ᾧ":"ω","ᾨ":"Ω","ᾩ":"Ω","ᾪ":"Ω","ᾫ":"Ω","ᾬ":"Ω","ᾭ":"Ω","ᾮ":"Ω","ᾯ":"Ω","ᾰ":"α","ᾱ":"α","ᾲ":"α","ᾳ":"α","ᾴ":"α","ᾶ":"α","ᾷ":"α","Ᾰ":"Α","Ᾱ":"Α","Ὰ":"Α","Ά":"Α","ᾼ":"Α","ι":"ι","῁":"¨","ῂ":"η","ῃ":"η","ῄ":"η","ῆ":"η","ῇ":"η","Ὲ":"Ε","Έ":"Ε","Ὴ":"Η","Ή":"Η","ῌ":"Η","῍":"᾿","῎":"᾿","῏":"᾿","ῐ":"ι","ῑ":"ι","ῒ":"ι","ΐ":"ι","ῖ":"ι","ῗ":"ι","Ῐ":"Ι","Ῑ":"Ι","Ὶ":"Ι","Ί":"Ι","῝":"῾","῞":"῾","῟":"῾","ῠ":"υ","ῡ":"υ","ῢ":"υ","ΰ":"υ","ῤ":"ρ","ῥ":"ρ","ῦ":"υ","ῧ":"υ","Ῠ":"Υ","Ῡ":"Υ","Ὺ":"Υ","Ύ":"Υ","Ῥ":"Ρ","῭":"¨","΅":"¨","`":"`","ῲ":"ω","ῳ":"ω","ῴ":"ω","ῶ":"ω","ῷ":"ω","Ὸ":"Ο","Ό":"Ο","Ὼ":"Ω","Ώ":"Ω","ῼ":"Ω","´":"´"," ":" "," ":" ","Ω":"Ω","K":"K","Å":"A"};
}
