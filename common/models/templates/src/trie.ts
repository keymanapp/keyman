import { isHighSurrogate, isSentinel, SearchKey, SENTINEL_CODE_UNIT, Wordform2Key } from "./common.js";
import { decompressNode, inflateChild } from "./trie-compression.js";

// The following trie implementation has been (heavily) derived from trie-ing
// by Conrad Irwin.
// trie-ing is copyright (C) 2015–2017 Conrad Irwin.
// Distributed under the terms of the MIT license:
// https://github.com/ConradIrwin/trie-ing/blob/df55d7af7068d357829db9e0a7faa8a38add1d1d/LICENSE

export type Node = InternalNode | Leaf;
/**
 * An internal node in the trie. Internal nodes NEVER contain entries; if an
 * internal node should contain an entry, then it has a dummy leaf node (see
 * below), that can be accessed by node.children["\uFDD0"].
 */
export interface InternalNode {
  type: 'internal';
  weight: number;
  /** Maintains the keys of children in descending order of weight. */
  values: string[]; // TODO: As an optimization, "values" can be a single string!
  /**
   * Maps a single UTF-16 code unit to a child node in the trie. This child
   * node may be a leaf or an internal node. The keys of this object are kept
   * in sorted order in the .values array.
   */
  children: { [codeunit: string]: Node | string };

  /**
   * Used during compilation.
   */
  unsorted?: boolean;
}
/** Only leaf nodes actually contain entries (i.e., the words proper). */
export interface Leaf {
  type: 'leaf';
  weight: number;
  entries: Entry[];

  /**
   * Used during compilation.
   */
  unsorted?: boolean;
}

/**
 * An entry in the prefix trie (stored in leaf nodes exclusively!)
 */
export interface Entry {
  /** The actual word form, stored in the trie. */
  content: string;
  /** A search key that usually simplifies the word form, for ease of search. */
  key: SearchKey;
  weight: number;
}

/**
 * Recursively sort the trie, in descending order of weight.
 * @param node any node in the trie
 */
export function sortNode(node: Node, toKey: Wordform2Key, onlyLocal?: boolean) {
  if (node.type === 'leaf') {
    if (!node.unsorted) {
      return;
    }

    node.entries.sort(function (a, b) { return b.weight - a.weight; });
  } else {
    if(!onlyLocal) {
      // We recurse and sort children before returning if sorting the full Trie.
      for (let char of node.values) {
        const childNode = inflateChild(node.children, char, toKey);
        sortNode(childNode, toKey, onlyLocal);
      }
    }

    if (!node.unsorted) {
      return;
    }

    node.values.sort((a, b) => {
      return (node.children[b] as Node).weight - (node.children[a] as Node).weight;
    });
  }

  delete node.unsorted;
}

export class TrieTraversal implements LexiconTraversal {
  /**
   * The lexical prefix corresponding to the current traversal state.
   */
  prefix: String;

  /**
   * The current traversal node.  Serves as the 'root' of its own sub-Trie,
   * and we cannot navigate back to its parent.
   */
  root: Node;

  readonly toKey: Wordform2Key;

  /**
   * The max weight for the Trie being 'traversed'.  Needed for probability
   * calculations.
   */
  totalWeight: number;

  constructor(root: Node, toKey: Wordform2Key, prefix: string, totalWeight: number) {
    this.root = root;
    this.toKey = toKey;
    this.prefix = prefix;
    this.totalWeight = totalWeight;
  }

  child(char: USVString): LexiconTraversal | undefined {
    // May result for blank tokens resulting immediately after whitespace.
    if(char == '') {
      return this;
    }

    // Split into individual code units.
    let steps = char.split('');
    let traversal: TrieTraversal | undefined = this;

    while(steps.length > 0 && traversal) {
      const step: string = steps.shift()!;
      traversal = traversal._child(step);
    }

    return traversal;
  }

  // Handles one code unit at a time.
  private _child(char: USVString): TrieTraversal | undefined {
    const root = this.root;
    const toKey = this.toKey;
    const totalWeight = this.totalWeight;
    const nextPrefix = this.prefix + char;

    // Sorts _just_ the current level, and only if needed.
    // We only care about sorting parts that we're actually accessing.
    sortNode(root, toKey, true);

    if(root.type == 'internal') {
      let child = root.children[char];
      if(!child) {
        return undefined;
      }

      const childNode = inflateChild(root.children, char, toKey);
      return new TrieTraversal(childNode, toKey, nextPrefix, totalWeight);
    } else {
      // root.type == 'leaf';
      const legalChildren = root.entries.filter(function(entry) {
        return entry.key.indexOf(nextPrefix) == 0;
      });

      if(!legalChildren.length) {
        return undefined;
      }

      return new TrieTraversal(root, toKey, nextPrefix, totalWeight);
    }
  }

  *children(): Generator<{char: USVString, traversal: () => LexiconTraversal}> {
    const root = this.root;
    const toKey = this.toKey;

    // We refer to the field multiple times in this method, and it doesn't change.
    // This also assists minification a bit, since we can't minify when re-accessing
    // through `this.`.
    const totalWeight = this.totalWeight;

    // Sorts _just_ the current level, and only if needed.
    // We only care about sorting parts that we're actually accessing.
    sortNode(root, toKey, true);

    if(root.type == 'internal') {
      for(let entry of root.values) {
        let entryNode = inflateChild(root.children, entry, toKey);

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
                traversal: function() { return new TrieTraversal(inflateChild(internalNode.children, lowSurrogate, toKey), toKey, prefix, totalWeight) }
              }
            }
          } else {
            // Determine how much of the 'leaf' entry has no Trie nodes, emulate them.
            let fullText = entryNode.entries[0].key;
            entry = entry + fullText[this.prefix.length + 1]; // The other half of the non-BMP char.
            let prefix = this.prefix + entry;

            yield {
              char: entry,
              traversal: function () {return new TrieTraversal(entryNode, toKey, prefix, totalWeight)}
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
            traversal: function() { return new TrieTraversal(entryNode, toKey, prefix, totalWeight)}
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
          traversal: function() { return new TrieTraversal(root, toKey, prefix + nodeKey, totalWeight)}
        }
      };
      return;
    }
  }

  get entries() {
    const root = this.root;
    const entryMapper = (value: Entry) => {
      return {
        text: value.content,
        p: value.weight / this.totalWeight
      }
    }

    if(root.type == 'leaf') {
      const leaf = root;
      let prefix = this.prefix;
      let matches = leaf.entries.filter(function(entry) {
        return entry.key == prefix;
      });

      return matches.map(entryMapper);
    } else {
      const node = root;
      let matchingLeaf = inflateChild(node.children, SENTINEL_CODE_UNIT, this.toKey);
      if(matchingLeaf && matchingLeaf.type == 'leaf') {
        return matchingLeaf.entries.map(entryMapper);
      } else {
        return [];
      }
    }
  }

  get p(): number {
    return this.root.weight / this.totalWeight;
  }
}

/**
 * Wrapper class for the trie and its nodes.
 */
export class Trie {
  readonly root: Node;
  /** The total weight of the entire trie. */
  readonly totalWeight: number;
  /**
   * Converts arbitrary strings to a search key. The trie is built up of
   * search keys; not each entry's word form!
   */
  toKey: Wordform2Key;

  constructor(trie: Node | string, totalWeight: number, wordform2key: Wordform2Key) {
    const root = (typeof trie == 'string') ? decompressNode(trie, wordform2key, 0) : trie;
    this.root = root;
    this.toKey = wordform2key;
    this.totalWeight = totalWeight;
  }

  public traverseFromRoot(): LexiconTraversal {
    return new TrieTraversal(this.root, this.toKey, '', this.totalWeight);
  }
}