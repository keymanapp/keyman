import { isHighSurrogate, isSentinel, SearchKey, SENTINEL_CODE_UNIT, Wordform2Key } from "./common.js";

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
  children: { [codeunit: string]: Node };

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

  /**
   * The max weight for the Trie being 'traversed'.  Needed for probability
   * calculations.
   */
  totalWeight: number;

  constructor(root: Node, prefix: string, totalWeight: number) {
    this.root = root;
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
    let traversal: ReturnType<TrieTraversal["_child"]> = this;

    while(steps.length > 0 && traversal) {
      const step: string = steps.shift()!;
      traversal = traversal._child(step);
    }

    return traversal;
  }

  private sortNodeIfNeeded(node: Node) {
    if(node.unsorted) {
      if(node.type == 'leaf') {
        node.entries.sort((a, b) => b.weight - a.weight)
      } else {
        node.values.sort((a, b) => node.children[b].weight - node.children[a].weight);
      }

      node.unsorted = false;
    }
  }

  // Handles one code unit at a time.
  private _child(char: USVString): TrieTraversal | undefined {
    const root = this.root;
    const totalWeight = this.totalWeight;
    const nextPrefix = this.prefix + char;

    this.sortNodeIfNeeded(root);

    if(root.type == 'internal') {
      let childNode = root.children[char];
      if(!childNode) {
        return undefined;
      }

      return new TrieTraversal(childNode, nextPrefix, totalWeight);
    } else {
      // root.type == 'leaf';
      const legalChildren = root.entries.filter(function(entry) {
        return entry.key.indexOf(nextPrefix) == 0;
      });

      if(!legalChildren.length) {
        return undefined;
      }

      return new TrieTraversal(root, nextPrefix, totalWeight);
    }
  }

  *children(): Generator<{char: USVString, traversal: () => LexiconTraversal}> {
    let root = this.root;

    // We refer to the field multiple times in this method, and it doesn't change.
    // This also assists minification a bit, since we can't minify when re-accessing
    // through `this.`.
    const totalWeight = this.totalWeight;

    this.sortNodeIfNeeded(root);

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
                traversal: function() { return new TrieTraversal(internalNode.children[lowSurrogate], prefix, totalWeight) }
              }
            }
          } else {
            // Determine how much of the 'leaf' entry has no Trie nodes, emulate them.
            let fullText = entryNode.entries[0].key;
            entry = entry + fullText[this.prefix.length + 1]; // The other half of the non-BMP char.
            let prefix = this.prefix + entry;

            yield {
              char: entry,
              traversal: function () {return new TrieTraversal(entryNode, prefix, totalWeight)}
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
            traversal: function() { return new TrieTraversal(entryNode, prefix, totalWeight)}
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
          traversal: function() { return new TrieTraversal(root, prefix + nodeKey, totalWeight)}
        }
      };
      return;
    }
  }

  get entries() {
    const entryMapper = (value: Entry) => {
      return {
        text: value.content,
        p: value.weight / this.totalWeight
      }
    }

    if(this.root.type == 'leaf') {
      let prefix = this.prefix;
      let matches = this.root.entries.filter(function(entry) {
        return entry.key == prefix;
      });

      return matches.map(entryMapper);
    } else {
      let matchingLeaf = this.root.children[SENTINEL_CODE_UNIT];
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
  protected root: Node;
  /** The total weight of the entire trie. */
  readonly totalWeight: number;
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

  public traverseFromRoot(): LexiconTraversal {
    return new TrieTraversal(this.root, '', this.totalWeight);
  }
}