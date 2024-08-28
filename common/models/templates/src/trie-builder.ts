import { SENTINEL_CODE_UNIT, Wordform2Key } from "./common.js";
import { compressNode } from "./trie-compression.js";
import { Entry, InternalNode, Leaf, Node, Trie, sortNode } from "./trie.js";

export function createRootNode(): Node {
  return {
    type: 'leaf',
    weight: 0,
    entries: []
  };
}

/**
 * Adds an entry to the trie.  Currently assumes there is no pre-existing match
 * for the entry.
 *
 * Note that the trie will likely be unsorted after the add occurs. Before
 * performing a lookup on the trie, use call sortTrie() on the root note!
 *
 * @param node Which node should the entry be added to?
 * @param entry the wordform/weight/key to add to the trie
 * @param index the index in the key and also the trie depth. Should be set to
 *              zero when adding onto the root node of the trie.
 */
export function addUnsorted(node: Node, entry: Entry, index: number = 0) {
  // Each node stores the MAXIMUM weight out of all of its decesdents, to
  // enable a greedy search through the trie.
  node.weight = Math.max(node.weight, entry.weight);

  // When should a leaf become an interior node?
  // When it already has a value, but the key of the current value is longer
  // than the prefix.
  if (node.type === 'leaf' && index < entry.key.length && node.entries.length >= 1) {
    convertLeafToInternalNode(node, index);
  }

  if (node.type === 'leaf') {
    // The key matches this leaf node, so add yet another entry.
    addItemToLeaf(node, entry);
  } else {
    // Push the node down to a lower node.
    addItemToInternalNode(node, entry, index);
  }

  node.unsorted = true;
}

/**
 * Adds an item to the internal node at a given depth.
 * @param item
 * @param index
 */
export function addItemToInternalNode(node: InternalNode, item: Entry, index: number) {
  let char = item.key[index];
  // If an internal node is the proper site for item, it belongs under the
  // corresponding (sentinel, internal-use) child node signifying this.
  if(char == undefined) {
    char = SENTINEL_CODE_UNIT;
  }
  if (!node.children[char]) {
    node.children[char] = createRootNode();
    node.values.push(char);
  }
  addUnsorted(node.children[char], item, index + 1);
}

export function addItemToLeaf(leaf: Leaf, item: Entry) {
  leaf.entries.push(item);
}

/**
 * Mutates the given Leaf to turn it into an InternalNode.
 *
 * NOTE: the node passed in will be DESTRUCTIVELY CHANGED into a different
 * type when passed into this function!
 *
 * @param depth depth of the trie at this level.
 */
export function convertLeafToInternalNode(leaf: Leaf, depth: number): void {
  let entries = leaf.entries;

  // Alias the current node, as the desired type.
  let internal = (<unknown> leaf) as InternalNode;
  internal.type = 'internal';

  delete (leaf as Partial<Leaf>).entries;
  internal.values = [];
  internal.children = {};

  // Convert the old values array into the format for interior nodes.
  for (let item of entries) {
    let char: string;
    if (depth < item.key.length) {
      char = item.key[depth];
    } else {
      char = SENTINEL_CODE_UNIT;
    }

    if (!internal.children[char]) {
      internal.children[char] = createRootNode();
      internal.values.push(char);
    }
    addUnsorted(internal.children[char], item, depth + 1);
  }

  internal.unsorted = true;
}

/**
 * Wrapper class for the trie and its nodes.
 */
export class TrieBuilder extends Trie {
  /** The total weight of the entire trie. */
  totalWeight: number;

  constructor(toKey: Wordform2Key) {
    super(createRootNode(), 0, toKey);
    this.totalWeight = 0;
  }

  addEntry(word: string, weight?: number) {
    weight = (isNaN(weight ?? NaN) || !weight) ? 1 : weight;
    this.totalWeight += weight;

    addUnsorted(
      this.root, {
        key: this.toKey(word),
        content: word,
        weight: weight
      },
      0
    );
  }

  sort() {
    // Sorts the full Trie, not just a part of it.
    sortNode(this.root);
  }

  getRoot(): Node {
    return this.root;
  }

  getTotalWeight(): number {
    return this.totalWeight;
  }

  compress(): string {
    return compressNode(this.root);
  }
}