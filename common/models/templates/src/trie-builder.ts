import { SENTINEL_CODE_UNIT, Wordform2Key } from "./common.js";
import { Entry, InternalNode, Leaf, Node, Trie } from "./trie.js";

function createRootNode(): Node {
  return {
    type: 'leaf',
    weight: 0,
    entries: []
  };
}

/**
 * Adds an entry to the trie.
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
function addItemToInternalNode(node: InternalNode, item: Entry, index: number) {
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

function addItemToLeaf(leaf: Leaf, item: Entry) {
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
function convertLeafToInternalNode(leaf: Leaf, depth: number): void {
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
 * Recursively sort the trie, in descending order of weight.
 * @param node any node in the trie
 */
function sortTrie(node: Node) {
  if (node.type === 'leaf') {
    if (!node.unsorted) {
      return;
    }

    node.entries.sort(function (a, b) { return b.weight - a.weight; });
  } else {
    // We MUST recurse and sort children before returning.
    for (let char of node.values) {
      sortTrie(node.children[char]);
    }

    if (!node.unsorted) {
      return;
    }

    node.values.sort((a, b) => {
      return node.children[b].weight - node.children[a].weight;
    });
  }

  delete node.unsorted;
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

  sort() {
    sortTrie(this.root);
  }

  getRoot(): Node {
    return this.root;
  }
}