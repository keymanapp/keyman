/*
 * Unit tests for the Trie prediction model.
 */

import { assert } from 'chai';
import { InternalNode, Leaf, SearchKey, SENTINEL_CODE_UNIT, trieConstruction, TrieBuilder } from '@keymanapp/models-templates';

describe('trie construction', () => {
  it('default root node', () => {
    const defaultRoot = trieConstruction.createRootNode() as Leaf;
    assert.equal(defaultRoot.type, 'leaf');
    assert.equal(defaultRoot.weight, 0);
    assert.sameMembers(defaultRoot.entries, []);
  });

  it('addItemToLeaf', () => {
    // `weight` is managed by addUnsorted, not by addItemToLeaf.
    // As a result, we don't test for it here.
    const defaultRoot = trieConstruction.createRootNode() as Leaf;
    trieConstruction.addItemToLeaf(defaultRoot, {
      content: 'cafe',
      weight: 2,
      key: 'cafe' as SearchKey
    });

    assert.equal(defaultRoot.type, 'leaf');
    assert.equal(defaultRoot.entries.length, 1);

    trieConstruction.addItemToLeaf(defaultRoot, {
      content: 'café',
      weight: 1,
      key: 'cafe' as SearchKey
    });

    assert.equal(defaultRoot.type, 'leaf');
    assert.equal(defaultRoot.entries.length, 2);
  });

  it('convertLeafToInternalNode', () => {
    const defaultRoot = trieConstruction.createRootNode() as InternalNode;
    const cafeEntry = {
      content: 'cafe',
      weight: 5,
      key: 'cafe' as SearchKey
    };
    trieConstruction.addItemToLeaf(defaultRoot as unknown as Leaf, cafeEntry);

    defaultRoot.weight = 5;
    trieConstruction.convertLeafToInternalNode(defaultRoot as unknown as Leaf, 4);

    assert.equal(defaultRoot.type, 'internal');
    assert.sameMembers(defaultRoot.values, [SENTINEL_CODE_UNIT]);
    assert.sameMembers(Object.keys(defaultRoot.children), defaultRoot.values);
    assert.equal(defaultRoot.weight, 5);

    const sentinelChild = defaultRoot.children[SENTINEL_CODE_UNIT] as Leaf;
    assert.equal(sentinelChild.type, 'leaf');
    assert.equal(sentinelChild.weight, 5);
    assert.equal(sentinelChild.entries.length, 1);
    // Should be strict-equal too, but we only really care if it's deep-equal.
    assert.deepEqual(sentinelChild.entries[0], cafeEntry);
  });

  it('addUnsorted - simple initial entry', () => {
    // `weight` is managed by addUnsorted, not by addItemToLeaf.
    // As a result, we don't test for it here.
    const defaultRoot = trieConstruction.createRootNode() as Leaf;
    trieConstruction.addUnsorted(defaultRoot, {
      content: 'cafe',
      weight: 2,
      key: 'cafe' as SearchKey
    }, 4);

    assert.equal(defaultRoot.type, 'leaf');
    assert.equal(defaultRoot.weight, 2);
    assert.equal(defaultRoot.entries.length, 1);

    // smaller weight, will not override.  Is not additive.
    trieConstruction.addUnsorted(defaultRoot, {
      content: 'café',
      weight: 1,
      key: 'cafe' as SearchKey
    }, 4);

    assert.equal(defaultRoot.type, 'leaf');
    assert.equal(defaultRoot.weight, 2);
    assert.equal(defaultRoot.entries.length, 2);
  });

  it('addUnsorted - short word, then longer word with same prefix', () => {
    const defaultRoot = trieConstruction.createRootNode();
    trieConstruction.addUnsorted(defaultRoot, {
      content: 'cafe',
      weight: 5,
      key: 'cafe' as SearchKey
    }, 4);

    const rootAsLeaf = defaultRoot as Leaf;

    assert.equal(rootAsLeaf.type, 'leaf');
    assert.equal(rootAsLeaf.weight, 5);
    assert.equal(rootAsLeaf.entries.length, 1);

    // smaller weight, will not override.  Is not additive.
    trieConstruction.addUnsorted(defaultRoot, {
      content: 'cafeteria',
      weight: 3,
      key: 'cafeteria' as SearchKey
    }, 4);

    const rootAsInternal = defaultRoot as InternalNode;

    assert.equal(rootAsInternal.type, 'internal');
    assert.equal(rootAsInternal.weight, 5);
    assert.deepEqual(rootAsInternal.values, [SENTINEL_CODE_UNIT, 't']);
    assert.equal((rootAsInternal.children[SENTINEL_CODE_UNIT] as Leaf).weight, 5);
    assert.equal((rootAsInternal.children['t'] as Leaf).weight, 3);
  });

  it('addUnsorted', () => {
    // Perspective:  current root is actually representing `ca`.
    const defaultRoot = trieConstruction.createRootNode();
    trieConstruction.addUnsorted(defaultRoot, {
      content: 'cafe',
      weight: 5,
      key: 'cafe' as SearchKey
    }, 2);

    // There's only one child, so there's no reason to start making
    // internal structure just yet.
    assert.equal(defaultRoot.type, 'leaf');
    assert.equal(defaultRoot.weight, 5);

    // Adds a second child; that child has more than one letter not-in-common with the first.
    trieConstruction.addUnsorted(defaultRoot, {
      content: 'call',
      weight: 8,
      key: 'call' as SearchKey
    }, 2);

    const rootAsInternal = defaultRoot as InternalNode;

    assert.equal(rootAsInternal.type, 'internal');
    assert.equal(rootAsInternal.weight, 8);
    assert.sameMembers(rootAsInternal.values, ['f', 'l']);
    assert.equal((rootAsInternal.children['f'] as Leaf).weight, 5);
    assert.equal((rootAsInternal.children['l'] as Leaf).weight, 8);
  });

  it('addUnsorted - lower frequency prefix', () => {
    // Will correspond to 'thin'.
    const root = trieConstruction.createRootNode();
    trieConstruction.addUnsorted(root, {
      key: 'think' as SearchKey,
      content: 'think',
      weight: 100
    }, 4);
    trieConstruction.addUnsorted(root, {
      key: 'thing' as SearchKey,
      content: 'thing',
      weight: 40
    }, 4);

    const rootAsInternal = root as InternalNode;
    assert.equal(rootAsInternal.type, 'internal');
    assert.sameMembers(rootAsInternal.values, ['k', 'g']);

    // Interesting noted behavior:  if just 'think' into 'thin', and nothing else...
    // it remains a leaf!  That doesn't seem... entirely proper.  With 'thing', it
    // will at least be an 'internal' node already.
    trieConstruction.addUnsorted(rootAsInternal, {
      key: 'thin' as SearchKey,
      content: 'thin',
      weight: 20
    }, 4);

    assert.sameMembers(rootAsInternal.values, ['k', 'g', SENTINEL_CODE_UNIT]);
    assert.isOk(rootAsInternal.children[SENTINEL_CODE_UNIT]);
    assert.equal((rootAsInternal.children[SENTINEL_CODE_UNIT] as Leaf).weight, 20);
  });

  describe('TrieBuilder', () => {
    it('standard Trie construction', () => {
      const builder = new TrieBuilder((text) => text as SearchKey);
      builder.addEntry('caffeine', 2);
      builder.addEntry('cafe', 5);
      builder.addEntry('calm', 3);
      builder.addEntry('calf', 4);
      builder.addEntry('call', 6); // total: 20
      builder.addEntry('can', 10); // total: 30
      builder.addEntry('and', 20); // total: 50

      assert.equal(builder.getTotalWeight(), 50);

      const root = builder.getRoot() as InternalNode;
      const aNode = (root.children['c'] as InternalNode).children['a'] as InternalNode;

      // As the nodes were not added in sorted order, they should not currently be ordered.
      assert.sameMembers(aNode.values, ['n', 'l', 'f']);
      assert.notSameOrderedMembers(aNode.values, ['n', 'l', 'f']);

      const lNode = aNode.children['l'] as InternalNode;
      assert.sameMembers(lNode.values, ['l', 'f', 'm']);
      assert.notSameOrderedMembers(lNode.values, ['l', 'f', 'm']);

      builder.sort();

      assert.sameOrderedMembers(aNode.values, ['n', 'l', 'f']);
      assert.sameOrderedMembers(lNode.values, ['l', 'f', 'm']);
    });

    // In case of high startup-time for user-dictionary processing, this could allow use
    // of a 'partially' processed user wordlist.  (We'd prioritize predicting when the
    // user is interacting with text, then resume processing once 'idle'.)
    it('interspersed construction + lookup', () => {
      const builder = new TrieBuilder((text) => text as SearchKey);
      builder.addEntry('caffeine', 2);
      builder.addEntry('cafe', 5);
      builder.addEntry('calm', 3);
      builder.addEntry('calf', 4);
      builder.addEntry('call', 6); // total: 20

      // ------------------------------------------
      // Pause construction; actually use the Trie.

      assert.equal(builder.getTotalWeight(), 20);
      const root = builder.getRoot() as InternalNode;
      const caNode = (root.children['c'] as InternalNode).children['a'] as InternalNode;

      // As the nodes were not added in sorted order, they should not currently be ordered.
      assert.sameMembers(caNode.values, ['l', 'f']);
      assert.notSameOrderedMembers(caNode.values, ['l', 'f']);

      const cal = builder.traverseFromRoot().child('cal');
      // Actually traversing through the node should auto-sort the entries.
      assert.sameOrderedMembers(caNode.values, ['l', 'f']);
      // Including the reached node's children.
      assert.sameOrderedMembers([...cal.children()].map((entry) => entry.char), ['l', 'f', 'm']);

      const cafNode = caNode.children['f'] as InternalNode;

      // 'caffeine' was added before 'cafe'.
      // Parts not 'traversed' should not be unnecessarily sorted.
      assert.notSameOrderedMembers(cafNode.values, ['e', 'f']);

      // -------------------
      // Resume construction

      builder.addEntry('can', 10); // total: 30
      builder.addEntry('and', 20); // total: 50

      assert.equal(builder.getTotalWeight(), 50);

      // As the nodes were not added in sorted order, they should not currently be ordered.
      assert.sameMembers(caNode.values, ['n', 'l', 'f']);
      // 'n' was added later, thus will be out of sorted order.
      assert.notSameOrderedMembers(caNode.values, ['n', 'l', 'f']);

      builder.sort();

      assert.sameOrderedMembers(caNode.values, ['n', 'l', 'f']);
    });
  });
});