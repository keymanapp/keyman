/*
 * Unit tests for the Trie prediction model.
 */

import { assert } from 'chai';
import {
  compressEntry, decompressEntry,
  compressNode, decompressNode,
  compressNumber, decompressNumber,
  ENCODED_NUM_BASE
} from '@keymanapp/models-templates/obj/trie-compression.js';

import { TrieBuilder } from '@keymanapp/models-templates';
import { Trie } from '../build/obj/trie.js';

const smpWordlist = [
  ['CRAZ🤪', 13644],
  ['🙄', 9134],
  ['😇', 4816],
  ['🇸'],
  ['u']
];

/**
 * @param {string} str
 * @returns
 */
const identityKey = (str) => str;
/**
 * @param {string} str
 * @returns
 */
const lowercaseKey = (str) => str.toLowerCase();

// Written with:
// const ENCODED_NUM_BASE = 0; // 0x0020;
// const SINGLE_CHAR_RANGE = Math.pow(2, 16) - ENCODED_NUM_BASE;
//
// Will need manual re-encoding for \u-sequences if ENCODED_NUM_BASE is changed.

const TEST_DATA = {};
TEST_DATA.ENTRIES = {
  four: {
    // total length: header = 4, text = 4 -> 8.  (Made with weight-width 2)
    //                  -totalLen-            -weight-
    compressed: `${compressNumber(8, 2)}${compressNumber(8, 2)}four`,
    decompressed: {
      key: 'four',
      content: 'four',
      weight: 8
    },
    original: {
      key: 'four',
      content: 'four',
      weight: 8
    }
  }
};

TEST_DATA.LEAVES = {
  four: {
    // expected width difference: 5 (2: total size, 2: weight, 1: entry count)
    //                   -totalLen-              -weight-              -type=leaf + size-
    compressed: `${compressNumber(13, 2)}${compressNumber(8, 2)}${compressNumber(0x8000 + 1, 1)}${TEST_DATA.ENTRIES.four.compressed}`,
    decompressed: {
      type: 'leaf',
      weight: 8,
      entries: [TEST_DATA.ENTRIES.four.decompressed]
    },
    original: {
      type: 'leaf',
      weight: 8,
      entries: [{
        key: 'four',
        content: 'four',
        weight: 8
      }]
    }
  }
}

TEST_DATA.NODES = {
  four: {
    // expected width difference: 6 (2: total size, 2: weight, 1: entry count, 1: value count)
    //             -totalLen-  -weight- -type/size-
    compressed: `${compressNumber(19, 2)}${compressNumber(8, 2)}${compressNumber(1)}r${TEST_DATA.LEAVES.four.compressed}`,
    decompressed: {
      type: 'internal',
      weight: 8,
      values: ['r'],
      children: {r: TEST_DATA.LEAVES.four.compressed}
    },
    original: {
      type: 'internal',
      weight: 8,
      values: ['r'],
      children: {r: TEST_DATA.LEAVES.four.decompressed}
    }
  }
}

describe('Trie compression', function() {
  describe('`number`s', () => {
    it('uses single-char compression by default', () => {
      assert.equal(compressNumber(0x0020).length, 1);
    });

    it('compresses properly when targeting single-char width', () => {
      assert.equal(compressNumber(0x0020, 1), String.fromCharCode(0x0020 + ENCODED_NUM_BASE));
      assert.equal(compressNumber('"'.charCodeAt(0), 1), String.fromCharCode('"'.charCodeAt(0) + ENCODED_NUM_BASE));
    });

    it('has non-null leading char for numbers needing two-char representations', () => {
      assert.notEqual(compressNumber(0x00200020, 2).charAt(0), String.fromCharCode(0));
      assert.notEqual(
        compressNumber(0x0321ad20, 2).charAt(0), String.fromCharCode(0)
      );
    });

    it('width 2: compressing values one-char wide', () => {
      assert.equal(compressNumber(0x0020, 2), `${String.fromCharCode(ENCODED_NUM_BASE)}${String.fromCharCode(0x0020 + ENCODED_NUM_BASE)}`);
    });

    it('throws when numbers are too large for the specified width', () => {
      assert.throws(() => compressNumber(0x00200020, 1));
      assert.throws(() => compressNumber(0x002000200020, 2));
    })
  });

  describe('`Entry`s', () => {
    it('compresses properly', () => {
      assert.equal(compressEntry(TEST_DATA.ENTRIES.four.original), TEST_DATA.ENTRIES.four.compressed);
    });
  });

  describe('Leaf nodes', () => {
    it('compresses (mocked Entry)', () => {
      // Should not attempt to recompress the mock-compressed entry.
      assert.equal(compressNode(TEST_DATA.LEAVES.four.decompressed), TEST_DATA.LEAVES.four.compressed);
    });

    it('compresses (unmocked Entry)', () => {
      assert.equal(compressNode(TEST_DATA.LEAVES.four.original), TEST_DATA.LEAVES.four.compressed);
    });
  });

  describe('Internal nodes', () => {
    it('compresses (mocked Leaf)', () => {
      // Should not attempt to recompress the mock-compressed leaf.
      assert.equal(compressNode(TEST_DATA.NODES.four.decompressed), TEST_DATA.NODES.four.compressed);
    });

    it('compresses (unmocked Leaf)', () => {
      assert.equal(compressNode(TEST_DATA.NODES.four.original), TEST_DATA.NODES.four.compressed);
    });
  });
});

describe('Trie decompression', function () {
  describe('`number`s', () => {
    describe('not inlined', () => {
      it('decompresses single-char strings', () => {
        assert.equal(decompressNumber(String.fromCharCode(0x0020 + ENCODED_NUM_BASE), 0), 0x0020);
        assert.equal(decompressNumber(String.fromCharCode('"'.charCodeAt(0) + ENCODED_NUM_BASE), 0), '"'.charCodeAt(0));
        assert.equal(decompressNumber('\ufffe', 0), 0xfffe - ENCODED_NUM_BASE);
      });

      it('decompresses two-char strings of one-char value width', () => {
        assert.equal(decompressNumber(`${String.fromCharCode(ENCODED_NUM_BASE)}${String.fromCharCode(0x0020 + ENCODED_NUM_BASE)}`, 0), 0x0020);
      });
    });

    describe('with mock-inlining', () => {
      it('decompresses single-char strings', () => {
        assert.equal(decompressNumber(`xxx${String.fromCharCode(0x0020 + ENCODED_NUM_BASE)}xx`, 3, 4), 0x0020);
        assert.equal(decompressNumber(`xx${String.fromCharCode('"'.charCodeAt(0) + ENCODED_NUM_BASE)}x`, 2, 3), '"'.charCodeAt(0));
        assert.equal(decompressNumber('\uffff\ufffe', 1), 0xfffe - ENCODED_NUM_BASE);
      });

      it('decompresses two-char strings', () => {
        assert.equal(decompressNumber(`xxxx${compressNumber(0x00200020, 2)}xx`, 4, 6), 0x00200020);
      });
    });
  });

  describe('`Entry`s', () => {
    it('not inlined', () => {
      const mockedDecompression = TEST_DATA.ENTRIES.four.decompressed;
      const compressionSrc = TEST_DATA.ENTRIES.four.compressed;
      assert.deepEqual(decompressEntry(compressionSrc, identityKey), mockedDecompression);
    });

    it('inlined', () => {
      const mockedDecompression = TEST_DATA.ENTRIES.four.decompressed;

      // total length: header = 5, text = 8 -> 13.
      const compressionSrc = `xxxxx${TEST_DATA.ENTRIES.four.compressed}xx`;
      assert.deepEqual(decompressEntry(compressionSrc, identityKey, /* start index */ 5), mockedDecompression);
    });
  });

  describe('Leaf nodes', () => {
    describe('bootstrapping cases', () => {
      it('not inlined', () => {
        const encodedLeaf = TEST_DATA.LEAVES.four.compressed;
        assert.deepEqual(decompressNode(encodedLeaf, identityKey, 0), TEST_DATA.LEAVES.four.decompressed);
      });

      it('inlined', () => {
        const encodedLeaf = TEST_DATA.LEAVES.four.compressed;
        assert.deepEqual(decompressNode(`xxxxxxxxx${encodedLeaf}xx`, identityKey, 9), TEST_DATA.LEAVES.four.decompressed);
      });
    });
  });

  describe('Internal nodes', () => {
    describe('bootstrapping cases', () => {
      it('not inlined', () => {
        const encodedNode = TEST_DATA.NODES.four.compressed;
        assert.deepEqual(decompressNode(encodedNode, identityKey, 0), TEST_DATA.NODES.four.decompressed);
      });

      it('inlined', () => {
        const encodedNode = TEST_DATA.NODES.four.compressed;
        assert.deepEqual(decompressNode(`xxxxxxx${encodedNode}xx`, identityKey, 7), TEST_DATA.NODES.four.decompressed);
      });
    });
  });

  it('compresses fixture successfully: english-1000', () => {
    const trieFixture = jsonFixture('tries/english-1000');
    const trie = new TrieBuilder(identityKey, trieFixture.root, trieFixture.totalWeight);

    assert.doesNotThrow(() => { return {
      // The encoding pattern used above achives FAR better compression than
      // JSON.stringify, which \u-escapes most chars.  As of the commit when
      // this was written, before we stopped storing 'key' for entries...
      // - length of encoding below: 26097
      // - JSON-encoding length:  69122
      // - Source fixture's filesize: 141309 bytes
      root: `\`${trie.compress()}\``,
      totalWeight: trie.getTotalWeight()
    } });

    // The test:  did it throw?  If no, we'll assume we're good.
  });

  describe('surrogate-pair handling', () => {
    it('compresses a Trie with non-BMP characters without throwing an error', () => {
      const builder = new TrieBuilder(identityKey);
      smpWordlist.forEach((tuple) => builder.addEntry(tuple[0], tuple[1]));

      assert.doesNotThrow(() => builder.compress());
    });

    it('properly round-trips a Trie through compression and decompression', () => {
      const builder = new TrieBuilder(lowercaseKey);
      smpWordlist.forEach((tuple) => builder.addEntry(tuple[0], tuple[1]));

      const compressedTrie = builder.compress();
      const root = decompressNode(compressedTrie, lowercaseKey);
      assert.equal(root.weight, smpWordlist[0][1]);
      assert.sameDeepMembers(
        root.values, [
          'c',  /* keyed to lowercase */
          '🙄'.charAt(0), /* is non-BMP, shares same high surrogate as '😇' */
          '🇸'.charAt(0),  /* is non-BMP, uses a different high surrogate */
          'u'   /* was already lowercase */
        ]
      );
      root.values.forEach((key) => assert.isOk(root.children[key]));

      const roundtrippedTrie = new Trie(root, builder.getTotalWeight(), lowercaseKey);

      // Decompresses the path traversed; we wish to ensure it's usable even if
      // initially compressed.
      const uNode = roundtrippedTrie.traverseFromRoot().child('u');
      assert.isOk(uNode);
      const uEntries = uNode.entries;
      assert.isOk(uEntries);
      assert.sameDeepMembers(uEntries, [{text: 'u', p: 1.0 / builder.getTotalWeight()}]);

      // Tests a simple surrogate-pair path; this path must also cross an internal node
      // before reaching a leaf.
      const emojiNode = roundtrippedTrie.traverseFromRoot().child('🙄');
      assert.isOk(emojiNode);
      const emojiEntries = emojiNode.entries;
      assert.isOk(emojiEntries);
      assert.sameDeepMembers(emojiEntries.map((entry) => entry.text), ['🙄']);

      // This sequence tests that keying is fully in-place, even if keys aren't
      // directly emitted.
      const crazNode = roundtrippedTrie.traverseFromRoot().child('craz');
      // There is no internal node at this position; it's an intermediate stage
      // of traversal based upon the key in the leaf's entry.
      assert.isOk(crazNode);
      const crazyNode = crazNode.child('🤪');
      assert.isOk(crazyNode);
      const crazyEntries = crazyNode.entries;
      assert.isOk(crazyEntries);
      assert.sameDeepMembers(crazyEntries.map((entry) => entry.text), ['CRAZ🤪']);
    });
  });
});