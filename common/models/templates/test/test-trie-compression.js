/*
 * Unit tests for the Trie prediction model.
 */

import { assert } from 'chai';
import {
  compressEntry, decompressEntry,
  compressNode, decompressNode,
  compressNumber, decompressNumber,
  ENCODED_NUM_BASE,
  SINGLE_CHAR_RANGE
} from '@keymanapp/models-templates/obj/trie-compression.js';

/**
 * @param {string} str
 * @returns
 */
const identityKey = (str) => str;

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
    const trie = jsonFixture('tries/english-1000');
    // The test:  does it throw?
    const compressedTrie = compressNode(trie.root);

    const encodedSerializableString = `${
      compressedTrie
        .replace(/\\/g, '\\\\') // preservation - escape char as literal
        .replace(/`/g, '\\`') // escape the primary quote
        .replace(/\n/g, '\\n') // prevent CRLF shenanigans
        .replace(/\r/g, '\\r')
    }`;

    const compression = {
      // The encoding pattern used above achives FAR better compression than
      // JSON.stringify, which \u-escapes most chars.  As of the commit when
      // this was written, before we stopped storing 'key' for entries...
      // - length of encoding below: 26097
      // - JSON-encoding length:  69122
      // - Source fixture's filesize: 141309 bytes
      root: `\`${encodedSerializableString}\``,
      totalWeight: trie.totalWeight
    }

    // TODO:  Temp code for diagnostics & exploration.
    console.log(`Compressed length: ${compression.root.length}`);
    console.log(`Result: ${compression.root}`);

    // Note: a naive round-tripping test won't work.  We only partly decompress
    // at each step, after all.

//     // Chrome parses it safely, but VS Code complains about the encoding.
//     // > "The file is not displayed in the text editor because it is either binary or uses an unsupported text encoding."
//     // - likely b/c the "binary" angle.
//     //
//     // Complaints are dropped if all encoded numbers are offset by +0x0020.
//     // - This does narrow the range of representable values a bit, though.
//     // - It -also- brings manual encoding and JSON encoding into near-parity;
//     //   control char escapes were using a lot of space.
//     //
//     fs.writeFileSync('temptemp.js', `let trieData = {
//   "root": ${compression.root},
//   "totalWeight": ${compression.totalWeight}
// }`
//     );

//     fs.writeFileSync('temptemp.json', `{
//   "root": ${JSON.stringify(compressedTrie)},
//   "totalWeight": ${compression.totalWeight}
// }`
//     );
  });

//   it('compresses fixture successfully: sil.km.gcc - 1.0', () => {
//     const trie = jsonFixture('tries/sil.km.gcc - 1.0');
//     // The test:  does it throw?
//     const compressedTrie = compressNode(trie.root);

//     const encodedSerializableString = `${
//       compressedTrie
//         .replace(/\\/g, '\\\\') // preservation - escape char as literal
//         .replace(/`/g, '\\`') // escape the primary quote
//         .replace(/\n/g, '\\n') // prevent CRLF shenanigans
//         .replace(/\r/g, '\\r')
//     }`;

//     const compression = {
//       // Achieves FAR better compression than JSON.stringify, which \u-escapes most chars.
//       // As of the commit when this was written...
//       // - length of encoding below: 405468
//       // - JSON-encoding length:  1145955
//       // - Source fixture's filesize: 2491696 bytes
//       //
//       // As is... it compressed in 99ms on my personal development machine.
//       root: `"${encodedSerializableString}"`,
//       totalWeight: trie.totalWeight
//     }

//     // We could easily get even better savings (with some cost) by using the search-term-to-key function
//     // as part of decompression, avoiding the near-duplicating key/content effect we currently have.

//     // TODO:  Temp code for diagnostics & exploration.
//     console.log(`Compressed length: ${compression.root.length}`);
//     // console.log(`Result: ${compression.root}`);

//     // Note: a naive round-tripping test won't work.  We only partly decompress
//     // at each step, after all.

//     fs.writeFileSync('temptemp.js', `let trieData = {
//   "root": ${compression.root},
//   "totalWeight": ${compression.totalWeight}
// }`
//     );

//     fs.writeFileSync('temptemp.json', `{
//   "root": ${JSON.stringify(compressedTrie)},
//   "totalWeight": ${compression.totalWeight}
// }`
//     );
//   });
});