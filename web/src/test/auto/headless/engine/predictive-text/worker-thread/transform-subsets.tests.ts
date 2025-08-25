/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-08-21
 *
 * This file defines tests for the transform-cataloging methods
 * used to assist the predictive-text correction-search heuristic.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';

import { subsetByChar, subsetByInterval, TransformSubset} from '@keymanapp/lm-worker/test-index';

import Transform = LexicalModelTypes.Transform;

function validateSubset<T>(subset: TransformSubset<T>, key: T, entryTexts: string[]) {
  assert.sameMembers(subset.entries.map(t => t.sample.insert), entryTexts);
  assert.equal(subset.cumulativeMass, entryTexts.length);
  assert.equal(subset.key, key);
}

describe('subsetByInterval', () => {
  it('properly arranges transforms into subsets based on range of Context application', () => {
    const transforms: Transform[] = [
      { insert: 'a', deleteLeft: 0 },
      { insert: 'b', deleteLeft: 0 },
      { insert: 'c', deleteLeft: 0 },
      { insert: 'de', deleteLeft: 0 },
      { insert: 'fg', deleteLeft: 0 },
      { insert: 'hij', deleteLeft: 0 },
      { insert: 'k', deleteLeft: 1 },
      { insert: 'l', deleteLeft: 1 },
      { insert: 'm', deleteLeft: 1 },
      { insert: 'no', deleteLeft: 1 },
      { insert: 'pq', deleteLeft: 1 },
      { insert: 'rs', deleteLeft: 2 },
      { insert: 'tu', deleteLeft: 2 },
      { insert: 'vw', deleteLeft: 2 },
      { insert: 'xyz', deleteLeft: 2 },
      { insert: '', deleteLeft: 4}
    ];

    const distribution = transforms.map(t => ({sample: t, p: 1}));
    const catalogs = subsetByInterval(distribution);

    assert.equal(catalogs.length, 5); // all keys <= 4, the max deleteLeft.
    catalogs.forEach((k, i) => assert.isOk(catalogs[i]));

    assert.equal(catalogs[0].length, 4); // max insert.length = 3
    assert.isNotOk(catalogs[0][0]);
    validateSubset(catalogs[0][1], 1, ['a', 'b', 'c']);
    validateSubset(catalogs[0][2], 2, ['de', 'fg']);
    validateSubset(catalogs[0][3], 3, ['hij']);
    assert.equal(catalogs[1].length, 3); // max insert.length = 2
    assert.isNotOk(catalogs[1][0]);
    validateSubset(catalogs[1][1], 1, ['k', 'l', 'm']);
    validateSubset(catalogs[1][2], 2, ['no', 'pq']);
    assert.isNotOk(catalogs[2][0]);
    assert.isNotOk(catalogs[2][1]);
    assert.equal(catalogs[2].length, 4); // max insert.length = 3
    validateSubset(catalogs[2][2], 2, ['rs', 'tu', 'vw']);
    assert.isNotOk(catalogs[3]); // no deleteLeft = 3 entries
    assert.equal(catalogs[4].length, 1); // max insert.length = 0
    validateSubset(catalogs[4][0], 0, ['']);
  });
});

describe('subsetByChar', () => {
  describe('without search-term keying', () => {
    const transforms: Transform[] = [
      { insert: 'ace', deleteLeft: 0 },
      { insert: 'add', deleteLeft: 0 },
      { insert: 'ade', deleteLeft: 0 },
      { insert: 'ado', deleteLeft: 0 },
      { insert: 'cab', deleteLeft: 0 },
      { insert: 'cad', deleteLeft: 0 },
      { insert: 'can', deleteLeft: 0 },
      { insert: 'cod', deleteLeft: 0 },
      { insert: 'con', deleteLeft: 0 },
      { insert: 'dab', deleteLeft: 0 },
      { insert: 'dad', deleteLeft: 0 },
      { insert: 'dan', deleteLeft: 0 },
      { insert: 'don', deleteLeft: 0 }
    ];

    const sourceSet: TransformSubset<number> = {
      key: 3,
      entries: transforms.map((t => ({sample: t, p: 1}))),
      cumulativeMass: transforms.length,
      insert: ''
    };

    it('places transforms into subsets based on char at index 0', () => {
      const subsetMap = subsetByChar(sourceSet, 0);

      assert.sameMembers([...subsetMap.keys()], ['a', 'c', 'd']);

      validateSubset(subsetMap.get('a'), 'a', ['ace', 'add', 'ade', 'ado']);
      validateSubset(subsetMap.get('c'), 'c', ['cab', 'cad', 'can', 'cod', 'con']);
      validateSubset(subsetMap.get('d'), 'd', ['dab', 'dad', 'dan', 'don']);
    });

    it('places transforms into subsets based on char at later index', () => {
      const subsetMap1 = subsetByChar(sourceSet, 1);

      assert.sameMembers([...subsetMap1.keys()], ['a', 'c', 'd', 'o']);

      validateSubset(subsetMap1.get('a'), 'a', ['cab', 'cad', 'can', 'dab', 'dad', 'dan']);
      validateSubset(subsetMap1.get('c'), 'c', ['ace']);
      validateSubset(subsetMap1.get('d'), 'd', ['add', 'ade', 'ado']);
      validateSubset(subsetMap1.get('o'), 'o', ['cod', 'con', 'don']);

      const subsetMap2 = subsetByChar(sourceSet, 2);

      assert.sameMembers([...subsetMap2.keys()], ['b', 'd', 'e', 'n', 'o']);

      validateSubset(subsetMap2.get('b'), 'b', ['cab', 'dab']);
      validateSubset(subsetMap2.get('d'), 'd', ['add', 'cad', 'cod', 'dad']);
      validateSubset(subsetMap2.get('e'), 'e', ['ace', 'ade']);
      validateSubset(subsetMap2.get('n'), 'n', ['can', 'con', 'dan', 'don']);
      validateSubset(subsetMap2.get('o'), 'o', ['ado']);
    });

    it('returns full set as \'\'-keyed subset when subindex is beyond range', () => {
      const subsetMap = subsetByChar(sourceSet, 4);

      assert.sameMembers([...subsetMap.keys()], ['']);

      validateSubset(subsetMap.get(''), '', transforms.map(t => t.insert));
    });
  });

  describe('with search-term keying', () => {
    // In essence, our original version of the search-term keyer function.
    // It's best to keep this one stable and locked for these tests.
    function toKey(wordform: string): string {
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
        .toLowerCase();
    }

    const transforms: Transform[] = [
      { insert: 'acé', deleteLeft: 0 },
      { insert: 'Add', deleteLeft: 0 },
      { insert: 'aDe', deleteLeft: 0 },
      { insert: 'àdo', deleteLeft: 0 },
      { insert: 'cab', deleteLeft: 0 },
      { insert: 'càd', deleteLeft: 0 },
      { insert: 'çan', deleteLeft: 0 },
      { insert: 'cÕd', deleteLeft: 0 },
      { insert: 'çon', deleteLeft: 0 },
      { insert: 'dab', deleteLeft: 0 },
      { insert: 'dàd', deleteLeft: 0 },
      { insert: 'dän', deleteLeft: 0 },
      { insert: 'doN', deleteLeft: 0 }
    ];

    const sourceSet: TransformSubset<number> = {
      key: 3,
      entries: transforms.map((t => ({sample: t, p: 1}))),
      cumulativeMass: transforms.length,
      insert: ''
    };

    it('places transforms into subsets based on char at index 0', () => {
      const subsetMap = subsetByChar(sourceSet, 0, toKey);

      assert.sameMembers([...subsetMap.keys()], ['a', 'c', 'd']);

      validateSubset(subsetMap.get('a'), 'a', ['acé', 'Add', 'aDe', 'àdo']);
      validateSubset(subsetMap.get('c'), 'c', ['cab', 'càd', 'çan', 'cÕd', 'çon']);
      validateSubset(subsetMap.get('d'), 'd', ['dab', 'dàd', 'dän', 'doN']);
    });

    it('places transforms into subsets based on char at later index', () => {
      const subsetMap1 = subsetByChar(sourceSet, 1, toKey);

      assert.sameMembers([...subsetMap1.keys()], ['a', 'c', 'd', 'o']);

      validateSubset(subsetMap1.get('a'), 'a', ['cab', 'càd', 'çan', 'dab', 'dàd', 'dän']);
      validateSubset(subsetMap1.get('c'), 'c', ['acé']);
      validateSubset(subsetMap1.get('d'), 'd', ['Add', 'aDe', 'àdo']);
      validateSubset(subsetMap1.get('o'), 'o', ['cÕd', 'çon', 'doN']);

      const subsetMap2 = subsetByChar(sourceSet, 2, toKey);

      assert.sameMembers([...subsetMap2.keys()], ['b', 'd', 'e', 'n', 'o']);

      validateSubset(subsetMap2.get('b'), 'b', ['cab', 'dab']);
      validateSubset(subsetMap2.get('d'), 'd', ['Add', 'càd', 'cÕd', 'dàd']);
      validateSubset(subsetMap2.get('e'), 'e', ['acé', 'aDe']);
      validateSubset(subsetMap2.get('n'), 'n', ['çan', 'çon', 'dän', 'doN']);
      validateSubset(subsetMap2.get('o'), 'o', ['àdo']);
    });
  })
});