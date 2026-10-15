/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-09
 *
 * This file defines tests for the correction-searching process of the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import {
  ExecutionTimer,
  generateSpaceSeed,
  getBestTokenMatches,
  LegacyQuotientSpur,
  models,
  LegacyQuotientRoot,
  SearchQuotientCluster,
  TokenResultMapping,
  CORRECTION_QUEUE_COMPARATOR
} from '@keymanapp/lm-worker/test-index';

import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

function buildTestTimer() {
  return new ExecutionTimer(Number.MAX_VALUE, Number.MAX_VALUE);
}

describe('Correction Searching', () => {
  describe('without multi-tokenization; using a single SearchPath sequence', () => {
    const checkRepeatableResults_teh = async (iter: AsyncGenerator<Readonly<TokenResultMapping>, any, any>) => {
      const expectedFirstTwenty = [
        'ten', // no edits required whatsoever
        'th',  // one edit (deletion), but gets to ignore the cost of a keystroke and still prefixes 'the'
        'the', // one edit (transposition), incurs the cost of all three keystrokes
        'te',  // one edit (deletion), but gets to ignore the cost of a keystroke
        'tel', 'beh', // both cost one edit (hard character replacement:  n/h -> l vs t -> b)
        // Other edits, generally of one edit cost, predicting words of varying frequency.
        'ter', 'tha', 'thi', 'thr', 'tho', 'tem', 'thu', 'then', 'men', 'wen', 'gen', 'en', 'sen', 'tec'
      ];

      let results: TokenResultMapping[] = [];
      for(let i = 0; i < expectedFirstTwenty.length; i++) {
        const iterResult = await iter.next();
        results.push(iterResult.value as TokenResultMapping);
      }

      assert.sameOrderedMembers(results.map((r) => r.matchString), expectedFirstTwenty);
      for(let i=0; i < expectedFirstTwenty.length - 1; i++) {
        assert.isAtLeast(results[i+1].totalCost, results[i].totalCost);
      }

      // The results will not be in the order as raw correction likelihood because some words
      // are more frequent than others.
      results.sort(CORRECTION_QUEUE_COMPARATOR);
      assert.notSameOrderedMembers(results.map((r) => r.matchString), expectedFirstTwenty);
    }

    it('Empty search root, loaded model', async () => {
      // The combinatorial effect here is a bit much to fully test.
      const rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      const searchSpace = new LegacyQuotientRoot(testModel);
      const timer = buildTestTimer();
      const iter = getBestTokenMatches([searchSpace], timer);

      // While there's no input, insertion operations can produce suggestions.
      const resultState = await iter.next();
      const result = resultState.value as TokenResultMapping;

      // Just one suggestion root should be returned as the first result.
      assert.equal(result.correctionCost, 0);             // Gives a perfect match
      assert.equal(result.matchString, '');          // an empty match string.
      assert.isFalse(resultState.done);
      assert.isAbove(result.totalCost, 0);
    });

    // Hmm... how best to update this...
    it('Simple search (paralleling "Small integration test")', async () => {
      // The combinatorial effect here is a bit much to fully test.
      const rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      const searchPath = new LegacyQuotientRoot(testModel);

      // VERY artificial distributions.
      const synthInput1 = [
        {sample: {insert: 't', deleteLeft: 0}, p: 1} // Transform, probability
      ];

      const synthInput2 = [
        {sample: {insert: 'e', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.25}
      ];

      const synthInput3 = [
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'n', deleteLeft: 0}, p: 0.25}
      ];

      const searchPath1 = new LegacyQuotientSpur(searchPath, synthInput1, synthInput1[0]);
      const searchPath2 = new LegacyQuotientSpur(searchPath1, synthInput2, synthInput2[0]);
      const searchPath3 = new LegacyQuotientSpur(searchPath2, synthInput3, synthInput3[0]);

      assert.notEqual(searchPath1.spaceId, searchPath.spaceId);
      assert.notEqual(searchPath2.spaceId, searchPath1.spaceId);
      assert.notEqual(searchPath3.spaceId, searchPath2.spaceId);

      const iter = getBestTokenMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
      await checkRepeatableResults_teh(iter);
    });

    it('Allows reiteration (sequentially)', async () => {
      // The combinatorial effect here is a bit much to fully test.
      const rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      const searchPath = new LegacyQuotientRoot(testModel);

      // VERY artificial distributions.
      const synthInput1 = [
        {sample: {insert: 't', deleteLeft: 0}, p: 1} // Transform, probability
      ];

      const synthInput2 = [
        {sample: {insert: 'e', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.25}
      ];

      const synthInput3 = [
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'n', deleteLeft: 0}, p: 0.25}
      ];

      const searchPath1 = new LegacyQuotientSpur(searchPath, synthInput1, synthInput1[0]);
      const searchPath2 = new LegacyQuotientSpur(searchPath1, synthInput2, synthInput2[0]);
      const searchPath3 = new LegacyQuotientSpur(searchPath2, synthInput3, synthInput3[0]);

      assert.notEqual(searchPath1.spaceId, searchPath.spaceId);
      assert.notEqual(searchPath2.spaceId, searchPath1.spaceId);
      assert.notEqual(searchPath3.spaceId, searchPath2.spaceId);

      const iter = getBestTokenMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
      await checkRepeatableResults_teh(iter);

      // The key: do we get the same results the second time?
      // Reset the iterator first...
      const iter2 = getBestTokenMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
      await checkRepeatableResults_teh(iter2);
    });

    describe('with divergent SearchSpaces', () => {
      const buildPathFixture = () => {
        const rootPath = new LegacyQuotientRoot(testModel);

        const distrib_t1 = [
          { sample: { insert: 't', deleteLeft: 0, id: 11 }, p: 1 }
        ];
        const tPath = new LegacyQuotientSpur(rootPath, distrib_t1, distrib_t1[0]);

        // Note:  this does not reflect the actual intended use pattern for these
        // types. It's useful for clear testing, though.
        //
        // In particular, this test is acting as if the following characters
        // wouldn't be part of the same TokenizationPath, yet also using the same
        // subsetId, as if they were part of the same TokenizationPath.
        const distrib_h2 = [
          { sample: { insert: 'h', deleteLeft: 0, id: 12 }, p: 0.5 }
        ];
        const distrib_o2 = [
          { sample: { insert: 'o', deleteLeft: 0, id: 12 }, p: 0.3 }
        ];
        const distrib_r2 = [
          { sample: { insert: 'r', deleteLeft: 0, id: 12 }, p: 0.2 }
        ];

        const thPath = new LegacyQuotientSpur(tPath, distrib_h2, distrib_h2[0]);
        const toPath = new LegacyQuotientSpur(tPath, distrib_o2, thPath.inputSource);
        const trPath = new LegacyQuotientSpur(tPath, distrib_r2, thPath.inputSource);

        const twoCharCluster = new SearchQuotientCluster([thPath, toPath, trPath]);

        const distrib_v3 = [
          { sample: { insert: 'e', deleteLeft: 0, id: 13 }, p: 0.4 },
          { sample: { insert: 'o', deleteLeft: 0, id: 13 }, p: 0.3 },
          { sample: { insert: 'a', deleteLeft: 0, id: 13 }, p: 0.2 },
          { sample: { insert: 'i', deleteLeft: 0, id: 13 }, p: 0.1 }
        ];

        const thvPath = new LegacyQuotientSpur(thPath, distrib_v3, distrib_v3[0]);
        const tovPath = new LegacyQuotientSpur(toPath, distrib_v3, thvPath.inputSource);
        const trvPath = new LegacyQuotientSpur(trPath, distrib_v3, thvPath.inputSource);

        const clvPath = new LegacyQuotientSpur(twoCharCluster, distrib_v3, thvPath.inputSource);

        const distrib_n4 = [
          { sample: { insert: 'n', deleteLeft: 0, id: 14 }, p: 0.4 },
          { sample: { insert: 'u', deleteLeft: 0, id: 14 }, p: 0.1 }
        ];
        const distrib_v3r = [
          { sample: { insert: 'é', deleteLeft: 1, id: 14 }, p: 0.2 },
          { sample: { insert: 'ó', deleteLeft: 1, id: 14 }, p: 0.15 },
          { sample: { insert: 'á', deleteLeft: 1, id: 14 }, p: 0.1 },
          { sample: { insert: 'í', deleteLeft: 1, id: 14 }, p: 0.05 }
        ];

        const thvnPath = new LegacyQuotientSpur(thvPath, distrib_n4, distrib_n4[0]);
        const tovnPath = new LegacyQuotientSpur(tovPath, distrib_n4, thvnPath.inputSource);
        const trvnPath = new LegacyQuotientSpur(trvPath, distrib_n4, thvnPath.inputSource);
        const clvnPath = new LegacyQuotientSpur(clvPath, distrib_n4, thvnPath.inputSource);

        const thvrPath = new LegacyQuotientSpur(thvPath, distrib_v3r, {...thvnPath.inputSource, subsetId: generateSpaceSeed()});
        const tovrPath = new LegacyQuotientSpur(tovPath, distrib_v3r, thvrPath.inputSource);
        const trvrPath = new LegacyQuotientSpur(trvPath, distrib_v3r, thvrPath.inputSource);
        const clvrPath = new LegacyQuotientSpur(clvPath, distrib_v3r, thvrPath.inputSource);

        const paths = {
          clusterless: {
            thvnPath, tovnPath, trvnPath, thvrPath, tovrPath, trvrPath
          },
          clustered: {
            clvnPath, clvrPath
          }
        };

        const clusterVsPaths = {
          paths: {
            thPath, trPath, toPath
          },
          cluster: twoCharCluster
        };

        return {paths, clusterVsPaths};
      }

      it('correctly searches across multiple paths with common ancestry (clusterless)', async () => {
        const paths = buildPathFixture().paths.clusterless;

        const gen_thvn = getBestTokenMatches([paths.thvnPath], buildTestTimer());
        assert.equal(((await gen_thvn.next()).value as TokenResultMapping).matchString, 'then');

        // Passes through both t and h, then diverges.
        const gen_thvr = getBestTokenMatches([paths.thvrPath], buildTestTimer());
        assert.equal(((await gen_thvr.next()).value as TokenResultMapping).matchString, 'the');

        // Passes through t, then diverges
        const gen_trvn = getBestTokenMatches([paths.trvnPath], buildTestTimer());
        assert.equal(((await gen_trvn.next()).value as TokenResultMapping).matchString, 'trou');

        // Passes through t and r, then diverges.
        const gen_trvr = getBestTokenMatches([paths.trvrPath], buildTestTimer());

        // Verify that each divergent QuotientNode has further expected results if we keep querying.
        assert.equal(((await gen_trvr.next()).value as TokenResultMapping).matchString, 'tre');
        assert.equal(((await gen_trvr.next()).value as TokenResultMapping).matchString, 'tro');
        assert.equal(((await gen_trvr.next()).value as TokenResultMapping).matchString, 'tra');
      });

      it('correctly searches across multiple paths with common ancestry (clustered)', async () => {
        const paths = buildPathFixture().paths.clustered;

        const gen_clvn = getBestTokenMatches([paths.clvnPath], buildTestTimer());
        const clvnMatches: string[] = [];

        for(let i=0; i < 10; i++) {
          clvnMatches.push(((await gen_clvn.next()).value as TokenResultMapping).matchString);
        }
        assert.includeMembers(clvnMatches, ['then', 'than', 'thin', 'thou', 'trou']);

        // Passes through both t and h, then diverges.
        const gen_clvr = getBestTokenMatches([paths.clvrPath], buildTestTimer());
        const clvrMatches: string[] = [];

        for(let i=0; i < 10; i++) {
          clvrMatches.push(((await gen_clvr.next()).value as TokenResultMapping).matchString);
        }
        assert.includeMembers(clvrMatches, ['the', 'tho', 'tha', 'tre', 'tro', 'thi']);
      });

      it('correctly searches across multiple paths when search is unevenly staggered', async () => {
        const isolatedPaths = buildPathFixture().paths.clustered;

        const gen_clvn1 = getBestTokenMatches([isolatedPaths.clvnPath], buildTestTimer());
        const isolatedClvnMatches: Set<string> = new Set();

        const SET_COUNT = 3;
        const COUNT_PER_SET = 4;
        const TOTAL_COUNT = SET_COUNT * COUNT_PER_SET;

        while(isolatedClvnMatches.size < TOTAL_COUNT) {
          isolatedClvnMatches.add(((await gen_clvn1.next()).value as TokenResultMapping).matchString);
        }

        // Passes through both t and h, then diverges.
        const gen_clvr1 = getBestTokenMatches([isolatedPaths.clvrPath], buildTestTimer());
        const isolatedClvrMatches: Set<string> = new Set();

        while(isolatedClvrMatches.size < TOTAL_COUNT) {
          isolatedClvrMatches.add(((await gen_clvr1.next()).value as TokenResultMapping).matchString);
        }

        // Rebuild anew, and stagger searching four at a time on each, landing on 12 in total per.
        const paths = buildPathFixture().paths.clustered;

        const gen_clvn2 = getBestTokenMatches([paths.clvnPath], buildTestTimer());
        const gen_clvr2 = getBestTokenMatches([paths.clvrPath], buildTestTimer());

        const clvnMatches: Set<string> = new Set();
        const clvrMatches: Set<string> = new Set();

        // Follow the search paths in a staggered manner; this may cause some
        // results in one to be of higher cost than what's available from the
        // other.
        for(let s=0; s < SET_COUNT; s++) {
          const SET_MAX = (s + 1) * COUNT_PER_SET;
          while(clvnMatches.size < SET_MAX) {
            clvnMatches.add(((await gen_clvn2.next()).value as TokenResultMapping).matchString);
          }

          while(clvrMatches.size < SET_MAX) {
            clvrMatches.add(((await gen_clvr2.next()).value as TokenResultMapping).matchString);
          }
        }

        assert.sameDeepMembers([...clvnMatches], [...isolatedClvnMatches]);
        assert.sameDeepMembers([...clvrMatches], [...isolatedClvrMatches]);
      });

      it('returns the same results, in order, from SearchCluster as from constituent SearchPaths', async () => {
        // See issue #14366 - duplicate results may appear due to a later
        // right-delete having a lower-cost total than its parent.  We use `Set`s
        // here to avoid duplicate issues and look simply at what results arise
        // first.
        //
        // From the fixture's construction, note `distrib_v3` and `distrib_v3r`.
        // The "duplicate results" scenario arises when the key selected from
        // `distrib_v3` does not match, but is deleted and replaced by a valid key
        // from `distrib_v3r`.  As the latter is reached later, with lower cost,
        // it does get reported again.  Resolving #14366 properly should help
        // mitigate this issue.

        // ---

        // Build independently; let the cluster own a separate, disconnected copy of the paths.
        const {paths: pathTest}   = buildPathFixture().clusterVsPaths;

        // Validate that the paths individually return the following match strings.
        const gen_th = getBestTokenMatches([pathTest.thPath], buildTestTimer());
        assert.equal(((await gen_th.next()).value as TokenResultMapping).matchString, 'th');

        const gen_to = getBestTokenMatches([pathTest.toPath], buildTestTimer());
        assert.equal(((await gen_to.next()).value as TokenResultMapping).matchString, 'to');

        const gen_tr = getBestTokenMatches([pathTest.trPath], buildTestTimer());
        assert.equal(((await gen_tr.next()).value as TokenResultMapping).matchString, 'tr');

        // And now for the real test.

        const {cluster} = buildPathFixture().clusterVsPaths;
        // Build independently; let the cluster own a separate, disconnected copy of the paths.
        const {paths}   = buildPathFixture().clusterVsPaths;

        const clusterGen = getBestTokenMatches([cluster], buildTestTimer());
        const pathsGen   = getBestTokenMatches([...Object.values(paths)], buildTestTimer());

        const genResults: TokenResultMapping[] = [];
        const pathsResults: TokenResultMapping[] = [];

        // Changes to implementation could cause a slight reordering of equal-cost entries.
        // Take all entries within a set cost instead.
        let baseCost = 0;
        while(baseCost < 6) {
          const nextFromCluster = (await clusterGen.next()).value as TokenResultMapping;
          const nextFromPaths   = (await pathsGen.next()).value as TokenResultMapping;
          genResults.push(nextFromCluster);
          // This one can see duplicates for some prefixes due to some paths having outbound
          // paths of lower total cost.
          pathsResults.push(nextFromPaths);

          assert.isAtLeast(nextFromCluster.correctionCost, baseCost);
          assert.isAtLeast(nextFromPaths.correctionCost, baseCost);
          baseCost = Math.max(baseCost, nextFromCluster.correctionCost, nextFromPaths.correctionCost);
        }

        assert.deepEqual(genResults.map(r => r.matchString), pathsResults.map(r => r.matchString));

        // Ensure that all of the clearly-supported prefixes above show up as results.
        assert.sameDeepMembers(pathsResults.slice(0, 3).map(r => r.matchString), ['th', 'to', 'tr']);
        // These involve likely-enough corrections that should show, given the model fixture.
        assert.includeDeepMembers(pathsResults.map(r => r.matchString), [
          't',  // Deleting the second keystroke outright lands here.
          'oth', // What if we insert an 'o' early on?  'other' is a very common English word
          'ti', // 'time' is pretty common too.
          'thi', // 'this' is common enough to show up early despite inserting the 'i'.
          'wh', // "which" is a super-frequent English word, worthy of being a forced correction
          'sh'  // "she" is also strong enough to force an early appearance.
        ]);
      });
    });
  });
});
