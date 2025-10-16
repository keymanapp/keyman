/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-09
 *
 * This file defines tests for the SearchSpace class of the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { correction, getBestMatches, models, SearchCluster, SearchPath } from '@keymanapp/lm-worker/test-index';

import SearchResult = correction.SearchResult;
import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

function buildTestTimer() {
  return new correction.ExecutionTimer(Number.MAX_VALUE, Number.MAX_VALUE);
}

const cantTestSetup = () => {
  const rootSpace = new SearchCluster(testModel);
  // The exact set of inputs and spaces here is a bit contrived, but the
  // design does accommodate this setup.

  // 'c' space
  const c1_t1_space = new SearchCluster([rootSpace.addInput([
    {sample: {insert: 'c', deleteLeft: 0}, p: 0.5}
  ], 0.5)]);
  const c_an_path = c1_t1_space.addInput([
    {sample: {insert: 'an', deleteLeft: 0}, p: 0.25}
  ], 0.5);
  const c2_t1_space = new SearchCluster([rootSpace.addInput([
    {sample: {insert: 'ca', deleteLeft: 0}, p: 0.5}
  ], 0.25)]);
  const ca_n_path = c2_t1_space.addInput([
    {sample: {insert: 'n', deleteLeft: 0}, p: 0.5}
  ], 0.5);

  // 3 codepoints in 2 transforms.
  // c + an
  // ca + n
  const c3_t2_space = new SearchCluster([c_an_path, ca_n_path]);

  // q for `'`, the single-Quote.
  const c_an_q_path = c3_t2_space.addInput([
    {sample: {insert: "'", deleteLeft: 0}, p: 0.5}
  ], 0.5);

  // c + a
  const c_a_path = c1_t1_space.addInput([
    {sample: {insert: 'a', deleteLeft: 0}, p: 0.25}
  ], 0.5);

  const c3_t1_space = new SearchCluster([rootSpace.addInput([
    {sample: {insert: 'cam', deleteLeft: 0}, p: 0.125},
    {sample: {insert: 'can', deleteLeft: 0}, p: 0.125}
  ], 0.5)]);

  // [cam, can] + dl1
  const ca_dl1_path = c3_t1_space.addInput([
    {sample: {insert: '', deleteLeft: 1}, p: 0.25}
  ], 0.5);

  const c2_t2_space = new SearchCluster([c_a_path, ca_dl1_path]);

  // c + a + n'
  // [cam, can] + dl1 + n'
  const c_a_nq_path = c2_t2_space.addInput([
    {sample: {insert: 'n\'', deleteLeft: 0}, p: 0.4}
  ], 0.5);

  // [cam, can] + "
  const c4_t2_space = new SearchCluster([c3_t1_space.addInput([
    {sample: {insert: '"', deleteLeft: 0}, p: 0.125},
  ], 0.5)]);
  const can_q_dl1q_path = c4_t2_space.addInput([
    {sample: {insert: "'", deleteLeft: 1}, p: 0.125},
  ], 0.5);

  const c4_t3_space = new SearchCluster([c_an_q_path, c_a_nq_path, can_q_dl1q_path]);
  const c_an_q_t_path = c4_t3_space.addInput([
    {sample: {insert: "t", deleteLeft: 0}, p: 0.9}
  ], 0.9);

  // c + a + n
  const c3_t3_space = new SearchCluster([c2_t2_space.addInput([
    {sample: {insert: "n", deleteLeft: 0}, p: 0.125},
  ], 0.5)])
  // c + a + n + 't
  const c_a_n_qt_path = c3_t3_space.addInput([
    {sample: {insert: "'t", deleteLeft: 0}, p: 0.1},
  ], 0.9);

  // ALL above lead here:  5 chars (`can't`) in 4 transforms.
  const c5_4t_space = new SearchCluster([c_an_q_t_path, c_a_n_qt_path]);

  return {
    paths: {
      c_a_path,
      c_an_path,
      ca_dl1_path,
      ca_n_path,
      c_an_q_path,
      c_an_q_t_path,
      c_a_nq_path,
      can_q_dl1q_path,
      c_a_n_qt_path
    },
    spaces: {
      rootSpace,
      c1_t1_space,
      c2_t1_space,
      c2_t2_space,
      c3_t1_space,
      c3_t2_space,
      c3_t3_space,
      c4_t2_space,
      c4_t3_space,
      c5_4t_space
    }
  };
}

describe('SearchSpace + SearchPath', () => {
  const checkRepeatableResults_teh = async (iter: AsyncGenerator<correction.SearchResult, any, any>, expectedSpaceId: number) => {
    const firstIterResult = await iter.next();  // {value: <actual value>, done: <iteration complete?>}
    assert.isFalse(firstIterResult.done);

    const firstResult: correction.SearchResult = firstIterResult.value; // Retrieves <actual value>
    // No checks on the first set's cost.
    assert.equal(firstResult.matchString, "ten");
    assert.equal(firstResult.spaceId, expectedSpaceId);

    // All start with 'te' but one, and invoke one edit of the same cost.
    // 'th' has an 'h' at the same cost (input 3) of the 'e' (input 2).
    const secondBatch = [
      'tec', 'tel', 'tem',
      'ter', 'tes', 'th',
      'te'
    ];

    async function checkBatch(batch: string[], prevCost: number) {
      let cost;
      while(batch.length > 0) {
        const iter_result = await iter.next();
        assert.isFalse(iter_result.done);

        const result = iter_result.value;
        assert.isAbove(result.totalCost, prevCost);
        if(cost !== undefined) {
          assert.equal(result.totalCost, cost);
        } else {
          cost = result.totalCost;
        }
        assert.equal(result.spaceId, expectedSpaceId);

        const matchIndex = batch.findIndex((entry) => entry == result.matchString);
        assert.notEqual(matchIndex, -1, `'${result.matchString}' received as prediction too early`);
        batch.splice(matchIndex, 1);
      }

      return cost;
    }

    const secondCost = await checkBatch(secondBatch, firstResult.totalCost);

    // Single hard edit, all other input probability aspects are equal
    const thirdBatch = [
      // 't' -> 'b' (sub)
      'beh',
      // '' -> 'c' (insertion)
      'tech'
    ];

    await checkBatch(thirdBatch, secondCost);

    // All replace the low-likelihood case for the third input.
    const fourthBatch = [
      'the', 'thi', 'tho', 'thr',
      'thu', 'tha'
    ];

    await checkBatch(fourthBatch, secondCost);

    // Replace the _first_ input's char OR insert an extra char,
    // also matching the low-likelihood third-char option.
    const fifthBatch = [
      'cen', 'en',  'gen',
      'ken', 'len', 'men',
      'sen', 'then', 'wen'
    ];

    await checkBatch(fifthBatch, secondCost);
  }

  it('Simple search without input', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    const searchSpace = new SearchCluster(testModel);

    const iter = getBestMatches([searchSpace], buildTestTimer());
    const firstResult = await iter.next();
    assert.isFalse(firstResult.done);
  });

  // Hmm... how best to update this...
  it('Simple search (paralleling "Small integration test")', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    const searchPath = new SearchPath(testModel);

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

    const searchPath1 = new SearchPath(searchPath, synthInput1, 1);
    const searchPath2 = new SearchPath(searchPath1, synthInput2, .75);
    const searchPath3 = new SearchPath(searchPath2, synthInput3, .75);

    assert.notEqual(searchPath1.spaceId, searchPath.spaceId);
    assert.notEqual(searchPath2.spaceId, searchPath1.spaceId);
    assert.notEqual(searchPath3.spaceId, searchPath2.spaceId);

    const iter = getBestMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter, searchPath3.spaceId);
  });

  it('Allows reiteration (sequentially)', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    const searchPath = new SearchPath(testModel);

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

    const searchPath1 = new SearchPath(searchPath, synthInput1, 1);
    const searchPath2 = new SearchPath(searchPath1, synthInput2, .75);
    const searchPath3 = new SearchPath(searchPath2, synthInput3, .75);

    assert.notEqual(searchPath1.spaceId, searchPath.spaceId);
    assert.notEqual(searchPath2.spaceId, searchPath1.spaceId);
    assert.notEqual(searchPath3.spaceId, searchPath2.spaceId);

    const iter = getBestMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter, searchPath3.spaceId);

    // The key: do we get the same results the second time?
    // Reset the iterator first...
    const iter2 = getBestMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter2, searchPath3.spaceId);
  });

  it('Empty search space, loaded model', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    const searchSpace = new SearchCluster(testModel);
    const timer = buildTestTimer();
    const iter = getBestMatches([searchSpace], timer);

    // While there's no input, insertion operations can produce suggestions.
    const resultState = await iter.next();
    const result: SearchResult = resultState.value;

    // Just one suggestion root should be returned as the first result.
    assert.equal(result.totalCost, 0);             // Gives a perfect match
    assert.equal(result.inputSequence.length, 0);  // for a state with no input and
    assert.equal(result.matchString, '');          // an empty match string.
    assert.isFalse(resultState.done);

    // Should be able to reach more, though.
    const laterResultState = await iter.next();
    const laterResult: SearchResult = laterResultState.value;

    // Edit required:  an 'insertion' edge (no input matched, but char pulled
    // from lexicon)
    assert.isAbove(laterResult.totalCost, 0);
    // The most likely word in the lexicon starts with 't'.
    assert.equal(laterResult.matchString, 't');
    assert.isFalse(resultState.done);
  });

  describe.skip('.inputSequences', () => {
    it('define tests here', () => {
      //
    });
  });

  describe("Splitting", () => {
    describe("on SearchPath", () => {
      it('handles simple cases built from two-char transforms', () => {
        let root = new SearchPath(testModel);
        let entries = ['ap', 'pl', 'es'];

        let path = root;
        for(let entry of entries) {
          path = new SearchCluster([path]).addInput([{sample: { insert: entry, deleteLeft: 0 }, p: .5}], .5);
        }

        const splitPath = path.split(1, testModel);
        assert.deepEqual(splitPath[0].bestExample, {
          text: 'apple',
          p: .125
        });
        assert.deepEqual(splitPath[1].bestExample, {
          text: 's',
          p: .5
        });
      });

      it('splits inputs with delete properly when needed - char index 0', () => {
        let root = new SearchPath(testModel);
        let entries = ['ap', 'pl', 'y'];

        let path = root;
        for(let entry of entries) {
          path = new SearchCluster([path]).addInput([{sample: { insert: entry, deleteLeft: 0 }, p: .5}], .5);
        }

        path = new SearchCluster([path]).addInput([{
          sample: { insert: 'es', deleteLeft: 1 }, p: .5
        }], .5)

        const splitPath = path.split(0, testModel); // do with a '1' too!
        assert.deepEqual(splitPath[0].bestExample, {
          text: 'appl',
          p: .0625
        });
        assert.deepEqual(splitPath[1].bestExample, {
          text: 'es',
          p: .5
        });
      });

      it('splits inputs with delete properly when needed - char index 1', () => {
        let root = new SearchPath(testModel);
        let entries = ['ap', 'pl', 'y'];

        let path = root;
        for(let entry of entries) {
          path = new SearchCluster([path]).addInput([{sample: { insert: entry, deleteLeft: 0 }, p: .5}], .5);
        }

        path = new SearchCluster([path]).addInput([{
          sample: { insert: 'es', deleteLeft: 1 }, p: .5
        }], .5)

        const splitPath = path.split(1, testModel); // do with a '1' too!
        assert.deepEqual(splitPath[0].bestExample, {
          text: 'apple',
          p: .0625
        });
        assert.deepEqual(splitPath[1].bestExample, {
          text: 's',
          p: .5
        });
      });
    });

    describe('on SearchSpace', () => {
      it('handles cases built solely from single-char transforms', () => {
        let rootSpace = new SearchCluster(testModel);
        const inputs = [...'apples'];

        let space = rootSpace;
        let lSpace: SearchCluster;
        for(const input of inputs) {
          space = new SearchCluster([space.addInput([{sample: {insert: input, deleteLeft: 0}, p: 0.5}], .5)]);

          if(input == 'l') {
            lSpace = space;
          }
        }

        const splitSpace = space.split(4, testModel);
        assert.equal(splitSpace.length, 1);
        const splitCase = splitSpace[0];
        assert.deepEqual(splitCase[0].bestExample, {
          text: 'appl',
          p: 0.0625 // .5 ^ 4
        });
        assert.deepEqual(splitCase[1].bestExample, {
          text: 'es',
          p: .25
        });

        assert.equal(splitCase[0], lSpace);
      });

      it('handles cases built from two paths', () => {
        let rootSpace = new SearchCluster(testModel);

        let space1 = rootSpace;
        space1 = new SearchCluster([space1.addInput([{sample: {insert: 'al', deleteLeft: 0}, p: 0.5}], .5)]);
        let path1 = space1.addInput([{sample: { insert: 'e', deleteLeft: 0}, p: 0.5}], .5);

        let space2 = rootSpace;
        space2 = new SearchCluster([space2.addInput([{sample: {insert: 'a', deleteLeft: 0}, p: 0.5}], .5)]);
        let path2 = space2.addInput([{sample: { insert: 'le', deleteLeft: 0}, p: 0.5}], .5);

        // Both end up with the same codepoint length, required the same number of inputs, and
        // started from the same root.
        let combinedSpace = new SearchCluster([path1, path2]);

        const splitSpace = combinedSpace.split(1, testModel);
        assert.equal(splitSpace.length, 2);
        assert.sameMembers(splitSpace.map(s => s[1].inputCount), [1, 2]);

        const cleanSplit = splitSpace.find(s => s[1].inputCount == 1);
        assert.deepEqual(cleanSplit[0].bestExample, {
          text: 'a',
          p: 0.5
        });
        assert.equal(cleanSplit[0].inputCount, 1);
        assert.deepEqual(cleanSplit[1].bestExample, {
          text: 'le',
          p: 0.5
        });

        const trickySplit = splitSpace.find(s => s[1].inputCount == 2);
        assert.deepEqual(trickySplit[0].bestExample, {
          text: 'a',
          p: 0.5
        });
        assert.equal(trickySplit[0].inputCount, 1);
        assert.deepEqual(trickySplit[1].bestExample, {
          text: 'le',
          p: 0.25
        });
        assert.equal(trickySplit[1].inputCount, 2);
      });

      it('does not alter contents of original spaces or paths', () => {
        let rootSpace = new SearchCluster(testModel);

        let space1 = rootSpace;
        space1 = new SearchCluster([space1.addInput([{sample: {insert: 'al', deleteLeft: 0}, p: 0.5}], .5)]);
        let path1 = space1.addInput([{sample: { insert: 'e', deleteLeft: 0}, p: 0.5}], .5);

        let space2 = rootSpace;
        space2 = new SearchCluster([space2.addInput([{sample: {insert: 'a', deleteLeft: 0}, p: 0.5}], .5)]);
        let path2 = space2.addInput([{sample: { insert: 'le', deleteLeft: 0}, p: 0.5}], .5);

        // Both end up with the same codepoint length, required the same number of inputs, and
        // started from the same root.
        let combinedSpace = new SearchCluster([path1, path2]);

        const space1InputSequences = space1.inputSequences.slice();
        const space2InputSequences = space2.inputSequences.slice();
        const combinedSpaceInputSequences = combinedSpace.inputSequences.slice();

        const splitSpace = combinedSpace.split(1, testModel);
        assert.equal(splitSpace.length, 2);

        assert.deepEqual(space1.inputSequences, space1InputSequences);
        assert.deepEqual(space2.inputSequences, space2InputSequences);
        assert.deepEqual(combinedSpace.inputSequences, combinedSpaceInputSequences);
      });

      describe(`complex setup:  5 codepoints in 4 transforms`, () => {
        it('sets up test resources correctly', () => {
          const cantFixtures = cantTestSetup();
          assert.isOk(cantFixtures);

          // Verify that all stages have the correct number of inputs.
          assert.equal(cantFixtures.paths.c_a_path.inputCount, 2);
          assert.equal(cantFixtures.paths.c_an_path.inputCount, 2);
          assert.equal(cantFixtures.paths.ca_dl1_path.inputCount, 2);
          assert.equal(cantFixtures.paths.ca_n_path.inputCount, 2);

          assert.equal(cantFixtures.paths.c_a_nq_path.inputCount, 3);
          assert.equal(cantFixtures.paths.c_an_q_path.inputCount, 3);
          assert.equal(cantFixtures.paths.can_q_dl1q_path.inputCount, 3);

          assert.equal(cantFixtures.paths.c_a_n_qt_path.inputCount, 4);
          assert.equal(cantFixtures.paths.c_an_q_t_path.inputCount, 4);

          assert.equal(cantFixtures.spaces.rootSpace.inputCount, 0);

          assert.equal(cantFixtures.spaces.c1_t1_space.inputCount, 1);
          assert.equal(cantFixtures.spaces.c2_t1_space.inputCount, 1);
          assert.equal(cantFixtures.spaces.c3_t1_space.inputCount, 1);

          assert.equal(cantFixtures.spaces.c2_t2_space.inputCount, 2);
          assert.equal(cantFixtures.spaces.c3_t2_space.inputCount, 2);
          assert.equal(cantFixtures.spaces.c4_t2_space.inputCount, 2);

          assert.equal(cantFixtures.spaces.c3_t3_space.inputCount, 3);
          assert.equal(cantFixtures.spaces.c4_t3_space.inputCount, 3);

          assert.equal(cantFixtures.spaces.c5_4t_space.inputCount, 4);
        });

        // do split-tests at each char position:  0 to 5.  (Technically, only 1-4 should be used
        // b/c word-internal, but... yeah.)

        it('splits correctly at index 0', () => {
          const cantFixtures = cantTestSetup();
          const finalSpace = cantFixtures.spaces.c5_4t_space;

          const results = finalSpace.split(0, testModel);

          assert.isOk(results);
          assert.equal(results.length, 1);
        });

        it('splits correctly at index 1', () => {
          const cantFixtures = cantTestSetup();
          const finalSpace = cantFixtures.spaces.c5_4t_space;

          const results = finalSpace.split(1, testModel);

          assert.isOk(results);
          // There exist 3 different spaces with a single transform,
          // outputting 1, 2, and 3 codepoints respectively.
          //
          // The '2' and '3' variants must be split, but their paths
          // recombine at the end.
          //
          // So, we get two:
          // - One with a perfect split after the full first input
          // - One with partial splits in the middle of that first input
          assert.equal(results.length, 2);
        });

        it('splits correctly at index 2', () => {
          const cantFixtures = cantTestSetup();
          const finalSpace = cantFixtures.spaces.c5_4t_space;

          const results = finalSpace.split(2, testModel);

          assert.isOk(results);
          // Expected paths:
          // - c + a, ca[mn] + dl1  (two transforms, no split)
          // - c + a | n            (two transforms with split at pos 1)
          // - ca                   (single transform)
          // - ca | [m, n]          (single transform with split at pos 2)
          assert.equal(results.length, 4);
        });

        it('splits correctly at index 3', () => {
          const cantFixtures = cantTestSetup();
          const finalSpace = cantFixtures.spaces.c5_4t_space;

          const results = finalSpace.split(3, testModel);

          assert.isOk(results);
          // There exist 3 variants of spaces with 3 codepoints:
          // 1, 2, and 3 transforms.
          // - ca[m|n]
          // - c + a + n
          // - ca + n, c + an
          // There's also a c + a + n' case, where the `n'` must be split.
          assert.equal(results.length, 4);
        });

        it('splits correctly at index 4', () => {
          const cantFixtures = cantTestSetup();
          const finalSpace = cantFixtures.spaces.c5_4t_space;

          const results = finalSpace.split(4, testModel);

          assert.isOk(results);
          // Some have just + t after the end of the head space
          // Some split the final transform:  `'t`.
          assert.equal(results.length, 2);
        });

        it('splits correctly at index 5', () => {
          const cantFixtures = cantTestSetup();
          const finalSpace = cantFixtures.spaces.c5_4t_space;

          const results = finalSpace.split(5, testModel);

          assert.isOk(results);
          assert.equal(results.length, 1);
        });
      });
    });
  });
});
