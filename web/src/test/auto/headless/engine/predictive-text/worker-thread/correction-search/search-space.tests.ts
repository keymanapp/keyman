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
import { correction, getBestMatches, models, SearchPath } from '@keymanapp/lm-worker/test-index';

import SearchResult = correction.SearchResult;
import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

function buildTestTimer() {
  return new correction.ExecutionTimer(Number.MAX_VALUE, Number.MAX_VALUE);
}

describe('Correction Searching', () => {
  const checkRepeatableResults_teh = async (iter: AsyncGenerator<correction.SearchResult, any, any>) => {
    const firstIterResult = await iter.next();  // {value: <actual value>, done: <iteration complete?>}
    assert.isFalse(firstIterResult.done);

    const firstResult: correction.SearchResult = firstIterResult.value; // Retrieves <actual value>
    // No checks on the first set's cost.
    assert.equal(firstResult.matchString, "ten");

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

    const searchSpace = new SearchPath(testModel);

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

    const searchPath1 = new SearchPath(searchPath, synthInput1, {trueTransform: synthInput1[0].sample, inputStartIndex: 0, bestProbFromSet: 1});
    const searchPath2 = new SearchPath(searchPath1, synthInput2, {trueTransform: synthInput1[0].sample, inputStartIndex: 0, bestProbFromSet: .75});
    const searchPath3 = new SearchPath(searchPath2, synthInput3, {trueTransform: synthInput1[0].sample, inputStartIndex: 0, bestProbFromSet: .75});

    assert.notEqual(searchPath1.spaceId, searchPath.spaceId);
    assert.notEqual(searchPath2.spaceId, searchPath1.spaceId);
    assert.notEqual(searchPath3.spaceId, searchPath2.spaceId);

    const iter = getBestMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter);
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

    const searchPath1 = new SearchPath(searchPath, synthInput1, {trueTransform: synthInput1[0].sample, inputStartIndex: 0, bestProbFromSet: 1});
    const searchPath2 = new SearchPath(searchPath1, synthInput2, {trueTransform: synthInput1[0].sample, inputStartIndex: 0, bestProbFromSet: .75});
    const searchPath3 = new SearchPath(searchPath2, synthInput3, {trueTransform: synthInput1[0].sample, inputStartIndex: 0, bestProbFromSet: .75});

    assert.notEqual(searchPath1.spaceId, searchPath.spaceId);
    assert.notEqual(searchPath2.spaceId, searchPath1.spaceId);
    assert.notEqual(searchPath3.spaceId, searchPath2.spaceId);

    const iter = getBestMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter);

    // The key: do we get the same results the second time?
    // Reset the iterator first...
    const iter2 = getBestMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter2);
  });

  it('Empty search space, loaded model', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    const searchSpace = new SearchPath(testModel);
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

  // Tests for mechanics reliant upon the SearchSpace interface, rather than a specific implementation.
  describe('SearchPath + SearchSpace', () => {
  });

  describe('SearchPath', () => {
    it('initializes from a lexical model', () => {
      const path = new SearchPath(testModel);
      assert.equal(path.inputCount, 0);
      // Should have codepointLength == 0 once it's defined.
      assert.isNumber(path.spaceId);
      assert.deepEqual(path.bestExample, {text: '', p: 1});
      assert.deepEqual(path.parents, []);
      assert.isNotOk(path.inputs);
      assert.equal(path.likeliestSourceText, '');
    });

    it('may be extended from root path', () => {
      const rootPath = new SearchPath(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const extendedPath = new SearchPath(
        rootPath,
        leadEdgeDistribution, {
        trueTransform: leadEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: leadEdgeDistribution[0].p
        }
      );

      assert.equal(extendedPath.inputCount, 1);
      // Should have codepointLength == 1 once it's defined.
      assert.isNumber(extendedPath.spaceId);
      assert.notEqual(extendedPath.spaceId, rootPath.spaceId);
      assert.deepEqual(extendedPath.bestExample, {text: 't', p: 0.5});
      assert.deepEqual(extendedPath.parents, [rootPath]);
      assert.deepEqual(extendedPath.inputs, leadEdgeDistribution);
      assert.equal(extendedPath.likeliestSourceText, 't');
      assert.deepEqual(extendedPath.sourceIdentifiers, [
        {
        trueTransform: leadEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: leadEdgeDistribution[0].p
        }
      ]);

      // Assert the root is unchanged.
      assert.equal(rootPath.inputCount, 0);
      // Should (still) have codepointLength == 0 once it's defined.
      assert.deepEqual(rootPath.bestExample, {text: '', p: 1});
      assert.deepEqual(rootPath.parents, []);
      assert.isNotOk(rootPath.inputs);
    });

    it('may be built from arbitrary prior SearchPath', () => {
      const rootPath = new SearchPath(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];
      const inputClone = leadEdgeDistribution.map(e => ({...e}));

      const length1Path = new SearchPath(
        rootPath,
        leadEdgeDistribution, {
        trueTransform: leadEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: leadEdgeDistribution[0].p
        }
      );

      const tailEdgeDistribution = [
        {sample: {insert: 'r', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'e', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'h', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new SearchPath(
        length1Path,
        tailEdgeDistribution, {
        trueTransform: tailEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: tailEdgeDistribution[0].p
        }
      );

      // Verify that the prior distribution remains fully unaltered.
      assert.deepEqual(leadEdgeDistribution, inputClone);

      assert.equal(length2Path.inputCount, 2);
      // Should have codepointLength == 2 once it's defined.
      assert.isNumber(length2Path.spaceId);
      assert.notEqual(length2Path.spaceId, length1Path.spaceId);
      assert.deepEqual(length2Path.bestExample, {text: 'tr', p: leadEdgeDistribution[0].p * tailEdgeDistribution[0].p});
      assert.deepEqual(length2Path.parents, [length1Path]);
      assert.deepEqual(length2Path.inputs, tailEdgeDistribution);
      assert.equal(length2Path.likeliestSourceText, 'tr');
      assert.deepEqual(length2Path.sourceIdentifiers, [
        {
        trueTransform: leadEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: leadEdgeDistribution[0].p
        }, {
          trueTransform: tailEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: tailEdgeDistribution[0].p
        }
      ]);

      assert.equal(length1Path.inputCount, 1);
      // Should have codepointLength == 1 once it's defined.
      assert.isNumber(length1Path.spaceId);
      assert.notEqual(length1Path.spaceId, rootPath.spaceId);
      assert.deepEqual(length1Path.bestExample, {text: 't', p: 0.5});
      assert.deepEqual(length1Path.parents, [rootPath]);
      assert.deepEqual(length1Path.inputs, leadEdgeDistribution);
    });

    it('may extend with a Transform inserting multiple codepoints', () => {
      const rootPath = new SearchPath(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];
      const inputClone = leadEdgeDistribution.map(e => ({...e}));

      const length1Path = new SearchPath(
        rootPath,
        leadEdgeDistribution, {
        trueTransform: leadEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: leadEdgeDistribution[0].p
        }
      );

      const tailEdgeDistribution = [
        {sample: {insert: 'ri', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'er', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'hi', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new SearchPath(
        length1Path,
        tailEdgeDistribution, {
        trueTransform: tailEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: tailEdgeDistribution[0].p
        }
      );

      // Verify that the prior distribution remains fully unaltered.
      assert.deepEqual(leadEdgeDistribution, inputClone);

      assert.equal(length2Path.inputCount, 2);
      // Should have codepointLength == 3 once it's defined.
      assert.isNumber(length2Path.spaceId);
      assert.notEqual(length2Path.spaceId, length1Path.spaceId);
      assert.deepEqual(length2Path.bestExample, {text: 'tri', p: leadEdgeDistribution[0].p * tailEdgeDistribution[0].p});
      assert.deepEqual(length2Path.parents, [length1Path]);
      assert.deepEqual(length2Path.inputs, tailEdgeDistribution);
      assert.equal(length2Path.likeliestSourceText, 'tri');
      assert.deepEqual(length2Path.sourceIdentifiers, [
        {
        trueTransform: leadEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: leadEdgeDistribution[0].p
        }, {
          trueTransform: tailEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: tailEdgeDistribution[0].p
        }
      ]);

      assert.equal(length1Path.inputCount, 1);
      // Should have codepointLength == 1 once it's defined.
      assert.isNumber(length1Path.spaceId);
      assert.notEqual(length1Path.spaceId, rootPath.spaceId);
      assert.deepEqual(length1Path.bestExample, {text: 't', p: 0.5});
      assert.deepEqual(length1Path.parents, [rootPath]);
      assert.deepEqual(length1Path.inputs, leadEdgeDistribution);
    });
  });
});

describe('SearchPath', () => {

});