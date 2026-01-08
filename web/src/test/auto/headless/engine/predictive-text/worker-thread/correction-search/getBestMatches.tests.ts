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
import { correction, getBestMatches, LegacyQuotientSpur, models, SearchQuotientRoot } from '@keymanapp/lm-worker/test-index';

import SearchResult = correction.SearchResult;
import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

function buildTestTimer() {
  return new correction.ExecutionTimer(Number.MAX_VALUE, Number.MAX_VALUE);
}

describe('getBestMatches', () => {
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

    const searchSpace = new SearchQuotientRoot(testModel);

    const iter = getBestMatches(searchSpace, buildTestTimer());
    const firstResult = await iter.next();
    assert.isFalse(firstResult.done);
  });

  // Hmm... how best to update this...
  it('Simple search (paralleling "Small integration test")', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    let searchPath = new SearchQuotientRoot(testModel);

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

    const searchPath1 = new LegacyQuotientSpur(searchPath, synthInput1, 1);
    const searchPath2 = new LegacyQuotientSpur(searchPath1, synthInput2, .75);
    const searchPath3 = new LegacyQuotientSpur(searchPath2, synthInput3, .75);

    assert.notEqual(searchPath1.spaceId, searchPath.spaceId);
    assert.notEqual(searchPath2.spaceId, searchPath1.spaceId);
    assert.notEqual(searchPath3.spaceId, searchPath2.spaceId);

    const iter = getBestMatches(searchPath3, buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter);
  });

  it('Allows reiteration (sequentially)', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    let searchPath = new SearchQuotientRoot(testModel);

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

    const searchPath1 = new LegacyQuotientSpur(searchPath, synthInput1, 1);
    const searchPath2 = new LegacyQuotientSpur(searchPath1, synthInput2, .75);
    const searchPath3 = new LegacyQuotientSpur(searchPath2, synthInput3, .75);

    assert.notEqual(searchPath1.spaceId, searchPath.spaceId);
    assert.notEqual(searchPath2.spaceId, searchPath1.spaceId);
    assert.notEqual(searchPath3.spaceId, searchPath2.spaceId);

    const iter = getBestMatches(searchPath3, buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter);

    // The key: do we get the same results the second time?
    // Reset the iterator first...
    const iter2 = getBestMatches(searchPath3, buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter2);
  });

  it('Empty search space, loaded model', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    const searchSpace = new SearchQuotientRoot(testModel);
    const timer = buildTestTimer();
    const iter = getBestMatches(searchSpace, timer);

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
});
