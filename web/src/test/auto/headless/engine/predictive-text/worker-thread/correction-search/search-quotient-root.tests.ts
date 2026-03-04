/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-03-04
 *
 * This file defines tests for the SearchQuotientRoot class of the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import {
  models,
  SearchQuotientRoot,
} from '@keymanapp/lm-worker/test-index';

import { constituentPaths } from '../../helpers/constituentPaths.js';

import DummyModel = models.DummyModel;
import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));
const altModel = new TrieModel(jsonFixture('models/tries/accented'));

describe('SearchQuotientSpur', () => {
  describe('constructor', () => {
    it('initializes from a lexical model', () => {
      const path = new SearchQuotientRoot(testModel);
      assert.equal(path.inputCount, 0);
      assert.equal(path.codepointLength, 0);
      assert.isNumber(path.spaceId);
      assert.deepEqual(path.bestExample, {text: '', p: 1});
      assert.deepEqual(path.parents, []);
    });

    it('throws an error for lexical models that do not support traversal-based searching', () => {
      assert.throws(() => new SearchQuotientRoot(new DummyModel({})));
    });
  });

  it('constituentPaths returns an empty array', () => {
    const path = new SearchQuotientRoot(testModel);
    assert.deepEqual(constituentPaths(path), []);
  });

  describe('.isSameNode returns true for separate instance on same model', () => {
    const root1 = new SearchQuotientRoot(testModel);
    const root2 = new SearchQuotientRoot(testModel);

    assert.isTrue(root1.isSameNode(root2));
  });

  it('split() results in two separate root instances', () => {
    const root = new SearchQuotientRoot(testModel);

    const splitResults = root.split(0);

    // Only one way to split it...
    assert.equal(splitResults.length, 1);

    const [head, tail] = splitResults[0];
    assert.notStrictEqual(head, tail);
    assert.isTrue(head.isSameNode(tail));
  });

  describe('merge()', () => {
    it('treats two copies of same-model root as same node', () => {
      const head = new SearchQuotientRoot(testModel);
      const tail = new SearchQuotientRoot(testModel);

      const merged = head.merge(tail);

      assert.isTrue(head.isSameNode(tail));
      assert.isTrue(merged.isSameNode(head));
      assert.isTrue(merged.isSameNode(tail));
    });

    it('rejects roots from different models', () => {
      const head = new SearchQuotientRoot(testModel);
      const tail = new SearchQuotientRoot(altModel);

      assert.throws(() => head.merge(tail));
    });
  });
});