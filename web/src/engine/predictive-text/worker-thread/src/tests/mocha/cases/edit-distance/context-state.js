import { assert } from 'chai';

import { ContextState } from '#./correction/context-state.js';
import * as models from '#./models/index.js';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

var TrieModel = models.TrieModel;

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

describe('ContextState', () => {
  it('<constructor>', () => {
    let context = { left: '', right: '' };
    let state = new ContextState(context, plainModel);

    assert.equal(state.context, context);
    assert.equal(state.model, plainModel);
    assert.isOk(state.tokenization);
    assert.isUndefined(state.isManuallyApplied);
    assert.isNotOk(state.suggestions);
    assert.isNotOk(state.appliedSuggestionId);
  });

  describe('initializing without prior tokenization', () => {
    it('<empty context>', () => {
      let context = { left: '', right: '' };
      let state = new ContextState(context, plainModel);
      assert.isOk(state.tokenization);
      assert.equal(state.tokenization.tokens.length, 1);
      assert.equal(state.tokenization.tail.exampleInput, '');
    });

    it('with initial text (without ending whitespace', () => {
      let context = { left: 'the quick brown fox', right: '' };
      let state = new ContextState(context, plainModel);
      assert.isOk(state.tokenization);
      assert.equal(state.tokenization.tokens.length, 7);
      assert.deepEqual(state.tokenization.exampleInput, ['the', ' ', 'quick', ' ', 'brown', ' ', 'fox']);
    });

    it('with initial text (with ending whitespace', () => {
      let context = { left: 'the quick brown fox ', right: '' };
      let state = new ContextState(context, plainModel);
      assert.isOk(state.tokenization);
      assert.equal(state.tokenization.tokens.length, 9);
      assert.deepEqual(state.tokenization.exampleInput, ['the', ' ', 'quick', ' ', 'brown', ' ', 'fox', ' ', '']);
    });
  });
});