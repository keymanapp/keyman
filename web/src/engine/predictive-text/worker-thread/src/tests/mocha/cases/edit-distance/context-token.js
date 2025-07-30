import { assert } from 'chai';

import { ContextToken } from '#./correction/context-token.js';
import { ExecutionTimer } from '#./correction/execution-timer.js';
import * as models from '#./models/index.js';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

var TrieModel = models.TrieModel;

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

describe('ContextToken', function() {
  describe("<constructor>", () => {
    it("(model: LexicalModel)", async () => {
      let token = new ContextToken(plainModel);

      assert.isEmpty(token.searchSpace.inputSequence);
      assert.isEmpty(token.exampleInput);
      assert.isFalse(token.isWhitespace);
      assert.isEmpty(token.suggestions);
      assert.isUndefined(token.appliedSuggestionId);

      // While searchSpace has no inputs, it _can_ match lexicon entries (via insertions).
      let searchIterator = token.searchSpace.getBestMatches(new ExecutionTimer(Number.POSITIVE_INFINITY, Number.POSITIVE_INFINITY));
      let firstEntry = await searchIterator.next();
      assert.isFalse(firstEntry.done);
    });

    it("(model: LexicalModel, text: string)", () => {
      let token = new ContextToken(plainModel, "and");

      assert.isNotEmpty(token.searchSpace.inputSequence);

      assert.equal(token.searchSpace.inputSequence.map((entry) => entry[0].sample.insert).join(''), 'and');
      token.searchSpace.inputSequence.forEach((entry) => assert.equal(entry[0].sample.deleteLeft, 0));
      assert.deepEqual(token.searchSpace.inputSequence, [..."and"].map((char) => {
        return [{
          sample: {
            insert: char,
            deleteLeft: 0
          },
          p: 1.0
        }];
      }));
      assert.equal(token.exampleInput, 'and');

      assert.isFalse(token.isWhitespace);

      // Is only set with a different value later, outside the constructor.
      assert.isEmpty(token.suggestions);
      assert.isUndefined(token.appliedSuggestionId);
    });

    it("(token: ContextToken", () => {
      // Same as in a test above, since we verified that it works correctly.
      let baseToken = new ContextToken(plainModel, "and");
      baseToken.suggestions = [
        {
          transform: {
            insert: 'd ',
            deleteLeft: 0
          },
          id: 37,
          transformId: 1,
          displayAs: '"and"',
          tag: 'keep',
          autoAccept: true
        },
        {
          transform: {
            insert: 'Andes ',
            deleteLeft: 2
          },
          id: 38,
          transformId: 1,
          displayAs: 'Andes',
        }
      ]
      baseToken.appliedSuggestionId = 37;

      let clonedToken = new ContextToken(baseToken);

      assert.notEqual(clonedToken.suggestions, baseToken.suggestions);
      assert.deepEqual(clonedToken.suggestions, baseToken.suggestions);

      assert.notEqual(clonedToken.searchSpace, baseToken.searchSpace);
      // Deep equality on .searchSpace can't be directly checked due to the internal complexities involved.
      // We CAN check for the most important members, though.
      assert.notEqual(clonedToken.searchSpace.inputSequence, baseToken.searchSpace.inputSequence);
      assert.deepEqual(clonedToken.searchSpace.inputSequence, baseToken.searchSpace.inputSequence);

      assert.notEqual(clonedToken, baseToken);
      // Perfectly deep-equal when we ignore .searchSpace.
      assert.deepEqual({...clonedToken, searchSpace: null}, {...baseToken, searchSpace: null});
    });
  });
});