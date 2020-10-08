/*
 * Unit tests for common utility functions/methods.
 */

var assert = require('chai').assert;
var models = require('../').models;

describe('Common utility functions', function() {
  // TODO:  unit tests for other common utility functions

  describe('transformToSuggestion', function() {
    it('p: undefined', function() {
      let suggestion = {
        transform: {
          insert: 'hello',
          deleteLeft: 0,
          id: 0
        },
        transformId: 0,
        displayAs: 'hello'
      };

      assert.deepEqual(models.transformToSuggestion(suggestion.transform), suggestion);
    });

    it('p: 0', function() {
      let suggestion = {
        transform: {
          insert: 'hello',
          deleteLeft: 0,
          id: 0
        },
        transformId: 0,
        displayAs: 'hello',
        p: 0
      };

      assert.deepEqual(models.transformToSuggestion(suggestion.transform, 0), suggestion);
    });

    it('p > 0', function() {
      let suggestion = {
        transform: {
          insert: 'hello',
          deleteLeft: 0,
          id: 0
        },
        transformId: 0,
        displayAs: 'hello',
        p: 0.5
      };

      assert.deepEqual(models.transformToSuggestion(suggestion.transform, 0.5), suggestion);
    });

    it('properly handles the transformId', function() {
      let suggestion = {
        transform: {
          insert: 'hello',
          deleteLeft: 0,
          id: 3
        },
        transformId: 3, // Ensures there isn't a separate ID seed in use.
        displayAs: 'hello'
      };

      assert.deepEqual(models.transformToSuggestion(suggestion.transform), suggestion);
    });
  });
});
