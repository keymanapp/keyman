/*
 * Unit tests for common utility functions/methods.
 */

import { assert } from 'chai';
import * as models from '@keymanapp/models-templates';

describe('Common utility functions', function() {
  // TODO:  unit tests for other common utility functions

  describe('buildMergedTransform', function() {
    it("simple case:  no deletions", function() {
      let apple = {
        insert: 'apple',
        deleteLeft: 0
      };

      let banana = {
        insert: 'banana',
        deleteLeft: 0
      };

      let final = {
        insert: 'applebanana',
        deleteLeft: 0,
        deleteRight: 0
      };

      let mergedTransform = models.buildMergedTransform(apple, banana);
      assert.deepEqual(mergedTransform, final);
    });

    it("first.deleteLeft > 0, second.deleteLeft = 0", function() {
      let apple = {
        insert: 'apple',
        deleteLeft: 2
      };

      let banana = {
        insert: 'banana',
        deleteLeft: 0
      };

      let final = {
        insert: 'applebanana',
        deleteLeft: 2,
        deleteRight: 0
      };

      let mergedTransform = models.buildMergedTransform(apple, banana);
      assert.deepEqual(mergedTransform, final);
    });

    it("first.deleteLeft = 0, second.deleteLeft > 0", function() {
      let banana = {
        insert: 'banana',
        deleteLeft: 0
      };

      let apple = {
        insert: 'apple',
        deleteLeft: 1
      };

      let final = {
        insert: 'bananapple',  // the 'apple' transform removes the final 'a' from 'banana'.
        deleteLeft: 0,
        deleteRight: 0
      };

      let mergedTransform = models.buildMergedTransform(banana, apple);
      assert.deepEqual(mergedTransform, final);
    });

    it("first.deleteLeft > 0, second.deleteLeft > 0", function() {
      let banana = {
        insert: 'banana',
        deleteLeft: 2
      };

      let apple = {
        insert: 'apple',
        deleteLeft: 1
      };

      let final = {
        insert: 'bananapple',  // the 'apple' transform removes the final 'a' from 'banana'.
        deleteLeft: 2,
        deleteRight: 0
      };

      let mergedTransform = models.buildMergedTransform(banana, apple);
      assert.deepEqual(mergedTransform, final);
    });

    it("first.deleteRight > 0, second.deleteRight = 0 (implied)", function() {
      let apple = {
        insert: 'apple',
        deleteLeft: 0,
        deleteRight: 2
      };

      let banana = {
        insert: 'banana',
        deleteLeft: 0
      };

      let final = {
        insert: 'applebanana',  // the 'apple' transform does NOT remove the front 'ba' from 'banana'.
                                // 'banana' is considered 'later' in time, after application of 'apple'
        deleteLeft: 0,
        deleteRight: 2
      };

      let mergedTransform = models.buildMergedTransform(apple, banana);
      assert.deepEqual(mergedTransform, final);
    });

    it("first.deleteRight = 0, second.deleteRight > 0", function() {
      let apple = {
        insert: 'apple',
        deleteLeft: 0
      };

      let banana = {
        insert: 'banana',
        deleteLeft: 0,
        deleteRight: 3
      };

      let final = {
        insert: 'applebanana',  // the 'apple' transform does NOT remove the front 'ba' from 'banana'.
                                // 'banana' is considered 'later' in time, after application of 'apple'
        deleteLeft: 0,
        deleteRight: 3
      };

      let mergedTransform = models.buildMergedTransform(apple, banana);
      assert.deepEqual(mergedTransform, final);
    });

    it("first.deleteRight > 0, second.deleteRight > 0", function() {
      let apple = {
        insert: 'apple',
        deleteLeft: 0,
        deleteRight: 2
      };

      let banana = {
        insert: 'banana',
        deleteLeft: 0,
        deleteRight: 2
      };

      let final = {
        insert: 'applebanana',  // the 'apple' transform does NOT remove the front 'ba' from 'banana'.
                                // 'banana' is considered 'later' in time, after application of 'apple'
        deleteLeft: 0,
        deleteRight: 4
      };

      let mergedTransform = models.buildMergedTransform(apple, banana);
      assert.deepEqual(mergedTransform, final);
    });

    it("complex case:  first.deleteRight > 0, second.deleteLeft > 0", function() {
      let apple = {
        insert: 'apple',
        deleteLeft: 0,
        deleteRight: 2
      };

      let banana = {
        insert: 'banana',
        deleteLeft: 2,
        deleteRight: 0
      };

      let final = {
        insert: 'appbanana',
        deleteLeft: 0,
        deleteRight: 2
      };

      let mergedTransform = models.buildMergedTransform(apple, banana);
      assert.deepEqual(mergedTransform, final);
    });
  });

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
