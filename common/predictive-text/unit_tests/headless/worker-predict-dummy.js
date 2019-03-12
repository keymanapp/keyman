/*
 * Unit tests for the Dummy prediction model.
 */

var assert = require('chai').assert;
var DummyModel = require('../../build/intermediate').models.DummyModel;

describe('LMLayerWorker dummy model', function() {
  describe('instantiation', function () {
    it('can be instantiated with no arguments', function () {
      var model = new DummyModel();
      assert.isObject(model);
    });
  });

  describe('prediction', function () {
    it('can predict send back an array of injected suggestions', function (){
      // We're pretending to be in this situation:
      //
      //   «I'm a little t|           » [Send]
      //   [   too   ] [    🍵    ] [   😪    ]
      //
      // Pressing the top suggestion (middle, highlighted)
      // will modified the buffer to:
      //
      //   «I'm a little teapot|      » [Send]

      var expectedSuggestions = [
        {
          transform: {
            insert: 'teapot',
            deleteLeft: 0,
          },
          displayAs: '🍵',
        },
        {
          transform: {
            insert: 'too',
            deleteLeft: 0,
          },
          displayAs: 'too',
        },
        {
          transform: {
            insert: 'tired',
            deleteLeft: 0,
          },
          displayAs: '😪',
        },
      ];

      var model = new DummyModel();

      // Type a 't'
      var suggestions = model.predict({
        insert: 't',
        deleteLeft: 0,
      },
      {
        left: "I'm a little ",
        startOfBuffer: true,
        endOfBuffer: true,
      }, expectedSuggestions);
     assert.deepEqual(suggestions, expectedSuggestions);
    });

    it('can be injected with multiple suggestions to send back', function () {
      // See the fixture. It's based on suggestions produced my phone's personal
      // language model.
      var futureSuggestions = iGotDistractedByHazel();
      assert.isDefined(futureSuggestions[0]);
      assert.isDefined(futureSuggestions[1]);
      assert.isDefined(futureSuggestions[2]);
      assert.isDefined(futureSuggestions[3]);

      var model = new DummyModel({futureSuggestions: futureSuggestions});

      // The dummy model should give suggestions in order,
      // regardless of the provided transform and context.
      assert.deepEqual(model.predict(zeroTransform(), emptyContext()),
                       futureSuggestions[0]);
      assert.deepEqual(model.predict(zeroTransform(), emptyContext()),
                       futureSuggestions[1]);
      assert.deepEqual(model.predict(zeroTransform(), emptyContext()),
                       futureSuggestions[2]);
      assert.deepEqual(model.predict(zeroTransform(), emptyContext()),
                       futureSuggestions[3]);
    });
  });
});
