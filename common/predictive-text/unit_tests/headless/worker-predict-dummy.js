/*
 * Unit tests for the Dummy prediction model.
 */

var assert = require('chai').assert;

var DummyModel = require('../../build/intermediate').models.DummyModel;

describe('LMLayerWorker dummy model', function() {
  describe('instantiation', function () {
    it('can be instantiated with capabilities', function () {
      var model = new DummyModel(defaultCapabilities);
      assert.isObject(model);
    });

    it('supports dependency-injected configuration', function () {
      let configuration = {
        leftContextCodeUnits: 64,
        rightContextCodeUnits: 0
      };

      var model = new DummyModel(
        {
          maxLeftContextCodeUnits: 64,
        },
        {
          configuration: configuration,
        }
      );

      assert.deepEqual(model.configuration, configuration);
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
        // The transforms are a bit redundant, but slightly
        // easier to program; they undo the 't' input, only to
        // type suggestions with that all start with 't'.
        {
          transform: {
            insert: 'teapot',
            deleteLeft: 1,
          },
          displayAs: '🍵',
        },
        {
          transform: {
            insert: 'too',
            deleteLeft: 1,
          },
          displayAs: 'too',
        },
        {
          transform: {
            insert: 'tired',
            deleteLeft: 1,
          },
          displayAs: '😪',
        },
      ];

      var model = new DummyModel(defaultCapabilities());

      var suggestions = model.predict(
        // Type a 't'
        {
          insert: 't',
          deleteLeft: 0,
        },
        {
          left: "I'm a little ",
          startOfBuffer: true,
          endOfBuffer: true,
        },
        expectedSuggestions
     );
     assert.deepEqual(suggestions, expectedSuggestions);
    });

    it('can be injected with multiple suggestions to send back', function () {
      // Based on suggestions produced my phone's personal language model.
      var futureSuggestions = [
        // Initial suggestions
        [
          {
            transform: {
              insert: 'I ',
              deleteLeft: 0
            },
            displayAs: 'I',
          },
          {
            transform: {
              insert: "I'm ",
            deleteLeft: 0
            },
            displayAs: "I'm",
          },
          {
            transform: {
              insert: "Oh ",
              deleteLeft: 0
            },
            displayAs: "Oh ",
          }
        ],
        // Second set of suggestions, after choosing "I"
        [
          {
            transform: {
              insert: 'love ',
              deleteLeft: 0
            },
            displayAs: 'love',
          },
          {
            transform: {
              insert: "am ",
              deleteLeft: 0
            },
            displayAs: "am",
          },
          {
            transform: {
              insert: "got ",
              deleteLeft: 0
            },
            displayAs: "got",
          }
        ],
        // Third set of suggestions, after choosing "got"
        [
          {
            transform: {
              insert: 'distracted ',
              deleteLeft: 0
            },
            displayAs: 'distracted by',
          },
          {
            transform: {
              insert: "distracted ",
              deleteLeft: 0
            },
            displayAs: "distracted",
          },
          {
            transform: {
              insert: "a ",
              deleteLeft: 0
            },
            displayAs: "a",
          }
        ],
        // Last set of suggestions, after choosing "distracted by"
        [
          {
            transform: {
              insert: 'Hazel ',
              deleteLeft: 0
            },
            displayAs: 'Hazel',
          },
          {
            transform: {
              insert: 'the ',
              deleteLeft: 0
            },
            displayAs: 'the',
          },
          {
            transform: {
              insert: 'a ',
              deleteLeft: 0
            },
            displayAs: 'a',
          },
        ],
      ];

      var model = new DummyModel(defaultCapabilities, {
        futureSuggestions: futureSuggestions
      });

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

  /**
   * Capabilities of a keyboard that will ONLY send left-sided capabilities.
   * The keyboard does not support deleting to the right.
   * 
   * @returns Capabilities
   */
  function defaultCapabilities() {
    return {
      maxLeftContextCodeUnits: 64,
    };
  }

  /**
   * Returns a transform that does nothing.
   *
   * @returns Transform
   */
  function zeroTransform() {
    return {
      insert: '',
      deleteLeft: 0,
    };
  }

  /**
   * Returns a context of an empty buffer.
   *
   * @returns Context
   */
  function emptyContext() {
    return {
      left: '',
      startOfBuffer: true,
      endOfBuffer: true
    };
  }
});
