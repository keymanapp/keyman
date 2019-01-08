/*
 * Unit tests for the Dummy prediction model.
 */

var assert = require('chai').assert;
var sinon = require('sinon');

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
        {
          // Type a 't'
          transform: {
            insert: 't',
            deleteLeft: 0,
          },
          context: {
            left: "I'm a little ",
            startOfBuffer: true,
            endOfBuffer: true,
          }
        },
        expectedSuggestions
     );
     assert.deepEqual(suggestions, expectedSuggestions);
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
});
