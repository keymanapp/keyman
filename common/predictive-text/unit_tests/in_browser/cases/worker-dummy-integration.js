var assert = chai.assert;

/*
 * Shows off the LMLayer API, using the full prediction interface.
 * The dummy model (or mock model) is a language model which has 
 * **injectable** suggestions: that is, you, as the tester, have
 * to provide the predictions. The dummy model does not create any
 * suggestions on its own. The dummy model can take in a series
 * of suggestions when initialized and return them sequentially.
 */
describe('LMLayer using dummy model', function () {
  describe('Prediction', function () {
    it('will predict future suggestions', function () {
      var lmLayer = new LMLayer();
      var capabilities = {
        maxLefContextCodeUnits: 32 + ~~Math.random() * 32
      };

      // Tower of promises!
      return lmLayer.initialize(
        capabilities,
        { model: {
            type: 'dummy',
            futureSuggestions: iGotDistractedByHazel()
        }}
      ).then(function (actualConfiguration) {
        return Promise.resolve();
      }).then(function () {
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        assert.deepEqual(suggestions, iGotDistractedByHazel()[0]);
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        assert.deepEqual(suggestions, iGotDistractedByHazel()[1]);
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        assert.deepEqual(suggestions, iGotDistractedByHazel()[2]);
        return lmLayer.predict(zeroTransform(), emptyContext());
      }).then(function (suggestions) {
        assert.deepEqual(suggestions, iGotDistractedByHazel()[3]);
        return Promise.resolve();
      });
    });
  });

  function emptyContext() {
    return { left: '', startOfBuffer: true, endOfBuffer: true };
  }

  function zeroTransform() {
    return { insert: '', deleteLeft: 0 };
  }

  function iGotDistractedByHazel() {
    return [
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

    ]
  }
});
