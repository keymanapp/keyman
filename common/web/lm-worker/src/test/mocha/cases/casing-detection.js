import { assert } from 'chai';
import { ModelCompositor } from '#./model-compositor.js';

import { detectCurrentCasing } from "#./model-helpers.js";
import { DummyModel } from "#./models/dummy-model.js";

const defaultCasingModel = new DummyModel({
  languageUsesCasing: true,
  // See: developer/src/kmc-model/model-defaults.ts, defaultApplyCasing
  applyCasing: (casing, text) => {
    switch(casing) {
      case 'lower':
        return text.toLowerCase();
      case 'upper':
        return text.toUpperCase();
      case 'initial':
        var headCode = text.charCodeAt(0);
        // The length of the first code unit, as measured in code points.
        var headUnitLength = 1;

        // Is the first character a high surrogate, indicating possible use of UTF-16
        // surrogate pairs?  Also, is the string long enough for there to BE a pair?
        if(text.length > 1 && headCode >= 0xD800 && headCode <= 0xDBFF) {
          // It's possible, so now we check for low surrogates.
          var lowSurrogateCode = text.charCodeAt(1);

          if(lowSurrogateCode >= 0xDC00 && lowSurrogateCode <= 0xDFFF) {
            // We have a surrogate pair; this pair is the 'first' character.
            headUnitLength++;
          }
        }

        // Capitalizes the first code unit of the string, leaving the rest intact.
        return text.substring(0, headUnitLength).toUpperCase() // head - uppercased
               .concat(text.substring(headUnitLength));        // tail - lowercased
    }
  }
});

/** @type {CasingFunction} */
const leetCasing = (casing, text) => {
  // Don't know if there's a standard analogue for '9', but this'll work well enough.
  const plain = ['O', 'L', 'Z', 'E', 'A', 'S', 'G', 'T', 'B']
  const leet  = ['0', '1', '2', '3', '4', '5', '6', '7', '8']

  switch(casing) {
    case 'lower':
      return [...text].map((entry) => {
        const mappingIndex = leet.indexOf(entry);
        if(mappingIndex == -1) {
          return entry;
        } else {
          return plain[mappingIndex];
        }
      }).join('');
    case 'upper':
      return [...text].map((entry) => {
        const mappingIndex = plain.indexOf(entry);
        if(mappingIndex == -1) {
          return entry;
        } else {
          return leet[mappingIndex];
        }
      }).join('');
    case 'initial':
      // Capitalizes the first code unit of the string, leaving the rest intact.
      return leetCasing('upper', text.substring(0, 1))        // head - uppercased
             .concat(leetCasing('lower', text.substring(1))); // tail - lowercased
  }
};

const leetCasingModel = new DummyModel({
  languageUsesCasing: true,
  // See: developer/src/kmc-model/model-defaults.ts, defaultApplyCasing
  applyCasing: leetCasing
});

/** @type {Context} */
const emptyContext = {
  left: '',
  right: '',
  startOfBuffer: true,
  endOfBuffer: true
}

describe('detectCasing', () => {
  it('with empty context, no pre-set casing', () => {
    assert.equal(detectCurrentCasing(defaultCasingModel, emptyContext), 'lower');
  });

  describe('standard latin-alphabet casing patterns', () => {
    it('without pre-set casing on context', () => {
      assert.equal(detectCurrentCasing(defaultCasingModel, {
        ...emptyContext,
        left: 'apple'
      }), 'lower');

      assert.equal(detectCurrentCasing(defaultCasingModel, {
        ...emptyContext,
        left: 'Apple'
      }), 'initial');

      assert.equal(detectCurrentCasing(defaultCasingModel, {
        ...emptyContext,
        left: 'APPLE'
      }), 'upper');

      assert.equal(detectCurrentCasing(defaultCasingModel, {
        ...emptyContext,
        left: 'aPpLe'
      }), null);
    });

    describe('with pre-set casing on context', () => {
      // When set to 'lower', it's just treated as a default, rather than an override.
      it('set to lower', () => {
        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'apple',
          casingForm: 'lower'
        }), 'lower');

        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'Apple',
          casingForm: 'lower'
        }), 'initial');

        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'APPLE',
          casingForm: 'lower'
        }), 'upper');

        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'aPpLe',
          casingForm: 'lower'
        }), 'lower');
      });

      // When set to 'initial', it's treated as an override.
      it('set to initial', () => {
        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'apple',
          casingForm: 'initial'
        }), 'initial');

        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'Apple',
          casingForm: 'initial'
        }), 'initial');

        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'APPLE',
          casingForm: 'initial'
        }), 'initial');

        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'aPpLe',
          casingForm: 'initial'
        }), 'initial');
      });

      // When set to 'upper', it's treated as an override.
      it('set to upper', () => {
        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'apple',
          casingForm: 'upper'
        }), 'upper');

        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'Apple',
          casingForm: 'upper'
        }), 'upper');

        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'APPLE',
          casingForm: 'upper'
        }), 'upper');

        assert.equal(detectCurrentCasing(defaultCasingModel, {
          ...emptyContext,
          left: 'aPpLe',
          casingForm: 'upper'
        }), 'upper');
      });
    });
  });

  describe('custom leet-based casing patterns', () => {
    it('without pre-set casing on context', () => {
      assert.equal(detectCurrentCasing(leetCasingModel, {
        ...emptyContext,
        left: 'EAST'
      }), 'lower');

      assert.equal(detectCurrentCasing(leetCasingModel, {
        ...emptyContext,
        left: '3AST'
      }), 'initial');

      assert.equal(detectCurrentCasing(leetCasingModel, {
        ...emptyContext,
        left: '3457'
      }), 'upper');

      assert.equal(detectCurrentCasing(leetCasingModel, {
        ...emptyContext,
        left: 'E45T'
      }), null);
    });

    describe('with pre-set casing on context', () => {
      // When set to 'lower', it's just treated as a default, rather than an override.
      it('set to lower', () => {
        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: 'EAST',
          casingForm: 'lower'
        }), 'lower');

        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: '3AST',
          casingForm: 'lower'
        }), 'initial');

        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: '3457',
          casingForm: 'lower'
        }), 'upper');

        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: 'E45T',
          casingForm: 'lower'
        }), 'lower');
      });

      // When set to 'initial', it's treated as an override.
      it('set to initial', () => {
        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: 'EAST',
          casingForm: 'initial'
        }), 'initial');

        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: '3AST',
          casingForm: 'initial'
        }), 'initial');

        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: '3457',
          casingForm: 'initial'
        }), 'initial');

        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: 'E45T',
          casingForm: 'initial'
        }), 'initial');
      });

      // When set to 'upper', it's treated as an override.
      it('set to upper', () => {
        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: 'EAST',
          casingForm: 'upper'
        }), 'upper');

        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: '3AST',
          casingForm: 'upper'
        }), 'upper');

        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: '3457',
          casingForm: 'upper'
        }), 'upper');

        assert.equal(detectCurrentCasing(leetCasingModel, {
          ...emptyContext,
          left: 'E45T',
          casingForm: 'upper'
        }), 'upper');
      });
    });
  });

  describe('throws errors for models without full casing support', () => {
    it('languageUsesCasing = false, applyCasing not defined', () => {
      const dummyModel = new DummyModel({
        languageUsesCasing: true
      });

      // We don't care which message.
      assert.throws(() => detectCurrentCasing(dummyModel, emptyContext));
    });

    it('languageUsesCasing = false, applyCasing defined', () => {
      const dummyModel = new DummyModel({
        applyCasing: defaultCasingModel.applyCasing
      });

      assert.throws(() => detectCurrentCasing(dummyModel, emptyContext), /languageUsesCasing is set to false/);
    });

    it('languageUsesCasing = true, applyCasing not defined', () => {
      const dummyModel = new DummyModel({
        languageUsesCasing: true
      });

      assert.throws(() => detectCurrentCasing(dummyModel, emptyContext), /no applyCasing function/);
    });
  });

  describe('predict() does not throw errors when the model has incomplete casing support', () => {
    it('languageUsesCasing = false, applyCasing defined', () => {
      const model = new DummyModel({
        languageUsesCasing: false,
        applyCasing: defaultCasingModel.applyCasing,
        futureSuggestions: [
          [
            {
              transform: {
                insert: 'apple',
                deleteLeft: 2
              },
              displayAs: 'apple'
            }
          ]
        ]
      });

      const compositor = new ModelCompositor(model, true);

      const context = {
        ...emptyContext,
        left: 'Ap'
      };

      assert.doesNotThrow(() => compositor.predict({
        insert: 'p',
        deleteLeft: 0
      }, context));
    });

    it('languageUsesCasing = true, applyCasing not defined', () => {
      const model = new DummyModel({
        languageUsesCasing: true,
        futureSuggestions: [
          [
            {
              transform: {
                insert: 'apple',
                deleteLeft: 2
              },
              displayAs: 'apple'
            }
          ]
        ]
      });

      const compositor = new ModelCompositor(model, true);

      const context = {
        ...emptyContext,
        left: 'Ap'
      };

      assert.doesNotThrow(() => compositor.predict({
        insert: 'p',
        deleteLeft: 0
      }, context));
    });
  });
});