/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-09-23
 *
 * This file contains low-level tests designed to validate the behavior of the
 * ContextTokenization class and its integration with the lower-level classes
 * that it utilizes.
 */

import { assert } from 'chai';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { SENTINEL_CODE_UNIT } from '@keymanapp/models-templates';
import { LexicalModelTypes } from '@keymanapp/common-types';
import { deepCopy } from '@keymanapp/web-utils';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import {
  buildEdgeWindow,
  ContextToken,
  ContextTokenization,
  generateSubsetId,
  models,
  precomputationSubsetKeyer,
  TokenizationTransitionEdits,
  TokenizationSubsetBuilder
} from '@keymanapp/lm-worker/test-index';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

function toToken(text: string) {
  let isWhitespace = text == ' ';
  let token = new ContextToken(plainModel, text);
  token.isWhitespace = isWhitespace;
  return token;
}

describe('precomputationSubsetKeyer', function() {
  it("safely generates keys for empty transition + empty contexts", () => {
    const rawTextTokens = [''];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    const precomputation1: TokenizationTransitionEdits = {
      alignment: {
        merges: [],
        splits: [],
        unmappedEdits: [],
        edgeWindow: {
          ...buildEdgeWindow(
            tokenization.tokens,
            { insert: '', deleteLeft: 0, deleteRight: 0 },
            false
          ),
          retokenization: [...rawTextTokens]
        },
        removedTokenCount: 0
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        map.set(0, { insert: '', deleteLeft: 0 });
        return map;
      })()
    };
    const key = precomputationSubsetKeyer(precomputation1);

    assert.isOk(key);
  });

  it("generates different keys for transforms of different insert lengths on the same context", () => {
    const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    const precomputation1: TokenizationTransitionEdits = {
      alignment: {
        merges: [],
        splits: [],
        unmappedEdits: [],
        edgeWindow: {
          ...buildEdgeWindow(
            tokenization.tokens,
            { insert: '', deleteLeft: 0, deleteRight: 0 },
            false
          ),
          retokenization: [...rawTextTokens]
        },
        removedTokenCount: 0
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        map.set(0, { insert: '', deleteLeft: 0 });
        return map;
      })()
    };
    const precomputation2 = deepCopy(precomputation1);
    precomputation2.alignment.edgeWindow = {
      ...buildEdgeWindow(
        tokenization.tokens,
        { insert: 's', deleteLeft: 0, deleteRight: 0 },
        false
      ),
      retokenization: [...rawTextTokens]
    };
    precomputation2.tokenizedTransform = (() => {
      const map = new Map<number, Transform>();
        map.set(0, { insert: 's', deleteLeft: 0 });
        return map;
    })()

    assert.deepEqual(precomputation2.alignment.edgeWindow, precomputation1.alignment.edgeWindow);
    const key1 = precomputationSubsetKeyer(precomputation1);
    const key2 = precomputationSubsetKeyer(precomputation2);

    assert.notEqual(key2, key1);
  });

  it("generates different keys for transforms of different deleteLeft lengths on the same context", () => {
    const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    const precomputation1: TokenizationTransitionEdits = {
      alignment: {
        merges: [],
        splits: [],
        unmappedEdits: [],
        edgeWindow: {
          ...buildEdgeWindow(
            tokenization.tokens,
            { insert: 's', deleteLeft: 0, deleteRight: 0 },
            false
          ),
          retokenization: [...rawTextTokens]
        },
        removedTokenCount: 0
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        map.set(0, { insert: 's', deleteLeft: 0 });
        return map;
      })()
    };
    const precomputation2 = deepCopy(precomputation1);
    precomputation2.alignment.edgeWindow = {
      ...buildEdgeWindow(
        tokenization.tokens,
        { insert: 'b', deleteLeft: 1, deleteRight: 0 },
        false
      ),
      retokenization: [...rawTextTokens]
    };
    precomputation2.tokenizedTransform = (() => {
      const map = new Map<number, Transform>();
        map.set(0, { insert: 'b', deleteLeft: 1 });
        return map;
    })()

    assert.notDeepEqual(precomputation2.alignment.edgeWindow, precomputation1.alignment.edgeWindow);
    const key1 = precomputationSubsetKeyer(precomputation1);
    const key2 = precomputationSubsetKeyer(precomputation2);

    assert.notEqual(key2, key1);
  });

  it("generates matching keys when boundary token + transform results in equal length token with same source text (1)", () => {
    const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' '];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    // concept:  inputs were 'd', 'a', 'te', 's'
    const precomputation1: TokenizationTransitionEdits = {
      alignment: {
        merges: [],
        splits: [],
        unmappedEdits: [],
        edgeWindow: {
          ...buildEdgeWindow(
            [...tokenization.tokens, (() => {
            const token = new ContextToken(plainModel, 'da');
            // source text:  'date'
            token.addInput({
              segment: {
                trueTransform: {
                  insert: 'te',
                  deleteLeft: 0,
                  id: 13
                }, transitionId: 13,
                start: 0
              }, bestProbFromSet: 1,
              subsetId: generateSubsetId()
            }, [
              {sample: {insert: 'te', deleteLeft: 0, id: 13}, p: 1}
            ]);
            return token;
          })()],
            { insert: 's', deleteLeft: 0, deleteRight: 0 },
            false
          ),
          retokenization: [...rawTextTokens]
        },
        removedTokenCount: 0
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        map.set(0, { insert: 's', deleteLeft: 0, id: 14 });
        return map;
      })()
    };

    // concept:  inputs were 'd', 'a', 't', 'es'
    const precomputation2 = deepCopy(precomputation1);
    precomputation2.alignment.edgeWindow = {
      ...buildEdgeWindow(
          [...tokenization.tokens, (() => {
            const token = new ContextToken(plainModel, 'da');
            // source text:  'date'
            token.addInput({
              segment: {
                trueTransform: {
                  insert: 'te',
                  deleteLeft: 0,
                  id: 13
                }, transitionId: 13,
                start: 0
              }, bestProbFromSet: 1,
              subsetId: generateSubsetId()
            }, [
              {sample: {insert: 't', deleteLeft: 0}, p: 1}
            ]);
            return token;
          })()],
        { insert: 'es', deleteLeft: 0, deleteRight: 0, id: 14 },
        false
      ),
      retokenization: [...rawTextTokens]
    };
    precomputation2.tokenizedTransform = (() => {
      const map = new Map<number, Transform>();
        map.set(0, { insert: 'es', deleteLeft: 0 });
        return map;
    })()

    assert.notDeepEqual(precomputation2.alignment.edgeWindow, precomputation1.alignment.edgeWindow);
    const key1 = precomputationSubsetKeyer(precomputation1);
    const key2 = precomputationSubsetKeyer(precomputation2);

    assert.equal(key2, key1);
  });

  // - also due to deleteLeft effects:  2 + 1 vs a 2 + 2-dl:1
  it("generates matching keys when boundary token + transform results in equal length token with same source text (1)", () => {
    const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' '];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    // concept:  inputs were 'd', 'a', 'ts', 'e' (with delete-left 1)
    const precomputation1: TokenizationTransitionEdits = {
      alignment: {
        merges: [],
        splits: [],
        unmappedEdits: [],
        edgeWindow: {
          ...buildEdgeWindow(
            [...tokenization.tokens, (() => {
              const token = new ContextToken(plainModel, 'da');
              token.isPartial = true;
              // source text:  'dat'
              token.addInput({
                segment: {
                  trueTransform: {
                    insert: 't',
                    deleteLeft: 0,
                    id: 13
                  }, transitionId: 13,
                  start: 0
                }, bestProbFromSet: 1,
                subsetId: generateSubsetId()
              }, [{sample: {insert: 'ts', deleteLeft: 0, id: 13}, p: 1}
              ]);
              return token;
            })()],
            { insert: 'e', deleteLeft: 1, deleteRight: 0, id: 14 },
            false
          ),
          retokenization: [...rawTextTokens]
        },
        removedTokenCount: 0
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        map.set(0, { insert: 'e', deleteLeft: 1, id: 14 });
        return map;
      })()
    };

    // concept:  inputs were 'd', 'a', 't', 'e'
    const precomputation2 = deepCopy(precomputation1);
    precomputation2.alignment.edgeWindow = {
      ...buildEdgeWindow(
        [...tokenization.tokens, (() => {
          const token = new ContextToken(plainModel, 'da');
          token.isPartial = true;
          // source text:  'dat'
          token.addInput({
            segment: {
              trueTransform: {
                insert: 't',
                deleteLeft: 0,
                id: 13
              }, transitionId: 13,
              start: 0
            }, bestProbFromSet: 1,
            subsetId: generateSubsetId()
          }, [
            {sample: {insert: 't', deleteLeft: 0, id: 13}, p: 1}
          ]);
          return token;
        })()],
        { insert: 'e', deleteLeft: 0, deleteRight: 0, id: 14 },
        false
      ),
      retokenization: [...rawTextTokens]
    };
    precomputation2.tokenizedTransform = (() => {
      const map = new Map<number, Transform>();
        map.set(0, { insert: 'e', deleteLeft: 0 });
        return map;
    })()

    // delete lengths differ, but that should be it.
    assert.notDeepEqual(precomputation2.alignment.edgeWindow, precomputation1.alignment.edgeWindow);
    const alteredEdgeWindow = {
      ...precomputation2.alignment.edgeWindow,
      deleteLengths: [1]
    }
    assert.deepEqual(alteredEdgeWindow, precomputation1.alignment.edgeWindow);
    const key1 = precomputationSubsetKeyer(precomputation1);
    const key2 = precomputationSubsetKeyer(precomputation2);

    // Both result in the same net length of token (4) starting at the same
    // point in the context / at the same keystroke.
    assert.equal(key2, key1);
  });

  it("properly notes new boundary token length on boundary-final merge", () => {
    const rawTextTokens = ['she', ' ', 'says', ' ', 'I', ' ', 'can', '\''];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    const edgeWindow1 = {
      ...buildEdgeWindow(
        tokenization.tokens,
        { insert: 't', deleteLeft: 0, deleteRight: 0 },
        false
      ),
      retokenization: [...rawTextTokens]
    };
    const precomputation1: TokenizationTransitionEdits = {
      alignment: {
        merges: [{
          // The indices specified here are the edge-window-internal indices.
          inputs: [
            { text: 'can', index: rawTextTokens.length - 2 - edgeWindow1.sliceIndex },
            { text: '\'', index: rawTextTokens.length - 1 - edgeWindow1.sliceIndex }
          ], match: {
            text: 'can\'',
            index: rawTextTokens.length - 2 - edgeWindow1.sliceIndex
          }
        }],
        splits: [],
        unmappedEdits: [],
        edgeWindow: edgeWindow1,
        removedTokenCount: 0
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        map.set(0, { insert: 't', deleteLeft: 0 });
        return map;
      })()
    };

    const key1 = precomputationSubsetKeyer(precomputation1);
    // Boundary token says is length 2 - as if appending to just `'`, not to `can'`.
    assert.isFalse(key1.indexOf("BI@0-2") > -1, "The key's merge marker length does not correspond to merged token length");
    // Boundary token says is length 5 - as if appending to `can'`.
    assert.isTrue(key1.indexOf("BI@0-5") > -1);
  });

  it("generates different keys for matching transforms when one causes token merge", () => {
    const rawTextTokens = ['she', ' ', 'says', ' ', 'I', ' ', 'can', '\''];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    const edgeWindow1 = {
      ...buildEdgeWindow(
        tokenization.tokens,
        { insert: 't', deleteLeft: 0, deleteRight: 0 },
        false
      ),
      retokenization: [...rawTextTokens]
    };
    const precomputation1: TokenizationTransitionEdits = {
      alignment: {
        merges: [{
          // The indices specified here are the edge-window-internal indices.
          inputs: [
            { text: 'can', index: rawTextTokens.length - 2 - edgeWindow1.sliceIndex },
            { text: '\'', index: rawTextTokens.length - 1 - edgeWindow1.sliceIndex }
          ], match: {
            text: 'can\'',
            index: rawTextTokens.length - 2 - edgeWindow1.sliceIndex
          }
        }],
        splits: [],
        unmappedEdits: [],
        edgeWindow: edgeWindow1,
        removedTokenCount: 0
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        map.set(0, { insert: 't', deleteLeft: 0 });
        return map;
      })()
    };
    const precomputation2 = deepCopy(precomputation1);
    precomputation2.alignment.merges = [];
    precomputation2.alignment.edgeWindow = {
      ...buildEdgeWindow(
        tokenization.tokens,
        { insert: '.', deleteLeft: 0, deleteRight: 0 },
        false
      ),
      retokenization: [...rawTextTokens]
    };
    precomputation2.tokenizedTransform = (() => {
      const map = new Map<number, Transform>();
        map.set(0, { insert: '.', deleteLeft: 0 });
        return map;
    })()

    assert.deepEqual(precomputation2.alignment.edgeWindow, precomputation1.alignment.edgeWindow);
    const key1 = precomputationSubsetKeyer(precomputation1);
    const key2 = precomputationSubsetKeyer(precomputation2);

    assert.notEqual(key2, key1);
  });

  it("properly notes new boundary token length on boundary-final split", () => {
    const rawTextTokens = ['she', ' ', 'says', ' ', 'I', ' ', 'can\''];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    const edgeWindow1 = {
      ...buildEdgeWindow(
        tokenization.tokens,
        { insert: '.', deleteLeft: 0, deleteRight: 0 },
        false
      ),
      retokenization: [...rawTextTokens]
    };
    const precomputation1: TokenizationTransitionEdits = {
      alignment: {
        merges: [],
        splits: [{
          // The indices specified here are the edge-window-internal indices.
          input: {
            text: 'can\'',
            index: rawTextTokens.length - 1 - edgeWindow1.sliceIndex
          }, matches: [
            { text: 'can', index: rawTextTokens.length - 1 - edgeWindow1.sliceIndex, textOffset: 0 },
            { text: '\'', index: rawTextTokens.length - 0 - edgeWindow1.sliceIndex, textOffset: 3 }
          ]
        }],
        unmappedEdits: [],
        edgeWindow: {
          ...buildEdgeWindow(
            tokenization.tokens,
            { insert: '.', deleteLeft: 0, deleteRight: 0 },
            false
          ),
          retokenization: [...rawTextTokens]
        },
        removedTokenCount: 0
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        // Creates a `'.` token.  The two chars should technically be separate
        // tokens, but... we should still get a distinct key if they're combined
        // this way.
        map.set(0, { insert: '.', deleteLeft: 0 });
        return map;
      })()
    };

    const key1 = precomputationSubsetKeyer(precomputation1);
    // Boundary token says is length 5 - as if appending to `can'`, not to just `'`.
    assert.isFalse(key1.indexOf("BI@0-5") > -1, "The key's split marker length does not correspond to last split token length");
    // Boundary token says is length 2 - as if appending to just `'`, not to `can'`.
    assert.isTrue(key1.indexOf("BI@0-2") > -1);
  });

  it("generates different keys for matching transforms when one causes token split", () => {
    const rawTextTokens = ['she', ' ', 'says', ' ', 'I', ' ', 'can\''];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    const edgeWindow1 = {
      ...buildEdgeWindow(
        tokenization.tokens,
        { insert: '.', deleteLeft: 0, deleteRight: 0 },
        false
      ),
      retokenization: [...rawTextTokens]
    };
    const precomputation1: TokenizationTransitionEdits = {
      alignment: {
        merges: [],
        splits: [{
          // The indices specified here are the edge-window-internal indices.
          input: {
            text: 'can\'',
            index: rawTextTokens.length - 1 - edgeWindow1.sliceIndex
          }, matches: [
            { text: 'can', index: rawTextTokens.length - 1 - edgeWindow1.sliceIndex, textOffset: 0 },
            { text: '\'', index: rawTextTokens.length - 0 - edgeWindow1.sliceIndex, textOffset: 3 }
          ]
        }],
        unmappedEdits: [],
        edgeWindow: {
          ...buildEdgeWindow(
            tokenization.tokens,
            { insert: '.', deleteLeft: 0, deleteRight: 0 },
            false
          ),
          retokenization: [...rawTextTokens]
        },
        removedTokenCount: 0
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        // Creates a `'.` token.  The two chars should technically be separate
        // tokens, but... we should still get a distinct key if they're combined
        // this way.
        map.set(0, { insert: '.', deleteLeft: 0 });
        return map;
      })()
    };
    const precomputation2 = deepCopy(precomputation1);
    precomputation2.alignment.splits = [];
    precomputation2.alignment.edgeWindow = {
      ...buildEdgeWindow(
        tokenization.tokens,
        { insert: 't', deleteLeft: 0, deleteRight: 0 },
        false
      ),
      retokenization: [...rawTextTokens]
    };
    precomputation2.tokenizedTransform = (() => {
      const map = new Map<number, Transform>();
        map.set(0, { insert: 't', deleteLeft: 0 });
        return map;
    })()

    assert.deepEqual(precomputation2.alignment.edgeWindow, precomputation1.alignment.edgeWindow);
    const key1 = precomputationSubsetKeyer(precomputation1);
    const key2 = precomputationSubsetKeyer(precomputation2);

    assert.notEqual(key2, key1);
  });

  it("generates a key without boundary-mutation marker on simple whitespace input after text", () => {
    const rawTextTokens = ['she', ' ', 'says', ' ', 'I', ' ', 'can'];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    const precomputation1: TokenizationTransitionEdits = {
      alignment: {
        merges: [],
        splits: [],
        unmappedEdits: [],
        edgeWindow: {
          ...buildEdgeWindow(
            tokenization.tokens,
            { insert: ' ', deleteLeft: 0, deleteRight: 0 },
            false
          ),
          retokenization: [...rawTextTokens]
        },
        removedTokenCount: 0
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        // Creates a `'.` token.  The two chars should technically be separate
        // tokens, but... we should still get a distinct key if they're combined
        // this way.
        map.set(1, { insert: ' ', deleteLeft: 0 });
        map.set(2, { insert: '', deleteLeft: 0 });
        return map;
      })()
    };

    const key1 = precomputationSubsetKeyer(precomputation1);

    // A very "whitebox" check, but it's the clearest way to validate this.
    assert.isFalse(key1.indexOf(SENTINEL_CODE_UNIT + 'BI@') > -1, "Key contains marker indicating boundary-token mutation");
  });
});

describe('TokenizationSubsetBuilder', function() {
  // Include a few tests related to precomputationSubsetKeyer
  //
  // Also do some tests that mimic intended use upon receiving an incoming
  // fat-finger distribution.
  it("builds a single subset when all keys are non-whitespace single-char inserts", () => {
    const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'da'];
    const baseTokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    const inputChars = ['b', 'd', 'i', 'm', 'n', 'p', 's', 't', 'y'];
    const totalMass = inputChars.reduce((accum, curr) => accum + curr.charCodeAt(0), 0);
    const inputDistribution = inputChars.map((c) => ({sample: { insert: c, deleteLeft: 0 }, p: c.charCodeAt(0) / totalMass}));

    const subsetBuilder = new TokenizationSubsetBuilder();

    inputChars.forEach((c) => {
      const {sample: transform, p} = inputDistribution.find(s => s.sample.insert == c);

      const precomputation = baseTokenization.mapWhitespacedTokenization(plainModel, transform);
      subsetBuilder.addPrecomputation(baseTokenization, precomputation, p);
    });

    assert.equal(subsetBuilder.subsets.size, 1); // All transforms have similar impacts.
    const subset = [...subsetBuilder.subsets.values()][0];
    assert.equal(subset.pendingSet.size, 1); // Built from only one tokenization
    assert.deepEqual(subset.pendingSet.get(baseTokenization).inputs,
      inputDistribution.map((sample) => {
        const map = new Map<number, Transform>();
        map.set(0, sample.sample);
        return { sample: map, p: sample.p };
    }));
  });

  it("builds two subsets when all but one keys are non-whitespace single-char inserts", () => {
    const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'da'];
    const baseTokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    const inputChars = ['b', 'd', 'i', 'm', 'n', 'p', 's', 't', 'y', ' '];
    const totalMass = inputChars.reduce((accum, curr) => accum + curr.charCodeAt(0), 0);
    const inputDistribution = inputChars.map((c) => ({sample: { insert: c, deleteLeft: 0 }, p: c.charCodeAt(0) / totalMass}));

    const subsetBuilder = new TokenizationSubsetBuilder();

    inputChars.forEach((c) => {
      const {sample: transform, p} = inputDistribution.find(s => s.sample.insert == c);

      const precomputation = baseTokenization.mapWhitespacedTokenization(plainModel, transform);
      subsetBuilder.addPrecomputation(baseTokenization, precomputation, p);
    });

    assert.equal(subsetBuilder.subsets.size, 2); // All transforms have similar impacts.
    const subsets = [...subsetBuilder.subsets.values()];
    subsets.forEach((subset) => assert.equal(subset.pendingSet.size, 1));  // Built from only one tokenization

    const distributionWithoutWhitespace = inputDistribution.slice(0, inputDistribution.length-1);
    const extendingSubset = subsets.find((subset) => subset.pendingSet.get(baseTokenization).inputs.length > 1);
    assert.deepEqual(extendingSubset.pendingSet.get(baseTokenization).inputs,
      distributionWithoutWhitespace.map((sample) => {
        const map = new Map<number, Transform>();
        map.set(0, sample.sample);
        return { sample: map, p: sample.p };
    }));

    const whitespaceSubset = subsets.find((subset) => subset.pendingSet.get(baseTokenization).inputs.length == 1);
    const whitespaceSample = inputDistribution[inputDistribution.length - 1];
    const expectedWhitespaceTransformTokenization = {
      sample: (() => {
        const map = new Map<number, Transform>();
        // Whitespace creates a new, appended token...
        map.set(1, whitespaceSample.sample);
        // ... and a following blank token with empty root text.
        map.set(2, { insert: '', deleteLeft: 0 });
        return map;
      })(),
      p: whitespaceSample.p
    };
    assert.deepEqual(whitespaceSubset.pendingSet.get(baseTokenization).inputs, [expectedWhitespaceTransformTokenization]);
  });

  it("builds different subsets for transforms resulting in different total lengths and token count", () => {
    const rawTextTokens = ['drink', ' ', 'coffee', ' ', 'at', ' ', 'a', ' ', 'cafe'];
    const baseTokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    const inputDistribution: Distribution<Transform> = [
      { sample: { insert: 'é', deleteLeft: 1 }, p: .35 }, // café (length 4)
      { sample: { insert: 't', deleteLeft: 0 }, p: .2 }, // cafet (length 5) (as in 'cafeteria')
      { sample: { insert: 's', deleteLeft: 0 }, p: .15 }, // cafes (length 4)
      { sample: { insert: '', deleteLeft: 1 }, p: .1 },  // caf (length 3)
      { sample: { insert: '', deleteLeft: 0 }, p: .1 },  // cafe (length 4)
      { sample: { insert: '.', deleteLeft: 0 }, p: .08 }, // cafe,. (lengths 4, 1)
      { sample: { insert: ' ', deleteLeft: 0 }, p: .12 }  // cafe, , (lengths 4, 1, 0)
    ];

    const subsetBuilder = new TokenizationSubsetBuilder();

    inputDistribution.forEach((entry) => {
      const precomputation = baseTokenization.mapWhitespacedTokenization(plainModel, entry.sample);
      subsetBuilder.addPrecomputation(baseTokenization, precomputation, entry.p);
    });

    assert.equal(subsetBuilder.subsets.size, 5); // All transforms have similar impacts.

    const subsets = [...subsetBuilder.subsets.values()];
    const sameTokenLen4Subset = subsets.find((subset) => {
      const dataForSet = subset.pendingSet.get(baseTokenization);
      const totalMass = dataForSet.inputs.reduce((accum, curr) => accum + curr.p, 0);
      // Thanks, floating-point precision.
      // Should land both the 'é' (delete 1) and empty-string transform (that lacks deletes)
      return Math.abs(totalMass - .45) < 1e-8;
    });
    assert.isOk(sameTokenLen4Subset);
    assert.equal(sameTokenLen4Subset.pendingSet.get(baseTokenization).inputs.length, 2);

    const sameTokenLen5Subset = subsets.find((subset) => {
      const dataForSet = subset.pendingSet.get(baseTokenization);
      const totalMass = dataForSet.inputs.reduce((accum, curr) => accum + curr.p, 0);
      // Thanks, floating-point precision.
      // Should land both the 't' and 's' transforms:  adds 1 char, deletes none
      return Math.abs(totalMass - .35) < 1e-8;
    });
    assert.isOk(sameTokenLen5Subset);
    assert.equal(sameTokenLen5Subset.pendingSet.get(baseTokenization).inputs.length, 2);

    const sameTokenLen3Subset = subsets.find((subset) => {
      const dataForSet = subset.pendingSet.get(baseTokenization);
      const totalMass = dataForSet.inputs.reduce((accum, curr) => accum + curr.p, 0);
      // Thanks, floating-point precision.
      // Should land the backspace transform.
      return Math.abs(totalMass - .1) < 1e-8;
    });
    assert.isOk(sameTokenLen3Subset);
    assert.equal(sameTokenLen3Subset.pendingSet.get(baseTokenization).inputs.length, 1);

    const plusOneTokenSubset = subsets.find((subset) => {
      const dataForSet = subset.pendingSet.get(baseTokenization);
      const totalMass = dataForSet.inputs.reduce((accum, curr) => accum + curr.p, 0);
      // Thanks, floating-point precision.
      // Should land the backspace transform.
      return Math.abs(totalMass - .08) < 1e-8;
    });
    assert.isOk(plusOneTokenSubset);
    assert.equal(plusOneTokenSubset.pendingSet.get(baseTokenization).inputs.length, 1);

    const plusTwoTokensSubset = subsets.find((subset) => {
      const dataForSet = subset.pendingSet.get(baseTokenization);
      const totalMass = dataForSet.inputs.reduce((accum, curr) => accum + curr.p, 0);
      // Thanks, floating-point precision.
      // Should land the backspace transform.
      return Math.abs(totalMass - .12) < 1e-8;
    });
    assert.isOk(plusTwoTokensSubset);
    assert.equal(plusTwoTokensSubset.pendingSet.get(baseTokenization).inputs.length, 1);
  });

  it("places compatible results from separate tokenizations in the same subset after whitespace", () => {
    const baseRawTextTokens = ['drink', ' ', 'coffee', ' ', 'at', ' ', 'a', ' ', 'cafe'];
    const baseTokenization = new ContextTokenization(baseRawTextTokens.map((text => toToken(text))));

    const trueSourceTransform: Transform = { insert: 'é', deleteLeft: 1, id: 13 };

    const fourCharTailToken = new ContextToken(baseTokenization.tail);
    fourCharTailToken.addInput({
      segment: {
        trueTransform: {
          insert: 'é',
          deleteLeft: 1,
          id: 13
        }, transitionId: 13,
        start: 0
      }, bestProbFromSet: 1,
      subsetId: generateSubsetId()
    }, [
      { sample: trueSourceTransform, p: .6 }
    ]);

    const fiveCharTailToken = new ContextToken(baseTokenization.tail);
    fiveCharTailToken.addInput({
      segment: {
        trueTransform: {
          insert: 'é',
          deleteLeft: 1,
          id: 13
        }, transitionId: 13,
        start: 0
      }, bestProbFromSet: 1,
      subsetId: generateSubsetId()
    }, [
      { sample: { insert: 's', deleteLeft: 0, id: 13 }, p: .4 }
    ]);

    const subsetBuilder = new TokenizationSubsetBuilder();
    const fourCharTokenization = new ContextTokenization([...baseTokenization.tokens.slice(0, -1), fourCharTailToken]);
    const fiveCharTokenization = new ContextTokenization([...baseTokenization.tokens.slice(0, -1), fiveCharTailToken]);

    const inputDistribution = [{sample: { insert: ' ', deleteLeft: 0 }, p: 1}];
    inputDistribution.forEach((entry) => {
      const precomputation1 = fourCharTokenization.mapWhitespacedTokenization(plainModel, entry.sample);
      subsetBuilder.addPrecomputation(fourCharTokenization, precomputation1, entry.p);

      const precomputation2 = fiveCharTokenization.mapWhitespacedTokenization(plainModel, entry.sample);
      subsetBuilder.addPrecomputation(fiveCharTokenization, precomputation2, entry.p);
    });

    // Both transition to a new token at the same time - there's no need to
    // consider their paths separately after the transition.
    assert.equal(subsetBuilder.subsets.size, 1);
    // Has entries from two different base tokenizations.
    assert.equal([...subsetBuilder.subsets.values()][0].pendingSet.size, 2);
  });

  it("places compatible results from separate tokenizations in the same subset (mid-token)", () => {
    const baseRawTextTokens = ['i', ' ', 'have', ' ', 'never', ' ' , 'been', ' ', 'to', ' ', 'a', ' ', 'se'];
    // target accented word:  séance
    const baseTokenization = new ContextTokenization(baseRawTextTokens.map((text => toToken(text))));

    const trueSourceTransform: Transform = { insert: 'é', deleteLeft: 1, id: 13 };

    const twoCharTailToken = new ContextToken(baseTokenization.tail);
    twoCharTailToken.addInput({
      segment: {
        trueTransform: {
          insert: 'é',
          deleteLeft: 1,
          id: 13
        }, transitionId: 13,
        start: 0
      }, bestProbFromSet: .6,
      subsetId: generateSubsetId()
    }, [
      { sample: trueSourceTransform, p: .6 }
    ]);

    const threeCharTailToken = new ContextToken(baseTokenization.tail);
    threeCharTailToken.addInput({
      segment: {
        trueTransform: {
          insert: 'é',
          deleteLeft: 1
        }, transitionId: 13,
        start: 0
      }, bestProbFromSet: .6,
      subsetId: generateSubsetId()
    }, [
      { sample: { insert: 'a', deleteLeft: 0, id: 13}, p: .4 }
    ]);

    const subsetBuilder = new TokenizationSubsetBuilder();
    const twoCharTokenization = new ContextTokenization([...baseTokenization.tokens.slice(0, -1), twoCharTailToken]);
    const threeCharTokenization = new ContextTokenization([...baseTokenization.tokens.slice(0, -1), threeCharTailToken]);

    const inputDistribution = [
      // ... ok, yeah, it's kinda forced on this aspect.
      {sample: { insert: 'an', deleteLeft: 0 }, p: 0.2},
      {sample: { insert: 'n',  deleteLeft: 0}, p: 0.8}
    ];

    inputDistribution.forEach((entry) => {
      const precomputation1 = twoCharTokenization.mapWhitespacedTokenization(plainModel, entry.sample);
      subsetBuilder.addPrecomputation(twoCharTokenization, precomputation1, entry.p);

      const precomputation2 = threeCharTokenization.mapWhitespacedTokenization(plainModel, entry.sample);
      subsetBuilder.addPrecomputation(threeCharTokenization, precomputation2, entry.p);
    });

    // Both transition to a new token at the same time - there's no need to
    // consider their paths separately after the transition.
    assert.equal(subsetBuilder.subsets.size, 3);
    const subsets = [...subsetBuilder.subsets.values()];

    // sé + an, sea + n:  both result in a four-char long token starting at the same point.
    // Same total amount of .deleteLeft is supported for both variations.
    const mergedSubset = subsets.find((subset) => subset.pendingSet.size);
    assert.isOk(mergedSubset);
    assert.isTrue(mergedSubset.pendingSet.has(twoCharTokenization));
    assert.isTrue(mergedSubset.pendingSet.has(threeCharTokenization));
  });
});