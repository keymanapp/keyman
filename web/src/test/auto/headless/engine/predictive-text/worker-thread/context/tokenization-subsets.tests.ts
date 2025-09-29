/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-09-23
 *
 * This file contains low-level tests designed to validate the behavior of the
 * of the ContextTokenization class and its integration with the lower-level
 * classes that it utilizes.
 */

import { assert } from 'chai';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { LexicalModelTypes } from '@keymanapp/common-types';
import { deepCopy } from '@keymanapp/web-utils';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import { buildEdgeWindow, ContextToken, ContextTokenization, models, precomputationSubsetKeyer, TokenizationTransitionEdits } from '@keymanapp/lm-worker/test-index';

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
        }
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
        }
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
        }
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
            token.addInput(
              {trueTransform: {insert: 'te', deleteLeft: 0}, inputStartIndex: 0},
              [{sample: {insert: 'te', deleteLeft: 0}, p: 1}]
            );
            return token;
          })()],
            { insert: 's', deleteLeft: 0, deleteRight: 0 },
            false
          ),
          retokenization: [...rawTextTokens]
        }
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        map.set(0, { insert: 's', deleteLeft: 0 });
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
            token.addInput(
              {trueTransform: {insert: 'te', deleteLeft: 0}, inputStartIndex: 0},
              [{sample: {insert: 't', deleteLeft: 0}, p: 1}]
            );
            return token;
          })()],
        { insert: 'es', deleteLeft: 0, deleteRight: 0 },
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
              token.addInput(
                {trueTransform: {insert: 't', deleteLeft: 0}, inputStartIndex: 0},
                [{sample: {insert: 'ts', deleteLeft: 0}, p: 1}]
              );
              return token;
            })()],
            { insert: 'e', deleteLeft: 1, deleteRight: 0 },
            false
          ),
          retokenization: [...rawTextTokens]
        }
      },
      tokenizedTransform: (() => {
        const map = new Map<number, Transform>();
        map.set(0, { insert: 'e', deleteLeft: 1 });
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
          token.addInput(
            {trueTransform: {insert: 't', deleteLeft: 0}, inputStartIndex: 0},
            [{sample: {insert: 't', deleteLeft: 0}, p: 1}]
          );
          return token;
        })()],
        { insert: 'e', deleteLeft: 0, deleteRight: 0 },
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
        edgeWindow: edgeWindow1
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
        edgeWindow: edgeWindow1
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
        }
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
        }
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
});