/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * This file contains low-level tests designed to validate the behavior of the
 * of the ContextTokenization class and its integration with the lower-level
 * classes that it utilizes.
 */

import { assert } from 'chai';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { LexicalModelTypes } from '@keymanapp/common-types';
import { KMWString } from '@keymanapp/web-utils';

import {
  analyzePathMergesAndSplits,
  assembleTransforms,
  buildEdgeWindow,
  ContextToken,
  ContextTokenization,
  EditOperation,
  EditTuple,
  ExtendedEditOperation,
  generateSubsetId,
  models,
  TransitionEdge,
  SearchQuotientSpur,
  traceInsertEdits
} from '@keymanapp/lm-worker/test-index';

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

let TOKEN_TRANSFORM_SEED = 0;
function toTransformToken(text: string, transformId?: number) {
  let idSeed = transformId === undefined ? TOKEN_TRANSFORM_SEED++ : transformId;
  let isWhitespace = text == ' ';
  let token = new ContextToken(plainModel);
  const textAsTransform = { insert: text, deleteLeft: 0, id: idSeed };
  token.addInput({
    segment: {
      transitionId: textAsTransform.id,
      start: 0
    }, bestProbFromSet: 1,
    subsetId: generateSubsetId()
  }, [ { sample: textAsTransform, p: 1 } ]);
  token.isWhitespace = isWhitespace;
  return token;
}

// https://www.compart.com/en/unicode/block/U+1D400
const mathBoldUpperA = 0x1D400; // Mathematical Bold Capital A
const mathBoldLowerA = 0x1D41A; //                   Small   A

function toMathematicalSMP(text: string) {
  const chars = [...text];

  const asSMP = chars.map((c) => {
    if(c >= 'a' && c <= 'z') {
      return String.fromCodePoint(mathBoldLowerA + (c.charCodeAt(0) - 'a'.charCodeAt(0)));
    } else if(c >= 'A' && c <= 'Z') {
      return String.fromCodePoint(mathBoldUpperA + (c.charCodeAt(0) - 'A'.charCodeAt(0)));
    } else {
      return c;
    }
  });

  return asSMP.join('');
}

const testEdgeWindowSpec = {
  minTokens: 3,
  minChars: 8
};

describe('ContextTokenization', function() {
  before(() => {
    KMWString.enableSupplementaryPlane(true);
  });

  describe("<constructor>", () => {
    it("constructs from just a token array", () => {
      const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));
      assert.deepEqual(tokenization.tokens.map((entry) => entry.exampleInput), rawTextTokens);
      assert.deepEqual(tokenization.tokens.map((entry) => entry.isWhitespace), rawTextTokens.map((entry) => entry == ' '));
      assert.isNotOk(tokenization.transitionEdits);
      assert.equal(tokenization.tail.exampleInput, 'day');
      assert.isFalse(tokenization.tail.isWhitespace);
    });

    it("constructs from a token array + alignment data", () => {
      const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const tokens = rawTextTokens.map((text => toTransformToken(text)));
      const emptyTransform = { insert: '', deleteLeft: 0, deleteRight: 0 };

      // We _could_ flesh this out a bit more... but it's not really needed for this test.
      const edgeWindow = buildEdgeWindow(tokens, emptyTransform, false, testEdgeWindowSpec);
      let transitionEdits: TransitionEdge = {
        alignment: {
          merges: [],
          splits: [],
          unmappedEdits: [],
          edgeWindow: {...edgeWindow, retokenization: rawTextTokens.slice(edgeWindow.sliceIndex)},
          removedTokenCount: 0
        },
        inputs: [{sample: (() => {
          const map = new Map<number, Transform>();
          map.set(0, emptyTransform);
          return map;
        })(), p: 1}],
        inputSubsetId: generateSubsetId()
      };

      let tokenization = new ContextTokenization(tokens, transitionEdits, null /* dummy val */);

      assert.deepEqual(tokenization.tokens.map((entry) => entry.exampleInput), rawTextTokens);
      assert.deepEqual(tokenization.tokens.map((entry) => entry.isWhitespace), rawTextTokens.map((entry) => entry == ' '));
      assert.isOk(tokenization.transitionEdits);
      assert.deepEqual(tokenization.transitionEdits, transitionEdits);
      assert.equal(tokenization.tail.exampleInput, 'day');
      assert.isFalse(tokenization.tail.isWhitespace);
    });

    it('clones', () => {
      const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const tokens = rawTextTokens.map((text => toTransformToken(text)));
      const emptyTransform = { insert: '', deleteLeft: 0, deleteRight: 0 };

      // We _could_ flesh this out a bit more... but it's not really needed for this test.
      const edgeWindow = buildEdgeWindow(tokens, emptyTransform, false, testEdgeWindowSpec);
      let transitionEdits: TransitionEdge = {
        alignment: {
          merges: [],
          splits: [],
          unmappedEdits: [],
          edgeWindow: {...edgeWindow, retokenization: rawTextTokens.slice(edgeWindow.sliceIndex)},
          removedTokenCount: 0
        },
        inputs: [{sample: (() => {
          const map = new Map<number, Transform>();
          map.set(0, emptyTransform);
          return map;
        })(), p: 1}],
        inputSubsetId: generateSubsetId()
      };

      let baseTokenization = new ContextTokenization(tokens, transitionEdits, null /* dummy val */);
      let cloned = new ContextTokenization(baseTokenization);

      assert.sameOrderedMembers(
        cloned.tokens.map((token) => token.searchModule),
        baseTokenization.tokens.map((token) => token.searchModule)
      );

      // The `.searchModule` instances will not be deep-equal; there are class properties
      // that hold functions with closures, configured at runtime.

      // @ts-ignore - TS2704 b/c deleting a readonly property.
      baseTokenization.tokens.forEach((token) => delete token.searchModule);
      // @ts-ignore - TS2704 b/c deleting a readonly property.
      cloned.tokens.forEach((token) => delete token.searchModule);

      assert.deepEqual(cloned, baseTokenization);
    });
  });

  it('exampleInput', () => {
    const rawTextTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
    let tokenization = new ContextTokenization(rawTextTokens.map((text => toToken(text))));

    assert.deepEqual(tokenization.exampleInput, rawTextTokens);
  });

  describe('evaluateTransition', () => {
    it('handles simple case - new whitespace + new empty token', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''].map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransform = { insert: ' ', deleteLeft: 0, deleteRight: 0 };
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(1, { insert: ' ', deleteLeft: 0 });
      inputTransformMap.set(2, { insert: '', deleteLeft: 0 });

      const edgeWindow = buildEdgeWindow(baseTokenization.tokens, inputTransform, false, testEdgeWindowSpec);
      const tokenization = baseTokenization.evaluateTransition({
        alignment: {
          merges: [],
          splits: [],
          unmappedEdits: [],
          edgeWindow: {
            ...edgeWindow,
            // The range within the window constructed by the prior call for its parameterization.
            retokenization: targetTokens.slice(edgeWindow.sliceIndex, -2).map(t => t.text)
          },
          removedTokenCount: 0
        },
        inputs: [{ sample: inputTransformMap, p: 1 }],
        inputSubsetId: generateSubsetId()
      },
        plainModel,
        inputTransform,
        1
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);

      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );
      assert.equal(
        tokenization.tokens[tokenization.tokens.length - 2].searchModule.inputCount, 1
      );
      assert.deepEqual(
        (tokenization.tokens[tokenization.tokens.length - 2].searchModule as SearchQuotientSpur).lastInput,
        [{sample: inputTransformMap.get(1), p: 1}]
      );
      assert.equal(
        tokenization.tail.searchModule.inputCount, 1
      );
      assert.deepEqual(
        (tokenization.tail.searchModule as SearchQuotientSpur).lastInput,
        [{sample: inputTransformMap.get(2), p: 1}]
      );
    });

    it('handles simple case - deletion of single post-word whitespace', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'].map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransform = { insert: '', deleteLeft: 1, deleteRight: 0, id: 42 };
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(-1, { insert: '', deleteLeft: 1, id: 42 });
      inputTransformMap.set( 0, { insert: '', deleteLeft: 0, id: 42 });

      const edgeWindow = buildEdgeWindow(baseTokenization.tokens, inputTransform, false, testEdgeWindowSpec);
      const tokenization = baseTokenization.evaluateTransition({
        alignment: {
          merges: [],
          splits: [],
          unmappedEdits: [],
          edgeWindow: {
            ...edgeWindow,
            // The range within the window constructed by the prior call for its parameterization.
            // Any adjustments on the boundary token itself are included here.
            retokenization: [...targetTokens.slice(edgeWindow.sliceIndex).map(t => t.text)]
          },
          removedTokenCount: 2
        },
        inputs: [{ sample: inputTransformMap, p: 1 }],
        inputSubsetId: generateSubsetId()
      },
        plainModel,
        inputTransform,
        1
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );
    });

    it('handles simple case - new character added to last token', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'da'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'].map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransform = { insert: 'y', deleteLeft: 0, deleteRight: 0 };
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(0, { insert: 'y', deleteLeft: 0 });

      const edgeWindow = buildEdgeWindow(baseTokenization.tokens, inputTransform, false, testEdgeWindowSpec);
      const tokenization = baseTokenization.evaluateTransition({
        alignment: {
          merges: [],
          splits: [],
          unmappedEdits: [],
          edgeWindow: {
            ...edgeWindow,
            // The range within the window constructed by the prior call for its parameterization.
            // Any adjustments on the boundary token itself are included here.
            retokenization: [...targetTokens.slice(edgeWindow.sliceIndex).map(t => t.text), 'day']
          },
          removedTokenCount: 0
        },
        inputs: [{ sample: inputTransformMap, p: 1 }],
        inputSubsetId: generateSubsetId()
      },
        plainModel,
        inputTransform,
        1
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );
      assert.equal(baseTokenization.tail.searchModule.inputCount, 2);
      assert.deepEqual(tokenization.tail.searchModule.parents, [baseTokenization.tail.searchModule]);
      assert.equal(
        tokenization.tail.searchModule.inputCount, 3
      );
      assert.deepEqual(
        (tokenization.tail.searchModule as SearchQuotientSpur).lastInput,
        [{sample: inputTransformMap.get(0), p: 1}]
      );
    });

    it('handles applied-suggestion cases - final token fully replaced by transform', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'week'].map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransform = { insert: 'week', deleteLeft: 3, deleteRight: 0 };
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(0, { insert: 'week', deleteLeft: 3 });

      const edgeWindow = buildEdgeWindow(baseTokenization.tokens, inputTransform, false, testEdgeWindowSpec);
      const tokenization = baseTokenization.evaluateTransition({
        alignment: {
          merges: [],
          splits: [],
          unmappedEdits: [],
          edgeWindow: {
            ...edgeWindow,
            // The range within the window constructed by the prior call for its parameterization.
            // Any adjustments on the boundary token itself are included here.
            retokenization: [...targetTokens.slice(edgeWindow.sliceIndex).map(t => t.text), 'week']
          },
          removedTokenCount: 0
        },
        inputs: [{ sample: inputTransformMap, p: 1 }],
        inputSubsetId: generateSubsetId()
      },
        plainModel,
        inputTransform,
        1
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );

      // As we fully deleted the old token, the new one "starts" after the deleteLeft.
      // The deleteLeft component should not be included here.  Mocking may be needed!
      assert.equal(
        tokenization.tail.searchModule.inputCount, 1 // is a single transform.
      );
      assert.equal(
        tokenization.tokens[tokenization.tokens.length - 2].searchModule,
        baseTokenization.tokens[tokenization.tokens.length - 2].searchModule
      )
      assert.notEqual(tokenization.tail.searchModule.parents, [baseTokenization.tail.searchModule]);
      assert.deepEqual(
        (tokenization.tail.searchModule as SearchQuotientSpur).lastInput,
        // As we fully deleted the old token, the new one "starts" after the deleteLeft.
        // The deleteLeft component should not be included here.
        [{sample: { insert: 'week', deleteLeft: 0 /* NOT 3 */ }, p: 1}]
      );
    });

    it('properly manages empty token re-inserted by complex transform', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', 'keeps'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'daily', ' ', ''].map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransform = { insert: 'ily ', deleteLeft: 1 + 1 + 5, deleteRight: 0, id: 42 };
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(-2, { insert: 'ily', deleteLeft: 1, id: 42 });
      inputTransformMap.set(-1, { insert: ' ', deleteLeft: 1, id: 42 });
      inputTransformMap.set( 0, { insert: '', deleteLeft: 5, id: 42 });

      const edgeWindow = buildEdgeWindow(baseTokenization.tokens, inputTransform, false, testEdgeWindowSpec);
      const tokenization = baseTokenization.evaluateTransition({
        alignment: {
          merges: [],
          splits: [],
          unmappedEdits: [],
          edgeWindow: {
            ...edgeWindow,
            // The range within the window constructed by the prior call for its parameterization.
            // Any adjustments on the boundary token itself are included here.
            retokenization: [...targetTokens.slice(edgeWindow.sliceIndex).map(t => t.text)]
          },
          removedTokenCount: 0
        },
        inputs: [{ sample: inputTransformMap, p: 1 }],
        inputSubsetId: generateSubsetId()
      },
        plainModel,
        inputTransform,
        1
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );
      const tailIndex = tokenization.tokens.length - 1;
      for(let i of inputTransformMap.keys()) {
        let transform = {...inputTransformMap.get(i)};
        if(i > -2) {
          // If of a greater index, we erased the old token; the deleteLeft
          // was applied there, not to its replacement.
          transform.deleteLeft = 0;
        }

        assert.deepEqual((tokenization.tokens[tailIndex + i].searchModule as SearchQuotientSpur).lastInput,
          [{sample: transform, p: 1}]
        );
      }
    });

    it('handles large delete + insert transforms properly', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'and', ' ', 'banana'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'].map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransform = { insert: ' day', deleteLeft: 2 + 1 + 6, deleteRight: 0, id: 42 };
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(-2, { insert: '', deleteLeft: 2, id: 42 });
      inputTransformMap.set(-1, { insert: ' ', deleteLeft: 1, id: 42 });
      inputTransformMap.set( 0, { insert: 'day', deleteLeft: 6, id: 42 });

      const edgeWindow = buildEdgeWindow(baseTokenization.tokens, inputTransform, false, testEdgeWindowSpec);
      const subsetId = generateSubsetId();
      const tokenization = baseTokenization.evaluateTransition({
        alignment: {
          merges: [],
          splits: [],
          unmappedEdits: [],
          edgeWindow: {
            ...edgeWindow,
            // The range within the window constructed by the prior call for its parameterization.
            retokenization: targetTokens.slice(edgeWindow.sliceIndex, -2).map(t => t.text)
          },
          removedTokenCount: 0
        },
        inputs: [{ sample: inputTransformMap, p: 1 }],
        inputSubsetId: subsetId
      },
        plainModel,
        inputTransform,
        1
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );

      const boundaryToken = tokenization.tokens[tokenization.tokens.length-3];
      const boundaryTailInput = boundaryToken.inputSegments[boundaryToken.inputSegments.length - 1];
      assert.deepEqual(boundaryTailInput, {
        transitionId: inputTransform.id,
        start: 0,
        end: 0
      });

      // The new tail tokens should not include anything from the original tail;
      // the token should be replaced.
      assert.deepEqual(tokenization.tokens[tokenization.tokens.length-2].inputSegments, [{
        transitionId: inputTransform.id,
        start: 0,
        end: 1 // captured the leading whitespace insert
      }]);
      assert.deepEqual(tokenization.tokens[tokenization.tokens.length-1].inputSegments, [{
        transitionId: inputTransform.id,
        start: 1
      }]);

      const tailIndex = tokenization.tokens.length - 1;
      for(let i of inputTransformMap.keys()) {
        let transform = {...inputTransformMap.get(i)};
        if(i > -2) {
          // If of a greater index, we erased the old token; the deleteLeft
          // was applied there, not to its replacement.
          transform.deleteLeft = 0;
        }

        assert.deepEqual((tokenization.tokens[tailIndex + i].searchModule as SearchQuotientSpur).lastInput,
          [{sample: transform, p: 1}]
        );
      }
    });

    it('merges new whitespace character added to last whitespace token if tail is empty', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', '  ', ''].map((t) => (
        {text: t, isWhitespace: t != '' && t.trim() == ''}
      ));
      const inputTransform = { insert: ' ', deleteLeft: 0, deleteRight: 0 };
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(-1, { insert: ' ', deleteLeft: 0 });
      inputTransformMap.set( 0, { insert: '',  deleteLeft: 0 });

      const edgeWindow = buildEdgeWindow(baseTokenization.tokens, inputTransform, false, testEdgeWindowSpec);
      const tokenization = baseTokenization.evaluateTransition({
        alignment: {
          merges: [],
          splits: [],
          unmappedEdits: [],
          edgeWindow: {
            ...edgeWindow,
            // The range within the window constructed by the prior call for its parameterization.
            retokenization: targetTokens.slice(edgeWindow.sliceIndex, -2).map(t => t.text)
          },
          removedTokenCount: 0
        },
        inputs: [{ sample: inputTransformMap, p: 1 }],
        inputSubsetId: generateSubsetId()
      },
        plainModel,
        { insert: ' ', deleteLeft: 0 },
        1
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);
      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );

      const tailIndex = tokenization.tokens.length - 1;
      for(let i of inputTransformMap.keys()) {
        let transform = {...inputTransformMap.get(i)};
        if(i > -1) {
          // If of a greater index, we erased the old token; the deleteLeft
          // was applied there, not to its replacement.
          transform.deleteLeft = 0;
        }

        assert.deepEqual((tokenization.tokens[tailIndex + i].searchModule as SearchQuotientSpur).lastInput,
          [{sample: transform, p: 1}]
        );
      }
    });

    it.skip('handles case that triggers a token merge:  can+\'+t', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', 'can', '\''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', 'can\'t'].map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransform = { insert: 't', deleteLeft: 0, deleteRight: 0 };
      const inputTransformMap: Map<number, Transform> = new Map();
      inputTransformMap.set(0, inputTransform);

      const edgeWindow = buildEdgeWindow(baseTokenization.tokens, inputTransform, false, testEdgeWindowSpec);
      const tokenization = baseTokenization.evaluateTransition({
        alignment: {
          merges: [{
            inputs: [{
                text: 'can',
                index: 8 - edgeWindow.sliceIndex
              }, {
                text: '\'',
                index: 9 - edgeWindow.sliceIndex
              }
            ],
            match: {
              text: 'can\'',
              index: 8 - edgeWindow.sliceIndex
            }
          }],
          splits: [],
          unmappedEdits: [],
          edgeWindow: {
            ...edgeWindow,
            // The range within the window constructed by the prior call for its parameterization.
            retokenization: [...targetTokens.slice(edgeWindow.sliceIndex, -1).map(t => t.text), 'can\'']
          },
          removedTokenCount: 0
        },
        inputs: [{ sample: inputTransformMap, p: 1 }],
        inputSubsetId: generateSubsetId()
      },
        plainModel,
        { insert: 't', deleteLeft: 0 },
        1
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);

      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );

      const basePreTail = baseTokenization.tokens[baseTokenization.tokens.length - 2];
      const baseTail = baseTokenization.tail;
      assert.equal(
        tokenization.tail.searchModule.inputCount,
        basePreTail.searchModule.inputCount + baseTail.searchModule.inputCount + 1 /* +1 - incoming transform */
      );
      assert.deepEqual((tokenization.tail.searchModule as SearchQuotientSpur).lastInput, [{ sample: inputTransform, p: 1 }]);
      assert.equal(tokenization.tail.exampleInput, 'can\'t');
      assert.deepEqual(tokenization.tail.searchModule.bestExample, {
        text: basePreTail.searchModule.bestExample.text + baseTail.searchModule.bestExample.text + inputTransform.insert,
        p: basePreTail.searchModule.bestExample.p * baseTail.searchModule.bestExample.p * 1 /* prob of input transform */
      });
    });

    it.skip('handles case that triggers a token split:  can\' +. => can, \', .', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', 'can\''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const targetTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', 'can', '\'', '.'].map((t) => ({text: t, isWhitespace: t == ' '}));
      const inputTransform = { insert: '.', deleteLeft: 0, deleteRight: 0 };
      const inputTransformMap: Map<number, Transform> = new Map();
      // Lands after the split-off '\''.
      inputTransformMap.set(1, { insert: '.', deleteLeft: 0 });

      const edgeWindow = buildEdgeWindow(baseTokenization.tokens, inputTransform, false, testEdgeWindowSpec);
      const tokenization = baseTokenization.evaluateTransition({
        alignment: {
          merges: [],
          splits: [{
            matches: [{
                text: 'can',
                index: 8 - edgeWindow.sliceIndex,
                textOffset: 0
              }, {
                text: '\'',
                index: 9 - edgeWindow.sliceIndex,
                textOffset: 3
              }
            ],
            input: {
              text: 'can\'',
              index: 8 - edgeWindow.sliceIndex
            }
          }],
          unmappedEdits: [],
          edgeWindow: {
            ...edgeWindow,
            // The range within the window constructed by the prior call for its parameterization.
            retokenization: [...targetTokens.slice(edgeWindow.sliceIndex, -1).map(t => t.text)]
          },
          removedTokenCount: 0
        },
        inputs: [{ sample: inputTransformMap, p: 1 }],
        inputSubsetId: generateSubsetId()
      },
        plainModel,
        inputTransform,
        1
      );

      assert.isOk(tokenization);
      assert.equal(tokenization.tokens.length, targetTokens.length);

      assert.deepEqual(tokenization.tokens.map((t) => ({text: t.exampleInput, isWhitespace: t.isWhitespace})),
        targetTokens
      );

      const prepreTail = tokenization.tokens[tokenization.tokens.length - 3];
      const preTail = tokenization.tokens[tokenization.tokens.length - 2];
      const tail = tokenization.tail;
      assert.equal(
        baseTokenization.tail.searchModule.inputCount,
        prepreTail.searchModule.inputCount + preTail.searchModule.inputCount
      );
      assert.equal(tail.searchModule.inputCount, 1);
      // base tokenization did not include the '.' component.
      assert.deepEqual((preTail.searchModule as SearchQuotientSpur).lastInput, (baseTokenization.tail.searchModule as SearchQuotientSpur).lastInput);
      assert.deepEqual((tail.searchModule as SearchQuotientSpur).lastInput, [{sample: inputTransformMap.get(1), p: 1}]);
      assert.equal(prepreTail.exampleInput, 'can');
      assert.equal(preTail.exampleInput, '\'');
      assert.equal(tail.exampleInput, '.');
      assert.deepEqual({
        text: prepreTail.searchModule.bestExample.text + preTail.searchModule.bestExample.text,
        p: prepreTail.searchModule.bestExample.p * preTail.searchModule.bestExample.p
      }, baseTokenization.tail.searchModule.bestExample);
    });
  });

  describe('buildEdgeWindow', () => {
    describe('with min token count 3, char count 8', () => {
      it('handles empty contexts', () => {
        const baseTokens = [''];
        const idSeed = TOKEN_TRANSFORM_SEED;
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 0 }, true, testEdgeWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: '',
          editBoundary: {
            isPartial: false,
            omitsEmptyToken: false,
            text: '',
            tokenIndex: 0,
            sourceRangeKey: `T${idSeed}`
          },
          deleteLengths: [0],
          sliceIndex: 1
        });
      });

      it('handles empty contexts and invalid Transforms', () => {
        const baseTokens = [''];
        const idSeed = TOKEN_TRANSFORM_SEED;
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 2 }, true, testEdgeWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: '',
          editBoundary: {
            isPartial: true,
            omitsEmptyToken: false,
            text: '',
            tokenIndex: 0,
            sourceRangeKey: `T${idSeed}`
          },
          deleteLengths: [0],
          sliceIndex: 1
        });
      });

      it('builds edge windows for the start of context with no edits', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
        const idSeed = TOKEN_TRANSFORM_SEED;
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 0 }, true, testEdgeWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: 'an apple',
          editBoundary: {
            isPartial: false,
            omitsEmptyToken: false,
            text: 'an',
            tokenIndex: 0,
            sourceRangeKey: `T${idSeed + 0}`
          },
          deleteLengths: [0],
          sliceIndex: 3
        });
      });

      it('builds edge windows for the start of context with no edits - SMP strings', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'].map(s => toMathematicalSMP(s));
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 0 }, true, testEdgeWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: toMathematicalSMP('an apple'),
          editBoundary: {
            isPartial: false,
            omitsEmptyToken: false,
            text: toMathematicalSMP('an'),
            tokenIndex: 0,
            // We'll not worry about matching a specific value for `sourceRangeKey`.
            sourceRangeKey: results.editBoundary.sourceRangeKey
          },
          deleteLengths: [0],
          sliceIndex: 3
        });
      });

      it('builds edge windows for the start of context with deletion edits (1)', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
        const idSeed = TOKEN_TRANSFORM_SEED;
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 2 }, true, testEdgeWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: ' apple a',
          editBoundary: {
            isPartial: false,
            omitsEmptyToken: false,
            text: ' ',
            tokenIndex: 1,
            sourceRangeKey: `T${idSeed + 1}`
          },
          deleteLengths: [2, 0],
          sliceIndex: 5
        });
      });

      it('builds edge windows for the start of context with deletion edits (1) - SMP strings', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'].map(s => toMathematicalSMP(s));
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 2 }, true, testEdgeWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: toMathematicalSMP(' apple a'),
          editBoundary: {
            isPartial: false,
            omitsEmptyToken: false,
            text: ' ',
            tokenIndex: 1,
            // We'll not worry about matching a specific value for `sourceRangeKey`.
            sourceRangeKey: results.editBoundary.sourceRangeKey
          },
          deleteLengths: [2, 0],
          sliceIndex: 5
        });
      });

      it('builds edge windows for the start of context with deletion edits (2)', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
        const idSeed = TOKEN_TRANSFORM_SEED;
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 4 }, true, testEdgeWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: 'pple a day',
          editBoundary: {
            isPartial: true,
            omitsEmptyToken: false,
            text: 'pple',
            tokenIndex: 2,
            sourceRangeKey: `T${idSeed + 2}`
          },
          deleteLengths: [2, 1, 1],
          sliceIndex: 7
        });
      });

      it('builds edge windows for the end of context with no edits', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
        const idSeed = TOKEN_TRANSFORM_SEED;
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));
        baseTokenization.tail.isPartial = true;

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 0 }, false, testEdgeWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: 'apple a day',
          editBoundary: {
            isPartial: true,
            omitsEmptyToken: false,
            text: 'day',
            tokenIndex: 6,
            sourceRangeKey: `T${idSeed + 6}`
          },
          deleteLengths: [0],
          sliceIndex: 2
        });
      });

      it('builds edge windows for the end of context with no edits, trailing whitespace', () => {
        const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
        const idSeed = TOKEN_TRANSFORM_SEED;
        const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));

        const results = buildEdgeWindow(baseTokenization.tokens, { insert: '', deleteLeft: 0, deleteRight: 0 }, false, testEdgeWindowSpec);
        assert.deepEqual(results, {
          retokenizationText: 'apple a day ',
          editBoundary: {
            isPartial: true,
            omitsEmptyToken: true,
            text: ' ',
            tokenIndex: 7,
            sourceRangeKey: `T${idSeed + 7}`
          },
          deleteLengths: [0],
          sliceIndex: 2
        });
      });
    });
  });

  describe('applyContextSlide', () => {
    it('handles empty contexts', () => {
      const baseTokens = [''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: '', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, ['']);
      assert.isFalse(resultTokenization.tokens[0].isPartial);
    });

    it('makes no changes when context does not slide', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));
      assert.isFalse(baseTokenization.tokens[0].isPartial);

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: '', deleteLeft: 0, deleteRight: 0});

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, baseTokens);
      assert.sameOrderedMembers(resultTokenization.exampleInput, baseTokenization.exampleInput);
      assert.isFalse(resultTokenization.tokens[0].isPartial);
    });

    it('preserves tokenization patterns when word slides partially out of window', () => {
      const baseTokens = ['apples', ' ', 'and', ' ', 'bananas'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: '', deleteLeft: 0, deleteRight: 2});

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.deepEqual(resultTokenization.exampleInput, ['ples', ' ', 'and', ' ', 'bananas']);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
    });

    it('preserves tokenization patterns when word slides partially out of window - SMP strings', () => {
      const baseTokens = ['apples', ' ', 'and', ' ', 'bananas'].map(s => toMathematicalSMP(s));
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: '', deleteLeft: 0, deleteRight: 2});

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.deepEqual(resultTokenization.exampleInput, ['ples', ' ', 'and', ' ', 'bananas'].map(s => toMathematicalSMP(s)));
      assert.isTrue(resultTokenization.tokens[0].isPartial);
    });

    it('does not preserve deleted tokens', () => {
      const baseTokens = ['apples', ' ', 'and', ' ', 'bananas'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: '', deleteLeft: 0, deleteRight: 7});

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.deepEqual(resultTokenization.exampleInput, ['and', ' ', 'bananas']);
      // preserves the entirety of what is now the first token
      assert.isFalse(resultTokenization.tokens[0].isPartial);
      assert.equal(resultTokenization.tail.exampleInput, baseTokenization.tail.exampleInput);
    });

    it('creates new lead tokens as needed', () => {
      const baseTokens = ['apples', ' ', 'and', ' ', 'bananas'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: 'I like ', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, ['I', ' ', 'like', ' ', ...baseTokens]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });

    it('creates new lead tokens and edits others as needed', () => {
      const baseTokens = ['apples', ' ', 'and', ' ', 'bananas'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: 'I like pine', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      // preserves the entirety of what is now the first token
      assert.sameOrderedMembers(resultTokenization.exampleInput, ['I', ' ', 'like', ' ', 'pineapples', ...baseTokens.slice(1)]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });

    it('updates internal tracking when backward slide adds word boundary', () => {
      const baseTokens = ['apples', ' ', 'and', ' ', 'bananas'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: ' ', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, [' ', ...baseTokens]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });

    it('handles tokenization shift (from split) when text inserted at start', () => {
      const baseTokens = ['\'t', ' ', 'talk'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: ' ', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, [' ', '\'', 't', ...baseTokens.slice(1)]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });

    it('handles tokenization shift (from merge) when text inserted at start', () => {
      const baseTokens = ['\'', 't', ' ', 'talk'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: 'n', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, ['n\'t', ...baseTokens.slice(2)]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });

    it('maintains token when deleting majority of it via forward slide', () => {
      // string length: 73
      const baseTexts = [
        "applesauce", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem", " ",
        "like", " ", "they'd", " ", "make", " ", "for", " ", "the", " ", "best", " ", "brea"
      ];
      assert.equal(baseTexts.join('').length, 73);

      assert.equal(baseTexts.length, 25);
      const baseTokenization = new ContextTokenization(baseTexts.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: ' ', deleteLeft: 0, deleteRight: 9 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, ['e', ...baseTexts.slice(1)]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });

    it('maintains token when extending it via backward context-window slide', () => {
      const baseTexts = [
        "sauce", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem"
      ];

      const baseTokenization = new ContextTokenization(baseTexts.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: 'apple', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, ['applesauce', ...baseTexts.slice(1)]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
      resultTokenization.tokens.slice(1).forEach((token, index) => assert.isFalse(token.isPartial, `token ${index} (${token.exampleInput}) still marked partial`));
    });

    it('properly handles large backward context-window slide jumps', () => {
      // Note:  this is not the actual pathway used for reverting suggestions,
      // though the scenario is somewhat analogous.
      const baseTexts = [
        "nd", " ", "orange", " ", "juice", " ", "seem", " ", "like", " ", "breakfast"
      ];

      const baseTokenization = new ContextTokenization(baseTexts.map(t => toToken(t)));

      const resultTokenization = baseTokenization.applyContextSlide(plainModel, { insert: 'applesauce a', deleteLeft: 0, deleteRight: 0 });

      assert.notStrictEqual(resultTokenization, baseTokenization);
      assert.sameOrderedMembers(resultTokenization.exampleInput, ['applesauce', ' ', 'and', ...baseTexts.slice(1)]);
      assert.isTrue(resultTokenization.tokens[0].isPartial);
    });
  });

  describe('mapWhitespacedTokenization', () => {
    const edgeWindowSpec = {
      minTokens: 3,
      minChars: 8
    }

    // TODO: deduplicate from `tokenizeTransform`; migrate away from that to this in due time.
    it('detects a single empty transform at index 0 when an empty transform is input', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'date'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: '',
        deleteLeft: 0
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      // for tokenization.
      assert.equal(results.tokenizedTransform.size, 1);
      assert.deepEqual(results.tokenizedTransform.get(0), editTransform);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('properly handles basic token-edit transform', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'da'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: 'y',
        deleteLeft: 0
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      assert.equal(results.tokenizedTransform.size, 1);
      assert.deepEqual(results.tokenizedTransform.get(0), editTransform);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('properly handles basic char-delete transform', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'days'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: '',
        deleteLeft: 1
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      assert.equal(results.tokenizedTransform.size, 1);
      assert.deepEqual(results.tokenizedTransform.get(0), editTransform);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('properly handles simple token-edit transform', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'date'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: 'y',
        deleteLeft: 2
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform,
        edgeWindowSpec
      );

      assert.equal(results.tokenizedTransform.size, 1);
      assert.deepEqual(results.tokenizedTransform.get(0), editTransform);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);

      assert.deepEqual(results.alignment.edgeWindow, {
        retokenization: ['apple', ' ', 'a', ' ', 'da'],
        retokenizationText: 'apple a da',
        editBoundary: {
          text: 'da',
          tokenIndex: 6,
          isPartial: true,
          omitsEmptyToken: false,
          // We'll not worry about matching a specific value for `sourceRangeKey`.
          sourceRangeKey: results.alignment.edgeWindow.editBoundary.sourceRangeKey
        },
        deleteLengths: [2],
        sliceIndex: 2
      });
    });

    it('properly handles simple token-edit transform - smp strings', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'date'].map(t => toMathematicalSMP(t));
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: toMathematicalSMP('y'),
        deleteLeft: 2
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform,
        edgeWindowSpec
      );

      assert.equal(results.tokenizedTransform.size, 1);
      assert.deepEqual(results.tokenizedTransform.get(0), editTransform);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);

      assert.deepEqual(results.alignment.edgeWindow, {
        retokenization: ['apple', ' ', 'a', ' ', 'da'].map(t => toMathematicalSMP(t)),
        retokenizationText: toMathematicalSMP('apple a da'),
        editBoundary: {
          text: toMathematicalSMP('da'),
          tokenIndex: 6,
          isPartial: true,
          omitsEmptyToken: false,
          // We'll not worry about matching a specific value for `sourceRangeKey`.
          sourceRangeKey: results.alignment.edgeWindow.editBoundary.sourceRangeKey
        },
        deleteLengths: [2],
        sliceIndex: 2
      });
    });

    it('properly handles simple token-replacing transform', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'date'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));
      const editTransform = {
        insert: 'week',
        deleteLeft: 4
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      assert.equal(results.tokenizedTransform.size, 1);
      assert.deepEqual(results.tokenizedTransform.get(0), editTransform);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('handles simple token-replacing transform with cross-token deleteLeft', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'date'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      // 'an apple any'
      const editTransform = {
        insert: 'ny',
        deleteLeft: 5
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(-2, {
        insert: 'ny',
        deleteLeft: 0
      });
      expectedMap.set(-1, {
        insert: '',
        deleteLeft: 1
      });
      expectedMap.set(0, {
        insert: '',
        deleteLeft: 4
      });

      assert.equal(results.tokenizedTransform.size, expectedMap.size);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 2);
    });

    it('handles token-replacing transform with cross-token deleteLeft after whitespace', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'date', ' ', ''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      // 'an apple any'
      const editTransform = {
        insert: 'ny',
        deleteLeft: 6
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(-4, {
        insert: 'ny',
        deleteLeft: 0
      });
      expectedMap.set(-3, {
        insert: '',
        deleteLeft: 1
      });
      expectedMap.set(-2, {
        insert: '',
        deleteLeft: 4
      });
      expectedMap.set(-1, {
        insert: '',
        deleteLeft: 1
      });
      expectedMap.set( 0, {
        insert: '',
        deleteLeft: 0
      });

      assert.equal(results.tokenizedTransform.size, expectedMap.size);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 4);
    });

    it('properly handles a simple appended whitespace', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: ' ',
        deleteLeft: 0
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      // The whitespace belongs on the whitespace token that will be added.
      expectedMap.set(1, editTransform);
      // The default-breaker adds an empty token after whitespace, hence this
      // empty transform.
      expectedMap.set(2, { insert: '', deleteLeft: 0 });

      assert.equal(results.tokenizedTransform.size, expectedMap.size);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('properly handles a simple appended period', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: '.',
        deleteLeft: 0
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      // The default wordbreaker does not (currently) append a blank token
      // after standard English punctuation.
      const expectedMap = new Map<number, Transform>();
      expectedMap.set(1, editTransform);
      assert.equal(results.tokenizedTransform.size, expectedMap.size);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('properly deletes a simple appended whitespace', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: '',
        deleteLeft: 1
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      // The whitespace belongs on the whitespace token that will be added.
      expectedMap.set(-1, editTransform);
      expectedMap.set( 0, { insert: '', deleteLeft: 0 });

      assert.equal(results.tokenizedTransform.size, expectedMap.size);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 2);
    });

    it('handles word-breakable transforms (case 1)', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'dat'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: 'y k',
        deleteLeft: 1
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      // dat => day
      expectedMap.set(0, { insert: 'y', deleteLeft: 1 });
      // new whitespace
      expectedMap.set(1, { insert: ' ', deleteLeft: 0 });
      // new 'k' token
      expectedMap.set(2, { insert: 'k', deleteLeft: 0 });
      assert.equal(results.tokenizedTransform.size, expectedMap.size);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);

      assert.deepEqual(results.alignment.edgeWindow, {
        retokenization: ['apple', ' ', 'a', ' ', 'da'],
        retokenizationText: 'apple a da',
        editBoundary: {
          text: 'da',
          tokenIndex: 6,
          isPartial: true,
          omitsEmptyToken: false,
          // We'll not worry about matching a specific value for `sourceRangeKey`.
          sourceRangeKey: results.alignment.edgeWindow.editBoundary.sourceRangeKey
        },
        deleteLengths: [1],
        sliceIndex: 2
      });
    });

    it('handles word-breakable transforms (case 1) - smp strings', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'dat'].map(t => toMathematicalSMP(t));
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: toMathematicalSMP('y k'),
        deleteLeft: 1
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      // dat => day
      expectedMap.set(0, { insert: toMathematicalSMP('y'), deleteLeft: 1 });
      // new whitespace
      expectedMap.set(1, { insert: toMathematicalSMP(' '), deleteLeft: 0 });
      // new 'k' token
      expectedMap.set(2, { insert: toMathematicalSMP('k'), deleteLeft: 0 });
      assert.equal(results.tokenizedTransform.size, expectedMap.size);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);

      assert.deepEqual(results.alignment.edgeWindow, {
        retokenization: ['apple', ' ', 'a', ' ', 'da'].map(t => toMathematicalSMP(t)),
        retokenizationText: toMathematicalSMP('apple a da'),
        editBoundary: {
          text: toMathematicalSMP('da'),
          tokenIndex: 6,
          isPartial: true,
          omitsEmptyToken: false,
          // We'll not worry about matching a specific value for `sourceRangeKey`.
          sourceRangeKey: results.alignment.edgeWindow.editBoundary.sourceRangeKey
        },
        deleteLengths: [1],
        sliceIndex: 2
      });
    });

    it('handles word-breakable transforms (case 2)', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'dat'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: 'y. ',
        deleteLeft: 1
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(0, { insert: 'y', deleteLeft: 1 });
      expectedMap.set(1, { insert: '.', deleteLeft: 0 });
      expectedMap.set(2, { insert: ' ', deleteLeft: 0 });
      expectedMap.set(3, { insert: '', deleteLeft: 0 });
      assert.equal(results.tokenizedTransform.size, 4);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('handles complex breakable cases', () => {
      const baseTokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'date'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      // 'an apple any'
      const editTransform = {
        insert: 'ny day',
        deleteLeft: 5
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      // as => any
      expectedMap.set(-2, { insert: 'ny', deleteLeft: 0 }); // 2 back from the last token before the text insertion point.
      // ' ' replaced with another ' ' (but still edited)
      expectedMap.set(-1, { insert: ' ', deleteLeft: 1 });
      // date => day, but with full replacement due to the large deleteLeft.
      expectedMap.set( 0, { insert: 'day', deleteLeft: 4 }); // The original token before the text insertion point.
      assert.equal(results.tokenizedTransform.size, expectedMap.size);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('properly aligns tokenization of transforms that match-replace existing tokens (1)', () => {
      const baseTokens = ['properly'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      // Case:  the user had input a backspace and then selected a suggestion that restored
      // the original word (which also appended whitespace).
      const editTransform = {
        insert: 'properly ',
        deleteLeft: 8
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(0, { insert: 'properly', deleteLeft: 8 });
      expectedMap.set(1, { insert: ' ', deleteLeft: 0 });
      expectedMap.set(2, { insert: '', deleteLeft: 0 });
      assert.equal(results.tokenizedTransform.size, 3);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('properly aligns tokenization of transforms that match-replace existing tokens (2)', () => {
      const baseTokens = ['do', ' ', 'it', ' ', 'properly'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      // Case:  the user had input a backspace and then selected a suggestion that restored
      // the original word (which also appended whitespace).
      const editTransform = {
        insert: 'properly ',
        deleteLeft: 8
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(0, { insert: 'properly', deleteLeft: 8 });
      expectedMap.set(1, { insert: ' ', deleteLeft: 0 });
      expectedMap.set(2, { insert: '', deleteLeft: 0 });
      assert.equal(results.tokenizedTransform.size, 3);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('properly places extra whitespaces on preceding whitespace token', () => {
      const baseTokens = ['do', ' ', 'it', ' ', 'properly', ' ', ''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      // Adjacent whitespace entries are generally merged into a single blob.
      const editTransform = {
        insert: ' ',  // Should be combined with the final ' ', not the tail ''.
        deleteLeft: 0
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(-1, { insert: ' ', deleteLeft: 0 });
      expectedMap.set(0, { insert: '', deleteLeft: 0 });
      assert.equal(results.tokenizedTransform.size, 2);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('properly aligns degenerate input cases (1)', () => {
      const baseTokens = ['quick', ' ', 'brown', ' ', 'fox'];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: 'fox and brown fox',  // => quick fox and brown fox
        deleteLeft: 9
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform,
        edgeWindowSpec
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(-2, { insert: 'fox', deleteLeft: 5 });
      expectedMap.set(-1, { insert: ' ', deleteLeft: 1 });
      expectedMap.set(0, { insert: 'and', deleteLeft: 3 });
      expectedMap.set(1, { insert: ' ', deleteLeft: 0 });
      expectedMap.set(2, { insert: 'brown', deleteLeft: 0 });
      expectedMap.set(3, { insert: ' ', deleteLeft: 0 });
      expectedMap.set(4, { insert: 'fox', deleteLeft: 0 });
      assert.equal(results.tokenizedTransform.size, 7);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('returns the standard edge window for empty transform inputs', () => {
      const baseTokens = ['quick', ' ', 'brown', ' ', 'fox'];
      const idSeed = TOKEN_TRANSFORM_SEED;
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));

      const editTransform = {
        insert: '',
        deleteLeft: 0
      };

      const windowResults = buildEdgeWindow(baseTokenization.tokens, {...editTransform, deleteRight: 0}, false, edgeWindowSpec);

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform,
        edgeWindowSpec
      );

      assert.deepEqual(results.alignment.edgeWindow, {...windowResults, retokenization: results.alignment.edgeWindow.retokenization});
      assert.deepEqual(results.alignment.edgeWindow, {
        retokenizationText: 'brown fox',
        retokenization: ['brown', ' ', 'fox'],
        editBoundary: {
          isPartial: false,
          omitsEmptyToken: false,
          text: 'fox',
          tokenIndex: 4,
          sourceRangeKey: `T${idSeed + 4}`
        },
        deleteLengths: [0],
        sliceIndex: 2
      });
    });

    it('returns the standard edge window for empty transforms with context-final whitespace', () => {
      const baseTokens = ['quick', ' ', 'brown', ' ', 'fox', ' '];
      const idSeed = TOKEN_TRANSFORM_SEED;
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));

      const editTransform = {
        insert: '',
        deleteLeft: 0
      };

      const windowResults = buildEdgeWindow(baseTokenization.tokens, {...editTransform, deleteRight: 0}, false, edgeWindowSpec);

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform,
        edgeWindowSpec
      );

      assert.deepEqual(results.alignment.edgeWindow, {...windowResults, retokenization: results.alignment.edgeWindow.retokenization});
      assert.deepEqual(results.alignment.edgeWindow, {
        retokenizationText: 'brown fox ',
        retokenization: ['brown', ' ', 'fox', ' '], // no final '' token
        editBoundary: {
          isPartial: false,
          omitsEmptyToken: false,
          text: ' ',
          tokenIndex: 5,
          sourceRangeKey: `T${idSeed + 5}`
        },
        deleteLengths: [0],
        sliceIndex: 2
      });
    });


    it('returns the standard edge window for pure transform w insert inputs', () => {
      const baseTokens = ['quick', ' ', 'brown', ' ', 'fox'];
        const idSeed = TOKEN_TRANSFORM_SEED;
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));

      const editTransform = {
        insert: ' jumped',
        deleteLeft: 0
      };

      const windowResults = buildEdgeWindow(baseTokenization.tokens, {...editTransform, deleteRight: 0}, false, edgeWindowSpec);

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform,
        edgeWindowSpec
      );

      assert.deepEqual(results.alignment.edgeWindow, {...windowResults, retokenization: results.alignment.edgeWindow.retokenization});
      assert.deepEqual(results.alignment.edgeWindow, {
        retokenizationText: 'brown fox',
        retokenization: ['brown', ' ', 'fox'],
        editBoundary: {
          isPartial: false,
          omitsEmptyToken: false,
          text: 'fox',
          tokenIndex: 4,
          sourceRangeKey: `T${idSeed + 4}`
        },
        deleteLengths: [0],
        sliceIndex: 2
      });
    });

    it('returns the proper edge window for transforms w deleteLeft inputs (1)', () => {
      const baseTokens = ['quick', ' ', 'brown', ' ', 'fox'];
      const idSeed = TOKEN_TRANSFORM_SEED;
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));

      const editTransform = {
        insert: 'rog',
        deleteLeft: 2
      };

      const windowResults = buildEdgeWindow(baseTokenization.tokens, {...editTransform, deleteRight: 0}, false, edgeWindowSpec);

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform,
        edgeWindowSpec
      );

      assert.deepEqual(results.alignment.edgeWindow, {...windowResults, retokenization: results.alignment.edgeWindow.retokenization});
      assert.deepEqual(results.alignment.edgeWindow, {
        retokenizationText: ' brown f',
        retokenization: [' ', 'brown', ' ', 'f'],
        editBoundary: {
          isPartial: true,
          omitsEmptyToken: false,
          text: 'f',
          tokenIndex: 4,
          // not yet altered by deleteLeft bits or the newly-incoming transform
          sourceRangeKey: `T${idSeed + 4}`
        },
        deleteLengths: [2],
        sliceIndex: 1
      });
    });

    it('returns the proper edge window for transforms w deleteLeft inputs (2)', () => {
      const baseTokens = ['quick', ' ', 'brown', ' ', 'fox'];
      const idSeed = TOKEN_TRANSFORM_SEED;
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toTransformToken(t)));

      const editTransform = {
        insert: 'fox and brown fox',  // => quick fox and brown fox
        deleteLeft: 9
      };

      const windowResults = buildEdgeWindow(baseTokenization.tokens, {...editTransform, deleteRight: 0}, false, edgeWindowSpec);

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform,
        edgeWindowSpec
      );

      assert.deepEqual(results.alignment.edgeWindow, {...windowResults, retokenization: results.alignment.edgeWindow.retokenization});
      assert.deepEqual(results.alignment.edgeWindow, {
        retokenizationText: 'quick ',
        retokenization: ['quick', ' '],
        editBoundary: {
          isPartial: false,
          omitsEmptyToken: false,
          text: ' ',
          tokenIndex: 1,
          sourceRangeKey: `T${idSeed + 1}`
        },
        deleteLengths: [3, 1, 5, 0],
        sliceIndex: 0
      });
    });

    it('properly handles English contraction transitions (1)', () => {
      const baseTokens = ['she', ' ', 'said', ' ', 'she', ' ', 'can', '\''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: 't',  // => can, ' => can't
        deleteLeft: 0
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform,
        edgeWindowSpec
      );

      const expectedMap = new Map<number, Transform>();
      // index 0:  the merged `can'` token
      expectedMap.set(0, { insert: 't', deleteLeft: 0 });
      assert.equal(results.tokenizedTransform.size, 1);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, [
        {
          inputs: [
            // The `index` values here are pre-offset from the edge window's .sliceIndex.
            { text: 'can', index: 6 - results.alignment.edgeWindow.sliceIndex },
            { text: '\'',  index: 7 - results.alignment.edgeWindow.sliceIndex }
          ], match: { text: 'can\'t', index: 6 - results.alignment.edgeWindow.sliceIndex }
        }
      ]);
      assert.deepEqual(results.alignment.splits, []);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('properly handles English contraction transitions (2)', () => {
      const baseTokens = ['she', ' ', 'said', ' ', 'she', ' ', 'can\''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: ' ',  // => can' => can, ', \u0020
        deleteLeft: 0
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform,
        edgeWindowSpec
      );

      const expectedMap = new Map<number, Transform>();
      // index 0:  the split-off `'` token.
      expectedMap.set(1, { insert: ' ', deleteLeft: 0 });
      expectedMap.set(2, { insert: '', deleteLeft: 0 });
      assert.equal(results.tokenizedTransform.size, 2);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, [
        {
          input: { text: 'can\'', index: 6 - results.alignment.edgeWindow.sliceIndex},
          matches: [
            { text: 'can', index: 6 - results.alignment.edgeWindow.sliceIndex, textOffset: 0 },
            { text: '\'',  index: 7 - results.alignment.edgeWindow.sliceIndex, textOffset: 3 }
          ]
        }
      ]);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });

    it('properly handles English contraction transitions (3)', () => {
      const baseTokens = ['she', ' ', 'said', ' ', 'she', ' ', 'can\''];
      const baseTokenization = new ContextTokenization(baseTokens.map(t => toToken(t)));

      const editTransform = {
        insert: '?',  // => can' => can, ', ?
        deleteLeft: 0
      };

      const results = baseTokenization.mapWhitespacedTokenization(
        plainModel,
        editTransform
      );

      const expectedMap = new Map<number, Transform>();
      expectedMap.set(1, { insert: '?', deleteLeft: 0 });
      assert.equal(results.tokenizedTransform.size, 1);
      assert.deepEqual(results.tokenizedTransform, expectedMap);
      assert.deepEqual(results.alignment.merges, []);
      assert.deepEqual(results.alignment.splits, [
        {
          input: { text: 'can\'', index: 6 - results.alignment.edgeWindow.sliceIndex},
          matches: [
            { text: 'can', index: 6 - results.alignment.edgeWindow.sliceIndex, textOffset: 0 },
            { text: '\'',  index: 7 - results.alignment.edgeWindow.sliceIndex, textOffset: 3 }
          ]
        }
      ]);
      assert.deepEqual(results.alignment.unmappedEdits, []);
      assert.equal(results.alignment.removedTokenCount, 0);
    });
  });

  describe('traceInsertEdits', () => {
    it('handles zero-length insert cases (1)', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const result = traceInsertEdits(tokens, {insert: '', deleteLeft: 0});

      assert.deepEqual(result, {
        stackedInserts: [''],
        firstInsertPostIndex: tokens.length - 1
      });
    });

    it('handles zero-length insert cases (2)', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
      const result = traceInsertEdits(tokens, {insert: '', deleteLeft: 0});

      assert.deepEqual(result, {
        stackedInserts: [''],
        firstInsertPostIndex: tokens.length - 1
      });
    });

    it('ignores deleteLefts and deleteRights', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const result = traceInsertEdits(tokens, {insert: '', deleteLeft: 10, deleteRight: -10});

      assert.deepEqual(result, {
        stackedInserts: [''],
        firstInsertPostIndex: tokens.length - 1
      });
    });

    it('handles simple char output transforms', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day'];
      const result = traceInsertEdits(tokens, {insert: 'y', deleteLeft: 0});

      assert.deepEqual(result, {
        stackedInserts: ['y'],
        firstInsertPostIndex: tokens.length - 1
      });
    });

    it('handles standard whitespace wordbreaks', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
      const result = traceInsertEdits(tokens, {insert: ' ', deleteLeft: 0});

      assert.deepEqual(result, {
        stackedInserts: ['', ' '],
        firstInsertPostIndex: tokens.length - 2
      });
    });

    it('handles large insert strings', () => {
      const tokens = ['an', ' ', 'apple', ' ', 'a', ' ', 'day', ' ', ''];
      const result = traceInsertEdits(tokens, {insert: 'ple a day ', deleteLeft: 0});

      assert.deepEqual(result, {
        stackedInserts: ['', ' ', 'day', ' ', 'a', ' ', 'ple'],
        firstInsertPostIndex: 2
      });
    });
  });

  describe('analyzePathMergesAndSplits', () => {
    it('handles empty tokenizations', () => {
      const results = analyzePathMergesAndSplits([], []);

      assert.deepEqual(results, {
        merges: [],
        splits: [],
        mergeOffset: 0,
        splitOffset: 0,
        editPath: [],
        mappedPath: []
      });
    });

    it('returns unadjusted edit path when no merges or splits are found', () => {
      const results = analyzePathMergesAndSplits(
        ['an', ' ', 'apple', ' ', 'a', ' ', 'da'],
        ['an', ' ', 'apple', ' ', 'a', ' ', 'day']
      );

      const editPath: EditTuple<EditOperation>[] = [
        { op: 'match', input: 0, match: 0 },
        { op: 'match', input: 1, match: 1 },
        { op: 'match', input: 2, match: 2 },
        { op: 'match', input: 3, match: 3 },
        { op: 'match', input: 4, match: 4 },
        { op: 'match', input: 5, match: 5 },
        { op: 'substitute', input: 6, match: 6 },
      ];

      assert.deepEqual(results, {
        merges: [],
        splits: [],
        mergeOffset: 0,
        splitOffset: 0,
        editPath: editPath,
        mappedPath: editPath
      });
    });

    it('returns adjusted path when merges are found', () => {
      const results = analyzePathMergesAndSplits(
        ['she', ' ', 'said', ' ', 'I', ' ', 'can', '\''],
        ['she', ' ', 'said', ' ', 'I', ' ', 'can\'t']
      );

      const editPath: EditTuple<ExtendedEditOperation>[] = [
        { op: 'match', input: 0, match: 0 },
        { op: 'match', input: 1, match: 1 },
        { op: 'match', input: 2, match: 2 },
        { op: 'match', input: 3, match: 3 },
        { op: 'match', input: 4, match: 4 },
        { op: 'match', input: 5, match: 5 },
        { op: 'merge', input: 6, match: 6 }, // gets the 'can'
        { op: 'merge', input: 7, match: 6 }, // gets the \', appends the t
      ];

      const mappedPath: EditTuple<EditOperation>[] = [
        { op: 'match', input: 0, match: 0 },
        { op: 'match', input: 1, match: 1 },
        { op: 'match', input: 2, match: 2 },
        { op: 'match', input: 3, match: 3 },
        { op: 'match', input: 4, match: 4 },
        { op: 'match', input: 5, match: 5 },
        { op: 'substitute', input: 6, match: 6 }, // gets all pieces + the new input `t`.
      ];

      assert.deepEqual(results, {
        merges: [ {
          inputs: [ { text: 'can', index: 6 }, { text: '\'', index: 7 }],
          match: { text: 'can\'t', index: 6 }
        } ],
        splits: [],
        mergeOffset: -1,
        splitOffset: 0,
        editPath,
        mappedPath
      });
    });

    it('returns adjusted path when splits are found', () => {
      const results = analyzePathMergesAndSplits(
        ['\'', 'she', ' ', 'said', ' ', 'I', ' ', 'can\''],
        ['\'', 'she', ' ', 'said', ' ', 'I', ' ', 'can' , '\'', '!']
      );

      const editPath: EditTuple<ExtendedEditOperation>[] = [
        { op: 'match', input: 0, match: 0 },
        { op: 'match', input: 1, match: 1 },
        { op: 'match', input: 2, match: 2 },
        { op: 'match', input: 3, match: 3 },
        { op: 'match', input: 4, match: 4 },
        { op: 'match', input: 5, match: 5 },
        { op: 'match', input: 6, match: 6 },
        { op: 'split', input: 7, match: 7 }, // gets the 'can'
        { op: 'split', input: 7, match: 8 }, // gets the \'
        { op: 'insert', match: 9 }, // gets the t
      ];

      const mappedPath: EditTuple<EditOperation>[] = [
        { op: 'match', input: 0, match: 0 },
        { op: 'match', input: 1, match: 1 },
        { op: 'match', input: 2, match: 2 },
        { op: 'match', input: 3, match: 3 },
        { op: 'match', input: 4, match: 4 },
        { op: 'match', input: 5, match: 5 },
        { op: 'match', input: 6, match: 6 },
        { op: 'match', input: 7, match: 7 }, // gets the 'can'
        { op: 'match', input: 8, match: 8 }, // gets the \'
        // gets the t, which wasn't available as part of the original token
        // being split
        { op: 'insert', match: 9 },
      ];

      assert.deepEqual(results, {
        merges: [],
        splits: [ {
          input: { text: 'can\'', index: 7 },
          matches: [ { text: 'can', index: 7, textOffset: 0 }, { text: '\'', index: 8, textOffset: 3 }]
        } ],
        mergeOffset: 0,
        splitOffset: -1,
        editPath,
        mappedPath
      });
    });
  });

  describe('assembleTransforms', () => {
    it('handles common single-char insert cases', () => {
      const results = assembleTransforms(['a'], [0], 0);

      const expectedMap: Map<number, Transform> = new Map();
      expectedMap.set(0, { insert: 'a', deleteLeft: 0 });
      assert.deepEqual(results, expectedMap);
    });

    it('handles single-char insert cases at adjusted indices', () => {
      const results = assembleTransforms(['a'], [0], 2);

      const expectedMap: Map<number, Transform> = new Map();
      expectedMap.set(2, { insert: 'a', deleteLeft: 0 });
      assert.deepEqual(results, expectedMap);
    });

    it('handles common backspace cases (1)', () => {
      const results = assembleTransforms([''], [1], 0);

      const expectedMap: Map<number, Transform> = new Map();
      expectedMap.set(0, { insert: '', deleteLeft: 1 });
      assert.deepEqual(results, expectedMap);
    });

    it('handles common backspace cases (2)', () => {
      // Emulates backspacing over whitespace immediately before the
      // text-insertion point
      const results = assembleTransforms(['', ''], [0, 1], 0);

      const expectedMap: Map<number, Transform> = new Map();
      expectedMap.set(0, { insert: '', deleteLeft: 1 });
      expectedMap.set(1, { insert: '', deleteLeft: 0 });
      assert.deepEqual(results, expectedMap);
    });

    it('handles cases with equal inserts and deletes', () => {
      const results = assembleTransforms(['day', ' ', 'a'], [2, 1, 2], -2);

      const expectedMap: Map<number, Transform> = new Map();
      expectedMap.set(-2, { insert: 'a', deleteLeft: 2 });
      expectedMap.set(-1, { insert: ' ', deleteLeft: 1 });
      expectedMap.set( 0, { insert: 'day', deleteLeft: 2 });
      assert.deepEqual(results, expectedMap);
    });

    it('handles cases with many inserts and few deletes', () => {
      const results = assembleTransforms(['day', ' ', 'a', ' ', 'apple'], [0, 2, 1], 1);

      const expectedMap: Map<number, Transform> = new Map();
      expectedMap.set(1, { insert: 'apple', deleteLeft: 1 });
      expectedMap.set(2, { insert: ' ', deleteLeft: 2 });
      expectedMap.set(3, { insert: 'a', deleteLeft: 0 });
      expectedMap.set(4, { insert: ' ', deleteLeft: 0 });
      expectedMap.set(5, { insert: 'day', deleteLeft: 0 });
      assert.deepEqual(results, expectedMap);
    });

    it('handles cases with few inserts and many deletes', () => {
      const results = assembleTransforms(['day', ' '], [0, 2, 3, 1, 2], -4);

      const expectedMap: Map<number, Transform> = new Map();
      expectedMap.set(-4, { insert: ' ', deleteLeft: 2 });
      expectedMap.set(-3, { insert: 'day', deleteLeft: 1 });
      expectedMap.set(-2, { insert: '', deleteLeft: 3 });
      expectedMap.set(-1, { insert: '', deleteLeft: 2 });
      expectedMap.set( 0, { insert: '', deleteLeft: 0 });
      assert.deepEqual(results, expectedMap);
    });
  });
});
