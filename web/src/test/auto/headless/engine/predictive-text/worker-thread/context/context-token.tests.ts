/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * This file contains low-level unit tests designed to validate the behavior
 * of the ContextToken class.
 */

import { assert } from 'chai';

// Aliased due to JS keyword.
import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { ContextToken, correction, models, preprocessInputSources } from '@keymanapp/lm-worker/test-index';

import Distribution = LexicalModelTypes.Distribution;
import ExecutionTimer = correction.ExecutionTimer;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;
import { KMWString } from '@keymanapp/web-utils';

var plainModel = new TrieModel(jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker});

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

describe('ContextToken', function() {
  before(() => {
    KMWString.enableSupplementaryPlane(true);
  });

  describe("<constructor>", () => {
    it("(model: LexicalModel)", async () => {
      let token = new ContextToken(plainModel);

      assert.isEmpty(token.searchSpace.inputSequence);
      assert.isEmpty(token.exampleInput);
      assert.isFalse(token.isWhitespace);

      // While searchSpace has no inputs, it _can_ match lexicon entries (via insertions).
      let searchIterator = token.searchSpace.getBestMatches(new ExecutionTimer(Number.POSITIVE_INFINITY, Number.POSITIVE_INFINITY));
      let firstEntry = await searchIterator.next();
      assert.isFalse(firstEntry.done);
    });

    it("(model: LexicalModel, text: string)", () => {
      let token = new ContextToken(plainModel, "and");

      assert.isNotEmpty(token.searchSpace.inputSequence);

      assert.equal(token.searchSpace.inputSequence.map((entry) => entry[0].sample.insert).join(''), 'and');
      token.searchSpace.inputSequence.forEach((entry) => assert.equal(entry[0].sample.deleteLeft, 0));
      assert.deepEqual(token.searchSpace.inputSequence, [..."and"].map((char) => {
        return [{
          sample: {
            insert: char,
            deleteLeft: 0
          },
          p: 1.0
        }];
      }));
      assert.equal(token.exampleInput, 'and');

      assert.isFalse(token.isWhitespace);
    });

    it("(token: ContextToken", () => {
      // Same as in a test above, since we verified that it works correctly.
      let baseToken = new ContextToken(plainModel, "and");
      let clonedToken = new ContextToken(baseToken);

      assert.notEqual(clonedToken.searchSpace, baseToken.searchSpace);
      // Deep equality on .searchSpace can't be directly checked due to the internal complexities involved.
      // We CAN check for the most important members, though.
      assert.notEqual(clonedToken.searchSpace.inputSequence, baseToken.searchSpace.inputSequence);
      assert.deepEqual(clonedToken.searchSpace.inputSequence, baseToken.searchSpace.inputSequence);

      assert.notEqual(clonedToken, baseToken);
      // Perfectly deep-equal when we ignore .searchSpace.
      assert.deepEqual({...clonedToken, searchSpace: null}, {...baseToken, searchSpace: null});
    });
  });

  describe("splitToken()", () => {
    it("handles clean two-way split correctly", () => {
      // Setup phase
      const keystrokeDistributions: Distribution<Transform>[] = [
        [
          { sample: { insert: 'c', deleteLeft: 0 }, p: 0.75 },
          { sample: { insert: 't', deleteLeft: 0 }, p: 0.25 }
        ],
        [
          { sample: { insert: 'a', deleteLeft: 0 }, p: 0.75 },
          { sample: { insert: 'o', deleteLeft: 0 }, p: 0.25 }
        ],
        [
          { sample: { insert: 'n', deleteLeft: 0 }, p: 0.75 },
          { sample: { insert: 'r', deleteLeft: 0 }, p: 0.25 }
        ],
        [
          { sample: { insert: '\'', deleteLeft: 0 }, p: 0.75 },
          { sample: { insert: 't', deleteLeft: 0 }, p: 0.25 }
        ]
      ]

      const tokenToSplit = new ContextToken(plainModel);
      for(let i = 0; i < keystrokeDistributions.length; i++) {
        tokenToSplit.addInput({trueTransform: keystrokeDistributions[i][0].sample, inputStartIndex: 0}, keystrokeDistributions[i]);
      };

      assert.equal(tokenToSplit.sourceText, 'can\'');
      assert.deepEqual(tokenToSplit.searchSpace.inputSequence, keystrokeDistributions);

      // And now for the "fun" part.
      const resultsOfSplit = tokenToSplit.split({
        // Input portion here can be ignored.
        input: {
          text: 'can\'',
          index: 0
        }, matches: [
          // For this part, the text entries are what really matters.
          { text: 'can', index: 0, textOffset: 0 },
          { text: '\'', index: 1, textOffset: 3 }
        ]
      }, plainModel);

      assert.equal(resultsOfSplit.length, 2);
      assert.sameOrderedMembers(resultsOfSplit.map(t => t.exampleInput), ['can', '\'']);
      assert.sameDeepOrderedMembers(resultsOfSplit.map(t => t.searchSpace.inputSequence), [
        keystrokeDistributions.slice(0, 3),
        [keystrokeDistributions[3]]
      ]);
    });

    it("handles mid-transform splits correctly", () => {
      // Setup phase
      const keystrokeDistributions: Distribution<Transform>[] = [
        [
          { sample: { insert: 'biglargetransform', deleteLeft: 0, deleteRight: 0 }, p: 1 },
        ]
      ];
      const splitTextArray = ['big', 'large', 'transform'];

      const tokenToSplit = new ContextToken(plainModel);
      for(let i = 0; i < keystrokeDistributions.length; i++) {
        tokenToSplit.addInput({trueTransform: keystrokeDistributions[i][0].sample, inputStartIndex: 0}, keystrokeDistributions[i]);
      };

      assert.equal(tokenToSplit.sourceText, 'biglargetransform');
      assert.deepEqual(tokenToSplit.searchSpace.inputSequence, keystrokeDistributions);

      // And now for the "fun" part.
      const resultsOfSplit = tokenToSplit.split({
        // Input portion here can be ignored.
        input: {
          text: 'biglargetransform',
          index: 0
        }, matches: [
          // For this part, the text entries are what really matters.
          { text: 'big', index: 0, textOffset: 0 },
          { text: 'large', index: 1, textOffset: 3 },
          { text: 'transform', index: 2, textOffset: 8 }
        ]
      }, plainModel);

      assert.equal(resultsOfSplit.length, 3);
      assert.sameOrderedMembers(resultsOfSplit.map(t => t.exampleInput), splitTextArray);
      assert.sameDeepOrderedMembers(resultsOfSplit.map(t => t.inputRange[0]), [0, 3, 8].map(i => ({
        trueTransform: {
          insert: 'biglargetransform',
          deleteLeft: 0,
          deleteRight: 0
        }, inputStartIndex: i
      })));
      assert.sameDeepOrderedMembers(resultsOfSplit.map(t => t.searchSpace.inputSequence[0]), splitTextArray.map(t => [{
        sample: { insert: t, deleteLeft: 0, deleteRight: 0 }, p: 1
      }]));
    });

    it("handles messy mid-transform splits correctly", () => {
      // Setup phase
      const keystrokeDistributions: Distribution<Transform>[] = [
        [
          { sample: { insert: 'long', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 1 }
        ], [
          { sample: { insert: 'argelovely', deleteLeft: 3, deleteRight: 0, id: 12 }, p: 1 }
        ], [
          { sample: { insert: 'ngtransforms', deleteLeft: 4, deleteRight: 0, id: 13 }, p: 1 }
        ]
      ];
      const splitTextArray = ['large', 'long', 'transforms'];

      const tokenToSplit = new ContextToken(plainModel);
      for(let i = 0; i < keystrokeDistributions.length; i++) {
        tokenToSplit.addInput({trueTransform: keystrokeDistributions[i][0].sample, inputStartIndex: 0}, keystrokeDistributions[i]);
      };

      assert.equal(tokenToSplit.exampleInput, 'largelongtransforms');
      assert.deepEqual(tokenToSplit.searchSpace.inputSequence, keystrokeDistributions);

      // And now for the "fun" part.
      const resultsOfSplit = tokenToSplit.split({
        // Input portion here can be ignored.
        input: {
          text: 'largelongtransforms',
          index: 0
        }, matches: [
          // For this part, the text entries are what really matters.
          { text: 'large', index: 0, textOffset: 0 },
          { text: 'long', index: 1, textOffset: 5 },
          { text: 'transforms', index: 2, textOffset: 9 }
        ]
      }, plainModel);

      assert.equal(resultsOfSplit.length, 3);
      assert.sameOrderedMembers(resultsOfSplit.map(t => t.exampleInput), splitTextArray);
      assert.deepEqual(resultsOfSplit[0].inputRange, [
        { trueTransform: keystrokeDistributions[0][0].sample, inputStartIndex: 0 },
        { trueTransform: keystrokeDistributions[1][0].sample, inputStartIndex: 0 },
      ]);
      assert.deepEqual(resultsOfSplit[1].inputRange, [
        { trueTransform: keystrokeDistributions[1][0].sample, inputStartIndex: 'arge'.length },
        { trueTransform: keystrokeDistributions[2][0].sample, inputStartIndex: 0 },
      ]);
      assert.deepEqual(resultsOfSplit[2].inputRange, [
        { trueTransform: keystrokeDistributions[2][0].sample, inputStartIndex: 'ng'.length }
      ]);

      assert.deepEqual(resultsOfSplit[0].searchSpace.inputSequence, [
        keystrokeDistributions[0],
        keystrokeDistributions[1].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: entry.sample.insert.slice(0, 4) // gets the 'arge' portion & the deleteLefts.
            }, p: entry.p
          }
        }),
      ]);

      assert.deepEqual(resultsOfSplit[1].searchSpace.inputSequence, [
        keystrokeDistributions[1].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: entry.sample.insert.slice('arge'.length),
              deleteLeft: 0
            }, p: entry.p
          }
        }),
        keystrokeDistributions[2].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: entry.sample.insert.slice(0, 'ng'.length), // gets the 'ng' portion.
            }, p: entry.p
          }
        }),
      ]);

      assert.deepEqual(resultsOfSplit[2].searchSpace.inputSequence, [
        keystrokeDistributions[2].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: entry.sample.insert.slice('ng'.length), // drops the 'ng' portion.
              deleteLeft: 0
            }, p: entry.p
          }
        }),
      ]);
    });

    it("handles messy mid-transform splits correctly - non-BMP text", () => {
      // Setup phase
      const keystrokeDistributions: Distribution<Transform>[] = [
        [
          { sample: { insert: toMathematicalSMP('long'), deleteLeft: 0, deleteRight: 0, id: 11 }, p: 1 }
        ], [
          { sample: { insert: toMathematicalSMP('argelovely'), deleteLeft: 3, deleteRight: 0, id: 12 }, p: 1 }
        ], [
          { sample: { insert: toMathematicalSMP('ngtransforms'), deleteLeft: 4, deleteRight: 0, id: 13 }, p: 1 }
        ]
      ];
      const splitTextArray = ['large', 'long', 'transforms'].map(t => toMathematicalSMP(t));

      const tokenToSplit = new ContextToken(plainModel);
      for(let i = 0; i < keystrokeDistributions.length; i++) {
        tokenToSplit.addInput({trueTransform: keystrokeDistributions[i][0].sample, inputStartIndex: 0}, keystrokeDistributions[i]);
      };

      assert.equal(tokenToSplit.exampleInput, toMathematicalSMP('largelongtransforms'));
      assert.deepEqual(tokenToSplit.searchSpace.inputSequence, keystrokeDistributions);

      // And now for the "fun" part.
      const resultsOfSplit = tokenToSplit.split({
        // Input portion here can be ignored.
        input: {
          text: toMathematicalSMP('largelongtransforms'),
          index: 0
        }, matches: [
          // For this part, the text entries are what really matters.
          { text: toMathematicalSMP('large'), index: 0, textOffset: 0 },
          { text: toMathematicalSMP('long'), index: 1, textOffset: 5 },
          { text: toMathematicalSMP('transforms'), index: 2, textOffset: 9 }
        ]
      }, plainModel);

      assert.equal(resultsOfSplit.length, 3);
      assert.sameOrderedMembers(resultsOfSplit.map(t => t.exampleInput), splitTextArray);
      assert.deepEqual(resultsOfSplit[0].inputRange, [
        { trueTransform: keystrokeDistributions[0][0].sample, inputStartIndex: 0 },
        { trueTransform: keystrokeDistributions[1][0].sample, inputStartIndex: 0 },
      ]);
      assert.deepEqual(resultsOfSplit[1].inputRange, [
        { trueTransform: keystrokeDistributions[1][0].sample, inputStartIndex: 'arge'.length },
        { trueTransform: keystrokeDistributions[2][0].sample, inputStartIndex: 0 },
      ]);
      assert.deepEqual(resultsOfSplit[2].inputRange, [
        { trueTransform: keystrokeDistributions[2][0].sample, inputStartIndex: 'ng'.length }
      ]);

      assert.deepEqual(resultsOfSplit[0].searchSpace.inputSequence, [
        keystrokeDistributions[0],
        keystrokeDistributions[1].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: KMWString.substring(entry.sample.insert, 0, 4) // gets the 'arge' portion & the deleteLefts.
            }, p: entry.p
          }
        }),
      ]);

      assert.deepEqual(resultsOfSplit[1].searchSpace.inputSequence, [
        keystrokeDistributions[1].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: KMWString.substring(entry.sample.insert, 'arge'.length),
              deleteLeft: 0
            }, p: entry.p
          }
        }),
        keystrokeDistributions[2].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: KMWString.substring(entry.sample.insert, 0, 'ng'.length), // gets the 'ng' portion.
            }, p: entry.p
          }
        }),
      ]);

      assert.deepEqual(resultsOfSplit[2].searchSpace.inputSequence, [
        keystrokeDistributions[2].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: KMWString.substring(entry.sample.insert, 'ng'.length), // drops the 'ng' portion.
              deleteLeft: 0
            }, p: entry.p
          }
        }),
      ]);
    });
  });
});

describe('preprocessInputSources', () => {
  it('properly preprocesses deleteLefts in the transforms', () => {
    const transforms: Transform[] = [
      { insert: 'long', deleteLeft: 0, deleteRight: 0 },
      { insert: 'argelovely', deleteLeft: 3, deleteRight: 0 },
      { insert: 'ngtransforms', deleteLeft: 4, deleteRight: 0 }
    ];

    const results = preprocessInputSources(transforms.map((t) => ({
      trueTransform: t,
      inputStartIndex: 0
    })));

    assert.equal(results.length, transforms.length);
    assert.sameOrderedMembers(results.map((entry) => entry.trueTransform.insert), ['l', 'argelo', 'ngtransforms']);
    assert.sameOrderedMembers(results.map((entry) => entry.trueTransform.deleteLeft), [0, 0, 0]);
  });
});