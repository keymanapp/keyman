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

import { ContextToken, correction, generateSubsetId, getBestMatches, InputSegment, models, SearchPath } from '@keymanapp/lm-worker/test-index';

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

      assert.equal(token.searchSpace.inputCount, 0);
      assert.isEmpty(token.exampleInput);
      assert.isFalse(token.isWhitespace);

      // While searchSpace has no inputs, it _can_ match lexicon entries (via insertions).
      let searchIterator = getBestMatches([token.searchSpace], new ExecutionTimer(Number.POSITIVE_INFINITY, Number.POSITIVE_INFINITY));
      let firstEntry = await searchIterator.next();
      assert.isFalse(firstEntry.done);
    });

    it("(model: LexicalModel, text: string)", () => {
      let token = new ContextToken(plainModel, "and");

      assert.equal(token.searchSpace.bestExample.text, 'and');
      assert.equal(token.exampleInput, 'and');

      assert.equal(token.searchSpace.inputCount, 3);
      assert.isTrue(token.searchSpace.hasInputs([
        [{sample: { insert: 'a', deleteLeft: 0 }, p: 1}],
        [{sample: { insert: 'n', deleteLeft: 0 }, p: 1}],
        [{sample: { insert: 'd', deleteLeft: 0 }, p: 1}]
      ]));

      assert.isFalse(token.isWhitespace);
    });

    it("(token: ContextToken", () => {
      // Same as in a test above, since we verified that it works correctly.
      let baseToken = new ContextToken(plainModel, "and");
      let clonedToken = new ContextToken(baseToken);

      assert.equal(clonedToken.searchSpace, baseToken.searchSpace);
      // Deep equality on .searchSpace can't be directly checked due to the internal complexities involved.
      // We CAN check for the most important members, though.
      assert.equal(clonedToken.searchSpace, baseToken.searchSpace);

      assert.notEqual(clonedToken, baseToken);
      // Perfectly deep-equal when we ignore .searchSpace.
      assert.deepEqual({...clonedToken, searchSpace: null}, {...baseToken, searchSpace: null});
    });
  });

  describe("merge()", () => {
    it("merges three tokens without previously-split transforms", () => {
      const token1 = new ContextToken(plainModel, "can");
      const token2 = new ContextToken(plainModel, "'");
      const token3 = new ContextToken(plainModel, "t");

      const merged = ContextToken.merge([token1, token2, token3]);
      assert.equal(merged.exampleInput, "can't");
      token1.inputSegments.forEach((entry) => assert.isTrue(merged.inputSegments.indexOf(entry) > -1));
      token2.inputSegments.forEach((entry) => assert.isTrue(merged.inputSegments.indexOf(entry) > -1));
      token3.inputSegments.forEach((entry) => assert.isTrue(merged.inputSegments.indexOf(entry) > -1));

      assert.isTrue(merged.searchSpace.hasInputs([
        [{sample: { insert: 'c', deleteLeft: 0 }, p: 1}],
        [{sample: { insert: 'a', deleteLeft: 0 }, p: 1}],
        [{sample: { insert: 'n', deleteLeft: 0 }, p: 1}],
        [{sample: { insert: '\'', deleteLeft: 0 }, p: 1}],
        [{sample: { insert: 't', deleteLeft: 0 }, p: 1}]
      ]));
    });

    it("merges three tokens from single previously-split transforms", () => {
      const srcTransform = { insert: "can't", deleteLeft: 0, deleteRight: 0, id: 1 };
      const srcSubsetId = generateSubsetId();

      let token1 = new ContextToken(plainModel);
      let token2 = new ContextToken(plainModel);
      let token3 = new ContextToken(plainModel);

      token1 = new ContextToken(new SearchPath(
        token1.searchSpace,
        [{sample: {insert: 'can', deleteLeft: 0, deleteRight: 0, id: 1}, p: 1}], {
          segment: {
            transitionId: srcTransform.id,
            start: 0
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetId
        }
      ));

      token2 = new ContextToken(new SearchPath(
        token2.searchSpace,
        [{sample: {insert: "'", deleteLeft: 0, deleteRight: 0, id: 1}, p: 1}], {
          segment: {
            transitionId: srcTransform.id,
            start: 3
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetId
        }
      ));

      token3 = new ContextToken(new SearchPath(
        token3.searchSpace,
        [{sample: {insert: 't', deleteLeft: 0, deleteRight: 0, id: 1}, p: 1}], {
          segment: {
            transitionId: srcTransform.id,
            start: 4
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetId
        }
      ));

      const merged = ContextToken.merge([token1, token2, token3]);
      assert.equal(merged.exampleInput, "can't");
      assert.deepEqual(merged.inputSegments, [ {
        transitionId: srcTransform.id,
        start: 0
      } ]);
      assert.equal(merged.searchSpace.inputCount, 1);
      assert.deepEqual((merged.searchSpace as SearchPath).lastInput, [{sample: srcTransform, p: 1}]);
    });

    it("merges four tokens with previously-split transforms", () => {
      // TODO:  need another case - pref where there are two diff boundary transforms
      // and where each token has multiple constituent transforms.
      const srcTransforms = [
        { insert: "apple", deleteLeft: 0, deleteRight: 0, id: 1 },
        { insert: "sands", deleteLeft: 0, deleteRight: 0, id: 2 },
        { insert: "our", deleteLeft: 0, deleteRight: 0, id: 3 },
        { insert: "grapes", deleteLeft: 0, deleteRight: 0, id: 4 }
      ];
      const srcSubsetIds = [
        generateSubsetId(),
        generateSubsetId(),
        generateSubsetId(),
        generateSubsetId()
      ];

      // apples
      let token1 = new ContextToken(plainModel);
      // and
      let token2 = new ContextToken(plainModel);
      // sour
      let token3 = new ContextToken(plainModel);
      // grapes
      let token4 = new ContextToken(plainModel);

      token1 = new ContextToken(new SearchPath(
        token1.searchSpace,
        [{sample: srcTransforms[0], p: 1}], {
          segment: {
            transitionId: srcTransforms[0].id,
            start: 0
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[0]
        }
      ));
      token1 = new ContextToken(new SearchPath(
        token1.searchSpace,
        [{sample: {insert: 's', deleteLeft: 0, deleteRight: 0, id: 2}, p: 1}], {
          segment: {
            transitionId: srcTransforms[1].id,
            start: 0
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[1]
        }
      ));

      token2 = new ContextToken(new SearchPath(
        token2.searchSpace,
        [{sample: {insert: "and", deleteLeft: 0, deleteRight: 0, id: 2}, p: 1}], {
          segment: {
            transitionId: srcTransforms[1].id,
            start: 1
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[1]
        }
      ));

      token3 = new ContextToken(new SearchPath(
        token3.searchSpace,
        [{sample: {insert: 's', deleteLeft: 0, deleteRight: 0, id: 2}, p: 1}], {
          segment: {
            transitionId: srcTransforms[1].id,
            start: 4
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[1]
        }
      ));
      token3 = new ContextToken(new SearchPath(
        token3.searchSpace,
        [{sample: srcTransforms[2], p: 1}], {
          segment: {
            transitionId: srcTransforms[2].id,
            start: 0
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[2]
        }
      ));

      token4 = new ContextToken(new SearchPath(
        token4.searchSpace,
        [{sample: srcTransforms[3], p: 1}], {
          segment: {
            transitionId: srcTransforms[3].id,
            start: 0
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[3]
        }
      ));

      const tokensToMerge = [token1, token2, token3, token4];
      const merged = ContextToken.merge(tokensToMerge);
      assert.equal(merged.exampleInput, "applesandsourgrapes");
      assert.deepEqual(merged.inputSegments, srcTransforms.map((t, i) => ({
        transitionId: t.id,
        start: 0
      }) ));
      assert.isTrue(merged.searchSpace.hasInputs(
        srcTransforms.map((t) => ([{sample: t, p: 1}]))
      ));
    });

    it("merges four tokens with previously-split transforms - non-BMP text", () => {
      // TODO:  need another case - pref where there are two diff boundary transforms
      // and where each token has multiple constituent transforms.
      const srcTransforms = [
        { insert: toMathematicalSMP("apple"), deleteLeft: 0, deleteRight: 0, id: 1 },
        { insert: toMathematicalSMP("sands"), deleteLeft: 0, deleteRight: 0, id: 2 },
        { insert: toMathematicalSMP("our"), deleteLeft: 0, deleteRight: 0, id: 3 },
        { insert: toMathematicalSMP("grapes"), deleteLeft: 0, deleteRight: 0, id: 4 }
      ];
      const srcSubsetIds = [
        generateSubsetId(),
        generateSubsetId(),
        generateSubsetId(),
        generateSubsetId()
      ];

            // apples
      let token1 = new ContextToken(plainModel);
      // and
      let token2 = new ContextToken(plainModel);
      // sour
      let token3 = new ContextToken(plainModel);
      // grapes
      let token4 = new ContextToken(plainModel);

      token1 = new ContextToken(new SearchPath(
        token1.searchSpace,
        [{sample: srcTransforms[0], p: 1}], {
          segment: {
            transitionId: srcTransforms[0].id,
            start: 0
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[0]
        }
      ));
      token1 = new ContextToken(new SearchPath(
        token1.searchSpace,
        [{sample: {insert: toMathematicalSMP('s'), deleteLeft: 0, deleteRight: 0, id: 2}, p: 1}], {
          segment: {
            transitionId: srcTransforms[1].id,
            start: 0
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[1]
        }
      ));

      token2 = new ContextToken(new SearchPath(
        token2.searchSpace,
        [{sample: {insert: toMathematicalSMP("and"), deleteLeft: 0, deleteRight: 0, id: 2}, p: 1}], {
          segment: {
            transitionId: srcTransforms[1].id,
            start: 1
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[1]
        }
      ));

      token3 = new ContextToken(new SearchPath(
        token3.searchSpace,
        [{sample: {insert: toMathematicalSMP('s'), deleteLeft: 0, deleteRight: 0, id: 2}, p: 1}], {
          segment: {
            transitionId: srcTransforms[1].id,
            start: 4
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[1]
        }
      ));
      token3 = new ContextToken(new SearchPath(
        token3.searchSpace,
        [{sample: srcTransforms[2], p: 1}], {
          segment: {
            transitionId: srcTransforms[2].id,
            start: 0
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[2]
        }
      ));

      token4 = new ContextToken(new SearchPath(
        token4.searchSpace,
        [{sample: srcTransforms[3], p: 1}], {
          segment: {
            transitionId: srcTransforms[3].id,
            start: 0
          },
          bestProbFromSet: 1,
          subsetId: srcSubsetIds[3]
        }
      ));

      const tokensToMerge = [token1, token2, token3, token4];
      const merged = ContextToken.merge(tokensToMerge);
      assert.equal(merged.exampleInput, toMathematicalSMP("applesandsourgrapes"));
      assert.deepEqual(merged.inputSegments, srcTransforms.map((t, i) => ({
        transitionId: t.id,
        start: 0
      }) ));
      assert.isTrue(merged.searchSpace.hasInputs(
        srcTransforms.map((t) => ([{sample: t, p: 1}]))
      ));
    });
  });

  describe("split()", () => {
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

      let tokenToSplit = new ContextToken(plainModel);
      for(let i = 0; i < keystrokeDistributions.length; i++) {
        tokenToSplit = new ContextToken(new SearchPath(
          tokenToSplit.searchSpace,
          keystrokeDistributions[i], {
            segment: {
              transitionId: keystrokeDistributions[i][0].sample.id,
              start: 0
            }, bestProbFromSet: .75,
            subsetId: generateSubsetId()
          }
        ));
      };

      tokenToSplit.searchSpace.hasInputs(keystrokeDistributions);

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
      });

      assert.equal(resultsOfSplit.length, 2);
      assert.sameOrderedMembers(resultsOfSplit.map(t => t.exampleInput), ['can', '\'']);
      assert.isTrue(resultsOfSplit[0].searchSpace.hasInputs(keystrokeDistributions.slice(0, 3)));
      assert.isTrue(resultsOfSplit[1].searchSpace.hasInputs([keystrokeDistributions[3]]));
    });

    it("handles mid-transform splits correctly", () => {
      // Setup phase
      const keystrokeDistributions: Distribution<Transform>[] = [
        [
          { sample: { insert: 'biglargetransform', deleteLeft: 0, deleteRight: 0, id: 13 }, p: 1 },
        ]
      ];
      const splitTextArray = ['big', 'large', 'transform'];
      const subsetId = generateSubsetId();

      let tokenToSplit = new ContextToken(plainModel);
      for(let i = 0; i < keystrokeDistributions.length; i++) {
        tokenToSplit = new ContextToken(new SearchPath(
          tokenToSplit.searchSpace,
          keystrokeDistributions[i], {
            segment: {
              transitionId: keystrokeDistributions[i][0].sample.id,
              start: 0
            },
            bestProbFromSet: 1,
            subsetId
          }
        ));
      };

      assert.isTrue(tokenToSplit.searchSpace.hasInputs(keystrokeDistributions));

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
      });

      assert.equal(resultsOfSplit.length, 3);
      assert.sameOrderedMembers(resultsOfSplit.map(t => t.exampleInput), splitTextArray);
      const offsets = [0, 3, 8];
      assert.sameDeepOrderedMembers(resultsOfSplit.map(t => t.inputSegments[0]), [0, 1, 2].map(i => {
        const segment: InputSegment = {
          transitionId: 13,
          start: offsets[i]
        };

        if(offsets[i+1] !== undefined) {
          segment.end = offsets[i+1];
        }

        return segment;
      }));

      for(let i = 0; i < resultsOfSplit.length; i++) {
        assert.isTrue(resultsOfSplit[i].searchSpace.hasInputs([
          [{sample: { insert: splitTextArray[i], deleteLeft: 0, deleteRight: 0, id: 13 }, p: 1}]
        ]));
      }
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
      const subsetIds = [
        generateSubsetId(),
        generateSubsetId(),
        generateSubsetId()
      ];

      let tokenToSplit = new ContextToken(plainModel);
      for(let i = 0; i < keystrokeDistributions.length; i++) {
        tokenToSplit = new ContextToken(new SearchPath(
          tokenToSplit.searchSpace,
          keystrokeDistributions[i], {
            segment: {
              transitionId: keystrokeDistributions[i][0].sample.id,
              start: 0
            },
            bestProbFromSet: 1,
            subsetId: subsetIds[i]
          }
        ));
      };

      assert.equal(tokenToSplit.exampleInput, 'largelongtransforms');
      tokenToSplit.searchSpace.hasInputs(keystrokeDistributions);

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
      });

      assert.equal(resultsOfSplit.length, 3);
      assert.sameOrderedMembers(resultsOfSplit.map(t => t.exampleInput), splitTextArray);
      assert.deepEqual(resultsOfSplit[0].inputSegments, [
        {
          transitionId: keystrokeDistributions[0][0].sample.id,
          start: 0
        }, {
          transitionId: keystrokeDistributions[1][0].sample.id,
          start: 0,
          end: 'arge'.length
        },
      ]);
      assert.deepEqual(resultsOfSplit[1].inputSegments, [
        {
          transitionId: keystrokeDistributions[1][0].sample.id,
          start: 'arge'.length
        }, {
          transitionId: keystrokeDistributions[2][0].sample.id,
          start: 0,
          end: 'ng'.length
        },
      ]);
      assert.deepEqual(resultsOfSplit[2].inputSegments, [
        {
          transitionId: keystrokeDistributions[2][0].sample.id,
          start: 'ng'.length,
        }
      ]);

      assert.isTrue(resultsOfSplit[0].searchSpace.hasInputs([
        keystrokeDistributions[0],
        keystrokeDistributions[1].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: entry.sample.insert.slice(0, 4) // gets the 'arge' portion & the deleteLefts.
            }, p: entry.p
          }
        }),
      ]));

      assert.isTrue(resultsOfSplit[1].searchSpace.hasInputs([
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
      ]));

      assert.isTrue(resultsOfSplit[2].searchSpace.hasInputs([
        keystrokeDistributions[2].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: entry.sample.insert.slice('ng'.length), // drops the 'ng' portion.
              deleteLeft: 0
            }, p: entry.p
          }
        }),
      ]));
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
      const subsetIds = [
        generateSubsetId(),
        generateSubsetId(),
        generateSubsetId()
      ];

      let tokenToSplit = new ContextToken(plainModel);
      for(let i = 0; i < keystrokeDistributions.length; i++) {
        tokenToSplit = new ContextToken(new SearchPath(
          tokenToSplit.searchSpace,
          keystrokeDistributions[i], {
            segment: {
              transitionId: keystrokeDistributions[i][0].sample.id,
              start: 0
            },
            bestProbFromSet: 1,
            subsetId: subsetIds[i]
          }
        ));
      };

      assert.equal(tokenToSplit.exampleInput, toMathematicalSMP('largelongtransforms'));
      tokenToSplit.searchSpace.hasInputs(keystrokeDistributions);

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
      });

      assert.equal(resultsOfSplit.length, 3);
      assert.sameOrderedMembers(resultsOfSplit.map(t => t.exampleInput), splitTextArray);
      assert.deepEqual(resultsOfSplit[0].inputSegments, [{
          transitionId: keystrokeDistributions[0][0].sample.id,
          start: 0
        }, {
          transitionId: keystrokeDistributions[1][0].sample.id,
          start: 0,
          end: 'arge'.length
        },
      ]);
      assert.deepEqual(resultsOfSplit[1].inputSegments, [{
          transitionId: keystrokeDistributions[1][0].sample.id,
          start: 'arge'.length
        }, {
          transitionId: keystrokeDistributions[2][0].sample.id,
          start: 0,
          end: 'ng'.length
        }
      ]);
      assert.deepEqual(resultsOfSplit[2].inputSegments, [{
        transitionId: keystrokeDistributions[2][0].sample.id,
        start: 'ng'.length
      }]);

      assert.isTrue(resultsOfSplit[0].searchSpace.hasInputs([
        keystrokeDistributions[0],
        keystrokeDistributions[1].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: KMWString.substring(entry.sample.insert, 0, 4) // gets the 'arge' portion & the deleteLefts.
            }, p: entry.p
          }
        }),
      ]));

      assert.isTrue(resultsOfSplit[1].searchSpace.hasInputs([
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
      ]));

      assert.isTrue(resultsOfSplit[2].searchSpace.hasInputs([
        keystrokeDistributions[2].map((entry) => {
          return {
            sample: {
              ...entry.sample,
              insert: KMWString.substring(entry.sample.insert, 'ng'.length), // drops the 'ng' portion.
              deleteLeft: 0
            }, p: entry.p
          }
        }),
      ]));
    });
  });
});