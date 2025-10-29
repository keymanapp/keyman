/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-09
 *
 * This file defines tests for the SearchSpace class of the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { KMWString } from '@keymanapp/web-utils';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { correction, getBestMatches, models, SearchPath } from '@keymanapp/lm-worker/test-index';

import SearchResult = correction.SearchResult;
import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

function buildTestTimer() {
  return new correction.ExecutionTimer(Number.MAX_VALUE, Number.MAX_VALUE);
}

// https://www.compart.com/en/unicode/block/U+1D400
const mathBoldUpperA = 0x1D400; // Mathematical Bold Capital A
const mathBoldLowerA = 0x1D41A; //                   Small   A

/**
 * Converts a plain, Latin-script ASCII [a-zA-Z]+ string into its
 * equivalent in the MATHEMATICAL BOLD range, which is non-BMP.
 *
 * Other characters will not be coerced into this range and will
 * retain their original values.
 * @param text
 * @returns
 */
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

describe('Correction Searching', () => {
  const checkRepeatableResults_teh = async (iter: AsyncGenerator<correction.SearchResult, any, any>) => {
    const firstIterResult = await iter.next();  // {value: <actual value>, done: <iteration complete?>}
    assert.isFalse(firstIterResult.done);

    const firstResult: correction.SearchResult = firstIterResult.value; // Retrieves <actual value>
    // No checks on the first set's cost.
    assert.equal(firstResult.matchString, "ten");

    // All start with 'te' but one, and invoke one edit of the same cost.
    // 'th' has an 'h' at the same cost (input 3) of the 'e' (input 2).
    const secondBatch = [
      'tec', 'tel', 'tem',
      'ter', 'tes', 'th',
      'te'
    ];

    async function checkBatch(batch: string[], prevCost: number) {
      let cost;
      while(batch.length > 0) {
        const iter_result = await iter.next();
        assert.isFalse(iter_result.done);

        const result = iter_result.value;
        assert.isAbove(result.totalCost, prevCost);
        if(cost !== undefined) {
          assert.equal(result.totalCost, cost);
        } else {
          cost = result.totalCost;
        }

        const matchIndex = batch.findIndex((entry) => entry == result.matchString);
        assert.notEqual(matchIndex, -1, `'${result.matchString}' received as prediction too early`);
        batch.splice(matchIndex, 1);
      }

      return cost;
    }

    const secondCost = await checkBatch(secondBatch, firstResult.totalCost);

    // Single hard edit, all other input probability aspects are equal
    const thirdBatch = [
      // 't' -> 'b' (sub)
      'beh',
      // '' -> 'c' (insertion)
      'tech'
    ];

    await checkBatch(thirdBatch, secondCost);

    // All replace the low-likelihood case for the third input.
    const fourthBatch = [
      'the', 'thi', 'tho', 'thr',
      'thu', 'tha'
    ];

    await checkBatch(fourthBatch, secondCost);

    // Replace the _first_ input's char OR insert an extra char,
    // also matching the low-likelihood third-char option.
    const fifthBatch = [
      'cen', 'en',  'gen',
      'ken', 'len', 'men',
      'sen', 'then', 'wen'
    ];

    await checkBatch(fifthBatch, secondCost);
  }

  it('Simple search without input', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    const searchSpace = new SearchPath(testModel);

    const iter = getBestMatches([searchSpace], buildTestTimer());
    const firstResult = await iter.next();
    assert.isFalse(firstResult.done);
  });

  // Hmm... how best to update this...
  it('Simple search (paralleling "Small integration test")', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    const searchPath = new SearchPath(testModel);

    // VERY artificial distributions.
    const synthInput1 = [
      {sample: {insert: 't', deleteLeft: 0}, p: 1} // Transform, probability
    ];

    const synthInput2 = [
      {sample: {insert: 'e', deleteLeft: 0}, p: 0.75}, // Transform, probability
      {sample: {insert: 'h', deleteLeft: 0}, p: 0.25}
    ];

    const synthInput3 = [
      {sample: {insert: 'h', deleteLeft: 0}, p: 0.75}, // Transform, probability
      {sample: {insert: 'n', deleteLeft: 0}, p: 0.25}
    ];

    const searchPath1 = new SearchPath(searchPath, synthInput1, synthInput1[0]);
    const searchPath2 = new SearchPath(searchPath1, synthInput2, synthInput2[0]);
    const searchPath3 = new SearchPath(searchPath2, synthInput3, synthInput3[0]);

    assert.notEqual(searchPath1.spaceId, searchPath.spaceId);
    assert.notEqual(searchPath2.spaceId, searchPath1.spaceId);
    assert.notEqual(searchPath3.spaceId, searchPath2.spaceId);

    const iter = getBestMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter);
  });

  it('Allows reiteration (sequentially)', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    const searchPath = new SearchPath(testModel);

    // VERY artificial distributions.
    const synthInput1 = [
      {sample: {insert: 't', deleteLeft: 0}, p: 1} // Transform, probability
    ];

    const synthInput2 = [
      {sample: {insert: 'e', deleteLeft: 0}, p: 0.75}, // Transform, probability
      {sample: {insert: 'h', deleteLeft: 0}, p: 0.25}
    ];

    const synthInput3 = [
      {sample: {insert: 'h', deleteLeft: 0}, p: 0.75}, // Transform, probability
      {sample: {insert: 'n', deleteLeft: 0}, p: 0.25}
    ];

    const searchPath1 = new SearchPath(searchPath, synthInput1, synthInput1[0]);
    const searchPath2 = new SearchPath(searchPath1, synthInput2, synthInput2[0]);
    const searchPath3 = new SearchPath(searchPath2, synthInput3, synthInput3[0]);

    assert.notEqual(searchPath1.spaceId, searchPath.spaceId);
    assert.notEqual(searchPath2.spaceId, searchPath1.spaceId);
    assert.notEqual(searchPath3.spaceId, searchPath2.spaceId);

    const iter = getBestMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter);

    // The key: do we get the same results the second time?
    // Reset the iterator first...
    const iter2 = getBestMatches([searchPath3], buildTestTimer()); // disables the correction-search timeout.
    await checkRepeatableResults_teh(iter2);
  });

  it('Empty search space, loaded model', async () => {
    // The combinatorial effect here is a bit much to fully test.
    const rootTraversal = testModel.traverseFromRoot();
    assert.isNotEmpty(rootTraversal);

    const searchSpace = new SearchPath(testModel);
    const timer = buildTestTimer();
    const iter = getBestMatches([searchSpace], timer);

    // While there's no input, insertion operations can produce suggestions.
    const resultState = await iter.next();
    const result: SearchResult = resultState.value;

    // Just one suggestion root should be returned as the first result.
    assert.equal(result.totalCost, 0);             // Gives a perfect match
    assert.equal(result.inputSequence.length, 0);  // for a state with no input and
    assert.equal(result.matchString, '');          // an empty match string.
    assert.isFalse(resultState.done);

    // Should be able to reach more, though.
    const laterResultState = await iter.next();
    const laterResult: SearchResult = laterResultState.value;

    // Edit required:  an 'insertion' edge (no input matched, but char pulled
    // from lexicon)
    assert.isAbove(laterResult.totalCost, 0);
    // The most likely word in the lexicon starts with 't'.
    assert.equal(laterResult.matchString, 't');
    assert.isFalse(resultState.done);
  });

  // Tests for mechanics reliant upon the SearchSpace interface, rather than a specific implementation.
  describe('SearchPath + SearchSpace', () => {
  });

  describe('SearchPath', () => {
    it('initializes from a lexical model', () => {
      const path = new SearchPath(testModel);
      assert.equal(path.inputCount, 0);
      assert.equal(path.codepointLength, 0);
      assert.isNumber(path.spaceId);
      assert.deepEqual(path.bestExample, {text: '', p: 1});
      assert.deepEqual(path.parents, []);
      assert.isNotOk(path.inputs);
      assert.equal(path.likeliestSourceText, '');
    });

    it('may be extended from root path', () => {
      const rootPath = new SearchPath(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      const extendedPath = new SearchPath(rootPath, leadEdgeDistribution, leadEdgeDistribution[0]);

      assert.equal(extendedPath.inputCount, 1);
      assert.equal(extendedPath.codepointLength, 1);
      assert.isNumber(extendedPath.spaceId);
      assert.notEqual(extendedPath.spaceId, rootPath.spaceId);
      assert.deepEqual(extendedPath.bestExample, {text: 't', p: 0.5});
      assert.deepEqual(extendedPath.parents, [rootPath]);
      assert.deepEqual(extendedPath.inputs, leadEdgeDistribution);
      assert.equal(extendedPath.likeliestSourceText, 't');
      assert.deepEqual(extendedPath.sourceIdentifiers, [
        {
        trueTransform: leadEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: leadEdgeDistribution[0].p
        }
      ]);

      // Assert the root is unchanged.
      assert.equal(rootPath.inputCount, 0);
      // Should (still) have codepointLength == 0 once it's defined.
      assert.deepEqual(rootPath.bestExample, {text: '', p: 1});
      assert.deepEqual(rootPath.parents, []);
      assert.isNotOk(rootPath.inputs);
    });

    it('may be built from arbitrary prior SearchPath', () => {
      const rootPath = new SearchPath(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];
      const inputClone = leadEdgeDistribution.map(e => ({...e}));

      const length1Path = new SearchPath(
        rootPath,
        leadEdgeDistribution,
        leadEdgeDistribution[0]
      );

      const tailEdgeDistribution = [
        {sample: {insert: 'r', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'e', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'h', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new SearchPath(
        length1Path,
        tailEdgeDistribution,
        tailEdgeDistribution[0]
      );

      // Verify that the prior distribution remains fully unaltered.
      assert.deepEqual(leadEdgeDistribution, inputClone);

      assert.equal(length2Path.inputCount, 2);
      assert.equal(length2Path.codepointLength, 2);
      assert.isNumber(length2Path.spaceId);
      assert.notEqual(length2Path.spaceId, length1Path.spaceId);
      assert.deepEqual(length2Path.bestExample, {text: 'tr', p: leadEdgeDistribution[0].p * tailEdgeDistribution[0].p});
      assert.deepEqual(length2Path.parents, [length1Path]);
      assert.deepEqual(length2Path.inputs, tailEdgeDistribution);
      assert.equal(length2Path.likeliestSourceText, 'tr');
      assert.deepEqual(length2Path.sourceIdentifiers, [
        {
        trueTransform: leadEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: leadEdgeDistribution[0].p
        }, {
          trueTransform: tailEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: tailEdgeDistribution[0].p
        }
      ]);

      assert.equal(length1Path.inputCount, 1);
      assert.equal(length1Path.codepointLength, 1);
      assert.isNumber(length1Path.spaceId);
      assert.notEqual(length1Path.spaceId, rootPath.spaceId);
      assert.deepEqual(length1Path.bestExample, {text: 't', p: 0.5});
      assert.deepEqual(length1Path.parents, [rootPath]);
      assert.deepEqual(length1Path.inputs, leadEdgeDistribution);
    });

    it('may extend with a Transform inserting multiple codepoints', () => {
      const rootPath = new SearchPath(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];
      const inputClone = leadEdgeDistribution.map(e => ({...e}));

      const length1Path = new SearchPath(
        rootPath,
        leadEdgeDistribution,
        leadEdgeDistribution[0]
      );

      const tailEdgeDistribution = [
        {sample: {insert: 'ri', deleteLeft: 0, id: 17 }, p: 0.6},
        {sample: {insert: 'er', deleteLeft: 0, id: 17 }, p: 0.25},
        {sample: {insert: 'hi', deleteLeft: 0, id: 17 }, p: 0.15}
      ];

      const length2Path = new SearchPath(
        length1Path,
        tailEdgeDistribution,
        tailEdgeDistribution[0]
      );

      // Verify that the prior distribution remains fully unaltered.
      assert.deepEqual(leadEdgeDistribution, inputClone);

      assert.equal(length2Path.inputCount, 2);
      assert.equal(length2Path.codepointLength, 3);
      assert.isNumber(length2Path.spaceId);
      assert.notEqual(length2Path.spaceId, length1Path.spaceId);
      assert.deepEqual(length2Path.bestExample, {text: 'tri', p: leadEdgeDistribution[0].p * tailEdgeDistribution[0].p});
      assert.deepEqual(length2Path.parents, [length1Path]);
      assert.deepEqual(length2Path.inputs, tailEdgeDistribution);
      assert.equal(length2Path.likeliestSourceText, 'tri');
      assert.deepEqual(length2Path.sourceIdentifiers, [
        {
        trueTransform: leadEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: leadEdgeDistribution[0].p
        }, {
          trueTransform: tailEdgeDistribution[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: tailEdgeDistribution[0].p
        }
      ]);

      assert.equal(length1Path.inputCount, 1);
      assert.equal(length1Path.codepointLength, 1);
      assert.isNumber(length1Path.spaceId);
      assert.notEqual(length1Path.spaceId, rootPath.spaceId);
      assert.deepEqual(length1Path.bestExample, {text: 't', p: 0.5});
      assert.deepEqual(length1Path.parents, [rootPath]);
      assert.deepEqual(length1Path.inputs, leadEdgeDistribution);
    });

    describe('split()', () => {
      describe(`on token comprised of single-char transforms:  [crt][ae][nr][t]`, () => {
        const buildPath = () => {
          let path = new SearchPath(testModel);

          const distrib1 = [
            { sample: {insert: 'c', deleteLeft: 0, id: 11}, p: 0.5 },
            { sample: {insert: 'r', deleteLeft: 0, id: 11}, p: 0.4 },
            { sample: {insert: 't', deleteLeft: 0, id: 11}, p: 0.1 }
          ];
          path = new SearchPath(path, distrib1, distrib1[0]);

          const distrib2 = [
            { sample: {insert: 'a', deleteLeft: 0, id: 12}, p: 0.7 },
            { sample: {insert: 'e', deleteLeft: 0, id: 12}, p: 0.3 }
          ];
          path = new SearchPath(path, distrib2, distrib2[0]);

          const distrib3 = [
            { sample: {insert: 'n', deleteLeft: 0, id: 13}, p: 0.8 },
            { sample: {insert: 'r', deleteLeft: 0, id: 13}, p: 0.2 }
          ];
          path = new SearchPath(path, distrib3, distrib3[0]);

          const distrib4 = [
            { sample: {insert: 't', deleteLeft: 0, id: 14}, p: 1 }
          ];
          path = new SearchPath(path, distrib4, distrib4[0]);

          return {
            path,
            distributions: [distrib1, distrib2, distrib3, distrib4]
          };
        }

        const runSplit = (splitIndex: number) => {
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(splitIndex);

          assert.equal(head.inputCount, splitIndex);
          assert.equal(tail.inputCount, pathToSplit.inputCount - splitIndex);
          // The split operation will still reconstruct the token; the head
          // is always built from the same root path, while the tail is not.
          const headDistrib = distributions.slice(0, splitIndex);
          const tailDistrib = distributions.slice(splitIndex);
          assert.isTrue(head.hasInputs(headDistrib));
          assert.isTrue(tail.hasInputs(tailDistrib));

          assert.deepEqual(head.bestExample, headDistrib.reduce((accum, curr) => {
            return {
              text: accum.text + curr[0].sample.insert,
              p: accum.p * curr[0].p
            }
          }, { text: '', p: 1 }));
          assert.deepEqual(tail.bestExample, tailDistrib.reduce((accum, curr) => {
            return {
              text: accum.text + curr[0].sample.insert,
              p: accum.p * curr[0].p
            }
          }, { text: '', p: 1 }));
        }

        it('setup: constructs path properly', () => {
          const { path: pathToSplit, distributions } = buildPath();

          assert.equal(pathToSplit.inputCount, 4);
          assert.equal(distributions.length, pathToSplit.inputCount);
          assert.equal(pathToSplit.codepointLength, 4); // one char per input, no deletions anywhere
          // Per assertions documented in the setup above.
          assert.deepEqual(pathToSplit.bestExample, distributions.reduce(
            (constructing, current) => ({text: constructing.text + current[0].sample.insert, p: constructing.p * current[0].p}),
            {text: '', p: 1})
          );
          assert.deepEqual(pathToSplit.parents[0].bestExample, distributions.slice(0, pathToSplit.inputCount-1).reduce(
            (constructing, current) => ({text: constructing.text + current[0].sample.insert, p: constructing.p * current[0].p}),
            {text: '', p: 1})
          );
          assert.isTrue(pathToSplit.hasInputs(distributions));
          assert.equal(pathToSplit.constituentPaths.length, 1);
        });

        it('splits properly at index 0', () => {
          runSplit(0);

          const { path: pathToSplit, distributions } = buildPath();
          const [head, tail] = pathToSplit.split(0);

          // The split operation will still reconstruct the token; the head
          // is always built from the same root path, while the tail is not.
          assert.notEqual(tail, pathToSplit);
          assert.deepEqual(head.parents, []);
          assert.equal(head, pathToSplit.constituentPaths[0][0]);

          assert.isTrue(head.hasInputs([]));
          assert.isTrue(tail.hasInputs(distributions));
        });

        it('splits properly at index 1', () => {
          runSplit(1);

          const { path: pathToSplit } = buildPath();
          const [head] = pathToSplit.split(1);

          assert.equal(head, pathToSplit.parents[0].parents[0].parents[0]);
        });

        it('splits properly at index 2', () => {
          runSplit(2);

          const { path: pathToSplit } = buildPath();
          const [head] = pathToSplit.split(2);

          assert.equal(head, pathToSplit.parents[0].parents[0]);
        });

        it('splits properly at index 3', () => {
          runSplit(3);

          const { path: pathToSplit } = buildPath();
          const [head] = pathToSplit.split(3);

          assert.equal(head, pathToSplit.parents[0]);
        });

        it('splits properly at index 4', () => {
          runSplit(4);

          const { path: pathToSplit } = buildPath();
          const [head] = pathToSplit.split(4);

          assert.equal(head, pathToSplit);
        });
      });

      describe(`on token comprised of two-char transforms:  ca nt el ou pe`, () => {
        const buildPath = () => {
          let path = new SearchPath(testModel);

          const distrib1 = [
            { sample: {insert: 'ca', deleteLeft: 0, id: 11}, p: 1 }
          ];
          path = new SearchPath(path, distrib1, distrib1[0]);

          const distrib2 = [
            { sample: {insert: 'nt', deleteLeft: 0, id: 12}, p: 1 }
          ];
          path = new SearchPath(path, distrib2, distrib2[0]);

          const distrib3 = [
            { sample: {insert: 'el', deleteLeft: 0, id: 13}, p: 1 }
          ];
          path = new SearchPath(path, distrib3, distrib3[0]);

          const distrib4 = [
            { sample: {insert: 'ou', deleteLeft: 0, id: 14}, p: 1 }
          ];
          path = new SearchPath(path, distrib4, distrib4[0]);

          const distrib5 = [
            { sample: {insert: 'pe', deleteLeft: 0, id: 15}, p: 1 }
          ];
          path = new SearchPath(path, distrib5, distrib5[0]);

          return {
            path,
            distributions: [distrib1, distrib2, distrib3, distrib4, distrib5]
          };
        }

        const runSplit = (splitIndex: number) => {
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(splitIndex);

          assert.equal(head.inputCount, Math.ceil(splitIndex/2));
          assert.equal(tail.inputCount, Math.ceil(pathToSplit.inputCount - splitIndex/2));
          // The split operation will still reconstruct the token; the head
          // is always built from the same root path, while the tail is not.
          const hasSplit = splitIndex % 2 == 1;
          const headDistribs = distributions.slice(0, Math.floor(splitIndex/2));
          const tailDistribs = distributions.slice(Math.floor((splitIndex+1)/2));

          if(hasSplit) {
            const splitDistrib = distributions[Math.floor(splitIndex/2)];
            const distribHalves = splitDistrib.map((entry) => {
              return {
                head: { sample: {...entry.sample, insert: entry.sample.insert.slice(0, 1)}, p: entry.p },
                tail: { sample: {...entry.sample, insert: entry.sample.insert.slice(1), deleteLeft: 0}, p: entry.p }
              }
            });

            headDistribs.push(distribHalves.map(p => p.head));
            tailDistribs.unshift(distribHalves.map(p => p.tail));
          }

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, headDistribs.reduce((accum, curr) => {
            return {
              text: accum.text + curr[0].sample.insert,
              p: accum.p * curr[0].p
            }
          }, { text: '', p: 1 }));
          assert.deepEqual(tail.bestExample, tailDistribs.reduce((accum, curr) => {
            return {
              text: accum.text + curr[0].sample.insert,
              p: accum.p * curr[0].p
            }
          }, { text: '', p: 1 }));
        }

        it('setup: constructs path properly', () => {
          const { path: pathToSplit, distributions } = buildPath();

          assert.equal(pathToSplit.inputCount, 5);
          assert.equal(distributions.length, pathToSplit.inputCount);
          assert.equal(pathToSplit.codepointLength, 10); // one char per input, no deletions anywhere
          // Per assertions documented in the setup above.
          assert.deepEqual(pathToSplit.bestExample, distributions.reduce(
            (constructing, current) => ({text: constructing.text + current[0].sample.insert, p: constructing.p * current[0].p}),
            {text: '', p: 1})
          );
          assert.deepEqual(pathToSplit.parents[0].bestExample, distributions.slice(0, pathToSplit.inputCount-1).reduce(
            (constructing, current) => ({text: constructing.text + current[0].sample.insert, p: constructing.p * current[0].p}),
            {text: '', p: 1})
          );
          assert.isTrue(pathToSplit.hasInputs(distributions));
          assert.equal(pathToSplit.constituentPaths.length, 1);
        });

        it('splits properly at index 0', () => {
          runSplit(0);

          const { path: pathToSplit, distributions } = buildPath();
          const [head, tail] = pathToSplit.split(0);

          // The split operation will still reconstruct the token; the head
          // is always built from the same root path, while the tail is not.
          assert.notEqual(tail, pathToSplit);
          assert.deepEqual(head.parents, []);
          assert.equal(head, pathToSplit.constituentPaths[0][0]);

          assert.isTrue(head.hasInputs([]));
          assert.isTrue(tail.hasInputs(distributions));
        });

        it('splits properly at index 1', () => {
          runSplit(1);
        });

        it('splits properly at index 2', () => {
          runSplit(2);
        });

        it('splits properly at index 3', () => {
          runSplit(3);
        });

        it('splits properly at index 4', () => {
          runSplit(4);
        });

        it('splits properly at index 5', () => {
          runSplit(5);
        });

        it('splits properly at index 6', () => {
          runSplit(6);
        });

        it('splits properly at index 7', () => {
          runSplit(7);
        });

        it('splits properly at index 8', () => {
          runSplit(8);

          const { path: pathToSplit } = buildPath();
          const [head] = pathToSplit.split(8);

          assert.equal(head, pathToSplit.parents[0]);
        });

        it('splits properly at index 9', () => {
          runSplit(9);

          const { path: pathToSplit } = buildPath();
          const [head] = pathToSplit.split(9);

          // Same parent, but not the same final step - it _was_ split, after
          // all.
          assert.equal(head.parents[0], pathToSplit.parents[0]);
        });

        it('splits properly at index 10', () => {
          runSplit(10);

          const { path: pathToSplit } = buildPath();
          const [head] = pathToSplit.split(10);

          assert.equal(head, pathToSplit);
        });
      });

      describe(`on token comprised of complex, rewriting transforms:  cello`, () => {
        const buildPath = () => {
          let path = new SearchPath(testModel);

          const distrib1 = [
            { sample: {insert: 'ca', deleteLeft: 0, id: 11}, p: 1 }
          ];
          path = new SearchPath(path, distrib1, distrib1[0]);

          const distrib2 = [
            { sample: {insert: 'ent', deleteLeft: 1, id: 12}, p: 1 }
          ];
          path = new SearchPath(path, distrib2, distrib2[0]);

          const distrib3 = [
            { sample: {insert: 'llar', deleteLeft: 2, id: 13}, p: 1 }
          ];
          path = new SearchPath(path, distrib3, distrib3[0]);

          const distrib4 = [
            { sample: {insert: 'o', deleteLeft: 2, id: 14}, p: 1 }
          ];
          path = new SearchPath(path, distrib4, distrib4[0]);

          return {
            path,
            distributions: [distrib1, distrib2, distrib3, distrib4]
          };
        }

        it('setup: constructs path properly', () => {
          const { path: pathToSplit, distributions } = buildPath();

          assert.equal(pathToSplit.inputCount, 4);
          assert.equal(distributions.length, pathToSplit.inputCount);
          assert.equal(pathToSplit.codepointLength, 5); // one char per input, no deletions anywhere
          // Per assertions documented in the setup above.
          assert.deepEqual(pathToSplit.bestExample, {
            text: "cello",
            p: distributions.slice(0, pathToSplit.inputCount-1).reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(pathToSplit.parents[0].bestExample, {
            text: "cellar",
            p: distributions.slice(0, pathToSplit.inputCount-1).reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.isTrue(pathToSplit.hasInputs(distributions));
          assert.equal(pathToSplit.constituentPaths.length, 1);
        });

        it('splits properly at index 0', () => {
          const splitIndex = 0;
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(splitIndex);

          assert.equal(head.inputCount, 0);
          assert.equal(tail.inputCount, 4);
          // is always built from the same root path, while the tail is not.
          const headDistribs = distributions.slice(0, 0);
          const tailDistribs = distributions.slice(0);

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: "",
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: "cello",
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });

          // The split operation will still reconstruct the token; the head
          // is always built from the same root path, while the tail is not.
          assert.notEqual(tail, pathToSplit);
          assert.deepEqual(head.parents, []);
          assert.equal(head, pathToSplit.constituentPaths[0][0]);

          assert.isTrue(head.hasInputs([]));
          assert.isTrue(tail.hasInputs(distributions));
        });

        it('splits properly at index 1', () => {
          const splitIndex = 1;
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(splitIndex);

          // ca => ce in the second input - there's a deleteLeft!
          assert.equal(head.inputCount, 2);
          assert.equal(tail.inputCount, 3); // split transform!
          // is always built from the same root path, while the tail is not.

          const headDistribs = distributions.slice(0, 1);
          const tailDistribs = distributions.slice(2);

          const splitDistrib = distributions[1];
          const distribHalves = splitDistrib.map((entry) => {
            return {
              head: { sample: {...entry.sample, insert: entry.sample.insert.slice(0, 0)}, p: entry.p },
              tail: { sample: {...entry.sample, insert: entry.sample.insert.slice(0), deleteLeft: 0}, p: entry.p }
            }
          });

          headDistribs.push(distribHalves.map(p => p.head));
          tailDistribs.unshift(distribHalves.map(p => p.tail));

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: "c",
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: "ello",
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
        });

        it('splits properly at index 2', () => {
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(2);

          // cen => cel in the third input - there's a deleteLeft!
          assert.equal(head.inputCount, 3);
          assert.equal(tail.inputCount, 2); // split transform!
          // is always built from the same root path, while the tail is not.

          const headDistribs = distributions.slice(0, 2);
          const tailDistribs = distributions.slice(3);

          const splitDistrib = distributions[2];
          const distribHalves = splitDistrib.map((entry) => {
            return {
              head: { sample: {...entry.sample, insert: entry.sample.insert.slice(0, 0)}, p: entry.p },
              tail: { sample: {...entry.sample, insert: entry.sample.insert.slice(0), deleteLeft: 0}, p: entry.p }
            }
          });

          headDistribs.push(distribHalves.map(p => p.head));
          tailDistribs.unshift(distribHalves.map(p => p.tail));

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: "ce",
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: "llo",
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
        });

        it('splits properly at index 3', () => {
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(3);

          // cen => cel in the third input, and there's no adjacent deleteLeft.
          assert.equal(head.inputCount, 3);
          assert.equal(tail.inputCount, 2); // split transform!
          // is always built from the same root path, while the tail is not.

          const headDistribs = distributions.slice(0, 2);
          const tailDistribs = distributions.slice(3);

          const splitDistrib = distributions[2];
          const distribHalves = splitDistrib.map((entry) => {
            return {
              head: { sample: {...entry.sample, insert: entry.sample.insert.slice(0, 1)}, p: entry.p },
              tail: { sample: {...entry.sample, insert: entry.sample.insert.slice(1), deleteLeft: 0}, p: entry.p }
            }
          });

          headDistribs.push(distribHalves.map(p => p.head));
          tailDistribs.unshift(distribHalves.map(p => p.tail));

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: "cel",
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: "lo",
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });

          assert.deepEqual(tail.parents[0].bestExample, {
            text: "lar",
            p: tailDistribs[0][0].p
          })
        });

        it('splits properly at index 4', () => {
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(4);

          // cella => cello in the fourth input; there's an adjacent deleteLeft.
          assert.equal(head.inputCount, 4);
          assert.equal(tail.inputCount, 1); // split transform!

          const headDistribs = distributions.slice(0, 3);
          const tailDistribs = distributions.slice(4);

          const splitDistrib = distributions[3];
          const distribHalves = splitDistrib.map((entry) => {
            return {
              head: { sample: {...entry.sample, insert: entry.sample.insert.slice(0, 0)}, p: entry.p },
              tail: { sample: {...entry.sample, insert: entry.sample.insert.slice(0), deleteLeft: 0}, p: entry.p }
            }
          });

          headDistribs.push(distribHalves.map(p => p.head));
          tailDistribs.unshift(distribHalves.map(p => p.tail));

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: "cell",
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: "o",
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });

          assert.deepEqual(head.parents[0].bestExample.text, "cellar");
        });
      });

      describe(`on token comprised of single titanic transform: biglargetransform`, () => {
        const buildPath = () => {
          let path = new SearchPath(testModel);

          const distrib1 = [
            { sample: {insert: 'biglargetransform', deleteLeft: 0, id: 11}, p: 1 }
          ];
          path = new SearchPath(path, distrib1, distrib1[0]);

          return {
            path,
            distributions: [distrib1]
          };
        }

        it('setup: constructs path properly', () => {
          const { path: pathToSplit, distributions } = buildPath();

          assert.equal(pathToSplit.inputCount, 1);
          assert.equal(distributions.length, pathToSplit.inputCount);
          assert.equal(pathToSplit.codepointLength, 'biglargetransform'.length); // one char per input, no deletions anywhere
          // Per assertions documented in the setup above.
          assert.deepEqual(pathToSplit.bestExample, {
            text: "biglargetransform",
            p: distributions[0][0].p
          });
          assert.equal(pathToSplit.parents[0].inputCount, 0);
          assert.isTrue(pathToSplit.hasInputs(distributions));
          assert.equal(pathToSplit.constituentPaths.length, 1);
        });

        it('splits properly after \'big\'', () => {
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(3);

          assert.equal(head.inputCount, 1);
          assert.equal(tail.inputCount, 1);

          const headDistribs = distributions.slice(0, 0);
          const tailDistribs = distributions.slice(1);

          const splitDistrib = distributions[0];
          const distribHalves = splitDistrib.map((entry) => {
            return {
              head: { sample: {...entry.sample, insert: entry.sample.insert.slice(0, 3)}, p: entry.p },
              tail: { sample: {...entry.sample, insert: entry.sample.insert.slice(3), deleteLeft: 0}, p: entry.p }
            }
          });

          headDistribs.push(distribHalves.map(p => p.head));
          tailDistribs.unshift(distribHalves.map(p => p.tail));

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: "big",
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: "largetransform",
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
        });

        it('splits properly after \'biglarge\'', () => {
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(8);

          assert.equal(head.inputCount, 1);
          assert.equal(tail.inputCount, 1);

          const headDistribs = distributions.slice(0, 0);
          const tailDistribs = distributions.slice(1);

          const splitDistrib = distributions[0];
          const distribHalves = splitDistrib.map((entry) => {
            return {
              head: { sample: {...entry.sample, insert: entry.sample.insert.slice(0, 8)}, p: entry.p },
              tail: { sample: {...entry.sample, insert: entry.sample.insert.slice(8), deleteLeft: 0}, p: entry.p }
            }
          });

          headDistribs.push(distribHalves.map(p => p.head));
          tailDistribs.unshift(distribHalves.map(p => p.tail));

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: "biglarge",
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: "transform",
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
        });
      });

      describe('correctly handles token with complex non-BMP transforms', () => {
        const buildPath = () => {
          let path = new SearchPath(testModel);

          const distrib1 = [
            { sample: {insert: toMathematicalSMP('ca'), deleteLeft: 0, id: 11}, p: 1 }
          ];
          path = new SearchPath(path, distrib1, distrib1[0]);

          const distrib2 = [
            { sample: {insert: toMathematicalSMP('ent'), deleteLeft: 1, id: 12}, p: 1 }
          ];
          path = new SearchPath(path, distrib2, distrib2[0]);

          const distrib3 = [
            { sample: {insert: toMathematicalSMP('llar'), deleteLeft: 2, id: 13}, p: 1 }
          ];
          path = new SearchPath(path, distrib3, distrib3[0]);

          const distrib4 = [
            { sample: {insert: toMathematicalSMP('o'), deleteLeft: 2, id: 14}, p: 1 }
          ];
          path = new SearchPath(path, distrib4, distrib4[0]);

          return {
            path,
            distributions: [distrib1, distrib2, distrib3, distrib4]
          };
        }

        it('setup: constructs path properly', () => {
          const { path: pathToSplit, distributions } = buildPath();

          // Validate that an SMP-conversion has occurred.
          assert.notEqual(toMathematicalSMP("cello"), "cello");
          assert.equal(toMathematicalSMP("cello").length, "cello".length * 2);
          assert.equal(KMWString.length(toMathematicalSMP("cello")), KMWString.length("cello"));

          assert.equal(pathToSplit.inputCount, 4);
          assert.equal(distributions.length, pathToSplit.inputCount);
          assert.equal(pathToSplit.codepointLength, 5); // one char per input, no deletions anywhere
          // Per assertions documented in the setup above.
          assert.deepEqual(pathToSplit.bestExample, {
            text: toMathematicalSMP("cello"),
            p: distributions.slice(0, pathToSplit.inputCount-1).reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(pathToSplit.parents[0].bestExample, {
            text: toMathematicalSMP("cellar"),
            p: distributions.slice(0, pathToSplit.inputCount-1).reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.isTrue(pathToSplit.hasInputs(distributions));
          assert.equal(pathToSplit.constituentPaths.length, 1);
        });

        it('splits properly at index 0', () => {
          const splitIndex = 0;
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(splitIndex);

          assert.equal(head.inputCount, 0);
          assert.equal(tail.inputCount, 4);
          // is always built from the same root path, while the tail is not.
          const headDistribs = distributions.slice(0, 0);
          const tailDistribs = distributions.slice(0);

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: "",
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: toMathematicalSMP("cello"),
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });

          // The split operation will still reconstruct the token; the head
          // is always built from the same root path, while the tail is not.
          assert.notEqual(tail, pathToSplit);
          assert.deepEqual(head.parents, []);
          assert.equal(head, pathToSplit.constituentPaths[0][0]);

          assert.isTrue(head.hasInputs([]));
          assert.isTrue(tail.hasInputs(distributions));
        });

        it('splits properly at index 1', () => {
          const splitIndex = 1;
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(splitIndex);

          // ca => ce in the second input - there's a deleteLeft!
          assert.equal(head.inputCount, 2);
          assert.equal(tail.inputCount, 3); // split transform!
          // is always built from the same root path, while the tail is not.

          const headDistribs = distributions.slice(0, 1);
          const tailDistribs = distributions.slice(2);

          const splitDistrib = distributions[1];
          const distribHalves = splitDistrib.map((entry) => {
            return {
              head: { sample: {...entry.sample, insert: entry.sample.insert.slice(0, 0)}, p: entry.p },
              tail: { sample: {...entry.sample, insert: entry.sample.insert.slice(0), deleteLeft: 0}, p: entry.p }
            }
          });

          headDistribs.push(distribHalves.map(p => p.head));
          tailDistribs.unshift(distribHalves.map(p => p.tail));

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: toMathematicalSMP("c"),
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: toMathematicalSMP("ello"),
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
        });

        it('splits properly at index 2', () => {
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(2);

          // cen => cel in the third input - there's a deleteLeft!
          assert.equal(head.inputCount, 3);
          assert.equal(tail.inputCount, 2); // split transform!
          // is always built from the same root path, while the tail is not.

          const headDistribs = distributions.slice(0, 2);
          const tailDistribs = distributions.slice(3);

          const splitDistrib = distributions[2];
          const distribHalves = splitDistrib.map((entry) => {
            return {
              head: { sample: {...entry.sample, insert: entry.sample.insert.slice(0, 0)}, p: entry.p },
              tail: { sample: {...entry.sample, insert: entry.sample.insert.slice(0), deleteLeft: 0}, p: entry.p }
            }
          });

          headDistribs.push(distribHalves.map(p => p.head));
          tailDistribs.unshift(distribHalves.map(p => p.tail));

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: toMathematicalSMP("ce"),
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: toMathematicalSMP("llo"),
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
        });

        it('splits properly at index 3', () => {
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(3);

          // cen => cel in the third input, and there's no adjacent deleteLeft.
          assert.equal(head.inputCount, 3);
          assert.equal(tail.inputCount, 2); // split transform!
          // is always built from the same root path, while the tail is not.

          const headDistribs = distributions.slice(0, 2);
          const tailDistribs = distributions.slice(3);

          const splitDistrib = distributions[2];
          const distribHalves = splitDistrib.map((entry) => {
            return {
              // KMWString.slice is bugged; substring works correctly.
              head: { sample: {...entry.sample, insert: KMWString.substring(entry.sample.insert, 0, 1)}, p: entry.p },
              tail: { sample: {...entry.sample, insert: KMWString.substring(entry.sample.insert, 1), deleteLeft: 0}, p: entry.p }
            }
          });

          headDistribs.push(distribHalves.map(p => p.head));
          tailDistribs.unshift(distribHalves.map(p => p.tail));

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: toMathematicalSMP("cel"),
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: toMathematicalSMP("lo"),
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });

          assert.deepEqual(tail.parents[0].bestExample, {
            text: toMathematicalSMP("lar"),
            p: tailDistribs[0][0].p
          })
        });

        it('splits properly at index 4', () => {
          const { path: pathToSplit, distributions } = buildPath();

          const [head, tail] = pathToSplit.split(4);

          // cella => cello in the fourth input; there's an adjacent deleteLeft.
          assert.equal(head.inputCount, 4);
          assert.equal(tail.inputCount, 1); // split transform!

          const headDistribs = distributions.slice(0, 3);
          const tailDistribs = distributions.slice(4);

          const splitDistrib = distributions[3];
          const distribHalves = splitDistrib.map((entry) => {
            return {
              head: { sample: {...entry.sample, insert: entry.sample.insert.slice(0, 0)}, p: entry.p },
              tail: { sample: {...entry.sample, insert: entry.sample.insert.slice(0), deleteLeft: 0}, p: entry.p }
            }
          });

          headDistribs.push(distribHalves.map(p => p.head));
          tailDistribs.unshift(distribHalves.map(p => p.tail));

          assert.isTrue(head.hasInputs(headDistribs));
          assert.isTrue(tail.hasInputs(tailDistribs));

          assert.deepEqual(head.bestExample, {
            text: toMathematicalSMP("cell"),
            p: headDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });
          assert.deepEqual(tail.bestExample, {
            text: toMathematicalSMP("o"),
            p: tailDistribs.reduce(
              (likelihood, current) => likelihood * current[0].p,
              1
            )
          });

          assert.deepEqual(head.parents[0].bestExample.text, toMathematicalSMP("cellar"));
        });
      });
    });
  });
});