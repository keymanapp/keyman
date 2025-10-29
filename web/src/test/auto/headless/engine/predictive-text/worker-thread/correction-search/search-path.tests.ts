/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-29
 *
 * This file defines tests for the SearchSpace class of the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { KMWString } from '@keymanapp/web-utils';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { models, SearchPath, TokenInputSource } from '@keymanapp/lm-worker/test-index';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));


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

    it('correctly splits mid-input when necessary', () => {
      let path = new SearchPath(testModel);
      const startSample = {sample: { insert: 'a', deleteLeft: 0 }, p: 1}
      path = new SearchPath(path, [startSample], startSample);

      const inputDistribution = [
        {sample: { insert: 'four', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.4},
        {sample: { insert: 'then', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.3},
        {sample: { insert: 'nine', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.2},
        {sample: { insert: 'what', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.06},
        {sample: { insert: 'cent', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.04}
      ];

      const pathToSplit = new SearchPath(path, inputDistribution, inputDistribution[0]);
      assert.equal(pathToSplit.codepointLength, 4);
      assert.equal(pathToSplit.inputCount, 2);

      // This test models a previous split at codepoint index 2, splitting
      // the input distribution accordingly.  (Note:  deleteLeft = 1!)
      const headDistributionSplit = [
        {sample: { insert: 'fo', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.4},
        {sample: { insert: 'th', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.3},
        {sample: { insert: 'ni', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.2},
        {sample: { insert: 'wh', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.06},
        {sample: { insert: 'ce', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.04}
      ];
      const headTarget = new SearchPath(
        path, headDistributionSplit, inputDistribution[0]
      );

      const tailDistributionSplit = [
        {sample: { insert: 'ur', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.4},
        {sample: { insert: 'en', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.3},
        {sample: { insert: 'ne', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.2},
        {sample: { insert: 'at', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.06},
        {sample: { insert: 'nt', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.04}
      ];
      const tailTarget = new SearchPath(
        new SearchPath(testModel), tailDistributionSplit, {
          trueTransform: inputDistribution[0].sample,
          bestProbFromSet: inputDistribution[0].p,
          inputStartIndex: 2
        }
      );

      const split = pathToSplit.split(2);

      assert.deepEqual(split[0].bestExample, headTarget.bestExample);
      assert.deepEqual(split[1].bestExample, tailTarget.bestExample);
      assert.equal(split[0].inputCount, headTarget.inputCount);
      assert.equal(split[1].inputCount, tailTarget.inputCount);
      assert.isTrue(split[0] instanceof SearchPath);
      assert.isTrue(split[1] instanceof SearchPath);
      assert.deepEqual((split[0] as SearchPath).inputs, headTarget.inputs);
      assert.deepEqual((split[1] as SearchPath).inputs, tailTarget.inputs);
      assert.deepEqual((split[0] as SearchPath).inputSource, headTarget.inputSource);
      assert.deepEqual((split[1] as SearchPath).inputSource, tailTarget.inputSource);
    });
  });

  // Placed after `split()` because many cases mock a reversal of split-test results.
  describe('merge()', () => {
    /*
     * To define:
     * - merging a standard case
     * - merging a split BMP case
     * - merging a standard SMP case
     * - merging a split SMP case
     * - merging a case where the deleteLeft was split from the insert
     *   - splitIndex = 0, but the deleteLeft is (conceptually) before that.
     * - this (empty) + param (full)
     * - this (full) + param (empty)
     * - merging with distributions (no split)
     * - merging with distributions (and a definite split)
     *
     * - biglargetransform for single-input multi-split remerge
     *   - merging a three-way split should be associative (not dependent on order) so
     *     long as the relative positions are correct
     *
     * - "cello" case(s) covers...
     *   - deleteLeft split from insert
     *   - a straight-up split (mid-insert)
     *   - standard case (no distrib)
     *   - with head + tail index inclusion, the empty + full versions
     *   - SMP variant:  the SMP cases.
     *
     * - then we may need a "merging with distributions" coverage
     *   - can prob make a simple BMP mocked version...
     *   - and a simple SMP mocked version
     *   - is actually pretty-much covered anyway... I believe.
     */

    // Covers cases where a single "input" was split into more than two fragments
    describe(`previously-split token comprised of single titanic transform: biglargetransform`, () => {
      const buildPath = () => {
        const distributions = [
          [{ sample: {insert: 'big', deleteLeft: 0, id: 11}, p: 1 }],
          [{ sample: {insert: 'large', deleteLeft: 0, id: 11}, p: 1 }],
          [{ sample: {insert: 'transform', deleteLeft: 0, id: 11}, p: 1 }]
        ];

        const originalInputBase: TokenInputSource = {
          trueTransform: {insert: 'biglargetransform', deleteLeft: 0, id: 11},
          inputStartIndex: 0,
          bestProbFromSet: 1
        };

        const originalInputs = [0, 3, 8].map(n => ({...originalInputBase, inputStartIndex: n}));

        const paths = distributions.map((d, i) => new SearchPath(new SearchPath(testModel), d, originalInputs[i]));

        return {
          paths,
          distributions,
          originalInputs
        };
      }

      const checkFinalStateAssertions = (merged: SearchPath, originalInput: TokenInputSource) => {
        assert.equal(merged.inputCount, 1);
        assert.isTrue(merged instanceof SearchPath);
        assert.deepEqual(merged.bestExample.text, "biglargetransform");
        assert.deepEqual((merged as SearchPath).inputs, [
          { sample: { insert: 'biglargetransform', deleteLeft: 0, id: 11 }, p: 1 }
        ]);
        assert.deepEqual((merged as SearchPath).inputSource, originalInput);
        // TODO:  check the 'source' input data (here and in callers)
      }

      it('setup: constructs paths properly', () => {
        const { paths, distributions, originalInputs } = buildPath();

        assert.equal(paths.length, 3);
        assert.equal(distributions.length, paths.length);
        paths.forEach((p, i) => {
          assert.equal(p.inputCount, 1);
          assert.equal(distributions[i].length, p.inputCount);
          assert.equal(p.codepointLength, KMWString.length(distributions[i][0].sample.insert));
          assert.deepEqual(p.bestExample, {
            text: ['big', 'large', 'transform'][i],
            p: 1
          });
          assert.equal(p.parents[0].inputCount, 0);
          assert.isTrue(p.hasInputs([distributions[i]]));
        });

        originalInputs.forEach((original) => {
          assert.deepEqual({...original, inputStartIndex: 0}, {...originalInputs[0], inputStartIndex: 0});
        });
      });

      it('merging order:  big + large, then + transform', () => {
        const { paths, originalInputs } = buildPath();

        const headMerge = paths[0].merge(paths[1]);

        // Assertions
        assert.equal(headMerge.inputCount, 1);
        assert.isTrue(headMerge instanceof SearchPath);
        assert.deepEqual(headMerge.bestExample.text, "biglarge");
        assert.deepEqual((headMerge as SearchPath).inputs, [
          { sample: { insert: 'biglarge', deleteLeft: 0, id: 11 }, p: 1 }
        ]);
        assert.deepEqual((headMerge as SearchPath).inputSource, originalInputs[0]);

        const fullMerge = headMerge.merge(paths[2]);
        checkFinalStateAssertions(fullMerge as SearchPath, originalInputs[0]);
      });

      it('merging order:  large + transform, then + big', () => {
        const { paths, originalInputs } = buildPath();

        const tailMerge = paths[1].merge(paths[2]);

        // Assertions
        assert.equal(tailMerge.inputCount, 1);
        assert.isTrue(tailMerge instanceof SearchPath);
        assert.deepEqual(tailMerge.bestExample.text, "largetransform");
        assert.deepEqual((tailMerge as SearchPath).inputs, [
          { sample: { insert: 'largetransform', deleteLeft: 0, id: 11 }, p: 1 }
        ]);
        assert.deepEqual((tailMerge as SearchPath).inputSource, originalInputs[1]);

        const fullMerge = paths[0].merge(tailMerge);
        checkFinalStateAssertions(fullMerge as SearchPath, originalInputs[0]);
      });
    });

    // Covers many common aspects of SearchPath merging, though not merging of
    // multi-member distributions.
    describe(`previously-split token comprised of complex, rewriting transforms:  cello`, () => {
      const buildPath = (inputs: Distribution<Transform>[], sources: TokenInputSource[], root?: SearchPath) => {
        return inputs.reduce((path, input, index) => new SearchPath(path, input, sources[index]), root ?? new SearchPath(testModel));
      }

      const buildFixtures = () => {
        const trueDistributions = [
          [
            { sample: {insert: 'ca', deleteLeft: 0, id: 11}, p: 1 }
          ], [
            { sample: {insert: 'ent', deleteLeft: 1, id: 12}, p: 1 }
          ], [
            { sample: {insert: 'llar', deleteLeft: 2, id: 13}, p: 1 }
          ], [
            { sample: {insert: 'o', deleteLeft: 2, id: 14}, p: 1 }
          ]
        ];

        const trueInputSources: TokenInputSource[] = trueDistributions.map((d) => {
          return {
            trueTransform: d[0].sample,
            bestProbFromSet: d[0].p,
            inputStartIndex: 0
          }
        })

        const commonRoot = new SearchPath(testModel);
        const mergeTarget = buildPath(trueDistributions, trueInputSources, commonRoot);

        // Index:  the position of the split.
        const splits: [SearchPath, SearchPath][] = [];

        // Case 0:  bare head path, reproduced token (on different root)
        splits.push([
          commonRoot, buildPath(trueDistributions, trueInputSources)
        ]);

        // Case 1: the split happens in token 2 (index 1), with the deleteLeft
        // split from the insert.
        splits.push([
          buildPath([
            trueDistributions[0],
            [{ sample: {insert: '', deleteLeft: 1, id: 12}, p: 1 }]
          ], trueInputSources.slice(0, 2), commonRoot),
          buildPath([
            [{ sample: {insert: 'ent', deleteLeft: 0, id: 12}, p: 1 }],
            ...trueDistributions.slice(2)
          ], [
            {...trueInputSources[1], inputStartIndex: 0},
            ...trueInputSources.slice(2)
          ])
        ]);

        // Case 2: the split happens in token 3 (index 2), with the deleteLeft
        // split from the insert.
        splits.push([
          buildPath([
            ...trueDistributions.slice(0, 2),
            [{ sample: {insert: '', deleteLeft: 2, id: 13}, p: 1 }]
          ], trueInputSources.slice(0, 3), commonRoot),
          buildPath([
            [{ sample: {insert: 'llar', deleteLeft: 0, id: 13}, p: 1 }],
            ...trueDistributions.slice(3)
          ], [
            {...trueInputSources[2], inputStartIndex: 0},
            ...trueInputSources.slice(3)
          ])
        ]);

        // Case 3: the split happens in token 3 (index 2), in the middle of the
        // insert.
        splits.push([
          buildPath([
            ...trueDistributions.slice(0, 2),
            [{ sample: {insert: 'l', deleteLeft: 2, id: 13}, p: 1 }]
          ], trueInputSources.slice(0, 3), commonRoot),
          buildPath([
            [{ sample: {insert: 'lar', deleteLeft: 0, id: 13}, p: 1 }],
            ...trueDistributions.slice(3)
          ], [
            {...trueInputSources[2], inputStartIndex: 1},
            ...trueInputSources.slice(3)
          ])
        ]);

        // Case 4: the split happens in token 4 (index 3), with the deleteLeft
        // split from the insert.
        splits.push([
          buildPath([
            ...trueDistributions.slice(0, 3),
            [{ sample: {insert: '', deleteLeft: 2, id: 14}, p: 1 }]
          ], trueInputSources.slice(), commonRoot),
          buildPath([
            [{ sample: {insert: 'o', deleteLeft: 0, id: 14}, p: 1 }]
          ], [
            {...trueInputSources[3], inputStartIndex: 0},
          ])
        ]);

        // Case 5: the split happens at the token's end, leaving the tail
        // as a fresh, empty token.
        splits.push([
          buildPath(trueDistributions, trueInputSources, commonRoot),
          new SearchPath(testModel)
        ]);

        return {
          mergeTarget,
          splits,
          trueDistributions
        };
      }

      const runCommonAssertions = (splitIndex: number) => {
        const { mergeTarget, splits, trueDistributions } = buildFixtures();
        const splitToTest = splits[splitIndex];

        const remergedPath = splitToTest[0].merge(splitToTest[1]);

        assert.deepEqual(remergedPath.bestExample, mergeTarget.bestExample);
        assert.equal(remergedPath.inputCount, mergeTarget.inputCount);
        assert.equal(remergedPath.codepointLength, mergeTarget.codepointLength);
        assert.sameDeepOrderedMembers(remergedPath.sourceIdentifiers, mergeTarget.sourceIdentifiers);
        assert.isTrue(remergedPath.hasInputs(trueDistributions));
      }

      it('setup: constructs path properly', () => {
        const { mergeTarget, splits } = buildFixtures();

        const targetText = mergeTarget.bestExample.text;

        for(let i = 0; i < splits.length; i++) {
          const splitSet = splits[i];

          assert.equal(splitSet[0].codepointLength, i);
          assert.equal(splitSet[0].bestExample.text, KMWString.substring(targetText, 0, i));
          assert.equal(splitSet[1].codepointLength, KMWString.length(targetText) - i);
          assert.equal(splitSet[1].bestExample.text, KMWString.substring(targetText, i));
        }
      });

      it('splits properly at index 0', () => {
        runCommonAssertions(0);
      });

      it('splits properly at index 1', () => {
        runCommonAssertions(1);
      });

      it('splits properly at index 2', () => {
        runCommonAssertions(2);
      });

      it('splits properly at index 3', () => {
        runCommonAssertions(3);
      });

      it('splits properly at index 4', () => {
        runCommonAssertions(4);
      });

      it('splits properly at index 5', () => {
        runCommonAssertions(5);
      });
    });

    // Same as the prior set, but now with non-BMP text!
    describe(`previously-split token comprised of complex, rewriting non-BMP transforms`, () => {
      const buildPath = (inputs: Distribution<Transform>[], sources: TokenInputSource[], root?: SearchPath) => {
        return inputs.reduce((path, input, index) => new SearchPath(path, input, sources[index]), root ?? new SearchPath(testModel));
      }

      const buildFixtures = () => {
        const trueDistributions = [
          [
            { sample: {insert: toMathematicalSMP('ca'), deleteLeft: 0, id: 11}, p: 1 }
          ], [
            { sample: {insert: toMathematicalSMP('ent'), deleteLeft: 1, id: 12}, p: 1 }
          ], [
            { sample: {insert: toMathematicalSMP('llar'), deleteLeft: 2, id: 13}, p: 1 }
          ], [
            { sample: {insert: toMathematicalSMP('o'), deleteLeft: 2, id: 14}, p: 1 }
          ]
        ];

        const trueInputSources: TokenInputSource[] = trueDistributions.map((d) => {
          return {
            trueTransform: d[0].sample,
            bestProbFromSet: d[0].p,
            inputStartIndex: 0
          }
        })

        const commonRoot = new SearchPath(testModel);
        const mergeTarget = buildPath(trueDistributions, trueInputSources, commonRoot);

        // Index:  the position of the split.
        const splits: [SearchPath, SearchPath][] = [];

        // Case 0:  bare head path, reproduced token (on different root)
        splits.push([
          commonRoot, buildPath(trueDistributions, trueInputSources)
        ]);

        // Case 1: the split happens in token 2 (index 1), with the deleteLeft
        // split from the insert.
        splits.push([
          buildPath([
            trueDistributions[0],
            [{ sample: {insert: toMathematicalSMP(''), deleteLeft: 1, id: 12}, p: 1 }]
          ], trueInputSources.slice(0, 2), commonRoot),
          buildPath([
            [{ sample: {insert: toMathematicalSMP('ent'), deleteLeft: 0, id: 12}, p: 1 }],
            ...trueDistributions.slice(2)
          ], [
            {...trueInputSources[1], inputStartIndex: 0},
            ...trueInputSources.slice(2)
          ])
        ]);

        // Case 2: the split happens in token 3 (index 2), with the deleteLeft
        // split from the insert.
        splits.push([
          buildPath([
            ...trueDistributions.slice(0, 2),
            [{ sample: {insert: toMathematicalSMP(''), deleteLeft: 2, id: 13}, p: 1 }]
          ], trueInputSources.slice(0, 3), commonRoot),
          buildPath([
            [{ sample: {insert: toMathematicalSMP('llar'), deleteLeft: 0, id: 13}, p: 1 }],
            ...trueDistributions.slice(3)
          ], [
            {...trueInputSources[2], inputStartIndex: 0},
            ...trueInputSources.slice(3)
          ])
        ]);

        // Case 3: the split happens in token 3 (index 2), in the middle of the
        // insert.
        splits.push([
          buildPath([
            ...trueDistributions.slice(0, 2),
            [{ sample: {insert: toMathematicalSMP('l'), deleteLeft: 2, id: 13}, p: 1 }]
          ], trueInputSources.slice(0, 3), commonRoot),
          buildPath([
            [{ sample: {insert: toMathematicalSMP('lar'), deleteLeft: 0, id: 13}, p: 1 }],
            ...trueDistributions.slice(3)
          ], [
            {...trueInputSources[2], inputStartIndex: 1},
            ...trueInputSources.slice(3)
          ])
        ]);

        // Case 4: the split happens in token 4 (index 3), with the deleteLeft
        // split from the insert.
        splits.push([
          buildPath([
            ...trueDistributions.slice(0, 3),
            [{ sample: {insert: toMathematicalSMP(''), deleteLeft: 2, id: 14}, p: 1 }]
          ], trueInputSources.slice(), commonRoot),
          buildPath([
            [{ sample: {insert: toMathematicalSMP('o'), deleteLeft: 0, id: 14}, p: 1 }]
          ], [
            {...trueInputSources[3], inputStartIndex: 0},
          ])
        ]);

        // Case 5: the split happens at the token's end, leaving the tail
        // as a fresh, empty token.
        splits.push([
          buildPath(trueDistributions, trueInputSources, commonRoot),
          new SearchPath(testModel)
        ]);

        return {
          mergeTarget,
          splits,
          trueDistributions
        };
      }

      const runCommonAssertions = (splitIndex: number) => {
        const { mergeTarget, splits, trueDistributions } = buildFixtures();
        const splitToTest = splits[splitIndex];

        const remergedPath = splitToTest[0].merge(splitToTest[1]);

        assert.deepEqual(remergedPath.bestExample, mergeTarget.bestExample);
        assert.equal(remergedPath.inputCount, mergeTarget.inputCount);
        assert.equal(remergedPath.codepointLength, mergeTarget.codepointLength);
        assert.sameDeepOrderedMembers(remergedPath.sourceIdentifiers, mergeTarget.sourceIdentifiers);
        assert.isTrue(remergedPath.hasInputs(trueDistributions));
      }

      it('setup: constructs path properly', () => {
        // Validate that an SMP-conversion has occurred.
        assert.notEqual(toMathematicalSMP("cello"), "cello");
        assert.equal(toMathematicalSMP("cello").length, "cello".length * 2);
        assert.equal(KMWString.length(toMathematicalSMP("cello")), KMWString.length("cello"));

        const { mergeTarget, splits } = buildFixtures();

        const targetText = mergeTarget.bestExample.text;
        assert.equal(targetText, toMathematicalSMP("cello"));

        for(let i = 0; i < splits.length; i++) {
          const splitSet = splits[i];

          assert.equal(splitSet[0].codepointLength, i);
          assert.equal(splitSet[0].bestExample.text, KMWString.substring(targetText, 0, i));
          assert.equal(splitSet[1].codepointLength, KMWString.length(targetText) - i);
          assert.equal(splitSet[1].bestExample.text, KMWString.substring(targetText, i));
        }
      });

      it('splits properly at index 0', () => {
        runCommonAssertions(0);
      });

      it('splits properly at index 1', () => {
        runCommonAssertions(1);
      });

      it('splits properly at index 2', () => {
        runCommonAssertions(2);
      });

      it('splits properly at index 3', () => {
        runCommonAssertions(3);
      });

      it('splits properly at index 4', () => {
        runCommonAssertions(4);
      });

      it('splits properly at index 5', () => {
        runCommonAssertions(5);
      });
    });

    it('correctly merges paths previously split mid-input', () => {
      let path = new SearchPath(testModel);
      const startSample = {sample: { insert: 'a', deleteLeft: 0 }, p: 1}
      path = new SearchPath(path, [startSample], startSample);

      const inputDistribution = [
        {sample: { insert: 'four', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.4},
        {sample: { insert: 'then', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.3},
        {sample: { insert: 'nine', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.2},
        {sample: { insert: 'what', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.06},
        {sample: { insert: 'cent', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.04}
      ];

      const mergeTarget = new SearchPath(path, inputDistribution, inputDistribution[0]);
      assert.equal(mergeTarget.codepointLength, 4);
      assert.equal(mergeTarget.inputCount, 2);

      // This test models a previous split at codepoint index 2, splitting
      // the input distribution accordingly.  (Note:  deleteLeft = 1!)
      const headDistributionSplit = [
        {sample: { insert: 'fo', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.4},
        {sample: { insert: 'th', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.3},
        {sample: { insert: 'ni', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.2},
        {sample: { insert: 'wh', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.06},
        {sample: { insert: 'ce', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.04}
      ];
      const headPath = new SearchPath(
        path, headDistributionSplit, inputDistribution[0]
      );

      const tailDistributionSplit = [
        {sample: { insert: 'ur', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.4},
        {sample: { insert: 'en', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.3},
        {sample: { insert: 'ne', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.2},
        {sample: { insert: 'at', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.06},
        {sample: { insert: 'nt', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.04}
      ];
      const tailPath = new SearchPath(
        new SearchPath(testModel), tailDistributionSplit, {
          trueTransform: inputDistribution[0].sample,
          bestProbFromSet: inputDistribution[0].p,
          inputStartIndex: 2
        }
      );

      const remerged = headPath.merge(tailPath);

      assert.deepEqual(remerged.bestExample, mergeTarget.bestExample);
      assert.equal(remerged.inputCount, 2);
      assert.isTrue(remerged instanceof SearchPath);
      assert.deepEqual((remerged as SearchPath).inputs, inputDistribution);
      assert.isTrue(remerged.hasInputs([[startSample], inputDistribution]));
    });
  });
});