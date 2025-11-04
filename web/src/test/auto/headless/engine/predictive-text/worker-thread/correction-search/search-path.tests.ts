/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-29
 *
 * This file defines tests for the SearchSpace class of the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { KMWString } from '@keymanapp/web-utils';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { models, SearchPath } from '@keymanapp/lm-worker/test-index';

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

export function buildSimplePathSplitFixture() {
  const rootPath = new SearchPath(testModel);

  const distrib1 = [
    { sample: {insert: 'c', deleteLeft: 0, id: 11}, p: 0.5 },
    { sample: {insert: 'r', deleteLeft: 0, id: 11}, p: 0.4 },
    { sample: {insert: 't', deleteLeft: 0, id: 11}, p: 0.1 }
  ];
  const path1 = new SearchPath(rootPath, distrib1, distrib1[0]);

  const distrib2 = [
    { sample: {insert: 'a', deleteLeft: 0, id: 12}, p: 0.7 },
    { sample: {insert: 'e', deleteLeft: 0, id: 12}, p: 0.3 }
  ];
  const path2 = new SearchPath(path1, distrib2, distrib2[0]);

  const distrib3 = [
    { sample: {insert: 'n', deleteLeft: 0, id: 13}, p: 0.8 },
    { sample: {insert: 'r', deleteLeft: 0, id: 13}, p: 0.2 }
  ];
  const path3 = new SearchPath(path2, distrib3, distrib3[0]);

  const distrib4 = [
    { sample: {insert: 't', deleteLeft: 0, id: 14}, p: 1 }
  ];
  const path4 = new SearchPath(path3, distrib4, distrib4[0]);

  return {
    paths: [rootPath, path1, path2, path3, path4],
    distributions: [distrib1, distrib2, distrib3, distrib4]
  };
}

describe('SearchPath', () => {
  describe('constructor', () => {
    it('initializes from a lexical model', () => {
      const path = new SearchPath(testModel);
      assert.equal(path.inputCount, 0);
      assert.equal(path.codepointLength, 0);
      assert.isNumber(path.spaceId);
      assert.deepEqual(path.bestExample, {text: '', p: 1});
      assert.deepEqual(path.parents, []);
      assert.isNotOk(path.inputs);
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
      assert.deepEqual(extendedPath.inputSegments, [
        {
          segment: {
            trueTransform: leadEdgeDistribution[0].sample,
            transitionId: leadEdgeDistribution[0].sample.id,
            start: 0
          },
          bestProbFromSet: leadEdgeDistribution[0].p,
          // Just write in the variable-value entry; the rest should match perfectly.
          subsetId: extendedPath.inputSegments[0].subsetId
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
      assert.deepEqual(length2Path.inputSegments, [
        {
          segment: {
            trueTransform: leadEdgeDistribution[0].sample,
            transitionId: leadEdgeDistribution[0].sample.id,
            start: 0
          },
          bestProbFromSet: leadEdgeDistribution[0].p,
          // Just write in the variable-value entry; the rest should match perfectly.
          subsetId: length2Path.inputSegments[0].subsetId
        }, {
          segment: {
            trueTransform: tailEdgeDistribution[0].sample,
            transitionId: tailEdgeDistribution[0].sample.id,
            start: 0
          },
          bestProbFromSet: tailEdgeDistribution[0].p,
          // Just write in the variable-value entry; the rest should match perfectly.
          subsetId: length2Path.inputSegments[1].subsetId
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

    it('throws if input and input-source transition IDs mismatch', () => {
      const rootPath = new SearchPath(testModel);

      const leadEdgeDistribution = [
        {sample: {insert: 't', deleteLeft: 0, id: 13 }, p: 0.5},
        {sample: {insert: 'a', deleteLeft: 0, id: 13 }, p: 0.3},
        {sample: {insert: 'o', deleteLeft: 0, id: 13 }, p: 0.2}
      ];

      assert.throws(() => new SearchPath(rootPath, leadEdgeDistribution, {
        ...leadEdgeDistribution[0],
        sample: {...leadEdgeDistribution[0].sample, id: 15}
      }));
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
      assert.deepEqual(length2Path.inputSegments, [
        {
          segment: {
            trueTransform: leadEdgeDistribution[0].sample,
            transitionId: leadEdgeDistribution[0].sample.id,
            start: 0
          },
          bestProbFromSet: leadEdgeDistribution[0].p,
          // Just write in the variable-value entry; the rest should match perfectly.
          subsetId: length2Path.inputSegments[0].subsetId
        }, {
          segment: {
            trueTransform: tailEdgeDistribution[0].sample,
            transitionId: tailEdgeDistribution[0].sample.id,
            start: 0
          },
          bestProbFromSet: tailEdgeDistribution[0].p,
          // Just write in the variable-value entry; the rest should match perfectly.
          subsetId: length2Path.inputSegments[1].subsetId
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
  });

  describe('fixture construction', () => {
    it('setup: buildSimplePathSplitFixture() constructs paths properly', () => {
      const { paths, distributions } = buildSimplePathSplitFixture();
      const pathToSplit = paths[4];

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
  });

  describe('hasInputs()', () => {
    it('matches an empty array on root SearchPaths', () => {
      assert.isTrue(new SearchPath(testModel).hasInputs([]));
    });

    it('matches all path inputs when provided in proper order', () => {
      const { paths, distributions } = buildSimplePathSplitFixture();
      assert.isTrue(paths[4].hasInputs(distributions));
    });

    it('does not match when any path input component is missing', () => {
      const { paths, distributions } = buildSimplePathSplitFixture();
      assert.isFalse(paths[4].hasInputs(distributions.slice(1)));
      assert.isFalse(paths[4].hasInputs(distributions.slice(2)));
      assert.isFalse(paths[4].hasInputs(distributions.slice(3)));
      assert.isFalse(paths[4].hasInputs(distributions.slice(0, 3)));
      assert.isFalse(paths[4].hasInputs(distributions.slice(0, 1).concat(distributions.slice(2))));
    });

    it('does not match when path inputs are not in proper order', () => {
      const { paths, distributions } = buildSimplePathSplitFixture();
      assert.isFalse(paths[4].hasInputs(distributions.slice().reverse()));

      // Random shuffle.
      let shuffled: typeof distributions;
      let isShuffled: boolean;
      do {
        shuffled = distributions.slice().sort(() => Math.random() * 2 - 1);
        // Validate that we actually shuffled - that we didn't land on the original order!
        isShuffled = false;
        for(let i = 0; i < distributions.length; i++) {
          if(distributions[i] != shuffled[i]) {
            isShuffled = true;
            break;
          }
        }
      } while(!isShuffled);
      assert.isFalse(paths[4].hasInputs(shuffled));
    });
  });

  describe('constituentPaths', () => {
    it('includes a single entry when all parents are SearchPaths', () => {
      const { paths } = buildSimplePathSplitFixture();
      const finalPath = paths[4];

      assert.equal(finalPath.constituentPaths.length, 1);

      const pathSequence = finalPath.constituentPaths[0];
      assert.equal(pathSequence.length, 5); // 4 inputs + 1 root node

      assert.sameOrderedMembers(pathSequence, paths);
    });

    // TODO:  add a test for mixed SearchPath / SearchCluster cases.
  });

  describe('split()', () => {
    describe(`on token comprised of single-char transforms:  [crt][ae][nr][t]`, () => {
      const runSplit = (splitIndex: number) => {
        const { paths, distributions } = buildSimplePathSplitFixture();
        const pathToSplit = paths[4];

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

      it('splits properly at index 0', () => {
        runSplit(0);

        const { paths, distributions } = buildSimplePathSplitFixture();
        const pathToSplit = paths[4];
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

        const { paths } = buildSimplePathSplitFixture();
        const pathToSplit = paths[4];
        const [head] = pathToSplit.split(1);

        assert.equal(head, pathToSplit.parents[0].parents[0].parents[0]);
      });

      it('splits properly at index 2', () => {
        runSplit(2);

        const { paths } = buildSimplePathSplitFixture();
        const pathToSplit = paths[4];
        const [head] = pathToSplit.split(2);

        assert.equal(head, pathToSplit.parents[0].parents[0]);
      });

      it('splits properly at index 3', () => {
        runSplit(3);

        const { paths } = buildSimplePathSplitFixture();
        const pathToSplit = paths[4];
        const [head] = pathToSplit.split(3);

        assert.equal(head, pathToSplit.parents[0]);
      });

      it('splits properly at index 4', () => {
        runSplit(4);

        const { paths } = buildSimplePathSplitFixture();
        const pathToSplit = paths[4];
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

        // c in the first input, though the 'a' part is deleted later.
        assert.equal(head.inputCount, 1);
        assert.equal(tail.inputCount, 4); // split transform!
        // is always built from the same root path, while the tail is not.

        const headDistribs = distributions.slice(0, 0);
        const tailDistribs = distributions.slice(1);

        const splitDistrib = distributions[0];
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

        // ce in the second input, though the n is deleted later.
        assert.equal(head.inputCount, 2);
        assert.equal(tail.inputCount, 3); // split transform!
        // is always built from the same root path, while the tail is not.

        const headDistribs = distributions.slice(0, 1);
        const tailDistribs = distributions.slice(2);

        const splitDistrib = distributions[1];
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

        // cellar in the third input, though the -ar is deleted later.
        assert.equal(head.inputCount, 3);
        assert.equal(tail.inputCount, 2); // split transform!

        const headDistribs = distributions.slice(0, 2);
        const tailDistribs = distributions.slice(3);

        const splitDistrib = distributions[2];
        const distribHalves = splitDistrib.map((entry) => {
          return {
            head: { sample: {...entry.sample, insert: entry.sample.insert.slice(0, 2)}, p: entry.p },
            tail: { sample: {...entry.sample, insert: entry.sample.insert.slice(2), deleteLeft: 0}, p: entry.p }
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

        assert.deepEqual(head.parents[0].bestExample.text, "cent");
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

        // c in the first input, though the 'a' part is deleted later.
        assert.equal(head.inputCount, 1);
        assert.equal(tail.inputCount, 4); // split transform!
        // is always built from the same root path, while the tail is not.

        const headDistribs = distributions.slice(0, 0);
        const tailDistribs = distributions.slice(1);

        const splitDistrib = distributions[0];
        const distribHalves = splitDistrib.map((entry) => {
          return {
            head: { sample: {...entry.sample, insert: KMWString.substring(entry.sample.insert, 0, 1)}, p: entry.p },
            tail: { sample: {...entry.sample, insert: KMWString.substring(entry.sample.insert, 1), deleteLeft: 0}, p: entry.p }
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

        // ce in the second input, though the n is deleted later.
        assert.equal(head.inputCount, 2);
        assert.equal(tail.inputCount, 3); // split transform!
        // is always built from the same root path, while the tail is not.

        const headDistribs = distributions.slice(0, 1);
        const tailDistribs = distributions.slice(2);

        const splitDistrib = distributions[1];
        const distribHalves = splitDistrib.map((entry) => {
          return {
            head: { sample: {...entry.sample, insert: KMWString.substring(entry.sample.insert, 0, 1)}, p: entry.p },
            tail: { sample: {...entry.sample, insert: KMWString.substring(entry.sample.insert, 1), deleteLeft: 0}, p: entry.p }
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

        // cellar in the third input, though the -ar is deleted later.
        assert.equal(head.inputCount, 3);
        assert.equal(tail.inputCount, 2); // split transform!

        const headDistribs = distributions.slice(0, 2);
        const tailDistribs = distributions.slice(3);

        const splitDistrib = distributions[2];
        const distribHalves = splitDistrib.map((entry) => {
          return {
            head: { sample: {...entry.sample, insert: KMWString.substring(entry.sample.insert, 0, 2)}, p: entry.p },
            tail: { sample: {...entry.sample, insert: KMWString.substring(entry.sample.insert, 2), deleteLeft: 0}, p: entry.p }
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

        assert.deepEqual(head.parents[0].bestExample.text, toMathematicalSMP("cent"));
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

      const [head, tail] = pathToSplit.split(2);

      assert.deepEqual(head.bestExample, headTarget.bestExample);
      assert.deepEqual(tail.bestExample, tailTarget.bestExample);
      assert.equal(head.inputCount, headTarget.inputCount);
      assert.equal(tail.inputCount, tailTarget.inputCount);
      assert.isTrue(head instanceof SearchPath);
      assert.isTrue(tail instanceof SearchPath);
      assert.deepEqual((head as SearchPath).inputs, headTarget.inputs);
      assert.deepEqual((tail as SearchPath).inputs, tailTarget.inputs);
      assert.deepEqual((head as SearchPath).inputSource, headTarget.inputSource);
      assert.deepEqual((tail as SearchPath).inputSource, tailTarget.inputSource);
    });
  });
});