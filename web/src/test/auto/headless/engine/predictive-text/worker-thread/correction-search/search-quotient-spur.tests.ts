/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-29
 *
 * This file defines tests for the SearchQuotientSpur classes of the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { KMWString } from '@keymanapp/web-utils';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import {
  generateSubsetId,
  LegacyQuotientRoot,
  LegacyQuotientSpur,
  models,
  PathInputProperties,
  SearchNode,
  SearchQuotientNode,
  SearchQuotientRoot,
  SearchQuotientSpur
} from '@keymanapp/lm-worker/test-index';

import { constituentPaths } from '../../helpers/constituentPaths.js';
import { toSpurTypeSequence } from '../../helpers/toSpurTypeSequence.js';
import { quotientPathHasInputs } from '../../helpers/quotientPathHasInputs.js';
import { buildCantLinearFixture } from '../../helpers/buildCantLinearFixture.js';
import { buildQuotientDocFixture } from '../../helpers/buildQuotientDocFixture.js';

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

class MockQuotientSpur extends SearchQuotientSpur {
  insertLength: number;
  leftDeleteLength: number;

  readonly _receivedNodes: SearchNode[] = [];

  public get receivedNodes(): SearchNode[] {
    // This triggers .buildEdgesForNodes() for any new results from the parent.
    //
    // .handleNextNode() and .currentCost also trigger this, but this mock
    // implementation bypasses both.
    this.processPendingRoots();
    return this._receivedNodes;
  }

  constructor(parentNode: SearchQuotientNode, inputs: Distribution<Transform>, inputSource: PathInputProperties) {
    super(parentNode, inputs, inputSource, 0);
  }

  construct(parentNode: SearchQuotientNode, inputs: Distribution<Transform>, inputSource: PathInputProperties): this {
    return new MockQuotientSpur(parentNode, inputs, inputSource) as this;
  }

  protected buildEdgesForNodes(baseNodes: ReadonlyArray<SearchNode>): SearchNode[] {
    baseNodes.forEach((n) => this._receivedNodes.push(n));
    return [];
  }
}

describe('SearchQuotientSpur', () => {
  it('constructor links to parent node and receives queued results from it', () => {
    const baseNode = new SearchQuotientRoot(testModel);
    const descendantSpur = new MockQuotientSpur(baseNode, /* ? */ null, /* ? */ null);

    assert.isEmpty(descendantSpur.receivedNodes);
    baseNode.handleNextNode();
    assert.isNotEmpty(descendantSpur.receivedNodes);
  });

  describe('split()', () => {
    describe(`on token comprised of single-char transforms:  [crt][ae][nr][t]`, () => {
      const runSplit = (splitIndex: number) => {
        const { paths, distributions } = buildCantLinearFixture();
        const pathToSplit = paths[4];

        const splitResults = pathToSplit.split(splitIndex);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

        assert.equal(head.inputCount, splitIndex);
        assert.equal(tail.inputCount, pathToSplit.inputCount - splitIndex);
        // The split operation will still reconstruct the token; the head
        // is always built from the same root path, while the tail is not.
        const headDistrib = distributions.slice(0, splitIndex);
        const tailDistrib = distributions.slice(splitIndex);
        assert.isTrue(quotientPathHasInputs(head, headDistrib));
        assert.isTrue(quotientPathHasInputs(tail, tailDistrib));

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

        const { paths, distributions } = buildCantLinearFixture();
        const pathToSplit = paths[4];
        const [head, tail] = pathToSplit.split(0)[0];

        // The split operation will still reconstruct the token; the head
        // is always built from the same root path, while the tail is not.
        assert.notEqual(tail, pathToSplit);
        assert.deepEqual(head.parents, []);
        assert.notEqual(head, constituentPaths(pathToSplit)[0][0]);

        assert.isTrue(quotientPathHasInputs(head, []));
        assert.isTrue(quotientPathHasInputs(tail, distributions));
      });

      it('splits properly at index 1', () => {
        runSplit(1);

        const { paths } = buildCantLinearFixture();
        const pathToSplit = paths[4];
        const [head] = pathToSplit.split(1)[0];

        assert.equal(head, pathToSplit.parents[0].parents[0].parents[0]);
      });

      it('splits properly at index 2', () => {
        runSplit(2);

        const { paths } = buildCantLinearFixture();
        const pathToSplit = paths[4];
        const [head] = pathToSplit.split(2)[0];

        assert.equal(head, pathToSplit.parents[0].parents[0]);
      });

      it('splits properly at index 3', () => {
        runSplit(3);

        const { paths } = buildCantLinearFixture();
        const pathToSplit = paths[4];
        const [head] = pathToSplit.split(3)[0];

        assert.equal(head, pathToSplit.parents[0]);
      });

      it('splits properly at index 4', () => {
        runSplit(4);

        const { paths } = buildCantLinearFixture();
        const pathToSplit = paths[4];
        const [head] = pathToSplit.split(4)[0];

        assert.equal(head, pathToSplit);
      });
    });

    describe(`on token comprised of two-char transforms:  ca nt el ou pe`, () => {
      const buildPath = () => {
        let path: SearchQuotientNode = new LegacyQuotientRoot(testModel);

        const distrib1 = [
          { sample: {insert: 'ca', deleteLeft: 0, id: 11}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib1, distrib1[0]);

        const distrib2 = [
          { sample: {insert: 'nt', deleteLeft: 0, id: 12}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib2, distrib2[0]);

        const distrib3 = [
          { sample: {insert: 'el', deleteLeft: 0, id: 13}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib3, distrib3[0]);

        const distrib4 = [
          { sample: {insert: 'ou', deleteLeft: 0, id: 14}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib4, distrib4[0]);

        const distrib5 = [
          { sample: {insert: 'pe', deleteLeft: 0, id: 15}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib5, distrib5[0]);

        return {
          path,
          distributions: [distrib1, distrib2, distrib3, distrib4, distrib5]
        };
      }

      const runSplit = (splitIndex: number) => {
        const { path: pathToSplit, distributions } = buildPath();

        const splitResults = pathToSplit.split(splitIndex);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

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

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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
        assert.isTrue(quotientPathHasInputs(pathToSplit, distributions));
        assert.equal(constituentPaths(pathToSplit).length, 1);
      });

      it('splits properly at index 0', () => {
        runSplit(0);

        const { path: pathToSplit, distributions } = buildPath();
        const [head, tail] = pathToSplit.split(0)[0];

        // The split operation will still reconstruct the token; the head
        // is always built from the same root path, while the tail is not.
        assert.notEqual(tail, pathToSplit);
        assert.deepEqual(head.parents, []);
        assert.notEqual(head, constituentPaths(pathToSplit)[0][0]);

        assert.isTrue(quotientPathHasInputs(head, []));
        assert.isTrue(quotientPathHasInputs(tail, distributions));
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
        const [head, tail] = pathToSplit.split(8)[0];

        assert.equal(head, pathToSplit.parents[0]);
        assert.equal((tail as SearchQuotientSpur).inputSource, (pathToSplit as SearchQuotientSpur).inputSource);
      });

      it('splits properly at index 9', () => {
        runSplit(9);

        const { path: pathToSplit } = buildPath();
        const [head, tail] = pathToSplit.split(9)[0];

        // Same parent, but not the same final step - it _was_ split, after
        // all.
        assert.equal(head.parents[0], pathToSplit.parents[0]);

        const headSrc = (head as SearchQuotientSpur).inputSource;
        const tailSrc = (tail as SearchQuotientSpur).inputSource;
        assert.equal(headSrc.subsetId, tailSrc.subsetId);
        assert.equal(headSrc.segment.transitionId, tailSrc.segment.transitionId);
        assert.equal(headSrc.segment.start, 0);
        assert.equal(tailSrc.segment.start, 1);
        assert.equal(headSrc.segment.end, tailSrc.segment.start);
        assert.isUndefined(tailSrc.segment.end);
      });

      it('splits properly at index 10', () => {
        runSplit(10);

        const { path: pathToSplit } = buildPath();
        const [head, tail] = pathToSplit.split(10)[0];

        assert.equal(head, pathToSplit);
        assert.isTrue(tail instanceof SearchQuotientRoot);
      });
    });

    describe(`on token comprised of complex, rewriting transforms:  cello`, () => {
      const buildPath = () => {
        let path: SearchQuotientNode = new LegacyQuotientRoot(testModel);

        const distrib1 = [
          { sample: {insert: 'ca', deleteLeft: 0, id: 11}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib1, distrib1[0]);

        const distrib2 = [
          { sample: {insert: 'ent', deleteLeft: 1, id: 12}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib2, distrib2[0]);

        const distrib3 = [
          { sample: {insert: 'llar', deleteLeft: 2, id: 13}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib3, distrib3[0]);

        const distrib4 = [
          { sample: {insert: 'o', deleteLeft: 2, id: 14}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib4, distrib4[0]);

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
        assert.isTrue(quotientPathHasInputs(pathToSplit, distributions));
        assert.equal(constituentPaths(pathToSplit).length, 1);
      });

      it('splits properly at index 0', () => {
        const splitIndex = 0;
        const { path: pathToSplit, distributions } = buildPath();

        const splitResults = pathToSplit.split(splitIndex);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

        assert.equal(head.inputCount, 0);
        assert.equal(tail.inputCount, 4);
        // is always built from the same root path, while the tail is not.
        const headDistribs = distributions.slice(0, 0);
        const tailDistribs = distributions.slice(0);

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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
        assert.notEqual(head, constituentPaths(pathToSplit)[0][0]);

        assert.isTrue(quotientPathHasInputs(head, []));
        assert.isTrue(quotientPathHasInputs(tail, distributions));
      });

      it('splits properly at index 1', () => {
        const splitIndex = 1;
        const { path: pathToSplit, distributions } = buildPath();

        const splitResults = pathToSplit.split(splitIndex);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

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

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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

        const splitResults = pathToSplit.split(2);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

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

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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

        const splitResults = pathToSplit.split(3);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

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

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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

        const splitResults = pathToSplit.split(4);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

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

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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
        let path: SearchQuotientNode = new LegacyQuotientRoot(testModel);

        const distrib1 = [
          { sample: {insert: 'biglargetransform', deleteLeft: 0, id: 11}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib1, distrib1[0]);

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
        assert.isTrue(quotientPathHasInputs(pathToSplit, distributions));
        assert.equal(constituentPaths(pathToSplit).length, 1);
      });

      it('splits properly after \'big\'', () => {
        const { path: pathToSplit, distributions } = buildPath();

        const splitResults = pathToSplit.split(3);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

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

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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

        const splitResults = pathToSplit.split(8);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

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

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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
        let path: SearchQuotientNode = new LegacyQuotientRoot(testModel);

        const distrib1 = [
          { sample: {insert: toMathematicalSMP('ca'), deleteLeft: 0, id: 11}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib1, distrib1[0]);

        const distrib2 = [
          { sample: {insert: toMathematicalSMP('ent'), deleteLeft: 1, id: 12}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib2, distrib2[0]);

        const distrib3 = [
          { sample: {insert: toMathematicalSMP('llar'), deleteLeft: 2, id: 13}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib3, distrib3[0]);

        const distrib4 = [
          { sample: {insert: toMathematicalSMP('o'), deleteLeft: 2, id: 14}, p: 1 }
        ];
        path = new LegacyQuotientSpur(path, distrib4, distrib4[0]);

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
        assert.isTrue(quotientPathHasInputs(pathToSplit, distributions));
        assert.equal(constituentPaths(pathToSplit).length, 1);
      });

      it('splits properly at index 0', () => {
        const splitIndex = 0;
        const { path: pathToSplit, distributions } = buildPath();

        const splitResults = pathToSplit.split(splitIndex);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

        assert.equal(head.inputCount, 0);
        assert.equal(tail.inputCount, 4);
        // is always built from the same root path, while the tail is not.
        const headDistribs = distributions.slice(0, 0);
        const tailDistribs = distributions.slice(0);

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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
        assert.notEqual(head, constituentPaths(pathToSplit)[0][0]);

        assert.isTrue(quotientPathHasInputs(head, []));
        assert.isTrue(quotientPathHasInputs(tail, distributions));
      });

      it('splits properly at index 1', () => {
        const splitIndex = 1;
        const { path: pathToSplit, distributions } = buildPath();

        const splitResults = pathToSplit.split(splitIndex);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

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

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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

        const splitResults = pathToSplit.split(2);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

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

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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

        const splitResults = pathToSplit.split(3);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

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

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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

        const splitResults = pathToSplit.split(4);
        assert.equal(splitResults.length, 1);
        const [head, tail] = splitResults[0];

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

        assert.isTrue(quotientPathHasInputs(head, headDistribs));
        assert.isTrue(quotientPathHasInputs(tail, tailDistribs));

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
      let path: SearchQuotientNode = new LegacyQuotientRoot(testModel);
      const startSample = {sample: { insert: 'a', deleteLeft: 0 }, p: 1}
      path = new LegacyQuotientSpur(path, [startSample], startSample);

      const inputDistribution = [
        {sample: { insert: 'four', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.4},
        {sample: { insert: 'then', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.3},
        {sample: { insert: 'nine', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.2},
        {sample: { insert: 'what', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.06},
        {sample: { insert: 'cent', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.04}
      ];

      const pathToSplit = new LegacyQuotientSpur(path, inputDistribution, inputDistribution[0]);
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
      const headTarget = new LegacyQuotientSpur(
        path, headDistributionSplit, {
          segment: {
            start: 0,
            end: 2,
            transitionId: inputDistribution[0].sample.id
          },
          bestProbFromSet: inputDistribution[0].p,
          subsetId: pathToSplit.inputSource.subsetId
        }
      );

      const tailDistributionSplit = [
        {sample: { insert: 'ur', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.4},
        {sample: { insert: 'en', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.3},
        {sample: { insert: 'ne', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.2},
        {sample: { insert: 'at', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.06},
        {sample: { insert: 'nt', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.04}
      ];
      const tailTarget = new LegacyQuotientSpur(
        new LegacyQuotientRoot(testModel), tailDistributionSplit, {
          segment: {
            start: 2,
            transitionId: inputDistribution[0].sample.id
          },
          bestProbFromSet: inputDistribution[0].p,
          subsetId: pathToSplit.inputSource.subsetId
        }
      );

      const splitResults = pathToSplit.split(2);
      assert.equal(splitResults.length, 1);
      const [head, tail] = splitResults[0];

      assert.deepEqual(head.bestExample, headTarget.bestExample);
      assert.deepEqual(tail.bestExample, tailTarget.bestExample);
      assert.equal(head.inputCount, headTarget.inputCount);
      assert.equal(tail.inputCount, tailTarget.inputCount);
      assert.isTrue(head instanceof LegacyQuotientSpur);
      assert.isTrue(tail instanceof LegacyQuotientSpur);
      assert.deepEqual((head as LegacyQuotientSpur).inputs, headTarget.inputs);
      assert.deepEqual((tail as LegacyQuotientSpur).inputs, tailTarget.inputs);
      assert.deepEqual((head as LegacyQuotientSpur).inputSource, headTarget.inputSource);
      assert.deepEqual((tail as LegacyQuotientSpur).inputSource, tailTarget.inputSource);
    });


    describe('correctly handles the quotient-path doc example', () => {
      it('splits properly at index 0', () => {
        const fixture = buildQuotientDocFixture();
        const k2c3 = fixture.nodes.k2c3;

        const splits = k2c3.split(0);
        assert.equal(splits.length, 1);
        assert.equal(splits[0][0], fixture.searchRoot);
        assert.isTrue(splits[0][1].isSameNode(k2c3));
      });

      it('splits properly at index 1', () => {
        const nodes = buildQuotientDocFixture().nodes;
        const k2c3 = nodes.k2c3;

        const splits = k2c3.split(1);
        /*
         * Unironically, actually 5.
         *
         * 3 are "clean", with 2 "dirty" - the "dirty" two both split in the
         * middle of input for the first keystroke.
         */
        assert.equal(splits.length, 5);
        assert.sameDeepMembers(splits.map(s => s.map(n => n.inputCount)), [
          // clean
          [0, 2], // 1 inserted char, then both inputs
          [1, 1], // 1 std keystroke, then the other input
          [2, 0], // BOTH keystrokes processed (with one deleted), then insertions afterward
          // dirty
          [1, 2], // 1/2 keystroke inserted, other 1/2 is in tail
          [2, 1]  // first keystroke deleted, second keystroke 1/2 inserted with remainder in tail
        ]);

        splits.forEach(s => {
          assert.equal(s[0].codepointLength, 1);
          assert.equal(s[1].codepointLength, 2);
        });

        const cleanSplit0KeyHead = splits.find(s => s[0].inputCount == 0 && s[1].inputCount == 2);
        const cleanSplit1KeyHead = splits.find(s => s[0].inputCount == 1 && s[1].inputCount == 1);
        const cleanSplit2KeyHead = splits.find(s => s[0].inputCount == 2 && s[1].inputCount == 0);

        assert.equal(constituentPaths(cleanSplit0KeyHead[0]).length, 1);
        assert.sameDeepMembers(constituentPaths(cleanSplit0KeyHead[0]).map(toSpurTypeSequence), [
          ['insert']
        ]);
        assert.equal(constituentPaths(cleanSplit1KeyHead[0]).length, 2);
        assert.sameDeepMembers(constituentPaths(cleanSplit1KeyHead[0]).map(toSpurTypeSequence), [
          ['insert', 'delete'],
          ['substitute']
        ]);
        assert.equal(constituentPaths(cleanSplit2KeyHead[0]).length, 3);
        assert.sameDeepMembers(constituentPaths(cleanSplit2KeyHead[0]).map(toSpurTypeSequence), [
          ['insert', 'delete', 'delete'],
          ['substitute', 'delete'],
          ['delete', 'substitute']
        ]);
      });

      it('splits properly at index 2', () => {
        const nodes = buildQuotientDocFixture().nodes;
        const k2c3 = nodes.k2c3;

        const splits = k2c3.split(2);
        /*
         * Unironically, actually 5.
         *
         * 3 are "clean", with 2 "dirty" - the "dirty" two both split in the
         * middle of input for the first keystroke.
         */
        assert.equal(splits.length, 5);
        assert.sameDeepMembers(splits.map(s => s.map(n => n.inputCount)), [
          // clean
          [0, 2], // 1 inserted char, then both inputs
          [1, 1], // 1 std keystroke, then the other input
          [2, 0], // BOTH keystrokes processed (with one deleted), then insertions afterward
          // dirty
          [1, 2], //insert, then 1/2 keystroke inserted, other 1/2 is in tail
          [2, 1]  // first keystroke deleted or short, second keystroke 1/2 inserted with remainder in tail
        ]);

        splits.forEach(s => {
          assert.equal(s[0].codepointLength, 2);
          assert.equal(s[1].codepointLength, 1);
        });

        const cleanSplit0KeyHead = splits.find(s => s[0].inputCount == 0 && s[1].inputCount == 2);
        const cleanSplit1KeyHead = splits.find(s => s[0].inputCount == 1 && s[1].inputCount == 1);
        const cleanSplit2KeyHead = splits.find(s => s[0].inputCount == 2 && s[1].inputCount == 0);

        assert.equal(constituentPaths(cleanSplit0KeyHead[0]).length, 1);
        assert.sameDeepMembers(constituentPaths(cleanSplit0KeyHead[0]).map(toSpurTypeSequence), [
          ['insert', 'insert']
        ]);
        assert.equal(constituentPaths(cleanSplit1KeyHead[0]).length, 4);
        assert.sameDeepMembers(constituentPaths(cleanSplit1KeyHead[0]).map(toSpurTypeSequence), [
          ['insert', 'insert', 'delete'],  // is too high an edit-cost, though...
          ['substitute', 'insert'],
          ['insert', 'substitute'],
          ['substitute']
        ]);
        assert.equal(constituentPaths(cleanSplit2KeyHead[0]).length, 8);
        assert.sameDeepMembers(constituentPaths(cleanSplit2KeyHead[0]).map(toSpurTypeSequence), [
          ['insert', 'insert', 'delete', 'delete'], // is too high an edit-cost, though...
          ['substitute', 'substitute'],
          ['substitute', 'delete'],
          ['delete', 'substitute'],
          ['insert', 'substitute', 'delete'],
          ['substitute', 'insert', 'delete'],
          ['insert', 'delete', 'substitute'],
          ['delete', 'substitute', 'insert']
        ]);
      });

      it('splits properly at index 3', () => {
        const fixture = buildQuotientDocFixture();
        const k2c3 = fixture.nodes.k2c3;

        const splits = k2c3.split(3);
        assert.equal(splits.length, 1);
        assert.equal(splits[0][0], k2c3);
        assert.isTrue(splits[0][1].isSameNode(fixture.searchRoot));
      });
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

        const originalInputBase: PathInputProperties = {
          segment: {
            start: 0,
            transitionId: 11
          },
          bestProbFromSet: 1,
          subsetId: generateSubsetId()
        };

        const splitOriginalInputs = [0, 3, 8].map(n => ({
          ...originalInputBase,
          segment: {
            ...originalInputBase.segment,
            start: n
          }
        }));
        splitOriginalInputs[0].segment.end = 3;
        splitOriginalInputs[1].segment.end = 8;

        const paths = distributions.map((d, i) => new LegacyQuotientSpur(new LegacyQuotientRoot(testModel), d, splitOriginalInputs[i]));

        return {
          paths,
          distributions,
          splitOriginalInputs,
          originalInput: originalInputBase
        };
      }

      const checkFinalStateAssertions = (merged: SearchQuotientSpur, originalInput: PathInputProperties) => {
        assert.equal(merged.inputCount, 1);
        assert.isTrue(merged instanceof SearchQuotientSpur);
        assert.deepEqual(merged.bestExample.text, "biglargetransform");
        assert.deepEqual((merged as SearchQuotientSpur).inputs, [
          { sample: { insert: 'biglargetransform', deleteLeft: 0, id: 11 }, p: 1 }
        ]);
        assert.deepEqual((merged as SearchQuotientSpur).inputSource, originalInput);
        // TODO:  check the 'source' input data (here and in callers)
      }

      it('setup: constructs paths properly', () => {
        const { paths, distributions, splitOriginalInputs: originalInputs } = buildPath();

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
          assert.isTrue(quotientPathHasInputs(p, [distributions[i]]));
        });

        originalInputs.forEach((original) => {
          assert.equal(original.segment.transitionId, originalInputs[0].segment.transitionId);
          assert.equal(original.bestProbFromSet, originalInputs[0].bestProbFromSet);
          assert.equal(original.subsetId, originalInputs[0].subsetId);
        });
      });

      it('merging order:  big + large, then + transform', () => {
        const { originalInput, paths, splitOriginalInputs } = buildPath();

        const headMerge = paths[0].merge(paths[1]);

        // Assertions
        assert.equal(headMerge.inputCount, 1);
        assert.isTrue(headMerge instanceof SearchQuotientSpur);
        assert.deepEqual(headMerge.bestExample.text, "biglarge");
        assert.deepEqual((headMerge as SearchQuotientSpur).inputs, [
          { sample: { insert: 'biglarge', deleteLeft: 0, id: 11 }, p: 1 }
        ]);
        assert.deepEqual((headMerge as SearchQuotientSpur).inputSource, {
          ...originalInput,
          segment: {
            ...splitOriginalInputs[0].segment,
            end: splitOriginalInputs[1].segment.end
          }
        });

        const fullMerge = headMerge.merge(paths[2]);
        checkFinalStateAssertions(fullMerge as SearchQuotientSpur, originalInput);
      });

      it('merging order:  large + transform, then + big', () => {
        const { originalInput, paths, splitOriginalInputs } = buildPath();

        const tailMerge = paths[1].merge(paths[2]);

        // Assertions
        assert.equal(tailMerge.inputCount, 1);
        assert.isTrue(tailMerge instanceof SearchQuotientSpur);
        assert.deepEqual(tailMerge.bestExample.text, "largetransform");
        assert.deepEqual((tailMerge as SearchQuotientSpur).inputs, [
          { sample: { insert: 'largetransform', deleteLeft: 0, id: 11 }, p: 1 }
        ]);
        assert.deepEqual((tailMerge as SearchQuotientSpur).inputSource, {
          ...originalInput,
          segment: {
            ...splitOriginalInputs[2].segment,
            start: splitOriginalInputs[1].segment.start
          }
        });

        const fullMerge = paths[0].merge(tailMerge);
        checkFinalStateAssertions(fullMerge as SearchQuotientSpur, originalInput);
      });
    });

    // Covers many common aspects of SearchQuotientSpur merging, though not merging of
    // multi-member distributions.
    describe(`previously-split token comprised of complex, rewriting transforms:  cello`, () => {
      const buildPath = (inputs: Distribution<Transform>[], sources: PathInputProperties[], root?: SearchQuotientNode) => {
        return inputs.reduce((path, input, index) => new LegacyQuotientSpur(path, input, sources[index]), root ?? new LegacyQuotientRoot(testModel));
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

        const trueInputSources: PathInputProperties[] = trueDistributions.map((d) => {
          return {
            segment: {
              start: 0,
              trueTransform: d[0].sample,
              transitionId: d[0].sample.id
            },
            bestProbFromSet: d[0].p,
            subsetId: generateSubsetId()
          }
        });

        const commonRoot = new LegacyQuotientRoot(testModel);
        const mergeTarget = buildPath(trueDistributions, trueInputSources, commonRoot);

        // Index:  the position of the split.
        const splits: [SearchQuotientNode, SearchQuotientNode][] = [];

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
            {...trueInputSources[1], segment: {...trueInputSources[1].segment, start: 0}},
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
            {...trueInputSources[2], segment: {...trueInputSources[2].segment, start: 0}},
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
            {...trueInputSources[2], segment: {...trueInputSources[2].segment, start: 1}},
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
            {...trueInputSources[3], segment: {...trueInputSources[3].segment, start: 0}},
          ])
        ]);

        // Case 5: the split happens at the token's end, leaving the tail
        // as a fresh, empty token.
        splits.push([
          buildPath(trueDistributions, trueInputSources, commonRoot),
          new LegacyQuotientRoot(testModel)
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

        const remergedPath = splitToTest[0].merge(splitToTest[1]) as SearchQuotientSpur;

        assert.deepEqual(remergedPath.bestExample, mergeTarget.bestExample);
        assert.equal(remergedPath.inputCount, mergeTarget.inputCount);
        assert.equal(remergedPath.codepointLength, mergeTarget.codepointLength);
        assert.sameDeepOrderedMembers(remergedPath.inputSegments, mergeTarget.inputSegments);
        assert.isTrue(quotientPathHasInputs(remergedPath, trueDistributions));
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

      it('merges tokens previously split at index 0', () => {
        runCommonAssertions(0);
      });

      it('merges tokens previously split at index 1', () => {
        runCommonAssertions(1);
      });

      it('merges tokens previously split at index 2', () => {
        runCommonAssertions(2);
      });

      it('merges tokens previously split at index 3', () => {
        runCommonAssertions(3);
      });

      it('merges tokens previously split at index 4', () => {
        runCommonAssertions(4);
      });

      it('merges tokens previously split at index 5', () => {
        runCommonAssertions(5);
      });
    });

    // Same as the prior set, but now with non-BMP text!
    describe(`previously-split token comprised of complex, rewriting non-BMP transforms`, () => {
      const buildPath = (inputs: Distribution<Transform>[], sources: PathInputProperties[], root?: SearchQuotientNode) => {
        return inputs.reduce((path, input, index) => new LegacyQuotientSpur(path, input, sources[index]), root ?? new LegacyQuotientRoot(testModel));
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

        const trueInputSources: PathInputProperties[] = trueDistributions.map((d) => {
          return {
            segment: {
              start: 0,
              trueTransform: d[0].sample,
              transitionId: d[0].sample.id
            },
            bestProbFromSet: d[0].p,
            subsetId: generateSubsetId()
          }
        });

        const commonRoot = new LegacyQuotientRoot(testModel);
        const mergeTarget = buildPath(trueDistributions, trueInputSources, commonRoot);

        // Index:  the position of the split.
        const splits: [SearchQuotientNode, SearchQuotientNode][] = [];

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
            {...trueInputSources[1], segment: {...trueInputSources[1].segment, start: 0}},
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
            {...trueInputSources[2], segment: {...trueInputSources[2].segment, start: 0}},
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
            {...trueInputSources[2], segment: {...trueInputSources[2].segment, start: 1}},
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
            {...trueInputSources[3], segment: {...trueInputSources[3].segment, start: 0}},
          ])
        ]);

        // Case 5: the split happens at the token's end, leaving the tail
        // as a fresh, empty token.
        splits.push([
          buildPath(trueDistributions, trueInputSources, commonRoot),
          new LegacyQuotientRoot(testModel)
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

        const remergedPath = splitToTest[0].merge(splitToTest[1]) as SearchQuotientSpur;

        assert.deepEqual(remergedPath.bestExample, mergeTarget.bestExample);
        assert.equal(remergedPath.inputCount, mergeTarget.inputCount);
        assert.equal(remergedPath.codepointLength, mergeTarget.codepointLength);
        assert.sameDeepOrderedMembers(remergedPath.inputSegments, mergeTarget.inputSegments);
        assert.isTrue(quotientPathHasInputs(remergedPath, trueDistributions));
        assert.isTrue(remergedPath.isSameNode(mergeTarget));
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

      it('merges tokens previously split at index 0', () => {
        runCommonAssertions(0);
      });

      it('merges tokens previously split at index 1', () => {
        runCommonAssertions(1);
      });

      it('merges tokens previously split at index 2', () => {
        runCommonAssertions(2);
      });

      it('merges tokens previously split at index 3', () => {
        runCommonAssertions(3);
      });

      it('merges tokens previously split at index 4', () => {
        runCommonAssertions(4);
      });

      it('merges tokens previously split at index 5', () => {
        runCommonAssertions(5);
      });
    });

    it('correctly merges paths previously split mid-input', () => {
      let path: SearchQuotientNode = new LegacyQuotientRoot(testModel);
      const startSample = {sample: { insert: 'a', deleteLeft: 0 }, p: 1}
      path = new LegacyQuotientSpur(path, [startSample], startSample);

      const inputDistribution = [
        {sample: { insert: 'four', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.4},
        {sample: { insert: 'then', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.3},
        {sample: { insert: 'nine', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.2},
        {sample: { insert: 'what', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.06},
        {sample: { insert: 'cent', deleteLeft: 1, deleteRight: 0, id: 42 }, p: 0.04}
      ];

      const mergeTarget = new LegacyQuotientSpur(path, inputDistribution, inputDistribution[0]);
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
      const headPath = new LegacyQuotientSpur(
        path, headDistributionSplit, {
          segment: {
            start: 0,
            transitionId: inputDistribution[0].sample.id
          },
          bestProbFromSet: inputDistribution[0].p,
          subsetId: mergeTarget.inputSource.subsetId
        }
      );

      const tailDistributionSplit = [
        {sample: { insert: 'ur', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.4},
        {sample: { insert: 'en', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.3},
        {sample: { insert: 'ne', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.2},
        {sample: { insert: 'at', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.06},
        {sample: { insert: 'nt', deleteLeft: 0, deleteRight: 0, id: 42 }, p: 0.04}
      ];
      const tailPath = new LegacyQuotientSpur(
        new LegacyQuotientRoot(testModel), tailDistributionSplit, {
          segment: {
            start: 2,
            transitionId: inputDistribution[0].sample.id
          },
          bestProbFromSet: inputDistribution[0].p,
          subsetId: mergeTarget.inputSource.subsetId
        }
      );

      const remerged = headPath.merge(tailPath);

      assert.deepEqual(remerged.bestExample, mergeTarget.bestExample);
      assert.equal(remerged.inputCount, 2);
      assert.isTrue(remerged instanceof SearchQuotientSpur);
      assert.deepEqual((remerged as SearchQuotientSpur).inputs, inputDistribution);
      assert.isTrue(quotientPathHasInputs(remerged, [[startSample], inputDistribution]));
      assert.isTrue(remerged.isSameNode(mergeTarget));
    });
  });
});