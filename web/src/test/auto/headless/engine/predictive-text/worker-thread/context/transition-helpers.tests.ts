/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-04-06.
 *
 * Implements unit tests against helper functions utilized for transitioning the
 * context to new states based on incoming input or applied suggestions.
 */

import { assert } from 'chai';

import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { LexicalModelTypes } from '@keymanapp/common-types';

import {
  buildEdgeWindow,
  ContextToken,
  ContextTokenization,
  generateSubsetId,
  LegacyQuotientRoot,
  LegacyQuotientSpur,
  models,
  PathInputProperties,
  precomputationSubsetKeyer,
  precomputeTransitions,
  TokenizationSubset,
  TokenizationTransitionEdits,
  TransitionEdge,
  transitionTokenizations
} from '@keymanapp/lm-worker/test-index';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

const plainModel = new TrieModel(
  jsonFixture('models/tries/english-1000'),
  {wordBreaker: defaultBreaker}
);

/**
 * Builds the outbound "transition edge" from the specified ContextTokenization
 * derived from the provided input distribution.
 * @param baseTokenization
 * @param inputs
 * @param tokenizedInputs
 * @returns
 */
function buildOutboundTransitionEdge (
  baseTokenization: ContextTokenization,
  inputs: Distribution<Required<Transform>>,
  tokenizedInputs: Distribution<Map<number, Transform>>
): TransitionEdge {
  const primaryTokenizedInput = tokenizedInputs[0].sample;
  const relativeTailIndex = [...primaryTokenizedInput.keys()][0];

  const tokens = baseTokenization.tokens;
  const baseTexts = tokens.map((t) => t.exampleInput);
  const tail = relativeTailIndex <= 0 ? tokens[tokens.length - 1 + relativeTailIndex] : ContextToken.fromRawText(plainModel, '', true);
  // Do NOT include incoming 'insert' text - it's not "retokenized", as it
  // wasn't in the original token.
  const newTailText = tail.exampleInput.substring(0, tail.exampleInput.length - primaryTokenizedInput.get(relativeTailIndex).deleteLeft);

  const edgeWindow = buildEdgeWindow(baseTokenization.tokens, inputs[0].sample, false);
  return {
    alignment: {
      merges: [],
      splits: [],
      unmappedEdits: [],
      edgeWindow: {
        ...edgeWindow,
        retokenization: baseTexts.slice(edgeWindow.sliceIndex, baseTexts.length - 1).concat(newTailText)
      },
      removedTokenCount: 0
    },
    inputs: tokenizedInputs,
    inputSubsetId: generateSubsetId()
  };
}

/**
 * Converts the incoming transform distribution into the most simple and direct
 * "tokenized" version of itself, which assumes that no actual tokenization
 * should occur.
 * @param inputs
 * @returns
 */
function buildTokenizationForSimpleInputs (
  inputs: Distribution<Required<Transform>>
) {
  return inputs.map(({sample, p}) => {
    const map = new Map<number, Transform>();
    // Only the 'insert' and 'deleteLeft' fields are set during transform
    // tokenization at this time.
    map.set(0, { insert: sample.insert, deleteLeft: sample.deleteLeft });
    return { sample: map, p };
  })
};

/**
 * Builds a tuple pairing the non-tokenized and tokenized versions of the input
 * distribution, under the assumption that no actual tokenization should occur.
 * @param inputs
 * @returns
 */
function buildDistTupleForSimpleInputs (
  inputs: Distribution<Required<Transform>>
) {
  return {
    raw: inputs,
    tokenized: buildTokenizationForSimpleInputs(inputs)
  }
};

/**
 * Generates all needed components for tokenization-transition fixtures corresponding to a _single_
 * spur transition from one ContextTokenization to another one.
 * @param srcTokenization
 * @param dists
 * @param inputPropBase
 * @returns
 */
function generateFixtureForTokenizationOutboundTransition (
  srcTokenization: ContextTokenization,
  dists: {
    raw: Distribution<Required<Transform>>
    tokenized: Distribution<Map<number, Transform>>
  }[],
  inputPropBase: PathInputProperties
) {
  return dists.map((dist) => {
    const primaryTokenizedInput = dist.tokenized[0].sample;
    const tokenizationEdge = buildOutboundTransitionEdge(srcTokenization, dist.raw, dist.tokenized);

    // Is only built for use in constructing the subset keys.  We only need data
    // from one of the inputs here.
    const keyable: TokenizationTransitionEdits = {
      tokenizedTransform: primaryTokenizedInput,
      alignment: tokenizationEdge.alignment
    };

    const key = precomputationSubsetKeyer(keyable);
    const subset: TokenizationSubset = { key, transitionEdges: new Map() };

    subset.transitionEdges.set(srcTokenization, tokenizationEdge);

    // Generate full transition for the affected token.
    const inputProp = {
      ...inputPropBase,
      subsetId: tokenizationEdge.inputSubsetId
    }

    const relativeTailIndex = [...primaryTokenizedInput.keys()][0];

    const quotientNodeToExtend = relativeTailIndex <= 0
      ? srcTokenization.tokens[srcTokenization.tokens.length - 1 + relativeTailIndex].searchModule
      : new LegacyQuotientRoot(plainModel);

    const token = new ContextToken(new LegacyQuotientSpur(
      quotientNodeToExtend,
      dist.tokenized.map((m) => {
        return {
          sample: m.sample.get(relativeTailIndex),
          p: m.p
        }
      }),
      inputProp
    ));

    // Assumes it's the final token.
    token.isPartial = true;

    // CURRENTLY NOT DONE:  adding new or replacement tokens for text to be placed after 'quotientNodeToExtend'.

    const transitionedTokenization = new ContextTokenization(
      srcTokenization.tokens.slice(0, srcTokenization.tokens.length - 1 + relativeTailIndex).concat(token),
      tokenizationEdge,
      null
    );

    return {
      /**
       * The first boundary token directly edited by the input's tokenization-transition.
       */
      token,
      /**
       * The tokenization resulting from the modeled SearchQuotientSpur, before
       * it converges into a SearchQuotientCluster with any other tokenizations
       * matching the same subset key.
       */
      transitionedTokenization,
      /**
       * The original input distribution triggering the tokenization-transition.
       */
      dist: dist.raw,
      /**
       * The subset key for the transition, as used to determine inbound convergent spurs
       * that comprise SearchQuotientClusters.
       */
      key,
      /**
       * The expected precomputed subset metadata for this
       * tokenization-transition, as output by `precomputeTransitions`.
       */
      subset,
      /**
       * The PathInputProperties value expected for the tokenization resulting
       * from the modeled spur's transition.  Note, however, that `subsetId` may
       * not match due to how it is generated.
       */
      inputProp
    };
  });
}

/**
 * Generates all actual fixtures used in the tokenization-transition unit tests found below.
 * @returns
 */
function generateFixturesForTransitionSource() {
  const existingTokenTexts = ['I', ' ', 'ate', '', 'at', ' ', 'a', ' ', 'cafe'];
  const cafeTokenization = new ContextTokenization(
    existingTokenTexts.map((t) => ContextToken.fromRawText(plainModel, t, t == 'cafe'))
  );
  cafeTokenization.tail.isPartial = true;

  const distrib1: Distribution<Required<Transform>> = [
    // tokenized form: should not have deleteRight or id fields!
    { sample: { insert: 'é', deleteLeft: 1, deleteRight: 0, id: 11 }, p: 0.6 },
    { sample: { insert: 't', deleteLeft: 0, deleteRight: 0, id: 11 }, p: 0.4 }
  ];
  const inputPropBase1: PathInputProperties = {
    segment: {
      transitionId: 11,
      start: 0
    },
    bestProbFromSet: distrib1[0].p,
    subsetId: 0
  }

  const transitionSet1 = generateFixtureForTokenizationOutboundTransition(cafeTokenization, [
    buildDistTupleForSimpleInputs([distrib1[0]]),
    buildDistTupleForSimpleInputs([distrib1[1]])
  ], inputPropBase1);

  const distrib1Subsets: Map<string, TokenizationSubset> = new Map();
  const distrib1Tokenizations: Map<string, ContextTokenization> = new Map();
  transitionSet1.forEach((tuple) => {
    distrib1Subsets.set(tuple.key, tuple.subset);
    distrib1Tokenizations.set(tuple.key, tuple.transitionedTokenization);
  });

  const distrib2: Distribution<Required<Transform>> = [
    // tokenized form: should not have deleteRight or id fields!
    { sample: { insert: 's', deleteLeft: 0, deleteRight: 0, id: 12 }, p: 0.6 },
    { sample: { insert: 's', deleteLeft: 1, deleteRight: 0, id: 12 }, p: 0.4 }
  ];
  const inputPropBase2: PathInputProperties = {
    segment: {
      transitionId: 12,
      start: 0
    },
    bestProbFromSet: distrib1[0].p,
    subsetId: 0
  }

  const transitionSet2 = transitionSet1.map(
    (t) => generateFixtureForTokenizationOutboundTransition(
      t.transitionedTokenization, [
        buildDistTupleForSimpleInputs([distrib2[0]]),
        buildDistTupleForSimpleInputs([distrib2[1]])
      ],
      inputPropBase2
    )
  );

  const distrib2Subsets: Map<string, TokenizationSubset> = new Map();
  transitionSet2.flat().forEach((tuple) => {
    const oldEntry = distrib2Subsets.get(tuple.key);
    if(!oldEntry) {
      distrib2Subsets.set(tuple.key, tuple.subset)
    } else {
      const combinedSubset: TokenizationSubset = {
        key: oldEntry.key,
        transitionEdges: new Map(oldEntry.transitionEdges)
      };

      tuple.subset.transitionEdges.forEach((value, key) => {
        combinedSubset.transitionEdges.set(key, value);
      });

      distrib2Subsets.set(tuple.key, combinedSubset);
    }
  });

  return {
    cafe: {
      base: cafeTokenization,
      key1: {
        dist: distrib1,
        transitionSet: transitionSet1,
        subsets: distrib1Subsets,
        contextKey: transitionSet1[0].key,
        tokenizations: distrib1Tokenizations
      },
      key2: {
        dist: distrib2,
        transitionSet: transitionSet2,
        subsets: distrib2Subsets,
        contextKey: transitionSet2[0][0].key,
        tokenizations: null as Map<string, ContextTokenization>
      }
    }
  }
}

/**
 * Verifies that a ContextToken result matches the critical properties of
 * mocked, preconstructed instances while ignoring the components that may not
 * match due solely to ID seeding.
 * @param actual
 * @param expected
 * @param msg
 */
function assertMatchingToken(actual: ContextToken, expected: ContextToken, msg: string) {
  assert.deepEqual(actual.inputSegments, expected.inputSegments, msg);
  assert.deepEqual(actual.sourceRangeKey, expected.sourceRangeKey, msg);
  assert.deepEqual(actual.searchModule.codepointLength, expected.searchModule.codepointLength, msg);
  assert.isTrue(actual.searchModule.isSameNode(expected.searchModule), msg);
  assert.equal(actual.isEmptyToken, expected.isEmptyToken, msg);
  assert.equal(actual.isWhitespace, expected.isWhitespace, msg);
  assert.equal(actual.isPartial, expected.isPartial, msg);
}

/**
 * Verifies that a ContextTokenization result matches the critical properties of
 * mocked, preconstructed instances while ignoring the components that may not
 * match due solely to ID seeding.
 * @param actual
 * @param expected
 * @param msg
 */
function assertMatchingTokenization(actual: ContextTokenization, expected: ContextTokenization, msg: string) {
  assert.equal(actual.tokens.length, expected.tokens.length, msg);
  assert.deepEqual(actual.exampleInput, expected.exampleInput, msg);
  assert.deepEqual(actual.transitionEdits, expected.transitionEdits, msg);

  for(let j=0; j < actual.tokens.length; j++) {
    const nestedMsg = `${msg}, token ${j}`;

    assertMatchingToken(actual.tokens[j], expected.tokens[j], nestedMsg);
  }
}

describe('transition-helper fixture setup', () => {
  it('builds mocked data as expected', () => {
    const { cafe } = generateFixturesForTransitionSource();

    assert.equal(cafe.key1.transitionSet.length, 2);
    assert.notEqual(cafe.key1.transitionSet[0].key, cafe.key1.transitionSet[1].key);

    assert.equal(cafe.key1.transitionSet[0].token.exampleInput, 'café');
    assert.equal(cafe.key1.transitionSet[0].dist.length, 1);
    assert.equal(cafe.key1.transitionSet[1].token.exampleInput, 'cafet');
    assert.equal(cafe.key1.transitionSet[1].dist.length, 1);

    assert.equal(cafe.key2.transitionSet.length, 2);
    assert.equal(cafe.key2.transitionSet[0].length, 2);
    assert.equal(cafe.key2.transitionSet[1].length, 2);
    assert.notEqual(cafe.key2.transitionSet[0][0].key, cafe.key2.transitionSet[0][1].key);
    assert.notEqual(cafe.key2.transitionSet[1][0].key, cafe.key2.transitionSet[1][1].key);
    // The distributions are constructed so that the two different start tokenizations
    // may converge for the inputs corresponding to the indices set below.
    assert.equal(cafe.key2.transitionSet[0][0].key, cafe.key2.transitionSet[1][1].key);

    ['cafés', 'cafs', 'cafets', 'cafes'].forEach((text, index) => {
      const i = Math.floor(index / 2);
      const j = index % 2;
      assert.equal(cafe.key2.transitionSet[i][j].token.exampleInput, text);
      assert.equal(cafe.key2.transitionSet[i][j].dist.length, 1);
    })
  });
});

describe('precomputeTransitions', () => {
  it('handles the "cafe" fixture\'s first input correctly', () => {
    const { cafe } = generateFixturesForTransitionSource();

    const precomputation = precomputeTransitions([cafe.base], cafe.key1.dist, precomputationSubsetKeyer);
    assert.sameOrderedMembers([...precomputation.subsets.keys()], [...cafe.key1.subsets.keys()]);

      // Note:  subset id entries won't match due to how they're generated!
    [...cafe.key1.subsets.keys()].forEach((key) => {
      assert.deepEqual(
        [...precomputation.subsets.get(key).transitionEdges.keys()],
        [...cafe.key1.subsets.get(key).transitionEdges.keys()]
      );
      assert.deepEqual([...precomputation.subsets.get(key).transitionEdges.keys()], [cafe.base]);

      // We coerce the id to match what is expected here by overwriting what our method call produced.
      const expectedTransitionEdge = cafe.key1.subsets.get(key).transitionEdges.get(cafe.base);
      const actualTransitionEdge = {
        ...precomputation.subsets.get(key).transitionEdges.get(cafe.base),
        inputSubsetId: expectedTransitionEdge.inputSubsetId
      };

      assert.deepEqual(actualTransitionEdge, expectedTransitionEdge, `error @ subset key ${key}`);
    });
  });

  it('handles the "cafe" fixture\'s second input correctly', () => {
    const { cafe } = generateFixturesForTransitionSource();

    const key1Tokenizations = cafe.key1.transitionSet.map((t) => t.transitionedTokenization);
    const precomputation = precomputeTransitions(
      key1Tokenizations,
      cafe.key2.dist,
      precomputationSubsetKeyer
    );
    assert.sameOrderedMembers([...precomputation.subsets.keys()], [...cafe.key2.subsets.keys()]);

    // Note:  subset id entries won't match due to how they're generated!
    [...cafe.key2.subsets.keys()].forEach((key) => {
      const expectedSubset = cafe.key2.subsets.get(key);
      const actualSubset = precomputation.subsets.get(key);

      const subsetKeys = [...expectedSubset.transitionEdges.keys()];
      // Some destination subsets aren't reachable from all source tokenizations.
      assert.includeDeepMembers(key1Tokenizations, subsetKeys);
      assert.includeDeepMembers(key1Tokenizations, [...actualSubset.transitionEdges.keys()]);

      // We coerce the id to match what is expected here by overwriting what our method call produced.
      subsetKeys.forEach((srcTokenization) => {
        const expectedTransitionEdge = expectedSubset.transitionEdges.get(srcTokenization);
        const actualTransitionEdge = {
          ...actualSubset.transitionEdges.get(srcTokenization),
          inputSubsetId: expectedTransitionEdge.inputSubsetId
        };

        assert.deepEqual(actualTransitionEdge, expectedTransitionEdge, `error @ subset key ${key}`);
      });
    });
  });
});

describe('transitionTokenizations', () => {
  it('handles the "cafe" fixture\'s first input correctly', () => {
    const { cafe } = generateFixturesForTransitionSource();

    const resultTokenizationMap = transitionTokenizations(cafe.key1.subsets, cafe.key1.dist);

    assert.equal(resultTokenizationMap.size, 2);
    for(let key of resultTokenizationMap.keys()) {
      const msg = `Invalid assumption for tokenization with key ${key}`;

      const actual = resultTokenizationMap.get(key);
      const expected = cafe.key1.tokenizations.get(key);

      assertMatchingTokenization(actual, expected, msg);
    }
  });

  // Issue:  we don't yet have converging tokenizations implemented.  That's kind of a prerequisite here.
  it.skip('handles the "cafe" fixture\'s second input correctly', () => {
    const { cafe } = generateFixturesForTransitionSource();

    const resultTokenizationMap = transitionTokenizations(cafe.key2.subsets, cafe.key2.dist);

    assert.equal(resultTokenizationMap.size, 3);
  });
});