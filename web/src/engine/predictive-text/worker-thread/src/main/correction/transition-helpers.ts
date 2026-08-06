/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-18
 *
 * Implements helper functions for transitioning the context to new states
 * based on incoming input or applied suggestions.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';

import { ContextToken } from './context-token.js';
import { ContextTokenization } from './context-tokenization.js';
import { SearchQuotientCluster } from './search-quotient-cluster.js';
import { InsertionQuotientSpur } from './insertion-quotient-spur.js';
import { precomputationSubsetKeyer, TokenizationSubset, TokenizationSubsetBuilder, TransitionEdge } from './tokenization-subsets.js';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;

/**
 * Given a set of initial tokenizations and a distribution of input transforms,
 * this method determines the resulting tokenization patterns each initial
 * tokenization will transition into.
 *
 * This can be a many-to-many relationship among initial and final tokenizations
 * if there are sufficient variations in the input Transform distribution and
 * its constituents' effects on the represented context variants.
 * @param startTokenizations
 * @param transformDistribution
 * @param baseDisplayKey The .clusteringKey value of the ContextTokenization
 * matching the context seen by the user
 * @param mayCorrect When set to false, disables corrections by blocking
 * generation of corrective edit-operation spurs.
 *
 * Designed/exposed for use in unit tests.
 * @param keyer Designed for use in unit tests.
 * @returns
 */
export function precomputeTransitions(
  startTokenizations: ContextTokenization[],
  transformDistribution: Distribution<Transform>,
  baseDisplayKey: string,
  mayCorrect: boolean,
  keyer?: typeof precomputationSubsetKeyer
): {
  /**
   * A Map whose values contain metadata useful for constructing the variations
   * in context (and the tokenizations that model each) that will result from
   * the currently-considered context variations and input.
   */
  subsets: Map<string, TokenizationSubset>,
  /**
   * The key matching the resulting context variation that will match the actual
   * context edited by the user.
   */
  keyMatchingUserContext: string
  } {
  keyer ??= precomputationSubsetKeyer;

  let keyMatchingUserContext: string;
  const trueInput = transformDistribution[0].sample;
  const lexicalModel = startTokenizations[0]?.tail.searchModule.model;

  const subsetBuilder = new TokenizationSubsetBuilder(keyer);

  for(let baseTokenization of startTokenizations) {
    for(let mass of transformDistribution) {
      const tokenizationAnalysis = baseTokenization.mapWhitespacedTokenization(lexicalModel, mass.sample);
      const alignment = tokenizationAnalysis.alignment;

      /* Pre-process any splits and merges; the result of these operations may
       * have the same properties as other base tokenizations within the subset
       * if compatible.
       *
       * Example case:  suppose the following two variations of context (forced
       * as they may be):
       * - [can, '] + t
       * - [cans] + t
       *
       * Both have the same total codepoint length and incoming input, but the
       * tokenization pattern is different.  But, if we pre-merge the first
       * version...
       * - [can'] + t
       * - [cans] + t
       *
       * Both land with the same tokenization pattern:
       * - [can't]
       * - [canst]
       *     - 5 total codepoints
       *     - same total count of input transforms
       *     - just one token for both cases.
       *
       * These two cases should converge within the same tokenization-pattern
       * "cluster"... and the key built from 'sourceTokenization' below (during
       * the .addPrecomputation call) is the tool used internally to recognize
       * this.
       */
      const needsRealignment = (alignment.merges.length > 0 || alignment.splits.length > 0 || alignment.unmappedEdits.length > 0);
      const sourceTokenization = needsRealignment ? baseTokenization.realign(alignment) : baseTokenization;

      subsetBuilder.addPrecomputation(sourceTokenization, tokenizationAnalysis, mass.p, mayCorrect);

      if(mass.sample == trueInput && baseDisplayKey == baseTokenization.clusteringKey) {
        keyMatchingUserContext = subsetBuilder.keyer(tokenizationAnalysis);
      }
    }

    if(mayCorrect) {
      // Also prep for a 'delete' transition:  no change to text, but handles the key anyway.
      const tokenizationAnalysis = baseTokenization.mapWhitespacedTokenization(lexicalModel, {insert: '', deleteLeft: 0});
      // The `null` case indicates the 'delete' transition.
      tokenizationAnalysis.tokenizedTransform = null;
      subsetBuilder.addPrecomputation(baseTokenization, tokenizationAnalysis, 0, mayCorrect);
    }
  }

  return {
    subsets: new Map(subsetBuilder.subsets),
    keyMatchingUserContext
  };
}

/**
 * Given results from `precomputeTransitions`, this function generates the
 * context variations that should result from the current context variants and
 * input.
 * @param precomputedTransitionSubsets
 * @param transformDistribution
 * @returns
 */
export function transitionTokenizations(
  precomputedTransitionSubsets: ReadonlyMap<string, TokenizationSubset>,
  transformDistribution: Distribution<Transform>
) {
  const trueInput = transformDistribution[0].sample;
  const bestProb = transformDistribution.reduce((best, curr) => Math.max(best, curr.p), 0);

  // For all target tokenizations - each transition subset...
  const intermediateTokenizations: Map<string, ContextTokenization[]> = new Map();
  precomputedTransitionSubsets.forEach((subset, key) => {
    // Iterate over all _source_ tokenizations and the changes used to transition them
    // to that target tokenization.
    const performTransitions = (precomp: [ContextTokenization, TransitionEdge]) => {
      const rootTokenization = precomp[0];

      // Following call:  is actually designed to build SubstitutionQuotientSpurs.
      const transitionedTokenization = rootTokenization.evaluateTransition(precomp[1], trueInput.id, bestProb);

      // If the last token is empty and has no flag for a revertable transition,
      // attempt to copy the previous token's revertable transition flag.
      const tokens = transitionedTokenization.tokens;
      const lastTokenIndex = tokens.length - 1;
      if(tokens[lastTokenIndex].isEmptyToken && tokens[lastTokenIndex-1]) {
        tokens[lastTokenIndex].appliedTransitionId ??= tokens[lastTokenIndex-1].appliedTransitionId
      }

      return transitionedTokenization;
    }

    const deletionSets = [...subset.deletionEdges.entries()];
    const substitutionSets = [...subset.substitutionEdges.entries()];
    const independentTransitionResults = substitutionSets.flatMap(performTransitions).concat(deletionSets.flatMap(performTransitions));

    intermediateTokenizations.set(key, independentTransitionResults);
  });

  // Process 'insert' edits (which may be clustered with current intermediate
  // ones) and finalize tokenizations.
  const baseToInsertMap: Map<string, string> = new Map();
  const insertToBaseMap: Map<string, string> = new Map();
  const outputTokenizationKeys: Set<string> = new Set();

  const mergedTokenizations: Map<string, ContextTokenization> = new Map();

  for(const subsetPairing of precomputedTransitionSubsets) {
    const [baseKey, subset] = subsetPairing;
    const insertionKeys = subset.insertEdgeKeys;

    outputTokenizationKeys.add(baseKey);
    if(insertionKeys[0] == '') {
      continue;
    }

    baseToInsertMap.set(baseKey, insertionKeys[0]);
    insertToBaseMap.set(insertionKeys[0], baseKey);

    baseToInsertMap.set(insertionKeys[0], insertionKeys[1]);
    insertToBaseMap.set(insertionKeys[1], insertionKeys[0]);

    outputTokenizationKeys.add(insertionKeys[0]);
    outputTokenizationKeys.add(insertionKeys[1]);
  }

  const completableTokenizationKeySet = new Set(outputTokenizationKeys);
  for(const key of insertToBaseMap.keys()) {
    completableTokenizationKeySet.delete(key);
  }

  if(completableTokenizationKeySet.size == 0) {
    throw new Error("Illegal state detected - loops exist in insertion-spur generation logic");
  }

  const completableTokenizationKeys = [...completableTokenizationKeySet];
  while(completableTokenizationKeys.length > 0) {
    const keyToFinalize = completableTokenizationKeys.pop();

    const clusteredTokenizations = intermediateTokenizations.get(keyToFinalize);
    // Super-easy case:  there's only the one tokenization anyway.
    if(clusteredTokenizations.length == 1) {
      mergedTokenizations.set(keyToFinalize, clusteredTokenizations[0]);
    } else {
      mergedTokenizations.set(keyToFinalize, mergeAlignedTokenizations(clusteredTokenizations));
    }

    const insertionKey = baseToInsertMap.get(keyToFinalize);
    if(insertionKey) {
      // Build the new insertion-edit based tokenization variant.
      const baseTokenization = mergedTokenizations.get(keyToFinalize)
      const insertionSpur = new InsertionQuotientSpur(baseTokenization.tail.searchModule);

      const tokens = baseTokenization.tokens.slice(0, -1).concat(new ContextToken(insertionSpur, true));
      const insertedTokenization = new ContextTokenization(tokens);

      const dependentClusterTokenizations = intermediateTokenizations.get(insertionKey) ?? [];
      dependentClusterTokenizations.push(insertedTokenization);
      intermediateTokenizations.set(insertionKey, dependentClusterTokenizations);

      // With the insertion-edit built, check for dependencies on the new
      // tokenization variant and update for future loop rounds.
      baseToInsertMap.delete(keyToFinalize);
      insertToBaseMap.delete(insertionKey);

      if(insertionKey) {
        completableTokenizationKeys.push(insertionKey);
      }
    }
  }

  return mergedTokenizations;
}

/**
 * Given two or more instances of ContextTokenization, this function will
 * attempt to merge each token's SearchQuotientNodes as necessary to result in a
 * single instance.
 *
 * An error will be thrown if the provided nodes do not sufficiently converge to
 * the same tokenization pattern.
 * @param tokenizations
 * @returns
 */
export function mergeAlignedTokenizations(tokenizations: ContextTokenization[]): ContextTokenization {
  const finalizedTokens: ContextToken[] = [];

  // Iterate through the token indices as long as at least one tokenization
  // remains with that index.
  //
  // Assumption:  all have at least one token.
  for(
    let i = 0;
    tokenizations.length > 0;
    // Two co-related iteration steps.  Could be expressed as just one with ++i in functor conditional.
    i++, tokenizations = tokenizations.filter((tokenization) => tokenization.tokens.length > i)
  ) {
    const bucket: ContextToken[] = [];

    tokenizations.forEach((tokenization) => {
      // Check for matches already within the bucket.
      const token = tokenization.tokens[i];

      if(!bucket.find((t) => t.searchModule.isSameNode(token.searchModule))) {
        bucket.push(token);
      }
    });

    if(bucket.length == 1) {
      finalizedTokens.push(bucket[0]);
    } else {
      const constituentSpurs = bucket.flatMap((token) => {
        const quotientNode = token.searchModule;

        if(quotientNode instanceof SearchQuotientCluster) {
          return quotientNode.parents;
        } else {
          return quotientNode;
        }
      });

      // Will throw an error (as documented in this function's description) if
      // the tokens' spurs obtained in `constituentSpurs` do not actually
      // represent the same input range, and thus do not have matching
      // wordbreaking boundaries - a state that invalidates this method's
      // stated preconditions.
      finalizedTokens.push(new ContextToken(new SearchQuotientCluster(constituentSpurs), i == tokenizations.length - 1));
    }
  }

  return new ContextTokenization(finalizedTokens);
}