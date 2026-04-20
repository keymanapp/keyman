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
import { legacySubsetKeyer, TokenizationSubset, TokenizationSubsetBuilder } from './tokenization-subsets.js';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;

/**
 * Given a set of initial tokenizations and a distribution of input transforms,
 * this method determines the resulting tokenization patterns each initial
 * tokenization will transition into.
 *
 * This can be a many-to-many relationship among initial and final tokenizations
 * if there are sufficient variations in the input Transform distribution and its
 * constituents' effects on the represented context variants.
 * @param startTokenizations
 * @param transformDistribution
 * @param keyer Designed for use in unit tests.
 * @returns
 */
export function precomputeTransitions(
  startTokenizations: ContextTokenization[],
  transformDistribution: Distribution<Transform>,
  keyer?: typeof legacySubsetKeyer
): {
  /**
   * A Map whose values contain metadata useful for constructing the variations
   * in context (and the tokenizations that model each) that will result from
   * the currently-considered context variations and input.
   */
  subsets: ReadonlyMap<string, TokenizationSubset>,
  /**
   * The key matching the resulting context variation that will match the actual
   * context edited by the user.
   */
  keyMatchingUserContext: string
  } {
  keyer ??= legacySubsetKeyer;

  let keyMatchingUserContext: string;
  const trueInput = transformDistribution[0].sample;
  const lexicalModel = startTokenizations[0]?.tail.searchModule.model;

  const subsetBuilder = new TokenizationSubsetBuilder(keyer);
  for(let baseTokenization of startTokenizations) {
    for(let mass of transformDistribution) {
      const tokenizationAnalysis = baseTokenization.mapWhitespacedTokenization(lexicalModel, mass.sample);
      const alignment = tokenizationAnalysis.alignment;

      // Pre-process any splits and merges; the result of these operations may
      // have the same properties as other base tokenizations within the
      // subset if compatible.
      const needsRealignment = (alignment.merges.length > 0 || alignment.splits.length > 0 || alignment.unmappedEdits.length > 0);
      const sourceTokenization = needsRealignment ? baseTokenization.realign(alignment) : baseTokenization;

      subsetBuilder.addPrecomputation(sourceTokenization, tokenizationAnalysis, mass.p);

      if(mass.sample == trueInput) {
        keyMatchingUserContext = subsetBuilder.keyer(tokenizationAnalysis);
      }
    }
  }

  return {
    subsets: subsetBuilder.subsets,
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
  const finalTokenizations: Map<string, ContextTokenization> = new Map();
  precomputedTransitionSubsets.forEach((subset, key) => {
    // Iterate over all _source_ tokenizations and the changes used to transition them
    // to that target tokenization.
    const transitionSets = [...subset.transitionEdges.entries()];
    const independentTransitionResults = transitionSets.flatMap((precomp) => {
      const rootTokenization = precomp[0];

      // Following call:  is actually designed to build SubstitutionQuotientSpurs.
      const transitionedTokenization = rootTokenization.evaluateTransition(precomp[1], trueInput.id, bestProb);
      const remadeTokenization = new ContextTokenization(transitionedTokenization.tokens, subset.transitionEdges.get(rootTokenization), transitionedTokenization.taillessTrueKeystroke);

      // If the last token is empty and has no flag for a revertable transition,
      // attempt to copy the previous token's revertable transition flag.
      const tokens = remadeTokenization.tokens;
      const lastTokenIndex = tokens.length - 1;
      if(tokens[lastTokenIndex].isEmptyToken && tokens[lastTokenIndex-1]) {
        tokens[lastTokenIndex].appliedTransitionId ??= tokens[lastTokenIndex-1].appliedTransitionId
      }

      return remadeTokenization;
    });

    // Super-easy case:  there's only the one tokenization anyway.
    if(independentTransitionResults.length == 1) {
      finalTokenizations.set(key, independentTransitionResults[0])
      return;
    }

    finalTokenizations.set(key, mergeAlignedTokenizations(independentTransitionResults));
  });

  return finalTokenizations;
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
      finalizedTokens.push(new ContextToken(new SearchQuotientCluster(constituentSpurs)));
    }
  }

  return new ContextTokenization(finalizedTokens);
}