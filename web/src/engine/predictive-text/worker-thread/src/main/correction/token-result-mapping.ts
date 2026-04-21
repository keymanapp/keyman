/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-04-02
 *
 * This file defines the type used for tracking critical graph-search properties
 * utilized during correction-search by the `getBestMatches` algorithm when run
 * against individual tokens / words.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';

import { SearchNode, TraversableToken } from "./distance-modeler.js";

import Distribution = LexicalModelTypes.Distribution;
import LexiconTraversal = LexicalModelTypes.LexiconTraversal;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

export function initTokenResultFilterer() {
  const priorReturns: Map<string, TokenResultMapping> = new Map();

  const closure = (searchResult: TokenResultMapping) => {
    if(searchResult.isFullReplacement) {
      // If the entry's 'match' fully replaces the input string, we consider it
      // unreasonable and ignore it.  Also, if we've reached this point...
      // we can(?) assume that everything thereafter is as well.
      return false;
    }

    if((priorReturns.get(searchResult.matchString)?.totalCost ?? Number.MAX_VALUE) > searchResult.totalCost) {
      priorReturns.set(searchResult.matchString, searchResult);

      return true;
    } else {
      return false;
    }
  };

  return closure;
}

export class TokenResultMapping {
  private readonly node: SearchNode;
  readonly spaceId: number;

  constructor(node: SearchNode);
  constructor(mapping: TokenResultMapping, spaceId: number);
  constructor(mappingBase: SearchNode | TokenResultMapping, spaceId?: number) {
    if(!mappingBase) {
      throw new Error("Result-mapping parameters may not be null");
    }

    if(mappingBase instanceof SearchNode) {
      this.node = mappingBase;
      this.spaceId = mappingBase.spaceId;
    } else {
      this.node = mappingBase.node;
      this.spaceId = spaceId;
    }
  }

  get inputSequence(): ProbabilityMass<Transform>[] {
    return this.node.priorInput;
  }

  get matchSequence(): TraversableToken<string>[] {
    return this.node.calculation.matchSequence.map((char, i) => ({key: char, traversal: this.node.matchedTraversals[i+1]}));
  };

  get matchString(): string {
    return this.node.resultKey;
  }

  get editCount(): number {
    return this.node.editCount;
  }

  get isFullReplacement(): boolean {
    return this.node.isFullReplacement;
  }
  /**
   * Gets the number of Damerau-Levenshtein edits needed to reach the node's
   * matchString from the output induced by the input sequence used to reach it.
   *
   * (This is scaled by `SearchSpace.EDIT_DISTANCE_COST_SCALE` when included in
   * `totalCost`.)
   */
  get knownCost(): number {
    return this.node.editCount;
  }

  /**
   * Gets the "input sampling cost" of the edge, which should be considered as the
   * negative log-likelihood of the input path taken to reach the node.
   */
  get inputSamplingCost(): number {
    return this.node.inputSamplingCost;
  }

  /**
   * Gets the "total cost" of the edge, which should be considered as the
   * negative log-likelihood of the input path taken to reach the node
   * multiplied by the 'probability' induced by needed Damerau-Levenshtein edits
   * to the resulting output.
   */
  get totalCost(): number {
    return this.node.currentCost;
  }

  get finalTraversal(): LexiconTraversal {
    return this.node.currentTraversal;
  }

  buildInsertionEdges(): SearchNode[] {
    return this.node.buildInsertionEdges();
  }

  buildDeletionEdges(dist: Distribution<Transform>, edgeId: number): SearchNode[] {
    return this.node.buildDeletionEdges(dist, edgeId);
  }

  buildSubstitutionEdges(dist: Distribution<Transform>, edgeId: number): SearchNode[] {
    return this.node.buildSubstitutionEdges(dist, edgeId);
  }
}