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

import LexiconTraversal = LexicalModelTypes.LexiconTraversal;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

export function initTokenResultFilterer() {
  const priorReturns: Map<string, SearchNode> = new Map();

  const closure = (searchResult: TokenResultMapping) => {
    const node = searchResult.node;

    if(node.isFullReplacement) {
      // If the entry's 'match' fully replaces the input string, we consider it
      // unreasonable and ignore it.  Also, if we've reached this point...
      // we can(?) assume that everything thereafter is as well.
      return false;
    }

    if((priorReturns.get(node.resultKey)?.currentCost ?? Number.MAX_VALUE) >= searchResult.totalCost) {
      priorReturns.set(node.resultKey, node);

      return true;
    } else {
      return false;
    }
  };

  return closure;
}

export class TokenResultMapping {
  readonly node: SearchNode;

  constructor(node: SearchNode) {
    this.node = node;
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
}