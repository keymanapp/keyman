/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-09
 *
 * This file defines the predictive-text engine's SearchSpace class, which is used to
 * manage the search-space(s) for text corrections within the engine.
 */

import { LexicalModelTypes } from "@keymanapp/common-types";

import { SearchNode, SearchResult } from "./distance-modeler.js";

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;

let SPACE_ID_SEED = 0;

export function generateSpaceSeed(): number {
  return SPACE_ID_SEED++;
}

type NullPath = {
  type: 'none'
}

type IntermediateSearchPath = {
  type: 'intermediate',
  cost: number
}

type CompleteSearchPath = {
  type: 'complete',
  cost: number,
  finalNode: SearchNode,
  spaceId: number
}

export type PathResult = NullPath | IntermediateSearchPath | CompleteSearchPath;

/**
 * Represents all or a portion of the dynamically-generated graph used to search
 * for predictive-text corrections.
 */
export interface SearchQuotientNode {
  /**
   * Returns an identifier uniquely identifying this search-batching structure
   * by correction-search results.
   */
  readonly spaceId: number;

  /**
   * Notes the SearchQuotientNode(s) whose correction-search paths are extended
   * by this SearchQuotientNode.
   */
  readonly parents: SearchQuotientNode[];

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the batcher's search
   * area, checks its validity as a correction to the input text, and reports on
   * what sort of result the edge's destination node represents.
   * @returns
   */
  handleNextNode(): PathResult;

  /**
   * Denotes whether or not the represented search space includes paths built from
   * the specified set of keystroke input distributions.  The distribution count
   * should match .inputCount - no omissions or extras are permitted.
   *
   * Designed explicitly for use in unit testing; it's not super-efficient, so
   * avoid live use.
   *
   * @param keystrokeDistributions
   * @internal
   */
  hasInputs(keystrokeDistributions: Distribution<Transform>[]): boolean;

  /**
   * Increases the editing range that will be considered for determining
   * correction distances.
   */
  increaseMaxEditDistance(): void;

  /**
   * Reports the cost of the lowest-cost / lowest-distance edge held within the
   * batcher's search area.
   * @returns
   */
  readonly currentCost: number;

  /**
   * Provides a heuristic for the base cost at this path's depth if the best
   * individual input were taken here, regardless of whether or not that's
   * possible.
   *
   * This cost is based on the negative log-likelihood of the probability and
   * includes the cost from the lowest possible parent nodes visited.
   */
  readonly lowestPossibleSingleCost: number;

  /**
   * Returns the set of previously-processed results under this batcher's domain.
   */
  readonly previousResults: SearchResult[];

  /**
   * When true, this indicates that the currently-represented portion of context
   * has fat-finger data available, which itself indicates that the user has
   * corrections enabled.
   */
  readonly correctionsEnabled: boolean;

  /**
   * Reports the total number of input keystrokes represented by this
   * graph/subgraph.
   *
   * (Their fat-finger alternates, when provided, do not influence this count -
   * they're associated with the original keystroke that affected the context.)
   */
  readonly inputCount: number;

  /**
   * Retrieves the sequence of inputs that led to this SearchSpace.
   *
   * THIS WILL BE REMOVED SHORTLY.  (Once SearchQuotientNode takes on merging &
   * splitting)
   */
  readonly inputSequence: Distribution<Transform>[];

  /**
   * Determines the best example text representable by this batcher's portion of
   * the correction-search graph and its paths.
   */
  readonly bestExample: { text: string, p: number };
}