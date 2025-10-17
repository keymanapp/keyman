/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-09
 *
 * This file the predictive-text engine's SearchSpace class, which is used to
 * manage the search-space(s) for text corrections within the engine.
 */

import { SearchNode, SearchResult } from "./distance-modeler.js";

export let SPACE_ID_SEED = 0;

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

export interface SearchBatcher {
  /**
   * Returns an identifier uniquely identifying this search-batching structure
   * by correction-search results.
   */
  readonly spaceId: number;

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the batcher's search
   * area, checks its validity as a correction to the input text, and reports on
   * what sort of result the edge's destination node represents.
   * @returns
   */
  handleNextNode(): PathResult;

  /**
   * Reports the cost of the lowest-cost / lowest-distance edge held within the
   * batcher's search area.
   * @returns
   */
  readonly currentCost: number;

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
   * Reports the number of input keystrokes (and fat-finger alternatives)
   * included by paths output by this batcher's correction-searching processes.
   */
  readonly inputCount: number;

  /**
   * Determines the best example text representable by this batcher's portion of
   * the correction-search graph and its paths.
   */
  readonly bestExample: { text: string, p: number };
}