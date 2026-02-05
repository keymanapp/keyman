/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-09
 *
 * This file defines the predictive-text engine's SearchQuotientNode class, which is used to
 * manage the search-space(s) for text corrections within the engine.
 */

import { LexicalModelTypes } from "@keymanapp/common-types";

import { SearchNode, SearchResult } from "./distance-modeler.js";

import LexicalModel = LexicalModelTypes.LexicalModel;

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

export interface InputSegment {
  /**
   * The transform / transition ID of the corresponding input event.
   */
  transitionId: number | undefined,

  /**
   * Marks the initial index (inclusive) within the insert strings for the
   * corresponding transitions' Transforms that are applied by the corresponding
   * tokenized correction-search input.
   */
  start: number

  /**
   * Marks the final index (exclusive) within the insert strings for the
   * corresponding transitions' Transforms that are applied by the corresponding
   * tokenized correction-search input.
   *
   * If undefined, there is no portion of the input-source transform split from
   * the right-hand side.  Otherwise, this value should match the `start` value of
   * the _next_ split-off component of the input-source.
   */
  end?: number;
}

/**
 * Models the properties and portion of an input event applied by a
 * SearchQuotientNode for correction-search purposes.
 */
export interface PathInputProperties {
  /**
   * Denotes the portion of the ongoing input stream represented by the corresponding
   * input distribution(s) of a SearchQuotientNode.
   */
  segment: InputSegment;

  /**
   * Notes the highest probability found in the input event's transform
   * distribution, regardless of whether or not that specific corresponding
   * input is included within the SearchQuotientNode's correction space.
   */
  bestProbFromSet: number;

  /**
   * A unique identifier noting membership in a specific set of input possibilities with
   * sufficiently similar properties that all correspond to the same "input segment".
   *
   * This tends to serve as an identifying factor for tokenized input distributions,
   * indicating the distributions were all sourced from the same original input event.
   *
   * @see TransitionEdge.inputSubsetId
   */
  subsetId: number;
}

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
   * The active LexicalModel for use with correction-search.
   */
  readonly model: LexicalModel;

  /**
   * Notes the SearchQuotientNode(s) whose correction-search paths are extended by this
   * SearchQuotientNode.
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
   * Reports the length in codepoints of corrected text represented by completed
   * paths from this instance.
   */
  readonly codepointLength: number;

  /**
   * Determines the best example text representable by this SearchQuotientNode's
   * portion of the correction-search graph and its paths.
   */
  readonly bestExample: { text: string, p: number };

  /**
   * Gets components representing the keystroke range corrected by this
   * search-space quotient node.   If only part of any keystroke's effects are
   * used, this will also be noted.
   */
  readonly inputSegments: InputSegment[];

  /**
   * Gets a compact string-based representation of `inputRange` that
   * maps compatible token source ranges to each other.
   */
  get sourceRangeKey(): string;

  /**
   * Appends this SearchQuotientNode with the provided SearchQuotientNode's search properties,
   * extending the represented search range accordingly.  If this operation
   * represents merging the result of a previous .split() call, the two halves
   * of any split input components will be fully re-merged.
   * @param space
   */
  merge(space: SearchQuotientNode): SearchQuotientNode;

  /**
   * Splits this SearchQuotientNode into two halves at the specified codepoint index.
   * The 'head' component will maximally re-use existing cached data, while the
   * 'tail' must be reconstructed from scratch due to the new start position.
   * @param charIndex
   */
  split(charIndex: number): [SearchQuotientNode, SearchQuotientNode];

  /**
   * Determines if the SearchQuotientNode is a duplicate of another instance.
   * For such cases, the total search space covered by the quotient-graph
   * path(s) taken to reach each must be 100% identical.
   * @param node
   */
  isSameNode(node: SearchQuotientNode): boolean;
}