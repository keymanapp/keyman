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
export abstract class SearchQuotientNode {
  /**
   * Holds all `incomingNode` child buffers - buffers to hold nodes processed by
   * this SearchCluster but not yet by child SearchSpaces.
   */
  private childQueues: SearchNode[][] = [];

  /**
   * Marks all results that have already been returned from this instance of SearchPath.
   * Should be deleted and cleared if any paths consider this one as a parent.
   */
  private returnedValues?: {[resultKey: string]: SearchNode} = {};


  // The TS type system prevents this method from being rooted on the instance provided in
  // the first parameter, sadly.
  /**
   * Links the provided queueing buffer to the provided parent node.  When the
   * parent produces new intermediate results, those results will be made
   * available for use in construction of extended paths.
   * @param parentNode
   * @param childQueue
   */
  protected linkAndQueueFromParent(parentNode: SearchQuotientNode, childQueue: SearchNode[]): void {
    parentNode.childQueues.push(childQueue);
  }

  /**
   * Log the results of a processed node and queue it within all subscribed
   * processor nodes for construction of deeper search paths.
   * @param node
   */
  protected saveResult(node: SearchNode): boolean {
    const priorMatch = this.returnedValues[node.resultKey];
    if(priorMatch !== undefined && priorMatch.currentCost < node.currentCost) {
      return false;
    }

    this.returnedValues[node.resultKey] = node;
    this.childQueues.forEach((buf) => buf.push(node));
    return true;
  }

  /**
   * Returns the set of existing, completed search-results with this node's domain.
   */
  public get previousResults(): SearchResult[] {
    return Object.values(this.returnedValues ?? {}).map(v => new SearchResult(v));
  }

  // -- Everything after this is abstract and implemented by derived child classes.

  /**
   * Returns an identifier uniquely identifying this search-batching structure
   * by correction-search results.
   */
  abstract get spaceId(): number;

  /**
   * The active LexicalModel for use with correction-search.
   */
  abstract get model(): LexicalModel;

  /**
   * Notes the SearchQuotientNode(s) whose correction-search paths are extended by this
   * SearchQuotientNode.
   */
  abstract get parents(): SearchQuotientNode[];

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the batcher's search
   * area, checks its validity as a correction to the input text, and reports on
   * what sort of result the edge's destination node represents.
   * @returns
   */
  abstract handleNextNode(): PathResult;

  /**
   * Increases the editing range that will be considered for determining
   * correction distances.
   */
  abstract increaseMaxEditDistance(): void;

  /**
   * Reports the cost of the lowest-cost / lowest-distance edge held within the
   * batcher's search area.
   * @returns
   */
  abstract get currentCost(): number;

  /**
   * Provides a heuristic for the base cost at this path's depth if the best
   * individual input were taken here, regardless of whether or not that's
   * possible.
   *
   * This cost is based on the negative log-likelihood of the probability and
   * includes the cost from the lowest possible parent nodes visited.
   */
  abstract readonly lowestPossibleSingleCost: number;

  /**
   * When true, this indicates that the currently-represented portion of context
   * has fat-finger data available, which itself indicates that the user has
   * corrections enabled.
   */
  abstract readonly correctionsEnabled: boolean;

  /**
   * Reports the total number of input keystrokes represented by this
   * graph/subgraph.
   *
   * (Their fat-finger alternates, when provided, do not influence this count -
   * they're associated with the original keystroke that affected the context.)
   */
  abstract readonly inputCount: number;

  /**
   * Reports the length in codepoints of corrected text represented by completed
   * paths from this instance.
   */
  abstract readonly codepointLength: number;

  /**
   * Determines the best example text representable by this SearchQuotientNode's
   * portion of the correction-search graph and its paths.
   */
  abstract readonly bestExample: { text: string, p: number };

  /**
   * Gets components representing the keystroke range corrected by this
   * search-space quotient node.   If only part of any keystroke's effects are
   * used, this will also be noted.
   */
  abstract readonly inputSegments: InputSegment[];

  /**
   * Gets a compact string-based representation of `inputRange` that
   * maps compatible token source ranges to each other.
   */
  abstract get sourceRangeKey(): string;

  /**
   * Appends this SearchQuotientNode with the provided SearchQuotientNode's search properties,
   * extending the represented search range accordingly.  If this operation
   * represents merging the result of a previous .split() call, the two halves
   * of any split input components will be fully re-merged.
   * @param space
   */
  abstract merge(space: SearchQuotientNode): SearchQuotientNode;

  /**
   * Splits this SearchQuotientNode into two halves at the specified codepoint index.
   * The 'head' component will maximally re-use existing cached data, while the
   * 'tail' must be reconstructed from scratch due to the new start position.
   *
   * It is possible that there are multiple distinct ways to split the
   * SearchSpace into halves if the split is not consistently clean (between
   * input boundaries) for all possible path-sequences modeled by the original
   * SearchSpace instance.
   * @param charIndex
   */
  abstract split(charIndex: number): [SearchQuotientNode, SearchQuotientNode][];

  /**
   * Determines if the SearchQuotientNode is a duplicate of another instance.
   * For such cases, the total search space covered by the quotient-graph
   * path(s) taken to reach each must be 100% identical.
   * @param node
   */
  abstract isSameNode(node: SearchQuotientNode): boolean;
}