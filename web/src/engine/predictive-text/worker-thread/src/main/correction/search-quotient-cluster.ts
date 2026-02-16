/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-20
 *
 * This file defines the predictive-text engine's SearchQuotientCluster class,
 * which is used to manage the search-space(s) for text corrections within the
 * engine.
 */

import { QueueComparator, PriorityQueue } from '@keymanapp/web-utils';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { SearchNode, SearchResult } from './distance-modeler.js';
import { generateSpaceSeed, InputSegment, PathResult, SearchQuotientNode } from './search-quotient-node.js';

const PATH_QUEUE_COMPARATOR: QueueComparator<SearchQuotientNode> = (a, b) => {
  return a.currentCost - b.currentCost;
}

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchQuotientCluster implements SearchQuotientNode {
  private selectionQueue: PriorityQueue<SearchQuotientNode> = new PriorityQueue(PATH_QUEUE_COMPARATOR);
  readonly spaceId: number;

  // We use an array and not a PriorityQueue b/c batch-heapifying at a single point in time
  // is cheaper than iteratively building a priority queue.
  /**
   * This tracks all paths that have reached the end of a viable input-matching path - even
   * those of lower cost that produce the same correction as other paths.
   *
   * When new input is received, its entries are then used to append edges to the path in order
   * to find potential paths to reach a new viable end.
   */
  private completedPaths?: SearchNode[] = [];

  /**
   * Acts as a Map that prevents duplicating a correction-search path if reached
   * more than once.
   */
  protected get processedEdgeSet(): {[pathKey: string]: boolean} {
    return this._processedEdgeSet;
  }

  private _processedEdgeSet?: {[pathKey: string]: boolean} = {};

  /**
   * Provides a heuristic for the base cost at each depth if the best individual
   * input were taken at that level.
   */
  readonly lowestPossibleSingleCost: number;

  /**
   * Constructs a fresh SearchQuotientCluster instance for use in
   * predictive-text correction and suggestion searches.
   * @param inboundPaths
   */
  constructor(inboundPaths: SearchQuotientNode[]) {
    if(inboundPaths.length == 0) {
      throw new Error("SearchQuotientCluster requires an array with at least one SearchQuotientNode");
    }

    let lowestPossibleSingleCost = Number.POSITIVE_INFINITY;
    const firstPath = inboundPaths[0];
    const inputCount = firstPath.inputCount;
    const codepointLength = firstPath.codepointLength;
    const sourceRangeKey = firstPath.sourceRangeKey;

    for(let path of inboundPaths) {
      if(path.inputCount != inputCount || path.codepointLength != codepointLength) {
        throw new Error(`SearchQuotientNode does not share same properties as others in the cluster:  inputCount ${path.inputCount} vs ${inputCount}, codepointLength ${path.codepointLength} vs ${codepointLength}`);
      }

      // If there's a source-range key mismatch - via mismatch in count or in actual ID, we have an error.
      if(path.sourceRangeKey != sourceRangeKey) {
        throw new Error(`SearchQuotientNode does not share the same source identifiers as others in the cluster`);
      }

      lowestPossibleSingleCost = Math.min(lowestPossibleSingleCost, path.lowestPossibleSingleCost);
    }

    this.spaceId = generateSpaceSeed();

    this.lowestPossibleSingleCost = lowestPossibleSingleCost;
    this.completedPaths = inboundPaths.flatMap(p => p.previousResults).map(r => r.node);
    this.selectionQueue.enqueueAll(inboundPaths);

    return;
  }

  public get inputCount(): number {
    return this.selectionQueue.peek()?.inputCount ?? 0;
  }

  public get bestExample(): {text: string, p: number} {
    const bestPrefixes = this.selectionQueue.toArray().map(p => p.bestExample);
    return bestPrefixes.reduce((max, curr) => max.p < curr.p ? curr : max);
  }

  public get parents(): SearchQuotientNode[] {
    return this.selectionQueue.toArray().slice();
  }

  increaseMaxEditDistance() {
    // By extracting the entries from the priority queue and increasing distance outside of it as a batch job,
    // we get an O(N) implementation, rather than the O(N log N) that would result from maintaining the original queue.
    const entries = this.selectionQueue.toArray();

    entries.forEach((path) => path.increaseMaxEditDistance());

    // Since we just modified the stored instances, and the costs may have shifted, we need to re-heapify.
    this.selectionQueue = new PriorityQueue<SearchQuotientNode>(PATH_QUEUE_COMPARATOR, entries.slice());
  }

  /**
   * When true, this indicates that the currently-represented portion of context
   * has fat-finger data available, which itself indicates that the user has
   * corrections enabled.
   */
  get correctionsEnabled(): boolean {
    const paths = this.selectionQueue.toArray();
    // When corrections are disabled, the Web engine will only provide individual Transforms
    // for an input, not a distribution.  No distributions means we shouldn't do corrections.
    return !!paths.find(p => p.correctionsEnabled);
  }

  public get currentCost(): number {
    return this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;
  }

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the selection queue,
   * checks its validity as a correction to the input text, and reports on what
   * sort of result the edge's destination node represents.
   * @returns
   */
  public handleNextNode(): PathResult {
    const bestPath = this.selectionQueue.dequeue();
    const currentResult = bestPath.handleNextNode();
    this.selectionQueue.enqueue(bestPath);

    if(currentResult.type == 'complete') {
      this.completedPaths?.push(currentResult.finalNode);
      currentResult.spaceId = this.spaceId;
    }

    return currentResult;
  }

  public get previousResults(): SearchResult[] {
    return this.completedPaths?.map((n => new SearchResult(n, this.spaceId))) ?? [];
  }

  get model(): LexicalModelTypes.LexicalModel {
    return this.parents[0].model;
  }

  get codepointLength(): number {
    return this.parents[0].codepointLength;
  }

  get inputSegments(): InputSegment[] {
    return this.parents[0].inputSegments;
  };

  /**
   * Gets a compact string-based representation of `inputRange` that
   * maps compatible token source ranges to each other.
   */
  get sourceRangeKey(): string {
    return this.parents[0].sourceRangeKey;
  }

  merge(space: SearchQuotientNode): SearchQuotientNode {
    throw new Error('Method not implemented.');
  }

  split(charIndex: number): [SearchQuotientNode, SearchQuotientNode] {
    throw new Error('Method not implemented.');
  }

  isSameNode(space: SearchQuotientNode): boolean {
    // Easiest cases:  when the instances or their ' `spaceId` matches, we have
    // a perfect match.
    if(this == space || this.spaceId == space.spaceId) {
      return true;
    }

    // If it's falsy or a different SearchSpace type, that's an easy filter.
    if(!space || !(space instanceof SearchQuotientCluster)) {
      return false;
    }

    // We need to check if the parents match.
    // First, is the parent count the same?
    if(this.parents.length != space.parents.length) {
      return false;
    } else if (this.parents.find((path) => !space.parents.find((path2) => path.isSameNode(path2)))) {
      // Done naively in the manner above, checking each pair of nodes, to
      // ensure a match is found for each, is O(N^2).
      //
      // Granted, we shouldn't have _that_ many incoming paths.
      return false;
    }

    return true;
  }
}