/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-20
 *
 * This file defines the predictive-text engine's SearchSpace class, which is used to
 * manage the search-space(s) for text corrections within the engine.
 */

import { QueueComparator as Comparator, PriorityQueue } from '@keymanapp/web-utils';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { SearchNode, SearchResult } from './distance-modeler.js';
import { LegacyQuotientRoot } from './legacy-quotient-root.js';
import { generateSpaceSeed, InputSegment, PathResult, SearchQuotientNode } from './search-quotient-node.js';
import { SearchQuotientSpur } from './search-quotient-spur.js';

const PATH_QUEUE_COMPARATOR: Comparator<SearchQuotientNode> = (a, b) => {
  return a.currentCost - b.currentCost;
}

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchQuotientCluster implements SearchQuotientNode {
  // While most functions can be done directly from SearchSpace, merging and
  // splitting will need access to SearchQuotientSpur-specific members.  It's
  // also cleaner to not allow nested SearchQuotientClusters while we haven't
  // worked out support for such a scenario.
  private selectionQueue: PriorityQueue<SearchQuotientNode> = new PriorityQueue(PATH_QUEUE_COMPARATOR);
  readonly spaceId: number;

  /**
   * Holds all `incomingNode` child buffers - buffers to hold nodes processed by
   * this SearchCluster but not yet by child SearchSpaces.
   */
  private childBuffers: SearchNode[][] = [];

  // We use an array and not a PriorityQueue b/c batch-heapifying at a single
  // point in time is cheaper than iteratively building a priority queue.
  /**
   * This tracks all paths that have reached the end of a viable input-matching
   * path - even those of lower cost that produce the same correction as other
   * paths.
   *
   * When new input is received, its entries are then used to append edges to
   * the path in order to find potential paths to reach a new viable end.
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
   * Provides a heuristic for the base cost at each depth if the best
   * individual input were taken at that level.
   */
  readonly lowestPossibleSingleCost: number;

  /**
   * Constructs a fresh SearchSpace instance for used in predictive-text correction
   * and suggestion searches.
   * @param baseSpaceId
   * @param model
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
    this.selectionQueue = new PriorityQueue(PATH_QUEUE_COMPARATOR, this.selectionQueue.toArray());

    if(currentResult.type == 'complete') {
      this.bufferNode(currentResult.finalNode);
      this.completedPaths?.push(currentResult.finalNode);
      currentResult.spaceId = this.spaceId;
    }

    return currentResult;
  }

  public get previousResults(): SearchResult[] {
    return this.completedPaths?.map((n => new SearchResult(n, this.spaceId))) ?? [];
  }

  public addResultBuffer(nodeBuffer: SearchNode[]): void {
    this.childBuffers.push(nodeBuffer);
  }

  private bufferNode(node: SearchNode) {
    this.childBuffers.forEach((buf) => buf.push(node));
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
    // If we're at a root (which is without inputs), bypass it.
    if(space.parents.length == 0) {
      return this;
    }

    // What if we're trying to merge something previously split?
    // That can only happen at the head of the incoming space, so we check for it early here.
    if(space.inputCount == 1 && space instanceof SearchQuotientSpur) {
      // In such a case... the 'leading edge' of the incoming space needs to be checked
      // against the trailing edge of `this` instance's entries.
      const thisTailInputSource = this.inputSegments[this.inputSegments.length - 1];
      const thisTailSpaceIds = this.parents.map((path) => (path as SearchQuotientSpur).inputSource.subsetId);
      const spaceHeadInputSource = space.inputSegments[0];

      const isOnSplitInput =
        thisTailSpaceIds.find((entry) => entry == space.inputSource.subsetId)
        && thisTailInputSource.end == spaceHeadInputSource.start;

      // In this case, we only rebuild the single path; an outer stack frame will reconstitute
      // the split cluster from the individual paths built here.
      if(isOnSplitInput) {
        const firstHalf = this.parents.find((tailPath) => (tailPath as SearchQuotientSpur).inputSource?.subsetId == space.inputSource?.subsetId);
        return firstHalf.merge(space);
      }
    }

    // Simple, straightforward.  SearchQuotientSpurs can easily built with a
    // SearchQuotientCluster as parent. In this case, there's also no chance of
    // a prior split; if we'd split, it'd be a SearchQuotientCluster on both
    // ends.
    if(space instanceof SearchQuotientSpur) {
      const parentMerge = this.merge(space.parents[0]) as SearchQuotientSpur;
      return space.construct(parentMerge, space.inputs, space.inputSource);
    }

    // If we're here, we have a SearchQuotientCluster being merged in... and to
    // something that's already a SearchQuotientCluster.
    //
    // Merge the parent components first as a baseline.  This specific state's
    // aspects have to come after their affects are merged in, anyway.
    // (Note:  is the main point of recursion.)
    const parents = (space as SearchQuotientCluster).parents; // the constituent paths

    // Assumes either none of the space's heads were split or that ALL were.
    const parentMerges = parents.map((p) => this.merge(p)); // we get paths out
    return new SearchQuotientCluster(parentMerges);
  }

  split(charIndex: number): [SearchQuotientNode, SearchQuotientNode][] {
    // Don't rebuild if this is already a perfect split point!
    if(this.codepointLength <= charIndex) {
      return [[this, new LegacyQuotientRoot(this.model)]];
    }

    const results = this.parents.flatMap((p) => p.split(charIndex));

    // Path-deduplication:  it is possible for paths to diverge after a point
    // and then reconverge at later points.  If the split happens before such
    // a divergence-reconvergence sequence, it is possible for left-hand side
    // entries to be duplicated.
    //
    // Deduplicate clusters based solely on spaceId, though; an intact cluster
    // should match on this basis alone.
    const headClusterResultMap: Map<number, {
      head: SearchQuotientCluster,
      tails: SearchQuotientNode[]
    }> = new Map();

    const headPathResultMap = new Map<string, {
      heads: SearchQuotientNode[],
      tails: SearchQuotientNode[]
    }>();

    results.forEach((result) => {
      const [head, tail] = result;

      if(head instanceof SearchQuotientCluster) {
        const bucket = headClusterResultMap.get(head.spaceId) ?? { head, tails: [] };
        bucket.tails.push(tail);
        headClusterResultMap.set(head.spaceId, bucket);
        return;
      }

      let key = head.inputCount + '+' + (!(head instanceof SearchQuotientSpur) ? '' : head.splitClusteringKey);
      const outerEntry = headPathResultMap.get(key) ?? { heads: [], tails: [] };

      if(!outerEntry.heads.find(p => head.isSameNode(p))) {
        outerEntry.heads.push(head as SearchQuotientSpur);
      }

      outerEntry.tails.push(tail);
      headPathResultMap.set(key, outerEntry);
    });

    const resultsFromClusterHeadPaths = [...headClusterResultMap.values()].map((entry) => {
      const tailSpace = entry.tails.length > 1 ? new SearchQuotientCluster(entry.tails) : entry.tails[0];
      return [entry.head, tailSpace] as [SearchQuotientNode, SearchQuotientNode];
    });

    const resultsFromHeadPaths = [...headPathResultMap.values()].map((entry) => {
      const headSpace = entry.heads.length > 1 ? new SearchQuotientCluster(entry.heads) : entry.heads[0];
      const tailSpace = entry.tails.length > 1 ? new SearchQuotientCluster(entry.tails) : entry.tails[0];
      return [headSpace, tailSpace] as [SearchQuotientNode, SearchQuotientNode];
    });

    return resultsFromClusterHeadPaths.concat(resultsFromHeadPaths);
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

    // We need to check if the parents match.  Done naively in the manner below, this is O(N^2).
    // Granted, we shouldn't have _that_ many incoming paths.
    if(this.parents.find((path) => !space.parents.find((path2) => path.isSameNode(path2)))) {
      return false;
    }

    return true;
  }
}