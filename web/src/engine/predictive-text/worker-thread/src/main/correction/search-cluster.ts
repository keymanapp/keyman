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
import { generateSpaceSeed, PathResult, SearchSpace, TokenInputSource } from './search-space.js';
import { SearchPath } from './search-path.js';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;
import { SearchPath } from './search-path.js';

const PATH_QUEUE_COMPARATOR: Comparator<SearchSpace> = (a, b) => {
  return a.currentCost - b.currentCost;
}

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchCluster implements SearchSpace {
  private selectionQueue: PriorityQueue<SearchSpace> = new PriorityQueue(PATH_QUEUE_COMPARATOR);
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
  constructor(inboundPaths: ReadonlyArray<SearchSpace>) {
    if(inboundPaths.length == 0) {
      throw new Error("SearchCluster requires an array with at least one SearchPath");
    }

    let lowestPossibleSingleCost = Number.POSITIVE_INFINITY;
    const inputCount = inboundPaths[0]?.inputCount;
    const transitionIds = inboundPaths[0]?.sourceIdentifiers.map(src => src.trueTransform.id);
    const codepointLength = inboundPaths[0]?.codepointLength;

    for(let path of inboundPaths) {
      if(path.inputCount != inputCount || path.codepointLength != codepointLength) {
        throw new Error(`SearchPath does not share same properties as others in the cluster:  inputCount ${path.inputCount} vs ${inputCount}, codepointLength ${path.codepointLength} vs ${codepointLength}`);
      }

      const pathTransitionIds = new Set(path.sourceIdentifiers.map(src => src.trueTransform.id));
      // If there's an ID mismatch - via mismatch in count or in actual ID, we have an error.
      if(transitionIds.length != pathTransitionIds.size || transitionIds.find(i => !pathTransitionIds.has(i)) !== undefined) {
        throw new Error(`SearchPath does not share the same source identifiers as others in the cluster`);
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

  public hasInputs(keystrokeDistributions: Distribution<Transform>[]): boolean {
    return !!this.parents.find((p) => p.hasInputs(keystrokeDistributions));
  }

  public get bestExample(): {text: string, p: number} {
    const bestPrefixes = this.selectionQueue.toArray().map(p => p.bestExample);
    return bestPrefixes.reduce((max, curr) => max.p < curr.p ? curr : max);
  }

  public get parents(): SearchSpace[] {
    return this.selectionQueue.toArray().slice();
  }

  increaseMaxEditDistance() {
    // By extracting the entries from the priority queue and increasing distance outside of it as a batch job,
    // we get an O(N) implementation, rather than the O(N log N) that would result from maintaining the original queue.
    const entries = this.selectionQueue.toArray();

    entries.forEach((path) => path.increaseMaxEditDistance());

    // Since we just modified the stored instances, and the costs may have shifted, we need to re-heapify.
    this.selectionQueue = new PriorityQueue<SearchSpace>(PATH_QUEUE_COMPARATOR, entries.slice());
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

  public stopTrackingResults() {
    delete this.completedPaths;
  }

  get model(): LexicalModelTypes.LexicalModel {
    return this.parents[0].model;
  }

  get codepointLength(): number {
    return this.parents[0].codepointLength;
  }

  get likeliestSourceText(): string {
    const results = this.parents.map((p) => ({ text: p.likeliestSourceText, p: p.bestExample.p }));
    results.sort((a, b) => b.p - a.p);

    return results[0].text;
  }

  get sourceIdentifiers(): TokenInputSource[] {
    return this.parents[0].sourceIdentifiers;
  };

  merge(space: SearchSpace): SearchSpace {
    // If we're at a root (which is without inputs), bypass it.
    if(space.parents.length == 0) {
      return this;
    }

    // Simple, straightforward.  SearchPaths can easily built with a SearchCluster as parent.
    // In this case, there's also no chance of a prior split; if we'd split, it'd be a
    // SearchCluster on both ends.
    if(space instanceof SearchPath) {
      return new SearchPath(this, space.inputs, space.inputSource);
    }

    // If we're here, we have a SearchCluster being merged in... and to
    // something that's already a SearchCluster.
    //
    // Merge the parent components first as a baseline.  This specific state's
    // aspects have to come after their affects are merged in, anyway.
    // (Note:  is the main point of recursion.)
    const parentMerges = space.parents.map((p) => this.merge(p));

    // Note how SearchCluster.split works - it always makes two SearchClusters.
    // There is a chance that constituent paths were split, and that's something
    // we need to check for.  Fortunately, if that did occur, the components of
    // the split paths should align - we took care of that in .split().  Also,
    // for such cases, the parent count should match, with each inputSource
    // having a match divergent only by the .inputStartIndex.
    //
    // If that did not occur, we simply append the SearchCluster's parents, one
    // at a time, and construct the rebuilt SearchCluster.
    //
    // Possible alternate route:  if a parent exists for each component of the
    // cluster to be merged in.  Not if there's a child for every parent - a
    // parent for every child is what we'd want if we relax restrictions.

    // Note:  we are constructing the full condition over multiple steps here.
    const splitClusterCondition1 = parentMerges.length == space.parents.length;
    const splitClusterCondition2 = splitClusterCondition1 && !parentMerges.find((p1, i) => {
      const p2 = space.parents[i];
      // If we somehow have a nested SearchCluster, we don't try to do nested merges.
      // We won't worry about the logic for that yet.
      if(!(p1 instanceof SearchPath) || !(p2 instanceof SearchPath)) {
        return true;
      }

      // If a `trueTransform` doesn't match, we don't have a proper split; don't try
      // to force a merge, as it won't be 'clean'.
      return p1.inputSource.trueTransform != p2.inputSource.trueTransform;
    });

    if(!splitClusterCondition1 || !splitClusterCondition2) {
      // Easy case:  we already built the paths, so just use that!
      return new SearchCluster(parentMerges);
    } else {
      // Re-merge case:  we built a path with the first half, so pull that off
      // and rebuild with the merged version.

      const recombinedPaths = parentMerges.map((parent, index) => {
        const child = space.parents[index];

        // Assumption:  parent = SearchPath.  SearchPath instances have only one
        // direct parent.
        const trueParent = parent.parents[0];
        // SearchPath will safely remerge this as a single input.
        const remerged = parent.merge(child);
        if(remerged.inputCount != parent.inputCount) {
          console.log("unexpected issue:  did not properly re-merge stuff");
        }

        return trueParent.merge(remerged);
      });

      return new SearchCluster(recombinedPaths);
    }
  }

  split(charIndex: number): [SearchSpace, SearchSpace] {
    // Don't rebuild if this is already a perfect split point!
    if(this.codepointLength <= charIndex) {
      return [this, new SearchPath(this.model)];
    }

    // It's... actually shockingly easy.  Split each constituent space, then
    // combine the halves into their own SearchClusters.
    //
    // Neither can be just a SearchPath:  we only become a SearchCluster if
    // there are different length edges on the path leading to the final
    // .codepointLength.
    const results = this.parents.map((p) => p.split(charIndex));

    // TODO:  consider deduplicating r[0]s?  Or is that possibly even necessary?
    // It doesn't seem possible for a keystroke-transform split, at least.
    //
    // If it's a clean "before and after" split, though... maybe it matters then?
    return [
      new SearchCluster(results.map(r => r[0])),
      new SearchCluster(results.map(r => r[1]))
    ];
  }

  get constituentPaths(): SearchPath[][] {
    return this.parents.flatMap((p) => p.constituentPaths);
  }
}