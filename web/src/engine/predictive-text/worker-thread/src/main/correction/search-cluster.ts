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
import { generateSpaceSeed, InputSegment, PathResult, SearchSpace } from './search-space.js';
import { SearchPath } from './search-path.js';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;

const PATH_QUEUE_COMPARATOR: Comparator<SearchPath> = (a, b) => {
  return a.currentCost - b.currentCost;
}

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchCluster implements SearchSpace {
  // While most functions can be done directly from SearchSpace, merging and splitting will need access
  // to SearchPath-specific members.  It's also cleaner to not allow nested SearchClusters while we
  // haven't worked out support for such a scenario.
  private selectionQueue: PriorityQueue<SearchPath> = new PriorityQueue(PATH_QUEUE_COMPARATOR);
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
  constructor(inboundPaths: ReadonlyArray<SearchPath>) {
    if(inboundPaths.length == 0) {
      throw new Error("SearchCluster requires an array with at least one SearchPath");
    }

    let lowestPossibleSingleCost = Number.POSITIVE_INFINITY;
    const firstPath = inboundPaths[0];
    const inputCount = firstPath.inputCount;
    const codepointLength = firstPath.codepointLength;
    const sourceRangeKey = firstPath.sourceRangeKey;

    for(let path of inboundPaths) {
      if(path.inputCount != inputCount || path.codepointLength != codepointLength) {
        throw new Error(`SearchPath does not share same properties as others in the cluster:  inputCount ${path.inputCount} vs ${inputCount}, codepointLength ${path.codepointLength} vs ${codepointLength}`);
      }

      // If there's a source-range key mismatch - via mismatch in count or in actual ID, we have an error.
      if(path.sourceRangeKey != sourceRangeKey) {
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

  public get parents(): SearchPath[] {
    return this.selectionQueue.toArray().slice();
  }

  increaseMaxEditDistance() {
    // By extracting the entries from the priority queue and increasing distance outside of it as a batch job,
    // we get an O(N) implementation, rather than the O(N log N) that would result from maintaining the original queue.
    const entries = this.selectionQueue.toArray();

    entries.forEach((path) => path.increaseMaxEditDistance());

    // Since we just modified the stored instances, and the costs may have shifted, we need to re-heapify.
    this.selectionQueue = new PriorityQueue<SearchPath>(PATH_QUEUE_COMPARATOR, entries.slice());
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

  merge(space: SearchCluster): SearchCluster;
  merge(space: SearchPath): SearchPath;
  merge(space: SearchSpace): SearchSpace;
  merge(space: SearchSpace): SearchSpace {
    // If we're at a root (which is without inputs), bypass it.
    if(space.parents.length == 0) {
      return this;
    }

    // What if we're trying to merge something previously split?
    // That can only happen at the head of the incoming space, so we check for it early here.
    if(space.inputCount == 1 && space instanceof SearchPath) {
      // In such a case... the 'leading edge' of the incoming space needs to be checked
      // against the trailing edge of `this` instance's entries.
      const thisTailInputSource = this.inputSegments[this.inputSegments.length - 1];
      const spaceHeadInputSource = space.inputSegments[0];

      const isOnSplitInput =
        // At this stage, spaceHeadInputSource.subsetId should not be accessible; clusters can't properly give that.
        thisTailInputSource.transitionId == spaceHeadInputSource.transitionId
        && thisTailInputSource.end == spaceHeadInputSource.start;

      // In this case, we only rebuild the single path; an outer stack frame will reconstitute
      // the split cluster from the individual paths built here.
      if(isOnSplitInput) {
        const firstHalf = this.parents.find((tailPath) => tailPath.inputSource?.subsetId == space.inputSource?.subsetId);
        return firstHalf.merge(space) as SearchPath;
      }
    }

    // Simple, straightforward.  SearchPaths can easily built with a SearchCluster as parent.
    // In this case, there's also no chance of a prior split; if we'd split, it'd be a
    // SearchCluster on both ends.
    if(space instanceof SearchPath) {
      const parentMerge = this.merge(space.parents[0]);
      return new SearchPath(parentMerge, space.inputs, space.inputSource);
    }

    // If we're here, we have a SearchCluster being merged in... and to
    // something that's already a SearchCluster.
    //
    // Merge the parent components first as a baseline.  This specific state's
    // aspects have to come after their affects are merged in, anyway.
    // (Note:  is the main point of recursion.)
    const parents = (space as SearchCluster).parents; // the constituent paths

    // Assumes either none of the space's heads were split or that ALL were.
    const parentMerges = parents.map((p) => this.merge(p)); // we get paths out

    // Build a new SearchCluster from the terminal paths of the cluster being merged in.
    return new SearchCluster(parentMerges);
  }

  split(charIndex: number): [SearchSpace, SearchSpace][] {
    // Don't rebuild if this is already a perfect split point!
    if(this.codepointLength <= charIndex) {
      return [[this, new SearchPath(this.model)]];
    }

    // Path-deduplication:  it is possible for paths to diverge after a point
    // and then reconverge at later points.  If the split happens before such
    // a divergence-reconvergence sequence, it is possible for left-hand side
    // entries to be duplicated.

    // this.parents:  should always be `SearchPath`s in practice.
    // - If one is actually a cluster, we probably shouldn't attempt to
    //   de-duplicate it; we haven't figured out the logic for that yet.
    const results = this.parents.flatMap((p) => p.split(charIndex));

    // Left-hand side:  could be either SearchPaths or SearchClusters.
    // If clusters, check space ID.
    // If paths... check in more detail.
    // ... `.matches(space)`?
    // ... `.isSplitFrom(space)`? (merges)
    // - is there a different helper method that could really save our bacon?
    //   - abstracting this for a possible future third type (or similar) may actually
    //     be the key to moving forward.
    // - do we need to verify EACH subset ID matches, rather than just the last?
    //   - wait... can we even do that?  (parent SearchCluster)

    // isSameSpace(space: SearchSpace) // or matches(space: __)
    // - for deduplication

    // Deduping LHS Path:
    // - spaceID match?  Yay, the easy case!
    // - else, TokenizationPath subset ID match + split indices match:  yay!  It's a match!
    //   - so, the clustering condition (split indices), PLUS subset ID match.
    //   - again, isSameSpace() - using get clusterKey()

    // Deduping LHS cluster:
    // - spaceID match?  Yay, the easy case!
    // - ... is there any other case?
    //   - ... well, if the cluster's paths (parents) all have matches, that's a match, yeah?
    //   - is _that_ needed, though?
    //     -
    //
    // ... suppose a cluster happens, then two divergent paths, cluster, divergent paths with clean split.
    // - ... then cluster-paths-cluster is clean
    //
    // ... cluster | paths, cluster, paths, cluster
    // - THAT would be the case to test for, wouldn't it?
    // - while I can't fully envision the fallout, I CAN envision the UNIT TEST - and that'll be
    //   very enlightening.  Use that!
    //   - so, we'd have the 2nd cluster entry attempt to de-dupe from the parent cluster/paths split.
    //
    // - alternatively, three clusters in a row (with alternate clusters reachable by different paths)
    //   - there's one test sequence I built with this; try splitting that properly for the diff cluster types.

    // Clustering RHS paths (will NOT be clusters!)
    // - RHS "split index" match?
    //   - not split?  Awesome!
    // - LHS "split index" match?
    //   - ... again, same deal.
    // ... so a combined split key:
    // - if not split, then can cluster
    // - if split, cluster entries with like combined split keys.
    // `get clusterKey(): string`
    // - codepoint length + split indices of final input.
    //   - must match for all paths in same cluster.
    //   - isn't it sourceRangeKey?
    //   - OH.  codepointLength - that part certainly matters.
    //   - ... but it will equal, b/c split works that way.

    // SearchPath
    // Right-hand side:  always SearchPaths.

    // -- OLD THOUGHTS --
    // .codepointLength + .inputSource.subsetId should be enough to uniquely ID
    // duplicated left-hand splits.

    // Clean splits and 'dirty' splits (where a constituent path is split in
    // two) should return separate SearchClusters...
    // - (?) they'll have different input counts and/or .codepointLength values.
    //   - not necessarily true on LHS.
    // - Partial transforms (that don't apply the whole thing) should be a range
    //   mis-match against cases applying the whole thing.
    //   - How to model that, then?  The distinction doesn't (yet) show up in keying!

    const resultsWithCluster: [SearchCluster, SearchSpace][] = [];

    const resultMap = new Map<string, {
      heads: SearchPath[],
      tails: SearchPath[]
    }>();
    results.forEach((result) => {
      const [head, tail] = result;

      if(head instanceof SearchCluster) {
        // TODO:  Is this actually a case that can happen?
        resultsWithCluster.push([head, tail]);
        return;
      }

      let key = head.inputCount + '+' + (!(head instanceof SearchPath) ? '' : head.clusterKey);
      const outerEntry = resultMap.get(key) ?? { heads: [], tails: [] };

      if(!outerEntry.heads.find(p => head.isSameSpace(p))) {
        outerEntry.heads.push(head as SearchPath);
      }

      outerEntry.tails.push(tail);
      resultMap.set(key, outerEntry);
    });

    const resultsFromPaths = [...resultMap.values()].map((entry) => {
      const headSpace = entry.heads.length > 1 ? new SearchCluster(entry.heads) : entry.heads[0];
      const tailSpace = entry.tails.length > 1 ? new SearchCluster(entry.tails) : entry.tails[0];
      return [headSpace, tailSpace] as [SearchSpace, SearchSpace];
    });
    return (resultsWithCluster as [SearchSpace, SearchSpace][]).concat(resultsFromPaths);
  }

  isSameSpace(space: SearchSpace): boolean {
    // Easiest cases:  when the instances or their ' `spaceId` matches, we have
    // a perfect match.
    if(this == space || this.spaceId == space.spaceId) {
      return true;
    }

    // If it's falsy or a different SearchSpace type, that's an easy filter.
    if(!space || !(space instanceof SearchCluster)) {
      return false;
    }

    // We need to check if the parents match.  Done naively in the manner below, this is O(N^2).
    // Granted, we shouldn't have _that_ many incoming paths.
    if(this.parents.find((path) => !space.parents.find((path2) => path.isSameSpace(path2)))) {
      return false;
    }

    return true;
  }

  get constituentPaths(): SearchPath[][] {
    return this.parents.flatMap((p) => p.constituentPaths);
  }
}