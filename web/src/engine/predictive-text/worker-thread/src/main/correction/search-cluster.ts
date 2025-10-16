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
import { generateSpaceSeed, PathResult, SearchSpace } from './search-space.js';
import { SearchPath } from './search-path.js';

import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;

const PATH_QUEUE_COMPARATOR: Comparator<SearchPath> = (a, b) => {
  return a.currentCost - b.currentCost;
}

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchCluster implements SearchSpace {
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
  constructor(model: LexicalModel);
  constructor(inboundPaths: ReadonlyArray<SearchPath>);
  constructor(arg2?: ReadonlyArray<SearchPath> | LexicalModel) {
    this.spaceId = generateSpaceSeed();

    if(Array.isArray(arg2)) {
      const inboundPaths = arg2 as ReadonlyArray<SearchPath>;
      this.lowestPossibleSingleCost = Math.min(...inboundPaths.map(p => p.lowestPossibleSingleCost));
      this.completedPaths = inboundPaths.flatMap(p => p.previousResults).map(r => r.node);
      inboundPaths.forEach(p => p.stopTrackingResults());
      this.selectionQueue.enqueueAll(inboundPaths);
    } else {
      const model = arg2 as LexicalModel;
      const rootPath = new SearchPath(model);
      this.selectionQueue.enqueue(rootPath);
    }

    return;
  }

  /**
   * Retrieves the sequence of inputs
   */
  public get inputSequences(): Distribution<Transform>[][] {
    const paths = this.selectionQueue.toArray();
    return paths.flatMap((p) => p.inputSequences);
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

  /**
   * Extends the correction-search process embodied by this SearchSpace by an extra
   * input character, according to the characters' likelihood in the distribution.
   * @param inputDistribution The fat-finger distribution for the incoming keystroke (or
   * just the raw keystroke if corrections are disabled)
   */
  addInput(inputDistribution: Distribution<Transform>, bestProbFromSet: number): SearchPath {
    return new SearchPath(this, inputDistribution, bestProbFromSet);
  }

  public get currentCost(): number {
    return this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;
  }

  public get rootPath() {
    return this.selectionQueue.peek().rootPath;
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

  public get codepointLength(): number {
    return this.parents?.[0].codepointLength ?? 0;
  }

  // public merge(space: SearchSpace): SearchSpace {
  //   // just... iterate through entries to construct an extended version of THIS
  //   // search-space.
  //   // - though... we aren't actually set up to... DO that, are we?
  //   //   - ... .inputs?  That might actually work!
  //   // - issue:  merging previously split inputs / input paths.  How to identify those?
  //   //   - ** wait:  they share the same transform ID on both sides! **
  //   //   - alternative thought:  could demark a source path spaceID on split-off paths?
  //   //     - or... maybe just a 'split ID'?
  //   //     - so... that's enough, right?  For our purposes?
  //   //     - but split + split again are not technically impossible...
  //   //     - and even then, isn't that actually overcomplicating things?
  //   //
  //   // Needs a new spaceID, of course - at each appended step, to be clear.
  //   //
  //   //
  //   // spaceID => SearchSpace, it would seem.  Just have the token use the ID
  //   // from SearchSpace / SearchEdge... wait.  SearchEdges with same ID... that's
  //   // only really available from SearchSpace.  May need to brainstorm that a bit.
  //   // - current design WAS to do the combination on the State level.
  //   // ... what if SearchSpace re-maps the search path stateIDs to a combined stateID
  //   // that it emits?  Then we don't need to worry about micromanaging search path
  //   // IDs given how they'll be constructed.
  // }

  public split(charIndex: number, model: LexicalModel): [SearchSpace, SearchSpace][] {
    return this._split(charIndex, model, new Map());
  }

  // splitCache:
  // - key:  id of original search path being split
  // - value's index:  the number of preserved codepoints
  // - value's instance at the index:  the spun-off search space
  private _split(
    charIndex: number,
    model: LexicalModel,
    splitCache: Map<number, {head: SearchCluster, tail: SearchCluster} []>
  ): [SearchCluster, SearchCluster][] {
    if(this.codepointLength == charIndex) {
      console.log('a');
      // If we're splitting at the tail end of an existing space, just re-use
      // the space and pass along an empty one for the end.
      return [[this, new SearchCluster(model)]];
    }

    // Ensure common split-ancestors still resolve to the same entity.
    const componentPaths = this.selectionQueue.toArray();
    let baseResultSet: [SearchCluster, SearchCluster][] = [];

    const deduplicateSplitResults = (results: [SearchCluster, SearchCluster][]) => {
      // Re-merge paths that converge to the same point.
      const duplicateMap: Map<number, [SearchCluster, SearchCluster][]> = new Map();
      results.forEach(result => {
        const headSpaceId = result[0].spaceId;
        const arr: [SearchCluster, SearchCluster][] = duplicateMap.get(headSpaceId) ?? [];
        arr.push(result);
        duplicateMap.set(result[0].spaceId, arr);
      });

      const finalResults: [SearchCluster, SearchCluster][] = [];
      for(const splits of duplicateMap.values()) {
        const headSpace = splits[0][0];

        // const uniqueTailSpaces = [...splits.reduce((set, curr) => {
        //   if(!set.has(curr[1].spaceId)) {
        //     set.set(curr[1].spaceId, curr[1]);
        //   } else {
        //     console.log('z');
        //   }

        //   return set;
        // }, new Map<number, SearchSpace>()).values()];

        const paths = splits.flatMap(split => split[1].selectionQueue.toArray());
        const tailSpace = new SearchCluster(paths);
        // const resultPaths: [SearchSpace, SearchSpace][] = uniqueTailSpaces.map((tailSpace) => ([headSpace, tailSpace]));

        // resultPaths.forEach(entry => finalResults.push(entry));
        finalResults.push([headSpace, tailSpace]);
      }

      return finalResults;
    }

    const pathFiltering = componentPaths.reduce((filtering, path) => {
      if(path.codepointLength - path.edgeLength > charIndex) {
        filtering.inParent.push(path);
      } else {
        filtering.inCurrent.push(path);
      }

      return filtering;
    }, { inParent: [] as SearchPath[], inCurrent: [] as SearchPath[]})

    // should filter all that meet the condition (and those that don't)
    if(pathFiltering.inParent.length > 0) {
      const parentResults = pathFiltering.inParent.flatMap((path) => {
        console.log(`b - ${path.bestExample.text}`);
        // TODO:  resolve!
        const results = (path.parents[0] as SearchCluster)._split(charIndex, model, splitCache);

        return results.map((results) => {
          const tailSpace = new SearchCluster([results[1].addInput([...path.inputs], path.bestProbInEdge)]);
          results[1] = tailSpace;
          return results;
        });
      });

      baseResultSet = parentResults;
    }

    // Re: space IDs - we can't reuse data for anything we're reconstructing
    // after the split point. Original space IDs on the left-hand side may
    // remain unaltered, but right-hand needs to be re-built from scratch, in
    // new SearchPaths / SearchSpaces.
    //
    // We can optimize how many new spaces/paths we create for the right-hand
    // side, though:  each starting the same count in, at the same input-offset
    // position, should be safe to amalgamate.
    const pathResults: [SearchCluster, SearchCluster][] = pathFiltering.inCurrent.map((path) => {
      const parentSpace = path.parents[0] ?? new SearchCluster(model);
      const pathStartIndex = path.codepointLength - path.edgeLength;
      if(path.codepointLength - path.edgeLength == charIndex) {
        console.log(`c - ${path.bestExample.text}`);
        // yay, great case!  Splits cleanly on the boundary BEFORE this path, at
        // its start.
        //
        // parentSpace is thus the END of the prior token.
        // Start a new one with the current Path.
        // return [parentSpace, new SearchSpace(/* new spaceId */, path /* reconstructed, now space ID */)];
        const newPath = new SearchCluster(model).addInput([...path.inputs], path.bestProbInEdge);
        return [
          parentSpace instanceof SearchPath ? new SearchCluster([parentSpace]) : parentSpace,
          new SearchCluster([newPath])
        ] as [SearchCluster, SearchCluster];
      } else {
        console.log(`d - ${path.bestExample.text}`);
        // OK, so we need to actually split this path in twain.
        const pathCharIndex = charIndex - pathStartIndex;
        const results = path.split(pathCharIndex, model);
        console.log(`pathId: ${path.spaceId} - ${splitCache.has(path.spaceId) ? 'found' : 'not found'}`);

        const pathSplitCacheArray = splitCache.get(path.spaceId) ?? [];
        splitCache.set(path.spaceId, pathSplitCacheArray);

        const newHeadSpace = pathSplitCacheArray[pathCharIndex]?.head ?? new SearchCluster([new SearchPath(parentSpace, [...results[0].inputs], path.bestProbInEdge)]);
        const newTailSpace = pathSplitCacheArray[pathCharIndex]?.tail ?? new SearchCluster([new SearchCluster(model).addInput([...results[1].inputs], path.bestProbInEdge)]);

        pathSplitCacheArray[pathCharIndex] = {
          head: newHeadSpace,
          tail: newTailSpace
        }
        return [newHeadSpace, newTailSpace];
      }
    });

    baseResultSet = pathResults.concat(baseResultSet);

    // From pathResults:
    // - LHS deduplicate:  if same spaceIDs appear on left-hand side, they're the same space;
    //   we likely split at the same pointt
    // - RHS:  check search depth + offset position
    //   - order by input set likelihood
    //   - replace other path variants with that
    //
    // Finally, deduplicate the tuples as much as possible.
    // ... wait.  Why do we have multiplicity in the paths?  We need to be able to reduce things
    // down to just 1 + 1 split token, not multiple in each position.
    //
    // ... first stop:  we could just... take the most likely case and ignore the others?
    // ... in which case, why evaluate ALL paths?
    // - b/c LHS matches could show up multiple times?
    //
    // Can we mitigate these cases with improved output from the wordbreaker(s) - say,
    // about "ambiguous wordbreak" scenarios?

    console.log(`result count: ${baseResultSet.length}; results ${JSON.stringify(baseResultSet.map(r => ([r[0].bestExample.text, r[1].bestExample.text])))}`);
    return deduplicateSplitResults(baseResultSet);
  }
}