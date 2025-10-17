/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-09
 *
 * This file defines the predictive-text engine's SearchEdge class, which is
 * used to model corrections utilizing one tokenization-transition comprising a
 * search-space during text correction within the engine.
 */

import { QueueComparator as Comparator, KMWString, PriorityQueue } from '@keymanapp/web-utils';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { EDIT_DISTANCE_COST_SCALE, SearchNode, SearchResult } from './distance-modeler.js';
import { SearchSpace } from './search-space.js';
import { generateSpaceSeed, PathResult, SearchBatcher } from './search-batcher.js';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;

export const QUEUE_NODE_COMPARATOR: Comparator<SearchNode> = function(arg1, arg2) {
  return arg1.currentCost - arg2.currentCost;
}

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchPath implements SearchBatcher {
  private selectionQueue: PriorityQueue<SearchNode> = new PriorityQueue(QUEUE_NODE_COMPARATOR);
  private inputs?: Distribution<Transform>;

  readonly rootPath: SearchPath;
  readonly spaceId: number;

  private parentSpace: SearchSpace;

  // We use an array and not a PriorityQueue b/c batch-heapifying at a single point in time
  // is cheaper than iteratively building a priority queue.

  /**
   * Marks all results that have already been returned since the last input was received.
   * Is cleared after .addInput() calls.
   */
  private returnedValues?: {[resultKey: string]: SearchNode} = {};

  /**
   * Acts as a Map that prevents duplicating a correction-search path if reached
   * more than once.
   */
  protected get processedEdgeSet(): {[pathKey: string]: boolean} {
    return this.rootPath._processedEdgeSet;
  }

  private _processedEdgeSet?: {[pathKey: string]: boolean} = {};

  /**
   * Provides a heuristic for the base cost at this path's depth if the best
   * individual input were taken here, regardless of whether or not that's possible.
   */
  readonly lowestPossibleSingleCost: number;

  /**
   * Constructs a fresh SearchSpace instance for used in predictive-text correction
   * and suggestion searches.
   * @param baseSpaceId
   * @param model
   */
  constructor(node?: SearchNode);
  constructor(space: SearchSpace, inputs: Distribution<Transform>, bestProbFromSet: number);
  constructor(arg1?: SearchNode | SearchSpace, inputs?: Distribution<Transform>, bestProbFromSet?: number) {
    // If we're taking in a pre-constructed search node, it's got an associated,
    // pre-assigned spaceID - so use that.
    const isExtending = arg1 instanceof SearchSpace;
    this.spaceId = isExtending ? generateSpaceSeed() : arg1.spaceId;

    if(isExtending) {
      const parentSpace = arg1;
      const logTierCost = -Math.log(bestProbFromSet);

      this.inputs = inputs;
      this.lowestPossibleSingleCost = parentSpace.lowestPossibleSingleCost + logTierCost;
      this.rootPath = parentSpace.rootPath;
      this.parentSpace = parentSpace;

      this.addEdgesForNodes(parentSpace.previousResults.map(r => r.node));

      return;
    }

    const node = arg1;
    this.selectionQueue.enqueue(node);
    this.lowestPossibleSingleCost = 0;
    this.rootPath = this;
  }

  /**
   * Retrieves the sequence of inputs
   */
  public get inputSequences(): Distribution<Transform>[][] {
    const parentSequences = this.parentSpace?.inputSequences ?? [];

    if(parentSequences.length == 0) {
      return this.inputs ? [[this.inputs]] : [];
    } else {
      return parentSequences.map(s => [...s, this.inputs]);
    }
  }

  public get inputCount(): number {
    if(!this.parentSpace) {
      return 0;
    } else {
      return this.parentSpace.inputCount + 1;
    }
  }

  public get bestExample(): {text: string, p: number} {
    const bestPrefix = this.parentSpace?.bestExample ?? { text: '', p: 1 };
    const bestLocalInput = this.inputs?.reduce((max, curr) => max.p < curr.p ? curr : max) ?? { sample: { insert: '', deleteLeft: 0 }, p: 1};

    return {
      text: KMWString.substring(bestPrefix.text, 0, KMWString.length(bestPrefix.text) - bestLocalInput.sample.deleteLeft) + bestLocalInput.sample.insert,
      p: bestPrefix.p * bestLocalInput.p
    }
  }

  increaseMaxEditDistance() {
    this.parentSpace.increaseMaxEditDistance();

    // By extracting the entries from the priority queue and increasing distance outside of it as a batch job,
    // we get an O(N) implementation, rather than the O(N log N) that would result from maintaining the original queue.
    const entries = this.selectionQueue.toArray();

    entries.forEach(function(edge) { edge.calculation = edge.calculation.increaseMaxDistance(); });

    // Since we just modified the stored instances, and the costs may have shifted, we need to re-heapify.
    this.selectionQueue = new PriorityQueue<SearchNode>(QUEUE_NODE_COMPARATOR, entries);
  }

  get correctionsEnabled(): boolean {
    // When corrections are disabled, the Web engine will only provide individual Transforms
    // for an input, not a distribution.  No distributions means we shouldn't do corrections.
    return this.parentSpace?.correctionsEnabled || this.inputs?.length > 1;
  }

  public get currentCost(): number {
    const parentCost = this.parentSpace?.currentCost ?? Number.POSITIVE_INFINITY;
    const localCost = this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;

    return Math.min(localCost, parentCost);
  }

  protected addEdgesForNodes(baseNodes: ReadonlyArray<SearchNode>) {
    // With a newly-available input, we can extend new input-dependent paths from
    // our previously-reached 'extractedResults' nodes.
    let outboundNodes = baseNodes.map((node) => {
      let deletions = node.buildDeletionEdges(this.inputs, this.spaceId);
      let substitutions = node.buildSubstitutionEdges(this.inputs, this.spaceId);

      const batch = deletions.concat(substitutions);

      // Skip the queue for the first pass; there will ALWAYS be at least one pass,
      // and queue-enqueing does come with a cost.  Avoid the unnecessary overhead.
      return batch.flatMap(e => e.processSubsetEdge());
    }).flat();

    this.selectionQueue.enqueueAll(outboundNodes);
  }

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the selection queue,
   * checks its validity as a correction to the input text, and reports on what
   * sort of result the edge's destination node represents.
   * @returns
   */
  public handleNextNode(): PathResult {
    const parentCost = this.parentSpace?.currentCost ?? Number.POSITIVE_INFINITY;
    const localCost = this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;

    if(parentCost <= localCost) {
      if(parentCost == Number.POSITIVE_INFINITY) {
        return {
          type: 'none'
        };
      }

      const result = this.parentSpace.handleNextNode();

      if(result.type == 'complete') {
        this.addEdgesForNodes([result.finalNode]);
      }

      return {
        ...result,
        type: 'intermediate'
      } as PathResult
    }

    // will have equal .spaceId.
    let currentNode = this.selectionQueue.dequeue();

    let unmatchedResult = {
      type: 'intermediate',
      cost: currentNode.currentCost
    }

    // Have we already processed a matching edge?  If so, skip it.
    // We already know the previous edge is of lower cost.
    if(this.processedEdgeSet[currentNode.pathKey]) {
      return unmatchedResult as PathResult;
    } else {
      this.processedEdgeSet[currentNode.pathKey] = true;
    }

    // Stage 1:  filter out nodes/edges we want to prune

    // Forbid a raw edit-distance of greater than 2.
    // Note:  .knownCost is not scaled, while its contribution to .currentCost _is_ scaled.
    let substitutionsOnly = false;
    if(currentNode.editCount > 2) {
      return unmatchedResult as PathResult;
    } else if(currentNode.editCount == 2) {
      substitutionsOnly = true;
    }

    // Thresholds _any_ path, partially based on currently-traversed distance.
    // Allows a little 'wiggle room' + 2 "hard" edits.
    // Can be important if needed characters don't actually exist on the keyboard
    // ... or even just not the then-current layer of the keyboard.
    if(currentNode.currentCost > this.lowestPossibleSingleCost + 2.5 * EDIT_DISTANCE_COST_SCALE) {
      return unmatchedResult as PathResult;
    }

    // Stage 2:  process subset further OR build remaining edges

    if(currentNode.hasPartialInput) {
      // Re-use the current queue; the number of total inputs considered still holds.
      this.selectionQueue.enqueueAll(currentNode.processSubsetEdge());
      return unmatchedResult as PathResult;
    }

    // OK, we fully crossed a graph edge and have landed on a transition point;
    // time to build more edges / edge batches.

    // Always possible, as this does not require any new input.
    if(!substitutionsOnly) {
      let insertionEdges = currentNode.buildInsertionEdges();
      this.selectionQueue.enqueueAll(insertionEdges);
    }

    if(currentNode.spaceId == this.spaceId) {
      if((this.returnedValues[currentNode.resultKey]?.currentCost ?? Number.POSITIVE_INFINITY) > currentNode.currentCost) {
        this.returnedValues[currentNode.resultKey] = currentNode;
      } else {
        // Not a better cost, so reject it and move on to the next potential result.
        return this.handleNextNode();
      }

      return {
        type: 'complete',
        cost: currentNode.currentCost,
        finalNode: currentNode,
        spaceId: this.spaceId
      };
    }

    // If we've somehow fully exhausted all search options, indicate that none remain.
    return unmatchedResult as PathResult;
  }

  public get previousResults(): SearchResult[] {
    return Object.values(this.returnedValues).map(v => new SearchResult(v, this.spaceId));
  }

  public stopTrackingResults() {
  }
}