/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-09
 *
 * This file defines tests for the predictive-text engine's SearchPath class,
 * which is used to manage the search-space(s) for text corrections within the
 * engine.
 */

import { QueueComparator as Comparator, KMWString, PriorityQueue } from '@keymanapp/web-utils';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { EDIT_DISTANCE_COST_SCALE, SearchNode, SearchResult } from './distance-modeler.js';
import { PathResult, SearchQuotientNode } from './search-quotient-node.js';

import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;

export const DEFAULT_ALLOTTED_CORRECTION_TIME_INTERVAL = 33; // in milliseconds.

export const QUEUE_NODE_COMPARATOR: Comparator<SearchNode> = function(arg1, arg2) {
  return arg1.currentCost - arg2.currentCost;
}

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchQuotientSpur implements SearchQuotientNode {
  private selectionQueue: PriorityQueue<SearchNode> = new PriorityQueue(QUEUE_NODE_COMPARATOR);
  private inputs: Distribution<Transform>;

  readonly rootPath: SearchQuotientSpur;

  private parentPath: SearchQuotientSpur;

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
   * Marks all results that have already been returned since the last input was received.
   * Is cleared after .addInput() calls.
   */
  public returnedValues?: {[resultKey: string]: SearchNode} = {}; // TODO:  make it private again!

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
  private lowestCostAtDepth: number[];

  /**
   * Clone constructor.  Deep-copies its internal queues, but not search nodes.
   * @param instance
   */
  constructor(instance: SearchQuotientSpur);
  /**
   * Constructs a fresh SearchSpace instance for used in predictive-text correction
   * and suggestion searches.
   * @param baseSpaceId
   * @param model
   */
  constructor(model: LexicalModel);
  constructor(arg1: LexicalModel | SearchQuotientSpur) {
    if(arg1 instanceof SearchQuotientSpur) {
      const parentSpace = arg1;
      this.lowestCostAtDepth = parentSpace.lowestCostAtDepth.slice();
      this.rootPath = parentSpace.rootPath;
      this.parentPath = parentSpace;

      return;
    }

    const model = arg1;
    if(!model.traverseFromRoot) {
      throw new Error("The provided model does not implement the `traverseFromRoot` function, which is needed to support robust correction searching.");
    }

    const rootNode = new SearchNode(model.traverseFromRoot(), model.toKey ? model.toKey.bind(model) : null);
    this.selectionQueue.enqueue(rootNode);
    this.lowestCostAtDepth = [];
    this.rootPath = this;

    this.completedPaths = [];
  }

  /**
   * Retrieves the sequence of inputs
   */
  public get inputSequence(): Distribution<Transform>[] {
    if(this.parentPath) {
      return [...this.parentPath.inputSequence, this.inputs];
    } else if(this.inputs) {
      return [this.inputs];
    } else {
      return [];
    }
  }

  public get inputCount(): number {
    return (this.parentPath?.inputCount ?? 0) + (this.inputs ? 1 : 0);
  }

  public get bestExample(): {text: string, p: number} {
    const bestPrefix = this.parentPath?.bestExample ?? { text: '', p: 1 };
    const bestLocalInput = this.inputs?.reduce((max, curr) => max.p < curr.p ? curr : max) ?? { sample: { insert: '', deleteLeft: 0 }, p: 1};

    return {
      text: KMWString.substring(bestPrefix.text, 0, KMWString.length(bestPrefix.text) - bestLocalInput.sample.deleteLeft) + bestLocalInput.sample.insert,
      p: bestPrefix.p * bestLocalInput.p
    }
  }

  increaseMaxEditDistance() {
    this.parentPath.increaseMaxEditDistance();

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
    return this.parentPath?.correctionsEnabled || this.inputs?.length > 1;
  }

  /**
   * Extends the correction-search process embodied by this SearchPath by an extra
   * input character, according to the characters' likelihood in the distribution.
   * @param inputDistribution The fat-finger distribution for the incoming keystroke (or
   * just the raw keystroke if corrections are disabled)
   */
  addInput(inputDistribution: Distribution<Transform>, bestProbFromSet: number): SearchQuotientSpur {
    const input = inputDistribution;

    const childSpace = new SearchQuotientSpur(this);

    childSpace.inputs = inputDistribution;
    const lastDepthCost = this.lowestCostAtDepth[this.lowestCostAtDepth.length - 1] ?? 0;
    const logTierCost = -Math.log(bestProbFromSet);
    childSpace.lowestCostAtDepth.push(lastDepthCost + logTierCost);

    // With a newly-available input, we can extend new input-dependent paths from
    // our previously-reached 'extractedResults' nodes.
    let newlyAvailableEdges: SearchNode[] = [];
    let batches = this.completedPaths?.map(function(node) {
      let deletions = node.buildDeletionEdges(input);
      let substitutions = node.buildSubstitutionEdges(input);

      const batch = deletions.concat(substitutions);

      // Skip the queue for the first pass; there will ALWAYS be at least one pass,
      // and queue-enqueing does come with a cost.  Avoid the unnecessary overhead.
      return batch.flatMap(e => e.processSubsetEdge());
    });

    childSpace.completedPaths = [];
    childSpace.returnedValues = {};

    batches?.forEach(function(batch) {
      newlyAvailableEdges = newlyAvailableEdges.concat(batch);
    });

    childSpace.selectionQueue.enqueueAll(newlyAvailableEdges);

    return childSpace;
  }

  public get currentCost(): number {
    const parentCost = this.parentPath?.currentCost ?? Number.POSITIVE_INFINITY;
    const localCost = this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;

    return Math.min(localCost, parentCost);
  }

  /**
   * Given an incoming SearchNode, this method will build all outgoing edges
   * from the node that correspond to processing this SearchPath instance's
   * input distribution.
   * @param currentNode
   */
  private addEdgesForNodes(currentNode: SearchNode) {
    // Hard restriction:  no further edits will be supported.  This helps keep the search
    // more narrowly focused.
    const substitutionsOnly = currentNode.editCount == 2;

    let deletionEdges: SearchNode[] = [];
    if(!substitutionsOnly) {
      deletionEdges       = currentNode.buildDeletionEdges(this.inputs);
    }
    const substitutionEdges = currentNode.buildSubstitutionEdges(this.inputs);
    let batch = deletionEdges.concat(substitutionEdges);

    // Skip the queue for the first pass; there will ALWAYS be at least one pass,
    // and queue-enqueing does come with a cost - avoid unnecessary overhead here.
    batch = batch.flatMap(e => e.processSubsetEdge());

    this.selectionQueue.enqueueAll(batch);
    // We didn't reach an end-node, so we just end the iteration and continue the search.
  }

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the selection queue,
   * checks its validity as a correction to the input text, and reports on what
   * sort of result the edge's destination node represents.
   * @returns
   */
  public handleNextNode(): PathResult {
    const parentCost = this.parentPath?.currentCost ?? Number.POSITIVE_INFINITY;
    const localCost = this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;

    if(parentCost <= localCost) {
      if(parentCost == Number.POSITIVE_INFINITY) {
        return {
          type: 'none'
        };
      }

      const result = this.parentPath.handleNextNode();

      if(result.type == 'complete' && !this.processedEdgeSet[result.finalNode.pathKey]) {
        this.addEdgesForNodes(result.finalNode);
      }

      return {
        ...result,
        type: 'intermediate'
      } as PathResult;
    }

    let currentNode = this.selectionQueue.dequeue();

    let unmatchedResult: PathResult = {
      type: 'intermediate',
      cost: currentNode.currentCost
    }

    // Have we already processed a matching edge?  If so, skip it.
    // We already know the previous edge is of lower cost.
    if(this.processedEdgeSet[currentNode.pathKey]) {
      return unmatchedResult;
    } else {
      this.processedEdgeSet[currentNode.pathKey] = true;
    }

    // Stage 1:  filter out nodes/edges we want to prune

    // Forbid a raw edit-distance of greater than 2.
    // Note:  .knownCost is not scaled, while its contribution to .currentCost _is_ scaled.
    let substitutionsOnly = false;
    if(currentNode.editCount > 2) {
      return unmatchedResult;
    } else if(currentNode.editCount == 2) {
      substitutionsOnly = true;
    }

    // Thresholds _any_ path, partially based on currently-traversed distance.
    // Allows a little 'wiggle room' + 2 "hard" edits.
    // Can be important if needed characters don't actually exist on the keyboard
    // ... or even just not the then-current layer of the keyboard.
    //
    // TODO:  still consider the lowest-cost individual edges for THIS specific criterion.
    const tierMinCost = this.lowestCostAtDepth[currentNode.priorInput.length-1];
    if(currentNode.currentCost > tierMinCost + 2.5 * EDIT_DISTANCE_COST_SCALE) {
      return unmatchedResult;
    }

    // Stage 2:  process subset further OR build remaining edges

    if(currentNode.hasPartialInput) {
      // Re-use the current queue; the number of total inputs considered still holds.
      this.selectionQueue.enqueueAll(currentNode.processSubsetEdge());
      return unmatchedResult;
    }

    // OK, we fully crossed a graph edge and have landed on a transition point;
    // time to build more edges / edge batches.

    // Always possible, as this does not require any new input.
    if(!substitutionsOnly) {
      let insertionEdges = currentNode.buildInsertionEdges();
      this.selectionQueue.enqueueAll(insertionEdges);
    }

    // It was the final tier - store the node for future reference.
    this.completedPaths?.push(currentNode);

    if((this.returnedValues[currentNode.resultKey]?.currentCost ?? Number.POSITIVE_INFINITY) > currentNode.currentCost) {
      this.returnedValues[currentNode.resultKey] = currentNode;
    } else {
      // Not a better cost, so reject it and move on to the next potential result.
      return this.handleNextNode();
    }

    return {
      type: 'complete',
      cost: currentNode.currentCost,
      finalNode: currentNode
    };
  }

  public get previousResults(): SearchResult[] {
    return Object.values(this.returnedValues).map(v => new SearchResult(v));
  }
}