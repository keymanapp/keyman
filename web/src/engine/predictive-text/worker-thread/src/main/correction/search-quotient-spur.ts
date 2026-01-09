/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-09
 *
 * This file defines tests for the predictive-text engine's SearchSpace class,
 * which is used to manage the search-space(s) for text corrections within the
 * engine.
 */

import { QueueComparator as Comparator, PriorityQueue } from '@keymanapp/web-utils';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { PathResult, SearchNode } from './distance-modeler.js';

import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;

export const QUEUE_NODE_COMPARATOR: Comparator<SearchNode> = function(arg1, arg2) {
  return arg1.currentCost - arg2.currentCost;
}

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchQuotientSpur {
  // p = 1 / (e^4) = 0.01831563888.  This still exceeds many neighboring keys!
  // p = 1 / (e^5) = 0.00673794699.  Strikes a good balance.
  // Should easily give priority to neighboring keys before edit-distance kicks in (when keys are a bit ambiguous)
  static readonly EDIT_DISTANCE_COST_SCALE = 5;
  static readonly MIN_KEYSTROKE_PROBABILITY = 0.0001;
  static readonly DEFAULT_ALLOTTED_CORRECTION_TIME_INTERVAL = 33; // in milliseconds.

  private selectionQueue: PriorityQueue<SearchNode>;
  private _inputSequence: Distribution<Transform>[] = [];
  private minInputCost: number[] = [];
  private rootNode: SearchNode;

  // We use an array and not a PriorityQueue b/c batch-heapifying at a single point in time
  // is cheaper than iteratively building a priority queue.
  /**
   * This tracks all paths that have reached the end of a viable input-matching path - even
   * those of lower cost that produce the same correction as other paths.
   *
   * When new input is received, its entries are then used to append edges to the path in order
   * to find potential paths to reach a new viable end.
   */
  private completedPaths: SearchNode[];

  /**
   * Marks all results that have already been returned since the last input was received.
   * Is cleared after .addInput() calls.
   */
  public returnedValues: {[resultKey: string]: SearchNode} = {}; // TODO:  make it private again!

  /**
   * Acts as a Map that prevents duplicating a correction-search path if reached
   * more than once.
   */
  private processedEdgeSet: {[pathKey: string]: boolean} = {};

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
   * @param model
   */
  constructor(model: LexicalModel);
  constructor(arg1: SearchQuotientSpur|LexicalModel) {
    if(arg1 instanceof SearchQuotientSpur) {
      this._inputSequence = [].concat(arg1._inputSequence);
      this.minInputCost = [].concat(arg1.minInputCost);
      this.rootNode = arg1.rootNode;
      // Re-use already-checked Nodes.
      this.completedPaths = [].concat(arg1.completedPaths);
      this.lowestCostAtDepth = arg1.lowestCostAtDepth.slice();
      this.returnedValues = {...arg1.returnedValues};
      this.processedEdgeSet = {...arg1.processedEdgeSet};

      this.selectionQueue = new PriorityQueue(QUEUE_NODE_COMPARATOR);
      this.selectionQueue.enqueueAll([...arg1.selectionQueue.toArray()]);
      return;
    }

    const model = arg1;
    if(!model) {
      throw new Error("The LexicalModel parameter must not be null / undefined.");
    } else if(!model.traverseFromRoot) {
      throw new Error("The provided model does not implement the `traverseFromRoot` function, which is needed to support robust correction searching.");
    }

    this.selectionQueue = new PriorityQueue<SearchNode>(QUEUE_NODE_COMPARATOR);
    this.rootNode = new SearchNode(model.traverseFromRoot(), model.toKey ? model.toKey.bind(model) : null);
    this.selectionQueue.enqueue(this.rootNode);
    this.lowestCostAtDepth = [];

    this.completedPaths = [];
  }

  /**
   * Retrieves the sequence of inputs
   */
  public get inputSequence() {
    return [...this._inputSequence];
  }

  increaseMaxEditDistance() {
    // By extracting the entries from the priority queue and increasing distance outside of it as a batch job,
    // we get an O(N) implementation, rather than the O(N log N) that would result from maintaining the original queue.
    const entries = this.selectionQueue.toArray();

    entries.forEach(function(edge) { edge.calculation = edge.calculation.increaseMaxDistance(); });

    // Since we just modified the stored instances, and the costs may have shifted, we need to re-heapify.
    this.selectionQueue = new PriorityQueue<SearchNode>(QUEUE_NODE_COMPARATOR, entries);
  }

  get correctionsEnabled() {
    // When corrections are disabled, the Web engine will only provide individual Transforms
    // for an input, not a distribution.  No distributions means we shouldn't do corrections.
    return !!this._inputSequence.find((distribution) => distribution.length > 1);
  }

  /**
   * Extends the correction-search process embodied by this SearchSpace by an extra
   * input character, according to the characters' likelihood in the distribution.
   * @param inputDistribution The fat-finger distribution for the incoming keystroke (or
   * just the raw keystroke if corrections are disabled)
   */
  addInput(inputDistribution: Distribution<Transform>, bestProbFromSet: number) {
    const input = inputDistribution;
    this._inputSequence.push(input);
    const lastDepthCost = this.lowestCostAtDepth[this.lowestCostAtDepth.length - 1] ?? 0;
    const logTierCost = -Math.log(bestProbFromSet);
    this.lowestCostAtDepth.push(lastDepthCost + logTierCost);

    // Assumes that `inputDistribution` is already sorted.
    this.minInputCost.push(-Math.log(inputDistribution[0].p));

    // With a newly-available input, we can extend new input-dependent paths from
    // our previously-reached 'extractedResults' nodes.
    let newlyAvailableEdges: SearchNode[] = [];
    let batches = this.completedPaths.map(function(node) {
      let deletions = node.buildDeletionEdges(inputDistribution);
      let substitutions = node.buildSubstitutionEdges(inputDistribution);

      const batch = deletions.concat(substitutions);

      // Skip the queue for the first pass; there will ALWAYS be at least one pass,
      // and queue-enqueing does come with a cost.  Avoid the unnecessary overhead.
      return batch.flatMap(e => e.processSubsetEdge());
    });

    // Don't forget to reset the array; the contained nodes no longer reach the search's end.
    this.completedPaths = [];
    this.returnedValues = {};

    batches.forEach(function(batch) {
      newlyAvailableEdges = newlyAvailableEdges.concat(batch);
    });

    this.selectionQueue.enqueueAll(newlyAvailableEdges);
  }

  // TODO: will want eventually for reversions and/or backspaces
  removeLastInput() {
    // 1.  truncate all entries from that search tier; we need to 'restore' extractedResults to match
    //     the state that would have existed without the last search tier.
    // 2.  remove the last search tier.  Which may necessitate reconstructing the tier queue, but oh well.
  }

  /**
   * Indicates if the correction-search has another entry (and thus has not yet
   * reached its end).
   * @returns
   */
  public hasNextMatchEntry(): boolean {
    return this.selectionQueue.count > 0 && this.selectionQueue.peek().currentCost < Number.POSITIVE_INFINITY;
  }

  public getCurrentCost(): number {
    if(this.selectionQueue.count > 0) {
      return this.selectionQueue.peek().currentCost;
    }

    return Number.POSITIVE_INFINITY;
  }

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the selection queue,
   * checks its validity as a correction to the input text, and reports on what
   * sort of result the edge's destination node represents.
   * @returns
   */
  handleNextNode(): PathResult {
    if(!this.hasNextMatchEntry()) {
      return { type: 'none' };
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
      // Hard restriction:  no further edits will be supported.  This helps keep the search
      // more narrowly focused.
      substitutionsOnly = true;
    }

    // Thresholds _any_ path, partially based on currently-traversed distance.
    // Allows a little 'wiggle room' + 2 "hard" edits.
    // Can be important if needed characters don't actually exist on the keyboard
    // ... or even just not the then-current layer of the keyboard.
    //
    // TODO:  still consider the lowest-cost individual edges for THIS specific criterion.
    const tierMinCost = this.lowestCostAtDepth[currentNode.priorInput.length-1];
    if(currentNode.currentCost > tierMinCost + 2.5 * SearchQuotientSpur.EDIT_DISTANCE_COST_SCALE) {
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

    if(currentNode.calculation.inputSequence.length == this.inputSequence.length) {
      // It was the final tier - store the node for future reference.
      this.completedPaths.push(currentNode);

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
    } else {
      // Time to construct new edges for the next tier!
      let inputIndex = currentNode.calculation.inputSequence.length;

      let deletionEdges: SearchNode[] = [];
      if(!substitutionsOnly) {
        deletionEdges       = currentNode.buildDeletionEdges(this._inputSequence[inputIndex]);
      }
      const substitutionEdges = currentNode.buildSubstitutionEdges(this._inputSequence[inputIndex]);
      let batch = deletionEdges.concat(substitutionEdges);

      // Skip the queue for the first pass; there will ALWAYS be at least one pass,
      // and queue-enqueing does come with a cost - avoid unnecessary overhead here.
      batch = batch.flatMap(e => e.processSubsetEdge());

      // Note:  we're live-modifying the tier's cost here!  The priority queue loses its guarantees as a result.
      this.selectionQueue.enqueueAll(batch);

      // We didn't reach an end-node, so we just end the iteration and continue the search.
    }

    // If we've somehow fully exhausted all search options, indicate that none remain.
    return unmatchedResult;
  }
}