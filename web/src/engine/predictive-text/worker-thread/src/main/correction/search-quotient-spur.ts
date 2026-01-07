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
import { generateSpaceSeed, PathResult, SearchQuotientNode } from './search-quotient-node.js';

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
  readonly inputs?: Distribution<Readonly<Transform>>;

  private parentPath: SearchQuotientSpur;
  readonly spaceId: number;

  /**
   * Marks all results that have already been returned since the last input was received.
   * Is cleared after .addInput() calls.
   */
  private returnedValues?: {[resultKey: string]: SearchNode} = {};

  /**
   * Provides a heuristic for the base cost at each depth if the best
   * individual input were taken at that level.
   */
  private lowestCostAtDepth: number[];

  /**
   * Constructs a fresh SearchSpace instance for used in predictive-text correction
   * and suggestion searches.
   * @param baseSpaceId
   * @param model
   */
  constructor(model: LexicalModel);
  constructor(space: SearchQuotientSpur, inputs: Distribution<Transform>, bestProbFromSet: number);
  constructor(arg1: LexicalModel | SearchQuotientSpur, inputs?: Distribution<Transform>, bestProbFromSet?: number) {
    this.spaceId = generateSpaceSeed();

    if(arg1 instanceof SearchQuotientSpur) {
      const parentNode = arg1 as SearchQuotientSpur;
      const logTierCost = -Math.log(bestProbFromSet);

      this.inputs = inputs;
      this.lowestCostAtDepth = parentNode.lowestCostAtDepth.concat(logTierCost);
      this.parentPath = parentNode;

      this.addEdgesForNodes(parentNode.previousResults.map(v => v.node));

      return;
    }

    const model = arg1 as LexicalModel;
    this.selectionQueue.enqueue(new SearchNode(model.traverseFromRoot(), this.spaceId, t => model.toKey(t)));
    this.lowestCostAtDepth = [];
  }

  /**
   * Retrieves the sequences of inputs that led to this SearchPath.
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

  public get currentCost(): number {
    const parentCost = this.parentPath?.currentCost ?? Number.POSITIVE_INFINITY;
    const localCost = this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;

    return Math.min(localCost, parentCost);
  }

  private addEdgesForNodes(baseNodes: ReadonlyArray<SearchNode>) {
    // With a newly-available input, we can extend new input-dependent paths from
    // our previously-reached 'extractedResults' nodes.
    let outboundNodes = baseNodes.map((node) => {
      // Hard restriction:  no further edits will be supported.  This helps keep the search
      // more narrowly focused.
      const substitutionsOnly = node.editCount == 2;

      let deletionEdges: SearchNode[] = [];
      if(!substitutionsOnly) {
        deletionEdges         = node.buildDeletionEdges(this.inputs, this.spaceId);
      }
      const substitutionEdges = node.buildSubstitutionEdges(this.inputs, this.spaceId);

      // Skip the queue for the first pass; there will ALWAYS be at least one pass,
      // and queue-enqueing does come with a cost - avoid unnecessary overhead here.
      return substitutionEdges.flatMap(e => e.processSubsetEdge()).concat(deletionEdges);
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
    const parentCost = this.parentPath?.currentCost ?? Number.POSITIVE_INFINITY;
    const localCost = this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;

    if(parentCost <= localCost) {
      if(parentCost == Number.POSITIVE_INFINITY) {
        return {
          type: 'none'
        };
      }

      const result = this.parentPath.handleNextNode();

      if(result.type == 'complete') {
        this.addEdgesForNodes([result.finalNode]);
      }

      return {
        ...result,
        type: 'intermediate'
      } as PathResult
    }

    let currentNode = this.selectionQueue.dequeue();

    let unmatchedResult: PathResult = {
      type: 'intermediate',
      cost: currentNode.currentCost
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

  public get previousResults(): SearchResult[] {
    return Object.values(this.returnedValues ?? {}).map(v => new SearchResult(v));
  }
}