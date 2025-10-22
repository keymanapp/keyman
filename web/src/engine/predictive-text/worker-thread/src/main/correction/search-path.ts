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
import { SearchCluster } from './search-cluster.js';
import { generateSpaceSeed, PathResult, SearchSpace } from './search-space.js';

import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;

export const QUEUE_NODE_COMPARATOR: Comparator<SearchNode> = function(arg1, arg2) {
  return arg1.currentCost - arg2.currentCost;
}

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchPath implements SearchSpace {
  private selectionQueue: PriorityQueue<SearchNode> = new PriorityQueue(QUEUE_NODE_COMPARATOR);
  private _inputs?: Distribution<Transform>;

  public get inputs(): Distribution<Transform> {
    return this._inputs;
  }

  readonly rootPath: SearchPath;

  readonly bestProbInEdge: number;

  private parentSpace: SearchSpace;
  readonly spaceId: number;

  // We use an array and not a PriorityQueue b/c batch-heapifying at a single point in time
  // is cheaper than iteratively building a priority queue.

  /**
   * Marks all results that have already been returned from this instance of SearchPath.
   * Should be deleted and cleared if any paths consider this one as a parent.
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
   * Constructs a fresh SearchPath instance for used in predictive-text correction
   * and suggestion searches.
   * @param baseSpaceId
   * @param model
   */
  constructor(model: LexicalModel);
  constructor(space: SearchSpace, inputs: Distribution<Transform>, bestProbFromSet: number);
  constructor(arg1: LexicalModel | SearchSpace, inputs?: Distribution<Transform>, bestProbFromSet?: number) {
    // If we're taking in a pre-constructed search node, it's got an associated,
    // pre-assigned spaceID - so use that.
    const isExtending = (arg1 instanceof SearchPath || arg1 instanceof SearchCluster);
    this.spaceId = generateSpaceSeed();

    if(isExtending) {
      const parentSpace = arg1 as SearchSpace;
      this.bestProbInEdge = bestProbFromSet;
      const logTierCost = -Math.log(bestProbFromSet);

      this._inputs = inputs;
      this.lowestPossibleSingleCost = parentSpace.lowestPossibleSingleCost + logTierCost;
      this.rootPath = parentSpace.rootPath;
      this.parentSpace = parentSpace;

      this.addEdgesForNodes(parentSpace.previousResults.map(r => r.node));

      return;
    }

    const model = arg1 as LexicalModel;
    this.selectionQueue.enqueue(new SearchNode(model.traverseFromRoot(), this.spaceId, t => model.toKey(t)));
    this.lowestPossibleSingleCost = 0;
    this.rootPath = this;
    this.bestProbInEdge = 1;
  }

  /**
   * Retrieves the sequences of inputs that led to this SearchPath.
   */
  public get inputSequences(): Distribution<Transform>[][] {
    const parentSequences = this.parentSpace?.inputSequences ?? [];

    if(parentSequences.length == 0) {
      return this._inputs ? [[this._inputs]] : [];
    } else {
      return parentSequences.map(s => [...s, this._inputs]);
    }
  }

  public hasInputs(keystrokeDistributions: Distribution<Transform>[]): boolean {
    if(this.inputCount == 0) {
      return keystrokeDistributions.length == 0;
    } else if(keystrokeDistributions.length != this.inputCount) {
      return false;
    }

    const tailInput = [...keystrokeDistributions.pop()];
    const localInput = this.lastInput;

    // Actual reference match?  Easy mode.
    if(localInput == tailInput) {
      return !!this.parents.find(p => p.hasInputs(keystrokeDistributions));
    } else if(localInput.length != tailInput.length) {
      return false;
    } else {
      for(let entry of tailInput) {
        const matchIndex = localInput.findIndex((x) => {
          const s1 = x.sample;
          const s2 = entry.sample;
          // Check for equal reference first before the other checks; it makes a nice shortcut.
          if(x == entry) {
            return true;
          } if(x.p == entry.p && s1.deleteLeft == s2.deleteLeft
            && s1.id == s2.id && ((s1.deleteRight ?? 0) == (s2.deleteRight ?? 0)) && s1.insert == s2.insert
          ) {
            return true;
          }
          return false;
        });

        if(matchIndex == -1) {
          return false;
        } else {
          tailInput.splice(matchIndex, 1);
        }
      }

      return !!this.parents.find(p => p.hasInputs(keystrokeDistributions));
    }
  }

  public get lastInput(): Distribution<Readonly<Transform>> {
    // Shallow-copies the array to prevent external modification; the Transforms
    // are marked Readonly to prevent their modification as well.
    return [...this.inputs];
  }

  public get inputCount(): number {
    if(!this.parentSpace) {
      return 0;
    } else {
      return this.parentSpace.inputCount + 1;
    }
  }

  public get logTierCost(): number {
    return -Math.log(this.bestProbInEdge);
  }

  // TODO:  track as a class property; avoid the need for repeated string calculations.
  // Or just use the subset and its pre-known length/delete values in some manner.
  public get edgeLength(): number {
    const insert = this._inputs?.[0].sample.insert ?? '';
    return KMWString.length(insert);
  }

  // TODO:  consider optimizing this; we could certainly precompute these values
  // rather than recalculating it each time.
  public get codepointLength(): number {
    const deleteLeft = this._inputs?.[0].sample.deleteLeft ?? 0;
    const baseLength = this.parentSpace?.codepointLength ?? 0;
    return baseLength + this.edgeLength - deleteLeft;
  }

  public get bestExample(): {text: string, p: number} {
    const bestPrefix = this.parentSpace?.bestExample ?? { text: '', p: 1 };
    const bestLocalInput = this._inputs?.reduce((max, curr) => max.p < curr.p ? curr : max) ?? { sample: { insert: '', deleteLeft: 0 }, p: 1};

    return {
      text: KMWString.substring(bestPrefix.text, 0, KMWString.length(bestPrefix.text) - bestLocalInput.sample.deleteLeft) + bestLocalInput.sample.insert,
      p: bestPrefix.p * bestLocalInput.p
    }
  }

  get parents() {
    // The SearchPath class may only have a single parent.
    return [this.parentSpace];
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

  // ... maaaaybe only call if actually splitting?
  // charIndex:  index within this.edgeLength where the split may occur.
  public split(charIndex: number, model: LexicalModel): [SearchPath, SearchPath] {
    // ... might be calculated from the SearchSpace class?
    if(charIndex < this.edgeLength) {
      // TODO:  split!
      const firstSet: Distribution<Transform> = this._inputs.map((input) => ({
        // keep insert head
        // keep deleteLeft
        sample: {
          insert: KMWString.substring(input.sample.insert, 0, charIndex),
          deleteLeft: input.sample.deleteLeft
        }, p: input.p
      }));

      const secondSet: Distribution<Transform> = this._inputs.map((input) => ({
        // keep insert tail
        // deleteLeft == 0
        sample: {
          insert: KMWString.substring(input.sample.insert, charIndex),
          deleteLeft: 0
        }, p: input.p
      }));

      // construct two SearchPath instances based on the two sets!
      return [
        new SearchPath(this.parentSpace, firstSet, this.logTierCost),
        new SearchPath(new SearchPath(model), secondSet, this.logTierCost)
      ];
    } else {
      // this instance = 'first set'
      // second instance:  empty transforms.
      //
      // stopgap:  maybe go ahead and check each input for any that are longer?
      // won't matter shortly, though.
      return [this, new SearchPath(model)];
    }
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
      if(this.returnedValues) {
        if((this.returnedValues[currentNode.resultKey]?.currentCost ?? Number.POSITIVE_INFINITY) > currentNode.currentCost) {
          this.returnedValues[currentNode.resultKey] = currentNode;
        } else {
          // Not a better cost, so reject it and move on to the next potential result.
          return this.handleNextNode();
        }
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
    return Object.values(this.returnedValues ?? {}).map(v => new SearchResult(v));
  }

  /**
   *
   */
  public stopTrackingResults() {
    delete this.returnedValues;
  }
}