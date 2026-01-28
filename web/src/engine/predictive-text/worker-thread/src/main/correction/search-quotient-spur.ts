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
import { generateSpaceSeed, PathResult, SearchQuotientNode, PathInputProperties } from './search-quotient-node.js';
import { generateSubsetId } from './tokenization-subsets.js';

import Distribution = LexicalModelTypes.Distribution;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

export const QUEUE_NODE_COMPARATOR: Comparator<SearchNode> = function(arg1, arg2) {
  return arg1.currentCost - arg2.currentCost;
}

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export abstract class SearchQuotientSpur implements SearchQuotientNode {
  private selectionQueue: PriorityQueue<SearchNode> = new PriorityQueue(QUEUE_NODE_COMPARATOR);
  readonly inputs?: Distribution<Transform>;
  readonly inputSource?: PathInputProperties;

  readonly bestProbInEdge: number;
  private parentNode: SearchQuotientNode;
  readonly spaceId: number;

  readonly inputCount: number;
  private _codepointLength: number;
  protected abstract readonly insertLength: number;
  protected abstract readonly leftDeleteLength: number

  /**
   * Marks all results that have already been returned from this instance of SearchPath.
   * Should be deleted and cleared if any paths consider this one as a parent.
   */
  private returnedValues?: {[resultKey: string]: SearchNode} = {};

  /**
   * Provides a heuristic for the base cost at this path's depth if the best
   * individual input were taken here, regardless of whether or not that's possible.
   */
  readonly lowestPossibleSingleCost: number;

  /**
   * Extends an existing SearchQuotientNode (and its correction data) by a keystroke based
   * on a subset of the incoming keystroke's fat-finger distribution.
   *
   * @param parentNode
   * @param inputs
   * @param inputSource Either:
   * 1.  Data about the actual context range represented by `inputs` and
   * its underlying keystroke.
   * 2.  The sample from the incoming distribution that represents data actually
   * applied to the context.  It need not be included within the subset passed to `inputs`.
   */
  constructor(
    parentNode: SearchQuotientNode,
    inputs: Distribution<Readonly<Transform>>,
    inputSource: PathInputProperties | ProbabilityMass<Transform>
  ) {
    this.spaceId = generateSpaceSeed();

    // Coerce inputSource to TokenInputSource format.
    if(inputSource && (inputSource as ProbabilityMass<Transform>).sample != undefined) {
      const keystroke = inputSource as ProbabilityMass<Transform>;
      inputSource = {
        segment: {
          trueTransform: keystroke.sample,
          transitionId: keystroke.sample.id,
          start: 0
        },
        bestProbFromSet: keystroke.p,
        subsetId: generateSubsetId()
      }
    };
    const inputSrc = inputSource as PathInputProperties;

    const transitionId = (inputs?.[0].sample.id);
    if(transitionId !== undefined && inputSrc?.segment.transitionId != transitionId) {
      throw new Error("Input distribution and input-source transition IDs must match");
    }

    this.parentNode = parentNode;
    this.inputSource = inputSource as PathInputProperties;
    this.lowestPossibleSingleCost = (parentNode?.lowestPossibleSingleCost ?? 0) - Math.log(inputSrc?.bestProbFromSet ?? 1);
    this.inputs = inputs?.length > 0 ? inputs : null;
    this.inputCount = (parentNode?.inputCount ?? 0) + (this.inputs ? 1 : 0);
  }

  /**
   * Retrieves the sequences of inputs that led to this SearchPath.
   */
  public get inputSequence(): Distribution<Transform>[] {
    const parentInputs = this.parentNode?.inputSequence.slice() ?? [];
    const localInputs = this.inputs ? [this.inputs.slice()] : [];
    return parentInputs.concat(localInputs);
  }

  get codepointLength(): number {
    if(this._codepointLength === undefined) {
      this._codepointLength = this.parentNode.codepointLength + this.insertLength - this.leftDeleteLength;
    }

    return this._codepointLength;
  }

  public get lastInput(): Distribution<Readonly<Transform>> {
    // Shallow-copies the array to prevent external modification; the Transforms
    // are marked Readonly to prevent their modification as well.
    return this.inputs ?? [];
  }

  public get bestExample(): {text: string, p: number} {
    const bestPrefix = this.parentNode?.bestExample ?? { text: '', p: 1 };
    const bestLocalInput = this.inputs?.reduce((max, curr) => max.p < curr.p ? curr : max) ?? { sample: { insert: '', deleteLeft: 0 }, p: 1};

    return {
      text: KMWString.substring(bestPrefix.text, 0, (this.parentNode?.codepointLength ?? 0) - bestLocalInput.sample.deleteLeft) + bestLocalInput.sample.insert,
      p: bestPrefix.p * bestLocalInput.p
    }
  }

  get parents() {
    // The SearchPath class may only have a single parent.
    return this.parentNode ? [this.parentNode] : [];
  }

  increaseMaxEditDistance() {
    this.parentNode.increaseMaxEditDistance();

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
    return this.parentNode?.correctionsEnabled || this.inputs?.length > 1;
  }

  public get currentCost(): number {
    const parentCost = this.parentNode?.currentCost ?? Number.POSITIVE_INFINITY;
    const localCost = this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;

    return Math.min(localCost, parentCost);
  }

  protected abstract buildEdgesForNodes(baseNodes: ReadonlyArray<SearchNode>): SearchNode[];

  protected queueNodes(nodes: SearchNode[]) {
    this.selectionQueue.enqueueAll(nodes);
  }

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the selection queue,
   * checks its validity as a correction to the input text, and reports on what
   * sort of result the edge's destination node represents.
   * @returns
   */
  public handleNextNode(): PathResult {
    const parentCost = this.parentNode?.currentCost ?? Number.POSITIVE_INFINITY;
    const localCost = this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;

    if(parentCost <= localCost) {
      if(parentCost == Number.POSITIVE_INFINITY) {
        return {
          type: 'none'
        };
      }

      const result = this.parentNode.handleNextNode();

      if(result.type == 'complete') {
        this.queueNodes(this.buildEdgesForNodes([result.finalNode]));
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

  public get inputSegments(): PathInputProperties[] {
    if(!this.parentNode) {
      return [];
    }

    const parentSources = this.parentNode.inputSegments;
    if(this.inputSource) {
      const inputId = this.inputSource.segment.transitionId;
      if(inputId && parentSources.length > 0 && parentSources[parentSources.length - 1].segment.transitionId == inputId) {
        return parentSources;
      }

      parentSources.push(this.inputSource);
    }

    return parentSources;
  }

  /**
   * Gets a compact string-based representation of `inputRange` that
   * maps compatible token source ranges to each other.
   */
  get sourceRangeKey(): string {
    const components: string[] = [];
    const sources = this.inputSegments;

    for(const source of sources) {
      const i = source.segment.start;
      components.push(`T${source.segment.transitionId}${i != 0 ? '@' + i : ''}`);
    }

    return components.join('+');
  }
}