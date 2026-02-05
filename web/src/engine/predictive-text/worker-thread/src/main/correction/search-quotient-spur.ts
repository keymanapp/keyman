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
import { buildMergedTransform } from '@keymanapp/models-templates';

import { EDIT_DISTANCE_COST_SCALE, SearchNode, SearchResult } from './distance-modeler.js';
import { generateSpaceSeed, InputSegment, PathInputProperties, PathResult, SearchQuotientNode } from './search-quotient-node.js';
import { generateSubsetId } from './tokenization-subsets.js';
import { SearchQuotientRoot } from './search-quotient-root.js';
import { LegacyQuotientRoot } from './legacy-quotient-root.js';

import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
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
    this.inputSource = inputSrc;
    this.lowestPossibleSingleCost = parentNode.lowestPossibleSingleCost - Math.log(inputSrc?.bestProbFromSet ?? 1);
    this.inputs = inputs?.length > 0 ? inputs : null;
    this.inputCount = parentNode.inputCount + (this.inputs ? 1 : 0);
  }

  public get model(): LexicalModel {
    return this.parentNode.model;
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
    const bestPrefix = this.parentNode.bestExample ?? { text: '', p: 1 };
    const bestLocalInput = this.inputs?.reduce((max, curr) => max.p < curr.p ? curr : max) ?? { sample: { insert: '', deleteLeft: 0 }, p: 1};

    return {
      // Take the parent node's result, apply delete-lefts, then apply our most
      // likely local insert.
      text: KMWString.substring(bestPrefix.text, 0, this.parentNode.codepointLength - bestLocalInput.sample.deleteLeft) + bestLocalInput.sample.insert,
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

  /**
   * Allows construction of new spur instances matching this spur's edge type.
   *
   * Target use cases:
   * - `SearchQuotientNode.split()`
   *     - an edge may need to be split into two parts
   *     - edges may need to be recreated on a shortened search path (for the
   *       split's right-hand side)
   * - `SearchQuotientNode.merge()`
   *     - two parts may need to be recombined into a single edge
   *     - edges from the 'right-hand side' may need to be recreated on the
   *       left-hand side for the merged quotient path
   * @param parentNode
   * @param inputs
   * @param inputSource
   */
  abstract construct(
    parentNode: SearchQuotientNode,
    inputs: Distribution<Transform>,
    inputSource: PathInputProperties
  ): this;

  // spaces are in sequence here.
  // `this` = head 'space'.
  public merge(space: SearchQuotientNode): SearchQuotientNode {
    // Head node for the incoming path is empty, so skip it.
    if(space.parents.length == 0 || space instanceof SearchQuotientRoot) {
      return this;
    }

    // Merge any parents first as a baseline.  We have to come after their
    // affects are merged in, anyway.
    const parentMerges = space.parents?.length > 0 ? space.parents.map((p) => this.merge(p)) : [this];

    // if parentMerges.length > 0, is a SearchCluster.
    const parentMerge = parentMerges[0];

    // Special case:  if we've reached the head of the space to be merged, check
    // for a split transform.
    //  - we return `this` from the root, so if that's what we received, we're
    //    on the first descendant - the first path component.
    if(space instanceof SearchQuotientSpur) {
      if(parentMerge != this) {
        // Here, we reconstruct the child `space` on a new root.  The new
        // instance needs to be of the same type as the original instance.
        return space.construct(parentMerge, space.inputs, space.inputSource);
      }

      const localInputId = this.inputSource?.segment.transitionId;
      const spaceInputId = space.inputSource?.segment.transitionId;
      // The 'id' may be undefined in some unit tests and for tokens
      // reconstructed after a backspace.  In either case, we consider the
      // related results as fully separate; our reconstructions are
      // per-codepoint.
      if(localInputId != spaceInputId || localInputId === undefined) {
        return space.construct(parentMerge, space.inputs, space.inputSource);
      }
      // Get the twin halves that were split.
      // Assumption:  the two halves are in their original order, etc.
      const localInputs = this.inputs;
      const spaceInputs = space.inputs;

      // Sanity check - ensure that the input distributions have the same length;
      // if not, this shouldn't represent a SearchPath split!
      if(localInputs.length != spaceInputs.length) {
        return space.construct(parentMerge, space.inputs, space.inputSource);
      }

      // Merge them!
      const mergedInputs = localInputs?.map((entry, index) => {
        return {
          sample: buildMergedTransform(entry.sample, spaceInputs[index].sample),
          p: entry.p
        }
      });

      const mergedInputSource = {
        ...this.inputSource,
        segment: {
          ...this.inputSource.segment,
          end: space.inputSource.segment.end
        }
      };

      if(mergedInputSource.segment.end == undefined) {
        delete mergedInputSource.segment.end;
      }

      // Now to re-merge the two halves.
      return space.construct(this.parentNode, mergedInputs, mergedInputSource);
    } else {
      // If the parent was a cluster, the cluster itself is the merge.
      return parentMerge;
    }
  }

  public split(charIndex: number): [SearchQuotientNode, SearchQuotientNode] {
    const internalSplitIndex = charIndex - (this.codepointLength - this.insertLength);

    if(internalSplitIndex <= 0 && this.parents[0]) {
      const parentResults = this.parents[0].split(charIndex);
      return [parentResults[0], this.construct(parentResults[1], this.inputs, this.inputSource)];
    } else if(charIndex >= this.codepointLength) {
      // this instance = 'first set'
      // second instance:  empty transforms.
      //
      // stopgap:  maybe go ahead and check each input for any that are longer?
      // won't matter shortly, though.
      return [this, new LegacyQuotientRoot(this.model)];
    } else {
      const firstSet: Distribution<Transform> = this.inputs.map((input) => ({
        // keep insert head
        // keep deleteLeft
        sample: {
          ...input.sample,
          insert: KMWString.substring(input.sample.insert, 0, internalSplitIndex),
          deleteRight: 0
        }, p: input.p
      }));

      const secondSet: Distribution<Transform> = this.inputs.map((input) => ({
        // keep insert tail
        // deleteLeft == 0
        sample: {
          ...input.sample,
          insert: KMWString.substring(input.sample.insert, internalSplitIndex),
          deleteLeft: 0
        }, p: input.p
      }));

      // If the transform to be split... isn't actually split (even for delete-lefts),
      // don't append any part of it to the parent; it's actually clean.
      const hasActualSplit = internalSplitIndex > 0 || this.inputs?.[0].sample.deleteLeft > 0;
      const parent = hasActualSplit
        ? this.construct(this.parentNode, firstSet, {
          ...this.inputSource,
          segment: {
            ...this.inputSource.segment,
            end: this.inputSource.segment.start + internalSplitIndex
          }
        })
        : this.parentNode;
      // construct two SearchPath instances based on the two sets!
      return [
        parent,
        this.construct(new LegacyQuotientRoot(this.model), secondSet, {
          ...this.inputSource,
          segment: {
            ...this.inputSource.segment,
            start: this.inputSource.segment.start + internalSplitIndex
          }
        })
      ];
    }
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

  public get inputSegments(): InputSegment[] {
    if(!this.parentNode) {
      return [];
    }

    const segments = this.parentNode.inputSegments;
    if(this.inputSource) {
      const inputId = this.inputSource.segment.transitionId;
      if(inputId !== undefined && segments.length > 0 && segments[segments.length - 1].transitionId == inputId) {
        // Fuse the input sources!
        const tailSegment = segments.pop();
        // Deep-copy the object and replace the segment end value.
        const extendedTailSegment = {...tailSegment, end: this.inputSource.segment.end};
        if(extendedTailSegment.end === undefined) {
          delete extendedTailSegment.end;
        }
        segments.push(extendedTailSegment);
      } else {
        segments.push(this.inputSource.segment);
      }
    }

    return segments;
  }

  /**
   * Gets a compact string-based representation of `inputRange` that
   * maps compatible token source ranges to each other.
   */
  get sourceRangeKey(): string {
    const components: string[] = [];
    const segments = this.inputSegments;

    for(const segment of segments) {
      const i = segment.start;
      const j = segment.end;
      let component = (`T${segment.transitionId}`);

      const parentSegs = this.parentNode.inputSegments;
      // It is possible for an .end to be 0 after a split - if an input's
      // left-deletions are applied without applying any of its insert string.
      const midInputStart = i != 0 || parentSegs[parentSegs.length - 1]?.end !== undefined;

      // If there's an entry for end, always include the start position.  Also
      // include the start position if the range for the source starts after
      // index 0.
      if(j !== undefined) {
        component = `${component}@${i}-${j}`;
      } else if(midInputStart) {
        component = `${component}@${i}`;
      }
      components.push(component);
    }

    return components.join('+');
  }
}