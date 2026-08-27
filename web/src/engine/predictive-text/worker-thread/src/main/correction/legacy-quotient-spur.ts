/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-09
 *
 * This file defines tests for the predictive-text engine's SearchPath class,
 * which is used to manage the search-space(s) for text corrections within the
 * engine.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';
import { KMWString, PriorityQueue } from 'keyman/common/web-utils';

import { CORRECTION_QUEUE_COMPARATOR, PathResult } from './correction-searchable.js';
import { SearchNode } from './distance-modeler.js';
import { SearchQuotientNode, PathInputProperties } from './search-quotient-node.js';
import { SearchQuotientSpur } from './search-quotient-spur.js';
import { TokenResultMapping } from './token-result-mapping.js';

import Distribution = LexicalModelTypes.Distribution;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class LegacyQuotientSpur extends SearchQuotientSpur {
  private transposeQueue: PriorityQueue<SearchNode> = new PriorityQueue(CORRECTION_QUEUE_COMPARATOR);
  private incomingTransposeRootNodes: TokenResultMapping[] = [];

  public readonly insertLength: number;
  public readonly leftDeleteLength: number;

  /**
   * Constructs a fresh SearchQuotientNode instance for use in predictive-text
   * correction and suggestion searches.
   * @param space
   * @param inputs
   * @param bestProbFromSet
   */
  constructor(space: SearchQuotientNode, inputs: Distribution<Transform>, inputSource: PathInputProperties | ProbabilityMass<Transform>) {
    // Compute this SearchPath's codepoint length & edge length.
    const inputSample = inputs?.[0].sample ?? { insert: '', deleteLeft: 0 };
    const insertLength = KMWString.length(inputSample.insert);

    const codepointLength = space.codepointLength + insertLength - inputSample.deleteLeft;

    super(space, inputs, inputSource, codepointLength);
    this.insertLength = insertLength;
    this.leftDeleteLength = inputSample.deleteLeft;

    // Link to the grandparent node if it exists; transposes start construction rooted there.
    const grandparentNode = this.parents[0].parents[0]
    if(grandparentNode) {
      this.incomingTransposeRootNodes = [...grandparentNode.previousResults];
      this.linkAndQueueFromParent(grandparentNode, this.incomingTransposeRootNodes);
    }
  }

  construct(parentNode: SearchQuotientNode, inputs?: Distribution<Transform>, inputSource?: PathInputProperties): this {
    return new LegacyQuotientSpur(parentNode, inputs, inputSource) as this;
  }

  protected buildEdgesFromResults(priorResults: ReadonlyArray<TokenResultMapping>, inputs?: Distribution<Transform>): SearchNode[] {
    const edgeInputs = inputs ?? this.inputs;

    // With a newly-available input, we can extend new input-dependent paths from
    // our previously-reached 'extractedResults' nodes.
    let outboundNodes = priorResults.map((result) => {
      // Hard restriction:  no further edits will be supported.  This helps keep the search
      // more narrowly focused.
      const substitutionsOnly = result.editCount == 2;

      let deletionEdges: SearchNode[] = [];
      if(!substitutionsOnly) {
        deletionEdges         = result.buildDeletionEdges(edgeInputs, this.spaceId);
      }
      const substitutionEdges = result.buildSubstitutionEdges(edgeInputs, this.spaceId);

      // Skip the queue for the first pass; there will ALWAYS be at least one pass,
      // and queue-enqueing does come with a cost - avoid unnecessary overhead here.
      return substitutionEdges.flatMap(e => e.processSubsetEdge()).concat(deletionEdges);
    }).flat();

    return outboundNodes;
  }

  get currentCost() {
    const defaultCost = super.currentCost;
    const transposeCost = this.transposeQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;

    return Math.min(transposeCost, defaultCost);
  }

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the selection queue,
   * checks its validity as a correction to the input text, and reports on what
   * sort of result the edge's destination node represents.
   * @returns
   */
  public handleNextNode(): PathResult<TokenResultMapping> {
    this.processPendingRoots();
    const transposeCost = this.transposeQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;

    // Handle transposition cases
    if(transposeCost < super.currentCost) {
      let currentNode = this.transposeQueue.dequeue();

      let unmatchedResult: PathResult<TokenResultMapping> = {
        type: 'intermediate',
        cost: currentNode.currentCost
      }

      // Stage 1:  filter out nodes/edges we want to prune

      // Forbid a raw edit-distance of greater than 2.
      // Note:  .knownCost is not scaled, while its contribution to .currentCost _is_ scaled.
      if(currentNode.editCount > 2) {
        return unmatchedResult;
      }

      // Stage 2:  process subset further OR build remaining edges

      if(currentNode.hasPartialInput) {
        // Re-use the current queue; the number of total inputs considered still holds.
        this.transposeQueue.enqueueAll(currentNode.processSubsetEdge());
        return unmatchedResult;
      }

      // If here, we've properly done the first half of a transpose.  Now for the other half...

      // const transposeSecondHalfNodes = currentNode.buildSubstitutionEdges((this.parents[0] as LegacyQuotientSpur).inputs, this.spaceId);
      const transposeSecondHalfNodes = this.buildEdgesFromResults([new TokenResultMapping(this, currentNode)], (this.parents[0] as LegacyQuotientSpur).inputs);
      this.queueNodes(transposeSecondHalfNodes);
      return unmatchedResult;
    }

    const result = super.handleNextNode();

    if(result.type == 'complete') {
      const parentResult = result.mapping;

      // Forbid a raw edit-distance of greater than 2.
      // Note:  .knownCost is not scaled, while its contribution to .currentCost _is_ scaled.
      if(parentResult.editCount < 2) {
        let insertionEdges = parentResult.buildInsertionEdges();
        this.queueNodes(insertionEdges);
      }
    }

    return result;
  }

  protected processPendingRoots(): void {
    super.processPendingRoots();

    while(this.incomingTransposeRootNodes.length > 0) {
      // Build only substitution edges from these.
      const transpositionFirstHalves = this.incomingTransposeRootNodes.pop().buildSubstitutionEdges(this.inputs, this.spaceId);
      transpositionFirstHalves.forEach((n) => n.addEdit());
      this.transposeQueue.enqueueAll(transpositionFirstHalves);
    }
  }
}