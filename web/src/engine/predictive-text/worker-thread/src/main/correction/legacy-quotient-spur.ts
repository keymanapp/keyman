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
import { KMWString } from '@keymanapp/web-utils';

import { SearchNode } from './distance-modeler.js';
import { PathResult, SearchQuotientNode, PathInputProperties } from './search-quotient-node.js';
import { SearchQuotientSpur } from './search-quotient-spur.js';

import Distribution = LexicalModelTypes.Distribution;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class LegacyQuotientSpur extends SearchQuotientSpur {
  protected readonly insertLength: number;
  protected readonly leftDeleteLength: number;

  /**
   * Constructs a fresh SearchQuotientNode instance for use in predictive-text
   * correction and suggestion searches.
   * @param space
   * @param inputs
   * @param bestProbFromSet
   */
  constructor(space: SearchQuotientNode, inputs: Distribution<Transform>, inputSource: PathInputProperties | ProbabilityMass<Transform>) {
    super(space, inputs, inputSource);
    this.queueNodes(this.buildEdgesForNodes(space.previousResults.map(r => r.node)));

    // Compute this SearchPath's codepoint length & edge length.
    const insert = this.inputs?.[0].sample.insert ?? '';
    this.insertLength = KMWString.length(insert);

    this.leftDeleteLength = this.inputs?.[0].sample.deleteLeft ?? 0;
    return;
  }

  protected construct(parentNode: SearchQuotientNode, inputs?: Distribution<Transform>, inputSource?: PathInputProperties): this {
    return new LegacyQuotientSpur(parentNode, inputs, inputSource) as this;
  }

  protected buildEdgesForNodes(baseNodes: ReadonlyArray<SearchNode>) {
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

    return outboundNodes;
  }

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the selection queue,
   * checks its validity as a correction to the input text, and reports on what
   * sort of result the edge's destination node represents.
   * @returns
   */
  public handleNextNode(): PathResult {
    const result = super.handleNextNode();

    if(result.type == 'complete') {
      const currentNode = result.finalNode;

      // Forbid a raw edit-distance of greater than 2.
      // Note:  .knownCost is not scaled, while its contribution to .currentCost _is_ scaled.
      if(currentNode.editCount < 2) {
        let insertionEdges = currentNode.buildInsertionEdges();
        this.queueNodes(insertionEdges);
      }
    }

    return result;
  }
}