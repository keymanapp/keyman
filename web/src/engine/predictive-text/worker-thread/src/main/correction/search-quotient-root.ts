
import { LexicalModelTypes } from '@keymanapp/common-types';

import { SearchNode } from './distance-modeler.js';
import { SearchQuotientSpur } from './search-quotient-spur.js';
import { PathResult } from './search-quotient-node.js';

import LexicalModel = LexicalModelTypes.LexicalModel;

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchQuotientRoot extends SearchQuotientSpur {
  readonly rootNode: SearchNode;

  /**
   * Constructs a fresh SearchSpace instance for used in predictive-text correction
   * and suggestion searches.
   * @param baseSpaceId
   * @param model
   */
  constructor(model: LexicalModel) {
    super(/* parent */ null, /* inputs */ null, /* cost heuristic */ 0);
    this.rootNode = new SearchNode(model.traverseFromRoot(), this.spaceId, t => model.toKey(t));
    this.queueNodes([this.rootNode]);
  }

  protected buildEdgesForNodes(baseNodes: ReadonlyArray<SearchNode>): SearchNode[] {
    return [];
  }

  // TODO:  Remove when removing LegacyQuotientSpur!
  // At that time, inserts should have their own devoted 'Spur' type and not be managed
  // within the same pre-existing instance.
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
      // Note:  .knownCost is not scaled, while its contribution to .currentCost _is_ scaled.      const currentNode = result.finalNode;
      if(currentNode.editCount < 2) {
        let insertionEdges = currentNode.buildInsertionEdges();
        this.queueNodes(insertionEdges);
      }
    }

    return result;
  }
}