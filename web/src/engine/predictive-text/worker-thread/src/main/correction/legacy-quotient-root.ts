import { PriorityQueue } from '@keymanapp/web-utils';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { PathResult } from './correction-searchable.js';
import { SearchQuotientNode } from './search-quotient-node.js';
import { SearchQuotientRoot } from './search-quotient-root.js';
import { QUEUE_NODE_COMPARATOR } from './search-quotient-spur.js';
import { SearchNode } from './distance-modeler.js';

import LexicalModel = LexicalModelTypes.LexicalModel;
import { TokenResultMapping } from './token-result-mapping.js';

export class LegacyQuotientRoot extends SearchQuotientRoot {
  private selectionQueue: PriorityQueue<SearchNode> = new PriorityQueue(QUEUE_NODE_COMPARATOR);
  private processed: SearchNode[] = [];

  constructor(model: LexicalModel) {
    super(model);

    this.selectionQueue.enqueue(this.rootNode);
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
  public handleNextNode(): PathResult<TokenResultMapping> {
    const node = this.selectionQueue.dequeue();

    if(!node) {
      return {
        type: 'none'
      };
    }

    // The legacy variant includes 'insert' operations!
    if(node.editCount < 2) {
      let insertionEdges = node.buildInsertionEdges();
      this.selectionQueue.enqueueAll(insertionEdges);
    }

    this.processed.push(node);
    return {
      type: 'complete',
      cost: node.currentCost,
      mapping: new TokenResultMapping(node, this),
      spaceId: this.spaceId
    };
  }

  public get currentCost(): number {
    return this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;
  }

  get previousResults(): TokenResultMapping[] {
    return this.processed.map((n) => new TokenResultMapping(n, this));
  }

  split(charIndex: number): [SearchQuotientNode, SearchQuotientNode][] {
    return [[this, new LegacyQuotientRoot(this.model)]];
  }
}