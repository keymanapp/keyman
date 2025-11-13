import { PriorityQueue } from '@keymanapp/web-utils';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { SearchQuotientRoot } from './search-quotient-root.js';
import { QUEUE_NODE_COMPARATOR } from './search-quotient-spur.js';
import { SearchNode, SearchResult } from './distance-modeler.js';

import LexicalModel = LexicalModelTypes.LexicalModel;
import { PathResult, SearchQuotientNode } from './search-quotient-node.js';

export class LegacyQuotientRoot extends SearchQuotientRoot {
  private selectionQueue: PriorityQueue<SearchNode> = new PriorityQueue(QUEUE_NODE_COMPARATOR);
  private processed: SearchResult[] = [];

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
  public handleNextNode(): PathResult {
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

    this.processed.push(new SearchResult(node));
    this.bufferNode(node);
    return {
      type: 'complete',
      cost: node.currentCost,
      finalNode: node,
      spaceId: this.spaceId
    };
  }

  public get currentCost(): number {
    return this.selectionQueue.peek()?.currentCost ?? Number.POSITIVE_INFINITY;
  }

  get previousResults(): SearchResult[] {
    return this.processed.slice();
  }

  split(charIndex: number): [SearchQuotientNode, SearchQuotientNode][] {
    return [[this, new LegacyQuotientRoot(this.model)]];
  }
}