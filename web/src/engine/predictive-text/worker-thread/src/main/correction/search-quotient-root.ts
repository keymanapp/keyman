
import { LexicalModelTypes } from '@keymanapp/common-types';

import { SearchNode, SearchResult } from './distance-modeler.js';
import { generateSpaceSeed, PathResult, SearchQuotientNode } from './search-quotient-node.js';

import LexicalModel = LexicalModelTypes.LexicalModel;

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchQuotientRoot implements SearchQuotientNode {
  readonly rootNode: SearchNode;
  private readonly rootResult: SearchResult;

  readonly lowestPossibleSingleCost: number = 0;

  readonly inputCount: number = 0;
  readonly correctionsEnabled: boolean = false;

  private hasBeenProcessed: boolean = false;

  /**
   * Constructs a fresh SearchSpace instance for used in predictive-text correction
   * and suggestion searches.
   * @param baseSpaceId
   * @param model
   */
  constructor(model: LexicalModel) {
    this.rootNode = new SearchNode(model.traverseFromRoot(), generateSpaceSeed(), t => model.toKey(t));
    this.rootResult = new SearchResult(this.rootNode);
  }

  get spaceId(): number {
    return this.rootNode.spaceId;
  }

  hasInputs(keystrokeDistributions: LexicalModelTypes.Distribution<LexicalModelTypes.Transform>[]): boolean {
    return keystrokeDistributions.length == 0;
  }

  // Return a new array each time; avoid aliasing potential!
  get parents(): SearchQuotientNode[] {
    return [];
  }

  // Return a new array each time; avoid aliasing potential!
  get inputSequence(): LexicalModelTypes.Distribution<LexicalModelTypes.Transform>[] {
    return [];
  }

  // Return a new instance each time; avoid aliasing potential!
  get bestExample(): { text: string; p: number; }  {
    return { text: '', p: 1 };
  }

  increaseMaxEditDistance(): void {
    this.rootNode.calculation = this.rootNode.calculation.increaseMaxDistance();
  }

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the selection queue,
   * checks its validity as a correction to the input text, and reports on what
   * sort of result the edge's destination node represents.
   * @returns
   */
  public handleNextNode(): PathResult {
    if(this.hasBeenProcessed) {
      return null;
    }

    this.hasBeenProcessed = true;

    return {
      type: 'complete',
      cost: 0,
      finalNode: this.rootNode,
      spaceId: this.spaceId
    };
  }

  public get currentCost(): number {
    return this.hasBeenProcessed ? Number.POSITIVE_INFINITY : 0;
  }

  get previousResults(): SearchResult[] {
    if(!this.hasBeenProcessed) {
      return [];
    } else {
      return [this.rootResult];
    }
  }
}