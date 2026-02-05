
import { LexicalModelTypes } from '@keymanapp/common-types';

import { SearchNode, SearchResult } from './distance-modeler.js';
import { generateSpaceSeed, PathInputProperties, PathResult, SearchQuotientNode } from './search-quotient-node.js';
import { SearchQuotientSpur } from './search-quotient-spur.js';

import LexicalModel = LexicalModelTypes.LexicalModel;

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchQuotientRoot implements SearchQuotientNode {
  readonly rootNode: SearchNode;
  readonly model: LexicalModel;
  private readonly rootResult: SearchResult;

  readonly lowestPossibleSingleCost: number = 0;

  readonly inputCount: number = 0;
  readonly codepointLength: number = 0;
  readonly correctionsEnabled: boolean = false;

  private hasBeenProcessed: boolean = false;

  /**
   * Constructs a fresh SearchQuotientRoot instance to be used as the root of
   * the predictive-text correction / suggestion search process.
   * @param baseSpaceId
   * @param model
   */
  constructor(model: LexicalModel) {
    this.rootNode = new SearchNode(model.traverseFromRoot(), generateSpaceSeed(), t => model.toKey(t));
    this.model = model;
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

  // Return a new array each time; avoid aliasing potential!
  get inputSegments(): PathInputProperties[] {
    return [];
  }

  get sourceRangeKey(): string {
    return '';
  }

  split(charIndex: number): [SearchQuotientNode, SearchQuotientNode] {
    return [this, new SearchQuotientRoot(this.model)];
  }

  merge(space: SearchQuotientNode): SearchQuotientNode {
    // Head node for the incoming path is empty, so skip it.
    if(space.parents.length == 0 || space instanceof SearchQuotientRoot) {
      return this;
    }

    // Merge any parents first as a baseline.  We have to come after their
    // affects are merged in, anyway.
    const parentMerges = space.parents?.length > 0 ? space.parents.map((p) => this.merge(p)) : [this];

    // if parentMerges.length > 0, is a SearchCluster.
    // const parentMerge = parentMerges.length > 0 ? new SearchCluster(parentMerges) : parentMerges[0];
    const parentMerge = parentMerges[0];

    // Special case:  if we've reached the head of the space to be merged, check
    // for a split transform.
    //  - we return `this` from the root, so if that's what we received, we're
    //    on the first descendant - the first path component.
    if(space instanceof SearchQuotientSpur) {
      // Needs to construct a NEW version of whatever the same type is, on this root.
      return space.construct(parentMerge, space.inputs, space.inputSource);
    } else {
      // If the parent was a cluster, the cluster itself is the merge.
      return parentMerge;
    }
  }
}