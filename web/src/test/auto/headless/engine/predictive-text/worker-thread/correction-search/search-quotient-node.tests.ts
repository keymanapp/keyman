/**
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-09
 *
 * This file adds unit tests for verifying core mechanics of the
 * SearchQuotientNode set of types.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { generateSpaceSeed, InputSegment, models, PathResult, SearchNode, SearchQuotientNode, SearchQuotientRoot, TokenResultMapping } from '@keymanapp/lm-worker/test-index';

import LexicalModel = LexicalModelTypes.LexicalModel;
import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

export class MockQuotientNode extends SearchQuotientNode {
  /* This section is the part relevant for SearchQuotientNode-specific testing. */
  public readonly receivedResults: TokenResultMapping[] = [];
  private _spaceId: number;

  constructor(parent: SearchQuotientNode) {
    super();
    if(parent) {
      this.linkAndQueueFromParent(parent, this.receivedResults);
    }
  }

  mockResult(resultNode: SearchNode) {
    // Temporarily set ._spaceId so that TokenResultMapping construction may proceed.
    this._spaceId = resultNode.spaceId;
    this.saveResult(new TokenResultMapping(this, resultNode));
    this._spaceId = undefined;
  }

  get spaceId(): number {
    return this._spaceId;
  }

  /* End relevant section */

  // The rest of this is simply just... implementing the abstract methods so
  // that the type signature is satisfied.
  get model(): LexicalModel {
    throw new Error('Method not implemented.');
  }
  get parents(): SearchQuotientNode[] {
    throw new Error('Method not implemented.');
  }
  handleNextNode(): PathResult<TokenResultMapping> {
    throw new Error('Method not implemented.');
  }
  increaseMaxEditDistance(): void {
    throw new Error('Method not implemented.');
  }
  get currentCost(): number {
    throw new Error('Method not implemented.');
  }
  lowestPossibleSingleCost: number;
  correctionsEnabled: boolean;
  inputCount: number;
  codepointLength: number;
  bestExample: { text: string; p: number; };
  inputSegments: InputSegment[];
  get sourceRangeKey(): string {
    throw new Error('Method not implemented.');
  }
  merge(space: SearchQuotientNode): SearchQuotientNode {
    throw new Error('Method not implemented.');
  }
  split(charIndex: number): [SearchQuotientNode, SearchQuotientNode][] {
    throw new Error('Method not implemented.');
  }
  isSameNode(node: SearchQuotientNode): boolean {
    throw new Error('Method not implemented.');
  }
}

describe('SearchQuotientNode', () => {
  it('propagates node search results to linked descendants', () => {
    const root = new SearchQuotientRoot(testModel);

    const baseNode = new MockQuotientNode(root);
    const descendants = [
      new MockQuotientNode(baseNode),
      new MockQuotientNode(baseNode),
      new MockQuotientNode(baseNode)
    ];

    const rootPath = new SearchNode(testModel.traverseFromRoot(), generateSpaceSeed());
    const path1 = rootPath.buildSubstitutionEdges(
      [{
        sample: {
          insert: 't',
          deleteLeft: 0,
          deleteRight: 0
        },
        p: 1.0
      }],
      13
    ).flatMap((n) => n.processSubsetEdge())[0];
    const path2 = rootPath.buildSubstitutionEdges(
      [{
        sample: {
          insert: 'a',
          deleteLeft: 0,
          deleteRight: 0
        },
        p: 1.0
      }],
      13
    ).flatMap((n) => n.processSubsetEdge())[0];
    baseNode.mockResult(path1);
    baseNode.mockResult(path2);

    // Check that each descendant received the node.
    descendants.forEach((qn) => assert.sameMembers(qn.receivedResults.map((r) => r.spaceId), [path1.spaceId, path2.spaceId]));
  });
});