/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2020-08-27
 *
 * This file defines tests for core structures and methods of the
 * predictive-text correction-search engine.
 */

import { assert } from 'chai';

import { PriorityQueue } from '@keymanapp/web-utils';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { LexicalModelTypes } from '@keymanapp/common-types';

import { correction, models, QUEUE_NODE_COMPARATOR } from '@keymanapp/lm-worker/test-index';

import SENTINEL_CODE_UNIT = models.SENTINEL_CODE_UNIT;
import Distribution = LexicalModelTypes.Distribution;
import SearchNode = correction.SearchNode;
import Transform = LexicalModelTypes.Transform;
import TrieModel = models.TrieModel;

// Exposes an extra property that's very useful for validating behaviors during unit testing:
// - `prefix`.
type TrieTraversal = ReturnType<TrieModel['traverseFromRoot']>;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));
const toKey = (s: string) => testModel.toKey(s);

/**
 * The total number of top-level child paths in the lexicon.
 */
const FIRST_CHAR_VARIANTS = 24;

let SEARCH_EDGE_SEED = 0;

function assertEdgeChars(edge: correction.SearchNode, input: string, match: string) {
  assert.isTrue(edgeHasChars(edge, input, match));
}

function lastEntry<T>(arr: readonly T[]): T {
  return arr.slice().pop();
}

function edgeHasChars(edge: correction.SearchNode, input: string, match: string) {
  if(edge.priorInput[edge.priorInput.length - 1].sample.insert != input) {
    return false;
  }

  return lastEntry(edge.calculation.matchSequence) == match;
}

function findEdgesWithChars(edgeArray: correction.SearchNode[], match: string) {
  let results = edgeArray.filter(function(value) {
    return lastEntry(value.calculation.matchSequence) == match;
  });

  assert.isAtLeast(results.length, 1);
  return results;
}

function fetchCommonTENode() {
  const rootSeed = SEARCH_EDGE_SEED++;
  const rootNode = new SearchNode(testModel.traverseFromRoot(), rootSeed, toKey);

  // Establish desired source node:  prefix 'te'.
  const firstLayerTransforms: Distribution<Transform> = [{
    sample: {
      insert: 't',
      deleteLeft: 0
    },
    p: 0.75
  }, {
    sample: {
      insert: 'r',
      deleteLeft: 0
    },
    p: 0.25
  }];
  const firstLayerSeed = SEARCH_EDGE_SEED++;
  const firstLayerNodes = rootNode
    .buildSubstitutionEdges(firstLayerTransforms, firstLayerSeed)
    .flatMap(node => node.processSubsetEdge());
  assert.isAbove(firstLayerNodes.length, FIRST_CHAR_VARIANTS);
  firstLayerNodes.sort((a, b) => a.currentCost - b.currentCost);

  const tNode = firstLayerNodes[0];
  assert.equal(tNode.resultKey, 't');
  assert.equal(tNode.spaceId, firstLayerSeed);
  assert.sameDeepMembers(tNode.priorInput, [firstLayerTransforms[0]]);
  assert.isFalse(tNode.hasPartialInput);

  const secondLayerTransforms: Distribution<Transform> = [{
    sample: {
      insert: 'e',
      deleteLeft: 0
    },
    p: 0.8
  }, {
    sample: {
      insert: 'r',
      deleteLeft: 0
    },
    p: 0.2
  }];
  const secondLayerSeed = SEARCH_EDGE_SEED++;
  const secondLayerNodes = tNode
    .buildSubstitutionEdges(secondLayerTransforms, secondLayerSeed)
    .flatMap(node => node.processSubsetEdge());
  // The field narrows down at this point, but still has a decent number
  // of variants (11).
  assert.isAbove(secondLayerNodes.length, 10);
  secondLayerNodes.sort((a, b) => a.currentCost - b.currentCost);

  const teNode = secondLayerNodes[0];

  assert.equal(teNode.resultKey, 'te');
  assert.equal(teNode.editCount, 0);
  assert.equal(teNode.spaceId, secondLayerSeed);

  return teNode;
}

describe('Correction Distance Modeler', () => {
  describe('SearchNode', () => {
    it('constructs a fresh instance from a traversal + keyingFunction', () => {
      const rootSeed = SEARCH_EDGE_SEED++;
      const rootNode = new SearchNode(testModel.traverseFromRoot(), rootSeed, toKey);
      assert.equal(rootNode.resultKey, '');

      assert.equal(rootNode.editCount, 0);
      assert.equal(rootNode.inputSamplingCost, 0);
      assert.equal(rootNode.currentCost, 0);

      assert.equal((rootNode.currentTraversal as TrieTraversal).prefix, '');
      assert.isFalse(rootNode.hasPartialInput);
      assert.isFalse(rootNode.isFullReplacement)
      assert.isUndefined(lastEntry(rootNode.calculation.inputSequence));
      assert.isUndefined(lastEntry(rootNode.calculation.matchSequence));
      assert.equal(rootNode.toKey, toKey);
      assert.equal(rootNode.spaceId, rootSeed);
    });

    describe('supports the cloning constructor pattern', () => {
      it('properly deep-copies the root node', () => {
        const originalNode = new SearchNode(testModel.traverseFromRoot(), SEARCH_EDGE_SEED++, toKey);
        const clonedNode = new SearchNode(originalNode);

        // Root node properties; may as well re-assert 'em.
        assert.equal(clonedNode.resultKey, '');

        assert.equal(clonedNode.editCount, 0);
        assert.equal(clonedNode.inputSamplingCost, 0);
        assert.equal(clonedNode.currentCost, 0);

        assert.equal((clonedNode.currentTraversal as TrieTraversal).prefix, '');
        assert.isFalse(clonedNode.hasPartialInput);
        assert.isFalse(clonedNode.isFullReplacement)
        assert.isUndefined(lastEntry(clonedNode.calculation.inputSequence));
        assert.isUndefined(lastEntry(clonedNode.calculation.matchSequence));
        assert.equal(clonedNode.toKey, toKey);
        assert.equal(clonedNode.spaceId, originalNode.spaceId);

        // Avoid aliasing for properties holding mutable objects
        assert.notEqual(clonedNode.priorInput, originalNode.priorInput);

        // Verify aliasing for properties holding immutable objects
        assert.equal(clonedNode.calculation, originalNode.calculation);
        assert.equal(clonedNode.currentTraversal, originalNode.currentTraversal);
      });

      it('properly deep-copies fully-processed nodes later in the search path', () => {
        const rootSeed = SEARCH_EDGE_SEED++;
        const rootNode = new SearchNode(testModel.traverseFromRoot(), rootSeed, toKey);

        // Establish desired source node:  prefix 'te'.
        const firstLayerTransforms: Distribution<Transform> = [{
          sample: {
            insert: 't',
            deleteLeft: 0
          },
          p: 0.75
        }, {
          sample: {
            insert: 'r',
            deleteLeft: 0
          },
          p: 0.25
        }];
        const firstLayerNodes = rootNode
          .buildSubstitutionEdges(firstLayerTransforms, SEARCH_EDGE_SEED++)
          .flatMap(node => node.processSubsetEdge());
        assert.isAbove(firstLayerNodes.length, FIRST_CHAR_VARIANTS);
        firstLayerNodes.sort((a, b) => a.currentCost - b.currentCost);

        const tNode = firstLayerNodes[0];
        assert.equal(tNode.resultKey, 't');
        assert.sameDeepMembers(tNode.priorInput, [firstLayerTransforms[0]]);
        assert.isFalse(tNode.hasPartialInput);

        const secondLayerTransforms: Distribution<Transform> = [{
          sample: {
            insert: 'e',
            deleteLeft: 0
          },
          p: 0.8
        }, {
          sample: {
            insert: 'r',
            deleteLeft: 0
          },
          p: 0.2
        }];
        const secondSpaceId = SEARCH_EDGE_SEED++;
        const secondLayerNodes = tNode
          .buildSubstitutionEdges(secondLayerTransforms, secondSpaceId)
          .flatMap(node => node.processSubsetEdge());
        assert.isAbove(secondLayerNodes.length, 10);
        secondLayerNodes.sort((a, b) => a.currentCost - b.currentCost);

        const teNode = secondLayerNodes[0];

        // *****

        function assertSourceNodeProps(node: SearchNode) {
          assert.equal(node.resultKey, 'te');

          assert.equal(node.editCount, 0);
          assert.equal(node.inputSamplingCost, -Math.log(firstLayerTransforms[0].p) - Math.log(secondLayerTransforms[0].p));
          assert.equal(node.currentCost, node.inputSamplingCost);

          assert.isFalse(node.hasPartialInput);
          assert.isFalse(node.isFullReplacement)
          assert.equal(lastEntry(node.calculation.inputSequence), 'e');
          assert.equal(lastEntry(node.calculation.matchSequence), 'e');
          assert.equal((node.currentTraversal as TrieTraversal).prefix, 'te');
          assert.equal(node.toKey, toKey);
          assert.equal(node.spaceId, secondSpaceId);
        }

        assertSourceNodeProps(teNode);

        const clonedNode = new SearchNode(teNode);

        // Root node properties; may as well re-assert 'em.
        assertSourceNodeProps(clonedNode);

        // Avoid aliasing for properties holding mutable objects
        assert.notEqual(clonedNode.priorInput, teNode.priorInput);

        // Verify aliasing for properties holding immutable objects
        assert.equal(clonedNode.calculation, teNode.calculation);
        assert.equal(clonedNode.currentTraversal, teNode.currentTraversal);
      });

      it('properly deep-copies partially-processed edges later in the search path', () => {
        const rootSeed = SEARCH_EDGE_SEED++;
        const rootNode = new SearchNode(testModel.traverseFromRoot(), rootSeed, toKey);

        // Establish desired source node:  prefix 'te', with 'e' the first letter of
        // a multi-char insert Transform.
        const firstLayerTransforms: Distribution<Transform> = [{
          sample: {
            insert: 't',
            deleteLeft: 0
          },
          p: 0.75
        }, {
          sample: {
            insert: 'r',
            deleteLeft: 0
          },
          p: 0.25
        }];
        const firstLayerNodes = rootNode
          .buildSubstitutionEdges(firstLayerTransforms, SEARCH_EDGE_SEED++)
          .flatMap(node => node.processSubsetEdge());
        assert.isAbove(firstLayerNodes.length, FIRST_CHAR_VARIANTS);
        firstLayerNodes.sort((a, b) => a.currentCost - b.currentCost);

        const tNode = firstLayerNodes[0];
        assert.equal(tNode.resultKey, 't');
        assert.sameDeepMembers(tNode.priorInput, [firstLayerTransforms[0]]);
        assert.isFalse(tNode.hasPartialInput);

        const secondLayerTransforms: Distribution<Transform> = [{
          sample: {
            insert: 'ests',
            deleteLeft: 0
          },
          p: 0.8
        }, {
          sample: {
            insert: 'rial',
            deleteLeft: 0
          },
          p: 0.2
        }];

        const secondLayerId = SEARCH_EDGE_SEED++;
        const secondLayerNodes = tNode
          .buildSubstitutionEdges(secondLayerTransforms, secondLayerId)
          .flatMap(node => node.processSubsetEdge());
        assert.isAbove(secondLayerNodes.length, 10);
        secondLayerNodes.sort((a, b) => a.currentCost - b.currentCost);

        const teNode = secondLayerNodes[0];

        // *****

        function assertSourceNodeProps(node: SearchNode) {
          assert.equal(node.resultKey, 'te');

          assert.equal(node.editCount, 0);
          assert.equal(node.inputSamplingCost, -Math.log(firstLayerTransforms[0].p) - Math.log(secondLayerTransforms[0].p));
          assert.equal(node.currentCost, node.inputSamplingCost);

          assert.isTrue(node.hasPartialInput);
          assert.isFalse(node.isFullReplacement)
          assert.equal(lastEntry(node.calculation.inputSequence), 'e');
          assert.equal(lastEntry(node.calculation.matchSequence), 'e');
          assert.equal((node.currentTraversal as TrieTraversal).prefix, 'te');
          assert.equal(node.toKey, toKey);
          assert.equal(node.spaceId, secondLayerId);
        }

        assertSourceNodeProps(teNode);

        const clonedNode = new SearchNode(teNode);

        // Root node properties; may as well re-assert 'em.
        assertSourceNodeProps(clonedNode);

        // Avoid aliasing for properties holding mutable objects
        assert.notEqual(clonedNode.priorInput, teNode.priorInput);

        // Verify aliasing for properties holding immutable objects
        assert.equal(clonedNode.calculation, teNode.calculation);
        assert.equal(clonedNode.currentTraversal, teNode.currentTraversal);
      });
    });

    // Consider adding more, deeper?
    it('builds insertion edges based on lexicon, from root', () => {
      const rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      const rootSeed = SEARCH_EDGE_SEED++;
      const rootNode = new correction.SearchNode(rootTraversal, rootSeed);
      assert.equal(rootNode.calculation.getHeuristicFinalCost(), 0);

      const edges = rootNode.buildInsertionEdges();
      assert.isAbove(edges.length, 0);

      let expectedChildCount = 0;
      for(const child of rootTraversal.children()) {
        expectedChildCount++;

        let childEdge = edges.filter(value => lastEntry(value.calculation.matchSequence) == child.char)[0];
        assert.isOk(childEdge);
        assert.isEmpty(childEdge.priorInput);
        assert.isEmpty(childEdge.calculation.inputSequence);
        assert.isAbove(childEdge.currentCost, 0);
        assert.equal(childEdge.spaceId, rootSeed);
      }

      assert.equal(edges.length, expectedChildCount);
    });

    describe('buildDeletionEdges @ token root', () => {
      const synthDistribution = [
        //              Transform,            probability
        //         insert 1,   deleteLeft  0,  p: 0.50
        {sample: {insert: 't', deleteLeft: 0}, p: 0.30},
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.15},
        {sample: {insert: 'x', deleteLeft: 0}, p: 0.05},
        //        insert  0    deleteLeft  1,  p: 0.1
        {sample: {insert: '',  deleteLeft: 1}, p: 0.1},
        //        insert   2    deleteLeft  0,  p: 0.15
        {sample: {insert: 'ch', deleteLeft: 0}, p: 0.15},
        //        insert   2    deleteLeft  1,  p: 0.25
        {sample: {insert: 'th', deleteLeft: 1}, p: 0.1},
        {sample: {insert: 'tr', deleteLeft: 1}, p: 0.15},
        // 4 distinct subsets.
      ];

      it('step 1: batches deletion edge(s) for input transforms', () => {
        const rootTraversal = testModel.traverseFromRoot();
        assert.isNotEmpty(rootTraversal);

        const rootSeed = SEARCH_EDGE_SEED++;
        const rootNode = new correction.SearchNode(rootTraversal, rootSeed);
        assert.equal(rootNode.calculation.getHeuristicFinalCost(), 0);

        const subsetSeed = SEARCH_EDGE_SEED++;
        const subsetNodes = rootNode.buildDeletionEdges(synthDistribution, subsetSeed);
        assert.equal(subsetNodes.length, 4);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);
        const expectedCosts = [0.5, .25, 0.15, 0.1].map(x => -Math.log(x))
        // The known subs for the subsets defined above.
        for(let i=0; i < expectedCosts.length; i++) {
          assert.isTrue(subsetNodes[i].hasPartialInput);
          // From root: the deleteLeft 1 entries have nothing to delete.
          assert.equal((subsetNodes[i].currentTraversal as TrieTraversal).prefix, '');
          assert.equal(subsetNodes[i].spaceId, subsetSeed);

          // Allow a little value wiggle due to double-precision limitations.
          assert.approximately(subsetNodes[i].inputSamplingCost, expectedCosts[i], 1e-8);
          // No actual edit-tracking is done yet, so these should also match.
          assert.approximately(subsetNodes[i].currentCost, expectedCosts[i], 1e-8);
        }
      });

      it('step 2: first processing layer resolves zero + one char inserts', () => {
        // From "step 1" above, assertions removed
        const rootTraversal = testModel.traverseFromRoot();
        const rootSeed = SEARCH_EDGE_SEED++;
        const rootNode = new correction.SearchNode(rootTraversal, rootSeed);
        const subsetSeed = SEARCH_EDGE_SEED++;
        const subsetNodes = rootNode.buildDeletionEdges(synthDistribution, subsetSeed);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);

        const processedNodes = subsetNodes.flatMap(n => n.processSubsetEdge());
        // All delete-oriented Transform subsets condense down to a single
        // transform (and edge) each.
        assert.equal(processedNodes.length, 4);
        processedNodes.forEach((n) => assert.equal(n.spaceId, subsetSeed));

        // Sorted index 0:  1 insert - should be processed already, in a single
        // step.
        assert.isFalse(processedNodes[0].hasPartialInput);
        assert.equal(processedNodes[0].editCount, 1);
        assert.equal(lastEntry(processedNodes[0].calculation.inputSequence), SENTINEL_CODE_UNIT);
        assert.equal(processedNodes[0].inputSamplingCost, subsetNodes[0].inputSamplingCost);
        assert.isAbove(processedNodes[0].currentCost, subsetNodes[0].currentCost);

        // Sorted index 3:  0 insert - should be processed already, in a single
        // step.
        assert.isFalse(processedNodes[3].hasPartialInput);
        // No insert string => no sentinel char to delete.
        assert.equal(processedNodes[3].editCount, 0);
        assert.isUndefined(lastEntry(processedNodes[3].calculation.inputSequence));
        assert.equal(processedNodes[3].inputSamplingCost, subsetNodes[3].inputSamplingCost);
        assert.equal(processedNodes[3].currentCost, subsetNodes[3].currentCost);

        // Sorted indices 2, 3:  both had 2 inserts.
        assert.isTrue(processedNodes[1].hasPartialInput);
        assert.equal(processedNodes[1].editCount, 1);
        assert.equal(lastEntry(processedNodes[1].calculation.inputSequence), SENTINEL_CODE_UNIT);
        assert.equal(processedNodes[1].inputSamplingCost, subsetNodes[1].inputSamplingCost);
        assert.isAbove(processedNodes[1].currentCost, subsetNodes[1].currentCost);

        assert.isTrue(processedNodes[2].hasPartialInput);
        assert.equal(processedNodes[2].editCount, 1);
        assert.equal(lastEntry(processedNodes[2].calculation.inputSequence), SENTINEL_CODE_UNIT);
        assert.equal(processedNodes[2].inputSamplingCost, subsetNodes[2].inputSamplingCost);
        assert.isAbove(processedNodes[2].currentCost, subsetNodes[2].currentCost);
      });

      it('step 3: second processing layer resolves two char inserts', () => {
        // From "steps 0, 1" above, assertions removed
        const rootTraversal = testModel.traverseFromRoot();
        const rootSeed = SEARCH_EDGE_SEED++;
        const rootNode = new correction.SearchNode(rootTraversal, rootSeed);
        const subsetSeed = SEARCH_EDGE_SEED++;
        const subsetNodes = rootNode.buildDeletionEdges(synthDistribution, subsetSeed);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);

        // Two nodes were unprocessed at the end of the last step; we handle
        // them now and filter the others out.  We need the indices here to be
        // aligned.
        const step2Nodes = subsetNodes.flatMap(n => n.processSubsetEdge()).filter(n => n.hasPartialInput);
        const processedNodes = step2Nodes.flatMap(n => n.processSubsetEdge());

        // All delete-oriented Transform subsets condense down to a single
        // transform (and edge) each.
        assert.equal(processedNodes.length, 2);
        // All nodes are now done processing.
        processedNodes.forEach((processedNode, index) => {
          assert.isFalse(processedNode.hasPartialInput);
          assert.isFalse(processedNode.hasPartialInput);
          assert.equal(processedNode.editCount, 2);
          assert.equal(processedNode.spaceId, subsetSeed);
          assert.equal(processedNode.calculation.inputSequence.length, 2);
          assert.equal(lastEntry(processedNode.calculation.inputSequence), SENTINEL_CODE_UNIT);
          assert.equal(processedNode.inputSamplingCost, step2Nodes[index].inputSamplingCost);
          assert.isAbove(processedNode.currentCost, step2Nodes[index].currentCost);
        });
      });
    });

    describe(`buildDeletionEdges starting @ 'te' prefix`, () => {
      // start prefix 'te'

      const synthDistribution = [
        //              Transform,            probability
        //         insert 1,   deleteLeft  0,  p: 0.50
        {sample: {insert: 'r', deleteLeft: 0}, p: 0.30}, // ter(m)
        {sample: {insert: 'l', deleteLeft: 0}, p: 0.15}, // tel(l)
        {sample: {insert: 'x', deleteLeft: 0}, p: 0.05}, // no entry available
        //        insert  0    deleteLeft  1,  p: 0.1
        {sample: {insert: '',  deleteLeft: 1}, p: 0.1},  // t (LOTS of followups)
        //        insert   2    deleteLeft  0,  p: 0.15
        {sample: {insert: 'ch', deleteLeft: 0}, p: 0.15}, // tech(nical)
        //        insert   2    deleteLeft  1,  p: 0.25
        {sample: {insert: 'ak', deleteLeft: 1}, p: 0.1},  // tak(e)
        {sample: {insert: 'al', deleteLeft: 1}, p: 0.15}, // tal(k)
        // 4 distinct subsets.
      ];

      it('step 1: batches deletion edge(s) for input transforms', () => {
        const teNode = fetchCommonTENode();
        assert.equal(teNode.calculation.getHeuristicFinalCost(), 0);

        const subsetSeed = SEARCH_EDGE_SEED++;
        const subsetNodes = teNode.buildDeletionEdges(synthDistribution, subsetSeed);
        assert.equal(subsetNodes.length, 4);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);


        const expectedCosts = [0.5, .25, 0.15, 0.1].map(x => -Math.log(x) + teNode.currentCost);
        // The known subs for the subsets defined above.
        for(let i=0; i < expectedCosts.length; i++) {
          assert.isTrue(subsetNodes[i].hasPartialInput);
          // From a 'te' prefix, the deleteLeft 1 entries do have something to delete.
          assert.equal((subsetNodes[i].currentTraversal as TrieTraversal).prefix, (i == 0 || i == 2) ? 'te' : 't');
          assert.notEqual(subsetNodes[i].spaceId, teNode.spaceId);
          assert.equal(subsetNodes[i].spaceId, subsetSeed);

          // Allow a little value wiggle due to double-precision limitations.
          assert.approximately(subsetNodes[i].inputSamplingCost, expectedCosts[i], 1e-8);
          // No actual edit-tracking is done yet, so these should also match.
          assert.approximately(subsetNodes[i].currentCost, expectedCosts[i], 1e-8);
        }
      });

      it('step 2: first processing layer resolves zero + one char inserts', () => {
        // From "step 1" above, assertions removed
        const teNode = fetchCommonTENode();
        const subsetSeed = SEARCH_EDGE_SEED++;
        const subsetNodes = teNode.buildDeletionEdges(synthDistribution, subsetSeed);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);

        const processedNodes = subsetNodes.flatMap(n => n.processSubsetEdge());
        // All delete-oriented Transform subsets condense down to a single
        // transform (and edge) each.
        assert.equal(processedNodes.length, 4);
        processedNodes.forEach(n => assert.notEqual(n.spaceId, teNode.spaceId));
        processedNodes.forEach(n => assert.equal(n.spaceId, subsetSeed));

        // Sorted index 0:  1 insert - should be processed already, in a single
        // step.
        assert.isFalse(processedNodes[0].hasPartialInput);
        assert.equal(processedNodes[0].editCount, 1);
        assert.equal(lastEntry(processedNodes[0].calculation.inputSequence), SENTINEL_CODE_UNIT);
        assert.equal(processedNodes[0].inputSamplingCost, subsetNodes[0].inputSamplingCost);
        assert.isAbove(processedNodes[0].currentCost, subsetNodes[0].currentCost);

        // Sorted index 3:  0 insert - should be processed already, in a single
        // step.
        assert.isFalse(processedNodes[3].hasPartialInput);
        // No insert string => no sentinel char to delete.
        assert.equal(processedNodes[3].editCount, 0);
        // ... and a prior character exists.
        assert.equal(lastEntry(processedNodes[3].calculation.inputSequence), 't');
        assert.equal(processedNodes[3].inputSamplingCost, subsetNodes[3].inputSamplingCost);
        assert.equal(processedNodes[3].currentCost, subsetNodes[3].currentCost);

        // Sorted indices 2, 3:  both had 2 inserts.
        assert.isTrue(processedNodes[1].hasPartialInput);
        assert.equal(processedNodes[1].editCount, 1);
        assert.equal(lastEntry(processedNodes[1].calculation.inputSequence), SENTINEL_CODE_UNIT);
        assert.equal(processedNodes[1].inputSamplingCost, subsetNodes[1].inputSamplingCost);
        assert.isAbove(processedNodes[1].currentCost, subsetNodes[1].currentCost);

        assert.isTrue(processedNodes[2].hasPartialInput);
        assert.equal(processedNodes[2].editCount, 1);
        assert.equal(lastEntry(processedNodes[2].calculation.inputSequence), SENTINEL_CODE_UNIT);
        assert.equal(processedNodes[2].inputSamplingCost, subsetNodes[2].inputSamplingCost);
        assert.isAbove(processedNodes[2].currentCost, subsetNodes[2].currentCost);
      });

      it('step 3: second processing layer resolves two char inserts', () => {
        const teNode = fetchCommonTENode();
        const subsetSeed = SEARCH_EDGE_SEED++;
        const subsetNodes = teNode.buildDeletionEdges(synthDistribution, subsetSeed);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);

        // Two nodes were unprocessed at the end of the last step; we handle
        // them now and filter the others out.  We need the indices here to be
        // aligned.
        const step2Nodes = subsetNodes.flatMap(n => n.processSubsetEdge()).filter(n => n.hasPartialInput);

        function assertExpectedProperties(processedNodes: SearchNode[], baseNode: SearchNode, dl: number) {
          // All delete-oriented Transform subsets condense down to a single
          // transform (and edge) each.
          assert.equal(processedNodes.length, 1);
          // All nodes are now done processing.
          processedNodes.forEach((processedNode) => {
            assert.isFalse(processedNode.hasPartialInput);
            assert.isFalse(processedNode.hasPartialInput);
            assert.equal(processedNode.editCount, 2);
            assert.notEqual(processedNode.spaceId, teNode.spaceId);
            assert.equal(processedNode.spaceId, subsetSeed);
            assert.equal(processedNode.calculation.inputSequence.length, 4 - dl);
            assert.equal(lastEntry(processedNode.calculation.inputSequence), SENTINEL_CODE_UNIT);
            assert.equal(processedNode.inputSamplingCost, baseNode.inputSamplingCost);
            assert.isAbove(processedNode.currentCost, baseNode.currentCost);
          });
        }

        assertExpectedProperties(step2Nodes[0].processSubsetEdge(), step2Nodes[0], 1);
        assertExpectedProperties(step2Nodes[1].processSubsetEdge(), step2Nodes[1], 0);
      });
    });

    describe('buildSubstitutionEdges @ token root', () => {
      const synthDistribution = [
        //              Transform,            probability
        //         insert 1,   deleteLeft  0,  p: 0.50
        {sample: {insert: 't', deleteLeft: 0}, p: 0.30},
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.15},
        {sample: {insert: 'x', deleteLeft: 0}, p: 0.05},
        //        insert  0    deleteLeft  1,  p: 0.1
        {sample: {insert: '',  deleteLeft: 1}, p: 0.1},
        //        insert   2    deleteLeft  0,  p: 0.15
        {sample: {insert: 'ch', deleteLeft: 0}, p: 0.15},
        //        insert   2    deleteLeft  1,  p: 0.25
        {sample: {insert: 'th', deleteLeft: 1}, p: 0.1},
        {sample: {insert: 'tr', deleteLeft: 1}, p: 0.15},
        // 4 distinct subsets.
      ];

      it('step 1: batches substitution edge(s) for input transforms', () => {
        const rootTraversal = testModel.traverseFromRoot();
        assert.isNotEmpty(rootTraversal);

        const rootNode = new correction.SearchNode(rootTraversal, SEARCH_EDGE_SEED++);
        assert.equal(rootNode.calculation.getHeuristicFinalCost(), 0);

        const subsetSeed = SEARCH_EDGE_SEED++;
        const subsetNodes = rootNode.buildSubstitutionEdges(synthDistribution, subsetSeed);
        assert.equal(subsetNodes.length, 4);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);
        const expectedCosts = [0.5, .25, 0.15, 0.1].map(x => -Math.log(x))
        // The known subs for the subsets defined above.
        for(let i=0; i < expectedCosts.length; i++) {
          assert.isTrue(subsetNodes[i].hasPartialInput);
          // From root: the deleteLeft 1 entries have nothing to insert.
          assert.equal((subsetNodes[i].currentTraversal as TrieTraversal).prefix, '');
          assert.equal(subsetNodes[i].spaceId, subsetSeed);

          // Allow a little value wiggle due to double-precision limitations.
          assert.approximately(subsetNodes[i].inputSamplingCost, expectedCosts[i], 1e-8);
          // No actual edit-tracking is done yet, so these should also match.
          assert.approximately(subsetNodes[i].currentCost, expectedCosts[i], 1e-8);
        }
      });

      it('step 2: first processing layer resolves zero + one char inserts', () => {
        // From "step 1" above, assertions removed
        const rootTraversal = testModel.traverseFromRoot();
        const rootNode = new correction.SearchNode(rootTraversal, SEARCH_EDGE_SEED++);
        const subsetSeed = SEARCH_EDGE_SEED++;
        const subsetNodes = rootNode.buildSubstitutionEdges(synthDistribution, subsetSeed);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);
        subsetNodes.forEach(n => assert.equal(n.spaceId, subsetSeed));

        // Set 0:  set for ins 1, dl 0
        const ins1_dl0 = subsetNodes[0].processSubsetEdge();
        // 3 transforms went in... but 1 ('x') had no lexical match.
        assert.equal(ins1_dl0.length, FIRST_CHAR_VARIANTS + 3 - 1);
        ins1_dl0.forEach(n => assert.equal(n.spaceId, subsetSeed));

        ins1_dl0.sort((a, b) => a.currentCost - b.currentCost);
        ins1_dl0.forEach(n => assert.isFalse(n.hasPartialInput)); // all fully-processed.
        assert.equal(lastEntry(ins1_dl0[0].calculation.inputSequence), 't');
        assert.equal(lastEntry(ins1_dl0[0].calculation.matchSequence), 't');
        assert.equal(ins1_dl0[0].editCount, 0);
        assert.equal(lastEntry(ins1_dl0[1].calculation.inputSequence), 'h');
        assert.equal(lastEntry(ins1_dl0[1].calculation.matchSequence), 'h');
        assert.equal(ins1_dl0[1].editCount, 0);
        assert.isBelow(ins1_dl0[0].inputSamplingCost, ins1_dl0[1].inputSamplingCost);
        assert.isBelow(ins1_dl0[0].currentCost, ins1_dl0[1].currentCost);

        // Correction of _other_ input characters to the 't' and the 'h' come
        // after ALL other corrections - these don't get both 't' and 'h' input
        // weights merged into them!
        assert.equal(lastEntry(ins1_dl0[FIRST_CHAR_VARIANTS].calculation.inputSequence), SENTINEL_CODE_UNIT);
        // 't' input is more likely, so 't' gives more edit-contribution to 'h' than
        // 'h' gives to 't'
        assert.equal(lastEntry(ins1_dl0[FIRST_CHAR_VARIANTS].calculation.matchSequence), 'h');
        assert.equal(ins1_dl0[FIRST_CHAR_VARIANTS].editCount, 1);
        assert.isBelow(ins1_dl0[FIRST_CHAR_VARIANTS-1].inputSamplingCost, ins1_dl0[FIRST_CHAR_VARIANTS].inputSamplingCost);
        assert.isBelow(ins1_dl0[FIRST_CHAR_VARIANTS-1].currentCost, ins1_dl0[FIRST_CHAR_VARIANTS].currentCost);

        assert.equal(lastEntry(ins1_dl0[FIRST_CHAR_VARIANTS+1].calculation.inputSequence), SENTINEL_CODE_UNIT);
        assert.equal(lastEntry(ins1_dl0[FIRST_CHAR_VARIANTS+1].calculation.matchSequence), 't');
        assert.equal(ins1_dl0[FIRST_CHAR_VARIANTS+1].editCount, 1);
        assert.isBelow(ins1_dl0[FIRST_CHAR_VARIANTS].inputSamplingCost, ins1_dl0[FIRST_CHAR_VARIANTS+1].inputSamplingCost);
        assert.isBelow(ins1_dl0[FIRST_CHAR_VARIANTS].currentCost, ins1_dl0[FIRST_CHAR_VARIANTS+1].currentCost);

        // For everything in between... well, the input-sampling weight is uniform, and
        // all require a full edit.
        const fullProbEditNodes = ins1_dl0.slice(2, -2);
        const fullInputCost = subsetNodes[0].inputSamplingCost; // the cumulative weight of the ins1_dl0 subset.
        fullProbEditNodes.forEach(n => {
          assert.equal(n.inputSamplingCost, fullInputCost);
          assert.equal(n.editCount, 1);
          assert.equal(lastEntry(n.calculation.inputSequence), SENTINEL_CODE_UNIT);
        });

        // END:  the ins 1, del 0 subset.

        // ************
        // Set 3:  set for ins 0, dl 1
        const ins0_dl1 = subsetNodes[3].processSubsetEdge();

        // No inserts, so no insert variants are possible.
        assert.equal(ins0_dl1.length, 1);
        ins0_dl1.forEach(n => assert.equal(n.spaceId, subsetSeed));
        assert.isFalse(ins0_dl1[0].hasPartialInput);

        // No insert string => no sentinel char to delete.
        assert.equal(ins0_dl1[0].editCount, 0);
        assert.isUndefined(lastEntry(ins0_dl1[0].calculation.inputSequence));
        assert.equal(ins0_dl1[0].inputSamplingCost, subsetNodes[3].inputSamplingCost);
        assert.equal(ins0_dl1[0].currentCost, subsetNodes[3].currentCost);

        // ************
        // Set 1:  set for ins 2, dl 1 - 'tr' + 'th'.
        const ins2_dl1 = subsetNodes[1].processSubsetEdge();

        // 2 transforms went in, but start with the same char - and importantly,
        // there are no other alternatives for that char as lead!
        assert.equal(ins2_dl1.length, FIRST_CHAR_VARIANTS + 1 - 1);
        ins2_dl1.sort((a, b) => a.currentCost - b.currentCost);
        // only one char is processed at this stage.
        ins2_dl1.forEach(n => assert.isTrue(n.hasPartialInput));
        ins2_dl1.forEach(n => assert.equal(n.spaceId, subsetSeed));

        assert.equal(lastEntry(ins2_dl1[0].calculation.inputSequence), 't');
        assert.equal(lastEntry(ins2_dl1[0].calculation.matchSequence), 't');
        assert.equal(ins2_dl1[0].editCount, 0);
        // The subset hasn't yet split!
        assert.equal(ins2_dl1[0].currentCost, subsetNodes[1].currentCost);
        assert.isBelow(ins2_dl1[0].currentCost, ins2_dl1[1].currentCost);

        // All other (non-'t') entries get full subset probability with edit count 1;
        // they're all substitutions, as they fail to match against a non-'t' path.
        assert.equal(ins2_dl1[0].inputSamplingCost, ins2_dl1[1].inputSamplingCost);

        const fullProbEditNodes1 = ins2_dl1.slice(1);
        const fullInputCost1 = subsetNodes[1].inputSamplingCost; // the cumulative weight of the ins2_dl1 subset.
        fullProbEditNodes1.forEach(n => {
          assert.equal(n.inputSamplingCost, fullInputCost1);
          assert.equal(n.editCount, 1);
          assert.equal(lastEntry(n.calculation.inputSequence), SENTINEL_CODE_UNIT);
        });


        // ************
        // Set 2:  set for ins 2, dl 0 - 'ch'.  Single entry.
        const ins2_dl0 = subsetNodes[2].processSubsetEdge();

        // Only 1 transforms went in - importantly, there are no other
        // alternatives for that char as lead!
        assert.equal(ins2_dl0.length, FIRST_CHAR_VARIANTS + 1 - 1);
        ins2_dl0.sort((a, b) => a.currentCost - b.currentCost);
        // only one char is processed at this stage.
        ins2_dl0.forEach(n => assert.isTrue(n.hasPartialInput));
        ins2_dl0.forEach(n => assert.equal(n.spaceId, subsetSeed));

        assert.equal(lastEntry(ins2_dl0[0].calculation.inputSequence), 'c');
        assert.equal(lastEntry(ins2_dl0[0].calculation.matchSequence), 'c');
        assert.equal(ins2_dl0[0].editCount, 0);
        // The subset won't split.
        assert.equal(ins2_dl0[0].currentCost, subsetNodes[2].currentCost);
        assert.isBelow(ins2_dl0[0].currentCost, ins2_dl0[1].currentCost);

        // All other (non-'c') entries get full subset probability with edit count 1;
        // they're all substitutions, as they fail to match against a non-'t' path.
        assert.equal(ins2_dl0[0].inputSamplingCost, ins2_dl0[1].inputSamplingCost);

        const fullProbEditNodes2 = ins2_dl0.slice(1);
        const fullInputCost2 = subsetNodes[2].inputSamplingCost; // the cumulative weight of the ins2_dl0 subset.
        fullProbEditNodes2.forEach(n => {
          assert.equal(n.inputSamplingCost, fullInputCost2);
          assert.equal(n.editCount, 1);
          assert.equal(lastEntry(n.calculation.inputSequence), SENTINEL_CODE_UNIT);
        });
      });

      it('step 3: second processing layer resolves two char inserts', () => {
        // From "steps 0, 1" above, assertions removed
        const rootTraversal = testModel.traverseFromRoot();
        const rootNode = new correction.SearchNode(rootTraversal, SEARCH_EDGE_SEED++);
        const subsetSeed = SEARCH_EDGE_SEED++;
        const subsetNodes = rootNode.buildSubstitutionEdges(synthDistribution, subsetSeed);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);


        // ************
        // The intermediate, not-fully-processed node sets.
        const ins2_dl1 = subsetNodes[1].processSubsetEdge(); // 'th', 'tr'
        const ins2_dl0 = subsetNodes[2].processSubsetEdge(); // 'ch'

        const fin_in2_dl1 = ins2_dl1.flatMap(n => n.processSubsetEdge());
        const fin_in2_dl0 = ins2_dl0.flatMap(n => n.processSubsetEdge());

        // All should be finished now!
        fin_in2_dl1.forEach(n => assert.isFalse(n.hasPartialInput));

        fin_in2_dl1.forEach(n => assert.equal(n.spaceId, subsetSeed));
        fin_in2_dl0.forEach(n => assert.equal(n.spaceId, subsetSeed));

        fin_in2_dl1.sort((a, b) => a.currentCost - b.currentCost);
        fin_in2_dl0.sort((a, b) => a.currentCost - b.currentCost);

        // Not going to define hard counts here, but there should easily
        // be more.
        assert.isAbove(fin_in2_dl1.length, ins2_dl1.length);
        assert.isAbove(fin_in2_dl1.length, 100 /* grows significantly! */);
        assert.isAbove(fin_in2_dl0.length, ins2_dl0.length);
        assert.isAbove(fin_in2_dl0.length, 100 /* grows significantly! */);

        // 'tr' had a notably higher base probability and wins.
        assert.equal(fin_in2_dl1[0].resultKey, 'tr');
        assert.equal(fin_in2_dl1[0].editCount, 0);
        assert.equal(fin_in2_dl1[1].resultKey, 'th');
        assert.equal(fin_in2_dl1[1].editCount, 0);

        // We should also have single-char replacement entries for cases where one char matches...
        assert.isAtLeast(fin_in2_dl1.filter(n => {
          return n.resultKey == 'ty' && n.calculation.inputSequence.map(i => i).join('') == 't' + SENTINEL_CODE_UNIT && n.editCount == 1
        }).length, 1, "did not find expected result type");
        assert.isAtLeast(fin_in2_dl1.filter(n => {
          return n.resultKey == 'ch' && n.calculation.inputSequence.map(i => i).join('') == SENTINEL_CODE_UNIT + 'h' && n.editCount == 1
        }).length, 1, "did not find expected result type");
        assert.isAtLeast(fin_in2_dl1.filter(n => {
          return n.resultKey == 'ar' && n.calculation.inputSequence.map(i => i).join('') == SENTINEL_CODE_UNIT + 'r' && n.editCount == 1
        }).length, 1, "did not find expected result type");

        // And even cases where none match, though at twice the edit cost.
        assert.isAtLeast(fin_in2_dl1.filter(n => {
          return n.resultKey == 'an' && n.calculation.inputSequence.map(i => i).join('') == SENTINEL_CODE_UNIT + SENTINEL_CODE_UNIT && n.editCount == 2
        }).length, 1, "did not find expected result type");

        // *****
        // Now for the other set...
        assert.equal(fin_in2_dl0[0].resultKey, 'ch');
        assert.equal(fin_in2_dl0[0].editCount, 0);

        // We should also have single-char replacement entries for cases where one char matches...
        assert.isAtLeast(fin_in2_dl0.filter(n => {
          return n.resultKey == 'ca' && n.calculation.inputSequence.map(i => i).join('') == 'c' + SENTINEL_CODE_UNIT && n.editCount == 1
        }).length, 1, "did not find expected result type");
        assert.isAtLeast(fin_in2_dl0.filter(n => {
          return n.resultKey == 'th' && n.calculation.inputSequence.map(i => i).join('') == SENTINEL_CODE_UNIT + 'h' && n.editCount == 1
        }).length, 1, "did not find expected result type");

        // And even cases where none match, though at twice the edit cost.
        assert.isAtLeast(fin_in2_dl0.filter(n => {
          return n.resultKey == 'an' && n.calculation.inputSequence.map(i => i).join('') == SENTINEL_CODE_UNIT + SENTINEL_CODE_UNIT && n.editCount == 2
        }).length, 1, "did not find expected result type");
      });
    });

    describe(`buildSubstitutionEdges starting @ 'te' prefix`, () => {
      // start prefix 'te'

      const synthDistribution = [
        //              Transform,            probability
        //         insert 1,   deleteLeft  0,  p: 0.50
        {sample: {insert: 'r', deleteLeft: 0}, p: 0.30}, // ter(m)
        {sample: {insert: 'l', deleteLeft: 0}, p: 0.15}, // tel(l)
        {sample: {insert: 'x', deleteLeft: 0}, p: 0.05}, // no entry available
        //        insert  0    deleteLeft  1,  p: 0.1
        {sample: {insert: '',  deleteLeft: 1}, p: 0.1},  // t (LOTS of followups)
        //        insert   2    deleteLeft  0,  p: 0.15
        {sample: {insert: 'ch', deleteLeft: 0}, p: 0.15}, // tech(nical)
        //        insert   2    deleteLeft  1,  p: 0.25
        {sample: {insert: 'ak', deleteLeft: 1}, p: 0.1},  // tak(e)
        {sample: {insert: 'al', deleteLeft: 1}, p: 0.15}, // tal(k)
        // 4 distinct subsets.
      ];

      it('step 1: batches substitution edge(s) for input transforms', () => {
        const teNode = fetchCommonTENode();
        assert.equal(teNode.calculation.getHeuristicFinalCost(), 0);

        const subsetNodes = teNode.buildSubstitutionEdges(synthDistribution, SEARCH_EDGE_SEED++);
        assert.equal(subsetNodes.length, 4);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);
        const expectedCosts = [0.5, .25, 0.15, 0.1].map(x => -Math.log(x) + teNode.currentCost);
        // The known subs for the subsets defined above.
        for(let i=0; i < expectedCosts.length; i++) {
          assert.isTrue(subsetNodes[i].hasPartialInput);
          // From root: the deleteLeft 1 entries have nothing to insert.
          assert.equal((subsetNodes[i].currentTraversal as TrieTraversal).prefix, (i == 0 || i == 2) ? 'te' : 't');

          // Allow a little value wiggle due to double-precision limitations.
          assert.approximately(subsetNodes[i].inputSamplingCost, expectedCosts[i], 1e-8);
          // No actual edit-tracking is done yet, so these should also match.
          assert.approximately(subsetNodes[i].currentCost, expectedCosts[i], 1e-8);
        }
      });

      it('step 2: first processing layer resolves zero + one char inserts', () => {
        // From "step 1" above, assertions removed
        const teNode = fetchCommonTENode();
        const subsetNodes = teNode.buildSubstitutionEdges(synthDistribution, SEARCH_EDGE_SEED++);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);

        // Set 0:  set for ins 1, dl 0
        const ins1_dl0 = subsetNodes[0].processSubsetEdge();
        const TE_CHILD_PATH_COUNT = 6; // 'l', 'n', 'r', 'm', 'c', 's'
        // 3 transforms went in... but 1 ('x') had no lexical match.
        assert.equal(ins1_dl0.length, TE_CHILD_PATH_COUNT + 3 - 1);

        ins1_dl0.sort((a, b) => a.currentCost - b.currentCost);
        ins1_dl0.forEach(n => assert.isFalse(n.hasPartialInput)); // all fully-processed.
        assert.equal(lastEntry(ins1_dl0[0].calculation.inputSequence), 'r');
        assert.equal(lastEntry(ins1_dl0[0].calculation.matchSequence), 'r');
        assert.equal(ins1_dl0[0].editCount, 0);
        assert.equal(lastEntry(ins1_dl0[1].calculation.inputSequence), 'l');
        assert.equal(lastEntry(ins1_dl0[1].calculation.matchSequence), 'l');
        assert.equal(ins1_dl0[1].editCount, 0);
        assert.isBelow(ins1_dl0[0].inputSamplingCost, ins1_dl0[1].inputSamplingCost);
        assert.isBelow(ins1_dl0[0].currentCost, ins1_dl0[1].currentCost);

        // Correction of _other_ input characters to the 't' and the 'h' come
        // after ALL other corrections - these don't get both 't' and 'h' input
        // weights merged into them!
        assert.equal(lastEntry(ins1_dl0[TE_CHILD_PATH_COUNT].calculation.inputSequence), SENTINEL_CODE_UNIT);
        // 't' input is more likely, so 't' gives more edit-contribution to 'h' than
        // 'h' gives to 't'
        assert.equal(lastEntry(ins1_dl0[TE_CHILD_PATH_COUNT].calculation.matchSequence), 'l');
        assert.equal(ins1_dl0[TE_CHILD_PATH_COUNT].editCount, 1);
        assert.isBelow(ins1_dl0[TE_CHILD_PATH_COUNT-1].inputSamplingCost, ins1_dl0[TE_CHILD_PATH_COUNT].inputSamplingCost);
        assert.isBelow(ins1_dl0[TE_CHILD_PATH_COUNT-1].currentCost, ins1_dl0[TE_CHILD_PATH_COUNT].currentCost);

        assert.equal(lastEntry(ins1_dl0[TE_CHILD_PATH_COUNT+1].calculation.inputSequence), SENTINEL_CODE_UNIT);
        assert.equal(lastEntry(ins1_dl0[TE_CHILD_PATH_COUNT+1].calculation.matchSequence), 'r');
        assert.equal(ins1_dl0[TE_CHILD_PATH_COUNT+1].editCount, 1);
        assert.isBelow(ins1_dl0[TE_CHILD_PATH_COUNT].inputSamplingCost, ins1_dl0[TE_CHILD_PATH_COUNT+1].inputSamplingCost);
        assert.isBelow(ins1_dl0[TE_CHILD_PATH_COUNT].currentCost, ins1_dl0[TE_CHILD_PATH_COUNT+1].currentCost);

        // For everything in between... well, the input-sampling weight is uniform, and
        // all require a full edit.
        const fullProbEditNodes = ins1_dl0.slice(2, -2);
        const fullInputCost = subsetNodes[0].inputSamplingCost; // the cumulative weight of the ins1_dl0 subset.
        fullProbEditNodes.forEach(n => {
          assert.equal(n.inputSamplingCost, fullInputCost);
          assert.equal(n.editCount, 1);
          assert.equal(lastEntry(n.calculation.inputSequence), SENTINEL_CODE_UNIT);
        });

        // END:  the ins 1, del 0 subset.

        // ************
        // Set 3:  set for ins 0, dl 1
        const ins0_dl1 = subsetNodes[3].processSubsetEdge();

        // No inserts, so no insert variants are possible.
        assert.equal(ins0_dl1.length, 1);
        assert.isFalse(ins0_dl1[0].hasPartialInput);

        // No insert string => no sentinel char to delete.
        assert.equal(ins0_dl1[0].editCount, 0);
        assert.equal(lastEntry(ins0_dl1[0].calculation.inputSequence), 't');
        assert.equal(ins0_dl1[0].inputSamplingCost, subsetNodes[3].inputSamplingCost);
        assert.equal(ins0_dl1[0].currentCost, subsetNodes[3].currentCost);

        // ************
        // Set 1:  set for ins 2, dl 1 - 'ak' + 'ax'.
        const ins2_dl1 = subsetNodes[1].processSubsetEdge();

        // 2 transforms went in, but start with the same char - and importantly,
        // there are no other alternatives for that char as lead!
        const T_CHILD_PATH_COUNT = 9; // 'h', 'o', 'i', 'w', 'a', 'u', 'e', 'r', 'y'
        assert.equal(ins2_dl1.length, T_CHILD_PATH_COUNT + 1 - 1);
        ins2_dl1.sort((a, b) => a.currentCost - b.currentCost);
        // only one char is processed at this stage.
        ins2_dl1.forEach(n => assert.isTrue(n.hasPartialInput));

        assert.equal(lastEntry(ins2_dl1[0].calculation.inputSequence), 'a');
        assert.equal(lastEntry(ins2_dl1[0].calculation.matchSequence), 'a');
        assert.equal(ins2_dl1[0].editCount, 0);
        // The subset hasn't yet split!
        assert.equal(ins2_dl1[0].currentCost, subsetNodes[1].currentCost);
        assert.isBelow(ins2_dl1[0].currentCost, ins2_dl1[1].currentCost);

        // All other (non-'t') entries get full subset probability with edit count 1;
        // they're all substitutions, as they fail to match against a non-'t' path.
        assert.equal(ins2_dl1[0].inputSamplingCost, ins2_dl1[1].inputSamplingCost);

        const fullProbEditNodes1 = ins2_dl1.slice(1);
        const fullInputCost1 = subsetNodes[1].inputSamplingCost; // the cumulative weight of the ins2_dl1 subset.
        fullProbEditNodes1.forEach(n => {
          assert.equal(n.inputSamplingCost, fullInputCost1);
          assert.equal(n.editCount, 1);
          assert.equal(lastEntry(n.calculation.inputSequence), SENTINEL_CODE_UNIT);
        });


        // ************
        // Set 2:  set for ins 2, dl 0 - 'ch'.  Single entry.
        const ins2_dl0 = subsetNodes[2].processSubsetEdge();

        // Only 1 transform went in - importantly, there are no other
        // alternatives for that char as lead!
        assert.equal(ins2_dl0.length, TE_CHILD_PATH_COUNT + 1 - 1);
        ins2_dl0.sort((a, b) => a.currentCost - b.currentCost);
        // only one char is processed at this stage.
        ins2_dl0.forEach(n => assert.isTrue(n.hasPartialInput));

        assert.equal(lastEntry(ins2_dl0[0].calculation.inputSequence), 'c');
        assert.equal(lastEntry(ins2_dl0[0].calculation.matchSequence), 'c');
        assert.equal(ins2_dl0[0].editCount, 0);
        // The subset won't split.
        assert.equal(ins2_dl0[0].currentCost, subsetNodes[2].currentCost);
        assert.isBelow(ins2_dl0[0].currentCost, ins2_dl0[1].currentCost);

        // All other (non-'c') entries get full subset probability with edit count 1;
        // they're all substitutions, as they fail to match against a non-'t' path.
        assert.equal(ins2_dl0[0].inputSamplingCost, ins2_dl0[1].inputSamplingCost);

        const fullProbEditNodes2 = ins2_dl0.slice(1);
        const fullInputCost2 = subsetNodes[2].inputSamplingCost; // the cumulative weight of the ins2_dl0 subset.
        fullProbEditNodes2.forEach(n => {
          assert.equal(n.inputSamplingCost, fullInputCost2);
          assert.equal(n.editCount, 1);
          assert.equal(lastEntry(n.calculation.inputSequence), SENTINEL_CODE_UNIT);
        });
      });

      it('step 3: second processing layer resolves two char inserts', () => {
        // From "steps 0, 1" above, assertions removed
        const teNode = fetchCommonTENode();
        const subsetNodes = teNode.buildSubstitutionEdges(synthDistribution, SEARCH_EDGE_SEED++);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);

        // ************
        // The intermediate, not-fully-processed node sets.
        const ins2_dl1 = subsetNodes[1].processSubsetEdge(); // 'tak', 'tax'
        const ins2_dl0 = subsetNodes[2].processSubsetEdge(); // 'tech'

        const fin_in2_dl1 = ins2_dl1.flatMap(n => n.processSubsetEdge());
        const fin_in2_dl0 = ins2_dl0.flatMap(n => n.processSubsetEdge());

        // All should be finished now!
        fin_in2_dl1.forEach(n => assert.isFalse(n.hasPartialInput));

        fin_in2_dl1.sort((a, b) => a.currentCost - b.currentCost);
        fin_in2_dl0.sort((a, b) => a.currentCost - b.currentCost);

        // Not going to define hard counts here, but there should easily
        // be more.
        assert.isAbove(fin_in2_dl1.length, ins2_dl1.length);
        assert.isAbove(fin_in2_dl1.length, 20 /* grows significantly - lots of 't'-prefixed words! */);
        // Not so many 'te' prefixed words, though - this is a very selective model.
        // 'ten' was possible after one inserted char, but the second inserted char blocks it as a
        // substitution target!
        assert.equal(fin_in2_dl0.length, ins2_dl0.length - 1);

        // 'tal' had a notably higher base probability and wins.
        assert.equal(fin_in2_dl1[0].resultKey, 'tal');
        assert.equal(fin_in2_dl1[0].editCount, 0);
        assert.equal(fin_in2_dl1[1].resultKey, 'tak');
        assert.equal(fin_in2_dl1[1].editCount, 0);

        // We should also have single-char replacement entries for cases where one char matches...
        assert.isAtLeast(fin_in2_dl1.filter(n => {
          return n.resultKey == 'tax' && n.calculation.inputSequence.map(i => i).join('') == 'ta' + SENTINEL_CODE_UNIT && n.editCount == 1
        }).length, 1, "did not find expected result type"); // 'tax'
        assert.isAtLeast(fin_in2_dl1.filter(n => {
          return n.resultKey == 'tol' && n.calculation.inputSequence.map(i => i).join('') == 't' + SENTINEL_CODE_UNIT + 'l' && n.editCount == 1
        }).length, 1, "did not find expected result type");
        assert.equal(fin_in2_dl1.filter(n => {
        // No other 't_k' entries. :()
          return n.resultKey == 'tir' && n.calculation.inputSequence.map(i => i).join('') == 't' + SENTINEL_CODE_UNIT + 'r' && n.editCount == 1
        }).length, 0, "found unexpected result type");

        // And even cases where none match, though at twice the edit cost.
        assert.isAtLeast(fin_in2_dl1.filter(n => {
          return n.resultKey == 'tim' && n.calculation.inputSequence.map(i => i).join('') == 't' + SENTINEL_CODE_UNIT + SENTINEL_CODE_UNIT && n.editCount == 2
        }).length, 1, "did not find expected result type");

        // *****
        // Now for the other set...
        assert.equal(fin_in2_dl0[0].resultKey, 'tech');
        assert.equal(fin_in2_dl0[0].editCount, 0);

        assert.equal(fin_in2_dl0.filter(n => {
          return n.resultKey == 'tec_' && n.calculation.inputSequence.map(i => i).join('') == 'tec' + SENTINEL_CODE_UNIT && n.editCount == 1
        }).length, 0, "found unexpected result type"); // No other 'tec'-prefixed entries.
        assert.isAtLeast(fin_in2_dl0.filter(n => {
          return n.resultKey == 'te_h' && n.calculation.inputSequence.map(i => i).join('') == 'te' + SENTINEL_CODE_UNIT + 'h' && n.editCount == 1
        }).length, 0, "found unexpected result type"); // No other 'te_h' entries here.

        // Cases where none match should exist, though at twice the edit cost.
        assert.isAtLeast(fin_in2_dl0.filter(n => {
          return n.resultKey == 'temp' && n.calculation.inputSequence.map(i => i).join('') == 'te' + SENTINEL_CODE_UNIT + SENTINEL_CODE_UNIT && n.editCount == 2
        }).length, 1, "did not find expected result type"); // temperature
      });
    });

    it('Small integration test:  "teh" => "ten", "the"', () => {
      // The combinatorial effect here is a bit much to fully test.
      const rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      const rootSeed = SEARCH_EDGE_SEED++;
      const rootNode = new correction.SearchNode(rootTraversal, rootSeed);
      assert.equal(rootNode.calculation.getHeuristicFinalCost(), 0);

      // VERY artificial distributions.
      const synthDistribution1 = [
        {sample: {insert: 't', deleteLeft: 0}, p: 1} // Transform, probability
      ];

      const synthDistribution2 = [
        {sample: {insert: 'e', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.25}
      ];

      const synthDistribution3 = [
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'n', deleteLeft: 0}, p: 0.25}
      ];

      const layer1Id = SEARCH_EDGE_SEED++;
      const layer1Edges = rootNode.buildSubstitutionEdges(synthDistribution1, layer1Id)
        // No 2+ inserts here; we're fine with just one call.
        .flatMap(e => e.processSubsetEdge());
      const layer1Queue = new PriorityQueue(QUEUE_NODE_COMPARATOR, layer1Edges);

      const tEdge = layer1Queue.dequeue();
      assertEdgeChars(tEdge, 't', 't');
      assert.equal(tEdge.spaceId, layer1Id); // would be obtained by the token after one input.

      const layer2Id = SEARCH_EDGE_SEED++;
      const layer2Edges = tEdge.buildSubstitutionEdges(synthDistribution2, layer2Id)
        // No 2+ inserts here; we're fine with just one call.
        .flatMap(e => e.processSubsetEdge());
      const layer2Queue = new PriorityQueue(QUEUE_NODE_COMPARATOR, layer2Edges);

      const eEdge = layer2Queue.dequeue();
      assertEdgeChars(eEdge, 'e', 'e');
      assert.equal(eEdge.spaceId, layer2Id);

      const hEdge = layer2Queue.dequeue();
      assertEdgeChars(hEdge, 'h', 'h');
      assert.equal(hEdge.spaceId, layer2Id);

      // Needed for a proper e <-> h transposition.
      const ehEdge = findEdgesWithChars(layer2Edges, 'h')[0];

      assert.isOk(ehEdge);

      // Final round:  we'll use three nodes and throw all of their results into the same priority queue.
      const layer3Id = SEARCH_EDGE_SEED++;
      const layer3eEdges  = eEdge.buildSubstitutionEdges(synthDistribution3, layer3Id)
        // No 2+ inserts here; we're fine with just one call.
        .flatMap(e => e.processSubsetEdge());
      const layer3hEdges  = hEdge.buildSubstitutionEdges(synthDistribution3, layer3Id)
        .flatMap(e => e.processSubsetEdge());
      const layer3ehEdges = ehEdge.buildSubstitutionEdges(synthDistribution3, layer3Id)
        .flatMap(e => e.processSubsetEdge());
      const layer3Queue = new PriorityQueue(QUEUE_NODE_COMPARATOR, layer3eEdges.concat(layer3hEdges).concat(layer3ehEdges));

      // Find the first result with an actual word directly represented.
      let bestEdge;
      do {
        bestEdge = layer3Queue.dequeue();
      } while(bestEdge.currentTraversal.entries.length == 0);

      assertEdgeChars(bestEdge, 'n', 'n'); // 'ten' - perfect edit distance of 0, though less-likely input sequence.
      assert.equal(bestEdge.spaceId, layer3Id);
      // No cost assumptions here.

      var sibling1;
      do {
        sibling1 = layer3Queue.dequeue();
      } while(sibling1.currentTraversal.entries.length == 0);
      assert.equal(sibling1.spaceId, layer3Id);

      // Both have a raw edit distance of 1 while using the same input-sequence root. ('th')
      let tenFlag = edgeHasChars(sibling1, SENTINEL_CODE_UNIT, 'n'); // subs out the 'h' entirely.  Could also occur with 'a', but is too unlikely.
      let theFlag = edgeHasChars(sibling1, SENTINEL_CODE_UNIT, 'e'); // looks for transposed 'h' and 'e'.

      assert.isTrue(tenFlag || theFlag);
      assert.isAbove(sibling1.currentCost, bestEdge.currentCost);

      var sibling2;
      do {
        sibling2 = layer3Queue.dequeue();
      } while(sibling2.currentTraversal.entries.length == 0);
      assert.equal(sibling2.spaceId, layer3Id);

      tenFlag = tenFlag || edgeHasChars(sibling2, SENTINEL_CODE_UNIT, 'n');
      theFlag = theFlag || edgeHasChars(sibling2, SENTINEL_CODE_UNIT, 'e');

      assert.isTrue(tenFlag && theFlag);
      assert.isAbove(sibling2.currentCost, sibling1.currentCost);
    });
  });
});
