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

import { correction, models } from '@keymanapp/lm-worker/test-index';

import SENTINEL_CODE_UNIT = models.SENTINEL_CODE_UNIT;
import SearchNode = correction.SearchNode;
import SearchResult = correction.SearchResult;
import TrieModel = models.TrieModel;

// Exposes an extra property that's very useful for validating behaviors during unit testing:
// - `prefix`.
type TrieTraversal = ReturnType<TrieModel['traverseFromRoot']>;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));
/**
 * The total number of top-level child paths in the lexicon.
 */
const FIRST_CHAR_VARIANTS = 24;

function buildTestTimer() {
  return new correction.ExecutionTimer(Number.MAX_VALUE, Number.MAX_VALUE);
}

function assertEdgeChars(edge: correction.SearchNode, input: string, match: string) {
  assert.isTrue(edgeHasChars(edge, input, match));
}

function edgeHasChars(edge: correction.SearchNode, input: string, match: string) {
  if(edge.priorInput[edge.priorInput.length - 1].sample.insert != input) {
    return false;
  }

  return edge.calculation.lastMatchEntry.key == match;
}

function findEdgesWithChars(edgeArray: correction.SearchNode[], match: string) {
  let results = edgeArray.filter(function(value) {
    return value.calculation.lastMatchEntry.key == match;
  });

  assert.isAtLeast(results.length, 1);
  return results;
}

describe('Correction Distance Modeler', function() {
  describe('SearchNode', function() {
    it('constructs a fresh instance from a traversal + keyingFunction', () => {
      const toKey = (s: string) => testModel.toKey(s);
      const rootNode = new SearchNode(testModel.traverseFromRoot(), toKey);
      assert.equal(rootNode.resultKey, '');

      assert.equal(rootNode.editCount, 0);
      assert.equal(rootNode.inputSamplingCost, 0);
      assert.equal(rootNode.currentCost, 0);

      assert.equal((rootNode.currentTraversal as TrieTraversal).prefix, '');
      assert.isFalse(rootNode.hasPartialInput);
      assert.isFalse(rootNode.isFullReplacement)
      assert.isUndefined(rootNode.calculation.lastInputEntry);
      assert.isUndefined(rootNode.calculation.lastMatchEntry);
      assert.equal(rootNode.toKey, toKey);
    });

    // TODO:
    it.skip('supports the cloning constructor pattern', () => {})

    // Consider adding more, deeper?
    it('builds insertion edges based on lexicon, from root', function() {
      let rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      let rootNode = new correction.SearchNode(rootTraversal);
      assert.equal(rootNode.calculation.getHeuristicFinalCost(), 0);

      let edges = rootNode.buildInsertionEdges();
      assert.isAbove(edges.length, 0);

      let expectedChildCount = 0;
      for(let child of rootTraversal.children()) {
        expectedChildCount++;

        let childEdge = edges.filter(value => value.calculation.lastMatchEntry.key == child.char)[0];
        assert.isOk(childEdge);
        assert.isEmpty(childEdge.priorInput);
        assert.isEmpty(childEdge.calculation.inputSequence);
        assert.isAbove(childEdge.currentCost, 0);
      }

      assert.equal(edges.length, expectedChildCount);
    });

    // TODO:  another copy, but prefixed by at least two chars - we need to confirm
    // deleteLeft behavior here.  That's the main thing to verify there.
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
        let rootTraversal = testModel.traverseFromRoot();
        assert.isNotEmpty(rootTraversal);

        let rootNode = new correction.SearchNode(rootTraversal);
        assert.equal(rootNode.calculation.getHeuristicFinalCost(), 0);

        const subsetNodes = rootNode.buildDeletionEdges(synthDistribution);
        assert.equal(subsetNodes.length, 4);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);
        const expectedCosts = [0.5, .25, 0.15, 0.1].map(x => -Math.log(x))
        // The known subs for the subsets defined above.
        for(let i=0; i < expectedCosts.length; i++) {
          assert.isTrue(subsetNodes[i].hasPartialInput);
          // From root: the deleteLeft 1 entries have nothing to delete.
          assert.equal((subsetNodes[i].currentTraversal as TrieTraversal).prefix, '');

          // Allow a little value wiggle due to double-precision limitations.
          assert.approximately(subsetNodes[i].inputSamplingCost, expectedCosts[i], 1e-8);
          // No actual edit-tracking is done yet, so these should also match.
          assert.approximately(subsetNodes[i].currentCost, expectedCosts[i], 1e-8);
        }
      });

      it('step 2: first processing layer resolves zero + one char inserts', () => {
        // From "step 1" above, assertions removed
        let rootTraversal = testModel.traverseFromRoot();
        let rootNode = new correction.SearchNode(rootTraversal);
        const subsetNodes = rootNode.buildDeletionEdges(synthDistribution);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);

        const processedNodes = subsetNodes.flatMap(n => n.processSubsetEdge());
        // All delete-oriented Transform subsets condense down to a single
        // transform (and edge) each.
        assert.equal(processedNodes.length, 4);

        // Sorted index 0:  1 insert - should be processed already, in a single
        // step.
        assert.isFalse(processedNodes[0].hasPartialInput);
        assert.equal(processedNodes[0].editCount, 1);
        assert.equal(processedNodes[0].calculation.lastInputEntry.key, SENTINEL_CODE_UNIT);
        assert.equal(processedNodes[0].inputSamplingCost, subsetNodes[0].inputSamplingCost);
        assert.isAbove(processedNodes[0].currentCost, subsetNodes[0].currentCost);

        // Sorted index 3:  0 insert - should be processed already, in a single
        // step.
        assert.isFalse(processedNodes[3].hasPartialInput);
        // No insert string => no sentinel char to delete.
        assert.equal(processedNodes[3].editCount, 0);
        assert.isUndefined(processedNodes[3].calculation.lastInputEntry);
        assert.equal(processedNodes[3].inputSamplingCost, subsetNodes[3].inputSamplingCost);
        assert.equal(processedNodes[3].currentCost, subsetNodes[3].currentCost);

        // Sorted indices 2, 3:  both had 2 inserts.
        assert.isTrue(processedNodes[1].hasPartialInput);
        assert.equal(processedNodes[1].editCount, 1);
        assert.equal(processedNodes[1].calculation.lastInputEntry.key, SENTINEL_CODE_UNIT);
        assert.equal(processedNodes[1].inputSamplingCost, subsetNodes[1].inputSamplingCost);
        assert.isAbove(processedNodes[1].currentCost, subsetNodes[1].currentCost);

        assert.isTrue(processedNodes[2].hasPartialInput);
        assert.equal(processedNodes[2].editCount, 1);
        assert.equal(processedNodes[2].calculation.lastInputEntry.key, SENTINEL_CODE_UNIT);
        assert.equal(processedNodes[2].inputSamplingCost, subsetNodes[2].inputSamplingCost);
        assert.isAbove(processedNodes[2].currentCost, subsetNodes[2].currentCost);
      });

      it('step 3: second processing layer resolves two char inserts', () => {
        // From "steps 0, 1" above, assertions removed
        let rootTraversal = testModel.traverseFromRoot();
        let rootNode = new correction.SearchNode(rootTraversal);
        const subsetNodes = rootNode.buildDeletionEdges(synthDistribution);
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
          assert.equal(processedNode.calculation.inputSequence.length, 2);
          assert.equal(processedNode.calculation.lastInputEntry.key, SENTINEL_CODE_UNIT);
          assert.equal(processedNode.inputSamplingCost, step2Nodes[index].inputSamplingCost);
          assert.isAbove(processedNode.currentCost, step2Nodes[index].currentCost);
        });
      });
    });

    // TODO:  another copy, but prefixed by at least two chars - we need to confirm
    // deleteLeft behavior here.  That's the main thing to verify there.
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
        let rootTraversal = testModel.traverseFromRoot();
        assert.isNotEmpty(rootTraversal);

        let rootNode = new correction.SearchNode(rootTraversal);
        assert.equal(rootNode.calculation.getHeuristicFinalCost(), 0);

        const subsetNodes = rootNode.buildSubstitutionEdges(synthDistribution);
        assert.equal(subsetNodes.length, 4);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);
        const expectedCosts = [0.5, .25, 0.15, 0.1].map(x => -Math.log(x))
        // The known subs for the subsets defined above.
        for(let i=0; i < expectedCosts.length; i++) {
          assert.isTrue(subsetNodes[i].hasPartialInput);
          // From root: the deleteLeft 1 entries have nothing to insert.
          assert.equal((subsetNodes[i].currentTraversal as TrieTraversal).prefix, '');

          // Allow a little value wiggle due to double-precision limitations.
          assert.approximately(subsetNodes[i].inputSamplingCost, expectedCosts[i], 1e-8);
          // No actual edit-tracking is done yet, so these should also match.
          assert.approximately(subsetNodes[i].currentCost, expectedCosts[i], 1e-8);
        }
      });

      it('step 2: first processing layer resolves zero + one char inserts', () => {
        // From "step 1" above, assertions removed
        let rootTraversal = testModel.traverseFromRoot();
        let rootNode = new correction.SearchNode(rootTraversal);
        const subsetNodes = rootNode.buildSubstitutionEdges(synthDistribution);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);

        // Set 0:  set for ins 1, dl 0
        const ins1_dl0 = subsetNodes[0].processSubsetEdge();
        // 3 transforms went in... but 1 ('x') had no lexical match.
        assert.equal(ins1_dl0.length, FIRST_CHAR_VARIANTS + 3 - 1);

        ins1_dl0.sort((a, b) => a.currentCost - b.currentCost);
        ins1_dl0.forEach(n => assert.isFalse(n.hasPartialInput)); // all fully-processed.
        assert.equal(ins1_dl0[0].calculation.lastInputEntry.key, 't');
        assert.equal(ins1_dl0[0].calculation.lastMatchEntry.key, 't');
        assert.equal(ins1_dl0[0].editCount, 0);
        assert.equal(ins1_dl0[1].calculation.lastInputEntry.key, 'h');
        assert.equal(ins1_dl0[1].calculation.lastMatchEntry.key, 'h');
        assert.equal(ins1_dl0[1].editCount, 0);
        assert.isBelow(ins1_dl0[0].inputSamplingCost, ins1_dl0[1].inputSamplingCost);
        assert.isBelow(ins1_dl0[0].currentCost, ins1_dl0[1].currentCost);

        // Correction of _other_ input characters to the 't' and the 'h' come
        // after ALL other corrections - these don't get both 't' and 'h' input
        // weights merged into them!
        assert.equal(ins1_dl0[FIRST_CHAR_VARIANTS].calculation.lastInputEntry.key, SENTINEL_CODE_UNIT);
        // 't' input is more likely, so 't' gives more edit-contribution to 'h' than
        // 'h' gives to 't'
        assert.equal(ins1_dl0[FIRST_CHAR_VARIANTS].calculation.lastMatchEntry.key, 'h');
        assert.equal(ins1_dl0[FIRST_CHAR_VARIANTS].editCount, 1);
        assert.isBelow(ins1_dl0[FIRST_CHAR_VARIANTS-1].inputSamplingCost, ins1_dl0[FIRST_CHAR_VARIANTS].inputSamplingCost);
        assert.isBelow(ins1_dl0[FIRST_CHAR_VARIANTS-1].currentCost, ins1_dl0[FIRST_CHAR_VARIANTS].currentCost);

        assert.equal(ins1_dl0[FIRST_CHAR_VARIANTS+1].calculation.lastInputEntry.key, SENTINEL_CODE_UNIT);
        assert.equal(ins1_dl0[FIRST_CHAR_VARIANTS+1].calculation.lastMatchEntry.key, 't');
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
          assert.equal(n.calculation.lastInputEntry.key, SENTINEL_CODE_UNIT);
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
        assert.isUndefined(ins0_dl1[0].calculation.lastInputEntry);
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

        assert.equal(ins2_dl1[0].calculation.lastInputEntry.key, 't');
        assert.equal(ins2_dl1[0].calculation.lastMatchEntry.key, 't');
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
          assert.equal(n.calculation.lastInputEntry.key, SENTINEL_CODE_UNIT);
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

        assert.equal(ins2_dl0[0].calculation.lastInputEntry.key, 'c');
        assert.equal(ins2_dl0[0].calculation.lastMatchEntry.key, 'c');
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
          assert.equal(n.calculation.lastInputEntry.key, SENTINEL_CODE_UNIT);
        });
      });

      it('step 3: second processing layer resolves two char inserts', () => {
        // From "steps 0, 1" above, assertions removed
        let rootTraversal = testModel.traverseFromRoot();
        let rootNode = new correction.SearchNode(rootTraversal);
        const subsetNodes = rootNode.buildSubstitutionEdges(synthDistribution);
        subsetNodes.sort((a, b) => a.currentCost - b.currentCost);


        // ************
        // The intermediate, not-fully-processed node sets.
        const ins2_dl1 = subsetNodes[1].processSubsetEdge(); // 'th', 'tr'
        const ins2_dl0 = subsetNodes[2].processSubsetEdge(); // 'ch'

        const fin_in2_dl1 = ins2_dl1.flatMap(n => n.processSubsetEdge());
        const fin_in2_dl0 = ins2_dl0.flatMap(n => n.processSubsetEdge());

        // All should be finished now!
        fin_in2_dl1.forEach(n => assert.isFalse(n.hasPartialInput));

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
          return n.resultKey == 'ty' && n.calculation.inputSequence.map(i => i.key).join('') == 't' + SENTINEL_CODE_UNIT && n.editCount == 1
        }).length, 1, "did not find expected result type");
        assert.isAtLeast(fin_in2_dl1.filter(n => {
          return n.resultKey == 'ch' && n.calculation.inputSequence.map(i => i.key).join('') == SENTINEL_CODE_UNIT + 'h' && n.editCount == 1
        }).length, 1, "did not find expected result type");
        assert.isAtLeast(fin_in2_dl1.filter(n => {
          return n.resultKey == 'ar' && n.calculation.inputSequence.map(i => i.key).join('') == SENTINEL_CODE_UNIT + 'r' && n.editCount == 1
        }).length, 1, "did not find expected result type");

        // And even cases where none match, though at twice the edit cost.
        assert.isAtLeast(fin_in2_dl1.filter(n => {
          return n.resultKey == 'an' && n.calculation.inputSequence.map(i => i.key).join('') == SENTINEL_CODE_UNIT + SENTINEL_CODE_UNIT && n.editCount == 2
        }).length, 1, "did not find expected result type");

        // *****
        // Now for the other set...
        assert.equal(fin_in2_dl0[0].resultKey, 'ch');
        assert.equal(fin_in2_dl0[0].editCount, 0);

        // We should also have single-char replacement entries for cases where one char matches...
        assert.isAtLeast(fin_in2_dl0.filter(n => {
          return n.resultKey == 'ca' && n.calculation.inputSequence.map(i => i.key).join('') == 'c' + SENTINEL_CODE_UNIT && n.editCount == 1
        }).length, 1, "did not find expected result type");
        assert.isAtLeast(fin_in2_dl0.filter(n => {
          return n.resultKey == 'th' && n.calculation.inputSequence.map(i => i.key).join('') == SENTINEL_CODE_UNIT + 'h' && n.editCount == 1
        }).length, 1, "did not find expected result type");

        // And even cases where none match, though at twice the edit cost.
        assert.isAtLeast(fin_in2_dl0.filter(n => {
          return n.resultKey == 'an' && n.calculation.inputSequence.map(i => i.key).join('') == SENTINEL_CODE_UNIT + SENTINEL_CODE_UNIT && n.editCount == 2
        }).length, 1, "did not find expected result type");
      });
    });

    it('Small integration test:  "teh" => "ten", "the"', function() {
      // The combinatorial effect here is a bit much to fully test.
      let rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      let rootNode = new correction.SearchNode(rootTraversal);
      assert.equal(rootNode.calculation.getHeuristicFinalCost(), 0);

      // VERY artificial distributions.
      let synthDistribution1 = [
        {sample: {insert: 't', deleteLeft: 0}, p: 1} // Transform, probability
      ];

      let synthDistribution2 = [
        {sample: {insert: 'e', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.25}
      ];

      let synthDistribution3 = [
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'n', deleteLeft: 0}, p: 0.25}
      ];

      let layer1Edges = rootNode.buildSubstitutionEdges(synthDistribution1)
        // No 2+ inserts here; we're fine with just one call.
        .flatMap(e => e.processSubsetEdge());
      let layer1Queue = new PriorityQueue(correction.QUEUE_NODE_COMPARATOR, layer1Edges);

      let tEdge = layer1Queue.dequeue();
      assertEdgeChars(tEdge, 't', 't');

      let layer2Edges = tEdge.buildSubstitutionEdges(synthDistribution2)
        // No 2+ inserts here; we're fine with just one call.
        .flatMap(e => e.processSubsetEdge());
      let layer2Queue = new PriorityQueue(correction.QUEUE_NODE_COMPARATOR, layer2Edges);

      let eEdge = layer2Queue.dequeue();
      assertEdgeChars(eEdge, 'e', 'e');

      let hEdge = layer2Queue.dequeue();
      assertEdgeChars(hEdge, 'h', 'h');

      // Needed for a proper e <-> h transposition.
      let ehEdge = findEdgesWithChars(layer2Edges, 'h')[0];

      assert.isOk(ehEdge);

      // Final round:  we'll use three nodes and throw all of their results into the same priority queue.
      let layer3eEdges  = eEdge.buildSubstitutionEdges(synthDistribution3)
        // No 2+ inserts here; we're fine with just one call.
        .flatMap(e => e.processSubsetEdge());
      let layer3hEdges  = hEdge.buildSubstitutionEdges(synthDistribution3)
        .flatMap(e => e.processSubsetEdge());
      let layer3ehEdges = ehEdge.buildSubstitutionEdges(synthDistribution3)
        .flatMap(e => e.processSubsetEdge());
      let layer3Queue = new PriorityQueue(correction.QUEUE_NODE_COMPARATOR, layer3eEdges.concat(layer3hEdges).concat(layer3ehEdges));

      // Find the first result with an actual word directly represented.
      let bestEdge;
      do {
        bestEdge = layer3Queue.dequeue();
      } while(bestEdge.calculation.lastMatchEntry.traversal.entries.length == 0);

      assertEdgeChars(bestEdge, 'n', 'n'); // 'ten' - perfect edit distance of 0, though less-likely input sequence.
      // No cost assumptions here.

      var sibling1;
      do {
        sibling1 = layer3Queue.dequeue();
      } while(sibling1.calculation.lastMatchEntry.traversal.entries.length == 0);

      // Both have a raw edit distance of 1 while using the same input-sequence root. ('th')
      let tenFlag = edgeHasChars(sibling1, SENTINEL_CODE_UNIT, 'n'); // subs out the 'h' entirely.  Could also occur with 'a', but is too unlikely.
      let theFlag = edgeHasChars(sibling1, SENTINEL_CODE_UNIT, 'e'); // looks for transposed 'h' and 'e'.

      assert.isTrue(tenFlag || theFlag);
      assert.isAbove(sibling1.currentCost, bestEdge.currentCost);

      var sibling2;
      do {
        sibling2 = layer3Queue.dequeue();
      } while(sibling2.calculation.lastMatchEntry.traversal.entries.length == 0);

      tenFlag = tenFlag || edgeHasChars(sibling2, SENTINEL_CODE_UNIT, 'n');
      theFlag = theFlag || edgeHasChars(sibling2, SENTINEL_CODE_UNIT, 'e');

      assert.isTrue(tenFlag && theFlag);
      assert.isAbove(sibling2.currentCost, sibling1.currentCost);
    });
  });

  describe('SearchSpaceTier + SearchSpace', function() {
    let checkResults_teh = async function(iter: AsyncGenerator<correction.SearchResult, any, any>) {
      let firstIterResult = await iter.next();  // {value: <actual value>, done: <iteration complete?>}
      assert.isFalse(firstIterResult.done);

      const firstResult: correction.SearchResult = firstIterResult.value; // Retrieves <actual value>
      // No checks on the first set's cost.
      assert.equal(firstResult.matchString, "ten");

      let secondBatch = [
        'beh',  'te',  'tec',
        'tech', 'tel', 'tem',
        'ter',  'tes', 'th',
        'the'
      ];

      async function checkBatch(batch: string[], prevCost: number) {
        let cost;
        while(batch.length > 0) {
          const iter_result = await iter.next();
          assert.isFalse(iter_result.done);

          const result = iter_result.value;
          assert.isAbove(result.totalCost, prevCost);
          if(cost !== undefined) {
            assert.equal(result.totalCost, cost);
          } else {
            cost = result.totalCost;
          }

          const matchIndex = batch.findIndex((entry) => entry == result.matchString);
          assert.notEqual(matchIndex, -1, `'${result.matchString}' received as prediction too early`);
          batch.splice(matchIndex, 1);
        }

        return cost;
      }

      const secondCost = await checkBatch(secondBatch, firstResult.totalCost);

      let thirdBatch = [
        'cen', 'en',  'gen',
        'ken', 'len', 'men',
        'sen', 'tha', 'then',
        'thi', 'tho', 'thr',
        'thu', 'wen'
      ];

      await checkBatch(thirdBatch, secondCost);
    }

    it('Simple search without input', async function() {
      // The combinatorial effect here is a bit much to fully test.
      let rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      let searchSpace = new correction.SearchSpace(testModel);

      let iter = searchSpace.getBestMatches(buildTestTimer());
      let firstResult = await iter.next();
      assert.isFalse(firstResult.done);
    });

    // Hmm... how best to update this...
    it.skip('Simple search (paralleling "Small integration test")', async function() {
      // The combinatorial effect here is a bit much to fully test.
      let rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      let searchSpace = new correction.SearchSpace(testModel);

      // VERY artificial distributions.
      let synthDistribution1 = [
        {sample: {insert: 't', deleteLeft: 0}, p: 1} // Transform, probability
      ];

      let synthDistribution2 = [
        {sample: {insert: 'e', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.25}
      ];

      let synthDistribution3 = [
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'n', deleteLeft: 0}, p: 0.25}
      ];

      searchSpace.addInput(synthDistribution1);
      searchSpace.addInput(synthDistribution2);
      searchSpace.addInput(synthDistribution3);

      let iter = searchSpace.getBestMatches(buildTestTimer()); // disables the correction-search timeout.
      await checkResults_teh(iter);
    });

    it.skip('Allows reiteration (sequentially)', async function() {
      // The combinatorial effect here is a bit much to fully test.
      let rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      let searchSpace = new correction.SearchSpace(testModel);

      // VERY artificial distributions.
      let synthDistribution1 = [
        {sample: {insert: 't', deleteLeft: 0}, p: 1} // Transform, probability
      ];

      let synthDistribution2 = [
        {sample: {insert: 'e', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.25}
      ];

      let synthDistribution3 = [
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'n', deleteLeft: 0}, p: 0.25}
      ];

      searchSpace.addInput(synthDistribution1);
      searchSpace.addInput(synthDistribution2);
      searchSpace.addInput(synthDistribution3);

      let iter = searchSpace.getBestMatches(buildTestTimer()); // disables the correction-search timeout.
      await checkResults_teh(iter);

      // The key: do we get the same results the second time?
      // Reset the iterator first...
      let iter2 = searchSpace.getBestMatches(buildTestTimer()); // disables the correction-search timeout.
      await checkResults_teh(iter2);
    });

    it('Empty search space, loaded model', async function() {
      // The combinatorial effect here is a bit much to fully test.
      let rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      let searchSpace = new correction.SearchSpace(testModel);
      const timer = buildTestTimer();
      let iter = searchSpace.getBestMatches(timer);

      // While there's no input, insertion operations can produce suggestions.
      let resultState = await iter.next();
      let result: SearchResult = resultState.value;

      // Just one suggestion root should be returned as the first result.
      assert.equal(result.totalCost, 0);             // Gives a perfect match
      assert.equal(result.inputSequence.length, 0);  // for a state with no input and
      assert.equal(result.matchString, '');          // an empty match string.
      assert.isFalse(resultState.done);

      // Should be able to reach more, though.
      let laterResultState = await iter.next();
      let laterResult: SearchResult = laterResultState.value;

      // Edit required:  an 'insertion' edge (no input matched, but char pulled
      // from lexicon)
      assert.isAbove(laterResult.totalCost, 0);
      // The most likely word in the lexicon starts with 't'.
      assert.equal(laterResult.matchString, 't');
      assert.isFalse(resultState.done);
    });
  });
});
