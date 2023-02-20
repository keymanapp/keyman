import { assert } from 'chai';
import * as models from '#./models/index.js';
import * as correction from '#./correction/index.js';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

function assertEdgeChars(edge, input, match) {
  assert.isTrue(edgeHasChars(edge, input, match));
}

function edgeHasChars(edge, input, match) {
  if(edge.priorInput[edge.priorInput.length - 1].sample.insert != input) {
    return false;
  }

  return edge.calculation.lastMatchEntry.key == match;
}

function findEdgeWithChars(edgeArray, input, match) {
  let results = edgeArray.filter(function(value) {
    return value.calculation.lastMatchEntry.key == match && value.priorInput[value.priorInput.length - 1].sample.insert == input;
  });

  assert.equal(results.length, 1);
  return results[0];
}

describe('Correction Distance Modeler', function() {
  describe('SearchNode + SearchEdge', function() {
    var testModel;

    before(function() {
      testModel = new models.TrieModel(jsonFixture('models/tries/english-1000'));
    });

    it('SearchNode.buildInsertionEdges() - from root', function() {
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

    it('SearchNode.buildDeletionEdges() - from root', function() {
      let rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      let rootNode = new correction.SearchNode(rootTraversal);
      assert.equal(rootNode.calculation.getHeuristicFinalCost(), 0);

      let synthDistribution = [
        {sample: {insert: 't', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.25}
      ];

      let edges = rootNode.buildDeletionEdges(synthDistribution);
      assert.equal(edges.length, 2);

      let highCost, lowCost;
      for(let edge of edges) {
        assert.isNotEmpty(edge.priorInput);
        assert.isEmpty(edge.calculation.matchSequence);

        if(edge.priorInput[0].p == 0.75) {
          // 't'.
          lowCost = edge.currentCost;  // higher prob = lower cost.
        } else if(edge.priorInput[0].p == 0.25) {
          // 'h'
          highCost = edge.currentCost;
        } else {
          assert.fail();
        }
      }

      assert.isAbove(highCost, lowCost);
    });

    it('SearchNode.buildSubstitutionEdges() - from root', function() {
      // The combinatorial effect here is a bit much to fully test.
      let rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      let rootNode = new correction.SearchNode(rootTraversal);
      assert.equal(rootNode.calculation.getHeuristicFinalCost(), 0);

      let synthDistribution = [
        {sample: {insert: 't', deleteLeft: 0}, p: 0.75}, // Transform, probability
        {sample: {insert: 'h', deleteLeft: 0}, p: 0.25}
      ];

      let edges = rootNode.buildSubstitutionEdges(synthDistribution);
      assert.isAbove(edges.length, 0);

      let expectedChildCount = 0;
      for(let mass of synthDistribution) { // probability may vary here
        let highCost, lowCost;

        // Within here, cost only varies based on edit-distance changes, allowing
        // us to make an assertion on the relationship between costs for each edit type.
        for(let child of rootTraversal.children()) {
          expectedChildCount++;

          let matchingEdge = findEdgeWithChars(edges, mass.sample.insert, child.char);

          // Substitution - matching char
          if(mass.sample.insert == matchingEdge.calculation.lastMatchEntry.key) {
            if(lowCost) {
              assert.equal(matchingEdge.currentCost, lowCost);
            }
            lowCost = matchingEdge.currentCost;
          } else {
            // not matching char.
            if(highCost) {
              assert.equal(matchingEdge.currentCost, highCost);
            }
            highCost = matchingEdge.currentCost;
          }
        }
        assert.isAbove(highCost, lowCost);
      }

      assert.equal(edges.length, expectedChildCount);

      // One final bit, which is a bit of integration - we know the top two nodes that should result.
      let queue = new models.PriorityQueue(correction.QUEUE_NODE_COMPARATOR, edges);

      let firstEdge = queue.dequeue();
      assert.equal(firstEdge.priorInput[0].sample.insert, 't');
      assert.equal(firstEdge.calculation.lastMatchEntry.key, 't');
      assert.isAbove(firstEdge.currentCost, 0);

      let secondEdge = queue.dequeue();
      assert.equal(secondEdge.priorInput[0].sample.insert, 'h');
      assert.equal(secondEdge.calculation.lastMatchEntry.key, 'h');
      assert.isAbove(secondEdge.currentCost, firstEdge.currentCost);

      // After this, a 't' input without a matching char.
      let nextEdge = queue.dequeue();
      assert.equal(nextEdge.priorInput[0].sample.insert, 't');
      assert.notEqual(nextEdge.calculation.lastMatchEntry.key, 't');
      assert.isAbove(nextEdge.currentCost, secondEdge.currentCost);
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

      let layer1Edges = rootNode.buildSubstitutionEdges(synthDistribution1);
      let layer1Queue = new models.PriorityQueue(correction.QUEUE_NODE_COMPARATOR, layer1Edges);

      let tEdge = layer1Queue.dequeue();
      assertEdgeChars(tEdge, 't', 't');

      let layer2Edges = tEdge.buildSubstitutionEdges(synthDistribution2);
      let layer2Queue = new models.PriorityQueue(correction.QUEUE_NODE_COMPARATOR, layer2Edges);

      let eEdge = layer2Queue.dequeue();
      assertEdgeChars(eEdge, 'e', 'e');

      let hEdge = layer2Queue.dequeue();
      assertEdgeChars(hEdge, 'h', 'h');

      // Needed for a proper e <-> h transposition.
      let ehEdge = findEdgeWithChars(layer2Edges, 'e', 'h');

      assert.isOk(ehEdge);

      // Final round:  we'll use three nodes and throw all of their results into the same priority queue.
      let layer3eEdges  = eEdge.buildSubstitutionEdges(synthDistribution3);
      let layer3hEdges  = hEdge.buildSubstitutionEdges(synthDistribution3);
      let layer3ehEdges = ehEdge.buildSubstitutionEdges(synthDistribution3);
      let layer3Queue = new models.PriorityQueue(correction.QUEUE_NODE_COMPARATOR, layer3eEdges.concat(layer3hEdges).concat(layer3ehEdges));

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
      let tenFlag = edgeHasChars(sibling1, 'h', 'n'); // subs out the 'h' entirely.  Could also occur with 'a', but is too unlikely.
      let theFlag = edgeHasChars(sibling1, 'h', 'e'); // looks for transposed 'h' and 'e'.

      assert.isTrue(tenFlag || theFlag);
      assert.isAbove(sibling1.currentCost, bestEdge.currentCost);

      var sibling2;
      do {
        sibling2 = layer3Queue.dequeue();
      } while(sibling2.calculation.lastMatchEntry.traversal.entries.length == 0);

      tenFlag = tenFlag || edgeHasChars(sibling2, 'h', 'n');
      theFlag = theFlag || edgeHasChars(sibling2, 'h', 'e');

      assert.isTrue(tenFlag && theFlag);
      assert.equal(sibling2.currentCost, sibling1.currentCost);
      assert.isAbove(sibling2.currentCost, bestEdge.currentCost);
    });
  });

  describe('SearchSpaceTier + SearchSpace', function() {
    var testModel;

    before(function() {
      testModel = new models.TrieModel(jsonFixture('models/tries/english-1000'));
    });

    let checkResults_teh = function(iter) {
      let firstSet = iter.next();  // {value: <actual value>, done: <iteration complete?>}
      assert.isFalse(firstSet.done);

      firstSet = firstSet.value; // Retrieves <actual value>
      // No checks on the first set's cost.
      assert.equal(firstSet.length, 1); // A single sequence ("ten") should be the best match.
      assert.equal(firstSet[0].matchString, "ten");

      let secondBatch = [
        'beh',  'te',  'tec',
        'tech', 'tel', 'tem',
        'ter',  'tes', 'th',
        'the'
      ];

      let secondSet = iter.next();  // {value: <actual value>, done: <iteration complete?>}
      assert.isFalse(secondSet.done);

      secondSet = secondSet.value; // Retrieves <actual value>
      assert.isAbove(secondSet[0].totalCost, firstSet[0].totalCost); // assert it 'costs more'.
      assert.equal(secondSet.length, secondBatch.length);

      let entries = secondSet.map(result => result.matchString).sort();
      assert.deepEqual(entries, secondBatch);

      let thirdBatch = [
        'cen', 'en',  'gen',
        'ken', 'len', 'men',
        'sen', 'tha', 'then',
        'thi', 'tho', 'thr',
        'thu', 'wen'
      ];

      let thirdSet = iter.next();  // {value: <actual value>, done: <iteration complete?>}
      assert.isFalse(thirdSet.done);

      thirdSet = thirdSet.value; // Retrieves <actual value>
      assert.isAbove(thirdSet[0].totalCost, secondSet[0].totalCost); // assert it 'costs more'.
      assert.equal(thirdSet.length, thirdBatch.length);

      entries = thirdSet.map(result => result.matchString).sort();
      assert.deepEqual(entries, thirdBatch);
    }

    it('Simple search without input', function() {
      // The combinatorial effect here is a bit much to fully test.
      let rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      let searchSpace = new correction.SearchSpace(testModel);

      let iter = searchSpace.getBestMatches();
      let firstSet = iter.next();
      assert.isFalse(firstSet.done);
    });

    it('Simple search (paralleling "Small integration test")', function() {
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

      let iter = searchSpace.getBestMatches(0); // disables the correction-search timeout.
      checkResults_teh(iter);

      // // Debugging method:  a simple loop for printing out the generated sets, in succession.
      // //
      // for(let i = 1; i <= 8; i++) {  // After 8 tiers, we run out of entries for this particular case.
      //   console.log();
      //   console.log("Batch " + i);

      //   let set = iter.next();
      //   assert.isFalse(set.done);

      //   set = set.value;

      //   let entries = set.map(function(result) {
      //     return result.matchString;
      //   });

      //   console.log("Entry count: " + set.length);
      //   entries.sort();
      //   console.log(entries);
      //   console.log("Probablility:  " + set[0].totalCost);
      //   console.log("Analysis (first entry):");
      //   console.log(" - Edit cost:  " + set[0].knownCost);
      //   console.log(" - Input cost: " + set[0].inputSamplingCost);
      // }
    });

    it('Allows reiteration (sequentially)', function() {
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

      let iter = searchSpace.getBestMatches(0); // disables the correction-search timeout.
      checkResults_teh(iter);

      // The key: do we get the same results the second time?
      // Reset the iterator first...
      let iter2 = searchSpace.getBestMatches(0); // disables the correction-search timeout.
      checkResults_teh(iter2);
    });

    it('Empty search space, loaded model', function() {
      // The combinatorial effect here is a bit much to fully test.
      let rootTraversal = testModel.traverseFromRoot();
      assert.isNotEmpty(rootTraversal);

      let searchSpace = new correction.SearchSpace(testModel);
      let iter = searchSpace.getBestMatches();

      // While there's no input, insertion operations can produce suggestions.
      let resultState = iter.next();
      let results = resultState.value;

      // Just one suggestion should be returned.
      assert.equal(results.length, 1);
      assert.equal(results[0].totalCost, 0);             // Gives a perfect match
      assert.equal(results[0].inputSequence.length, 0);  // for a state with no input and
      assert.equal(results[0].matchString, '');          // an empty match string.
      assert.isFalse(resultState.done);
    });
  });
});
