var assert = require('chai').assert;
let models = require('../../../build/intermediate').models;
let correction = require('../../../build/intermediate').correction;

function assertEdgeChars(edge, input, match) {
  assert.isTrue(edgeHasChars(edge, input, match));
}

function edgeHasChars(edge, input, match) {
  if(edge.optimalInput[edge.optimalInput.length - 1].sample.insert != input) {
    return false;
  }

  return edge.calculation.lastMatchEntry.key == match;
}

function findEdgeWithChars(edgeArray, input, match) {
  let results = edgeArray.filter(function(value) {
    return value.calculation.lastMatchEntry.key == match && value.optimalInput[value.optimalInput.length - 1].sample.insert == input;
  });

  assert.equal(results.length, 1);
  return results[0];
}

describe('Correction Distance Modeler', function() {
  describe('Search Node + Search Edge', function() {
    var testModel;

    before(function() {
      testModel = new models.TrieModel(jsonFixture('tries/english-1000'));
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
        assert.isEmpty(childEdge.optimalInput);
        assert.isEmpty(childEdge.calculation.inputSequence);
        assert.equal(childEdge.currentCost, 1);
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

      for(let edge of edges) {
        assert.isNotEmpty(edge.optimalInput);
        assert.isEmpty(edge.calculation.matchSequence);

        if(edge.optimalInput[0].p == 0.75) {
          // 't'.
          // 1.25:  assumes p = .75 ==> transform to input string cost of .25.
          assert.equal(edge.currentCost, 1.25);
        } else if(edge.optimalInput[0].p == 0.25) {
          // 'h'
        } else {
          assert.fail();
          assert.equal(edge.currentCost, 1.75);
        }
      }
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
      for(let child of rootTraversal.children()) {
        for(let mass of synthDistribution) {
          expectedChildCount++;
          
          let matchingEdge = findEdgeWithChars(edges, mass.sample.insert, child.char);

          // Substitution - matching char
          if(mass.sample.insert == matchingEdge.calculation.lastMatchEntry.key) {
            assert.equal(matchingEdge.currentCost, 1 - mass.p);
          } else {
            // not matching char.
            assert.equal(matchingEdge.currentCost, 2 - mass.p);
          }
        }
      }

      assert.equal(edges.length, expectedChildCount);

      // One final bit, which is a bit of integration - we know the top two nodes that should result.
      let queue = new models.PriorityQueue(correction.QUEUE_EDGE_COMPARATOR, edges);

      let firstEdge = queue.dequeue();
      assert.equal(firstEdge.optimalInput[0].sample.insert, 't');
      assert.equal(firstEdge.calculation.lastMatchEntry.key, 't');
      assert.equal(firstEdge.currentCost, 0.25);

      let secondEdge = queue.dequeue();
      assert.equal(secondEdge.optimalInput[0].sample.insert, 'h');
      assert.equal(secondEdge.calculation.lastMatchEntry.key, 'h');
      assert.equal(secondEdge.currentCost, 0.75);

      // After this, a 't' input without a matching char.
      let nextEdge = queue.dequeue();
      assert.equal(nextEdge.optimalInput[0].sample.insert, 't');
      assert.notEqual(nextEdge.calculation.lastMatchEntry.key, 't');
      assert.equal(nextEdge.currentCost, 1.25);
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
      let layer1Queue = new models.PriorityQueue(correction.QUEUE_EDGE_COMPARATOR, layer1Edges);

      let tEdge = layer1Queue.dequeue();
      assertEdgeChars(tEdge, 't', 't');

      let layer2Edges = new correction.SearchNode(tEdge).buildSubstitutionEdges(synthDistribution2);
      let layer2Queue = new models.PriorityQueue(correction.QUEUE_EDGE_COMPARATOR, layer2Edges);

      let eEdge = layer2Queue.dequeue();
      assertEdgeChars(eEdge, 'e', 'e');

      let hEdge = layer2Queue.dequeue();
      assertEdgeChars(hEdge, 'h', 'h');

      // Needed for a proper e <-> h transposition.
      let ehEdge = findEdgeWithChars(layer2Edges, 'e', 'h');

      assert.isOk(ehEdge);

      // Final round:  we'll use three nodes and throw all of their results into the same priority queue.
      let layer3eEdges  = new correction.SearchNode(eEdge).buildSubstitutionEdges(synthDistribution3);
      let layer3hEdges  = new correction.SearchNode(hEdge).buildSubstitutionEdges(synthDistribution3);
      let layer3ehEdges = new correction.SearchNode(ehEdge).buildSubstitutionEdges(synthDistribution3);
      let layer3Queue = new models.PriorityQueue(correction.QUEUE_EDGE_COMPARATOR, layer3eEdges.concat(layer3hEdges).concat(layer3ehEdges));

      // Find the first result with an actual word directly represented.
      do {
        edge = layer3Queue.dequeue();
      } while(edge.calculation.lastMatchEntry.traversal.entries.length == 0);

      assertEdgeChars(edge, 'n', 'n'); // 'ten' - perfect edit distance of 0, though less-likely input sequence.
      assert(edge.currentCost, 1);

      var edge;
      do {
        edge = layer3Queue.dequeue();
      } while(edge.calculation.lastMatchEntry.traversal.entries.length == 0);

      // Both have a raw edit distance of 1 while using the same input-sequence root. ('th')
      let tenFlag = edgeHasChars(edge, 'h', 'n'); // subs out the 'h' entirely.  Could also occur with 'a', but is too unlikely.
      let theFlag = edgeHasChars(edge, 'h', 'e'); // looks for transposed 'h' and 'e'.

      assert.isTrue(tenFlag || theFlag);
      assert.equal(edge.currentCost, 1.5);

      do {
        edge = layer3Queue.dequeue();
      } while(edge.calculation.lastMatchEntry.traversal.entries.length == 0);

      tenFlag = tenFlag || edgeHasChars(edge, 'h', 'n');
      theFlag = theFlag || edgeHasChars(edge, 'h', 'e');

      assert.isTrue(tenFlag && theFlag);
      assert.equal(edge.currentCost, 1.5);
    });
  });
});
