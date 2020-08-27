var assert = require('chai').assert;
let models = require('../../../build/intermediate').models;
let correction = require('../../../build/intermediate').correction;

describe.only('Correction Distance Modeler', function() {
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

        let childEdge = edges.filter(value => value.calculation.lastMatchEntry.char == child.char)[0];
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
          
          let matchingEdge = edges.filter(function(value) {
            return value.calculation.lastMatchEntry.char == child.char &&
                   value.optimalInput[0] == mass;
          });

          assert.equal(matchingEdge.length, 1); // An array is returned, but should only hold the one entry.
          matchingEdge = matchingEdge[0]; // Array -> that one entry.

          // Substitution - matching char
          if(mass.sample.insert == matchingEdge.calculation.lastMatchEntry.char) {
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
      assert.equal(firstEdge.calculation.lastMatchEntry.char, 't');
      assert.equal(firstEdge.currentCost, 0.25);

      let secondEdge = queue.dequeue();
      assert.equal(secondEdge.optimalInput[0].sample.insert, 'h');
      assert.equal(secondEdge.calculation.lastMatchEntry.char, 'h');
      assert.equal(secondEdge.currentCost, 0.75);

      // After this, a 't' input without a matching char.
      let nextEdge = queue.dequeue();
      assert.equal(nextEdge.optimalInput[0].sample.insert, 't');
      assert.notEqual(nextEdge.calculation.lastMatchEntry.char, 't');
      assert.equal(nextEdge.currentCost, 1.25);
    });
  });
});