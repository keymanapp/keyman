import { assert } from 'chai';
import { ClassicalDistanceCalculation } from '#./correction/classical-calculation.js';

function prettyPrintMatrix(matrix) {
  for(let r = 0; r < matrix.length; r++) {
    console.log(JSON.stringify(matrix[r], function(key, value) {
      if(value == Number.MAX_VALUE) {
        return "MAX";
      } else if(value === undefined) {
        return -1;
      } else {
        return value;
      }
    }));
  }
}

function compute(input, match, mode, bandSize) {
  let buffer = new ClassicalDistanceCalculation();

  /* SUPPORTED MODES:
   * "InputThenMatch"  // adds all input chars, then all match chars.
   * "MatchThenInput"  // adds all match chars, then all input chars.
   */

  // TEMP:  once diagonal expansion is implemented, do this LATER, AFTER adding the chars.
  bandSize = bandSize || 1;
  buffer.diagonalWidth = bandSize;

  switch(mode || "InputThenMatch") {
    case "InputThenMatch":
      for(let i = 0; i < input.length; i++) {
        buffer = buffer.addInputChar({
          key: input.charAt(i)
        });
      }

      for(let j = 0; j < match.length; j++) {
        buffer = buffer.addMatchChar({
          key: match.charAt(j)
        });
      }
      break;
    case "MatchThenInput":
      for(let j = 0; j < match.length; j++) {
        buffer = buffer.addMatchChar({
          key: match.charAt(j)
        });
      }

      for(let i = 0; i < input.length; i++) {
        buffer = buffer.addInputChar({
          key: input.charAt(i)
        });
      }
      break;
    default:
      throw "Invalid test mode specified!"
  }

  return buffer;
}

describe('Classical Damerau-Levenshtein edit-distance calculation', function() {
  describe('unweighted', function() {
    it("'abc' -> 'abc' = 0", function() {
      assert.equal(compute("abc", "abc", "InputThenMatch").getFinalCost(), 0);
      assert.equal(compute("abc", "abc", "MatchThenInput").getFinalCost(), 0);
    });

    it("'abc' -> 'cab' = 2", function() {
      assert.equal(compute("abc", "cab", "InputThenMatch").getFinalCost(), 2);
      assert.equal(compute("abc", "cab", "MatchThenInput").getFinalCost(), 2);
    });

    it("'abc' -> 'cdb' = 3", function() {
      assert.equal(compute("abc", "cdb", "InputThenMatch").getFinalCost(), 3);
      assert.equal(compute("abc", "cdb", "MatchThenInput").getFinalCost(), 3);
    });

    it("'cab' -> 'bac' = 2", function() {
      assert.equal(compute("cab", "bac", "InputThenMatch").getFinalCost(), 2);
      assert.equal(compute("cab", "bac", "MatchThenInput").getFinalCost(), 2);
    });

    it("'cab' -> 'bdc' = 2", function() {
      assert.equal(compute("cab", "bdc", "InputThenMatch").getFinalCost(), 3);
      assert.equal(compute("cab", "bdc", "MatchThenInput").getFinalCost(), 3);
    });

    it("'access' -> 'assess' = 2", function() {
      assert.equal(compute("access", "assess", "InputThenMatch").getFinalCost(), 2);
      assert.equal(compute("access", "assess", "MatchThenInput").getFinalCost(), 2);
    })

    it("'foo' -> 'foo' = 0", function() {
      assert.equal(compute("foo", "foo", "InputThenMatch").getFinalCost(), 0);
      assert.equal(compute("foo", "foo", "MatchThenInput").getFinalCost(), 0);
    })

    it("'help' -> 'yelp' = 1", function() {
      assert.equal(compute("help", "yelp", "InputThenMatch").getFinalCost(), 1);
      assert.equal(compute("help", "yelp", "MatchThenInput").getFinalCost(), 1);
    })

    it("'teh' -> 'the' = 1", function() {
      assert.equal(compute("teh", "the", "InputThenMatch").getFinalCost(), 1);
      assert.equal(compute("teh", "the", "MatchThenInput").getFinalCost(), 1);
    });

    // 'aa' considered to be inserted within the transposition.
    it("'teaah' -> 'the' = 3", function() {
      assert.equal(compute("teaah", "the", "InputThenMatch").getFinalCost(), 3);  // Note:  requires diagonal of width 2, not 1.
      assert.equal(compute("teaah", "the", "MatchThenInput").getFinalCost(), 3);
    });

    // d & b transposed, then 'c' inserted.
    it("'adb' -> 'abcd' = 2", function() {
      assert.equal(compute("adb", "abcd", "InputThenMatch").getFinalCost(), 2);
      assert.equal(compute("adb", "abcd", "MatchThenInput").getFinalCost(), 2);
    });

    it("'the' -> '' = 3", function() {
      // Oh yeah, gotta do the null-string match case.
      assert.equal(compute("the", "", "InputThenMatch").getFinalCost(), 3);
      assert.equal(compute("the", "", "MatchThenInput").getFinalCost(), 3);
    });

    it("'' -> 'the' = 3", function() {
      // Oh yeah, gotta do the null-string match case.
      assert.equal(compute("", "the", "InputThenMatch").getFinalCost(), 3);
      assert.equal(compute("", "the", "MatchThenInput").getFinalCost(), 3);
    });

    it("'accomodate' -> 'accommodate' = 1", function() {
      assert.equal(compute("accomodate", "accommodate", "InputThenMatch").getFinalCost(), 1);
      assert.equal(compute("accomodate", "accommodate", "MatchThenInput").getFinalCost(), 1);
    });

    it("'belitttle' -> 'belittle' = 1", function() {
      assert.equal(compute("belitttle", "belittle", "InputThenMatch").getFinalCost(), 1);
      assert.equal(compute("belitttle", "belittle", "MatchThenInput").getFinalCost(), 1);
    });

    it("'harras' -> 'harass' = 2", function() {
      assert.equal(compute("harras", "harass", "InputThenMatch").getFinalCost(), 2);
      assert.equal(compute("harras", "harass", "MatchThenInput").getFinalCost(), 2);
    });

    it("'hiegth' -> 'height' = 2", function() {
      assert.equal(compute("hiegth", "height", "InputThenMatch").getFinalCost(), 2);
      assert.equal(compute("hiegth", "height", "MatchThenInput").getFinalCost(), 2);
    });

    it("'jellyifhs' -> 'jellyfish' = 2", function() {
      assert.equal(compute("jellyifhs", "jellyfish", "InputThenMatch").getFinalCost(), 2);
      assert.equal(compute("jellyifhs", "jellyfish", "MatchThenInput").getFinalCost(), 2);
    });

    it("'aadddres' -> 'address' = 3", function() {
      // If diagonal set to '1', cost is reported as 4.
      assert.equal(compute("aadddres", "address", "InputThenMatch").getFinalCost(), 3); // Error - is returning 5, not 4 (which would be correct for current implementation state)
      assert.equal(compute("aadddres", "address", "MatchThenInput").getFinalCost(), 3);
    });

    describe("Intermediate cost tests", function() {
      it("'abc' -> 'cab' (width 1) = 2", function() {
        // Technically a heuristic here, but it gets the right value b/c all changes are on the diagonal.
        assert.equal(compute("abc", "cab", "InputThenMatch").getHeuristicFinalCost(), 2);
        assert.equal(compute("abc", "cab", "MatchThenInput").getHeuristicFinalCost(), 2);
      });

      it("'aadddres' -> 'address' (width 1) = 4", function() {
        // If diagonal set to '1', cost is reported as 4.
        assert.equal(compute("aadddres", "address", "InputThenMatch").getHeuristicFinalCost(), 4);
        assert.equal(compute("aadddres", "address", "MatchThenInput").getHeuristicFinalCost(), 4);
      });

      it("'aadddres' -> 'address' (width 2) = 3", function() {
        // If diagonal set to '1', cost is reported as 4.
        assert.equal(compute("aadddres", "address", "InputThenMatch", 2).getHeuristicFinalCost(), 3);
        assert.equal(compute("aadddres", "address", "MatchThenInput", 2).getHeuristicFinalCost(), 3);
      });

      it("'jellyifhs' -> 'jellyfish' (width 1) = 2", function() {
        // Technically a heuristic here, but it gets the right value b/c all changes are on the diagonal.
        assert.equal(compute("jellyifhs", "jellyfish", "InputThenMatch").getHeuristicFinalCost(), 2);
        assert.equal(compute("jellyifhs", "jellyfish", "MatchThenInput").getHeuristicFinalCost(), 2);
      });

      // Two transpositions:  abc -> ca, ig <- ghi.  Also, one deletion:  'd'.
      it("'abcdefig' -> 'caefghi' (width 1) = 7", function() {
        let buffer = compute("abcdefig", "caefghi", "InputThenMatch");
        // This test case was constructed with the tranposition parts outside of the center diagonal.
        assert.equal(buffer.getHeuristicFinalCost(), 7);
      });

      // Two transpositions:  abc -> ca, ig <- ghi.  Also, one deletion:  'd'.
      it("'abcdefig' -> 'caefghi' (width 2) = 5", function() {
        let buffer = compute("abcdefig", "caefghi", "InputThenMatch", 2);
        assert.equal(buffer.getHeuristicFinalCost(), 5);
      });

      // Two transpositions:  abc -> ca, ig <- ghi.  Also, one deletion:  'd'.
      it("'abcdefigj' -> 'caefghij' (width 1) = 7", function() {
        let buffer = compute("abcdefigj", "caefghij", "InputThenMatch", 1);
        assert.equal(buffer.getHeuristicFinalCost(), 7);
      });

      // Two transpositions:  abc -> ca, ig <- ghi.  Also, one deletion:  'd'.
      it("'abcdefigj' -> 'caefghij' (width 2) = 5", function() {
        let buffer = compute("abcdefigj", "caefghij", "InputThenMatch", 2);
        assert.equal(buffer.getHeuristicFinalCost(), 5);
      });

      // Two transpositions:  abcd -> da, zx <- xyz and one deletion ('g')
      // The intermediate 'ef[g]hi' ensures that the two transpositions are kept separate.
      it("'abcdefghizx' -> 'daefhixyz' (width 2) = 8", function() {
        let buffer = compute("abcdefghizx", "daefhixyz", "InputThenMatch", 2);
        assert.equal(buffer.getHeuristicFinalCost(), 8);
      });

      // Two transpositions:  abcd -> da, zx <- xyz and one deletion ('g')
      // The intermediate 'ef[g]hi' ensures that the two transpositions are kept separate.
      it("'abcdefghizx' -> 'daefhixyz' (width 3) = 6", function() {
        let buffer = compute("abcdefghizx", "daefhixyz", "InputThenMatch", 3);
        assert.equal(buffer.getHeuristicFinalCost(), 6);
      });

      // Two transpositions:  abcd -> da, zw <- wxyz and two deletions ('gh')
      // The intermediate deletions help to ensure that the two transpositions are kept separate.
      it("'abcdefijzw' -> 'daefghijwxyz' (width 2) = 8", function() {
        let buffer = compute("abcdefijzw", "daefghijwxyz", "InputThenMatch", 2);
        assert.equal(buffer.getHeuristicFinalCost(), 8);
      });

      // One transposition:  da <- abcd, one insertion ('g'), and two deletions ('yz').
      it("'daefhiwxyz' -> 'abcdefghiyz' (width 1) = 9", function() {
        let buffer = compute("daefhiwxyz", "abcdefghiyz", "InputThenMatch", 1);
        assert.equal(buffer.getHeuristicFinalCost(), 9);
      });

      // One transposition:  da <- abcd, one insertion ('g'), and two deletions ('yz').
      it("'daefhiwxyz' -> 'abcdefghiyz' (width 2) = 7", function() {
        let buffer = compute("daefhiwxyz", "abcdefghiyz", "InputThenMatch", 2);
        assert.equal(buffer.getHeuristicFinalCost(), 7);
      });

      // One transposition:  da <- abcd, one insertion ('g'), and two deletions ('yz').
      it("'daefhiwxyz' -> 'abcdefghiyz' (width 3) = 6", function() {
        let buffer = compute("daefhiwxyz", "abcdefghiyz", "InputThenMatch", 3);
        assert.equal(buffer.getHeuristicFinalCost(), 6);
      });

      // Two transpositions:  da <- abcd, wxyz -> zw and one deletion ('g')
      it("'daefghiwxyz' -> 'abcdefhizw' (width 1) = 9", function() {
        let buffer = compute("daefghiwxyz", "abcdefhizw", "InputThenMatch", 1);
        assert.equal(buffer.getHeuristicFinalCost(), 9);
      });

      // Two transpositions:  da <- abcd, wxyz -> zw and one deletion ('g')
      it("'daefghiwxyz' -> 'abcdefhizw' (width 2) = 7", function() {
        let buffer = compute("daefghiwxyz", "abcdefhizw", "InputThenMatch", 2);
        assert.equal(buffer.getHeuristicFinalCost(), 7);
      });
    });

    describe("Diagonal extension tests", function() {
      it("'aadddres' -> 'address' (width 1->2) = 3", function() {
        // If diagonal set to '1', cost is reported as 4.
        let buffer = compute("aadddres", "address", "InputThenMatch", 1);
        assert.equal(buffer.getHeuristicFinalCost(), 4);

        // 1 -> 2
        buffer = buffer.increaseMaxDistance();
        assert.equal(buffer.getHeuristicFinalCost(), 3);
      });

      // Two transpositions:  abc -> ca, ig <- ghi.  Also, one deletion:  'd'.
      it("'abcdefig' -> 'caefghi' (width 1->2) = 5", function() {
        let buffer = compute("abcdefig", "caefghi", "InputThenMatch", 1);
        // This test case was constructed with the tranposition parts outside of the center diagonal.
        assert.equal(buffer.getHeuristicFinalCost(), 7);

        // 1 -> 2
        buffer = buffer.increaseMaxDistance();
        assert.equal(buffer.getHeuristicFinalCost(), 5);
      });

      // Two transpositions:  abc -> ca, ig <- ghi.  Also, one deletion:  'd'.
      it("'abcdefigj' -> 'caefghij' (width 1->2) = 5", function() {
        let buffer = compute("abcdefigj", "caefghij", "InputThenMatch", 1);
        // This test case was constructed with the tranposition parts outside of the center diagonal.
        assert.equal(buffer.getHeuristicFinalCost(), 7);

        // 1 -> 2
        buffer = buffer.increaseMaxDistance();
        assert.equal(buffer.getHeuristicFinalCost(), 5);
      });

      // Two transpositions:  abcd -> da, zx <- xyz and one deletion ('g')
      // The intermediate 'ef[g]hi' ensures that the two transpositions are kept separate.
      it("'abcdefghizx' -> 'daefhixyz' (width 1->2) = 8", function() {
        let buffer = compute("abcdefghizx", "daefhixyz", "InputThenMatch", 1);
        assert.equal(buffer.getHeuristicFinalCost(), Number.MAX_VALUE);

        // 1 -> 2
        // Will become too small during expansion if not properly checking
        // transpositions for the diagonal's left cell.
        buffer = buffer.increaseMaxDistance();
        assert.equal(buffer.getHeuristicFinalCost(), 8);
      });

      // Two transpositions:  abcd -> da, zx <- xyz and one deletion ('g')
      // The intermediate 'ef[g]hi' ensures that the two transpositions are kept separate.
      it("'abcdefghizx' -> 'daefhixyz' (width 2->3) = 6", function() {
        let buffer = compute("abcdefghizx", "daefhixyz", "InputThenMatch", 2);
        assert.equal(buffer.getHeuristicFinalCost(), 8);

        // 2 -> 3
        buffer = buffer.increaseMaxDistance();
        assert.equal(buffer.getHeuristicFinalCost(), 6);
      });

      // Two transpositions:  abcd -> da, zw <- wxyz and two deletions ('gh')
      // The intermediate deletions help to ensure that the two transpositions are kept separate.
      it("'abcdefijzw' -> 'daefghijwxyz' (width 1->2) = 8", function() {
        // Relies on a propagated insertion.
        let buffer = compute("abcdefijzw", "daefghijwxyz", "InputThenMatch", 1);
        assert.equal(buffer.getHeuristicFinalCost(), Number.MAX_VALUE);

        // 1 -> 2
        buffer = buffer.increaseMaxDistance();
        assert.equal(buffer.getHeuristicFinalCost(), 8);
      });

      // Two transpositions:  da <- abcd, wxyz -> zw and one deletion ('g')
      it("'daefghiwxyz' -> 'abcdefhizw' (width 1->2) = 7", function() {
        // Relies on a propagated transposition and multiple propagated substitutions.
        // Inspection of the diagonal-expansion update shows it involves a LOT of propagations!
        let buffer = compute("daefghiwxyz", "abcdefhizw", "InputThenMatch", 1);
        assert.equal(buffer.getHeuristicFinalCost(), 9);

        // 1 -> 2
        buffer = buffer.increaseMaxDistance();
        assert.equal(buffer.getHeuristicFinalCost(), 7);
      });

      // One transposition:  da <- abcd, one insertion ('g'), and two deletions ('yz').
      it("'daefhiwxyz' -> 'abcdefghiyz' (width 1->2) = 6", function() {
        let buffer = compute("daefhiwxyz", "abcdefghiyz", "InputThenMatch", 1);
        assert.equal(buffer.getHeuristicFinalCost(), 9);

        // 1 -> 2
        // Will become too small during expansion if not properly checking
        // transpositions for the diagonal's right cell.
        buffer = buffer.increaseMaxDistance();
        assert.equal(buffer.getHeuristicFinalCost(), 7);
      });

      // One transposition:  da <- abcd, one insertion ('g'), and two deletions ('yz').
      it("'daefhiwxyz' -> 'abcdefghiyz' (width 2->3) = 6", function() {
        let buffer = compute("daefhiwxyz", "abcdefghiyz", "InputThenMatch", 2);
        assert.equal(buffer.getHeuristicFinalCost(), 7);

        // 2 -> 3
        // relies on a propagated deletion!
        buffer = buffer.increaseMaxDistance();
        assert.equal(buffer.getHeuristicFinalCost(), 6);
      });
    });


    describe("Bounded final cost tests", function() {
      it("'adddress' -> 'address' has final cost within 4", function() {
        let buffer = compute("aadddres", "address", "InputThenMatch");
        assert.isTrue(buffer.hasFinalCostWithin(4));
      })

      it("'adddress' -> 'address' has final cost within 3", function() {
        let buffer = compute("aadddres", "address", "InputThenMatch");
        assert.isTrue(buffer.hasFinalCostWithin(3));
      })

      it("'adddress' -> 'address' does not have final cost within 2", function() {
        let buffer = compute("aadddres", "address", "InputThenMatch");
        assert.isFalse(buffer.hasFinalCostWithin(2));
      })
    });
  });

  describe("subset operations", function() {
    it("properly truncates tracked input & match sequences", function() {
      let buffer = compute("tear", "there", "InputThenMatch");
      assert.equal(buffer.getFinalCost(), 3);

      let trimmedBuffer = buffer.getSubset(3, 3);
      assert.equal(trimmedBuffer.inputSequence.length, 3);
      assert.equal(trimmedBuffer.matchSequence.length, 3);

      let expectedInput = ['t', 'e', 'a'];
      for(let i=0; i < expectedInput.length; i++) {
        assert.equal(trimmedBuffer.inputSequence[i].key, expectedInput[i]);
      }

      let expectedMatch = ['t', 'h', 'e'];
      for(let i=0; i < expectedMatch.length; i++) {
        assert.equal(trimmedBuffer.matchSequence[i].key, expectedMatch[i]);
      }
    });

    it("['tear' => 'there'] trimmed to ['tea' => 'the']", function() {
      let buffer = compute("tear", "there", "InputThenMatch");
      assert.equal(buffer.getFinalCost(), 3);

      let trimmedBuffer = buffer.getSubset(3, 3);
      assert.equal(trimmedBuffer.getFinalCost(), 2);
    });

    it("['tear' => 'there'] trimmed to ['te' => 'the']", function() {
      let buffer = compute("tear", "there", "InputThenMatch");
      assert.equal(buffer.getFinalCost(), 3);

      let trimmedBuffer = buffer.getSubset(2, 3);
      assert.equal(trimmedBuffer.getFinalCost(), 1);
    });

    it("['tear' => 'there'] trimmed to ['tear' => '']", function() {
      let buffer = compute("tear", "there", "InputThenMatch");
      assert.equal(buffer.getFinalCost(), 3);

      let trimmedBuffer = buffer.getSubset(4, 0);
      assert.equal(trimmedBuffer.getFinalCost(), 4);
    });

    it("['tear' => 'there'] trimmed to ['' => 'there']", function() {
      let buffer = compute("tear", "there", "InputThenMatch");
      assert.equal(buffer.getFinalCost(), 3);

      let trimmedBuffer = buffer.getSubset(0, 5);
      assert.equal(trimmedBuffer.getFinalCost(), 5);
    });

    it("['jellyifhs' -> 'jellyfish'] trimmed to ['jelly' => 'jelly']", function() {
      let buffer = compute("jellyifhs", "jellyfish");

      let trimmedBuffer = buffer.getSubset(5, 5);
      assert.equal(trimmedBuffer.getFinalCost(), 0)
    });

    it("['jellyifhs' -> 'jellyfish'] trimmed to ['jellyifh' => 'jellyfis']", function() {
      let buffer = compute("jellyifhs", "jellyfish");

      let trimmedBuffer = buffer.getSubset(8, 8);
      assert.equal(trimmedBuffer.getFinalCost(), 2)
    });
  });

  describe("edit path construction", function() {
    let op = {
      s: 'substitute',
      m: 'match',
      i: 'insert',
      d: 'delete',
      ts: 'transpose-start',
      te: 'transpose-end',
      ti: 'transpose-insert',
      td: 'transpose-delete'
    }

    it("'accomodate' -> 'accommodate'", function() {
      let buffer = compute("accomodate", "accommodate", "InputThenMatch");

      let editSequence = [
      //  a     c     c     o     m
        op.m, op.m, op.m, op.m, op.m,
      // +m    o      d     a     t    e
        op.i, op.m, op.m, op.m, op.m, op.m
      ];

      assert.deepEqual(buffer.editPath(), editSequence);
    });

    it("'harras' -> 'harass'", function() {
      let buffer = compute("harras", "harass", "InputThenMatch");

      let editSequence = [
        //  h     a     r    -r     a    s     +s
          op.m, op.m, op.m, op.d, op.m, op.m, op.i
      ];

      assert.deepEqual(buffer.editPath(), editSequence);
    });

    it("'access' -> 'assess'", function() {
      let buffer = compute("access", "assess", "InputThenMatch");

      let editSequence = [
        //  a    c/s   c/s    e     s    s
          op.m, op.s, op.s, op.m, op.m, op.m
      ];

      assert.deepEqual(buffer.editPath(), editSequence);
    });

    it("'ifhs' -> 'fish'", function() {
      let buffer = compute("ifhs", "fish");

      let editSequence = [
          op.ts, op.te, op.ts, op.te
      ];

      assert.deepEqual(buffer.editPath(), editSequence);
    });

    it("'then' -> 'their'", function() {
      let buffer = compute("then", "their");

      let editSequence = [
          op.m, op.m, op.m, op.s, op.i
      ];

      assert.deepEqual(buffer.editPath(), editSequence);
    });

    // Two transpositions:  abc -> ca, ig <- ghi.  Also, one deletion:  'd'.
    it("'abczdefig' -> 'cazefghi'", function() {
      let buffer = compute("abczdefig", "cazefghi", "InputThenMatch", 2);

      let editSequence = [
        // --- abc -> ca ----
        // a/c    b/_    c/a    z     -d    e     f
          op.ts, op.td, op.te, op.m, op.d, op.m, op.m,
        // --- ig -> ghi ----
        // i/g    _/h    g/i
          op.ts, op.ti, op.te
      ];

      assert.deepEqual(buffer.editPath(), editSequence);
    });
  })
});
