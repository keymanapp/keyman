import { assert } from 'chai';
import { ClassicalDistanceCalculation, EditOperation, EditTuple, forNewIndices } from '@keymanapp/lm-worker/test-index';
import sinon from 'sinon';

// Very useful for diagnosing unit test issues when path inspection is needed.
// Not currently used ('cause that's noisy during test runs).
export function prettyPrintMatrix(matrix: number[][]) {
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

function compute(input: string, match: string, mode?: string, bandSize?: number, allowTransposes: boolean = true) {
  let buffer = new ClassicalDistanceCalculation({noTransposes: !allowTransposes, diagonalWidth: bandSize || 1});

  /* SUPPORTED MODES:
   * "InputThenMatch"  // adds all input chars, then all match chars.
   * "MatchThenInput"  // adds all match chars, then all input chars.
   */

  switch(mode || "InputThenMatch") {
    case "InputThenMatch":
      for(let i = 0; i < input.length; i++) {
        buffer = buffer.addInputChar(input.charAt(i));
      }

      for(let j = 0; j < match.length; j++) {
        buffer = buffer.addMatchChar(match.charAt(j));
      }
      break;
    case "MatchThenInput":
      for(let j = 0; j < match.length; j++) {
        buffer = buffer.addMatchChar(match.charAt(j));
      }

      for(let i = 0; i < input.length; i++) {
        buffer = buffer.addInputChar(input.charAt(i));
      }
      break;
    default:
      throw "Invalid test mode specified!"
  }

  return buffer;
}

describe('forNewIndices', () => {
  it('iterates extended row properly (small calc)', () => {
    const mockedCalc = new ClassicalDistanceCalculation({diagonalWidth: 2});
    sinon.replaceGetter(mockedCalc, 'inputSequence', () => new Array(2));
    sinon.replaceGetter(mockedCalc, 'matchSequence', () => new Array(3));

    const fake = sinon.fake();
    forNewIndices(mockedCalc, true, fake);

    assert.equal(fake.callCount, 3);
    for(let i=0; i < fake.callCount; i++) {
      // row value should be fixed - the last row.
      assert.equal(fake.args[i][0], 1);

      // column value should increment on each step.
      assert.equal(fake.args[i][1], 0 + i);

      // actual column position within the internal representation of the row
      assert.equal(fake.args[i][2], 1 + i);
    }
  });

  it('iterates extended row properly (large calc)', () => {
    const mockedCalc = new ClassicalDistanceCalculation({diagonalWidth: 2});
    sinon.replaceGetter(mockedCalc, 'inputSequence', () => new Array(8));
    sinon.replaceGetter(mockedCalc, 'matchSequence', () => new Array(9));

    const fake = sinon.fake();
    forNewIndices(mockedCalc, true, fake);

    assert.equal(fake.callCount, 4);  // diagonalWidth + 1, + 1 because +1 column over rows.
    for(let i=0; i < fake.callCount; i++) {
      // row value should be fixed - the last row.
      assert.equal(fake.args[i][0], 7);

      // column value should increment on each step
      assert.equal(fake.args[i][1], 5 /* last row index - diagonalWidth */ + i);

      // actual column position within the internal representation of the row
      // and start as far left as possible
      assert.equal(fake.args[i][2], i);
    }
  });

  it('iterates extended column properly (small calc)', () => {
    const mockedCalc = new ClassicalDistanceCalculation({diagonalWidth: 2});
    sinon.replaceGetter(mockedCalc, 'inputSequence', () => new Array(3));
    sinon.replaceGetter(mockedCalc, 'matchSequence', () => new Array(2));

    const fake = sinon.fake();
    forNewIndices(mockedCalc, false, fake);

    assert.equal(fake.callCount, 3);
    for(let i=0; i < fake.callCount; i++) {
      // row value should increment on each step.
      assert.equal(fake.args[i][0], 0 + i);

      // column value should be fixed - the last column.
      assert.equal(fake.args[i][1], 1);

      // Actual column position within the internal representation of the row.
      // Note that internally, columns 'shift backward' when moving forward
      // in rows.
      assert.equal(fake.args[i][2], 3 - i);
    }
  });

  it('iterates extended column properly (large calc) (1)', () => {
    const mockedCalc = new ClassicalDistanceCalculation({diagonalWidth: 3});
    sinon.replaceGetter(mockedCalc, 'inputSequence', () => new Array(8));
    sinon.replaceGetter(mockedCalc, 'matchSequence', () => new Array(9));

    const fake = sinon.fake();
    forNewIndices(mockedCalc, false, fake);

    assert.equal(fake.callCount, 3); // diagonalWidth + 1,  -1 because 1 less row
    for(let i=0; i < fake.callCount; i++) {
      // row value should increment on each step.
      assert.equal(fake.args[i][0], (8 - 3) + i);

      // column value should be fixed - the last column.
      assert.equal(fake.args[i][1], 8);

      // Actual column position within the internal representation of the row.
      // Note that internally, columns 'shift backward' when moving forward
      // in rows.
      assert.equal(fake.args[i][2], 6 - i); // 6 = 2 * diagWidth
    }
  });

  it('iterates extended column properly (large calc) (2)', () => {
    const mockedCalc = new ClassicalDistanceCalculation({diagonalWidth: 3});
    sinon.replaceGetter(mockedCalc, 'inputSequence', () => new Array(9));
    sinon.replaceGetter(mockedCalc, 'matchSequence', () => new Array(9));

    const fake = sinon.fake();
    forNewIndices(mockedCalc, false, fake);

    assert.equal(fake.callCount, 4); // diagonalWidth + 1
    for(let i=0; i < fake.callCount; i++) {
      // row value should increment on each step.
      assert.equal(fake.args[i][0], (9 - 4) + i);

      // column value should be fixed - the last column.
      assert.equal(fake.args[i][1], 8);

      // Actual column position within the internal representation of the row.
      // Note that internally, columns 'shift backward' when moving forward
      // in rows.
      assert.equal(fake.args[i][2], 6 - i); // 6 = 2 * diagWidth
    }
  });
});

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

    it("'jellyifhs' -> 'jellyfish' = 2 (no transposes)", function() {
      // edits when without transposes:
      // delete i           (jellyfhs)
      // substitute h -> i  (jellyfis)
      // insert h at end    (jellyfish)
      assert.equal(compute("jellyifhs", "jellyfish", "InputThenMatch", 3, false).getFinalCost(), 3);
      assert.equal(compute("jellyifhs", "jellyfish", "MatchThenInput", 3, false).getFinalCost(), 3);
    });

    it("'aadddres' -> 'address' = 3", function() {
      // If diagonal set to '1', cost is reported as 4.
      assert.equal(compute("aadddres", "address", "InputThenMatch").getFinalCost(), 3); // Error - is returning 5, not 4 (which would be correct for current implementation state)
      assert.equal(compute("aadddres", "address", "MatchThenInput").getFinalCost(), 3);
    });

    it("handles empty-char inputs / matches appropriately", () => {
      let buffer = new ClassicalDistanceCalculation();

      // One transpose ('' vs 'b'), one insert ('' at end)
      const input = ['a', '', 'b', 'c', 'd'];
      const match = ['a', 'b', '', 'c', 'd', ''];

      for(let i = 0; i < input.length; i++) {
        buffer = buffer.addInputChar(input[i]);
      }

      for(let j = 0; j < match.length; j++) {
        buffer = buffer.addMatchChar(match[j]);
      }

      assert.equal(buffer.getFinalCost(), 2);
      const path = buffer.editPath();
      assert.includeMembers(path[0].map(e => e.op), ['transpose-start', 'transpose-end', 'insert']);
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
        assert.equal(trimmedBuffer.inputSequence[i], expectedInput[i]);
      }

      let expectedMatch = ['t', 'h', 'e'];
      for(let i=0; i < expectedMatch.length; i++) {
        assert.equal(trimmedBuffer.matchSequence[i], expectedMatch[i]);
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
    it("'accomodate' -> 'accommodate'", function() {
      let buffer = compute("accomodate", "accommodate", "InputThenMatch");

      // While the two 'm's are interchangable, the path will favor
      // a result with the longest contiguous set of matches.
      let editSequences: EditTuple<EditOperation>[][] = [[
        { op: 'match', input: 0 /* 'a' */, match: 0 /* 'a' */},
        { op: 'match', input: 1 /* 'c' */, match: 1 /* 'c' */},
        { op: 'match', input: 2 /* 'c' */, match: 2 /* 'c' */},
        { op: 'match', input: 3 /* 'o' */, match: 3 /* 'o' */},
        { op: 'insert',                    match: 4 /* 'm' */},  // insert is earlier
        { op: 'match', input: 4 /* 'm' */, match: 5 /* 'm' */}, // longer contiguous match sequence
        { op: 'match', input: 5 /* 'o' */, match: 6 /* 'o' */},
        { op: 'match', input: 6 /* 'd' */, match: 7 /* 'd' */},
        { op: 'match', input: 7 /* 'a' */, match: 8 /* 'a' */},
        { op: 'match', input: 8 /* 't' */, match: 9 /* 't' */},
        { op: 'match', input: 9 /* 'e' */, match: 10 /* 'e' */}
      ], [
        { op: 'match', input: 0 /* 'a' */, match: 0 /* 'a' */},
        { op: 'match', input: 1 /* 'c' */, match: 1 /* 'c' */},
        { op: 'match', input: 2 /* 'c' */, match: 2 /* 'c' */},
        { op: 'match', input: 3 /* 'o' */, match: 3 /* 'o' */},
        { op: 'match', input: 4 /* 'm' */, match: 4 /* 'm' */},
        { op: 'insert',                    match: 5 /* 'm' */}, // now inserting later.
        { op: 'match', input: 5 /* 'o' */, match: 6 /* 'o' */},
        { op: 'match', input: 6 /* 'd' */, match: 7 /* 'd' */},
        { op: 'match', input: 7 /* 'a' */, match: 8 /* 'a' */},
        { op: 'match', input: 8 /* 't' */, match: 9 /* 't' */},
        { op: 'match', input: 9 /* 'e' */, match: 10 /* 'e' */}
      ]];

      assert.sameDeepOrderedMembers(buffer.editPath(), editSequences);
    });

    it("'harras' -> 'harass'", function() {
      let buffer = compute("harras", "harass", "InputThenMatch");

      let bestEditSequence: EditTuple<string>[] = [
        { op: 'match', input: 0, match: 0}, // h, h
        { op: 'match', input: 1, match: 1}, // a, a
        { op: 'match', input: 2, match: 2}, // r, r
        { op: 'substitute', input: 3, match: 3}, // r => a
        { op: 'substitute', input: 4, match: 4}, // a => s
        { op: 'match', input: 5, match: 5}
      ];

      let altSequence: EditTuple<string>[] = [
        { op: 'match', input: 0, match: 0},
        { op: 'match', input: 1, match: 1},
        { op: 'match', input: 2, match: 2},
        { op: 'delete', input: 3},
        { op: 'match', input: 4, match: 3},
        { op: 'match', input: 5, match: 4},
        { op: 'insert',          match: 5}
      ];

      const viablePaths = buffer.editPath();
      // Note that the position of the 'delete' and the 'insert' in
      // `altSequence` may be repositioned +1 way each, making 2x2 = 4 ways in
      // total... then +1 for the substitute approach.
      assert.equal(viablePaths.length, 5);
      assert.deepEqual(viablePaths[0], bestEditSequence);
      assert.includeDeepMembers(viablePaths, [altSequence]);
    });

    it("'access' -> 'assess'", function() {
      let buffer = compute("access", "assess", "InputThenMatch");

      let editSequence: EditTuple<string>[] = [
        { op: 'match', input: 0, match: 0},
        { op: 'substitute', input: 1, match: 1},
        { op: 'substitute', input: 2, match: 2},
        { op: 'match', input: 3, match: 3},
        { op: 'match', input: 4, match: 4},
        { op: 'match', input: 5, match: 5}
      ];

      const viablePaths = buffer.editPath();
      assert.equal(viablePaths.length, 1);
      assert.deepEqual(viablePaths[0], editSequence);
    });

    it("'ifhs' -> 'fish'", function() {
      let buffer = compute("ifhs", "fish");

      let editSequence: EditTuple<string>[] = [
        { op: 'transpose-start', input: 0, match: 0},
        { op: 'transpose-end', input: 1, match: 1},
        { op: 'transpose-start', input: 2, match: 2},
        { op: 'transpose-end', input: 3, match: 3}
      ];

      const viablePaths = buffer.editPath();
      assert.equal(viablePaths.length, 1);
      assert.deepEqual(viablePaths[0], editSequence);
    });

    it("'jellyifhs' -> 'jellyfish' (no transposes)", function() {
      // edits when without transposes:
      // delete i           (jellyfhs)
      // substitute h -> i  (jellyfis)
      // insert h at end    (jellyfish)
      const buffer = compute("jellyifhs", "jellyfish", "InputThenMatch", 3, false);

      const viablePaths = buffer.editPath();
      assert.isAtLeast(viablePaths.length, 1);

      // Assert:  the following path exists as one option.
      assert.includeDeepMembers(viablePaths, [[
        { op: 'match', input: 0, match: 0},
        { op: 'match', input: 1, match: 1},
        { op: 'match', input: 2, match: 2},
        { op: 'match', input: 3, match: 3},
        { op: 'match', input: 4, match: 4},
        { op: 'delete', input: 5},
        { op: 'match', input: 6, match: 5},
        { op: 'substitute', input: 7, match: 6},
        { op: 'match', input: 8, match: 7},
        { op: 'insert', match: 8}
      ]]);
    });

    it("'then' -> 'their'", function() {
      let buffer = compute("then", "their");

      let editSequence: EditTuple<string>[] = [
        { op: 'match', input: 0, match: 0},
        { op: 'match', input: 1, match: 1}, // best sequence:  continguous matches
        { op: 'match', input: 2, match: 2}, // + adjacent substitutes
        { op: 'substitute', input: 3, match: 3},
        { op: 'insert',               match: 4},
      ];

      let altSequence: EditTuple<string>[] = [
        { op: 'match', input: 0, match: 0},
        { op: 'match', input: 1, match: 1},
        { op: 'match', input: 2, match: 2},
        { op: 'insert',          match: 3},
        { op: 'substitute', input: 3, match: 4},
      ];

      const viablePaths = buffer.editPath();
      assert.equal(viablePaths.length, 2);
      assert.deepEqual(viablePaths[0], editSequence);
      assert.deepEqual(viablePaths[1], altSequence);
    });

    // Two transpositions:  abc -> ca, ig <- ghi.  Also, one deletion:  'd'.
    it("'abczdefig' -> 'cazefghi'", function() {
      let buffer = compute("abczdefig", "cazefghi", "InputThenMatch", 2);

      let editSequence: EditTuple<string>[] = [
        { op: 'transpose-start', input: 0, match: 0},
        { op: 'transpose-delete', input: 1},
        { op: 'transpose-end', input: 2, match: 1},
        { op: 'match', input: 3, match: 2},
        { op: 'delete', input: 4},
        { op: 'match', input: 5, match: 3},
        { op: 'match', input: 6, match: 4},
        { op: 'transpose-start', input: 7, match: 5},
        { op: 'transpose-insert',          match: 6},
        { op: 'transpose-end', input: 8, match: 7}
      ];

      const viablePaths = buffer.editPath();
      assert.equal(viablePaths.length, 1);
      assert.deepEqual(viablePaths[0], editSequence);
    });

    it('works well for whitespaced-context analogues', () => {
      // Corresponds closely to an actual sliding-context window scenario that
      // once occurred. I did tweak the end ('r' was actually a 'p') for a bit
      // more rigor.
      const src = "a b c d?;e f g, h i j k l m n o p"
      const dst =   "q c d?;e f g, h i j k l m n o r"
      const buffer = compute(src, dst, "InputThenMatch", 4);

      let editSequence: EditTuple<string>[] = [
        { op: 'delete', input: 0},
        { op: 'delete', input: 1},
        { op: 'substitute', input: 2, match: 0},
        { op: 'match', input: 3, match: 1},
        { op: 'match', input: 4, match: 2},
        { op: 'match', input: 5, match: 3},
        { op: 'match', input: 6, match: 4},
        { op: 'match', input: 7, match: 5},
        { op: 'match', input: 8, match: 6},
        { op: 'match', input: 9, match: 7},
        { op: 'match', input: 10, match: 8},
        { op: 'match', input: 11, match: 9},
        { op: 'match', input: 12, match: 10},
        { op: 'match', input: 13, match: 11},
        { op: 'match', input: 14, match: 12},
        { op: 'match', input: 15, match: 13},
        { op: 'match', input: 16, match: 14},
        { op: 'match', input: 17, match: 15},
        { op: 'match', input: 18, match: 16},
        { op: 'match', input: 19, match: 17},
        { op: 'match', input: 20, match: 18},
        { op: 'match', input: 21, match: 19},
        { op: 'match', input: 22, match: 20},
        { op: 'match', input: 23, match: 21},
        { op: 'match', input: 24, match: 22},
        { op: 'match', input: 25, match: 23},
        { op: 'match', input: 26, match: 24},
        { op: 'match', input: 27, match: 25},
        { op: 'match', input: 28, match: 26},
        { op: 'match', input: 29, match: 27},
        { op: 'match', input: 30, match: 28},
        { op: 'match', input: 31, match: 29},
        { op: 'substitute', input: 32, match: 30},
      ];

      const viablePaths = buffer.editPath();
      // There are alternate variations for the first four tokens, but only one
      // puts the substitute next to the long stretch of matches.
      assert.equal(viablePaths.length, 4);
      assert.deepEqual(viablePaths[0], editSequence);
    });
  })
});
