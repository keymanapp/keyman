import { assert } from 'chai';
import { SegmentableDistanceCalculation } from '@keymanapp/lm-worker/test-index';

describe('Split/merge aware edit-distance calculation', () => {
  it("a,b,c -> a,b,d = 1", () => {
    let calc = new SegmentableDistanceCalculation({diagonalWidth: 2});

    ['a', 'b', 'c'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'b', 'd'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 1);
  });

  it("a,b,c -> a,bc = 1", () => {
    let calc = new SegmentableDistanceCalculation({diagonalWidth: 2});

    ['a', 'b', 'c'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'bc'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 1);
  });

  it("ab,c,d -> a,b,cd = 2", () => {
    let calc = new SegmentableDistanceCalculation({diagonalWidth: 2});

    ['ab', 'c', 'd'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'b', 'cd'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 2);
  });

  it("a,bc,d -> a,b,c,d = 1", () => {
    let calc = new SegmentableDistanceCalculation({diagonalWidth: 2});

    ['a', 'bc', 'd'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'b', 'c', 'd'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 1);
  });

  it("a,b,c,d -> a,bc,d = 1", () => {
    let calc = new SegmentableDistanceCalculation({diagonalWidth: 2});

    ['a', 'b', 'c', 'd'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'bc', 'd'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 1);
  });

  it("ab,d,c,e,g,h -> a,b,c,d,f,gh = 4", () => {
    let calc = new SegmentableDistanceCalculation({diagonalWidth: 4});

    // 'ab' => 'a', 'b'
    // d,c transposed
    // e => f substitution
    // 'g', 'h' => 'gh'

    ['ab', 'd', 'c', 'e', 'g', 'h'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'b', 'c', 'd', 'f', 'gh'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 4);

    const editPaths = calc.editPath();
    assert.equal(editPaths.length, 1);
    assert.sameDeepOrderedMembers(editPaths[0], [
      { op: 'split', input: 0, match: 0 },
      { op: 'split', input: 0, match: 1 },
      { op: 'transpose-start', input: 1, match: 2 },
      { op: 'transpose-end', input: 2, match: 3 },
      { op: 'substitute', input: 3, match: 4 },
      { op: 'merge', input: 4, match: 5 },
      { op: 'merge', input: 5, match: 5 }
    ]);
  });

  it("abc,d,f,g,h -> a,b,c,d,fgh = 2", () => {
    let calc = new SegmentableDistanceCalculation({diagonalWidth: 4});

    ['abc', 'd', 'f', 'g', 'h'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'b', 'c', 'd', 'fgh'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 2);

    const editPaths = calc.editPath();
    assert.equal(editPaths.length, 1);
    assert.sameDeepOrderedMembers(editPaths[0], [
      { op: 'split', input: 0, match: 0 },
      { op: 'split', input: 0, match: 1 },
      { op: 'split', input: 0, match: 2 },
      { op: 'match', input: 1, match: 3 },
      { op: 'merge', input: 2, match: 4 },
      { op: 'merge', input: 3, match: 4 },
      { op: 'merge', input: 4, match: 4 }
    ]);
  });

  it("abcde,f -> b,c,d,e,f = 2", () => {
    let calc = new SegmentableDistanceCalculation({diagonalWidth: 4});

    // Partial split = cost of 2:  1 for splitting, 1 for being incomplete.
    // (The 'a' doesn't land.)
    ['abcde', 'f'].forEach(c => calc = calc.addInputChar(c));
    ['b', 'c', 'd', 'e', 'f'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 2);

    const editPaths = calc.editPath();
    assert.equal(editPaths.length, 1);
    assert.sameDeepOrderedMembers(editPaths[0], [
      { op: 'split', input: 0, match: 0 },
      { op: 'split', input: 0, match: 1 },
      { op: 'split', input: 0, match: 2 },
      { op: 'split', input: 0, match: 3 },
      { op: 'match', input: 1, match: 4 }
    ]);
  });

  it("a,b,c,d,e -> a, bcdef = 2", () => {
    let calc = new SegmentableDistanceCalculation({diagonalWidth: 4});

    // Partial split = cost of 2:  1 for splitting, 1 for being incomplete.
    // (The 'a' doesn't land.)
    ['a', 'b', 'c', 'd', 'e'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'bcdef'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 2);

    const editPaths = calc.editPath();
    assert.equal(editPaths.length, 1);
    assert.sameDeepOrderedMembers(editPaths[0], [
      { op: 'match', input: 0, match: 0 },
      { op: 'merge', input: 1, match: 1 },
      { op: 'merge', input: 2, match: 1 },
      { op: 'merge', input: 3, match: 1 },
      { op: 'merge', input: 4, match: 1 }
    ]);
  });

  it("abc,d,e,f,g -> b,c,d,e,fgh = 4", () => {
    let calc = new SegmentableDistanceCalculation({diagonalWidth: 4});

    // Partial merge = cost of 2:  1 for merging, 1 for being incomplete.
    // Partial split = cost of 2:  1 for splitting, 1 for being incomplete.
    ['abc', 'd', 'e', 'f', 'g'].forEach(c => calc = calc.addInputChar(c));
    ['b', 'c', 'd', 'e', 'fgh'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 4);

    const editPaths = calc.editPath();
    assert.sameDeepOrderedMembers(editPaths[0], [
      { op: 'split', input: 0, match: 0 },
      { op: 'split', input: 0, match: 1 },
      { op: 'match', input: 1, match: 2 },
      { op: 'match', input: 2, match: 3 },
      { op: 'merge', input: 3, match: 4 },
      { op: 'merge', input: 4, match: 4 }
    ]);

    // ('insert', 'substitute') can be reordered
    // ('substitute', 'delete') can be reordered
    // There also exist cases where one pair or the other, but not both,
    // are considered for the edit path.
    assert.includeDeepMembers(editPaths, [[
      { op: 'insert',               match: 0 }, //       -> 'b'
      { op: 'substitute', input: 0, match: 1 }, // 'abc' -> 'c'
      { op: 'match',      input: 1, match: 2 }, // 'd'   == 'd'
      { op: 'match',      input: 2, match: 3 }, // 'e'   == 'e'
      { op: 'substitute', input: 3, match: 4 }, // 'f'   -> 'fgh'
      { op: 'delete',     input: 4 }            // 'g'   ->
    ]]);

    // both split and merge:  1 variation
    // split, but not merge:  2 variations
    // merge, but not split:  2 variations
    // neither:         2x2 = 4 variations
    // sum: 9
    assert.equal(editPaths.length, 9);
  });

  it("abcd,e,f,g -> b,c,d,efgh = 4", () => {
    let calc = new SegmentableDistanceCalculation({diagonalWidth: 4});

    // Partial merge = cost of 2:  1 for merging, 1 for being incomplete.
    // Partial split = cost of 2:  1 for splitting, 1 for being incomplete.
    ['abcd', 'e', 'f', 'g'].forEach(c => calc = calc.addInputChar(c));
    ['b', 'c', 'd', 'efgh'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 4);

    const editPaths = calc.editPath();
    assert.equal(editPaths.length, 2);
    assert.sameDeepOrderedMembers(editPaths[0], [
      { op: 'split', input: 0, match: 0 },
      { op: 'split', input: 0, match: 1 },
      { op: 'split', input: 0, match: 2 },
      { op: 'merge', input: 1, match: 3 },
      { op: 'merge', input: 2, match: 3 },
      { op: 'merge', input: 3, match: 3 }
    ]);

    assert.sameDeepOrderedMembers(editPaths[1], [
      { op: 'substitute', input: 0, match: 0 }, // 'abcd' -> 'b'
      { op: 'substitute', input: 1, match: 1 }, // 'e' -> 'c'
      { op: 'substitute', input: 2, match: 2 }, // 'f' -> 'd'
      { op: 'substitute', input: 3, match: 3 }  // 'g' -> 'efgh'
    ]);
  });
});