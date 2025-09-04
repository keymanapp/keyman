import { assert } from 'chai';
import { SegmentableDistanceCalculation } from '@keymanapp/lm-worker/test-index';

describe('Split/merge aware edit-distance calculation', () => {
  it("a,b,c -> a,b,d = 1", () => {
    let calc = new SegmentableDistanceCalculation();
    calc.diagonalWidth = 2;

    ['a', 'b', 'c'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'b', 'd'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 1);
  });

  it("a,b,c -> a,bc = 1", () => {
    let calc = new SegmentableDistanceCalculation();
    calc.diagonalWidth = 2;

    ['a', 'b', 'c'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'bc'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 1);
  });

  it("ab,c,d -> a,b,cd = 2", () => {
    let calc = new SegmentableDistanceCalculation();
    calc.diagonalWidth = 2;

    ['ab', 'c', 'd'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'b', 'cd'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 2);
  });

  it("a,bc,d -> a,b,c,d = 1", () => {
    let calc = new SegmentableDistanceCalculation();
    calc.diagonalWidth = 2;

    ['a', 'bc', 'd'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'b', 'c', 'd'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 1);
  });

  it("a,b,c,d -> a,bc,d = 1", () => {
    let calc = new SegmentableDistanceCalculation();
    calc.diagonalWidth = 2;

    ['a', 'b', 'c', 'd'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'bc', 'd'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 1);
  });

  it("ab,d,c,e,g,h -> a,b,c,d,f,gh = 4", () => {
    let calc = new SegmentableDistanceCalculation();
    calc.diagonalWidth = 4;

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
      { op: 'split', input: 'ab', match: 'a' },
      { op: 'split', input: 'ab', match: 'b' },
      { op: 'transpose-start', input: 'd', match: 'c' },
      { op: 'transpose-end', input: 'c', match: 'd' },
      { op: 'substitute', input: 'e', match: 'f' },
      { op: 'merge', input: 'g', match: 'gh' },
      { op: 'merge', input: 'h', match: 'gh' }
    ]);
  });

  it("abc,d,f,g,h -> a,b,c,d,fgh = 2", () => {
    let calc = new SegmentableDistanceCalculation();
    calc.diagonalWidth = 4;

    ['abc', 'd', 'f', 'g', 'h'].forEach(c => calc = calc.addInputChar(c));
    ['a', 'b', 'c', 'd', 'fgh'].forEach(c => calc = calc.addMatchChar(c));

    assert.equal(calc.getFinalCost(), 2);

    const editPaths = calc.editPath();
    assert.equal(editPaths.length, 1);
    assert.sameDeepOrderedMembers(editPaths[0], [
      { op: 'split', input: 'abc', match: 'a' },
      { op: 'split', input: 'abc', match: 'b' },
      { op: 'split', input: 'abc', match: 'c' },
      { op: 'match', input: 'd', match: 'd' },
      { op: 'merge', input: 'f', match: 'fgh' },
      { op: 'merge', input: 'g', match: 'fgh' },
      { op: 'merge', input: 'h', match: 'fgh' }
    ]);
  });
});