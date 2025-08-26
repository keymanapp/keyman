/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * This file contains low-level tests designed to validate helper functions
 * used when aligning cached context states to incoming contexts and when
 * validating potential substitution edit operations.
 */

import { assert } from 'chai';
import { computeAlignment, EditOperation, getEditPathLastMatch, isSubstitutionAlignable } from '@keymanapp/lm-worker/test-index';

describe('getEditPathLastMatch', () => {
  it('returns the last match when no substitutions exist', () => {
    const path: EditOperation[] = ['delete', 'delete', 'match', 'match', 'match', 'match', 'insert'];
    assert.equal(path.lastIndexOf('match'), 5);
    assert.equal(getEditPathLastMatch(path), 5);
  });

  it('returns the last match when no substitutions exist left of a "match"', () => {
    const path: EditOperation[] = ['delete', 'delete', 'match', 'match', 'match', 'match', 'substitute', 'insert'];
    assert.equal(path.lastIndexOf('match'), 5);
    assert.equal(getEditPathLastMatch(path), 5);
  });

  // is intended to handle application of suggestions.
  it('returns the last match before a substitute occurring after the first match', () => {
    // limitation:  if there is _anything_ after that last match, the first assertion will fail.
    //                                0         1         2        3           4          5        6
    const path: EditOperation[] = ['delete', 'delete', 'match', 'match', 'substitute', 'match', 'match'];
    assert.notEqual(getEditPathLastMatch(path), 6);
    assert.equal(getEditPathLastMatch(path), 3);
  });

  // is intended to handle complex transforms that include a whitespace and affect prior tokens.
  it('returns the last match before a substitute occurring after the first match', () => {
    // limitation:  if there is _anything_ after that last match, the first assertion will fail.
    //                                0         1         2        3           4          5           6
    const path: EditOperation[] = ['delete', 'delete', 'match', 'match', 'substitute', 'match', 'substitute'];
    assert.notEqual(getEditPathLastMatch(path), 5);
    assert.equal(getEditPathLastMatch(path), 3);
  });
});

describe('isSubstitutionAlignable', () => {
  it(`returns true:  'ca' => 'can'`, () => {
    assert.isTrue(isSubstitutionAlignable('can', 'ca'));
  });

  // Leading word in context window starts sliding out of said window.
  it(`returns true:  'can' => 'an'`, () => {
    assert.isTrue(isSubstitutionAlignable('an', 'can'));
  });

  // Same edits on both sides:  not valid.
  it(`returns false: 'apple' => 'grapples'`, () => {
    assert.isFalse(isSubstitutionAlignable('grapples', 'apple'));
  });

  // Edits on one side:  valid.
  it(`returns true: 'apple' => 'grapple'`, () => {
    assert.isTrue(isSubstitutionAlignable('grapple', 'apple'));
  });

  // Edits on one side:  valid.
  it(`returns true: 'apple' => 'grapple'`, () => {
    assert.isTrue(isSubstitutionAlignable('apples', 'apple'));
  });

  // Same edits on both sides:  not valid.
  it(`returns false: 'grapples' => 'apple'`, () => {
    assert.isFalse(isSubstitutionAlignable('apple', 'grapples'));
  });

  // Substitution:  not valid when not permitted via parameter.
  it(`returns false:  'apple' => 'banana'`, () => {
    // edit path:  'insert' ('b' of banana), 'match' (on leading a), rest are 'substitute'.
    assert.isFalse(isSubstitutionAlignable('banana', 'apple'));
  });

  // Substitution:  not valid if too much is substituted, even if allowed via parameter.
  it(`returns false:  'apple' => 'banana' (subs allowed)`, () => {
    // edit path:  'insert' ('b' of banana), 'match' (on leading a), rest are 'substitute'.
    // 1 match vs 4 substitute = no bueno.  It'd require too niche of a keyboard rule.
    assert.isFalse(isSubstitutionAlignable('banana', 'apple', true));
  });

  it(`returns true: 'a' => 'à' (subs allowed)`, () => {
    assert.isTrue(isSubstitutionAlignable('à', 'a', true));
  });

  // Leading substitution:  valid if enough of the remaining word matches.
  // Could totally happen from a legit Keyman keyboard rule.
  it(`returns true: 'can' => 'van' (subs allowed)`, () => {
    assert.isTrue(isSubstitutionAlignable('van', 'can', true));
  });

  // Trailing substitution:  invalid if not allowed.
  it(`returns false: 'can' => 'cap' (subs not allowed)`, () => {
    assert.isFalse(isSubstitutionAlignable('cap', 'can'));
  });

  // Trailing substitution:  valid.
  it(`returns false: 'can' => 'cap' (subs allowed)`, () => {
    assert.isTrue(isSubstitutionAlignable('cap', 'can', true));
  });

  it(`returns true:  'clasts' => 'clasps' (subs allowed)`, () => {
    assert.isTrue(isSubstitutionAlignable('clasps', 'clasts', true));
  });

  // random deletion at the start + later substitution = still permitted
  it(`returns false:  'clasts' => 'lasps' (subs allowed)`, () => {
    assert.isTrue(isSubstitutionAlignable('lasps', 'clasts', true));
  });

  // deletion, then sub at the start, duplicate letters with one dropped
  it(`returns true:  'applesauce' => 'plesauce' (subs not allowed)`, () => {
    // The double-p adds a fun complication once the first gets dropped.
    assert.isTrue(isSubstitutionAlignable('applesauce', 'plesauce'));
  });
});


describe('computeAlignment', () => {
  it("properly matches and aligns when contexts match", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [...baseContext];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 0,
      matchLength: 5,
      tailEditLength: 0,
      tailTokenShift: 0
    });
  });

  it("properly matches and aligns with applied-suggestion contexts", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'o'
    ];
    const newContext = [...baseContext];
    newContext[4] = 'over';

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 0,
      matchLength: 4,
      tailEditLength: 1,
      tailTokenShift: 0
    });
  });

  it("properly matches and aligns with applied-suggestion at start of context", () => {
    const baseContext = [
      'te'
    ];
    const newContext = [
      'testing',
      ' ',
      ''
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false, true);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 0,
      matchLength: 0,
      tailEditLength: 1,
      tailTokenShift: 2
    });
  });

  it("detects unalignable contexts - no matching tokens", () => {
    const baseContext = [
      'swift', 'tan', 'wolf', 'leaped', 'across'
    ];
    const newContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {canAlign: false, editPath: ['substitute', 'substitute', 'substitute', 'substitute', 'substitute']});
  });

  it("detects unalignable contexts - too many mismatching tokens", () => {
    const baseContext = [
      'swift', 'tan', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {canAlign: false, editPath: ['substitute', 'substitute', 'match', 'match', 'match']});
  });

  it("fails alignment for leading-edge word substitutions", () => {
    const baseContext = [
      'swift', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {canAlign: false, editPath: ['substitute', 'match', 'match', 'match', 'match']});
  });

  it("fails alignment for small leading-edge word substitutions", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'sick', 'brown', 'fox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {canAlign: false, editPath: ['substitute', 'match', 'match', 'match', 'match']});
  });

  it("properly matches and aligns when lead token is modified", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'uick', 'brown', 'fox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 0,
      matchLength: 5,
      tailEditLength: 0,
      tailTokenShift: 0
    });
  });

  it("properly matches and aligns when lead token is removed", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'brown', 'fox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: -1,
      matchLength: 4,
      tailEditLength: 0,
      tailTokenShift: 0
    });
  });

  it("properly matches and aligns when lead token is added", () => {
    const baseContext = [
      'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 1,
      matchLength: 4,
      tailEditLength: 0,
      tailTokenShift: 0
    });
  });

  it("properly matches and aligns when lead tokens are removed and modified", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'ox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: -2,
      matchLength: 3,
      tailEditLength: 0,
      tailTokenShift: 0
    });
  });

  it("properly matches and aligns when lead tokens are added and modified", () => {
    const baseContext = [
      'rown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 1,
      matchLength: 4,
      tailEditLength: 0,
      tailTokenShift: 0
    });
  });

  it("properly matches and aligns when lead token is removed and tail token is added", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'brown', 'fox', 'jumped', 'over', 'the'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: -1,
      matchLength: 4,
      tailEditLength: 0,
      tailTokenShift: 1
    });
  });

  it("properly matches and aligns when lead token and tail token are modified", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'ove'
    ];
    const newContext = [
      'uick', 'brown', 'fox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 0,
      matchLength: 4, // we treat 'quick' and 'uick' as the same
      tailEditLength: 1,
      tailTokenShift: 0
    });
  });

  it("properly matches and aligns when lead token and tail token are modified + new token appended", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'ove'
    ];
    const newContext = [
      'uick', 'brown', 'fox', 'jumped', 'over', 't'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 0,
      matchLength: 4, // we treat 'quick' and 'uick' as the same
      tailEditLength: 1,
      tailTokenShift: 1
    });
  });

  it("properly handles context window sliding backward", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'e', 'quick', 'brown', 'fox', 'jumped', 'ove'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 1,
      matchLength: 4, // we treat 'quick' and 'uick' as the same
      tailEditLength: 1,
      tailTokenShift: 0
    });
  });

  it("properly handles context window sliding far backward", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'the', 'quick', 'brown', 'fox', 'jumped'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 1,
      matchLength: 4, // we treat 'quick' and 'uick' as the same
      tailEditLength: 0,
      tailTokenShift: -1
    });
  });

  it("properly handles context window sliding farther backward", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'the', 'quick', 'brown', 'fox', 'jumpe'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 1,
      matchLength: 3, // we treat 'quick' and 'uick' as the same
      tailEditLength: 1,
      tailTokenShift: -1
    });
  });

  it("fails alignment for mid-head deletion", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'quick', 'fox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {canAlign: false, editPath: ["match", "delete", "match", "match", "match"]});
  });

  it("fails alignment for mid-head insertion", () => {
    const baseContext = [
      'quick', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {canAlign: false, editPath: ["match", "insert", "match", "match", "match"]});
  });

  it("fails alignment for mid-tail deletion", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'quick', 'brown', 'fox', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {canAlign: false, editPath: ["match", "match", "match", "delete", "match"]});
  });

  it("fails alignment for mid-tail insertion", () => {
    const baseContext = [
      'quick', 'brown', 'fox', 'jumped', 'over'
    ];
    const newContext = [
      'quick', 'brown', 'fox', 'jumped', 'far', 'over'
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {canAlign: false, editPath: ["match", "match", "match", "match", "insert", "match"]});
  });

  it("handles late-context suggestion application after backspace", () => {
    const baseContext = [
      'quick', ' ', 'brown', ' ', 'fox', ' ', 'jumped', ' ', 'oven', ' ', ''
    ];
    const newContext = [
      'quick', ' ', 'brown', ' ', 'fox', ' ', 'jumped', ' ', 'over', ' ', ''
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);
    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 0,
      matchLength: 8,
      tailEditLength: 3,
      tailTokenShift: 0
    });
  });

  it("handles late-context application of default suggestion", () => {
    const baseContext = [
      'quick', ' ', 'brown', ' ', 'fox', ' ', 'jumped', ' ', 'over', ' ', ''
    ];
    const newContext = [
      'quick', ' ', 'brown', ' ', 'fox', ' ', 'jumped', ' ', 'over', ' ', 'the', ' ', ''
    ];

    const computedAlignment = computeAlignment(baseContext, newContext, false);

    assert.deepEqual(computedAlignment, {
      canAlign: true,
      leadTokenShift: 0,
      matchLength: 10,
      tailEditLength: 1,
      tailTokenShift: 2
    });
  });

  it("handles sliding context-window scenarios", () => {
    // // Explicitly-defined window, though it's not needed directly by the method.
    // const config = {
    //   leftContextCodePoints: 64,
    //   rightContextCodePoints: 64
    // };

    const baseContext1 = [
      // "ap" prefix not in actual view, but preserved by prior tokenization rounds.
      "applesauce", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem", " ",
      "like", " ", "they'd", " ", "make", " ", "for", " ", "the", " ", "be"
    ];

    const incomingContext1 = [
      "plesauce", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem", " ",
      "like", " ", "they'd", " ", "make", " ", "for", " ", "the", " ", "bes"
    ];

    // 66 chars above, vs a sliding window of length 64.
    assert.equal(baseContext1.reduce((accum, curr) => accum + curr.length, 0), 66);
    // Actual window + one newly-typed character
    assert.equal(incomingContext1.reduce((accum, curr) => accum + curr.length, 0), 65);

    assert.deepEqual(computeAlignment(baseContext1, incomingContext1, true), {
      canAlign: true,
      leadTokenShift: 0,
      matchLength: 22,
      tailEditLength: 1,
      tailTokenShift: 0
    });

    // Our tokenization scheme remembers the full original word before any of it slid out of
    // the context window.
    const baseContext2 = [
      "applesauce", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem", " ",
      //                                                                 +2    +1     +4
      "like", " ", "they'd", " ", "make", " ", "for", " ", "the", " ", "best", " ", "brea"
    ];

    const incomingContext2 = [
      // "plesauce" => "e":  -7 chars.
      "e", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem", " ",
      "like", " ", "they'd", " ", "make", " ", "for", " ", "the", " ", "best", " ", "break"
    ];

    // 73 chars above, vs a sliding window of length 64.
    assert.equal(baseContext2.reduce((accum, curr) => accum + curr.length, 0), 73);
    // Actual window + one newly-typed character
    assert.equal(incomingContext2.reduce((accum, curr) => accum + curr.length, 0), 65);

    assert.deepEqual(computeAlignment(baseContext2, incomingContext2, true), {
      canAlign: true,
      leadTokenShift: 0,
      matchLength: 24,
      tailEditLength: 1,
      tailTokenShift: 0
    });

    const baseContext3 = [
      "applesauce", " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem", " ", "like", " ",
      "they'd", " ", "make", " ", "for", " ", "the", " ", "best", " ", "break"
    ];

    const incomingContext3 = [
      " ", "and", " ", "orange", " ", "juice", " ", "don't", " ", "seem", " ", "like", " ",
      "they'd", " ", "make", " ", "for", " ", "the", " ", "best", " ", "breakf"
    ];

    // 74 chars above, vs a sliding window of length 64.
    assert.equal(baseContext3.reduce((accum, curr) => accum + curr.length, 0), 74);
    // Actual window + one newly-typed character
    assert.equal(incomingContext3.reduce((accum, curr) => accum + curr.length, 0), 65);

    assert.deepEqual(computeAlignment(baseContext3, incomingContext3, true), {
      canAlign: true,
      leadTokenShift: -1,
      matchLength: 23,
      tailEditLength: 1,
      tailTokenShift: 0
    });
  });
});