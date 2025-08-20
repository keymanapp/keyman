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
import { EditOperation, getEditPathLastMatch, isSubstitutionAlignable } from '@keymanapp/lm-worker/test-index';

describe('getEditPathLastMatch', () => {
  it('returns the last match when no substitutions exist', () => {
    const path: EditOperation[] = ['delete', 'delete', 'match', 'match', 'match', 'match', 'insert'];
    assert.equal(getEditPathLastMatch(path), path.lastIndexOf('match'));
  });

  it('returns the last match when no substitutions exist left of a "match"', () => {
    const path: EditOperation[] = ['delete', 'delete', 'match', 'match', 'match', 'match', 'substitute', 'insert'];
    assert.equal(getEditPathLastMatch(path), path.lastIndexOf('match'));
  });

  // is intended to handle application of suggestions.
  it('returns the last match before a substitute occurring after the first match', () => {
    // limitation:  if there is _anything_ after that last match, the first assertion will fail.
    const path: EditOperation[] = ['delete', 'delete', 'match', 'match', 'substitute', 'match', 'match'];
    assert.notEqual(getEditPathLastMatch(path), path.lastIndexOf('match'));
    assert.equal(getEditPathLastMatch(path), path.lastIndexOf('match', path.lastIndexOf('substitute')));
  });

  // is intended to handle complex transforms that include a whitespace and affect prior tokens.
  it('returns the last match before a substitute occurring after the first match', () => {
    // limitation:  if there is _anything_ after that last match, the first assertion will fail.
    const path: EditOperation[] = ['delete', 'delete', 'match', 'match', 'substitute', 'match', 'substitute'];
    assert.notEqual(getEditPathLastMatch(path), path.lastIndexOf('match'));
    assert.equal(getEditPathLastMatch(path), path.lastIndexOf('match', path.indexOf('substitute')));
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