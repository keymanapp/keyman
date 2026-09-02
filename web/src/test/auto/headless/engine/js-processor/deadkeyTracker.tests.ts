/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { assert } from 'chai';
import { Deadkey, unitTestEndpoints } from 'keyman/engine/keyboard';

const DeadkeyTracker = unitTestEndpoints.DeadkeyTracker;

describe('DeadkeyTracker', function() {
  it('can add deadkeys and return count', function() {
    const tracker = new DeadkeyTracker();
    tracker.add(new Deadkey(3,  1));
    tracker.add(new Deadkey(5,  2));
    tracker.add(new Deadkey(10, 3));

    assert.equal(tracker.dks.length, 3);
    assert.equal(tracker.count(), 3);
  });

  it('sorts deadkeys by position, latest first', function() {
    const tracker = new DeadkeyTracker();
    tracker.add(new Deadkey(5,  2));
    tracker.add(new Deadkey(3,  1));
    tracker.add(new Deadkey(10, 3));

    const sortedDeadkeys = tracker.toSortedArray();

    assert.equal(sortedDeadkeys[0].p, 10);
    assert.equal(sortedDeadkeys[1].p, 5);
    assert.equal(sortedDeadkeys[2].p, 3);
  });

  it('can adjust positions', function () {
    const tracker = new DeadkeyTracker();
    tracker.add(new Deadkey(3, 1));
    tracker.add(new Deadkey(5, 2));
    tracker.add(new Deadkey(10, 3));

    tracker.adjustPositions(4, 2); // adjust deadkeys after position 4 by +2

    const sortedDeadkeys = tracker.toSortedArray();
    assert.equal(sortedDeadkeys[0].p, 12); // 10 + 2
    assert.equal(sortedDeadkeys[1].p, 7);  // 5 + 2
    assert.equal(sortedDeadkeys[2].p, 3);  // unchanged
  });

  describe('equal', function () {
    it('treats cloned tracker as equal', function () {
      const tracker1 = new DeadkeyTracker();
      tracker1.add(new Deadkey(3, 1));
      tracker1.add(new Deadkey(5, 2));
      tracker1.add(new Deadkey(10, 3));

      const tracker2 = tracker1.clone();

      assert.isTrue(tracker1.equal(tracker2));
    });

    it('treats two trackers with identical deadkeys as equal', function () {
      const dk1 = new Deadkey(3, 1);
      const dk2 = new Deadkey(5, 2);
      const dk3 = new Deadkey(10, 3);
      const tracker1 = new DeadkeyTracker();
      tracker1.add(dk1);
      tracker1.add(dk2);
      tracker1.add(dk3);

      const tracker2 = new DeadkeyTracker();
      tracker2.add(dk1);
      tracker2.add(dk2);
      tracker2.add(dk3);

      assert.isTrue(tracker1.equal(tracker2));
    });

    it('treats two trackers with same deadkeys create separately not as equal', function () {
      const tracker1 = new DeadkeyTracker();
      tracker1.add(new Deadkey(3, 1));
      tracker1.add(new Deadkey(5, 2));
      tracker1.add(new Deadkey(10, 3));

      const tracker2 = new DeadkeyTracker();
      tracker2.add(new Deadkey(3, 1));
      tracker2.add(new Deadkey(5, 2));
      tracker2.add(new Deadkey(10, 3));

      assert.isFalse(tracker1.equal(tracker2));
    });

    it('treats two trackers with same deadkeys in different order not as equal', function () {
      const tracker1 = new DeadkeyTracker();
      tracker1.add(new Deadkey(5, 2));
      tracker1.add(new Deadkey(3, 1));
      tracker1.add(new Deadkey(10, 3));

      const tracker2 = new DeadkeyTracker();
      tracker2.add(new Deadkey(10, 3));
      tracker2.add(new Deadkey(5, 2));
      tracker2.add(new Deadkey(3, 1));

      assert.isFalse(tracker1.equal(tracker2));
    });

    it('treats two trackers with same deadkeys in different places not as equal', function () {
      const tracker1 = new DeadkeyTracker();
      tracker1.add(new Deadkey(7, 2));
      tracker1.add(new Deadkey(3, 1));
      tracker1.add(new Deadkey(12, 3));

      const tracker2 = new DeadkeyTracker();
      tracker2.add(new Deadkey(10, 3));
      tracker2.add(new Deadkey(5, 2));
      tracker2.add(new Deadkey(3, 1));

      assert.isFalse(tracker1.equal(tracker2));
    });

    it('treats a tracker with additional deadkeys not as equal', function () {
      const tracker1 = new DeadkeyTracker();
      tracker1.add(new Deadkey(3, 1));
      tracker1.add(new Deadkey(5, 2));
      tracker1.add(new Deadkey(10, 3));

      const tracker2 = tracker1.clone();
      tracker2.add(new Deadkey(15, 4));

      assert.isFalse(tracker1.equal(tracker2));
    });

    it('treats non-indentical trackers with duplicate deadkeys not as equal', function () {
      const dk1 = new Deadkey(3, 1);
      const dk2 = new Deadkey(5, 2);
      const dk3 = new Deadkey(10, 3);
      const tracker1 = new DeadkeyTracker();
      tracker1.add(dk1);
      tracker1.add(dk1); // add same again
      tracker1.add(dk2);

      const tracker2 = new DeadkeyTracker();
      tracker2.add(dk1);
      tracker2.add(dk2);
      tracker2.add(dk3);

      assert.isFalse(tracker1.equal(tracker2));
    });

    it('treats indentical trackers with duplicate deadkeys as equal', function () {
      const dk1 = new Deadkey(3, 1);
      const dk2 = new Deadkey(5, 2);
      const dk3 = new Deadkey(10, 3);
      const tracker1 = new DeadkeyTracker();
      tracker1.add(dk1);
      tracker1.add(dk1); // add same again
      tracker1.add(dk2);
      tracker1.add(dk3);

      const tracker2 = new DeadkeyTracker();
      tracker2.add(dk1);
      tracker2.add(dk1); // add same again
      tracker2.add(dk2);
      tracker2.add(dk3);

      assert.isTrue(tracker1.equal(tracker2));
    });
  });

  it('can clear deadkeys', function() {
    const tracker = new DeadkeyTracker();
    tracker.add(new Deadkey(3,  1));
    tracker.add(new Deadkey(5,  2));
    tracker.add(new Deadkey(10, 3));

    tracker.clear();

    assert.equal(tracker.count(), 0);
  });

  describe('remove', function () {
    it('can remove existing deadkey', function () {
      const tracker = new DeadkeyTracker();
      tracker.add(new Deadkey(3, 1));
      const dk = new Deadkey(5, 2);
      tracker.add(dk);
      tracker.add(new Deadkey(10, 3));

      tracker.remove(dk);

      assert.equal(tracker.count(), 2);
      assert.equal(tracker.dks[0].p, 3);
      assert.equal(tracker.dks[1].p, 10);
    });

    it('does not remove similar deadkey', function () {
      const tracker = new DeadkeyTracker();
      tracker.add(new Deadkey(3, 1));
      tracker.add(new Deadkey(5, 2));
      tracker.add(new Deadkey(10, 3));

      tracker.remove(new Deadkey(5, 2));

      assert.equal(tracker.count(), 3);
      assert.equal(tracker.dks[0].p, 3);
      assert.equal(tracker.dks[1].p, 5);
      assert.equal(tracker.dks[2].p, 10);
    });
  });

  it('sorts deadkeys in cloned tracker', function () {
    const tracker1 = new DeadkeyTracker();
    tracker1.add(new Deadkey(5, 1));
    tracker1.add(new Deadkey(3, 2));
    tracker1.add(new Deadkey(10, 3));

    const tracker2 = tracker1.clone();

    assert.equal(tracker2.count(), 3);
    assert.equal(tracker2.dks[0].p, 10);
    assert.equal(tracker2.dks[1].p, 5);
    assert.equal(tracker2.dks[2].p, 3);
  });

  it('can delete matched deadkeys', function () {
    const dk1 = new Deadkey(5, 1);
    const dk2 = new Deadkey(3, 2);
    const dk3 = new Deadkey(10, 3);
    const tracker = new DeadkeyTracker();
    tracker.add(dk1);
    tracker.add(dk2);
    tracker.add(dk3);
    dk2.set();
    dk3.set();

    tracker.deleteMatched();

    assert.equal(tracker.count(), 1);
    assert.equal(tracker.dks[0].p, 5);
  });

  it('can reset matched deadkeys', function () {
    const dk1 = new Deadkey(5, 1);
    const dk2 = new Deadkey(3, 2);
    const dk3 = new Deadkey(10, 3);
    const tracker = new DeadkeyTracker();
    tracker.add(dk1);
    tracker.add(dk2);
    tracker.add(dk3);
    dk2.set();
    dk3.set();

    tracker.resetMatched();

    assert.equal(tracker.count(), 3);
    assert.equal(tracker.dks[0].matched, 0);
    assert.equal(tracker.dks[1].matched, 0);
    assert.equal(tracker.dks[2].matched, 0);
  });

  describe('isMatch', function () {
    it('returns false without having any deadkeys', function () {
      const tracker = new DeadkeyTracker();

      assert.isFalse(tracker.isMatch(0, 1, 2));
    });

    it('returns false for non-matching deadkey', function () {
      const tracker = new DeadkeyTracker();
      tracker.add(new Deadkey(3, 1));
      tracker.add(new Deadkey(5, 2));
      tracker.add(new Deadkey(10, 3));

      assert.isFalse(tracker.isMatch(0, 88, 99));

      assert.equal(tracker.dks[0].matched, 0);
      assert.equal(tracker.dks[1].matched, 0);
      assert.equal(tracker.dks[2].matched, 0);
    });

    it('returns false if offset is wrong', function () {
      const tracker = new DeadkeyTracker();
      tracker.add(new Deadkey(3, 1));
      tracker.add(new Deadkey(5, 2));
      tracker.add(new Deadkey(10, 3));

      assert.isFalse(tracker.isMatch(0, 3, 1));

      assert.equal(tracker.dks[0].matched, 0);
      assert.equal(tracker.dks[1].matched, 0);
      assert.equal(tracker.dks[2].matched, 0);
    });

    it('returns true if offset is correct and we have multiple deadkeys with same id', function () {
      const tracker = new DeadkeyTracker();
      tracker.add(new Deadkey(3, 1));
      tracker.add(new Deadkey(5, 1));
      tracker.add(new Deadkey(10, 3));

      assert.isTrue(tracker.isMatch(7, 2, 1));

      assert.equal(tracker.dks[0].matched, 0);
      assert.equal(tracker.dks[1].matched, 1);
      assert.equal(tracker.dks[2].matched, 0);
    });

    it('matches entry with lowest ordinal value', function () {
      const tracker = new DeadkeyTracker();
      tracker.add(new Deadkey(3, 1));
      tracker.add(new Deadkey(3, 1));
      tracker.add(new Deadkey(10, 3));

      assert.isTrue(tracker.isMatch(0, -3, 1));

      assert.equal(tracker.dks[0].matched, 1);
      assert.equal(tracker.dks[1].matched, 0);
      assert.equal(tracker.dks[2].matched, 0);
    });

    it('matches entry with lowest ordinal value if caret is after deadkey', function () {
      const tracker = new DeadkeyTracker();
      tracker.add(new Deadkey(3, 1));
      tracker.add(new Deadkey(3, 1));
      tracker.add(new Deadkey(10, 3));

      assert.isTrue(tracker.isMatch(5, 2, 1));

      assert.equal(tracker.dks[0].matched, 1);
      assert.equal(tracker.dks[1].matched, 0);
      assert.equal(tracker.dks[2].matched, 0);
    });

    it('returns true for matching deadkey and sets matched flag on deadkey', function () {
      const tracker = new DeadkeyTracker();
      tracker.add(new Deadkey(3, 1));
      tracker.add(new Deadkey(5, 2));
      tracker.add(new Deadkey(10, 3));

      assert.isTrue(tracker.isMatch(0, -3, 1));

      assert.equal(tracker.dks[0].matched, 1);
      assert.equal(tracker.dks[1].matched, 0);
      assert.equal(tracker.dks[2].matched, 0);
    });

    it('returns true for matching deadkey if caret on dk', function () {
      const tracker = new DeadkeyTracker();
      tracker.add(new Deadkey(3, 1));
      tracker.add(new Deadkey(5, 2));
      tracker.add(new Deadkey(10, 3));

      assert.isTrue(tracker.isMatch(3, 0, 1));

      assert.equal(tracker.dks[0].matched, 1);
      assert.equal(tracker.dks[1].matched, 0);
      assert.equal(tracker.dks[2].matched, 0);
    });

    it('returns false for matching deadkey if already matched and resets matched flag', function () {
      const tracker = new DeadkeyTracker();
      tracker.add(new Deadkey(3, 1));
      tracker.add(new Deadkey(5, 2));
      tracker.add(new Deadkey(10, 3));
      tracker.dks[0].matched = 1;
      tracker.dks[1].matched = 1;
      tracker.dks[2].matched = 1;

      assert.isFalse(tracker.isMatch(0, -3, 1));

      assert.equal(tracker.dks[0].matched, 0);
      assert.equal(tracker.dks[1].matched, 0);
      assert.equal(tracker.dks[2].matched, 0);
    });
  });
});
