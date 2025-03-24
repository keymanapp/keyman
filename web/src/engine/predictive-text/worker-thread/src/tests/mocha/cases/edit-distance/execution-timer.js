import { assert } from 'chai';
import { useFakeTimers } from 'sinon';

import { ExecutionBucket, ExecutionTimer } from '#./correction/index.js';

describe('ExecutionTimer', () => {
  /** @type {import('sinon').SinonFakeTimers} */
  let timeControl;

  beforeEach(() => {
    timeControl = useFakeTimers();
  });

  afterEach(() => {
    timeControl.restore();
  });

  it('has expected state on construction', () => {
    const timer = new ExecutionTimer(40, 100);
    assert.equal(timer.executionTime, 0);
    assert.equal(timer.deferredTime, 0);
    assert.isFalse(timer.elapsed);
  });

  it('time()', () => {
    const timer = new ExecutionTimer(40, 100);
    // Mocked timers give us 100% full control.
    timer.time(() => timeControl.tick(12.34), 1);
    assert.equal(timer.executionTime, 12.34);

    timer.time(() => timeControl.tick(40), 2);
    assert.equal(timer.executionTime, 52.34);

    assert.equal(timer.deferredTime, 0);
  });

  it('start()', () => {
    const timer = new ExecutionTimer(40, 100);
    const timeSpan1 = timer.start(1);
    timeControl.tick(12.34);

    // The ExecutionTimer class does not include uncompleted timings.
    // Checking total execution-time during an active timing isn't
    // something we aim to support for the class.
    assert.equal(timer.executionTime, 0);

    // That said, the `.duration` property can be useful for tracking
    // time since the last `.defer()` call, even when not completed...
    // so it should be updated without calling `.end()`.
    assert.equal(timeSpan1.duration, 12.34);
    timeSpan1.end();
    assert.equal(timer.executionTime, 12.34);

    const timeSpan2 = timer.start(2);
    timeControl.tick(40);
    assert.equal(timer.executionTime, 12.34);  // does not include an uncompleted timing.
    assert.equal(timeSpan2.duration, 40);
    timeSpan2.end();
    assert.equal(timer.executionTime, 52.34);

    // Previously-elapsed 'spans', if kept around, should have their duration locked.
    assert.equal(timeSpan1.duration, 12.34);

    // No simulated defers were triggered; time should equal 0.
    assert.equal(timer.deferredTime, 0);
  });

  it('throws when start() not end()-ed', async () => {
    const timer = new ExecutionTimer(40, 100);
    timer.start(1);

    assert.throws(() => timer.start());
    assert.throws(() => timer.time(() => {}));
    try {
      await timer.defer();
      assert.fail('timer.defer() did not throw');
    } catch (err) {}
  });

  it('defer()', async () => {
    const timer = new ExecutionTimer(50, 100);

    const promise = timer.defer(20);
    const runAll = timeControl.runAllAsync();
    await Promise.all([promise, runAll]);

    assert.equal(timer.deferredTime, 20);

    assert.equal(timer.executionTime, 0);
  });

  it('defer() - alternate setup', async () => {
    const timer = new ExecutionTimer(50, 100);

    const delaySetup = new Promise(async (resolve) => {
      // The passed-in function does not delay by default; this will wait for
      // one microtask delay before proceeding.
      //
      // (This allows defer's start to begin before this function takes
      // control.)
      await Promise.resolve();
      timeControl.tick(20);

      resolve();
    });

    const promise = timer.defer();
    const runAll = timeControl.runAllAsync();
    await Promise.all([delaySetup, promise, runAll]);

    assert.equal(timer.deferredTime, 20);

    assert.equal(timer.executionTime, 0);
  });

  it('elapsed - from active time', () => {
    const timer = new ExecutionTimer(50, 100);
    timer.time(() => timeControl.tick(12.34));
    assert.isFalse(timer.elapsed);
    timer.time(() => timeControl.tick(40));
    assert.isTrue(timer.elapsed);
  });

  it('elapsed - from total time waited', async () => {
    const timer = new ExecutionTimer(50, 100);

    const promise1 = timer.defer(60);
    timeControl.runAllAsync();
    await promise1;

    assert.isFalse(timer.elapsed);

    const promise2 = timer.defer(40);
    timeControl.runAllAsync();
    await promise2;

    assert.isTrue(timer.elapsed);
  });

  it('terminate() - instantly called', async () => {
    const timer = new ExecutionTimer(50, 100);
    assert.isFalse(timer.elapsed);

    timer.terminate();
    assert.isTrue(timer.elapsed);

    assert.doesNotThrow(() => timer.terminate());
  });

  it('terminate() - called after some operations', async () => {
    const timer = new ExecutionTimer(50, 100);
    assert.isFalse(timer.elapsed);

    timer.time(() => timeControl.tick(10));

    const defer = timer.defer(10);
    const allAsync = timeControl.runAllAsync();
    await Promise.all([defer, allAsync]);

    assert.isFalse(timer.elapsed);
    assert.equal(timer.executionTime, 10);
    assert.equal(timer.deferredTime, 10);

    timer.terminate();
    assert.isTrue(timer.elapsed);
    assert.equal(timer.executionTime, 10);
    assert.equal(timer.deferredTime, 10);

    assert.doesNotThrow(() => timer.terminate());
  });

  it('timeSinceLastDefer', async () => {
    const timer = new ExecutionTimer(50, 100);
    assert.equal(timer.timeSinceLastDefer, 0);

    timeControl.tick(2);
    assert.equal(timer.timeSinceLastDefer, 2);

    timer.time(() => timeControl.tick(8));
    assert.equal(timer.timeSinceLastDefer, 10);

    const defer = timer.defer(5);
    const allAsync = timeControl.runAllAsync();
    await Promise.all([defer, allAsync]);

    assert.equal(timer.timeSinceLastDefer, 0);
    timeControl.tick(3);
    assert.equal(timer.timeSinceLastDefer, 3);
  });
});

describe('ExecutionBucket', () => {
  // No need for time-control here; this class does not internally reference
  // performance.now or similar constructs.
  describe('without outlier logic', () => {
    it('has expected values after construction', () => {
      const bucket = new ExecutionBucket(true);

      assert.equal(bucket.timeSpent, 0);
      assert.equal(bucket.outlierTime, 0);
      assert.equal(bucket.eventCount, 0);
      assert.isNaN(bucket.average);  // requires at least one sample
      assert.isNaN(bucket.variance); // requires at least two samples
    });

    it('throws when expected', () => {
      const bucket = new ExecutionBucket(true);

      assert.throws(() => bucket.add(-1));
      assert.throws(() => bucket.add(-0.0001));
      assert.doesNotThrow(() => bucket.add(0));
      assert.doesNotThrow(() => bucket.add(2));
    });

    it('has expected values after adding one observation', () => {
      const bucket = new ExecutionBucket(true);
      bucket.add(3);

      assert.equal(bucket.timeSpent, 3);
      assert.equal(bucket.outlierTime, 0);
      assert.equal(bucket.eventCount, 1);
      assert.equal(bucket.average, 3);
      assert.isNaN(bucket.variance);
    });

    it('has expected values after adding two observations', () => {
      const bucket = new ExecutionBucket(true);
      bucket.add(3);
      bucket.add(5);

      assert.equal(bucket.timeSpent, 8);
      assert.equal(bucket.outlierTime, 0);
      assert.equal(bucket.eventCount, 2);
      assert.equal(bucket.average, 4);
      assert.equal(bucket.variance, 1); // both are evenly spaced on either side of the average.
    });

    it('has expected values after adding numerous observations', () => {
      const bucket = new ExecutionBucket(true);

      for(let i=1; i <= 9; i++) {
        bucket.add(i);
      }

      assert.equal(bucket.timeSpent, 45); // 1+9, 2+8... but 5 is unpaired.
      assert.equal(bucket.outlierTime, 0);
      assert.equal(bucket.eventCount, 9);
      assert.equal(bucket.average, 5);

      // 20/3 = 6.666..., which means a std. deviation of about 2.58.
      // We're using an approximately uniform distribution, not a bell-curve,
      // so it makes reasonable sense.
      assert.approximately(bucket.variance, 20/3, 0.001);
    });
  });

  describe('with outlier logic', () => {
    it('does not find outlier among 4 nearby samples', () => {
      const bucket = new ExecutionBucket();

      const samples = [0, 1, 2, 1];
      samples.forEach((entry) => bucket.add(entry));

      assert.equal(bucket.timeSpent, 4);
      assert.equal(bucket.outlierTime, 0);
      assert.equal(bucket.eventCount, 4);
      assert.equal(bucket.average, 1);

      assert.approximately(bucket.variance, 0.5, 0.001);
    });

    it('does not find outlier among 3 nearby samples + 1 somewhat far sample', () => {
      const bucket = new ExecutionBucket();

      // 4 looks like it could be an outlier, but we have too low a sample count
      // to definitely exclude it at this point, statistically-speaking.
      const samples = [0, 1, 3.9, 1];  // 4.0 actually IS far enough:  50 vs 49 in squared-variance.
      samples.forEach((entry) => bucket.add(entry));

      assert.equal(bucket.timeSpent, 5.9);
      assert.equal(bucket.outlierTime, 0);
      assert.equal(bucket.eventCount, 4);
      assert.equal(bucket.average, 1.475);

      assert.isAbove(bucket.variance, 2);
    });

    it('does find outlier among 3 nearby samples + 1 far sample', () => {
      const bucket = new ExecutionBucket();

      // 6 is definitely a much larger value when the other three observed entries thus far.
      const samples = [0, 1, 6, 0];
      samples.forEach((entry) => bucket.add(entry));

      assert.equal(bucket.timeSpent, 1);
      assert.equal(bucket.outlierTime, 6);
      assert.equal(bucket.eventCount, 3);
      assert.approximately(bucket.average, 1/3, 1e-6);

      assert.isBelow(bucket.variance, .5);
    });

    it('finds 1 outlier among 5 nearby samples + 2 slightly far samples', () => {
      const bucket = new ExecutionBucket();

      // The 2 is a notable jump above the others.  1 is kinda close, though.
      const samples = [.5, 1, 2, .5, .6, .8, .6];

      // Outlier logic rules out the 2 on the final 'add'.
      samples.forEach((entry) => bucket.add(entry));

      assert.approximately(bucket.timeSpent, 4, 1e-6);
      assert.equal(bucket.outlierTime, 2);
      assert.equal(bucket.eventCount, 6);
      assert.approximately(bucket.average, 4/6, 1e-6);

      // Only the 1 is over 0.2 away, and it's not that far outside.
      assert.isBelow(bucket.variance, 0.2);
    });

    it('finds 1 outlier among 7 nearby samples + 2 slightly far samples', () => {
      const bucket = new ExecutionBucket();

      // The 2 is a notable jump above the others.  1 is kind of too far from the others
      // too, but without the 2, we're one sample shy of excluding the 1.
      const samples = [.5, 1, 2, .5, .6, .8, .6, .7, .7];

      // Outlier logic rules out the 2 on the final 0.6 'add'.
      samples.forEach((entry) => bucket.add(entry));

      assert.approximately(bucket.timeSpent, 5.4, 1e-6);
      assert.equal(bucket.outlierTime, 2);
      assert.equal(bucket.eventCount, 8);
      assert.approximately(bucket.average, .675, 1e-6);

      assert.isBelow(bucket.variance, 0.1);
    });

    it('finds 2 outliers among 8 nearby samples + 2 slightly far samples', () => {
      const bucket = new ExecutionBucket();

      // The 2 is a notable jump above the others.  1 is too far from the others
      // as well; we only gain confidence in this when reaching the 8-sample threshold.
      //
      // Note: 1 is the minimum value we currently allow for anything to be considered
      // an 'outlier'.
      const samples = [.5, 1, 2, .5, .6, .8, .6, .7, .7, .6];

      // Outlier logic rules out the 2 on the final 'add'.
      samples.forEach((entry) => bucket.add(entry));

      assert.approximately(bucket.timeSpent, 5, 1e-6);
      assert.equal(bucket.outlierTime, 3);
      assert.equal(bucket.eventCount, 8);
      assert.approximately(bucket.average, .625, 1e-6);

      assert.isBelow(bucket.variance, 0.05);
    });

    it('finds no outliers among 10 large but nearby samples', () => {
      const bucket = new ExecutionBucket();

      // The 4 is a bit of a jump above the other values, and 2 a bit below.
      // The spread is wide enough that we can't consider them outliers in good
      // faith.  (Also, we don't consider low-end outliers given our domain
      // knowledge.)
      //
      // 4 is over 2 std-deviations out when excluded, but not the required 3.
      const samples = [2, 3, 4, 3.5, 2.5, 3.1, 3.3, 2.7, 2.9, 3.2];

      // Outlier logic rules out the 2 on the final 'add'.
      samples.forEach((entry) => bucket.add(entry));

      assert.approximately(bucket.timeSpent, 30.2, 1e-6);
      assert.equal(bucket.outlierTime, 0);
      assert.equal(bucket.eventCount, 10);
      assert.approximately(bucket.average, 3.02, 1e-6);

      // actual:  0.2736, putting std.dev ~= .523 (with `4` included).
      assert.isBelow(bucket.variance, 0.5);
    });
  });
});