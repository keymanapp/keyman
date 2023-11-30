import { assert } from 'chai';
import { CumulativePathStats, RegressiblePathStats, InputSample } from '@keymanapp/gesture-recognizer';

import { TouchpathTurtle } from '#tools';

describe("CumulativePathStats", function() {
  it("Sample count = 0", function() {
    let stats = new CumulativePathStats();

    assert.equal(stats.duration, 0);
    assert.equal(stats.angle, undefined);
    assert.equal(stats.cardinalDirection, undefined);
    assert.equal(stats.rawDistance, 0);
    assert.equal(stats.netDistance, 0);
    assert.isNaN(stats.mean('x'));
    assert.isNaN(stats.mean('y'));
  });

  it("Sample count = 1", function() {
    let stats = new CumulativePathStats();
    stats = stats.extend({
      targetX: 4,
      targetY: 8,
      t: 40
    });

    assert.equal(stats.duration, 0);
    assert.equal(stats.angle, undefined);
    assert.equal(stats.cardinalDirection, undefined);
    assert.equal(stats.mean('x'), 4);
    assert.equal(stats.mean('y'), 8);
  });

  it("Basic accumulation (perfect correlation)", function() {
    const samples: InputSample<any>[] = [];

    // Exactly 5 points, evenly spaced and linear.
    // So, the arithmetic mean should be very obvious - it's the middle sample.
    for(let i = 0; i < 5; i++) {
      samples.push({
        targetX: 4 * i + 10,
        targetY: 4 * i + 20,
        t: 100 * i + 10000
      });
    }

    let stats = new CumulativePathStats();

    for(const sample of samples) {
      stats = stats.extend(sample);
    }

    assert.equal(stats.mean('x'), samples[2].targetX);
    assert.equal(stats.mean('y'), samples[2].targetY);
    assert.equal(stats.mean('t'), samples[2].t);

    assert.equal(stats.angle, 135 * Math.PI / 180);
    assert.equal(stats.rawDistance, 16 * Math.SQRT2); // 4 intervals of length 4 * sqrt(2)
    assert.equal(stats.cardinalDirection, 'se');
  });

  it("Immutability (aside from .followingSample)", function() {
    const samples: InputSample<any>[] = [];

    // Exactly 5 points, evenly spaced and linear.
    // So, the arithmetic mean should be very obvious - it's the middle sample.
    for(let i = 0; i < 5; i++) {
      samples.push({
        targetX: 4 * i + 10,
        targetY: 4 * i + 20,
        t: 100 * i + 10000
      });
    }

    let stats = new CumulativePathStats();
    let preStats: CumulativePathStats[]  = [];
    let postStats: CumulativePathStats[] = [];

    for(const sample of samples) {
      const initialValue = stats;
      // The constructor provides a deep-copy mechanism.
      preStats.push(new CumulativePathStats(initialValue));
      stats = stats.extend(sample);
      postStats.push(new CumulativePathStats(initialValue));
    }

    // The one not-immutable part:  `.followingSample`.  It's needed for some of the
    // internal mechanisms - for `.deaccumulate`, in particular.
    for(let obj of postStats) {
      // is technically private; we delete it b/c it'd get in the way of the assertion below.
      delete obj['followingSample'];
    }

    // The very first sample has a few more changes because of recording the first (and thus, base) sample.
    // So we ignore it.
    preStats.splice(0, 1);
    postStats.splice(0, 1);

    assert.sameDeepOrderedMembers(postStats, preStats);
  });

  it("Deaccumulation", function() {
    const sampleSet1: InputSample<any>[] = [];

    // Exactly 5 points, evenly spaced and linear.
    // So, the arithmetic mean should be very obvious - it's the middle sample.
    for(let i = 0; i < 5; i++) {
      sampleSet1.push({
        targetX: 4 * i + 10,  // Final:  26
        targetY: 4 * i + 20,  // Final:  36
        t: 100 * i + 10000    // Final:  10400
      });
    }

    let firstHalfStats = new CumulativePathStats();

    for(const sample of sampleSet1) {
      firstHalfStats = firstHalfStats.extend(sample);
    }

    const splitPoint = {
      targetX: 30,
      targetY: 40,
      t: 10500
    };

    firstHalfStats = firstHalfStats.extend(splitPoint);

    const sampleSet2: InputSample<any>[] = [];

    // Exactly 5 points, evenly spaced and linear.
    // So, the arithmetic mean should be very obvious - it's the middle sample.
    for(let i = 0; i < 5; i++) {
      sampleSet2.push({
        targetX:  4 * i + 34, // unchanged direction
        targetY: -4 * i + 36, // flipped to the opposite direction
        t: 100 * i + 10600
      });
    }

    let secondHalfStats = new CumulativePathStats();
    let combinedStats   = firstHalfStats;

    for(const sample of sampleSet2) {
      secondHalfStats = secondHalfStats.extend(sample);
      combinedStats   = combinedStats.extend(sample);
    }

    // Reconstructs the second half by 'deaccumulating' the first half from the full accumulation.
    // (Not including the split-point.)
    // This is pretty core to our segmentation algorithm's efficiency.
    let deaccumulatedSecondHalfStats = combinedStats.deaccumulate(firstHalfStats);

    // Base sample will differ because `secondHalfStats` was started independently.
    // This means that the linear, cross, and squaredSums WILL NOT BE EQUAL.
    // But, the statistical properties?  THOSE should match.

    assert.equal(deaccumulatedSecondHalfStats.mean('x'), secondHalfStats.mean('x'));
    assert.equal(deaccumulatedSecondHalfStats.mean('y'), secondHalfStats.mean('y'));
    assert.equal(deaccumulatedSecondHalfStats.mean('t'), secondHalfStats.mean('t'));

    // Floating-point "equality".
    assert.closeTo(deaccumulatedSecondHalfStats.netDistance, secondHalfStats.netDistance, 1e-8);
    assert.closeTo(deaccumulatedSecondHalfStats.rawDistance, secondHalfStats.rawDistance, 1e-8);

    assert.equal(deaccumulatedSecondHalfStats.duration, secondHalfStats.duration);
    assert.equal(deaccumulatedSecondHalfStats.angle, secondHalfStats.angle);

    assert.equal(deaccumulatedSecondHalfStats.initialSample, secondHalfStats.initialSample);
    assert.equal(deaccumulatedSecondHalfStats.lastSample, secondHalfStats.lastSample);
  });
});

describe("RegressiblePathStats", function() {
  it("Sample count = 0", function() {
    let stats = new RegressiblePathStats();

    assert.equal(stats.duration, 0);
    assert.equal(stats.angle, undefined);
    assert.isNaN(stats.angleDeviation);
    assert.equal(stats.cardinalDirection, undefined);
    assert.equal(stats.rawDistance, 0);
    assert.equal(stats.netDistance, 0);
    assert.isNaN(stats.mean('x'));
    assert.isNaN(stats.mean('y'));
  });

  it("Sample count = 1", function() {
    let stats = new RegressiblePathStats();
    stats = stats.extend({
      targetX: 4,
      targetY: 8,
      t: 40
    });

    assert.equal(stats.duration, 0);
    assert.equal(stats.angle, undefined);
    assert.isNaN(stats.angleDeviation);
    assert.equal(stats.cardinalDirection, undefined);
    assert.isNaN(stats.variance('x'));
    assert.isNaN(stats.variance('y'));
    assert.equal(stats.mean('x'), 4);
    assert.equal(stats.mean('y'), 8);
  });

  it("Basic accumulation (perfect correlation)", function() {
    const samples: InputSample<any>[] = [];

    // Exactly 5 points, evenly spaced and linear.
    // So, the arithmetic mean should be very obvious - it's the middle sample.
    for(let i = 0; i < 5; i++) {
      samples.push({
        targetX: 4 * i + 10,
        targetY: 4 * i + 20,
        t: 100 * i + 10000
      });
    }

    let stats = new RegressiblePathStats();

    for(const sample of samples) {
      stats = stats.extend(sample);
    }

    assert.equal(stats.mean('x'), samples[2].targetX);
    assert.equal(stats.mean('y'), samples[2].targetY);
    assert.equal(stats.mean('t'), samples[2].t);


    // The two are perfectly correlated and have the same variance.
    // Since we renormalize samples, the base 'intercept' of each variable's linear form should have no effect..
    assert.equal(stats.squaredSum('x'), stats.crossSum('xy'));
    assert.equal(stats.squaredSum('y'), stats.crossSum('xy'));
    // As x and y are perfectly correlated with the same variance, their cross-sum with t should match.
    assert.equal(stats.crossSum('tx'), stats.crossSum('ty'));

    assert.equal(stats.angle, 135 * Math.PI / 180);
    assert.equal(stats.rawDistance, 16 * Math.SQRT2); // 4 intervals of length 4 * sqrt(2)
    assert.equal(stats.cardinalDirection, 'se');
  });

  it("Immutability (aside from .followingSample)", function() {
    const samples: InputSample<any>[] = [];

    // Exactly 5 points, evenly spaced and linear.
    // So, the arithmetic mean should be very obvious - it's the middle sample.
    for(let i = 0; i < 5; i++) {
      samples.push({
        targetX: 4 * i + 10,
        targetY: 4 * i + 20,
        t: 100 * i + 10000
      });
    }

    let stats = new RegressiblePathStats();
    let preStats: RegressiblePathStats[]  = [];
    let postStats: RegressiblePathStats[] = [];

    for(const sample of samples) {
      const initialValue = stats;
      // The constructor provides a deep-copy mechanism.
      preStats.push(new RegressiblePathStats(initialValue));
      stats = stats.extend(sample);
      postStats.push(new RegressiblePathStats(initialValue));
    }

    // The one not-immutable part:  `.followingSample`.  It's needed for some of the
    // internal mechanisms - for `.deaccumulate`, in particular.
    for(let obj of postStats) {
      // is technically private; we delete it b/c it'd get in the way of the assertion below.
      delete obj['followingSample'];
    }

    // The very first sample has a few more changes because of recording the first (and thus, base) sample.
    // So we ignore it.
    preStats.splice(0, 1);
    postStats.splice(0, 1);

    assert.sameDeepOrderedMembers(postStats, preStats);
  });

  it("Basic regressions (perfect correlation)", function() {
    const samples: InputSample<any>[] = [];

    // Exactly 5 points, evenly spaced and linear.
    // So, the arithmetic mean should be very obvious - it's the middle sample.
    for(let i = 0; i < 5; i++) {
      samples.push({
        targetX: 4 * i + 10,
        targetY: 4 * i + 20,
        t: 100 * i // since it makes head-math simpler for the regressions we'll be doing.
      });
    }

    let stats = new RegressiblePathStats();

    for(const sample of samples) {
      stats = stats.extend(sample);
    }

    const xtRegression = stats.fitRegression('x', 't');
    assert.equal(xtRegression.sumOfSquaredError, 0);
    assert.equal(xtRegression.slope, 4 / 100);
    assert.equal(xtRegression.intercept, 10);
    assert.equal(xtRegression.predictFromValue(1000), 40 + 10);

    const ytRegression = stats.fitRegression('y', 't');
    assert.equal(ytRegression.sumOfSquaredError, 0);
    assert.equal(ytRegression.slope, 4 / 100);
    assert.equal(ytRegression.intercept, 20);
    assert.equal(ytRegression.predictFromValue(1000), 40 + 20);

    const xyRegression = stats.fitRegression('y', 'x');
    assert.equal(xyRegression.sumOfSquaredError, 0);
    assert.equal(xyRegression.slope, 1);
    assert.equal(xyRegression.intercept, 10);
    assert.equal(xyRegression.predictFromValue(50), 60);
  });

  it("Deaccumulation", function() {
    const sampleSet1: InputSample<any>[] = [];

    // Exactly 5 points, evenly spaced and linear.
    // So, the arithmetic mean should be very obvious - it's the middle sample.
    for(let i = 0; i < 5; i++) {
      sampleSet1.push({
        targetX: 4 * i + 10,  // Final:  26
        targetY: 4 * i + 20,  // Final:  36
        t: 100 * i + 10000    // Final:  10400
      });
    }

    let firstHalfStats = new RegressiblePathStats();

    for(const sample of sampleSet1) {
      firstHalfStats = firstHalfStats.extend(sample);
    }

    const splitPoint = {
      targetX: 30,
      targetY: 40,
      t: 10500
    };

    firstHalfStats = firstHalfStats.extend(splitPoint);

    const sampleSet2: InputSample<any>[] = [];

    // Exactly 5 points, evenly spaced and linear.
    // So, the arithmetic mean should be very obvious - it's the middle sample.
    for(let i = 0; i < 5; i++) {
      sampleSet2.push({
        targetX:  4 * i + 34, // unchanged direction
        targetY: -4 * i + 36, // flipped to the opposite direction
        t: 100 * i + 10600
      });
    }

    let secondHalfStats = new RegressiblePathStats();
    let combinedStats   = firstHalfStats;

    for(const sample of sampleSet2) {
      secondHalfStats = secondHalfStats.extend(sample);
      combinedStats   = combinedStats.extend(sample);
    }

    // Reconstructs the second half by 'deaccumulating' the first half from the full accumulation.
    // (Not including the split-point.)
    // This is pretty core to our segmentation algorithm's efficiency.
    let deaccumulatedSecondHalfStats = combinedStats.deaccumulate(firstHalfStats);

    // Base sample will differ because `secondHalfStats` was started independently.
    // This means that the linear, cross, and squaredSums WILL NOT BE EQUAL.
    // But, the statistical properties?  THOSE should match.

    assert.equal(deaccumulatedSecondHalfStats.mean('x'), secondHalfStats.mean('x'));
    assert.equal(deaccumulatedSecondHalfStats.mean('y'), secondHalfStats.mean('y'));
    assert.equal(deaccumulatedSecondHalfStats.mean('t'), secondHalfStats.mean('t'));

    assert.equal(deaccumulatedSecondHalfStats.variance('x'), secondHalfStats.variance('x'));
    assert.equal(deaccumulatedSecondHalfStats.variance('y'), secondHalfStats.variance('y'));
    assert.equal(deaccumulatedSecondHalfStats.variance('t'), secondHalfStats.variance('t'));

    assert.equal(deaccumulatedSecondHalfStats.covariance('tx'), secondHalfStats.covariance('tx'));
    assert.equal(deaccumulatedSecondHalfStats.covariance('ty'), secondHalfStats.covariance('ty'));
    assert.equal(deaccumulatedSecondHalfStats.covariance('xy'), secondHalfStats.covariance('xy'));

    // Floating-point "equality".
    assert.closeTo(deaccumulatedSecondHalfStats.netDistance, secondHalfStats.netDistance, 1e-8);
    assert.closeTo(deaccumulatedSecondHalfStats.rawDistance, secondHalfStats.rawDistance, 1e-8);

    assert.equal(deaccumulatedSecondHalfStats.duration, secondHalfStats.duration);
    assert.equal(deaccumulatedSecondHalfStats.angle, secondHalfStats.angle);

    assert.equal(deaccumulatedSecondHalfStats.initialSample, secondHalfStats.initialSample);
    assert.equal(deaccumulatedSecondHalfStats.lastSample, secondHalfStats.lastSample);

    // Angle deviation stuff doesn't currently have the same level of catastrophic cancellation protection
    // as the main statistical properties.  But... even still, we can add a reasonable test.
    assert.isBelow(Math.abs(deaccumulatedSecondHalfStats.angleDeviation - secondHalfStats.angleDeviation), 1e-7);
  });

  it("Renormalization", () => {
    let stats = new RegressiblePathStats();

    const turtle = new TouchpathTurtle({
      targetX: 50,
      targetY: -25,
      t: 500,
      item: 'a'
    });

    turtle.on('sample', (sample) => {
      stats = stats.extend(sample);
    });

    function assertMatchingStats(actual: RegressiblePathStats, expected: RegressiblePathStats) {
      assert.equal(actual.duration, expected.duration);
      assert.equal(actual.sampleCount, expected.sampleCount);

      assert.closeTo(actual.angle, expected.angle, 1e-8);
      assert.closeTo(actual.mean('x'), expected.mean('x'), 1e-8);
      assert.closeTo(actual.mean('y'), expected.mean('y'), 1e-8);
      assert.closeTo(actual.mean('t'), expected.mean('t'), 1e-8);

      assert.closeTo(actual.variance('x'), expected.variance('x'), 1e-8);
      assert.closeTo(actual.variance('y'), expected.variance('y'), 1e-8);
      assert.closeTo(actual.variance('t'), expected.variance('t'), 1e-8);

      assert.closeTo(actual.rawDistance, expected.rawDistance, 1e-8);
      assert.closeTo(actual.netDistance, expected.netDistance, 1e-8);

      const actualRegression = actual.fitRegression('x', 'y');
      const expectedRegression = expected.fitRegression('x', 'y');
      assert.closeTo(
        actualRegression.coefficientOfDetermination,
        expectedRegression.coefficientOfDetermination,
        1e-8
      );
      assert.closeTo(actualRegression.slope, expectedRegression.slope, 1e-8);
    }

    turtle.move(90, 4, 20, 2);
    turtle.move(60, 5, 20, 2); // net move:  <4, -3>.
    turtle.commitPending();

    let renormalizedStats = stats.buildRenormalized();
    assertMatchingStats(renormalizedStats, stats);

    // Continue to accumulate stats for both.
    turtle.on('sample', (sample) => {
      renormalizedStats = renormalizedStats.extend(sample);
    })

    turtle.move(0, 10, 40, 4);
    turtle.move(45, 3 * Math.sqrt(2), 40, 4);
    turtle.commitPending();

    // After the renormalization, there should be no externally-visible distinction between the two stats objects.
    assertMatchingStats(renormalizedStats, stats);
  });
});