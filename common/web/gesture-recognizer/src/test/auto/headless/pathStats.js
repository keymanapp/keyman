const assert = require('chai').assert;

const GestureRecognizer = require('../../../../build/index.js');
const com = GestureRecognizer.com;
const CumulativePathStats = com.keyman.osk.CumulativePathStats;

describe("CumulativePathStats", function() {
  it("Basic accumulation (perfect correlation)", function() {
    const samples = [];

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
    const samples = [];

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
    let preStats  = [];
    let postStats = [];

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
      delete obj.followingSample;
    }

    // The very first sample has a few more changes because of recording the first (and thus, base) sample.
    // So we ignore it.
    preStats.splice(0, 1);
    postStats.splice(0, 1);

    assert.sameDeepOrderedMembers(postStats, preStats);
  });

  it("Basic regressions (perfect correlation)", function() {
    const samples = [];

    // Exactly 5 points, evenly spaced and linear.
    // So, the arithmetic mean should be very obvious - it's the middle sample.
    for(let i = 0; i < 5; i++) {
      samples.push({
        targetX: 4 * i + 10,
        targetY: 4 * i + 20,
        t: 100 * i // since it makes head-math simpler for the regressions we'll be doing.
      });
    }

    let stats = new CumulativePathStats();

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
    const sampleSet1 = [];

    // Exactly 5 points, evenly spaced and linear.
    // So, the arithmetic mean should be very obvious - it's the middle sample.
    for(let i = 0; i < 5; i++) {
      sampleSet1.push({
        targetX: 4 * i + 10,  // Final:  26
        targetY: 4 * i + 20,  // Final:  36
        t: 100 * i + 10000    // Final:  10400
      });
    }

    let firstHalfStats  = new CumulativePathStats();

    for(const sample of sampleSet1) {
      firstHalfStats = firstHalfStats.extend(sample);
    }

    const splitPoint = {
      targetX: 30,
      targetY: 40,
      t: 10500
    };

    firstHalfStats = firstHalfStats.extend(splitPoint);

    const sampleSet2 = [];

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

    assert.equal(deaccumulatedSecondHalfStats.variance('x'), secondHalfStats.variance('x'));
    assert.equal(deaccumulatedSecondHalfStats.variance('y'), secondHalfStats.variance('y'));
    assert.equal(deaccumulatedSecondHalfStats.variance('t'), secondHalfStats.variance('t'));

    assert.equal(deaccumulatedSecondHalfStats.covariance('xt'), secondHalfStats.covariance('xt'));
    assert.equal(deaccumulatedSecondHalfStats.covariance('yt'), secondHalfStats.covariance('yt'));
    assert.equal(deaccumulatedSecondHalfStats.covariance('xy'), secondHalfStats.covariance('xy'));

    assert.equal(deaccumulatedSecondHalfStats.distance, secondHalfStats.distance);
    assert.equal(deaccumulatedSecondHalfStats.duration, secondHalfStats.duration);
    assert.equal(deaccumulatedSecondHalfStats.angle, secondHalfStats.angle);

    assert.equal(deaccumulatedSecondHalfStats.initialSample, secondHalfStats.initialSample);
    assert.equal(deaccumulatedSecondHalfStats.lastSample, secondHalfStats.lastSample);

    // Angle deviation stuff doesn't currently have the same level of catastrophic cancellation protection
    // as the main statistical properties.  But... even still, we can add a reasonable test.
    assert.isBelow(Math.abs(deaccumulatedSecondHalfStats.angleDeviation - secondHalfStats.angleDeviation), 1e-7);
  });
});