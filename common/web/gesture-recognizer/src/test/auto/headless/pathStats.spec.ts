import { assert } from 'chai';
import { CumulativePathStats, InputSample } from '@keymanapp/gesture-recognizer';

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