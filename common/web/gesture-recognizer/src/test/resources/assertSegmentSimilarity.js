const assert = require('chai').assert;

const GestureRecognizer = require('../../../build/index.js');
const com = GestureRecognizer.com;
const PathSegmenter = com.keyman.osk.PathSegmenter;

/**
 * Defines common assertions for loosely comparing reproduced segments to their original counterparts.
 *
 * @param {*} repro The Segment object produced by the test.
 * @param {*} rawOriginal The raw version of the original Segment from JSON.parse()
 * @param {*} type The type of Segment to expect.
 */
const assertSegmentSimilarity = (repro, rawOriginal, type) => {
  // Complete our deserialization of the original segment in question - needed for
  // clearer, more direct comparisons.
  const original = new com.keyman.osk.Segment(rawOriginal);

  assert.equal(original.type, type, "Test setup issue - unexpected type for original segment");
  assert.equal(repro.type, type, "Reproduced segment of unexpected type");
  assert.isAtLeast(repro.duration, original.duration - PathSegmenter.DEFAULT_CONFIG.holdMinimumDuration);
  assert.isAtMost(repro.duration, original.duration + PathSegmenter.DEFAULT_CONFIG.holdMinimumDuration);

  if(type == 'move') {
    // Move-specific checks
    assert.equal(repro.direction, original.direction);
    assert.isAtLeast(repro.distance, original.distance - 2 * PathSegmenter.DEFAULT_CONFIG.holdMoveTolerance);
    assert.isAtMost(repro.distance, original.distance + 2 * PathSegmenter.DEFAULT_CONFIG.holdMoveTolerance);
    assert.isAtLeast(repro.peakSpeed, 0.9 * original.peakSpeed);
  }
}

(function () {
  // Lets the function be available both in the browser and in Node.
  if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
    module.exports = assertSegmentSimilarity;
  }
}());