import { assert } from 'chai';

import { PathSegmenter, Segment } from '@keymanapp/gesture-recognizer';

/**
 * Defines common assertions for loosely comparing reproduced segments to their original counterparts.
 *
 * @param {*} repro The Segment object produced by the test.
 * @param {*} rawOriginal The raw version of the original Segment from JSON.parse()
 * @param {*} type The type of Segment to expect.
 */
export const assertSegmentSimilarity = (repro, rawOriginal, type) => {
  // Complete our deserialization of the original segment in question - needed for
  // clearer, more direct comparisons.
  const original = new Segment(rawOriginal);

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