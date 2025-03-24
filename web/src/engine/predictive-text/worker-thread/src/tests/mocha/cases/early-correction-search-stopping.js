import { assert } from 'chai';

import { CORRECTION_SEARCH_THRESHOLDS, shouldStopSearchingEarly } from "#./predict-helpers.js";
import { ModelCompositor } from '#./model-compositor.js';

describe('correction-search: shouldStopSearchingEarly', () => {
  it('stops early once new corrections are less likely than currently discovered predictions', () => {
    const predictionProbs = [.10, .09, .08, .08, .075, .075, .07, .07, .06, .06, .05, .05];

    // If we change the value of MAX_SUGGESTIONS, then the function will be looking at a different
    // "last" suggestion, which may have a different probability value than expected, which
    // would invalidate the values used for the second parameters in the following assertions.
    assert.equal(predictionProbs.length, ModelCompositor.MAX_SUGGESTIONS, "test setup no longer valid");

    // The only part for each entry we actually care about here:  .totalProb.
    /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]} */
    const predictions = predictionProbs.map((entry) => {
      return {
        totalProb: entry
      }
    });

    // Thresholding is performed in log-space.
    // 0.0501 and 0.0499 are offset on each side of 0.05, the last value in the array defined above.
    assert.isFalse(shouldStopSearchingEarly(-Math.log(.4), -Math.log(.0501), predictions));
    assert.isTrue(shouldStopSearchingEarly( -Math.log(.4), -Math.log(.0499), predictions));
  });

  it('stops early when all reasonably-likely corrections have been exhausted', () => {
    const baseCost = 1;

    // Thresholding is performed in log-space.
    const expectedThreshold = CORRECTION_SEARCH_THRESHOLDS.MAX_SEARCH_THRESHOLD;
    //
    // Can technically run the method with an empty array, but the actual scenario would have
    // at least one prediction present in the "found predictions" array.
    assert.isFalse(shouldStopSearchingEarly(baseCost, baseCost + expectedThreshold - 0.01, [{ totalProb: Math.exp(-1) }]));
    assert.isTrue(shouldStopSearchingEarly( baseCost, baseCost + expectedThreshold + 0.01, [{ totalProb: Math.exp(-1) }]));
  });

  it('stops checking corrections earlier when enough predictions have been found', () => {
    const predictionProbs = [.010, .009, .008, .008, .0075, .0075, .007, .007, .006, .006, .005, .005];
    assert.isAtLeast(predictionProbs.length, ModelCompositor.MAX_SUGGESTIONS, "test setup no longer valid");

    // The only part for each entry we actually care about here:  .totalProb.
    /** @type {import('#./predict-helpers.js').CorrectionPredictionTuple[]} */
    const predictions = predictionProbs.map((entry) => {
      return {
        totalProb: entry
      }
    });

    const baseCost = 1;

    // Thresholding is performed in log-space.
    const expectedThreshold = CORRECTION_SEARCH_THRESHOLDS.REPLACEMENT_SEARCH_THRESHOLD;

    assert.isFalse(shouldStopSearchingEarly(baseCost, baseCost + expectedThreshold - 0.01, predictions));
    assert.isTrue(shouldStopSearchingEarly( baseCost, baseCost + expectedThreshold + 0.01, predictions));
  });
});