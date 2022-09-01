const assert = require('chai').assert;
const sinon = require('sinon');

const fs = require('fs');

const PromiseStatusModule = require('promise-status-async');
const promiseStatus       = PromiseStatusModule.promiseStatus;
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;

const GestureRecognizer = require('../../../../build/index.js');
const com = GestureRecognizer.com;
const PathSegmenter = com.keyman.osk.PathSegmenter;

const timedPromise = require('../../resources/timedPromise.js');

const SEGMENT_TEST_JSON_FOLDER = 'src/test/resources/json/segmentation';

describe("Segmentation", function() {
  describe("Recorded sequence tests", function() {
    beforeEach(function() {
      this.fakeClock = sinon.useFakeTimers();
    })

    afterEach(function() {
      // NOTE:  for debugging investigations, it may be necessary to use .only on
      // the test under investigation and to disable the `this.fakeClock.restore()` line.
      //
      // Tests tend to timeout when interactively debugging, and having unmocked timers
      // suddenly restored during investigation can cause some very confusing behavior.
      this.fakeClock.restore();
    })

    it("simple_ne_move.json", async function() {
      // NOTE:  this recording's final 'hold' segment is somewhat tightly attuned to the DEFAULT_CONFIG
      // hold time setting.  Changing default values there may necessitate a hand-edit tweak to the
      // test recording's data in order for this test to continue passing as-is.

      let testJSONtext = fs.readFileSync(`${SEGMENT_TEST_JSON_FOLDER}/simple_ne_move.json`);
      let jsonObj = JSON.parse(testJSONtext);

      // Prepare some of the basic setup.
      let spy = sinon.fake();

      const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

      // Next:  drill down to the relevant part(s).
      const sourceTrackedPath = jsonObj.inputs[0].touchpoints[0].path;

      const sourceSamples = sourceTrackedPath.coords;
      const originalSegments = sourceTrackedPath.segments;
      const lastSegment = originalSegments[originalSegments.length-1];

      // Build promises designed to reproduce the events at the correct times.
      const samplePromises = sourceSamples.map((sample) => {
        return timedPromise(() => {
          segmenter.add(sample);
        }, sample.t - sourceSamples[0].t);
      });

      // Extend any Promises here if relevant

      // Segmentation is then ended via a followup event.
      // The source recording does not save an input for the 'touchend' event - but
      // we can infer the correct time from the final segment.

      const segmentationEndPromise = timedPromise(() => {
        segmenter.close();
      }, lastSegment.lastCoord.t - sourceSamples[0].t);

      // Wrap it all together with a nice little bow.
      const finalPromise = Promise.all([...samplePromises, segmentationEndPromise]).catch((reason) => {
        // Because we use a `setInterval` internally, we need cleanup if things go wrong.
        segmenter.close();
        throw reason;
      });

      await this.fakeClock.runAllAsync();

      // Make sure this Promise also gets to complete.
      await finalPromise;

      // Any post-sequence tests to run.
      const originalSegmentTypeSequence = originalSegments.map((segment) => segment.type);

      const reproedSegments = spy.getCalls().map((call) => call.args[0]);
      const reproedSegmentTypeSequence  = reproedSegments.map((segment) => segment.type);

      assert.sameOrderedMembers(reproedSegmentTypeSequence, originalSegmentTypeSequence);

      for(let segment of reproedSegments) {
        assert.isTrue(await promiseStatus(segment.whenRecognized) == PromiseStatuses.PROMISE_RESOLVED);
        assert.isTrue(await promiseStatus(segment.whenResolved)   == PromiseStatuses.PROMISE_RESOLVED);
      }

      // Complete our deserialization of the original segment in question - needed for
      // clearer, more direct comparisons.
      const holdSegmentOriginal = new com.keyman.osk.Segment(originalSegments[1]);
      const holdSegmentRepro = reproedSegments[1];

      assert.equal(holdSegmentRepro.type, 'hold');
      assert.isAtLeast(holdSegmentRepro.duration, holdSegmentOriginal.duration - PathSegmenter.DEFAULT_CONFIG.holdMinimumDuration);

      // Complete our deserialization of the original segment in question - needed for
      // clearer, more direct comparisons.
      const moveSegmentOriginal = new com.keyman.osk.Segment(originalSegments[2]);
      const moveSegmentRepro = reproedSegments[2];

      assert.equal(moveSegmentRepro.type, 'move');
      assert.isAtLeast(moveSegmentRepro.duration, moveSegmentOriginal.duration - PathSegmenter.DEFAULT_CONFIG.holdMinimumDuration);

      // Move-specific checks
      assert.equal(moveSegmentRepro.direction, moveSegmentOriginal.direction);
      assert.isAtLeast(moveSegmentRepro.distance, moveSegmentOriginal.distance - 2 * PathSegmenter.DEFAULT_CONFIG.holdMoveTolerance);
      assert.isAtLeast(moveSegmentRepro.peakSpeed, 0.9 * moveSegmentOriginal.peakSpeed);
    });
  });
});