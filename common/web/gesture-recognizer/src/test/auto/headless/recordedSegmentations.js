const assert = require('chai').assert;
const sinon = require('sinon');

const fs = require('fs');

const PromiseStatusModule = require('promise-status-async');
const promiseStatus       = PromiseStatusModule.promiseStatus;
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;

const GestureRecognizer = require('../../../../build/index.js');
const com = GestureRecognizer.com;
const PathSegmenter = com.keyman.osk.PathSegmenter;

const Testing = require('../../../../build/tools/unit-test-resources.js');
const HeadlessRecordingSimulator = Testing.HeadlessRecordingSimulator;

const SEGMENT_TEST_JSON_FOLDER = 'src/test/resources/json/segmentation';

const assertSegmentSimilarity = require('../../resources/assertSegmentSimilarity.js');

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

      const configObj = {
        replaySample: (sample) => segmenter.add(sample),
        endSequence:  () => segmenter.close()
      }

      const testObj = HeadlessRecordingSimulator.prepareTest(jsonObj, configObj);

      await this.fakeClock.runAllAsync();
      await testObj.compositePromise;

      // Any post-sequence tests to run.
      const originalSegments = testObj.originalSegments;
      const originalSegmentTypeSequence = originalSegments.map((segment) => segment.type);

      const reproedSegments = spy.getCalls().map((call) => call.args[0]);
      const reproedSegmentTypeSequence  = reproedSegments.map((segment) => segment.type);

      assert.sameOrderedMembers(reproedSegmentTypeSequence, originalSegmentTypeSequence);

      // Ensure all relevant Promises resolved.
      for(let segment of reproedSegments) {
        assert.isTrue(await promiseStatus(segment.whenRecognized) == PromiseStatuses.PROMISE_RESOLVED);
        assert.isTrue(await promiseStatus(segment.whenResolved)   == PromiseStatuses.PROMISE_RESOLVED);
      }

      assertSegmentSimilarity(reproedSegments[1], originalSegments[1], 'hold');
      assertSegmentSimilarity(reproedSegments[2], originalSegments[2], 'move');
    });
  });
});