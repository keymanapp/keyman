const assert = require('chai').assert;
const sinon = require('sinon');

const fs = require('fs');

const GestureRecognizer = require('../../../../build/index.js');
const com = GestureRecognizer.com;
const TrackedPath = com.keyman.osk.TrackedPath;

const timedPromise = require('../../resources/timedPromise.js');

describe("TrackedPath", function() {
  // // File paths need to be from the package's / module's root folder
  // let testJSONtext = fs.readFileSync('src/test/resources/json/canaryRecording.json');

  describe("Single-sample 'sequence'", function() {
    // A near-duplicate of the "Segmentation" -> "Single-sample 'sequence'" test.
    // Acts as a mild 'integration' test of the segmentation engine + the public-facing
    // TrackedPath events it advertises.
    it("'segmentation' - expected segment types", function() {
      let spy = sinon.fake();

      const touchpath = new TrackedPath();
      touchpath.on('segmentation', spy);

      const sample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      // Expected sequence:
      // 1:  on `segmenter.add(sample)`:
      //     a. 'start' segment
      //     b.  unrecognized (null-type) segment.
      // 2: on `segmenter.close()`:
      //     b. 'end' segment

      touchpath.extend(sample);
      try {
        assert.isTrue(spy.calledTwice, "'segmentation' event was not raised exactly twice before the .close().");
      } finally {
        touchpath.terminate(false); // false = "not cancelled"
      }
      assert.isTrue(spy.calledThrice, "'segmentation' event was not raised exactly once after the .close().");

      for(let i=0; i < 3; i++) {
        assert.equal(spy.args[i].length, 1, "'segmentation' event was raised with an unexpected number of arguments");
      }

      assert.equal(spy.firstCall.args[0].type, 'start', "First event should provide a 'start'-type segment.");
      assert.equal(spy.secondCall.args[0].type, undefined, "Second event's segment should not be classified.");
      assert.equal(spy.thirdCall.args[0].type, 'end', "Third event should provide an 'end'-type segment.");

      assert.deepEqual(touchpath.coords, [sample]);
      assert.deepEqual(touchpath.segments, spy.getCalls().map((call) => call.args[0]),
        "The touchpath's segment array does not match the `Segment`s from raised events.");
    });

    it("'step', 'complete' events", function() {
      const spyEventStep        = sinon.fake();
      const spyEventComplete    = sinon.fake();
      const spyEventInvalidated = sinon.fake();

      const touchpath = new TrackedPath();
      touchpath.on('step', spyEventStep);
      touchpath.on('complete', spyEventComplete);
      touchpath.on('invalidated', spyEventInvalidated);

      const sample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      touchpath.extend(sample);
      try {
        assert(spyEventStep.calledOnce, "'step' event was not raised exactly once.");

        const stepSample = spyEventStep.firstCall.args[0];
        assert.equal(stepSample, sample, "'step' did not provide the expected sample.");

        assert(spyEventComplete.notCalled, "'complete' event was raised early.");
        assert(spyEventInvalidated.notCalled, "'invalidated' event was erroneously raised.");
      } finally {
        touchpath.terminate(false); // false = "not cancelled"
      }
      assert(spyEventComplete.calledOnce, "'complete' event was not raised.");
      assert(spyEventInvalidated.notCalled, "'invalidated' event was erroneously raised.");
    });


    it("'step', 'invalidated' events", function() {
      const spyEventStep        = sinon.fake();
      const spyEventComplete    = sinon.fake();
      const spyEventInvalidated = sinon.fake();

      const touchpath = new TrackedPath();
      touchpath.on('step', spyEventStep);
      touchpath.on('complete', spyEventComplete);
      touchpath.on('invalidated', spyEventInvalidated);

      const sample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      touchpath.extend(sample);
      try {
        assert(spyEventStep.calledOnce, "'step' event was not raised exactly once.");

        const stepSample = spyEventStep.firstCall.args[0];
        assert.equal(stepSample, sample, "'step' did not provide the expected sample.");

        assert(spyEventComplete.notCalled, "'complete' event was erroneously raised.");
        assert(spyEventInvalidated.notCalled, "'invalidated' event was raised early.");
      } finally {
        touchpath.terminate(true); // true = "cancelled"
      }
      assert(spyEventComplete.notCalled, "'complete' event was erroneously raised.");
      assert(spyEventInvalidated.calledOnce, "'invalidated' event was not raised.");
    });

    it("event ordering - 'complete'", function() {
      const spyEventStep         = sinon.fake();
      const spyEventComplete     = sinon.fake();
      const spyEventInvalidated  = sinon.fake();
      const spyEventSegmentation = sinon.fake();

      const touchpath = new TrackedPath();
      touchpath.on('step', spyEventStep);
      touchpath.on('complete', spyEventComplete);
      touchpath.on('invalidated', spyEventInvalidated);
      touchpath.on('segmentation', spyEventSegmentation);

      const sample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      touchpath.extend(sample);
      touchpath.terminate(false); // false = "not cancelled"

      assert(spyEventStep.firstCall.calledBefore(spyEventSegmentation.firstCall),
        "'step' should be raised before 'segmentation'");
      assert(spyEventSegmentation.thirdCall.calledBefore(spyEventComplete.firstCall),
        "all 'segmentation' events should be raised before 'complete'");

      const spy = spyEventSegmentation;
      assert.deepEqual(touchpath.coords, [sample]);
      spy.call
      assert.deepEqual(touchpath.segments, spy.getCalls().map((call) => call.args[0]));
    });

    it("event ordering - 'invalidated'", function() {
      const spyEventStep         = sinon.fake();
      const spyEventComplete     = sinon.fake();
      const spyEventInvalidated  = sinon.fake();
      const spyEventSegmentation = sinon.fake();

      const touchpath = new TrackedPath();
      touchpath.on('step', spyEventStep);
      touchpath.on('complete', spyEventComplete);
      touchpath.on('invalidated', spyEventInvalidated);
      touchpath.on('segmentation', spyEventSegmentation);

      const sample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      touchpath.extend(sample);
      touchpath.terminate(true); // false = "not cancelled"

      assert(spyEventStep.firstCall.calledBefore(spyEventSegmentation.firstCall),
        "'step' should be raised before 'segmentation'");
      assert(spyEventSegmentation.secondCall.calledBefore(spyEventInvalidated.firstCall),
        "first 'segmentation' event ('start' segment) not raised before cancellation");
      assert(spyEventSegmentation.thirdCall.calledAfter(spyEventInvalidated.firstCall),
        "Cancellation event not raised before cancelled segment completion");

      // Even though the touchpath was 'cancelled', we should still see the segments that finished processing.
      const spy = spyEventSegmentation;
      assert.deepEqual(touchpath.coords, [sample]);
      assert.deepEqual(touchpath.segments, spy.getCalls().map((call) => call.args[0]));
    });
  });


  describe("Single held point", function() {
    beforeEach(function() {
      this.fakeClock = sinon.useFakeTimers();
    })

    afterEach(function() {
      this.fakeClock.restore();
    })

    it("path.segments + path.coords - no extra samples", async function() {
      const spyEventStep         = sinon.fake();
      const spyEventComplete     = sinon.fake();
      const spyEventInvalidated  = sinon.fake();
      const spyEventSegmentation = sinon.fake();

      const touchpath = new TrackedPath();
      touchpath.on('step', spyEventStep);
      touchpath.on('complete', spyEventComplete);
      touchpath.on('invalidated', spyEventInvalidated);
      touchpath.on('segmentation', spyEventSegmentation);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      const endSample = {
        targetX: 1,
        targetY: 1,
        t: 1100  // total duration:  1 sec.
      };

      // Sample 'playback' Promise setup
      const samples = [startSample, endSample];

      const samplePromises = samples.map((sample) => {
        return timedPromise(() => {
          touchpath.extend(sample);
        }, sample.t - startSample.t);
      });

      // Finalization: segmentation is then ended via a followup event as soon as the
      // final sample's execution returns down to the base event loop.
      const segmentationEndPromise = Promise.all(samplePromises).then(() => {
        touchpath.terminate(false);
      }).then(() => {
        // The main test assertions.
        assert.deepEqual(touchpath.coords, [startSample, endSample]);
        assert.deepEqual(touchpath.segments, spyEventSegmentation.getCalls().map((call) => call.args[0]));
        assert.deepEqual(spyEventSegmentation.getCalls().map((call) => call.args[0].type), ['start', 'hold', 'end']);
      });

      const finalPromise = segmentationEndPromise.catch((reason) => {
        if(!touchpath.isComplete) {
          touchpath.terminate(true);
        }
        throw reason;
      });

      this.fakeClock.runAllAsync();

      // This is the one that reports all of our async assertion failures.
      return finalPromise;
    });
  });
});