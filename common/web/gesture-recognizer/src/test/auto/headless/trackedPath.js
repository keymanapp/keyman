import { assert } from 'chai'
import sinon  from 'sinon';

import { TrackedPath } from '@keymanapp/gesture-recognizer';

import { timedPromise } from '@keymanapp/web-utils';

// End of "for the integrated style..."

describe("TrackedPath", function() {
  // // File paths need to be from the package's / module's root folder
  // let testJSONtext = fs.readFileSync('src/test/resources/json/canaryRecording.json');

  describe("Single-sample 'sequence'", function() {
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
      touchpath.terminate(false); // false = "not cancelled"

      assert.deepEqual(touchpath.coords, [sample]);
    });

    it("event ordering - 'invalidated'", function() {
      const spyEventStep         = sinon.fake();
      const spyEventComplete     = sinon.fake();
      const spyEventInvalidated  = sinon.fake();

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
      touchpath.terminate(true); // false = "not cancelled"

      assert.deepEqual(touchpath.coords, [sample]);
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

      const touchpath = new TrackedPath();
      touchpath.on('step', spyEventStep);
      touchpath.on('complete', spyEventComplete);
      touchpath.on('invalidated', spyEventInvalidated);

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
        return timedPromise(sample.t - startSample.t).then(() => {
          touchpath.extend(sample);
        });
      });

      // Finalization: segmentation is then ended via a followup event as soon as the
      // final sample's execution returns down to the base event loop.
      const segmentationEndPromise = Promise.all(samplePromises).then(() => {
        touchpath.terminate(false);
      }).then(() => {
        // The main test assertions.
        assert.deepEqual(touchpath.coords, [startSample, endSample]);
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

  describe("Recording-based integration tests", function() {
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
  });
});