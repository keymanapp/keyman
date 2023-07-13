import { assert } from 'chai'
import sinon  from 'sinon';

import * as PromiseStatusModule from 'promise-status-async';
const promiseStatus       = PromiseStatusModule.promiseStatus;
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;

import {  PathSegmenter } from '@keymanapp/gesture-recognizer';
import { timedPromise } from '@keymanapp/web-utils';

describe("Basic segmentation cases", function() {
  describe("Single-sample 'sequence'", function() {
    it("expected segment types", function() {
      let spy = sinon.fake();

      const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

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

      segmenter.add(sample);
      try {
        assert.isTrue(spy.calledTwice, "Segmenter callback was not called exactly twice before the .close().");
      } finally {
        segmenter.close();
      }
      assert.isTrue(spy.calledThrice, "Segmenter callback was not called exactly once after the .close().");

      for(let i=0; i < 3; i++) {
        assert.equal(spy.args[i].length, 1, "Segmenter callback received an unexpected number of arguments");
      }

      // Is the first argument of each call's argument set.
      assert.equal(spy.firstCall .args[0].type, 'start', "First call should receive a 'start'-type segment.");
      assert.equal(spy.secondCall.args[0].type, undefined, "Second call's segment should not be classified.");
      assert.equal(spy.thirdCall .args[0].type, 'end', "Third call should receive an 'end'-type segment.");
    });

    it("segment recognition + resolution", async function() {
      let spy = sinon.fake();

      const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

      const sample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      segmenter.add(sample);
      try {
        // It's still best to verify this before proceeding.
        assert.isTrue(spy.calledTwice, "Segmenter callback was not called exactly twice before the .close().");

        // 'start':  should be fully resolved, right out of the gate.
        assert.equal(await promiseStatus(spy.firstCall.args[0].whenRecognized), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(spy.firstCall.args[0].whenResolved), PromiseStatuses.PROMISE_RESOLVED);

        // null-typed, just-starting segment:  neither recognized nor resolved.
        assert.equal(await promiseStatus(spy.secondCall.args[0].whenRecognized), PromiseStatuses.PROMISE_PENDING);
        assert.equal(await promiseStatus(spy.secondCall.args[0].whenResolved), PromiseStatuses.PROMISE_PENDING);
      } finally {
        segmenter.close();
      }

      assert.isTrue(spy.calledThrice, "Segmenter callback was not called exactly once after the .close().");

      // null-typed segment should now be recognized and resolved.
      assert.equal(await promiseStatus(spy.secondCall.args[0].whenRecognized), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(spy.secondCall.args[0].whenResolved), PromiseStatuses.PROMISE_RESOLVED);

      // 'end':  should be fully resolved, right out of the gate.
      assert.equal(await promiseStatus(spy.thirdCall.args[0].whenRecognized), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(spy.thirdCall.args[0].whenResolved), PromiseStatuses.PROMISE_RESOLVED);
    });
  });

  describe("Single held point", function() {
    beforeEach(function() {
      this.fakeClock = sinon.useFakeTimers();
    })

    afterEach(function() {
      this.fakeClock.restore();
    })

    it("'hold' segment recognition + resolution", async function() {
      let spy = sinon.fake();

      const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

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

      const segment2Recognition = sinon.fake();
      const segment2Resolution  = sinon.fake();

      // Timestamp 1:  segmentation begins, with an initial Sample recorded.
      const firstPromise = timedPromise(0).then(() => {
        segmenter.add(startSample);
        assert.isTrue(spy.calledTwice, "Segmenter callback was not called exactly twice upon adding the first point.");

        // The focus of this unit test is the two `Promise`s provided by this specific `Segment`.
        const pendingSegment = spy.secondCall.args[0];
        assert.exists(pendingSegment);

        pendingSegment.whenRecognized.then(segment2Recognition);
        pendingSegment.whenResolved.then(segment2Resolution);
      }).then(() => {
        // At time = 0, neither recognition nor resolution should have triggered.
        assert.isFalse(segment2Recognition.called);

        const pendingSegment = spy.secondCall.args[0];
        assert.isNull(pendingSegment.type); // not yet recognized.

        assert.isFalse(segment2Resolution.called);
      });

      // Timestamp 2:  segmentation continues... a second later.  A second Sample is recorded.
      const secondSampleTestPromise = firstPromise.then(() => {
        return timedPromise(1000).then(() => {
          // This should have occurred already, despite no new sample having been provided
          // to the segmenter.
          assert.isTrue(segment2Recognition.called);

          const pendingSegment = spy.secondCall.args[0];
          assert.equal(pendingSegment.type, 'hold'); // now has a type, as it's been recognized.

          // And now to update with a new sample.
          segmenter.add(endSample);
        }).then(() => {
          assert.isFalse(segment2Resolution.called);

          const pendingSegment = spy.secondCall.args[0];
          assert.isAtLeast(pendingSegment.duration, 1000); // Latest sample's timestamp gives exactly 1000.
        });
      });

      // Timestamp 3: segmentation is then ended via a followup event.
      const segmentationEndPromise = secondSampleTestPromise.then(() => {
        assert.isFalse(segment2Resolution.called);
        segmenter.close();
        // Fun fact:  assert.isFalse(segment2Resolution.called) still holds here
        //            (after `segmenter.close()`) because Promises are async.
      }).then(() => {
        assert.isTrue(segment2Resolution.called);
        assert.isTrue(spy.calledThrice);
      })

      const finalPromise = segmentationEndPromise.catch((reason) => {
        segmenter.close();
        throw reason;
      });

      this.fakeClock.runAllAsync();

      // This is the one that reports all of our async assertion failures.
      return finalPromise;
    });

    it("motionless 'hold' segment properties", function() {
      const spy = sinon.fake();

      const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

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
          segmenter.add(sample);
        });
      });

      // Timestamp 3: segmentation is then ended via a followup event.
      const segmentationEndPromise = Promise.all(samplePromises).then(() => {
        segmenter.close();
      });

      const finalPromise = segmentationEndPromise.catch((reason) => {
        segmenter.close();
        throw reason;
      });

      this.fakeClock.runAllAsync();

      // This is the one that reports all of our async assertion failures.
      return finalPromise.then(() => {
        const holdSegment = spy.secondCall.args[0];

        assert.equal(holdSegment.type, 'hold');
        assert.isAtLeast(holdSegment.duration, endSample.t - startSample.t);
        assert.equal(holdSegment.speed, 0);
        assert.isUndefined(holdSegment.angle);
        assert.isUndefined(holdSegment.direction);
        assert.equal(holdSegment.peakSpeed, 0);
        assert.equal(holdSegment.distance, 0);

        // Samples may be cloned when passed through the segmenter!
        assert.deepEqual(holdSegment.initialCoord, startSample);
        assert.deepEqual(holdSegment.lastCoord, endSample);
      });
    });
  });

  describe("Idealized 'move'", function() {
    beforeEach(function() {
      this.fakeClock = sinon.useFakeTimers();
    })

    afterEach(function() {
      this.fakeClock.restore();
    })

    /**
     * Builds a diagonal sequence traveling 40 pixels 's', 40 pixels 'e' in a perfect diagonal lock-step.
     */
    const buildSampleSequence = () => {
      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      let samples = [startSample];

      for(let i=1; i <=20; i++) {
        samples.push({
          targetX: startSample.targetX + 2 * i,
          targetY: startSample.targetY + 2 * i,
          t: startSample.t + 10 * i
        });
      }

      return samples;
    }

    it("'move' segment recognition + resolution", async function() {
      let spy = sinon.fake();

      const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

      const samples = buildSampleSequence();

      const samplePromises = samples.map((sample) => {
        return timedPromise(sample.t - samples[0].t).then(() => {
          segmenter.add(sample);
        });
      });

      // Find and replace select promises with then'd versions of themselves to 'hook in' as needed.
      // We know the 'move' segment will appear upon completion of the first sample's Promise.

      const segment2Recognition = sinon.fake();
      const segment2Resolution  = sinon.fake();

      samplePromises[0] = samplePromises[0].then(async () => {
        assert.isAtLeast(spy.callCount, 2);
        const moveSegment = spy.secondCall.args[0];

        assert.equal(await promiseStatus(moveSegment.whenRecognized), PromiseStatuses.PROMISE_PENDING);
        assert.equal(await promiseStatus(moveSegment.whenResolved), PromiseStatuses.PROMISE_PENDING);

        // Now that we're sure they haven't already been called, let's set the `fake` calls in place.
        moveSegment.whenRecognized.then(segment2Recognition);
        moveSegment.whenResolved.then(segment2Resolution);

        assert.isNotNull(moveSegment);
      });

      // Given the current engine setup, we know that after 5 samples, we pass the movement-based
      // recognition threshold and thus should be classified as a 'move'.
      samplePromises[5] = samplePromises[5].then(() => {
        const moveSegment = spy.secondCall.args[0];

        assert.isTrue(segment2Recognition.called);
        assert.isFalse(segment2Resolution.called);

        assert.equal(moveSegment.type, 'move');
      });

      samplePromises[20] = samplePromises[20].then(() => {
        const moveSegment = spy.secondCall.args[0];

        assert.isTrue(segment2Recognition.called);
        assert.isFalse(segment2Resolution.called);

        assert.isAtLeast(moveSegment.duration, samples[20].t - samples[0].t);
      });

      // Timestamp 3: segmentation is then ended via a followup event.
      const segmentationEndPromise = Promise.all(samplePromises).then(() => {
        assert.isFalse(segment2Resolution.called);
        segmenter.close();
      }).then(() => {
        assert.isTrue(segment2Resolution.called);
        assert.equal(spy.callCount, 3);
      });

      const finalPromise = segmentationEndPromise.catch((reason) => {
        segmenter.close();
        throw reason;
      });

      this.fakeClock.runAllAsync();

      // This is the one that reports all of our async assertion failures.
      return finalPromise;
    });

    it("idealized 'move' segment properties", function() {
      let spy = sinon.fake();

      const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

      const samples = buildSampleSequence();

      const samplePromises = samples.map((sample) => {
        return timedPromise(sample.t - samples[0].t).then(() => {
          segmenter.add(sample);
        });
      });

      // Timestamp 3: segmentation is then ended via a followup event.
      const segmentationEndPromise = Promise.all(samplePromises).then(() => {
        segmenter.close();
      });

      const finalPromise = segmentationEndPromise.catch((reason) => {
        segmenter.close();
        throw reason;
      });

      this.fakeClock.runAllAsync();

      // This is the one that reports all of our async assertion failures.
      return finalPromise.then(() => {
        // The real checks.
        const moveSegment = spy.secondCall.args[0];

        assert.equal(moveSegment.type, 'move');
        assert.isAtLeast(moveSegment.duration, samples[20].t - samples[0].t);
        assert.equal(moveSegment.direction, 'se');
        assert.equal(moveSegment.distance, 40 * Math.SQRT2);
        assert.equal(moveSegment.angle, (135 / 180) * Math.PI);

        // Speed:  our idealized sequence moves 2*sqrt(2) px distance every 10ms.
        const idealSpeed = 2 * Math.sqrt(2) / 10;

        assert.isAtLeast(moveSegment.speed, 0.99 * idealSpeed);
        assert.isAtLeast(moveSegment.peakSpeed, 0.99 * idealSpeed);

        // Samples may be cloned when passed through the segmenter!
        assert.deepEqual(moveSegment.initialCoord, samples[0]);
        assert.deepEqual(moveSegment.lastCoord, samples[20]);
      });
    });
  });
});