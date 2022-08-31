const assert = require('chai').assert;
const sinon = require('sinon');

const fs = require('fs');

const PromiseStatusModule = require('promise-status-async');
const promiseStatus       = PromiseStatusModule.promiseStatus;
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;

const GestureRecognizer = require('../../../../build/index.js');
const com = GestureRecognizer.com;
const PathSegmenter = com.keyman.osk.PathSegmenter;

/**
 * Acts as a Promise-form of `setTimeout`.
 * @param {*} func A function to run after the specified amount of time.
 * @param {*} time The timeout to wait.
 * @returns {*} A `Promise` that will either resolve or reject after the specified amount of time.
 */
const timedPromise = (func, time) => {
  return new Promise((resolve, reject) => {
    setTimeout(() => {
      try {
        func();
        resolve();
      } catch (err) {
        reject(err);
      }
    }, time);
  });
}

describe("Segmentation", function() {
  // // File paths need to be from the package's / module's root folder
  // let testJSONtext = fs.readFileSync('src/test/resources/json/canaryRecording.json');

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
      const firstPromise = timedPromise(() => {
        segmenter.add(startSample);
        assert.isTrue(spy.calledTwice, "Segmenter callback was not called exactly twice upon adding the first point.");

        // The focus of this unit test is the two `Promise`s provided by this specific `Segment`.
        const pendingSegment = spy.secondCall.args[0];
        assert.exists(pendingSegment);

        pendingSegment.whenRecognized.then(segment2Recognition);
        pendingSegment.whenResolved.then(segment2Resolution);
      }, 0).then(() => {
        // At time = 0, neither recognition nor resolution should have triggered.
        assert.isFalse(segment2Recognition.called);

        const pendingSegment = spy.secondCall.args[0];
        assert.isNull(pendingSegment.type); // not yet recognized.

        assert.isFalse(segment2Resolution.called);
      });

      // Timestamp 2:  segmentation continues... a second later.  A second Sample is recorded.
      const secondSampleTestPromise = firstPromise.then(() => {
        return timedPromise(() => {
          // This should have occurred already, despite no new sample having been provided
          // to the segmenter.
          assert.isTrue(segment2Recognition.called);

          const pendingSegment = spy.secondCall.args[0];
          assert.equal(pendingSegment.type, 'hold'); // now has a type, as it's been recognized.

          // And now to update with a new sample.
          segmenter.add(endSample);
        }, 1000).then(() => {
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
  });
});