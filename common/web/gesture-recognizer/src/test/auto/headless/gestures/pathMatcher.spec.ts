import { assert } from 'chai'
import sinon from 'sinon';

import * as PromiseStatusModule from 'promise-status-async';
const promiseStatus       = PromiseStatusModule.promiseStatus;
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;

import { InputSample, SimpleGestureSource, gestures } from '@keymanapp/gesture-recognizer';
import { timedPromise } from '@keymanapp/web-utils';

import {
  InstantRejectionModel,
  InstantResolutionModel,
  MainLongpressSourceModel,
  MainLongpressSourceModelWithShortcut
} from './isolatedPathSpecs.js';

async function simulateSequence(
  samples: InputSample<string>[],
  fakeClock: sinon.SinonFakeTimers,
  emulatedContactPoint: SimpleGestureSource<string>,
  modelMatcher: gestures.matchers.PathMatcher<string>,
  options?: { terminate: boolean }
  ) {
  if(samples.length == 0) {
    return;
  }

  const startSample = samples[0];
  const samplePromises = samples.map((sample) => {
    return timedPromise(sample.t - startSample.t).then(async () => {
      emulatedContactPoint.path.extend(sample);
      modelMatcher.update();

      if((options?.terminate ?? true) && sample == samples[samples.length-1]) {
        emulatedContactPoint.path.terminate(false);
        modelMatcher.update();
      }
    });
  });

  await fakeClock.runAllAsync();
  // In case of unexpected errors during sample or cancel simulation.
  await Promise.all(samplePromises);
}

describe("PathMatcher", function() {
  // // File paths need to be from the package's / module's root folder
  // let testJSONtext = fs.readFileSync('src/test/resources/json/canaryRecording.json');

  beforeEach(function() {
    this.fakeClock = sinon.useFakeTimers();
  })

  afterEach(function() {
    this.fakeClock.restore();
  })

  describe("Instant fulfillment modeling", function() {
    it("resolve", async function() {
      const emulatedContactPoint = new SimpleGestureSource<string>(1, true);
      const modelMatcher = new gestures.matchers.PathMatcher(InstantResolutionModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      };

      const samples = [startSample];
      // In case of unexpected errors during sample or cancel simulation.
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: false});

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve'});
    });

    it("reject", async function() {
      const emulatedContactPoint = new SimpleGestureSource<string>(1, true);
      const modelMatcher = new gestures.matchers.PathMatcher(InstantRejectionModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      };

      const samples = [startSample];
      // In case of unexpected errors during sample or cancel simulation.
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: false});

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'reject'});
    });
  });

  describe("Longpress: primary path modeling", function() {
    it("resolve: longpress timer completed", async function() {
      const emulatedContactPoint = new SimpleGestureSource<string>(1, true);
      const modelMatcher = new gestures.matchers.PathMatcher(MainLongpressSourceModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      };

      const endSample = {
        targetX: 1,
        targetY: 1,
        t: 1100,  // total duration:  1 sec.
        item: 'a'
      };

      const samples = [startSample, endSample];
      // In case of unexpected errors during sample or cancel simulation.
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve'});
    });

    it("reject: longpress timer not completed", async function() {
      const emulatedContactPoint = new SimpleGestureSource<string>(1, true);
      const modelMatcher = new gestures.matchers.PathMatcher(MainLongpressSourceModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      };

      const endSample = {
        targetX: 1,
        targetY: 1,
        t: 350,  // total duration:  1 sec.
        item: 'a'
      };

      // Sample 'playback' Promise setup
      const samples = [startSample, endSample];
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'reject'});
    });

    it("reject: path cancelled", async function() {
      const emulatedContactPoint = new SimpleGestureSource<string>(1, true);
      const modelMatcher = new gestures.matchers.PathMatcher(MainLongpressSourceModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 0,
        item: 'a'
      };

      const cancelPromise = timedPromise(250).then(async () => {
        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_PENDING);

        emulatedContactPoint.path.terminate(true);
        modelMatcher.update();

        // As there's a minor level of indirection in the internal promises, we need the following
        // line for the assertion to hold.
        await Promise.resolve();
        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      });

      // Sample 'playback' Promise setup
      const samples = [startSample];
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: false});
      // // In case of unexpected errors during cancel simulation.
      await cancelPromise;

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'reject'});
    });

    it("reject: distance moved", async function() {
      const emulatedContactPoint = new SimpleGestureSource<string>(1, true);
      const modelMatcher = new gestures.matchers.PathMatcher(MainLongpressSourceModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      };

      // Obviously, this is a bit of a jump... but it's enough for the test.
      const midSample = {
        // rawDistance threshold: 10.  We're moving 9*sqrt(2), which is easily greater.
        targetX: 10,
        targetY: 10,
        t: 350,  // within standard longpress hold time
        item: 'a'
      };

      const endSample = {
        targetX: 10,
        targetY: 10,
        t: 1000,  // total duration:  1 sec.
        item: 'a'
      };

      // Sample 'playback' Promise setup
      const samples = [startSample, midSample, endSample];
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'reject'});
    });

    it("reject: item changed", async function() {
      const emulatedContactPoint = new SimpleGestureSource<string>(1, true);
      const modelMatcher = new gestures.matchers.PathMatcher(MainLongpressSourceModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      };

      const midSample = {
        // within the rawDistance threshold of 10.  We're moving sqrt(2), which is easily smaller.
        targetX: 2,
        targetY: 2,
        t: 350,  // within standard longpress hold time
        // Different 'item' than initial:  longpress reset will be needed.
        item: 'b'
      };

      const endSample = {
        targetX: 2,
        targetY: 2,
        t: 1000,  // total duration:  1 sec.
        item: 'b'
      };

      const samples = [startSample, midSample, endSample];
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'reject'});
    });

    it("reject: disabled up-flick shortcut", async function() {
      const emulatedContactPoint = new SimpleGestureSource<string>(1, true);
      const modelMatcher = new gestures.matchers.PathMatcher(MainLongpressSourceModel, emulatedContactPoint);

      const startSample = {
        targetX: 0,
        targetY: 0,
        t: 100,
        item: 'a'
      };

      const endSample = {
        // past the rawDistance threshold of 10.   Would fail from distance.
        targetX: 0,
        targetY: -20, // vertical north - should trigger the up-flick shortcut if enabled.
        t: 350,  // within standard longpress hold time
        item: 'a'
      };

      const samples = [startSample, endSample];
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'reject'});
    });

    it("resolve: enabled up-flick shortcut", async function() {
      const emulatedContactPoint = new SimpleGestureSource<string>(1, true);
      const modelMatcher = new gestures.matchers.PathMatcher(MainLongpressSourceModelWithShortcut, emulatedContactPoint);

      const startSample = {
        targetX: 0,
        targetY: 0,
        t: 100,
        item: 'a'
      };

      const endSample = {
        // past the rawDistance threshold of 10.   Would fail from distance.
        targetX: 0,
        targetY: -20, // vertical north - should trigger the up-flick shortcut if enabled.
        t: 350,  // within standard longpress hold time
        item: 'a'
      };

      const samples = [startSample, endSample];
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve'});
    });
  });
});