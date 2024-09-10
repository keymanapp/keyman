import { assert } from 'chai'
import sinon from 'sinon';

import * as PromiseStatusModule from 'promise-status-async';
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;
import { assertingPromiseStatus as promiseStatus } from '../../../../../resources/assertingPromiseStatus.js';

import { InputSample, GestureSource, gestures, CumulativePathStats } from '@keymanapp/gesture-recognizer';
import { timedPromise } from '@keymanapp/web-utils';

import {
  InstantRejectionModel,
  InstantResolutionModel,
  MainLongpressSourceModel,
  MainLongpressSourceModelWithShortcut,
  ModipressStartModel,
  ModipressEndModel,
  SimpleTapModel,
  FlickStartThreshold,
  FlickStartContactModel,
  FlickEndContactModel,
  FlickEndThreshold
} from './isolatedPathSpecs.js';

async function simulateSequence(
  samples: InputSample<string>[],
  fakeClock: sinon.SinonFakeTimers,
  emulatedContactPoint: GestureSource<string>,
  modelMatcher: gestures.matchers.PathMatcher<string>,
  options?: { terminate: boolean }
  ) {
  if(samples.length == 0) {
    return;
  }

  const startSample = samples[0];
  const samplePromises = samples.map((sample) => {
    return timedPromise(sample.t - startSample.t).then(async () => {
      emulatedContactPoint.update(sample);
      modelMatcher.update();

      if((options?.terminate ?? true) && sample == samples[samples.length-1]) {
        emulatedContactPoint.terminate(false);
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

  describe("Flick:  primary path modeling", function() {
    describe('Initial stage', function() {
      it("resolve (simple)", async function() {
        const emulatedContactPoint = new GestureSource<string>(1, null, true);
        const modelMatcher = new gestures.matchers.PathMatcher(FlickStartContactModel, emulatedContactPoint);

        const startSample = {
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        };

        const endSample = {
          targetX: FlickStartThreshold+1,
          targetY: FlickStartThreshold+1,
          t: 200,
          item: 'a'
        };

        const samples = [startSample, endSample];
        // In case of unexpected errors during sample or cancel simulation.
        await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: false});

        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
        assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'path'});
      });

      it("resolve (complex): verify *net* distance, not *raw*", async function() {
        const emulatedContactPoint = new GestureSource<string>(1, null, true);
        const modelMatcher = new gestures.matchers.PathMatcher(FlickStartContactModel, emulatedContactPoint);

        const startSample = {
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        };

        const baseSamples = [
          startSample,
          {...startSample, targetX: 2, t: 110},
          {...startSample, targetX: 2, targetY: 2, t: 120},
          {...startSample, targetY: 2, t: 130}
        ];

        // NOTE:  total distance traveled should exceed FlickStartThreshold.
        // Each `baseSamples` bit added gives a 'raw distance' of 4 aside from the first (3).
        const samples = baseSamples.concat(baseSamples.map((entry) => {
          return {...entry, t: entry.t+40};
        })).concat(baseSamples.map((entry) => {
          return {...entry, t: entry.t+80};
        }));

        // In case of unexpected errors during sample or cancel simulation.
        await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: false});

        // See NOTE above.  This test is intended to test a specific configuration condition and will
        // not do so if the .isAbove assertion below does not hold.
        assert.isAbove(emulatedContactPoint.path.stats.rawDistance, FlickStartThreshold, "Test precondition invalidated!");
        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_PENDING);

        // Now to properly complete the flick-start path.
        await simulateSequence([
          {
            targetX: FlickStartThreshold,
            targetY: FlickStartThreshold,
            item: 'a',
            t: samples[samples.length-1].t + 10
          }
        ], this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: false});

        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
        assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'path'});
      });
    });

    describe("Second stage", async function() {
      let commonBase: {
        samples: {
            targetX: number;
            targetY: number;
            t: number;
            item: string;
        }[];
        stats: CumulativePathStats<string>;
      };

      before(async function() {
        const emulatedContactPoint = new GestureSource<string>(1, null, true);
        const modelMatcher = new gestures.matchers.PathMatcher(FlickStartContactModel, emulatedContactPoint);

        const startSample = {
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        };

        const endSample = {
          targetX: FlickStartThreshold+1,
          targetY: FlickStartThreshold+1,
          t: 200,
          item: 'a'
        };

        const samples = [startSample, endSample];
        // In case of unexpected errors during sample or cancel simulation.
        await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: false});

        commonBase = {
          samples: samples,
          stats: emulatedContactPoint.path.stats
        };
      });

      it("resolve (simple)", async function() {
        const emulatedContactPoint = new GestureSource<string>(1, null, true);
        commonBase.samples.forEach((entry) => emulatedContactPoint.path.extend(entry));

        const modelMatcher = new gestures.matchers.PathMatcher(FlickEndContactModel, emulatedContactPoint, commonBase.stats);

        const endSample = {
          targetX: FlickEndThreshold+1,
          targetY: FlickEndThreshold+1,
          t: 400,
          item: 'a+'
        };

        const samples = [endSample];

        // terminate: true - we don't autotrigger on reaching the threshold.  (Can go to 'false' if we
        // enable auto-termination upon reaching the threshold.)
        await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: true});

        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
        assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'path'});
      });

      it("resolve (mild divergence)", async function() {
        const emulatedContactPoint = new GestureSource<string>(1, null, true);
        commonBase.samples.forEach((entry) => emulatedContactPoint.path.extend(entry));

        const modelMatcher = new gestures.matchers.PathMatcher(FlickEndContactModel, emulatedContactPoint, commonBase.stats);

        const endSample = {
          targetX: FlickEndThreshold+1,
          targetY: 1,
          t: 400,
          item: 'a+'
        };

        const samples = [endSample];

        // terminate: true - we don't autotrigger on reaching the threshold.  (Can go to 'false' if we
        // enable auto-termination upon reaching the threshold.)
        await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: true});

        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
        assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'path'});
      });

      it("reject (divergent direction)", async function() {
        const emulatedContactPoint = new GestureSource<string>(1, null, true);
        commonBase.samples.forEach((entry) => emulatedContactPoint.path.extend(entry));

        const modelMatcher = new gestures.matchers.PathMatcher(FlickEndContactModel, emulatedContactPoint, commonBase.stats);

        const endSample = {
          targetX: 1 - FlickEndThreshold,
          targetY: 1,
          t: 400,
          item: 'a+'
        };

        const samples = [endSample];

        // terminate: true - we don't autotrigger on reaching the threshold.  (Can go to 'false' if we
        // enable auto-termination upon reaching the threshold.)
        await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: false});

        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
        assert.deepEqual(await modelMatcher.promise, {type: 'reject', cause: 'path'});
      });
    });
  });

  describe("Instant fulfillment modeling", function() {
    it("resolve", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
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
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'path'});
    });

    it("reject", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
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
      assert.deepEqual(await modelMatcher.promise, {type: 'reject', cause: 'path'});
    });
  });

  describe("Longpress: primary path modeling", function() {
    it("resolve: path completed (long wait)", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
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
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'timer'});
    });

    it("resolve: path not completed (long wait)", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
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
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: false});

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'timer'});
    });

    it("reject: path completed (short wait)", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
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
      assert.deepEqual(await modelMatcher.promise, {type: 'reject', cause: 'path'});
    });

    it("reject: path cancelled", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
      const modelMatcher = new gestures.matchers.PathMatcher(MainLongpressSourceModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 0,
        item: 'a'
      };

      const cancelPromise = timedPromise(250).then(async () => {
        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_PENDING);

        emulatedContactPoint.terminate(true);
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
      assert.deepEqual(await modelMatcher.promise, {type: 'reject', cause: 'path'});
    });

    it("reject: distance moved", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
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
      assert.deepEqual(await modelMatcher.promise, {type: 'reject', cause: 'path'});
    });

    it("reject: item changed", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
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
      assert.deepEqual(await modelMatcher.promise, {type: 'reject', cause: 'item'});
    });

    it("reject: disabled up-flick shortcut", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
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
      assert.deepEqual(await modelMatcher.promise, {type: 'reject', cause: 'path'});
    });

    it("resolve: enabled up-flick shortcut", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
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
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, { terminate: false });

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'path'});
    });
  });

  describe("Modipress: primary path modeling", function () {
    it("push: on path start", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
      const modelMatcher = new gestures.matchers.PathMatcher(ModipressStartModel, emulatedContactPoint);

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
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'path'});
    });

    it("pop: released", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
      const modelMatcher = new gestures.matchers.PathMatcher(ModipressEndModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      };

      const samples = [startSample];
      // In case of unexpected errors during sample or cancel simulation.
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: true});

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'path'});
    });

    it("pop: item changed", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
      const modelMatcher = new gestures.matchers.PathMatcher(ModipressEndModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      };

      const endSample = {
        targetX: 2,
        targetY: 2,
        t: 500,
        item: 'b'
      }

      const samples = [startSample, endSample];
      // In case of unexpected errors during sample or cancel simulation.
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher, {terminate: false});

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'item'});
    });
  });

  describe("Simple Tap: primary path modeling", function() {
    it("resolve: path completed (long wait)", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
      const modelMatcher = new gestures.matchers.PathMatcher(SimpleTapModel, emulatedContactPoint);

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
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'path'});
    });

    it("resolve: path completed (short wait)", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
      const modelMatcher = new gestures.matchers.PathMatcher(SimpleTapModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      };

      const endSample = {
        targetX: 1,
        targetY: 1,
        t: 150,  // total duration:  50ms.
        item: 'a'
      };

      // Sample 'playback' Promise setup
      const samples = [startSample, endSample];
      await simulateSequence(samples, this.fakeClock, emulatedContactPoint, modelMatcher);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'path'});
    });

    it("reject: path cancelled", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
      const modelMatcher = new gestures.matchers.PathMatcher(SimpleTapModel, emulatedContactPoint);

      const startSample = {
        targetX: 1,
        targetY: 1,
        t: 0,
        item: 'a'
      };

      const cancelPromise = timedPromise(250).then(async () => {
        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_PENDING);

        emulatedContactPoint.terminate(true);
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
      assert.deepEqual(await modelMatcher.promise, {type: 'reject', cause: 'path'});
    });

    it("resolve: significant movement, but no item change", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
      const modelMatcher = new gestures.matchers.PathMatcher(SimpleTapModel, emulatedContactPoint);

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
      assert.deepEqual(await modelMatcher.promise, {type: 'resolve', cause: 'path'});
    });

    it("reject: little movement, but item changed", async function() {
      const emulatedContactPoint = new GestureSource<string>(1, null, true);
      const modelMatcher = new gestures.matchers.PathMatcher(SimpleTapModel, emulatedContactPoint);

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
      assert.deepEqual(await modelMatcher.promise, {type: 'reject', cause: 'item'});
    });
  });
});