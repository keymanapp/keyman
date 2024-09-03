import { assert } from 'chai'
import sinon from 'sinon';

import * as PromiseStatusModule from 'promise-status-async';
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;
import { assertingPromiseStatus as promiseStatus } from '../../../../../resources/assertingPromiseStatus.js';

import { InputSample, gestures, GestureDebugPath } from '@keymanapp/gesture-recognizer';

import { TouchpathTurtle } from '#gesture-tools';

import { simulateMultiSourceMatcherInput } from "../../../../../resources/simulateMultiSourceInput.js";

import {
  FlickEndModel,
  FlickStartModel,
  LongpressModel,
  MultitapModel,
  SimpleTapModel,
  SubkeySelectModel
} from './isolatedGestureSpecs.js';

import {
  FlickEndThreshold,
  FlickStartThreshold,
  LongpressDistanceThreshold,
  MainLongpressSourceModel
} from './isolatedPathSpecs.js';

type PathInheritanceType = gestures.specs.ContactModel<string>['pathInheritance'];
function dummyInheritanceMatcher(inheritanceType: PathInheritanceType): gestures.specs.GestureModel<string> {
  return {
    id: "dummy",
    resolutionPriority: 0,
    resolutionAction: { type: 'complete', item: 'current' },
    contacts: [
      {
        model: {
          pathInheritance: inheritanceType,
          itemPriority: 0,
          pathResolutionAction: 'resolve',
          pathModel: {
            evaluate: () => undefined
          }
        }
      }
    ]
  }
}

describe("GestureMatcher", function() {
  // // File paths need to be from the package's / module's root folder
  // let testJSONtext = fs.readFileSync('src/test/resources/json/canaryRecording.json');

  beforeEach(function() {
    this.fakeClock = sinon.useFakeTimers();
  })

  afterEach(function() {
    this.fakeClock.restore();
  })

  describe("Path inheritance handling", function() {
    it("'chop'", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      });
      turtle.wait(MainLongpressSourceModel.timer.duration, 25);
      turtle.hoveredItem = 'b'; // A distinct aspect of 'chop' behavior.
      const waitCompletionSample = turtle.commitPending();
      turtle.move(90, 50, 100, 4);
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        { type: 'sequence', samples: turtle.path, terminate: true }
      ], this.fakeClock, LongpressModel);

      let completion = executor();
      const modelMatcher = await modelMatcherPromise;

      await Promise.race([completion, modelMatcher.promise]);

      // Copies from the longpress unit test to find a good spot to split the path.
      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

      // And now for the real meat of the test.

      // Starts a new matcher for a followup gesture component/link, which has a number of fun
      // intentional side-effects.
      const secondMatcher = new gestures.matchers.GestureMatcher<string>(dummyInheritanceMatcher('chop'), modelMatcher);
      // There's only the one touchpoint, so there's no need for synchronization overhead here.
      sources[0].path.on('step', () => secondMatcher.update());
      sources[0].path.on('complete', () => {
        secondMatcher.update();
      });


      // Because 'chopped'.
      assert.equal(secondMatcher.sources[0].path.stats.sampleCount, 1);
      assert.deepEqual(sources[0].currentSample, waitCompletionSample);
      // because we 'chopped' the path, we use the current sample's item as the new base.
      assert.equal(secondMatcher.sources[0].baseItem, 'b');
      // This technically does affect what the first `modelMatcher` would see as the base item, but its Promise
      // is already fulfilled - its effects are already fully committed.

      const firstMatcherStats = modelMatcher.sources[0].path.stats;
      assert.equal(firstMatcherStats.duration, MainLongpressSourceModel.timer.duration);
      assert.equal(firstMatcherStats.rawDistance, 0);

      // subview
      assert.equal(secondMatcher.sources[0].path.stats.duration, 0);
      // original
      assert.equal(sources[0].path.stats.duration, MainLongpressSourceModel.timer.duration);

      await completion;

      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      const secondMatcherStats = secondMatcher.sources[0].path.stats;
      assert.equal(secondMatcherStats.duration, 100);
      assert.closeTo(secondMatcherStats.netDistance, 50, 1e-6);
      assert.equal(secondMatcherStats.cardinalDirection, 'e');
    });

    it("'partial'", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      });
      turtle.wait(MainLongpressSourceModel.timer.duration, 25);
      turtle.hoveredItem = 'b'; // A distinct aspect of 'chop' behavior.
      const waitCompletionSample = turtle.commitPending();
      turtle.move(90, 50, 100, 4);
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        { type: 'sequence', samples: turtle.path, terminate: true }
      ], this.fakeClock, LongpressModel);

      let completion = executor();
      const modelMatcher = await modelMatcherPromise;

      await Promise.race([completion, modelMatcher.promise]);

      // Copies from the longpress unit test to find a good spot to split the path.
      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

      // And now for the real meat of the test.

      // Starts a new matcher for a followup gesture component/link, which has a number of fun
      // intentional side-effects.
      const secondMatcher = new gestures.matchers.GestureMatcher<string>(dummyInheritanceMatcher('partial'), modelMatcher);
      // There's only the one touchpoint, so there's no need for synchronization overhead here.
      sources[0].path.on('step', () => secondMatcher.update());
      sources[0].path.on('complete', () => {
        secondMatcher.update();
      });


      // 'partial' path inheritance still drops the pre-existing path components...
      assert.equal(secondMatcher.sources[0].path.stats.sampleCount, 1);
      assert.deepEqual(sources[0].currentSample, waitCompletionSample);
      // ... but preserves the original base item.
      assert.equal(sources[0].baseItem, 'a');

      // The rest of what's below should match the assertions for the 'chop' path.
      const firstMatcherStats = modelMatcher.sources[0].path.stats;
      assert.equal(firstMatcherStats.duration, MainLongpressSourceModel.timer.duration);
      assert.equal(firstMatcherStats.rawDistance, 0);

      assert.equal(secondMatcher.sources[0].path.stats.duration, 0);
      assert.equal(sources[0].path.stats.duration, MainLongpressSourceModel.timer.duration);

      await completion;

      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      const secondMatcherStats = secondMatcher.sources[0].path.stats;
      assert.equal(secondMatcherStats.duration, 100);
      assert.closeTo(secondMatcherStats.netDistance, 50, 1e-6);
      assert.equal(secondMatcherStats.cardinalDirection, 'e');
    });

    it("'reject'", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      });
      turtle.wait(MainLongpressSourceModel.timer.duration, 25);
      turtle.move(90, 50, 100, 4);
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        { type: 'sequence', samples: turtle.path, terminate: true }
      ], this.fakeClock, LongpressModel);

      let completion = executor();
      const modelMatcher = await modelMatcherPromise;

      await Promise.race([completion, modelMatcher.promise]);

      // Copies from the longpress unit test to find a good spot to split the path.
      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

      // And now for the real meat of the test.

      // Starts a new matcher for a followup gesture component/link, which has a number of fun
      // intentional side-effects.
      const secondMatcher = new gestures.matchers.GestureMatcher<string>(dummyInheritanceMatcher('reject'), modelMatcher);
      // There's only the one touchpoint, so there's no need for synchronization overhead here.
      sources[0].path.on('step', () => secondMatcher.update());
      sources[0].path.on('complete', () => {
        secondMatcher.update();
      });


      // 'reject' path inheritance should instantly resolve.
      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatusModule.PROMISE_RESOLVED);
      assert.isFalse((await secondMatcher.promise).matched);
    });

    it("'full'", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      });
      turtle.wait(MainLongpressSourceModel.timer.duration, 25);
      turtle.hoveredItem = 'b'; // A distinct aspect of 'chop' behavior.
      const waitCompletionSample = turtle.commitPending();
      turtle.move(90, 50, 100, 4);
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        { type: 'sequence', samples: turtle.path, terminate: true }
      ], this.fakeClock, LongpressModel);

      let completion = executor();
      const modelMatcher = await modelMatcherPromise;

      await Promise.race([completion, modelMatcher.promise]);

      // Copies from the longpress unit test to find a good spot to split the path.
      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

      // And now for the real meat of the test.

      // Starts a new matcher for a followup gesture component/link, which has a number of fun
      // intentional side-effects.
      const secondMatcher = new gestures.matchers.GestureMatcher<string>(dummyInheritanceMatcher('full'), modelMatcher);
      // There's only the one touchpoint, so there's no need for synchronization overhead here.
      sources[0].path.on('step', () => secondMatcher.update());
      sources[0].path.on('complete', () => {
        secondMatcher.update();
      });


      // 'full' path inheritance maintains all pre-existing path components...
      assert.equal(secondMatcher.sources[0].path.stats.sampleCount, sources[0].path.stats.sampleCount);

      assert.deepEqual(
        (secondMatcher.sources[0].path as GestureDebugPath<string>).coords,
        (sources[0].path as GestureDebugPath<string>).coords
      );

      assert.deepEqual(sources[0].currentSample, waitCompletionSample);
      // ... and also preserves the original base item.
      assert.equal(sources[0].baseItem, 'a');

      const firstMatcherStats = modelMatcher.sources[0].path.stats;
      assert.equal(firstMatcherStats.duration, MainLongpressSourceModel.timer.duration);
      assert.equal(firstMatcherStats.rawDistance, 0);

      assert.equal(sources[0].path.stats.duration, MainLongpressSourceModel.timer.duration);
      assert.equal(secondMatcher.sources[0].path.stats.duration, sources[0].path.stats.duration);

      await completion;

      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      const secondMatcherStats = secondMatcher.sources[0].path.stats;
      assert.equal(secondMatcherStats.duration, MainLongpressSourceModel.timer.duration + 100);
      assert.closeTo(secondMatcherStats.netDistance, 50, 1e-6);
      assert.equal(secondMatcherStats.cardinalDirection, 'e');
    });
  });

  describe("Flicks", function() {
    describe("Initial stage", function() {
      it("resolve: threshold crossed", async function() {
        const turtle = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        turtle.move(45, FlickStartThreshold+1, 40, 2);
        turtle.commitPending();

        const {
          sources,
          modelMatcherPromise,
          executor
        } = simulateMultiSourceMatcherInput([
          { type: 'sequence', samples: turtle.path, terminate: false }
        ], this.fakeClock, FlickStartModel);

        let completion = executor();
        const modelMatcher = await modelMatcherPromise;
        await Promise.race([completion, modelMatcher.promise]);

        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        assert.deepEqual(await modelMatcher.promise, {matched: true, action: { type: 'chain', item: null, next: 'flick-end'}});
        assert.isFalse(sources[0].path.isComplete);

        const finalStats = modelMatcher.sources[0].path.stats;
        assert.isAtLeast(finalStats.netDistance, FlickStartThreshold);

        // Allow the rest of the simulation to play out; it's easy cleanup that way.
        await completion;
      });
    });

    describe("Final stage", function() {
      it("resolve: threshold crossed", async function() {
        const baseTurtle = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        baseTurtle.move(45, FlickStartThreshold+1, 40, 2);
        baseTurtle.move(45, FlickEndThreshold - FlickStartThreshold, 160, 8);
        baseTurtle.hoveredItem = 'a+';
        baseTurtle.commitPending();

        const {
          sources,
          modelMatcherPromise,
          executor
        } = simulateMultiSourceMatcherInput([
          { type: 'sequence', samples: baseTurtle.path, preplayCount: 3, terminate: true }
        ], this.fakeClock, FlickEndModel);

        let completion = executor();
        const modelMatcher = await modelMatcherPromise;
        await Promise.race([completion, modelMatcher.promise]);

        assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        assert.deepEqual(await modelMatcher.promise, {matched: true, action: { type: 'complete', item: 'a+'}});
        assert.isTrue(sources[0].path.isComplete);

        const finalStats = modelMatcher.sources[0].path.stats;
        assert.isAtLeast(finalStats.netDistance, FlickEndThreshold);

        // Allow the rest of the simulation to play out; it's easy cleanup that way.
        await completion;
      });
    });
  });

  describe("Longpress", function() {
    it("resolve: path not completed (long wait)", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      });
      turtle.wait(1000, 50);
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        { type: 'sequence', samples: turtle.path, terminate: false }
      ], this.fakeClock, LongpressModel);

      let completion = executor();
      const modelMatcher = await modelMatcherPromise;
      await Promise.race([completion, modelMatcher.promise]);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

      assert.deepEqual(await modelMatcher.promise, {matched: true, action: { type: 'chain', item: null, selectionMode: 'none', next: 'subkey-select'}});
      assert.isFalse(sources[0].path.isComplete);

      // Did we resolve at the expected point in the path - once the timer duration had passed?
      assert.isAtLeast(sources[0].currentSample.t, turtle.path[0].t + MainLongpressSourceModel.timer.duration - 1);

      const finalStats = modelMatcher.sources[0].path.stats;
      assert.isAtLeast(finalStats.duration, MainLongpressSourceModel.timer.duration - 1);
      assert.equal(finalStats.rawDistance, 0)

      // Allow the rest of the simulation to play out; it's easy cleanup that way.
      await completion;
    });

    it("reject: distance moved", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      });
      turtle.move(45, 16, 240, 12);
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        { type: 'sequence', samples: turtle.path, terminate: false }
      ], this.fakeClock, LongpressModel);


      let completion = executor();
      const modelMatcher = await modelMatcherPromise;
      await Promise.race([completion, modelMatcher.promise]);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);
      assert.deepEqual(await modelMatcher.promise, {matched: false, action: { type: 'replace', item: null, replace: 'longpress'}});
      assert.isFalse(sources[0].path.isComplete);

      const dist = (sample1: InputSample<any>, sample2: InputSample<any>) => {
        const deltaX = sample1.targetX - sample2.targetX;
        const deltaY = sample1.targetY - sample2.targetY;

        return Math.sqrt(deltaX * deltaX + deltaY * deltaY);
      };

      assert.isAtLeast(dist(sources[0].currentSample, turtle.path[0]), LongpressDistanceThreshold);

      // Allow the rest of the simulation to play out; it's easy cleanup that way.
      await completion;
    });

    it("reject: item changed", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      });
      turtle.move(45, 2, 40, 2);
      turtle.hoveredItem = 'b'; // updates the 'item' entry for the 'pending' sample

      // Commit it now so to gain a reference to the sample we can use later for comparison
      // within this unit test.
      const transitionSample = turtle.commitPending();
      turtle.move(45, 2, 40, 2);
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        { type: 'sequence', samples: turtle.path, terminate: false }
      ], this.fakeClock, LongpressModel);

      let completion = executor();
      const modelMatcher = await modelMatcherPromise;
      await Promise.race([completion, modelMatcher.promise]);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);
      assert.deepEqual(await modelMatcher.promise, {matched: false, action: { type: 'replace', item: null, replace: 'longpress'}});
      assert.isFalse(sources[0].path.isComplete);

      // The sample at which the item changed from 'a' to 'b'.
      assert.deepEqual(sources[0].currentSample, transitionSample);

      await completion;
    });
  });

  describe("Multitap", function() {
    it("reject: pre-existing path", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 0,
        targetY: 0,
        t: 0,
        item: 'a'
      });
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([{type: 'sequence', samples: turtle.path, terminate: false}], this.fakeClock, MultitapModel);

      await executor();
      const modelMatcher = await modelMatcherPromise;

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {matched: false, action: { type: 'none', item: null }});
      assert.isFalse(sources[0].path.isComplete);
    });

    it("resolve: touch after 100ms, release after 300ms", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 0,
        targetY: 0,
        t: 200,
        item: 'a'
      });
      turtle.wait(200, 10);
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        {
          type: 'timer',
          lastSample: {
            targetX: 0,
            targetY: 0,
            t: 100,
            item: 'a'
          }
        },
        {
          type: 'sequence', samples: turtle.path, terminate: true
        }
      ], this.fakeClock, MultitapModel);

      await executor();
      const modelMatcher = await modelMatcherPromise;

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {matched: true, action: { type: 'chain', item: 'a', next: 'multitap' }});
      // touchpoints[0] - a pre-completed path.
      assert.isTrue(sources[1].path.isComplete);
    });

    it("reject: touch after 600ms (threshold = 500ms)", async function() {
      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        {
          type: 'timer',
          lastSample: {

            targetX: 0,
            targetY: 0,
            t: 100,
            item: 'a'
          }
        },
        {
          type: 'sequence', samples: [{
            targetX: 0,
            targetY: 0,
            t: 700,
            item: 'a'
          }], terminate: false
        }
      ], this.fakeClock, MultitapModel);

      await executor();
      const modelMatcher = await modelMatcherPromise;

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {matched: false, action: { type: 'none', item: null }});
      assert.isFalse(sources[1].path.isComplete);
    });

    it("pending: touchstart within sustain timer, but no touchend", async function() {
      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        {
          type: 'timer',
          lastSample: {
            targetX: 0,
            targetY: 0,
            t: 100,
            item: 'a'
          }
        },
        {
          type: 'sequence', samples: [{
            targetX: 0,
            targetY: 0,
            t: 200,
            item: 'a'  // is 'failing' with the same item!?
          }], terminate: false
        }
      ], this.fakeClock, MultitapModel);

      await executor();
      const modelMatcher = await modelMatcherPromise;

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_PENDING);
      assert.isFalse(sources[1].path.isComplete);

      // Yet to be determined:  if the touchpoint is held until after the sustain timer passes,
      // should we reject the multitap?  A valid 'touchstart' will have occurred, but not the
      // 'touchend'.
    });

    it("reject: different base item", async function() {
      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        {
          type: 'timer',
          lastSample: {
            targetX: 0,
            targetY: 0,
            t: 100,
            item: 'a'
          }
        },
        {
          type: 'sequence', samples: [{
            targetX: 0,
            targetY: 0,
            t: 200,
            item: 'b'  // is 'failing' with the same item!?
          }], terminate: false
        }
      ], this.fakeClock, MultitapModel);

      await executor();
      const modelMatcher = await modelMatcherPromise;

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {matched: false, action: { type: 'none', item: null }});
      assert.isFalse(sources[1].path.isComplete);
    });

    it("resolve: second contact-point", async function() {
      const turtle1 = new TouchpathTurtle({
        targetX: 0,
        targetY: 0,
        t: 100,
        item: 'a'
      });

      turtle1.wait(100, 5);
      turtle1.commitPending();

      const turtle2 = new TouchpathTurtle({
        targetX: 50,
        targetY: 0,
        t: 200,
        item: 'b'
      });
      turtle2.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        {
          type: 'timer',
          lastSample: {
            targetX: 0,
            targetY: 0,
            t: 0,
            item: 'a'
          }
        }, {
          type: 'sequence', samples: turtle1.path, terminate: false,
        }, {
          type: 'sequence', samples: turtle2.path, terminate: false
        }
      ], this.fakeClock, MultitapModel);

      await executor();
      const modelMatcher = await modelMatcherPromise;

      if(await promiseStatus(modelMatcher.promise) == PromiseStatusModule.PROMISE_REJECTED) {
        await modelMatcher.promise;
      }
      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);

      assert.deepEqual(await modelMatcher.promise, {matched: true, action: { type: 'chain', item: 'a', next: 'multitap'}});
      modelMatcher.finalizeSources();
      assert.isTrue(sources[1].path.isComplete);

      // Design note:  as this one is _not_ complete, when gesture chaining tries to do a followup multitap match,
      // it will fail.  Same mechanism as the "Pre-existing route" test.
      assert.isFalse(sources[2].path.isComplete);
    });
  });

  describe("Simple tap", function() {
    it("resolve: single path, touch release", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 0,
        targetY: 0,
        t: 0,
        item: 'a'
      });

      turtle.wait(100, 5);
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([{type: 'sequence', samples: turtle.path, terminate: true}], this.fakeClock, SimpleTapModel);

      await executor();
      const modelMatcher = await modelMatcherPromise;

      if(await promiseStatus(modelMatcher.promise) == PromiseStatusModule.PROMISE_REJECTED) {
        await modelMatcher.promise;
      }
      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {matched: true, action: { type: 'chain', item: 'a', next: 'multitap'}});
      assert.isTrue(sources[0].path.isComplete);

      const finalStats = modelMatcher.sources[0].path.stats;
      assert.isAtLeast(finalStats.duration, 100);
      assert.equal(finalStats.rawDistance, 0)
    });

    it("resolve: second contact-point", async function() {
      const turtle1 = new TouchpathTurtle({
        targetX: 0,
        targetY: 0,
        t: 0,
        item: 'a'
      });

      turtle1.wait(100, 5);
      turtle1.commitPending();

      const turtle2 = new TouchpathTurtle({
        targetX: 50,
        targetY: 0,
        t: 100,
        item: 'b'
      });
      turtle2.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([{
        type: 'sequence', samples: turtle1.path, terminate: false,
      }, {
        type: 'sequence', samples: turtle2.path, terminate: false
      }], this.fakeClock, SimpleTapModel);

      await executor();
      const modelMatcher = await modelMatcherPromise;

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);

      assert.deepEqual(await modelMatcher.promise, {matched: true, action: { type: 'chain', item: 'a', next: 'multitap'}});
      modelMatcher.finalizeSources();
      assert.isTrue(sources[0].path.isComplete);
      assert.isFalse(sources[1].path.isComplete);
    });
  });

  describe("Subkey selection", function() {
    it("resolved: subkey hovered", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      });
      turtle.wait(MainLongpressSourceModel.timer.duration, 25);
      turtle.hoveredItem = null;
      turtle.commitPending();
      turtle.move(45, 50, 100, 4);
      turtle.hoveredItem = 'b';
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        { type: 'sequence', samples: turtle.path, terminate: true }
      ], this.fakeClock, LongpressModel);

      let completion = executor();
      const modelMatcher = await modelMatcherPromise;

      await Promise.race([completion, modelMatcher.promise]);

      // Copies from the longpress unit test to find a good spot to split the path.
      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

      // And now for the real meat of the test.

      // Starts a new matcher for a followup gesture component/link, which has a number of fun
      // intentional side-effects.
      const secondMatcher = new gestures.matchers.GestureMatcher<string>(SubkeySelectModel, modelMatcher);
      // There's only the one touchpoint, so there's no need for synchronization overhead here.
      sources[0].path.on('step', () => secondMatcher.update());
      sources[0].path.on('complete', () => secondMatcher.update());
      sources[0].path.on('invalidated', () => secondMatcher.update());

      // With the followup matcher now fully constructed & connected, play the rest of the sequence out.
      await completion;

      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await secondMatcher.promise, { matched: true, action: { type: 'complete', item: 'b' } });
      assert.isTrue(sources[0].path.isComplete);
    });

    it("rejected: path cancelled", async function() {
      const turtle = new TouchpathTurtle({
        targetX: 1,
        targetY: 1,
        t: 100,
        item: 'a'
      });
      turtle.wait(MainLongpressSourceModel.timer.duration, 25);
      turtle.hoveredItem = null;
      turtle.commitPending();
      turtle.move(45, 50, 100, 4);
      turtle.hoveredItem = 'b';
      turtle.commitPending();

      const {
        sources,
        modelMatcherPromise,
        executor
      } = simulateMultiSourceMatcherInput([
        { type: 'sequence', samples: turtle.path, terminate: false }
      ], this.fakeClock, LongpressModel);

      let completion = executor();
      const modelMatcher = await modelMatcherPromise;

      await Promise.race([completion, modelMatcher.promise]);

      // Copies from the longpress unit test to find a good spot to split the path.
      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

      // And now for the real meat of the test.

      // Starts a new matcher for a followup gesture component/link, which has a number of fun
      // intentional side-effects.
      const secondMatcher = new gestures.matchers.GestureMatcher<string>(SubkeySelectModel, modelMatcher);
      // There's only the one touchpoint, so there's no need for synchronization overhead here.
      sources[0].path.on('step', () => secondMatcher.update());
      sources[0].path.on('complete', () => secondMatcher.update());
      sources[0].path.on('invalidated', () => secondMatcher.update());

      // With the followup matcher now fully constructed & connected, play the rest of the sequence out.
      await completion;

      // Manually cancel.
      sources[0].terminate(true);
      secondMatcher.update();
      await Promise.resolve(); // let any Promise followups shake out before continuing.

      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await secondMatcher.promise, { matched: false, action: { type: 'none', item: null } });
      assert.isTrue(sources[0].path.isComplete);
    });
  });
});