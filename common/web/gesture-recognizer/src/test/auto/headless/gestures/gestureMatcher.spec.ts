import { assert } from 'chai'
import sinon from 'sinon';

import * as PromiseStatusModule from 'promise-status-async';
const promiseStatus       = PromiseStatusModule.promiseStatus;
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;

import { ComplexGestureSource, InputSample, SimpleGestureSource, gestures } from '@keymanapp/gesture-recognizer';

import { TouchpathTurtle } from '#tools';
import { ManagedPromise, timedPromise } from '@keymanapp/web-utils';

import {
  LongpressModel,
  MultitapModel,
  SimpleTapModel,
  SubkeySelectModel
} from './isolatedGestureSpecs.js';
import { GestureMatcher } from '../../../../../build/obj/headless/gestures/matchers/gestureMatcher.js';

import {
  LongpressDistanceThreshold,
  MainLongpressSourceModel
} from './isolatedPathSpecs.js';

interface SimSpecSequence<Type> {
  type: 'sequence',
  samples: InputSample<Type>[],
  terminate: boolean
}

interface SimSpecTimer<Type> {
  type: 'timer',
  lastSample: InputSample<Type>
}

interface MockedPredecessor<Type> {
  comparisonStandard: {
    sample: InputSample<Type>,
    baseItem: Type
  },
  pathMatchers: GestureMatcher<Type>[],
  _result: {
    action: {
      item: Type
    }
  }
}

type PathInheritanceType = gestures.specs.ContactModel<string>['pathInheritance'];
function dummyInheritanceMatcher(inheritanceType: PathInheritanceType): gestures.specs.GestureModel<string> {
  return {
    id: "dummy",
    itemPriority: 0,
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

function mockedPredecessor<Type>(
  lastSample: InputSample<Type>,
  baseItem?: Type
): MockedPredecessor<Type> {
  return {
    comparisonStandard: {
      sample: lastSample,
      baseItem: baseItem ?? lastSample.item
    },
    pathMatchers: [],
    _result: {
      action: {
        item: baseItem ?? lastSample.item
      }
    }
  };
}

function simulateComplexGestureSource<Type>(
  sequences: (SimSpecTimer<Type> | SimSpecSequence<Type>)[],
  fakeClock: sinon.SinonFakeTimers,
  modelSpec: gestures.specs.GestureModel<Type>
  ): {
    source: ComplexGestureSource<Type>,
    modelMatcherPromise: Promise<gestures.matchers.GestureMatcher<Type>>,
    executor: () => Promise<void>
  } {

  if(sequences.length == 0) {
    return;
  }

  const firstEntry = sequences[0];
  const startTimestamp = firstEntry.type == 'timer' ? firstEntry.lastSample.t : firstEntry.samples[0].t;
  // Expectation (that should probably be an assertion) - the first entry in the input should hold the
  // earliest timestamp.

  let allPromises: Promise<void>[] = [];

  let source: ComplexGestureSource<Type>;
  let contacts: SimpleGestureSource<Type>[] = [];
  let modelMatcher: gestures.matchers.GestureMatcher<Type>;
  let modelMatcherPromise = new ManagedPromise<gestures.matchers.GestureMatcher<Type>>();

  // Build Promises for these first; that way, new contacts are connected before any new samples
  // for already-existing touchpaths are observed, facilitating synchronization of coordinate
  // sample updates.
  //
  // We do accept the _first_ coordinate for a sample, as this is necessary for accepting
  // new contact points for some gesture types (such as multitap).  Previously-existing touchpaths
  // will gain their corresponding update in a Promise created later, thus coming second.
  // (If it came first, that update would be desynced with the initial sample for the new path.)
  for(let i = 0; i < sequences.length; i++) {
    const simpleSource = new SimpleGestureSource<Type>(i, true);
    contacts.push(simpleSource);
    let entry = sequences[i];

    if(i == 0) {
      source = new ComplexGestureSource(simpleSource);
      timedPromise(0).then(() => {
        let predecessor: GestureMatcher<Type>;
        if(entry.type == 'timer') {
          // We're simulating a previously-completed contact point's path..
          // Preserve the supposed 'last sample' from that previous contact point.
          simpleSource.update(entry.lastSample);
          simpleSource.terminate(false);

          predecessor = mockedPredecessor(entry.lastSample) as any as GestureMatcher<Type>;
        }

        // The final parameter mocks a previous match attempt for the same ComplexGestureSource.
        modelMatcher = new gestures.matchers.GestureMatcher<Type>(modelSpec, predecessor || source);
        modelMatcherPromise.resolve(modelMatcher);
      });
    } else {
      if(entry.type != 'sequence') {
        throw new Error("Only the first entry for complex gesture input simulation may be a timer.");
      } else {
        timedPromise(entry.samples[0].t - startTimestamp).then(() => {
          // Acceptance of new contact points requires an existing sample.
          simpleSource.update((entry as SimSpecSequence<Type>).samples[0]);
          source.addTouchpoint(simpleSource);
          modelMatcher.addContact(simpleSource);
        });
      }
    }
  }

  // Now that touchpath-creation Promises are registered first, we can start adding in the updates for
  // already-existing sequences.  Any gesture-management updates based on path will check for timestamp
  // synchronization across all constituent paths / tracked contact points.  (Having the 'new contacts'
  // registered first is necessary for including them in the synchronization check.)
  for(let i = 0; i < sequences.length; i++) {
    const sequenceSpec = sequences[i];
    if(sequenceSpec.type != 'sequence') {
      continue;
    }

    const simpleSource = contacts[i];

    const sequence = sequenceSpec.samples;
    const sequencePromises = sequence.map((sample) => {
      return timedPromise(sample.t - startTimestamp).then(async () => {
        // We already committed the sample early, to facilitate new contact-point acceptance,
        // so we skip re-adding it here.  We DO allow all other update functionality to
        // proceed as normal, though.
        if(simpleSource.path.coords[0] != sample && !simpleSource.isPathComplete) {
          simpleSource.update(sample);
        }

        // Includes the synchronization check.
        modelMatcher.update();

        // All path updates for synchronization have Promises predating this one.
        // This ensures those are all processed before we move on to default handling of the path
        // for cases that don't otherwise look at path termination.
        await Promise.resolve();

        if((sequenceSpec.terminate ?? true) && sample == sequence[sequence.length-1]) {
          if(!simpleSource.isPathComplete) {
            simpleSource.terminate(false);
            modelMatcher.update();
          }
        }
      });
    });

    allPromises = allPromises.concat(sequencePromises);
  }

  const executor = async () => {
    // Runs until the last already-scheduled timer.  We haven't actually built the
    // GestureMatcher instance yet - only for the actual path observations.
    // Thus, it's possible for the simulation to end before a 'sustain timer' elapses.
    await fakeClock.runToLastAsync();
    // In case of unexpected errors during sample or cancel simulation.
    await Promise.all(allPromises);
  }

  return {
    source: source,
    modelMatcherPromise: modelMatcherPromise.corePromise,
    executor: executor
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      source.touchpoints[0].path.on('step', () => secondMatcher.update());
      source.touchpoints[0].path.on('complete', () => {
        secondMatcher.update();
      });


      // Because 'chopped'.
      assert.equal(secondMatcher.pathMatchers[0].source.path.coords.length, 1);
      assert.deepEqual(source.touchpoints[0].currentSample, waitCompletionSample);
      // because we 'chopped' the path, we use the current sample's item as the new base.
      assert.equal(secondMatcher.pathMatchers[0].source.baseItem, 'b');
      // This technically does affect what the first `modelMatcher` would see as the base item, but its Promise
      // is already fulfilled - its effects are already fully committed.

      const firstMatcherStats = modelMatcher.pathMatchers[0].stats;
      assert.equal(firstMatcherStats.duration, MainLongpressSourceModel.timer.duration);
      assert.equal(firstMatcherStats.rawDistance, 0);

      // subview
      assert.equal(secondMatcher.pathMatchers[0].source.path.stats.duration, 0);
      // original
      assert.equal(source.touchpoints[0].path.stats.duration, MainLongpressSourceModel.timer.duration);

      await completion;

      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      const secondMatcherStats = secondMatcher.pathMatchers[0].stats;
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      source.touchpoints[0].path.on('step', () => secondMatcher.update());
      source.touchpoints[0].path.on('complete', () => {
        secondMatcher.update();
      });


      // 'partial' path inheritance still drops the pre-existing path components...
      assert.equal(secondMatcher.pathMatchers[0].source.path.coords.length, 1);
      assert.deepEqual(source.touchpoints[0].currentSample, waitCompletionSample);
      // ... but preserves the original base item.
      assert.equal(source.touchpoints[0].baseItem, 'a');

      // The rest of what's below should match the assertions for the 'chop' path.
      const firstMatcherStats = modelMatcher.pathMatchers[0].stats;
      assert.equal(firstMatcherStats.duration, MainLongpressSourceModel.timer.duration);
      assert.equal(firstMatcherStats.rawDistance, 0);

      assert.equal(secondMatcher.pathMatchers[0].source.path.stats.duration, 0);
      assert.equal(source.touchpoints[0].path.stats.duration, MainLongpressSourceModel.timer.duration);

      await completion;

      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      const secondMatcherStats = secondMatcher.pathMatchers[0].stats;
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      source.touchpoints[0].path.on('step', () => secondMatcher.update());
      source.touchpoints[0].path.on('complete', () => {
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      source.touchpoints[0].path.on('step', () => secondMatcher.update());
      source.touchpoints[0].path.on('complete', () => {
        secondMatcher.update();
      });


      // 'full' path inheritance maintains all pre-existing path components...
      assert.equal(secondMatcher.pathMatchers[0].source.path.coords.length, source.touchpoints[0].path.coords.length);
      assert.deepEqual(secondMatcher.pathMatchers[0].source.path.coords, source.touchpoints[0].path.coords)
      assert.deepEqual(source.touchpoints[0].currentSample, waitCompletionSample);
      // ... and also preserves the original base item.
      assert.equal(source.touchpoints[0].baseItem, 'a');

      const firstMatcherStats = modelMatcher.pathMatchers[0].stats;
      assert.equal(firstMatcherStats.duration, MainLongpressSourceModel.timer.duration);
      assert.equal(firstMatcherStats.rawDistance, 0);

      assert.equal(source.touchpoints[0].path.stats.duration, MainLongpressSourceModel.timer.duration);
      assert.equal(secondMatcher.pathMatchers[0].source.path.stats.duration, source.touchpoints[0].path.stats.duration);

      await completion;

      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      const secondMatcherStats = secondMatcher.pathMatchers[0].stats;
      assert.equal(secondMatcherStats.duration, MainLongpressSourceModel.timer.duration + 100);
      assert.closeTo(secondMatcherStats.netDistance, 50, 1e-6);
      assert.equal(secondMatcherStats.cardinalDirection, 'e');
    });
  });

  describe.skip("Flicks", function() {
    it("Actual expectations for flick behavior still in flux - cannot spec yet", async function() {

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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
        { type: 'sequence', samples: turtle.path, terminate: false }
      ], this.fakeClock, LongpressModel);

      let completion = executor();
      const modelMatcher = await modelMatcherPromise;
      await Promise.race([completion, modelMatcher.promise]);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

      assert.deepEqual(await modelMatcher.promise, {matched: true, action: { type: 'chain', item: null, next: 'subkeyselect'}});
      assert.isFalse(source.touchpoints[0].path.isComplete);

      // Did we resolve at the expected point in the path - once the timer duration had passed?
      assert.isAtLeast(source.touchpoints[0].currentSample.t, turtle.path[0].t + MainLongpressSourceModel.timer.duration - 1);

      const finalStats = modelMatcher.pathMatchers[0].stats;
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
        { type: 'sequence', samples: turtle.path, terminate: false }
      ], this.fakeClock, LongpressModel);


      let completion = executor();
      const modelMatcher = await modelMatcherPromise;
      await Promise.race([completion, modelMatcher.promise]);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);
      assert.deepEqual(await modelMatcher.promise, {matched: false, action: { type: 'optional-chain', item: null, allowNext: 'longpress'}});
      assert.isFalse(source.touchpoints[0].path.isComplete);

      const dist = (sample1: InputSample<any>, sample2: InputSample<any>) => {
        const deltaX = sample1.targetX - sample2.targetX;
        const deltaY = sample1.targetY - sample2.targetY;

        return Math.sqrt(deltaX * deltaX + deltaY * deltaY);
      };

      assert.isAtLeast(dist(source.touchpoints[0].currentSample, turtle.path[0]), LongpressDistanceThreshold);

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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
        { type: 'sequence', samples: turtle.path, terminate: false }
      ], this.fakeClock, LongpressModel);

      let completion = executor();
      const modelMatcher = await modelMatcherPromise;
      await Promise.race([completion, modelMatcher.promise]);

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);
      assert.deepEqual(await modelMatcher.promise, {matched: false, action: { type: 'optional-chain', item: null, allowNext: 'longpress'}});
      assert.isFalse(source.touchpoints[0].path.isComplete);

      // The sample at which the item changed from 'a' to 'b'.
      assert.deepEqual(source.touchpoints[0].currentSample, transitionSample);

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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([{type: 'sequence', samples: turtle.path, terminate: false}], this.fakeClock, MultitapModel);

      await executor();
      const modelMatcher = await modelMatcherPromise;

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {matched: false, action: { type: 'none', item: null }});
      assert.isFalse(source.touchpoints[0].path.isComplete);
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      assert.isTrue(source.touchpoints[1].path.isComplete);
    });

    it("reject: touch after 600ms (threshold = 500ms)", async function() {
      const {
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      assert.isFalse(source.touchpoints[1].path.isComplete);
    });

    it("pending: touchstart within sustain timer, but no touchend", async function() {
      const {
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      assert.isFalse(source.touchpoints[1].path.isComplete);

      // Yet to be determined:  if the touchpoint is held until after the sustain timer passes,
      // should we reject the multitap?  A valid 'touchstart' will have occurred, but not the
      // 'touchend'.
    });

    it("reject: different base item", async function() {
      const {
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      assert.isFalse(source.touchpoints[1].path.isComplete);
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      assert.isTrue(source.touchpoints[1].path.isComplete);

      // Design note:  as this one is _not_ complete, when gesture chaining tries to do a followup multitap match,
      // it will fail.  Same mechanism as the "Pre-existing route" test.
      assert.isFalse(source.touchpoints[2].path.isComplete);
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([{type: 'sequence', samples: turtle.path, terminate: true}], this.fakeClock, SimpleTapModel);

      await executor();
      const modelMatcher = await modelMatcherPromise;

      if(await promiseStatus(modelMatcher.promise) == PromiseStatusModule.PROMISE_REJECTED) {
        await modelMatcher.promise;
      }
      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await modelMatcher.promise, {matched: true, action: { type: 'optional-chain', item: 'a', allowNext: 'multitap'}});
      assert.isTrue(source.touchpoints[0].path.isComplete);

      const finalStats = modelMatcher.pathMatchers[0].stats;
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([{
        type: 'sequence', samples: turtle1.path, terminate: false,
      }, {
        type: 'sequence', samples: turtle2.path, terminate: false
      }], this.fakeClock, SimpleTapModel);

      await executor();
      const modelMatcher = await modelMatcherPromise;

      assert.equal(await promiseStatus(modelMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);

      assert.deepEqual(await modelMatcher.promise, {matched: true, action: { type: 'optional-chain', item: 'a', allowNext: 'multitap'}});
      assert.isTrue(source.touchpoints[0].path.isComplete);
      assert.isFalse(source.touchpoints[1].path.isComplete);
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      source.touchpoints[0].path.on('step', () => secondMatcher.update());
      source.touchpoints[0].path.on('complete', () => secondMatcher.update());
      source.touchpoints[0].path.on('invalidated', () => secondMatcher.update());

      // With the followup matcher now fully constructed & connected, play the rest of the sequence out.
      await completion;

      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await secondMatcher.promise, { matched: true, action: { type: 'complete', item: 'b' } });
      assert.isTrue(source.touchpoints[0].path.isComplete);
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      source.touchpoints[0].path.on('step', () => secondMatcher.update());
      source.touchpoints[0].path.on('complete', () => secondMatcher.update());
      source.touchpoints[0].path.on('invalidated', () => secondMatcher.update());

      // With the followup matcher now fully constructed & connected, play the rest of the sequence out.
      await completion;

      // Manually cancel.
      source.touchpoints[0].terminate(true);
      secondMatcher.update();
      await Promise.resolve(); // let any Promise followups shake out before continuing.

      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await secondMatcher.promise, { matched: false, action: { type: 'none', item: null } });
      assert.isTrue(source.touchpoints[0].path.isComplete);
    });

    it("rejected: extra touch detected", async function() {
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
        source,
        modelMatcherPromise,
        executor
      } = simulateComplexGestureSource([
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
      source.touchpoints[0].path.on('step', () => secondMatcher.update());
      source.touchpoints[0].path.on('complete', () => secondMatcher.update());
      source.touchpoints[0].path.on('invalidated', () => secondMatcher.update());

      timedPromise(50).then(() => {
        const secondContact = new SimpleGestureSource<string>(5, true);
        source.addTouchpoint(secondContact);
        secondMatcher.addContact(secondContact);
        secondContact.update({
          targetX: 50,
          targetY: 50,
          t: 100 + MainLongpressSourceModel.timer.duration + 50,
          item: 'c'
        });
      });

      // 100ms left to simulate until the end.

      // With the followup matcher now fully constructed & connected, play the rest of the sequence out.
      await completion;

      assert.equal(await promiseStatus(secondMatcher.promise), PromiseStatuses.PROMISE_RESOLVED);
      assert.deepEqual(await secondMatcher.promise, { matched: false, action: { type: 'none', item: null } });
      assert.isTrue(source.touchpoints[0].path.isComplete);
      assert.isTrue(source.touchpoints[0].path.wasCancelled);
    });
  });
});