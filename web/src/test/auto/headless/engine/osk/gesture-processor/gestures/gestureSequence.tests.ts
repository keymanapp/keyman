import { assert } from 'chai'
import sinon from 'sinon';

import * as PromiseStatusModule from 'promise-status-async';
import { assertingPromiseStatus as promiseStatus } from '../../../../../resources/assertingPromiseStatus.js';

import { GestureModelDefs, buildGestureMatchInspector, gestures } from '@keymanapp/gesture-recognizer';
const { matchers } = gestures;

// Huh... gotta do BOTH here?  One for constructor use, the other for generic-parameter use?
const { GestureSequence, MatcherSelector } = matchers;
type GestureSequence<Type> = gestures.matchers.GestureSequence<Type>;
type MatcherSelector<Type> = gestures.matchers.MatcherSelector<Type>;
type MatcherSelection<Type> = gestures.matchers.MatcherSelection<Type>;

const getGestureModelSet = gestures.specs.getGestureModelSet;
const modelSetForAction = gestures.matchers.modelSetForAction;

import { HeadlessInputEngine, TouchpathTurtle } from '#gesture-tools';
import { ManagedPromise, timedPromise } from '@keymanapp/web-utils';

import { assertGestureSequence, SequenceAssertion } from "../../../../../resources/sequenceAssertions.js";

import {
  LongpressModel,
  MultitapModel,
  SimpleTapModel,
  SubkeySelectModel
} from './isolatedGestureSpecs.js';

const TestGestureModelDefinitions: GestureModelDefs<string> = {
  gestures: [
    LongpressModel,
    MultitapModel,
    SimpleTapModel,
    SubkeySelectModel,
    // While modipress should be in the final set, it's not particularly testable without
    // TouchpointCoordinator integration.
  ],
  sets: {
    default: [LongpressModel.id, SimpleTapModel.id],
  }
}

describe("modelSetForAction", function() {
  it('successful longpress', () => {
    const nextModels = modelSetForAction({
      type: 'chain',
      item: null,
      next: 'subkey-select'
    }, TestGestureModelDefinitions, 'default');

    assert.sameMembers(nextModels, [SubkeySelectModel]);
  });

  // A cancelled longpress that is reset is handled by different mechanisms;
  // no test is appropriate in this location.

  it('successful simple-tap or multi-tap', () => {
    const nextModels = modelSetForAction({
      type: 'chain',
      item: 'a',
      next: 'multitap'
    }, TestGestureModelDefinitions, 'default');

    assert.sameMembers(nextModels, [MultitapModel]);
  });

  it('successful subkey-select', () => {
    const nextModels = modelSetForAction({
      type: 'complete',
      item: 'b',
    }, TestGestureModelDefinitions, 'default');

    assert.sameMembers(nextModels, []);
  });
});

let fakeClock: ReturnType<typeof sinon.useFakeTimers>;
async function sequenceEmulationAndAssertion(emulationEngine: HeadlessInputEngine, emulationCompletion: Promise<void>, sequenceAssertions: SequenceAssertion<string>[]) {
  const selectionPromise = new ManagedPromise<MatcherSelection<string>>;
  const testPromise = new ManagedPromise<void>();

  // One selector for ALL sources, not per-source.
  const selector = new MatcherSelector<string>('default');

  // Track pre-built sequences; we need to double-check that new touchpoints don't correspond to existing sequences.
  const sequences: GestureSequence<string>[] = [];
  let indexSeed = 0;

  // Note:  errors from async handlers do not get caught by Mocha if unhandled.
  // The workaround: we build Promises for would-be async handlers that can sync; we pass caught errors
  // to them so that they're reported by the automated test.
  emulationEngine.on('pointstart', async (source) => {
    source.setGestureMatchInspector(buildGestureMatchInspector(selector));

    try {
      // These parts should be handled by TouchpointCoordinator.  This is a simplified mocked version
      // of what lies there.
      const matchPromise = (await selector.matchGesture(source, getGestureModelSet(TestGestureModelDefinitions, 'default'))).selectionPromise;

      matchPromise.then(async (selection) => {
        if(!selectionPromise.isResolved) {
          selectionPromise.resolve(selection);
        }

        // Ensure that existing sequences have a chance to include the new GestureSource before proceeding.
        // (This handler is called synchronously, while the Sequence updates asynchronously.)
        await Promise.resolve();

        if(sequences.find((sequence) => sequence.allSourceIds.find((identifier) => identifier == source.identifier))) {
          // This touchpoint has already been included within an existing GestureSequence.
          return;
        }

        // And that should be enough to spin up a GestureSequence for continuation.
        // The `null` bit is "cheating" a bit, but is "fine" for this test.
        const sequence = new GestureSequence<string>(
          selection,
          TestGestureModelDefinitions,
          selector,
          null // We're 'mocking out' the TouchpointCoordinator.
        );
        sequences.push(sequence);
        const sequenceIndex = indexSeed++;

        const assertion = sequenceAssertions[sequenceIndex];
        if(assertion) {
          try {
            await assertGestureSequence(sequence, emulationCompletion, assertion);
            testPromise.resolve();
          } catch(err) {
            testPromise.reject(err);
          }
        } else {
          testPromise.reject(new Error(`Missing assertion for sequence ${sequenceIndex} of test`));
        }
      });
      selectionPromise.catch((err) => testPromise.reject(err));
    } catch(err) {
      testPromise.reject(err);
      return;
    }
  });

  //fakeClock.runToLastAsync();
  fakeClock.runAllAsync();

  // Assert that an initial 'stage' (component of the sequence) is available - it's needed to
  // build the sequence object.
  await Promise.race([selectionPromise, emulationCompletion]);
  assert.equal(await promiseStatus(selectionPromise.corePromise), PromiseStatusModule.PROMISE_RESOLVED);
  await selectionPromise;

  // Other assertions are embedded in the simulation bit above.

  await testPromise;
}

// TODO(?): right, simulation.  Again.  Yaaaaay.
// - Fortunately, the _start_ can just use selection-sim semantics; the GestureSequence
//   constructor takes in an existing Selector & its selection, after all.
// - in fact... that should be 100% fine, right?  There's only ever the one selector!
//   - the issue:  we do want to 'select' early, before later-stage timers are all run.

// Later, in a different file:  testing TouchpointCoordinator's integration with this.

describe("GestureSequence", function() {
  beforeEach(function() {
    fakeClock = sinon.useFakeTimers();
  });

  afterEach(function() {
    fakeClock.restore();
  });

  // TODO:  author tests for (at least) the following
  // Modipress - but it expects an actual TouchpointCoordinator instance.  May not be testable on this level.
  // Android longpress delegation?  (Though... we _are_ killing this Android aspect, so maybe it's not worth explicitly testing anymore.)
  //
  // Defer: flick test - confirm => execute
  //
  // .on('complete') may be checked to validate if any further match attempts will be possible
  // based on the condition; it may be worth 'extending' tests (via mocked timer) to
  // double-check such scenarios.

  it('longpress -> subkey select', async () => {
    const turtle = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'a'
    });
    turtle.wait(1000, 50);
    turtle.move(0, 10, 100, 5);
    turtle.hoveredItem = 'à';
    turtle.move(90, 10, 100, 5);
    turtle.hoveredItem = 'â';
    turtle.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const completionPromise = emulationEngine.playbackRecording({
      inputs: [ {
        path: {
          coords: turtle.path,
        },
        isFromTouch: true
      }],
      config: null
    });

    const sequenceAssertion: SequenceAssertion<string> = [
      {
        matchedId: 'longpress',
        item: null,
        linkType: 'chain',
        sources: (sources) => {
          // Assert single-source
          assert.equal(sources.length, 1);

          // Assert wait appropriate to the longpress threshold.  Likely won't be the full 1000 ms.
          const pathStats = sources[0].path.stats;
          assert.isAtLeast(pathStats.duration, LongpressModel.contacts[0].model.timer.duration - 1);
          assert.isAtMost(pathStats.rawDistance, 0.1);
          return;
        }
      },
      {
        matchedId: 'subkey-select',
        item: 'â',
        linkType: 'complete',
        sources: (sources) => {
          const pathStats = sources[0].path.stats;
          assert.isAtLeast(pathStats.rawDistance, 19.9);
          assert.isAtLeast(pathStats.duration, 1200 - LongpressModel.contacts[0].model.timer.duration - 2);
        }
      }
    ];

    await sequenceEmulationAndAssertion(emulationEngine, completionPromise, [sequenceAssertion]);

    // simulateSelectorInput should be sufficient for sequence emulation; just capture
    // the first selection once kick-started, then add the "fun" hooks for the rest of the test.
    // We do have to build a selector and complete the first pass to create a Sequence object, after all.
    //
    // For equivalent TouchpointCoordinator auto-tests, the HeadlessInputEngine class should work well.
    // ... it might even be possible here, since we'll always be starting from the 'head' of the
    // GestureSequence for these tests.  (Later starts fall within the domain of MatcherSelector.)
  });

  // Note:  cannot do longpress-blocking of secondary gestures here, since that requires TouchpointCoordinator
  // integration.

  it('a single, standalone simple tap', async () => {
    const turtle = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'a'
    });
    turtle.wait(40, 2);
    turtle.commitPending();


    const emulationEngine = new HeadlessInputEngine();
    const completionPromise = emulationEngine.playbackRecording({
      inputs: [ {
        path: {
          coords: turtle.path
        },
        isFromTouch: true
      }],
      config: null
    }).then(async () => {
      // Ride out the multitap timer so we can achieve full completion.
      let promise = timedPromise(MultitapModel.sustainTimer.duration+1).then(() => {});
      await fakeClock.runToLastAsync();
      await promise;
    });

    const sequenceAssertion: SequenceAssertion<string> = [
      {
        matchedId: 'simple-tap',
        item: 'a',
        linkType: 'chain',
        sources: (sources) => {
          assert.equal(sources.length, 1);
          assert.isTrue(sources[0].isPathComplete);

          // Assert wait appropriate to the longpress threshold.  Likely won't be the full 1000 ms.
          const pathStats = sources[0].path.stats;
          assert.isAtLeast(pathStats.duration, 40);
          return;
        }
      }
    ];

    await sequenceEmulationAndAssertion(emulationEngine, completionPromise, [sequenceAssertion]);
  });

  it('two overlapping simple taps', async () => {
    const turtle0 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'a'
    });
    turtle0.wait(40, 2);
    turtle0.commitPending();

    const turtle1 = new TouchpathTurtle({
      targetX: 101,
      targetY: 101,
      t: 120,
      item: 'b'
    });
    turtle1.wait(40, 2);
    turtle1.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const completionPromise = emulationEngine.playbackRecording({
      inputs: [ {
        path: {
          coords: turtle0.path,
        },
        isFromTouch: true
      }, {
        path: {
          coords: turtle1.path,
        },
        isFromTouch: true
      }],
      config: null
    }).then(async () => {
      // Ride out the multitap timer so we can achieve full completion.
      let promise = timedPromise(MultitapModel.sustainTimer.duration+1).then(() => {});
      await fakeClock.runToLastAsync();
      await promise;
    });

    // The two will be treated as separate sequences.
    const sequenceAssertions: SequenceAssertion<string>[] = [
      [
        {
          matchedId: 'simple-tap',
          item: 'a',
          linkType: 'chain',
          sources: (sources) => {
            // Assert dual-source; the first tap was early-triggered because of the concurrent second tap.
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            // assert.isFalse(sources[1].isPathComplete);

            // Assert wait appropriate to the longpress threshold.  Likely won't be the full 1000 ms.
            const pathStats = sources[0].path.stats;
            assert.isAtMost(pathStats.duration, 21);
            return;
          }
        }
      ], [
        {
          // OK... this one's not happening because it's not an allowed 'next' followup.  Riiiight.
          // Need a way for this to 'fall back' and not be included... might be best to move forward with
          // that 'make it a separate sequence' idea given the spec shift.
          matchedId: 'simple-tap',
          item: 'b',
          linkType: 'chain',
          sources: (sources) => {
            // Assert single-source; the first tap is not under consideration for this stage.
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            const pathStats = sources[0].path.stats;
            assert.isAtMost(pathStats.duration, 40);
          }
        }
      ]
    ];

    await sequenceEmulationAndAssertion(emulationEngine, completionPromise, sequenceAssertions);
  });

  it('2 consecutive simple taps', async () => {
    const turtle0 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'a'
    });
    turtle0.wait(40, 2);
    turtle0.commitPending();

    const turtle1 = new TouchpathTurtle({
      targetX: 11,
      targetY: 11,
      t: 200,
      item: 'b'
    });
    turtle1.wait(40, 2);
    turtle1.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const completionPromise = emulationEngine.playbackRecording({
      inputs: [ {
        path: {
          coords: turtle0.path,
        },
        isFromTouch: true
      }, {
        path: {
          coords: turtle1.path,
        },
        isFromTouch: true
      }],
      config: null
    });

    const sequenceAssertions: SequenceAssertion<string>[] = [
      [
        {
          matchedId: 'simple-tap',
          item: 'a',
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
        // The second one is a separate sequence; no data for it should show up here.
      ],
      // The 'separate sequence'.
      [
        {
          matchedId: 'simple-tap',
          item: 'b',
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
      ]
    ];

    await sequenceEmulationAndAssertion(emulationEngine, completionPromise, sequenceAssertions);
  });

  it('simple tap followed by longpress', async () => {
    const turtle0 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'a'
    });
    turtle0.wait(40, 2);
    turtle0.commitPending();

    const turtle1 = new TouchpathTurtle({
      targetX: 11,
      targetY: 11,
      t: 200,
      item: 'b'
    });
    turtle1.wait(600, 30);
    turtle1.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const completionPromise = emulationEngine.playbackRecording({
      inputs: [ {
        path: {
          coords: turtle0.path,
        },
        isFromTouch: true
      }, {
        path: {
          coords: turtle1.path,
        },
        isFromTouch: true
      }],
      config: null
    });

    const sequenceAssertions: SequenceAssertion<string>[] = [
      [
        {
          matchedId: 'simple-tap',
          item: 'a',
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
        // The second one is a separate sequence; no data for it should show up here.
      ],
      // The 'separate sequence'.
      [
        {
          matchedId: 'longpress',
          item: null,
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isFalse(sources[0].isPathComplete);
            return;
          }
        },
        {
          matchedId: 'subkey-select',
          item: 'b',
          linkType: 'complete',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
      ]
    ];

    await sequenceEmulationAndAssertion(emulationEngine, completionPromise, sequenceAssertions);
  });

  it('basic multitap - 2 taps total', async () => {
    const turtle0 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'a'
    });
    turtle0.wait(40, 2);
    turtle0.commitPending();

    const turtle1 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 200,
      item: 'a'
    });
    turtle1.wait(40, 2);
    turtle1.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const completionPromise = emulationEngine.playbackRecording({
      inputs: [ {
        path: {
          coords: turtle0.path,
        },
        isFromTouch: true
      }, {
        path: {
          coords: turtle1.path,
        },
        isFromTouch: true
      }],
      config: null
    }).then(async () => {
      // Ride out the multitap timer so we can achieve full completion.
      let promise = timedPromise(MultitapModel.sustainTimer.duration+1).then(() => {});
      await fakeClock.runToLastAsync();
      await promise;
    });

    const sequenceAssertion: SequenceAssertion<string> = [
      {
        matchedId: 'simple-tap',
        item: 'a',
        linkType: 'chain',
        sources: (sources) => {
          assert.equal(sources.length, 1);
          assert.isTrue(sources[0].isPathComplete);
          return;
        }
      },
      {
        matchedId: 'multitap',
        item: 'a',
        linkType: 'chain',
        sources: (sources) => {
          // Assert single-source; the first tap is not under consideration for this stage.
          assert.equal(sources.length, 1);
        }
      }
    ];

    await sequenceEmulationAndAssertion(emulationEngine, completionPromise, [sequenceAssertion]);
  });
});