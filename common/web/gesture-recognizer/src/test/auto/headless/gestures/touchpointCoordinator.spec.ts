import { assert } from 'chai'
import sinon from 'sinon';

import * as PromiseStatusModule from 'promise-status-async';
import { assertingPromiseStatus as promiseStatus } from '../../../resources/assertingPromiseStatus.js';

import { GestureModelDefs, GestureSource, gestures, TouchpointCoordinator } from '@keymanapp/gesture-recognizer';
const { matchers } = gestures;

// Huh... gotta do BOTH here?  One for constructor use, the other for generic-parameter use?
const { GestureSequence, GestureStageReport, MatcherSelector } = matchers;
type GestureSequence<Type> = gestures.matchers.GestureSequence<Type>;
type MatcherSelector<Type> = gestures.matchers.MatcherSelector<Type>;
type MatcherSelection<Type> = gestures.matchers.MatcherSelection<Type>;
type GestureStageReport<Type> = gestures.matchers.GestureStageReport<Type>;

import { HeadlessInputEngine, TouchpathTurtle } from '#tools';
import { ManagedPromise, timedPromise } from '@keymanapp/web-utils';

import { assertGestureSequence, SequenceAssertion } from "../../../resources/sequenceAssertions.js";

import {
  LongpressModel,
  ModipressEndModel,
  ModipressStartModel,
  MultitapModel,
  SimpleTapModel,
  SubkeySelectModel
} from './isolatedGestureSpecs.js';
import { PROMISE_PENDING } from 'promise-status-async';

const TestGestureModelDefinitions: GestureModelDefs<string> = {
  gestures: [
    LongpressModel,
    MultitapModel,
    SimpleTapModel,
    SubkeySelectModel,
    ModipressStartModel,
    ModipressEndModel
  ],
  sets: {
    default: [LongpressModel.id, SimpleTapModel.id, ModipressStartModel.id],
    modipress: [LongpressModel.id, SimpleTapModel.id], // no nested modipressing
    none: []
  }
}

let fakeClock: ReturnType<typeof sinon.useFakeTimers>;

describe("TouchpointCoordinator", () => {
  beforeEach(function() {
    fakeClock = sinon.useFakeTimers();
  });

  afterEach(function() {
    fakeClock.restore();
  });

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
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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

    const sequencePromise = new ManagedPromise<void>();
    const sequenceAssertionPromise = new ManagedPromise<void>();
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      try {
        sequencePromise.resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertion);
        sequenceAssertionPromise.resolve();
      } catch(err) {
        sequenceAssertionPromise.reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromise.corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await runnerPromise;

    await sequenceAssertionPromise.corePromise;
    assert.isEmpty(touchpointCoordinator.activeGestures);
  });

  it('longpress -> attempted simple-tap during subkey select', async () => {
    const turtle1 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'a'
    });
    turtle1.wait(1000, 50);
    turtle1.move(0, 10, 100, 5);
    turtle1.hoveredItem = 'à';
    turtle1.move(90, 10, 100, 5);
    turtle1.hoveredItem = 'â';
    turtle1.commitPending();

    const turtle2 = new TouchpathTurtle({
      targetX: 101,
      targetY: 101,
      t: 700,
      item: 'b'
    });
    turtle2.wait(40, 2);
    turtle2.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
    const completionPromise = emulationEngine.playbackRecording({
      inputs: [ {
        path: {
          coords: turtle1.path,
        },
        isFromTouch: true
      }, {
        path: {
          coords: turtle2.path,
        },
        isFromTouch: true
      }],
      config: null
    });

    const sequenceAssertions: SequenceAssertion<string>[] = [
      [
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
      ],
      [ /* there should be no simple-tap here - it should be blocked. */]
    ];

    const sequencePromise = new ManagedPromise<void>();
    const sequenceAssertionPromise = new ManagedPromise<void>();
    let sequenceIndex = 0;
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      try {
        sequencePromise.resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertions[sequenceIndex++]);
        sequenceAssertionPromise.resolve();
      } catch(err) {
        sequenceAssertionPromise.reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromise.corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await runnerPromise;

    await sequenceAssertionPromise.corePromise;
    assert.isEmpty(touchpointCoordinator.activeGestures);
  });

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
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
    const completionPromise = emulationEngine.playbackRecording({
      inputs: [ {
        path: {
          coords: turtle.path,
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

    const sequencePromise = new ManagedPromise<void>();
    const sequenceAssertionPromise = new ManagedPromise<void>();
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      try {
        sequencePromise.resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertion);
        sequenceAssertionPromise.resolve();
      } catch(err) {
        sequenceAssertionPromise.reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromise.corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);
    await runnerPromise;

    await sequenceAssertionPromise.corePromise;
    assert.isEmpty(touchpointCoordinator.activeGestures);
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
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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

    /* The fact that the two simple-taps are treated as part of the same sequence, rather than distinct ones,
     * is a consequence of the existing gesture-modeling infrastructure.  The second tap is what triggers
     * "early completion" of the first tap, thus it's considered part of the same sequence at present.
     *
     * Rough notes toward potential mitigation / fix in the future:
     * // - could be mitigated with a special 'flag' on the gesture-model, perhaps?
     * //   - something to indicate "early-termination second-touchpoint should mark a sequence split-point"
     * // - the GestureSequence class/instance does have a reference to TouchpointCoordinator; it should
     * //     be able to use that reference to facilitate a split if/when appropriate, like here.
     *
     * Obviously having a separate, second sequence would be 'nice', conceptually, for consumers...
     * but I don't think it's worth prioritizing at the moment; got enough else to deal with for now.
     */
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

            // Assert wait appropriate to the longpress threshold.  Likely won't be the full 1000 ms.
            const pathStats = sources[0].path.stats;
            assert.isAtMost(pathStats.duration, 21);
            return;
          }
        }
      ], [
        {
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

    const sequencePromise = new ManagedPromise<void>();
    const sequenceAssertionPromise = new ManagedPromise<void>();
    let sequenceIndex = 0;
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      try {
        sequencePromise.resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertions[sequenceIndex++]);
        sequenceAssertionPromise.resolve();
      } catch(err) {
        sequenceAssertionPromise.reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromise.corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await sequenceAssertionPromise.corePromise;
    await runnerPromise;

    assert.isEmpty(touchpointCoordinator.activeGestures);
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
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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

    const sequencePromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    const sequenceAssertionPromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    let index = 0;
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      const i = index++;

      try {
        sequencePromises[i].resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertions[i]);
        // but this waits on the assertions, which means we don't get the intermediate state stuff this way.
        sequenceAssertionPromises[i].resolve();
      } catch(err) {
        sequenceAssertionPromises[i].reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromises[0].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await sequencePromises[1].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await runnerPromise;

    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await completionPromise;
    await Promise.all(sequenceAssertionPromises);

    assert.isEmpty(touchpointCoordinator.activeGestures);
  });

  it('2 consecutive simple taps (long intermediate delay)', async () => {
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
      t: 1100,
      item: 'a'
    });
    turtle1.wait(40, 2);
    turtle1.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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
          item: 'a',
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
      ]
    ];

    const sequencePromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    const sequenceAssertionPromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    let index = 0;
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      const i = index++;
      try {
        sequencePromises[i].resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertions[i]);
        sequenceAssertionPromises[i].resolve();
      } catch(err) {
        sequenceAssertionPromises[i].reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromises[0].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await sequencePromises[1].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await runnerPromise;

    await completionPromise;
    await sequenceAssertionPromises[1].corePromise;

    assert.isEmpty(touchpointCoordinator.activeGestures);
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
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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

    const sequencePromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    const sequenceAssertionPromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    let index = 0;
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      const i = index++;
      try {
        sequencePromises[i].resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertions[i]);
        sequenceAssertionPromises[i].resolve();
      } catch(err) {
        sequenceAssertionPromises[i].reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromises[0].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await sequencePromises[1].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await runnerPromise;
    await sequenceAssertionPromises[1].corePromise;

    assert.isEmpty(touchpointCoordinator.activeGestures);
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
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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

    const sequencePromise = new ManagedPromise<void>();
    const sequenceAssertionPromise = new ManagedPromise<void>();
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      try {
        sequencePromise.resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertion);
        sequenceAssertionPromise.resolve();
      } catch(err) {
        sequenceAssertionPromise.reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromise.corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await runnerPromise;
    await sequenceAssertionPromise.corePromise;
    assert.isEmpty(touchpointCoordinator.activeGestures);
  });

  it('modipress: one simple tap before its end', async () => {
    const turtle0 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'shift'
    });
    turtle0.wait(180, 9);
    turtle0.commitPending();

    const turtle1 = new TouchpathTurtle({
      targetX: 11,
      targetY: 11,
      t: 200,
      item: 'a'
    });
    turtle1.wait(40, 2);
    turtle1.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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
          matchedId: 'modipress-start',
          item: 'shift',
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isFalse(sources[0].isPathComplete);
            return;
          }
        }, {
          matchedId: 'modipress-end',
          item: null,
          linkType: 'complete',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
      ], [
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
      ]
    ];

    const sequencePromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    const sequenceAssertionPromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    let sequenceIndex = 0;
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      const index = sequenceIndex++;
      try {
        sequencePromises[index].resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertions[index]);
        sequenceAssertionPromises[index].resolve();
      } catch(err) {
        sequenceAssertionPromises[index].reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromises[0].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await sequencePromises[1].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await runnerPromise;

    // index 0 should complete before index 1... when the earlier TODO is fixed.
    await sequenceAssertionPromises[1].corePromise;
    assert.isEmpty(touchpointCoordinator.activeGestures);
  });

  it('modipress: held simple tap at modipress end', async () => {
    const turtle0 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'shift'
    });
    turtle0.wait(120, 6);
    turtle0.commitPending();

    const turtle1 = new TouchpathTurtle({
      targetX: 11,
      targetY: 11,
      t: 200,
      item: 'a'
    });
    turtle1.wait(40, 2);
    turtle1.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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
          matchedId: 'modipress-start',
          item: 'shift',
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isFalse(sources[0].isPathComplete);
            return;
          }
        }, {
          matchedId: 'modipress-end',
          item: null,
          linkType: 'complete',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
      ], [ /* the simple-tap never gets triggered! */ ]
    ];

    const sequencePromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    const sequenceAssertionPromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    let sequenceIndex = 0;
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      const index = sequenceIndex++;
      try {
        sequencePromises[index].resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertions[index]);
        sequenceAssertionPromises[index].resolve();
      } catch(err) {
        sequenceAssertionPromises[index].reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromises[0].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await runnerPromise;

    // index 0 should complete before index 1... when the earlier TODO is fixed.
    await sequenceAssertionPromises[0].corePromise;
    assert.isEmpty(touchpointCoordinator.activeGestures);
  });

  it('modipress: disallows nested modipress attempts', async () => {
    const turtle0 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'shift'
    });
    turtle0.wait(180, 9);
    turtle0.commitPending();

    const turtle1 = new TouchpathTurtle({
      targetX: 11,
      targetY: 11,
      t: 200,
      item: 'alt'
    });
    turtle1.wait(40, 2);
    turtle1.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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
          matchedId: 'modipress-start',
          item: 'shift',
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isFalse(sources[0].isPathComplete);
            return;
          }
        }, {
          matchedId: 'modipress-end',
          item: null,
          linkType: 'complete',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
      ], [
        /* In the test config, there's nothing preventing 'alt' from being
         * considered a legal key for simple taps.  Because of the ongoing
         * shift-modipress, it can't itself be modipressed... and so it
         * shows up like this.
         */
        {
          matchedId: 'simple-tap',
          item: 'alt',
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
      ]
    ];

    const sequencePromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    const sequenceAssertionPromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    let sequenceIndex = 0;
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      const index = sequenceIndex++;
      try {
        sequencePromises[index].resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertions[index]);
        sequenceAssertionPromises[index].resolve();
      } catch(err) {
        sequenceAssertionPromises[index].reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromises[0].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await sequencePromises[1].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await runnerPromise;

    // index 0 should complete before index 1... when the earlier TODO is fixed.
    await sequenceAssertionPromises[1].corePromise;
    assert.isEmpty(touchpointCoordinator.activeGestures);
  });

  it('modipress: simple longpress within its duration', async () => {
    const turtle0 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'shift'
    });
    turtle0.wait(1000, 50);
    turtle0.commitPending();

    const turtle1 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 120,
      item: 'a'
    });
    turtle1.wait(600, 50);
    turtle1.move(0, 10, 100, 5);
    turtle1.hoveredItem = 'à';
    turtle1.move(90, 10, 100, 5);
    turtle1.hoveredItem = 'â';
    turtle1.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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
          matchedId: 'modipress-start',
          item: 'shift',
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isFalse(sources[0].isPathComplete);
            return;
          }
        }, {
          matchedId: 'modipress-end',
          item: null,
          linkType: 'complete',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
      ], [
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
      ]
    ];

    const sequencePromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    const sequenceAssertionPromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    let sequenceIndex = 0;
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      const index = sequenceIndex++;
      try {
        sequencePromises[index].resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertions[index]);
        sequenceAssertionPromises[index].resolve();
      } catch(err) {
        sequenceAssertionPromises[index].reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromises[0].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await sequencePromises[1].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await runnerPromise;

    await sequenceAssertionPromises[0].corePromise;
    await sequenceAssertionPromises[1].corePromise;
    assert.isEmpty(touchpointCoordinator.activeGestures);
  });

  it('modipress: shorter than longpress timer - failed attempt at longpress', async () => {
    const turtle0 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'shift'
    });
    turtle0.wait(400, 50);
    turtle0.commitPending();

    const turtle1 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 120,
      item: 'a'
    });
    turtle1.wait(600, 50);
    turtle1.move(0, 10, 100, 5);
    turtle1.hoveredItem = 'à';
    turtle1.move(90, 10, 100, 5);
    turtle1.hoveredItem = 'â';
    turtle1.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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
          matchedId: 'modipress-start',
          item: 'shift',
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isFalse(sources[0].isPathComplete);
            return;
          }
        }, {
          matchedId: 'modipress-end',
          item: null,
          linkType: 'complete',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
      ], [ ]
    ];

    const sequencePromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    const sequenceAssertionPromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    let sequenceIndex = 0;
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      const index = sequenceIndex++;
      try {
        sequencePromises[index].resolve();
        await assertGestureSequence(sequence, completionPromise, sequenceAssertions[index]);
        sequenceAssertionPromises[index].resolve();
      } catch(err) {
        sequenceAssertionPromises[index].reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    await sequencePromises[0].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await runnerPromise;

    await sequenceAssertionPromises[0].corePromise;

    // No GestureSequence should exist for the longpress - it's cancelled before things can
    // reach that point.
    assert.equal(await promiseStatus(sequencePromises[1].corePromise), PROMISE_PENDING);
    assert.isEmpty(touchpointCoordinator.activeGestures);
  });

  it('modipress: sustaining subkey-select after modipress end', async () => {
    const turtle0 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 100,
      item: 'shift'
    });
    turtle0.wait(700, 50);
    turtle0.commitPending();

    const turtle1 = new TouchpathTurtle({
      targetX: 1,
      targetY: 1,
      t: 120,
      item: 'a'
    });
    turtle1.wait(600, 50);
    turtle1.move(0, 10, 100, 5);
    turtle1.hoveredItem = 'à';
    turtle1.move(90, 10, 100, 5);
    turtle1.hoveredItem = 'â';
    turtle1.commitPending();

    const emulationEngine = new HeadlessInputEngine();
    const touchpointCoordinator = new TouchpointCoordinator(TestGestureModelDefinitions, [emulationEngine]);
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
          matchedId: 'modipress-start',
          item: 'shift',
          linkType: 'chain',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isFalse(sources[0].isPathComplete);
            return;
          }
        }, {
          matchedId: 'modipress-end',
          item: null,
          linkType: 'complete',
          sources: (sources) => {
            assert.equal(sources.length, 1);
            assert.isTrue(sources[0].isPathComplete);
            return;
          }
        }
      ], [
        {
          matchedId: 'longpress',
          item: null,
          linkType: 'chain',
          sources: (sources) => {
            // Assert single-source
            assert.equal(sources.length, 1);

            // Assert wait appropriate to the longpress threshold.  Likely won't be the full 1000 ms.
            const pathStats = sources[0].path.stats;
            // Note:  performance.now() == 620, while pathStats.lastSample == 612.  That's the cause of the assertion errors.
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
      ]
    ];

    const sequencePromises = [new ManagedPromise(), new ManagedPromise<void>()] as ManagedPromise<GestureSequence<string>>[];
    const sequenceAssertionPromises = [new ManagedPromise<void>(), new ManagedPromise<void>()];
    let sequenceIndex = 0;
    touchpointCoordinator.on('recognizedgesture', async (sequence) => {
      const index = sequenceIndex++;
      try {
        sequencePromises[index].resolve(sequence);
        await assertGestureSequence(sequence, completionPromise, sequenceAssertions[index]);
        sequenceAssertionPromises[index].resolve();
      } catch(err) {
        sequenceAssertionPromises[index].reject(err);
      }
    });

    const runnerPromise = fakeClock.runToLastAsync();

    // The "assertion promises" all wait until all inputs have been processed, so they're not good
    // for checking the relative order in which sequences end.
    const sequenceCompletions: ManagedPromise<void>[] = [new ManagedPromise(), new ManagedPromise()];
    sequencePromises[0].then((sequence) => {
      sequence.on('complete', () => sequenceCompletions[0].resolve());
    });
    sequencePromises[1].then((sequence) => {
      sequence.on('complete', () => sequenceCompletions[1].resolve());
    });

    await sequencePromises[0].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await sequencePromises[1].corePromise;
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    // index 0 completes before index 1!
    await Promise.race([sequenceCompletions[0].corePromise, sequenceCompletions[1].corePromise]);
    assert.equal(await promiseStatus(sequenceCompletions[0].corePromise), PromiseStatusModule.PROMISE_RESOLVED);
    assert.equal(await promiseStatus(sequenceCompletions[1].corePromise), PromiseStatusModule.PROMISE_PENDING);
    assert.isNotEmpty(touchpointCoordinator.activeGestures);

    await Promise.all([sequenceAssertionPromises[0].corePromise, sequenceAssertionPromises[1].corePromise]);

    await runnerPromise;
    assert.isEmpty(touchpointCoordinator.activeGestures);
  });
});