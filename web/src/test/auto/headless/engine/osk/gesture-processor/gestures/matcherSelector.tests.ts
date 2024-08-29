import { assert } from 'chai'
import sinon from 'sinon';

import * as PromiseStatusModule from 'promise-status-async';
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;
import { assertingPromiseStatus as promiseStatus } from '../../../../../resources/assertingPromiseStatus.js';

import { simulateMultiSourceMatcherInput, simulateSelectorInput } from "../../../../../resources/simulateMultiSourceInput.js";

import { timedPromise } from '@keymanapp/web-utils';
import { gestures } from '@keymanapp/gesture-recognizer';

import { TouchpathTurtle } from '#gesture-tools';

type MatcherSelection<Type> = gestures.matchers.MatcherSelection<Type>;
type GestureModel<Type> = gestures.specs.GestureModel<Type>;

import {
  LongpressModel,
  MultitapModel,
  SimpleTapModel,
  SubkeySelectModel
} from './isolatedGestureSpecs.js';

import {
  LongpressDistanceThreshold
} from './isolatedPathSpecs.js';

describe("MatcherSelector", function () {
  beforeEach(function() {
    this.fakeClock = sinon.useFakeTimers();
  });

  afterEach(function() {
    this.fakeClock.restore();
  });

  describe("Single-source", function() {
    describe("First stage", function() {
      it("Longpress (in isolation)", async function() {
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
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput({
          pathSpecs: [{ type: 'sequence', samples: turtle.path, terminate: false }],
          specSet: [LongpressModel]
        }, this.fakeClock);

        let completion = executor();
        await selectorPromise;
        await Promise.race([completion, selectionPromises[0]]);

        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        const selection = await selectionPromises[0];

        assert.deepEqual(selection.result, {matched: true, action: { type: 'chain', item: null, selectionMode: 'none', next: 'subkey-select'}});
        assert.deepEqual(selection.matcher.model, LongpressModel);
        assert.isFalse(sources[0].path.isComplete);

        // Allow the rest of the simulation to play out; it's easy cleanup that way.
        await completion;
      });

      it("Longpress (with other possibilities)", async function() {
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
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput({
          pathSpecs: [{ type: 'sequence', samples: turtle.path, terminate: false }],
          specSet: [LongpressModel, MultitapModel, SimpleTapModel]
        }, this.fakeClock);

        let completion = executor();
        const selector = await selectorPromise;

        await timedPromise(200);
        // Multitap should already be eliminated from consideration.
        assert.sameOrderedMembers(selector.potentialMatchersForSource(sources[0]).map((matcher) => matcher.model.id), ['simple-tap', 'longpress']);
        assert.sameOrderedMembers(sources[0].potentialModelMatchIds, ['simple-tap', 'longpress']);

        await Promise.race([completion, selectionPromises[0]]);

        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        const selection = await selectionPromises[0];

        assert.deepEqual(selection.result, {matched: true, action: { type: 'chain', item: null, selectionMode: 'none', next: 'subkey-select'}});
        assert.deepEqual(selection.matcher.model, LongpressModel);
        assert.isFalse(sources[0].path.isComplete);

        // Allow the rest of the simulation to play out; it's easy cleanup that way.
        await completion;
      });

      it("Longpress reject (due to path) -> reset request (in isolation)", async function() {
        const turtle = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        turtle.move(90, LongpressDistanceThreshold + 2, LongpressModel.contacts[0].model.timer.duration, 20);
        turtle.wait(1000, 50);
        turtle.commitPending();

        const {
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput({
          pathSpecs: [{ type: 'sequence', samples: turtle.path, terminate: false }],
          specSet: [LongpressModel]
        }, this.fakeClock);


        let completion = executor();
        const selector = await selectorPromise;
        const rejectionStub = sinon.fake();
        selector.on('rejectionwithaction', rejectionStub);

        await Promise.race([completion, selectionPromises[0]]);

        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_PENDING);
        // It finished simulating without a match.  (Note:  we don't terminate the simulated
        // touchpath - `terminate: false`.)
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_RESOLVED);
        assert.isTrue(rejectionStub.calledOnce);

        const rejectionData = rejectionStub.firstCall.args as [ MatcherSelection<string>, (model: GestureModel<string>) => void ];
        assert.equal(rejectionData[0].matcher.model.id, 'longpress');
        assert.equal(rejectionData[0].result.matched, false);
        assert.deepEqual(rejectionData[0].result.action, { type: 'replace', replace: 'longpress', item: null});

        // ... we technically already have it, but this _is_ a convenient pattern to maintain for
        // consistency among all this suite's tests.
        await completion;
      });

      it("Longpress rejection replacement (in isolation)", async function() {
        const turtle = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        turtle.move(90, LongpressDistanceThreshold + 2, LongpressModel.contacts[0].model.timer.duration - 2, 20);
        turtle.hoveredItem = 'b';
        turtle.wait(1000, 50);
        turtle.commitPending();

        const {
          sources,
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput({
          pathSpecs: [{ type: 'sequence', samples: turtle.path, terminate: false }],
          specSet: [LongpressModel]
        }, this.fakeClock);


        let completion = executor();
        const selector = await selectorPromise;
        let rejectionCounter = 0;
        selector.on('rejectionwithaction', (selection, replaceModelWith) => {
          assert.equal(selection.matcher.model.id, 'longpress');

          rejectionCounter++;
          // Just... restart the model.
          replaceModelWith(LongpressModel);
        });

        await Promise.race([completion, selectionPromises[0]]);

        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_RESOLVED);
        // It finished simulating without a match.  (Note:  we don't terminate the simulated
        // touchpath - `terminate: false`.)
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        const selection = await selectionPromises[0];
        assert.deepEqual(selection.result, {matched: true, action: { type: 'chain', item: null, selectionMode: 'none', next: 'subkey-select'}});
        assert.deepEqual(selection.matcher.model, LongpressModel);

        // Original base item was 'a'; 'b' proves that a reset occurred by the point of the 'item' change.
        assert.equal(selection.matcher.baseItem, 'b');
        assert.isFalse(sources[0].path.isComplete);

        // One for path distance before the longpress timer completed, then one for change of current path 'item'.
        // (from 'a' to 'b')
        assert.equal(rejectionCounter, 2);

        await completion;
      });

      it("Longpress rejection replacement (with other possibilities)", async function() {
        const turtle = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        turtle.move(90, LongpressDistanceThreshold + 2, LongpressModel.contacts[0].model.timer.duration - 2, 20);
        turtle.hoveredItem = 'b';
        turtle.wait(1000, 50);
        turtle.commitPending();

        const {
          sources,
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput({
          pathSpecs: [{ type: 'sequence', samples: turtle.path, terminate: false }],
          // Current problem:  adding the extra models leads to rejection of all?
          specSet: [LongpressModel, MultitapModel, SimpleTapModel]
        }, this.fakeClock);


        let completion = executor();
        const selector = await selectorPromise;
        selector.on('rejectionwithaction', (selection, replaceModelWith) => {
          if(selection.matcher.model.id == 'longpress') {
            replaceModelWith(LongpressModel);
          } else if(selection.matcher.model.id == 'simple-tap') {
            replaceModelWith(SimpleTapModel);
          } else {
            assert.fail();
          }
        });

        await timedPromise(200); // after the longpress-reset
        // Multitap should already be eliminated from consideration.
        assert.sameOrderedMembers(selector.potentialMatchersForSource(sources[0]).map((matcher) => matcher.model.id), ['simple-tap', 'longpress']);
        assert.sameOrderedMembers(sources[0].potentialModelMatchIds, ['simple-tap', 'longpress']);

        await Promise.race([completion, selectionPromises[0]]);

        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_RESOLVED);
        // It finished simulating without a match.  (Note:  we don't terminate the simulated
        // touchpath - `terminate: false`.)
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        const selection = await selectionPromises[0];
        assert.deepEqual(selection.result, {matched: true, action: { type: 'chain', item: null, selectionMode: 'none', next: 'subkey-select'}});
        assert.deepEqual(selection.matcher.model, LongpressModel);

        // Original base item was 'a'; 'b' proves that a reset occurred by the point of the 'item' change.
        assert.equal(selection.matcher.baseItem, 'b');
        assert.isFalse(sources[0].path.isComplete);

        await completion;
      });

      // roaming touch:  relatively long + slow move; expect longpress reset w/ matched: false
      // (not in isolation)

      it("Simple Tap (single source)", async function() {
        const turtle = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        turtle.wait(100, 5);
        turtle.commitPending();

        const {
          sources,
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput({
          pathSpecs: [{ type: 'sequence', samples: turtle.path, terminate: true }],
          specSet: [LongpressModel, MultitapModel, SimpleTapModel]
        }, this.fakeClock);

        let completion = executor();
        const selector = await selectorPromise;

        // Multitap should already be eliminated from consideration... even at timestamp 0.
        assert.sameOrderedMembers(selector.potentialMatchersForSource(sources[0]).map((matcher) => matcher.model.id), ['simple-tap', 'longpress']);
        assert.sameOrderedMembers(sources[0].potentialModelMatchIds, ['simple-tap', 'longpress']);
        await Promise.race([completion, selectionPromises[0]]);

        // So, the terminate signal didn't complete the selection?
        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        const selection = await selectionPromises[0];

        assert.deepEqual(selection.result, {matched: true, action: { type: 'chain', item: 'a', next: 'multitap' }});
        assert.deepEqual(selection.matcher.model, SimpleTapModel);
        assert.isTrue(sources[0].path.isComplete);

        // Allow the rest of the simulation to play out; it's easy cleanup that way.
        await completion;
      });

      it("Simple Tap (single source) with reset", async function() {
        const turtle = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        turtle.wait(40, 5);
        turtle.move(90, 2, 20, 1);
        turtle.hoveredItem = 'b';
        turtle.wait(40, 5);

        turtle.commitPending();

        const {
          sources,
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput({
          pathSpecs: [{ type: 'sequence', samples: turtle.path, terminate: true }],
          specSet: [LongpressModel, MultitapModel, SimpleTapModel]
        }, this.fakeClock);

        let completion = executor();
        let resets = 0;
        const selector = await selectorPromise;
        selector.on('rejectionwithaction', (selection, replaceModelWith) => {
          if(selection.matcher.model.id == 'longpress') {
            replaceModelWith(LongpressModel);
          } else if(selection.matcher.model.id == 'simple-tap') {
            resets++;
            replaceModelWith(SimpleTapModel);
          } else {
            assert.fail();
          }
        });

        await Promise.race([completion, selectionPromises[0]]);

        // So, the terminate signal didn't complete the selection?
        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        const selection = await selectionPromises[0];

        assert.deepEqual(selection.result, {matched: true, action: { type: 'chain', item: 'b', next: 'multitap' }});
        assert.deepEqual(selection.matcher.model, SimpleTapModel);
        assert.isTrue(sources[0].path.isComplete);
        assert.isAtLeast(resets, 1);

        // Allow the rest of the simulation to play out; it's easy cleanup that way.
        await completion;
      });
    });

    describe("Later stages", function() {
      it("Subkey selection", async function() {
        const turtle = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        turtle.wait(520, 26);
        turtle.commitPending();

        // Set up the prior stage's match
        const {
          executor: predExecutor,
          modelMatcherPromise
        } = simulateMultiSourceMatcherInput([
          { type: 'sequence', samples: turtle.path, terminate: false }
        ], this.fakeClock, LongpressModel);

       await predExecutor();
       const predecessorMatcher = await modelMatcherPromise;
       assert.equal(await promiseStatus(modelMatcherPromise), PromiseStatuses.PROMISE_RESOLVED);

        // Now, forward it (in some manner) to the simulation engine as part of its setup.

        // Also, construct the remainder of the path.
        const parentLength = turtle.path.length;

        turtle.move(45, 5, 60, 3);
        turtle.hoveredItem = 'b';
        turtle.move(90, 5, 60, 3);
        turtle.hoveredItem = 'c';
        turtle.commitPending();

        // Get the new part of the turtle's path, which has not yet been simulated.
        const remainingPath = [].concat(turtle.path).splice(parentLength, turtle.path.length - parentLength);

        const {
          sources,
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput(
          {
            pathSpecs: [{
              type: 'prior-matcher',
              matcher: predecessorMatcher,
              continuation: [{ type: 'sequence', samples: remainingPath, terminate: true }]
            }],
            specSet: [SubkeySelectModel]
          }
        , this.fakeClock);

        let completion = executor();
        await selectorPromise;
        await Promise.race([completion, selectionPromises[0]]);

        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        const selection = await selectionPromises[0];

        assert.deepEqual(selection.result, {matched: true, action: { type: 'complete', item: 'c' }});
        assert.deepEqual(selection.matcher.model, SubkeySelectModel);
        assert.isTrue(sources[0].path.isComplete);

        // Allow the rest of the simulation to play out; it's easy cleanup that way.
        await completion;
      });
    });
  });

  describe("Multi-source", function() {
    describe("First stage", function() {
      it("Simple tap (from second contact point during lifetime)", async function() {
        const turtle1 = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        turtle1.wait(100, 5);
        turtle1.commitPending();

        const {
          sources,
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput({
          pathSpecs: [
            { type: 'sequence', samples: turtle1.path, terminate: false },
            { type: 'sequence', samples: [{
              targetX: 5,
              targetY: 1,
              t: 200,
              item: 'b'
            }], terminate: false }
          ],
          specSet: [LongpressModel, MultitapModel, SimpleTapModel]
        }, this.fakeClock);

        let completion = executor();
        await selectorPromise;
        await Promise.race([completion, selectionPromises[0]]);

        // So, the terminate signal didn't complete the selection?
        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        const selection = await selectionPromises[0];

        assert.deepEqual(selection.result, {matched: true, action: { type: 'chain', item: 'a', next: 'multitap' }});
        assert.deepEqual(selection.matcher.model, SimpleTapModel);
        assert.isTrue(sources[0].path.isComplete);
        assert.isAtMost(sources[0].path.stats.duration, 101);

        // Allow the rest of the simulation to play out; it's easy cleanup that way.
        await completion;
      });
    });

    describe("Later stages", function() {
      it("Multi-tap", async function() {
        const turtle1 = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        turtle1.wait(100, 5);
        turtle1.commitPending();

        // Set up the prior stage's match
        const {
          executor: predExecutor,
          modelMatcherPromise
        } = simulateMultiSourceMatcherInput([
          { type: 'sequence', samples: turtle1.path, terminate: true }
        ], this.fakeClock, SimpleTapModel);

        await predExecutor();
        const predecessorMatcher = await modelMatcherPromise;
        assert.equal(await promiseStatus(modelMatcherPromise), PromiseStatuses.PROMISE_RESOLVED);

        const turtle2 = new TouchpathTurtle({
          targetX: 2,
          targetY: 2,
          t: 300,
          item: 'a'
        });
        turtle2.wait(100, 5);
        turtle2.commitPending();

        const {
          sources,
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput(
          {
            pathSpecs: [
              {
                type: 'prior-matcher',
                matcher: predecessorMatcher,
                continuation: null  // that GestureSource is DONE, terminated.  No continues.
              }, {
                type: 'sequence',
                samples: turtle2.path,
                terminate: true
              }
            ],
            specSet: [LongpressModel, MultitapModel, SimpleTapModel]
          }
        , this.fakeClock);

        let completion = executor();
        const selector = await selectorPromise;

        await timedPromise(350); // in the middle of the second tap.
        // The first tap is done and over... but it should be associated with a potential multitap.
        assert.sameOrderedMembers(selector.potentialMatchersForSource(sources[0]).map((matcher) => matcher.model.id), ['multitap']);
        assert.sameOrderedMembers(sources[0].potentialModelMatchIds, ['multitap']);
        // The second tap - that one isn't locked-in as a multitap yet.
        assert.sameOrderedMembers(selector.potentialMatchersForSource(sources[1]).map((matcher) => matcher.model.id), ['multitap', 'simple-tap', 'longpress']);
        assert.sameOrderedMembers(sources[1].potentialModelMatchIds, ['multitap', 'simple-tap', 'longpress']);

        await Promise.race([completion, selectionPromises[0]]);

        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        const selection = await selectionPromises[0];

        assert.deepEqual(selection.result, {matched: true, action: { type: 'chain', item: 'a', next: 'multitap' }});
        assert.deepEqual(selection.matcher.model, MultitapModel);
        assert.isTrue(sources[0].path.isComplete);

        // Allow the rest of the simulation to play out; it's easy cleanup that way.
        await completion;
      });

      it("Single tap (on different key/item than prior tap)", async function() {
        const turtle1 = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        turtle1.wait(100, 5);
        turtle1.commitPending();

        // Set up the prior stage's match
        const {
          executor: predExecutor,
          modelMatcherPromise
        } = simulateMultiSourceMatcherInput([
          { type: 'sequence', samples: turtle1.path, terminate: true }
        ], this.fakeClock, SimpleTapModel);

        await predExecutor();
        const predecessorMatcher = await modelMatcherPromise;
        assert.equal(await promiseStatus(modelMatcherPromise), PromiseStatuses.PROMISE_RESOLVED);

        const turtle2 = new TouchpathTurtle({
          targetX: 2,
          targetY: 2,
          t: 300,
          item: 'b'  // different item; the multitap should fail.
        });
        turtle2.wait(100, 5);
        turtle2.commitPending();

        const {
          sources,
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput(
          {
            pathSpecs: [
              {
                type: 'prior-matcher',
                matcher: predecessorMatcher,
                continuation: null  // that GestureSource is DONE, terminated.  No continues.
              }, {
                type: 'sequence',
                samples: turtle2.path,
                terminate: true
              }
            ],
            specSet: [LongpressModel, MultitapModel, SimpleTapModel]
          }
        , this.fakeClock);

        let completion = executor();
        await selectorPromise;
        await Promise.race([completion, selectionPromises[0]]);

        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        // Using the multi-tap leadup to attempt to find a match...
        const selection1 = await selectionPromises[0];
        // Sorry, nope, that gesture-stage sequence does not continue; it's over.
        assert.deepEqual(selection1.result, {matched: false, action: { type: 'complete', item: null }});
        assert.isTrue(sources[0].path.isComplete);

        // Make sure the macroqueue has a chance to process; the second tap must first cancel the
        // potential multitap, and moving past that includes 1 wait on the macroqueue... which may
        // be scheduled after the first such wait below, hence the second.
        await timedPromise(0);
        await timedPromise(0);

        await Promise.race([completion, selectionPromises[1]]);
        assert.equal(await promiseStatus(selectionPromises[1]), PromiseStatuses.PROMISE_RESOLVED);

        // Ignoring the multi-tap leadup and starting a new gesture-stage sequence instead...
        const selection2 = await selectionPromises[1];
        assert.isOk(selection2);
        assert.deepEqual(selection2.result, {matched: true, action: { type: 'chain', item: 'b', next: 'multitap' }});
        assert.deepEqual(selection2.matcher.model, SimpleTapModel);
        assert.isTrue(sources[0].path.isComplete);

        // Allow the rest of the simulation to play out if any remaining processing is queued;
        // it's easy cleanup that way.
        await completion;
      });

      it("Single tap (after overly-long delay from prior tap)", async function() {
        const turtle1 = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 100,
          item: 'a'
        });
        turtle1.wait(100, 5);
        turtle1.commitPending();

        // Set up the prior stage's match
        const {
          executor: predExecutor,
          modelMatcherPromise
        } = simulateMultiSourceMatcherInput([
          { type: 'sequence', samples: turtle1.path, terminate: true }
        ], this.fakeClock, SimpleTapModel);

        await predExecutor();
        const predecessorMatcher = await modelMatcherPromise;
        assert.equal(await promiseStatus(modelMatcherPromise), PromiseStatuses.PROMISE_RESOLVED);

        const turtle2 = new TouchpathTurtle({
          targetX: 1,
          targetY: 1,
          t: 1100,
          item: 'a'  // different item; the multitap should fail.
        });
        turtle2.wait(100, 5);
        turtle2.commitPending();

        const {
          sources,
          selectionPromises,
          selectorPromise,
          executor
        } = simulateSelectorInput(
          {
            pathSpecs: [
              {
                type: 'prior-matcher',
                matcher: predecessorMatcher,
                continuation: null  // that GestureSource is DONE, terminated.  No continues.
              }, {
                type: 'sequence',
                samples: turtle2.path,
                terminate: true
              }
            ],
            specSet: [LongpressModel, MultitapModel, SimpleTapModel]
          }
        , this.fakeClock);

        let completion = executor();
        await selectorPromise;
        await Promise.race([completion, selectionPromises[0]]);

        assert.equal(await promiseStatus(selectionPromises[0]), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(completion), PromiseStatusModule.PROMISE_PENDING);

        // Using the multi-tap leadup to attempt to find a match...
        const selection1 = await selectionPromises[0];
        // Sorry, nope, that gesture-stage sequence does not continue; it's over.
        assert.deepEqual(selection1.result, {matched: false, action: { type: 'complete', item: null }});
        assert.isTrue(sources[0].path.isComplete);

        // At this point in time, the second source hasn't yet been started; thus, there's no Promise yet to wait upon.
        // The simplest way to move forward:  assert that it'll exist once simulation is complete & check then.
        assert.isNotOk(selectionPromises[1]);
        await completion;

        assert.isOk(selectionPromises[1]);
        assert.equal(await promiseStatus(selectionPromises[1]), PromiseStatuses.PROMISE_RESOLVED);

        const selection2 = await selectionPromises[1];
        assert.deepEqual(selection2.result, {matched: true, action: { type: 'chain', item: 'a', next: 'multitap' }});
        assert.deepEqual(selection2.matcher.model, SimpleTapModel);
        assert.isTrue(sources[0].path.isComplete);

        // Allow the rest of the simulation to play out if any remaining processing is queued;
        // it's easy cleanup that way.
        await completion;
      });
    });
  });
});