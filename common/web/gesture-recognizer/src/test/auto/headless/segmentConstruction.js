const assert = require('chai').assert;

const PromiseStatusModule = require('promise-status-async');
const promiseStatus       = PromiseStatusModule.promiseStatus;
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;

const GestureRecognizer = require('../../../../build/index.js');
const com = GestureRecognizer.com;
global.com = com; // TouchpathTurtle has issues without this at present.

const ConstructingSegment = com.keyman.osk.ConstructingSegment;
const SegmentClassifier   = com.keyman.osk.SegmentClassifier;

const TouchpathTurtle     = require('../../../../build/tools/unit-test-resources.js').TouchpathTurtle;

describe("Iterative Segment Construction (ConstructingSegment)", function() {
  describe("constructor()", function() {
    const classifier = new SegmentClassifier({
      holdMinimumDuration: 100,
      holdMoveTolerance: 5
    });

    it("Single-sample based", function() {
      const turtle = new TouchpathTurtle({
        targetX: 10,
        targetY: 20,
        t: 100
      });

      // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
      const constructor = new ConstructingSegment(turtle.pathComponents[0], classifier);
      assert.isTrue(constructor.hasPendingSubsegment);
      assert.isFalse(constructor.hasPrecommittedSubsegment);

      assert.equal(constructor.subsegmentCount, 0); // no committed subsegments yet!
      assert.isNull(constructor.committedInterval);
      assert.exists(constructor.pathSegment);
    });

    it("Based on short interval within thresholds", async function() {
      const firstSample = {
        targetX: 10,
        targetY: 20,
        t: 100
      };

      const turtle = new TouchpathTurtle(firstSample);

      const baseComponent = turtle.move(45, 3, 66, 33); // 45 degrees, 3px distance, 66ms, 33ms at a time.

      // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
      const constructor = new ConstructingSegment(baseComponent, classifier);
      assert.isTrue(constructor.hasPendingSubsegment);

      assert.equal(constructor.subsegmentCount, 0); // no committed subsegments yet!
      assert.isNull(constructor.committedInterval);
      assert.exists(constructor.pathSegment);
      assert.deepEqual(constructor.pathSegment.initialCoord, firstSample);

      // The public-facing Segment should not be recognized yet.
      assert.isFalse(constructor.hasPrecommittedSubsegment);
      const segment = constructor.pathSegment;
      assert.isNull(segment.type);
      assert.equal(await promiseStatus(segment.whenRecognized), PromiseStatuses.PROMISE_PENDING);
      assert.equal(await promiseStatus(segment.whenResolved), PromiseStatuses.PROMISE_PENDING);

      // Based directly on the `turtle.move` call.
      assert.isAtLeast(segment.distance, 2.999);
      assert.isAtMost(segment.distance, 3.001); // thank you, floating-point precision issues.
      assert.equal(segment.duration, 66);
      assert.isAtLeast(segment.angle, 45 * Math.PI / 180 - 10 * Number.EPSILON);
      assert.isAtMost (segment.angle, 45 * Math.PI / 180 + 10 * Number.EPSILON);
    });

    it("Instant pre-commit due to move", async function() {
      const firstSample = {
        targetX: 10,
        targetY: 20,
        t: 100
      };

      const turtle = new TouchpathTurtle(firstSample);

      const baseComponent = turtle.move(45, 30, 80, 10); // 45 degrees, 30px distance, 80ms, 10ms at a time.

      // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
      const constructor = new ConstructingSegment(baseComponent, classifier);
      assert.isTrue(constructor.hasPendingSubsegment);

      assert.equal(constructor.subsegmentCount, 0); // no committed subsegments yet!
      assert.isNull(constructor.committedInterval);
      assert.exists(constructor.pathSegment);
      assert.deepEqual(constructor.pathSegment.initialCoord, firstSample);

      // The public-facing Segment should already be recognized!
      assert.isTrue(constructor.hasPrecommittedSubsegment);
      const segment = constructor.pathSegment;
      assert.equal(segment.type, 'move');
      assert.equal(await promiseStatus(segment.whenRecognized), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(segment.whenResolved), PromiseStatuses.PROMISE_PENDING);

      // Based directly on the `turtle.move` call.
      assert.isAtLeast(segment.distance, 29.999);
      assert.isAtMost(segment.distance, 30.001); // thank you, floating-point precision issues.
      assert.equal(segment.duration, 80);
      assert.isAtLeast(segment.angle, 45 * Math.PI / 180 - 10 * Number.EPSILON);
      assert.isAtMost (segment.angle, 45 * Math.PI / 180 + 10 * Number.EPSILON);
    });

    it("Instant pre-commit due to hold", async function() {
      const firstSample = {
        targetX: 10,
        targetY: 20,
        t: 100
      };

      const turtle = new TouchpathTurtle(firstSample);

      const baseComponent = turtle.wait(1000, 30); // 1000ms wait, 30ms interval for repetitions.

      // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
      const constructor = new ConstructingSegment(baseComponent, classifier);
      assert.isTrue(constructor.hasPendingSubsegment);

      assert.equal(constructor.subsegmentCount, 0); // no committed subsegments yet!
      assert.isNull(constructor.committedInterval);
      assert.exists(constructor.pathSegment);
      assert.deepEqual(constructor.pathSegment.initialCoord, firstSample);

      // The public-facing Segment should already be recognized!
      assert.isTrue(constructor.hasPrecommittedSubsegment);
      const segment = constructor.pathSegment;
      assert.equal(segment.type, 'hold');
      assert.equal(await promiseStatus(segment.whenRecognized), PromiseStatuses.PROMISE_RESOLVED);
      assert.equal(await promiseStatus(segment.whenResolved), PromiseStatuses.PROMISE_PENDING);

      // Based directly on the `turtle.hold` call.
      assert.equal(segment.distance, 0);
      assert.equal(segment.duration, 1000);
    });
  });

  describe("precommitted subsegment handling", function() {
    const classifier = new SegmentClassifier({
      holdMinimumDuration: 100,
      holdMoveTolerance: 5
    });

    // Keep in mind, tests here assume that segmentation has already done its job, and
    // thus the segment "should" be the same, before and after.  We can cheat this slightly,
    // but only in certain directions.

    // Fortunately, this is a pretty easy "direction" to cheat in.  The opposite... not so much.
    it("pre-committed hold -> move", function() {
      const firstSample = {
        targetX: 10,
        targetY: 20,
        t: 100
      };

      const turtle = new TouchpathTurtle(firstSample);

      const baseComponent = turtle.wait(1000, 20); // 1000ms, record every 20ms.

      // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
      const constructor = new ConstructingSegment(baseComponent, classifier);
      assert.isTrue(constructor.hasPendingSubsegment);
      assert.exists(constructor.pathSegment);

      // The public-facing Segment should already be recognized!
      assert.isTrue(constructor.hasPrecommittedSubsegment);
      const segment = constructor.pathSegment;
      assert.equal(segment.type, 'hold');

      const followup = turtle.move(45, 30, 80, 10); // 45 degrees, 30px distance, 80ms, 10ms at a time.

      const concatenated = {
        stats: followup.endingAccumulation.deaccumulate(baseComponent.baseAccumulation),
        endingAccumulation: followup.endingAccumulation,
        baseAccumulation: baseComponent.baseAccumulation
      };

      // The pending segment may not update b/c of incompatibility with the already-pending part.
      assert.isFalse(constructor.updatePendingSubsegment(concatenated));
    });

    it("precommit cancel prevention", function() {
      const firstSample = {
        targetX: 10,
        targetY: 20,
        t: 100
      };

      const turtle = new TouchpathTurtle(firstSample);

      const baseComponent = turtle.wait(1000, 20); // 1000ms, record every 20ms.

      // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
      const constructor = new ConstructingSegment(baseComponent, classifier);

      assert.throws(() => constructor.clearPendingSubsegment());
    });
  });

  describe("subsegment compatibility handling", function() {
    const classifier = new SegmentClassifier({
      holdMinimumDuration: 100,
      holdMoveTolerance: 5
    });

    it("hold -> move", function() {
      const firstSample = {
        targetX: 10,
        targetY: 20,
        t: 100
      };

      const turtle = new TouchpathTurtle(firstSample);

      const baseComponent = turtle.wait(1000, 20); // 1000ms, record every 20ms.

      // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
      const constructor = new ConstructingSegment(baseComponent, classifier);
      assert.isTrue(constructor.hasPendingSubsegment);
      assert.exists(constructor.pathSegment);

      // The public-facing Segment should already be recognized!
      assert.isTrue(constructor.hasPrecommittedSubsegment);
      constructor.commitPendingSubsegment();

      const followup = turtle.move(45, 30, 80, 10); // 45 degrees, 30px distance, 80ms, 10ms at a time.

      assert.isFalse(constructor.isCompatible(followup));
    });

    it("hold -> hold", function() {
      const firstSample = {
        targetX: 10,
        targetY: 20,
        t: 100
      };

      const turtle = new TouchpathTurtle(firstSample);

      const baseComponent = turtle.wait(1000, 20); // 1000ms, record every 20ms.

      // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
      const constructor = new ConstructingSegment(baseComponent, classifier);
      assert.isTrue(constructor.hasPendingSubsegment);
      assert.exists(constructor.pathSegment);

      // The public-facing Segment should already be recognized!
      assert.isTrue(constructor.hasPrecommittedSubsegment);
      constructor.commitPendingSubsegment();

      const followup = turtle.wait(200, 40); // 200ms, record every 40 ms.

      assert.isTrue(constructor.isCompatible(followup));
    });

    it("move -> hold", function() {
      const firstSample = {
        targetX: 10,
        targetY: 20,
        t: 100
      };

      const turtle = new TouchpathTurtle(firstSample);

      const baseComponent = turtle.move(45, 30, 80, 10); // 45 degrees, 30px distance, 80ms, 10ms at a time.

      // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
      const constructor = new ConstructingSegment(baseComponent, classifier);
      assert.isTrue(constructor.hasPendingSubsegment);
      assert.exists(constructor.pathSegment);

      // The public-facing Segment should already be recognized!
      assert.isTrue(constructor.hasPrecommittedSubsegment);
      constructor.commitPendingSubsegment();

      const followup = turtle.wait(1000, 20); // 1000ms, record every 20ms.

      assert.isFalse(constructor.isCompatible(followup));
    });

    it("move (se) -> move (se)", function() {
      const firstSample = {
        targetX: 10,
        targetY: 20,
        t: 100
      };

      const turtle = new TouchpathTurtle(firstSample);

      const baseComponent = turtle.move(45, 30, 80, 10); // 45 degrees, 30px distance, 80ms, 10ms at a time.

      // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
      const constructor = new ConstructingSegment(baseComponent, classifier);
      assert.isTrue(constructor.hasPendingSubsegment);
      assert.exists(constructor.pathSegment);

      // The public-facing Segment should already be recognized!
      assert.isTrue(constructor.hasPrecommittedSubsegment);
      constructor.commitPendingSubsegment();

      const followup = turtle.move(45, 10, 160, 20); // 45 degrees, 10px distance, 160ms, 20ms at a time.

      assert.isTrue(constructor.isCompatible(followup));
    });

    it("move (se) -> move (ne)", function() {
      const firstSample = {
        targetX: 10,
        targetY: 20,
        t: 100
      };

      const turtle = new TouchpathTurtle(firstSample);

      const baseComponent = turtle.move(45, 30, 80, 10); // 45 degrees, 30px distance, 80ms, 10ms at a time.

      // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
      const constructor = new ConstructingSegment(baseComponent, classifier);
      assert.isTrue(constructor.hasPendingSubsegment);
      assert.exists(constructor.pathSegment);

      // The public-facing Segment should already be recognized!
      assert.isTrue(constructor.hasPrecommittedSubsegment);
      constructor.commitPendingSubsegment();

      const followup = turtle.move(315, 30, 80, 10); // 315 degrees, 30px distance, 80ms, 10ms at a time.

      assert.isFalse(constructor.isCompatible(followup));
    });

    // // Does not work; we don't yet have thresholding for minimal gains if the segmentation is literally perfect
    // // as occurs here.
    // it("move (n) -> move (ne) [bucket edge]", function() {
    //   const firstSample = {
    //     targetX: 10,
    //     targetY: 20,
    //     t: 100
    //   };

    //   const turtle = new TouchpathTurtle(firstSample);

    //   const baseComponent = turtle.move(22.4, 30, 80, 10); // 45 degrees, 30px distance, 80ms, 10ms at a time.

    //   // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
    //   const constructor = new ConstructingSegment(baseComponent, classifier);
    //   assert.isTrue(constructor.hasPendingSubsegment);
    //   assert.exists(constructor.pathSegment);

    //   // The public-facing Segment should already be recognized!
    //   assert.isTrue(constructor.hasPrecommittedSubsegment);
    //   constructor.commitPendingSubsegment();

    //   const followup = turtle.move(22.6, 30, 80, 10); // 315 degrees, 30px distance, 80ms, 10ms at a time.

    //   assert.isTrue(constructor.isCompatible(followup));
    // });
  });

  describe("pending subsegment updates", function() {
    const classifier = new SegmentClassifier({
      holdMinimumDuration: 100,
      holdMoveTolerance: 5
    });

    describe("no committed or precommitted sections", function() {
      it("mini-move -> hold", async function() {
        const firstSample = {
          targetX: 10,
          targetY: 20,
          t: 100
        };

        const turtle = new TouchpathTurtle(firstSample);

        const baseComponent = turtle.move(45, 3, 66, 33); // 45 degrees, 3px distance, 66ms, 33ms at a time.

        // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
        const constructor = new ConstructingSegment(baseComponent, classifier);
        assert.isTrue(constructor.hasPendingSubsegment);
        assert.exists(constructor.pathSegment);

        // The public-facing Segment should already be recognized!
        assert.isFalse(constructor.hasPrecommittedSubsegment);

        const followup = turtle.wait(34, 17); // 34ms, record every 17ms.  Reaches the 'hold' time threshold.

        assert.isTrue(constructor.updatePendingSubsegment(followup));
        const segment = constructor.pathSegment;
        assert.equal(segment.type, 'hold');
      });

      it("mini-move -> full move", async function() {
        const firstSample = {
          targetX: 10,
          targetY: 20,
          t: 100
        };

        const turtle = new TouchpathTurtle(firstSample);

        const baseComponent = turtle.move(45, 3, 66, 33); // 45 degrees, 3px distance, 66ms, 33ms at a time.

        // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
        const constructor = new ConstructingSegment(baseComponent, classifier);
        assert.isTrue(constructor.hasPendingSubsegment);
        assert.exists(constructor.pathSegment);

        // The public-facing Segment should already be recognized!
        assert.isFalse(constructor.hasPrecommittedSubsegment);

        const followup = turtle.move(45, 7, 34, 17); // enough to cross the 'move' distance threshold in time.

        assert.isTrue(constructor.updatePendingSubsegment(followup));
        const segment = constructor.pathSegment;
        assert.equal(segment.type, 'move');
      });

      it("mini-hold -> move", async function() {
        const firstSample = {
          targetX: 10,
          targetY: 20,
          t: 100
        };

        const turtle = new TouchpathTurtle(firstSample);

        const baseComponent = turtle.wait(34, 17); // 34 ms of no movement.

        // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
        const constructor = new ConstructingSegment(baseComponent, classifier);
        assert.isTrue(constructor.hasPendingSubsegment);
        assert.exists(constructor.pathSegment);

        // The public-facing Segment should already be recognized!
        assert.isFalse(constructor.hasPrecommittedSubsegment);

        const followup = turtle.move(45, 10, 66, 33); // in 66 ms, travels far enough to cross the 'move' distance threshold in time.

        assert.isTrue(constructor.updatePendingSubsegment(followup));
        const segment = constructor.pathSegment;
        assert.equal(segment.type, 'move');
      });
    });

    describe("adding subsegments", function() {
      it("slow move (se) -> quick move (se)", async function() {
        const firstSample = {
          targetX: 10,
          targetY: 20,
          t: 100
        };

        const turtle = new TouchpathTurtle(firstSample);

        const baseComponent = turtle.move(45, 4, 80, 10); // 45 degrees, 30px distance, 80ms, 10ms at a time.

        // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
        const constructor = new ConstructingSegment(baseComponent, classifier);
        assert.isTrue(constructor.hasPendingSubsegment);

        const segment = constructor.pathSegment;
        assert.exists(segment);
        assert.equal(segment.type, null);
        assert.equal(await promiseStatus(segment.whenRecognized), PromiseStatuses.PROMISE_PENDING);
        assert.equal(await promiseStatus(segment.whenResolved), PromiseStatuses.PROMISE_PENDING);
        assert.isFalse(constructor.hasPrecommittedSubsegment);

        constructor.commitPendingSubsegment();

        // Committing the short-move shouldn't change recognition status.
        assert.equal(segment.type, null);
        assert.equal(await promiseStatus(segment.whenRecognized), PromiseStatuses.PROMISE_PENDING);
        assert.equal(await promiseStatus(segment.whenResolved), PromiseStatuses.PROMISE_PENDING);

        const followup = turtle.move(45, 10, 160, 20); // 45 degrees, 10px distance, 160ms, 20ms at a time.

        assert.isTrue(constructor.updatePendingSubsegment(followup));
        assert.equal(segment.type, 'move');
        assert.equal(await promiseStatus(segment.whenRecognized), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(segment.whenResolved), PromiseStatuses.PROMISE_PENDING);
      });

      it("hold -> 2px move -> hold -> 1px move", async function() {
        const firstSample = {
          targetX: 10,
          targetY: 20,
          t: 100
        };

        const turtle = new TouchpathTurtle(firstSample);

        const baseComponent = turtle.wait(40, 20);

        // Initialization of the TouchpathTurtle provides a Subsegment for its base point.
        const constructor = new ConstructingSegment(baseComponent, classifier);
        assert.isTrue(constructor.hasPendingSubsegment);

        const segment = constructor.pathSegment;
        assert.exists(segment);
        assert.equal(segment.type, null);
        assert.equal(await promiseStatus(segment.whenRecognized), PromiseStatuses.PROMISE_PENDING);
        assert.equal(await promiseStatus(segment.whenResolved), PromiseStatuses.PROMISE_PENDING);

        constructor.commitPendingSubsegment();
        assert.equal(segment.type, null);
        assert.equal(await promiseStatus(segment.whenRecognized), PromiseStatuses.PROMISE_PENDING);
        assert.equal(await promiseStatus(segment.whenResolved), PromiseStatuses.PROMISE_PENDING);

        // The public-facing Segment should already be recognized!
        assert.isFalse(constructor.hasPrecommittedSubsegment);

        let followup = turtle.move(90, 2, 60, 20);

        assert.isTrue(constructor.updatePendingSubsegment(followup));
        assert.equal(segment.type, 'hold'); // We've reached 100 ms.
        assert.equal(await promiseStatus(segment.whenRecognized), PromiseStatuses.PROMISE_RESOLVED);
        assert.equal(await promiseStatus(segment.whenResolved), PromiseStatuses.PROMISE_PENDING);

        constructor.commitPendingSubsegment();

        followup = turtle.wait(50, 25);
        assert.isTrue(constructor.isCompatible(followup)); // To verify the following method's precondition.
        assert.isTrue(constructor.updatePendingSubsegment(followup));

        followup = turtle.move(90, 100, 20, 20);
        assert.isTrue(constructor.isCompatible(followup)); // To verify the following method's precondition.
        assert.isTrue(constructor.updatePendingSubsegment(followup));
      });
    });
  });
});