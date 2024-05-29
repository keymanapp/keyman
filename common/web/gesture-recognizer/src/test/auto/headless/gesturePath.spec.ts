import { assert } from 'chai'
import sinon  from 'sinon';
import path from 'path';
import url from 'url';
import fs from 'fs';

import { GestureDebugPath } from '@keymanapp/gesture-recognizer';
import { timedPromise } from '@keymanapp/web-utils';
import { TouchpathTurtle } from '#tools';

// Ensures that the resources are resolved relative to this script, not to the cwd when the test
// runner was launched.
const scriptFolder = path.dirname(url.fileURLToPath(import.meta.url));
const SEGMENT_TEST_JSON_FOLDER = path.resolve(`${scriptFolder}/../../resources/json/segmentation`);

describe("GesturePath", function() {
  // Note:  if part of the suite below fails, it'll probably cascade into failures for some of the
  // other automated tests for the module.
  describe("canary tests: serialization & deserialization", function() {
    it('from fixture', () => {
      let testJSONtext = fs.readFileSync(`${SEGMENT_TEST_JSON_FOLDER}/simple_ne_move.json`, 'utf-8');
      let fullSerializedJSON = JSON.parse(testJSONtext);
      let rawPathObject = fullSerializedJSON.inputs[0].path;

      if(rawPathObject.segments) {
        // Left-over fixture data from before we removed subsegmentation.
        delete rawPathObject.segments;
      }

      let reconstructedPath = GestureDebugPath.deserialize(rawPathObject);
      assert.isFalse(reconstructedPath.wasCancelled);

      assert.sameDeepOrderedMembers([].concat(reconstructedPath.coords), rawPathObject.coords);
      const reconstructedPathJSON = reconstructedPath.toJSON();
      delete reconstructedPathJSON.stats;
      assert.notEqual(reconstructedPathJSON, rawPathObject);
      assert.deepEqual(reconstructedPathJSON, rawPathObject);
    });

    it('synthetic', () => {
      const initialSample = {
        targetX: 0,
        targetY: 0,
        item: 'a',
        t: 0
      };

      let path = new GestureDebugPath();
      let turtle = new TouchpathTurtle(initialSample);
      turtle.on('sample', (sample) => path.extend(sample));

      turtle.move(90, 2, 20, 2);
      turtle.hoveredItem = 'b';
      turtle.move(0, 2, 20, 2);
      turtle.hoveredItem = 'c';
      turtle.commitPending();
      path.terminate(true);

      const serializationObj = path.toJSON();
      const SERIALIZATION_TO_MATCH = `
{
  "coords": [
    {
      "targetX": 0,
      "targetY": 0,
      "t": 0,
      "item": "a"
    },
    {
      "targetX": 1,
      "targetY": 0,
      "t": 10,
      "item": "a"
    },
    {
      "targetX": 2,
      "targetY": 0,
      "t": 20,
      "item": "b"
    },
    {
      "targetX": 2,
      "targetY": -1,
      "t": 30,
      "item": "b"
    },
    {
      "targetX": 2,
      "targetY": -2,
      "t": 40,
      "item": "c"
    }
  ],
  "wasCancelled": true
}
      `.trim();
      assert.equal(JSON.stringify(serializationObj, (key, value) => key == 'stats' ? undefined : value, 2), SERIALIZATION_TO_MATCH);
    });
  });

  describe("Single-sample 'sequence'", function() {
    it("'step', 'complete' events", function() {
      const spyEventStep        = sinon.fake();
      const spyEventComplete    = sinon.fake();
      const spyEventInvalidated = sinon.fake();

      const touchpath = new GestureDebugPath();
      touchpath.on('step', spyEventStep);
      touchpath.on('complete', spyEventComplete);
      touchpath.on('invalidated', spyEventInvalidated);

      const sample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      touchpath.extend(sample);
      try {
        assert(spyEventStep.calledOnce, "'step' event was not raised exactly once.");

        const stepSample = spyEventStep.firstCall.args[0];
        assert.equal(stepSample, sample, "'step' did not provide the expected sample.");

        assert(spyEventComplete.notCalled, "'complete' event was raised early.");
        assert(spyEventInvalidated.notCalled, "'invalidated' event was erroneously raised.");
      } finally {
        touchpath.terminate(false); // false = "not cancelled"
      }
      assert(spyEventComplete.calledOnce, "'complete' event was not raised.");
      assert(spyEventInvalidated.notCalled, "'invalidated' event was erroneously raised.");
    });


    it("'step', 'invalidated' events", function() {
      const spyEventStep        = sinon.fake();
      const spyEventComplete    = sinon.fake();
      const spyEventInvalidated = sinon.fake();

      const touchpath = new GestureDebugPath();
      touchpath.on('step', spyEventStep);
      touchpath.on('complete', spyEventComplete);
      touchpath.on('invalidated', spyEventInvalidated);

      const sample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      touchpath.extend(sample);
      try {
        assert(spyEventStep.calledOnce, "'step' event was not raised exactly once.");

        const stepSample = spyEventStep.firstCall.args[0];
        assert.equal(stepSample, sample, "'step' did not provide the expected sample.");

        assert(spyEventComplete.notCalled, "'complete' event was erroneously raised.");
        assert(spyEventInvalidated.notCalled, "'invalidated' event was raised early.");
      } finally {
        touchpath.terminate(true); // true = "cancelled"
      }
      assert(spyEventComplete.notCalled, "'complete' event was erroneously raised.");
      assert(spyEventInvalidated.calledOnce, "'invalidated' event was not raised.");
    });

    it("event ordering - 'complete'", function() {
      const spyEventStep         = sinon.fake();
      const spyEventComplete     = sinon.fake();
      const spyEventInvalidated  = sinon.fake();

      const touchpath = new GestureDebugPath();
      touchpath.on('step', spyEventStep);
      touchpath.on('complete', spyEventComplete);
      touchpath.on('invalidated', spyEventInvalidated);

      const sample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      touchpath.extend(sample);
      touchpath.terminate(false); // false = "not cancelled"

      assert.deepEqual(touchpath.coords, [sample]);
    });

    it("event ordering - 'invalidated'", function() {
      const spyEventStep         = sinon.fake();
      const spyEventComplete     = sinon.fake();
      const spyEventInvalidated  = sinon.fake();

      const touchpath = new GestureDebugPath();
      touchpath.on('step', spyEventStep);
      touchpath.on('complete', spyEventComplete);
      touchpath.on('invalidated', spyEventInvalidated);

      const sample = {
        targetX: 1,
        targetY: 1,
        t: 100
      };

      touchpath.extend(sample);
      touchpath.terminate(true); // false = "not cancelled"

      assert.deepEqual(touchpath.coords, [sample]);
    });
  });


  describe("Single held point", function() {
    beforeEach(function() {
      this.fakeClock = sinon.useFakeTimers();
    })

    afterEach(function() {
      this.fakeClock.restore();
    })

    it("path.coords - no extra samples", async function() {
      const spyEventStep         = sinon.fake();
      const spyEventComplete     = sinon.fake();
      const spyEventInvalidated  = sinon.fake();

      const touchpath = new GestureDebugPath();
      touchpath.on('step', spyEventStep);
      touchpath.on('complete', spyEventComplete);
      touchpath.on('invalidated', spyEventInvalidated);

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
          touchpath.extend(sample);
        });
      });

      // Finalization: segmentation is then ended via a followup event as soon as the
      // final sample's execution returns down to the base event loop.
      const segmentationEndPromise = Promise.all(samplePromises).then(() => {
        touchpath.terminate(false);
      }).then(() => {
        // The main test assertions.
        assert.deepEqual(touchpath.coords, [startSample, endSample]);
      });

      const finalPromise = segmentationEndPromise.catch((reason) => {
        if(!touchpath.isComplete) {
          touchpath.terminate(true);
        }
        throw reason;
      });

      this.fakeClock.runAllAsync();

      // This is the one that reports all of our async assertion failures.
      return finalPromise;
    });
  });

  describe("Recording-based integration tests", function() {
    beforeEach(function() {
      this.fakeClock = sinon.useFakeTimers();
    })

    afterEach(function() {
      // NOTE:  for debugging investigations, it may be necessary to use .only on
      // the test under investigation and to disable the `this.fakeClock.restore()` line.
      //
      // Tests tend to timeout when interactively debugging, and having unmocked timers
      // suddenly restored during investigation can cause some very confusing behavior.
      this.fakeClock.restore();
    })
  });
});