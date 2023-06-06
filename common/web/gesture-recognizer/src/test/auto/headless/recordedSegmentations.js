import { assert } from 'chai';
import sinon from 'sinon';
import fs from 'fs';

import * as PromiseStatusModule from 'promise-status-async';
const promiseStatus       = PromiseStatusModule.promiseStatus;
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;

import { PathSegmenter } from '@keymanapp/gesture-recognizer';

import { HeadlessRecordingSimulator } from '../../../../build/tools/obj/index.js';

const SEGMENT_TEST_JSON_FOLDER = 'src/test/resources/json/segmentation';

import { assertSegmentSimilarity } from '../../resources/assertSegmentSimilarity.js';

describe("Segmentation - from recorded sequences", function() {
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

  it("simple_ne_move.json", async function() {
    // NOTE:  this recording's final 'hold' segment is somewhat tightly attuned to the DEFAULT_CONFIG
    // hold time setting.  Changing default values there may necessitate a hand-edit tweak to the
    // test recording's data in order for this test to continue passing as-is.

    let testJSONtext = fs.readFileSync(`${SEGMENT_TEST_JSON_FOLDER}/simple_ne_move.json`);
    let jsonObj = JSON.parse(testJSONtext);

    // Some special setup - we're going to capture the 'move' segment in-process and run a check that way.
    const recognitionTestCapturer = {
      recognizedPromise: null
    }

    // Spy:  the method we pass in is _actually called_ while _also_ capturing metadata for every call.
    let spy = sinon.spy((segment) => {
      if(segment.type == 'move' && !recognitionTestCapturer.recognizedPromise) {
        segment.whenRecognized.then(() => {
          recognitionTestCapturer.recognizedPromise = new Promise((resolve, reject) => {
            // Verify that we resolved because of distance, not time.
            assert.isAtLeast(segment.distance, PathSegmenter.DEFAULT_CONFIG.holdMoveTolerance);
            assert.isBelow(segment.duration, PathSegmenter.DEFAULT_CONFIG.holdMinimumDuration);

            // Makes sure the segment isn't resolved at the same time it is recognized.
            promiseStatus(segment.whenResolved).then((status) => {
              assert.equal(status, PromiseStatuses.PROMISE_PENDING);
              resolve();
            }).catch((err) => {
              reject(err);
            });
          });

          return recognitionTestCapturer.recognizedPromise;
        });
      }
    });

    // Now the normal basic setup.
    const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

    const configObj = {
      replaySample: (sample) => segmenter.add(sample),
      endSequence:  () => segmenter.close()
    }

    const testObj = HeadlessRecordingSimulator.prepareTest(jsonObj, configObj);

    // The fakeClock await must be first.  The other two should be fine in either order.
    await this.fakeClock.runAllAsync();
    await testObj.compositePromise;
    await recognitionTestCapturer.recognizedPromise;

    // Any post-sequence tests to run.
    const originalSegments = testObj.originalSegments;
    const originalSegmentTypeSequence = originalSegments.map((segment) => segment.type);

    const reproedSegments = spy.getCalls().map((call) => call.args[0]);
    const reproedSegmentTypeSequence  = reproedSegments.map((segment) => segment.type);

    assert.sameOrderedMembers(reproedSegmentTypeSequence, originalSegmentTypeSequence);

    // Ensure all relevant Promises resolved.
    for(let segment of reproedSegments) {
      assert.isTrue(await promiseStatus(segment.whenRecognized) == PromiseStatuses.PROMISE_RESOLVED);
      assert.isTrue(await promiseStatus(segment.whenResolved)   == PromiseStatuses.PROMISE_RESOLVED);
    }

    assertSegmentSimilarity(reproedSegments[1], originalSegments[1], 'hold'); // ~200ms
    assertSegmentSimilarity(reproedSegments[2], originalSegments[2], 'move'); // 'ne'
  });

  it("nonstationary_hold.json", async function() {
    let testJSONtext = fs.readFileSync(`${SEGMENT_TEST_JSON_FOLDER}/nonstationary_hold.json`);
    let jsonObj = JSON.parse(testJSONtext);

    // Some special setup - we're going to capture the 'hold' segment in-process and run a check that way.
    const recognitionTestCapturer = {
      recognizedPromise: null
    }

    // Spy:  the method we pass in is _actually called_ while _also_ capturing metadata for every call.
    let spy = sinon.spy((segment) => {
      if(segment.type == 'hold' && !recognitionTestCapturer.recognizedPromise) {
        segment.whenRecognized.then(() => {
          recognitionTestCapturer.recognizedPromise = new Promise((resolve, reject) => {
            // Verify that we resolved because of time, not distance.
            assert.isBelow(segment.distance, PathSegmenter.DEFAULT_CONFIG.holdMoveTolerance);
            assert.isAtLeast(segment.duration, PathSegmenter.DEFAULT_CONFIG.holdMinimumDuration);

            // Makes sure the segment isn't resolved at the same time it is recognized.
            promiseStatus(segment.whenResolved).then((status) => {
              assert.equal(status, PromiseStatuses.PROMISE_PENDING);
              resolve();
            }).catch((err) => {
              reject(err);
            });
          });

          return recognitionTestCapturer.recognizedPromise;
        });
      }
    });

    // Now the normal basic setup.
    const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

    const configObj = {
      replaySample: (sample) => segmenter.add(sample),
      endSequence:  () => segmenter.close()
    }

    const testObj = HeadlessRecordingSimulator.prepareTest(jsonObj, configObj);

    // The fakeClock await must be first.  The other two should be fine in either order.
    await this.fakeClock.runAllAsync();
    await testObj.compositePromise;
    await recognitionTestCapturer.recognizedPromise;

    // Any post-sequence tests to run.
    const originalSegments = testObj.originalSegments;
    const originalSegmentTypeSequence = originalSegments.map((segment) => segment.type);

    const reproedSegments = spy.getCalls().map((call) => call.args[0]);
    const reproedSegmentTypeSequence  = reproedSegments.map((segment) => segment.type);

    assert.sameOrderedMembers(reproedSegmentTypeSequence, originalSegmentTypeSequence);

    // Ensure all relevant Promises resolved.
    for(let segment of reproedSegments) {
      assert.isTrue(await promiseStatus(segment.whenRecognized) == PromiseStatuses.PROMISE_RESOLVED);
      assert.isTrue(await promiseStatus(segment.whenResolved)   == PromiseStatuses.PROMISE_RESOLVED);
    }

    assertSegmentSimilarity(reproedSegments[1], originalSegments[1], 'hold'); // ~1200ms
  });

  it("flick_ne_se.json", async function() {
    let testJSONtext = fs.readFileSync(`${SEGMENT_TEST_JSON_FOLDER}/flick_ne_se.json`);
    let jsonObj = JSON.parse(testJSONtext);

    // Prepares some basic setup - we won't set up segment-specific recognition tests here.
    let spy = sinon.fake();
    const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

    const configObj = {
      replaySample: (sample) => segmenter.add(sample),
      endSequence:  () => segmenter.close()
    }

    const testObj = HeadlessRecordingSimulator.prepareTest(jsonObj, configObj);

    await this.fakeClock.runAllAsync();
    await testObj.compositePromise;

    // Any post-sequence tests to run.
    const originalSegments = testObj.originalSegments;
    const originalSegmentTypeSequence = originalSegments.map((segment) => segment.type);

    const reproedSegments = spy.getCalls().map((call) => call.args[0]);
    const reproedSegmentTypeSequence  = reproedSegments.map((segment) => segment.type);

    assert.sameOrderedMembers(reproedSegmentTypeSequence, originalSegmentTypeSequence);

    // Ensure all relevant Promises resolved.
    for(let segment of reproedSegments) {
      assert.isTrue(await promiseStatus(segment.whenRecognized) == PromiseStatuses.PROMISE_RESOLVED);
      assert.isTrue(await promiseStatus(segment.whenResolved)   == PromiseStatuses.PROMISE_RESOLVED);
    }

    assertSegmentSimilarity(reproedSegments[1], originalSegments[1], 'hold');  // ~820ms
    assertSegmentSimilarity(reproedSegments[2], originalSegments[2], 'move');  // 'ne'
    assertSegmentSimilarity(reproedSegments[3], originalSegments[3], 'hold');  // ~580ms
    assertSegmentSimilarity(reproedSegments[4], originalSegments[4], 'move');  // 'se'
    assertSegmentSimilarity(reproedSegments[5], originalSegments[5], 'hold');  // ~300ms
  });

  it("longpress_to_ne.json", async function() {
    let testJSONtext = fs.readFileSync(`${SEGMENT_TEST_JSON_FOLDER}/longpress_to_ne.json`);
    let jsonObj = JSON.parse(testJSONtext);

    // Prepares some basic setup - we won't set up segment-specific recognition tests here.
    let spy = sinon.fake();
    const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

    const configObj = {
      replaySample: (sample) => segmenter.add(sample),
      endSequence:  () => segmenter.close()
    }

    const testObj = HeadlessRecordingSimulator.prepareTest(jsonObj, configObj);

    await this.fakeClock.runAllAsync();
    await testObj.compositePromise;

    // Any post-sequence tests to run.
    const originalSegments = testObj.originalSegments;
    const reproedSegments = spy.getCalls().map((call) => call.args[0]);

    // Because of the sweeping arc motion, we won't assume a perfect match to the segmentation here.

    // Ensure all relevant Promises resolved.
    for(let segment of reproedSegments) {
      assert.isTrue(await promiseStatus(segment.whenRecognized) == PromiseStatuses.PROMISE_RESOLVED);
      assert.isTrue(await promiseStatus(segment.whenResolved)   == PromiseStatuses.PROMISE_RESOLVED);
    }

    assertSegmentSimilarity(reproedSegments[1], originalSegments[1], 'hold');  // ~1460ms
    // there are actually a LOT of move segments here; the motion was in an arc.
    // will make a good test case for the eventual longpress gesture.
    assert.equal(reproedSegments[2].type, 'move');
    assert.equal(reproedSegments[2].direction, 'n'); // the initial direction once motion started.
  });

  it("quick_small_square.json", async function() {
    // NOTE:  this recording has a number of borderline-duration 'hold' segments between its moves.
    // Naturally, there has to be SOME kind of transition when sharply changing direction.
    // NOTE: We wish to ensure that each 'edge' of the 'square' remains reasonably distinct.

    let testJSONtext = fs.readFileSync(`${SEGMENT_TEST_JSON_FOLDER}/quick_small_square.json`);
    let jsonObj = JSON.parse(testJSONtext);

    // Prepares some basic setup - we won't set up segment-specific recognition tests here.
    let spy = sinon.fake();
    const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

    const configObj = {
      replaySample: (sample) => segmenter.add(sample),
      endSequence:  () => segmenter.close()
    }

    const testObj = HeadlessRecordingSimulator.prepareTest(jsonObj, configObj);

    await this.fakeClock.runAllAsync();
    await testObj.compositePromise;

    // Any post-sequence tests to run.
    const reproedSegments = spy.getCalls().map((call) => call.args[0]);

    // Ensure all relevant Promises resolved.
    for(let segment of reproedSegments) {
      assert.isTrue(await promiseStatus(segment.whenRecognized) == PromiseStatuses.PROMISE_RESOLVED);
      assert.isTrue(await promiseStatus(segment.whenResolved)   == PromiseStatuses.PROMISE_RESOLVED);
    }

    const holds = reproedSegments.filter((segment) => segment.type == 'hold');
    const moves = reproedSegments.filter((segment) => segment.type == 'move');
    const nulls = reproedSegments.filter((segment) => segment.type === null);

    assert.isEmpty(nulls);
    assert.isEmpty(holds.filter((hold) => hold.duration > 200)); // all motions were very quick.

    // True (intended) motions were 'e' -> 's' -> 'w' -> 'n', but the motions weren't that precise
    // due to prioritizing speed.
    const eMoveIndex = moves.findIndex((move) => move.direction.includes('e'));
    const sMoveIndex = moves.findIndex((move) => move.direction.includes('s'));
    const wMoveIndex = moves.findIndex((move) => move.direction.includes('w'));
    const nMoveIndex = moves.findIndex((move) => move.direction.includes('n'));

    assert.notEqual(eMoveIndex, -1);
    assert.notEqual(sMoveIndex, -1);
    assert.notEqual(wMoveIndex, -1);
    assert.notEqual(nMoveIndex, -1);

    // Again, the true (intended) motions were 'e' -> 's' -> 'w' -> 'n'.  This verifies the relative ordering.
    assert.isBelow(eMoveIndex, sMoveIndex);
    assert.isBelow(sMoveIndex, wMoveIndex);
    assert.isBelow(wMoveIndex, nMoveIndex);

    // Original's length: 12, but there are some borderline holds there & we don't want this test
    // to be too rigid.
    assert.isAtLeast(reproedSegments.length, 9);  // 2 = 'start' + 'end'
                                                  // 2 = 1 hold after start, 1 hold before end
                                                  // 4 cardinal directions
                                                  // At least one notable hold during direction changes
  });
});