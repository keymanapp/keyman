import {
  InputSample,
  Segment
} from '@keymanapp/gesture-recognizer';
import { RecordedCoordSequenceSet } from './inputRecording.js';
import { timedPromise } from './timedPromise.js';

export class ProcessedSequenceTest<Type> {
  samplePromises: Promise<void>[];

  endPromise: Promise<void>;
  compositePromise: Promise<any>; // nasty-complex return type.

  originalSamples: InputSample<Type>[];
  originalSegments: Segment[];
}

// Designed to faciliate testing against both PathSegmenter and TrackedPath.
export interface RecordingTestConfig<Type> {
  replaySample: (sample: InputSample<Type>) => void;
  endSequence:  () => void;
}

// Note:  this class is currently only used by subsegmentation unit tests.
export class HeadlessRecordingSimulator {
  // Designed to test against PathSegmenter and TrackedPath - just implement the config interface appropriately!
  static prepareTest<Type>(recordingObj: RecordedCoordSequenceSet, config: RecordingTestConfig<Type>): ProcessedSequenceTest<Type> {
    const testObj = new ProcessedSequenceTest<Type>();

    // Next:  drill down to the relevant part(s).
    const sourceTrackedPath = recordingObj.inputs[0].touchpoints[0].path;

    testObj.originalSamples = sourceTrackedPath.coords;
    const sampleCount = testObj.originalSamples.length;
    // still exists on original recorded sequences that exist before we removed segmentation.
    testObj.originalSegments = sourceTrackedPath['segments'];
    const lastSegment = testObj.originalSegments[testObj.originalSegments.length-1];

    // Build promises designed to reproduce the events at the correct times.
    testObj.samplePromises = testObj.originalSamples.map((sample) => {
      return timedPromise(() => {
        config.replaySample(sample);
      }, sample.t - testObj.originalSamples[0].t);
    });


    // Originally we did not record the release-timing of the touch, instead using the timing of
    // the last sample for the last subsegmentation's last sample.  We've disabled that in order
    // to get out a release in a more timely manner, but we'll still use it first if it's available.
    //
    // If not... we use a more current style.  TODO:  resolve 'release timing' aspect of input
    // sequence recording + playback.
    const endTime = lastSegment ? lastSegment.lastCoord.t : sourceTrackedPath.coords[sampleCount-1].t;

    testObj.endPromise = timedPromise(() => {
      config.endSequence();
    }, endTime - testObj.originalSamples[0].t);

    // Wrap it all together with a nice little bow.
    testObj.compositePromise = Promise.all([testObj.endPromise].concat(testObj.samplePromises)).catch((reason) => {
      // Because we use a `setInterval` internally, we need cleanup if things go wrong.
      config.endSequence();
      throw reason;
    });

    return testObj;
  }
}