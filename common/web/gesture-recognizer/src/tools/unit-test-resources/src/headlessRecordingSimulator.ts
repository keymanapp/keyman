import {
  InputSample,
  Segment
} from '@keymanapp/gesture-recognizer';
import { RecordedCoordSequenceSet } from './inputRecording.js';
import { timedPromise } from './timedPromise.js';

export class ProcessedSequenceTest {
  samplePromises: Promise<void>[];

  endPromise: Promise<void>;
  compositePromise: Promise<any>; // nasty-complex return type.

  originalSamples: InputSample[];
  originalSegments: Segment[];
}

// Designed to faciliate testing against both PathSegmenter and TrackedPath.
export interface RecordingTestConfig {
  replaySample: (sample: InputSample) => void;
  endSequence:  () => void;
}

export class HeadlessRecordingSimulator {
  // Designed to test against PathSegmenter and TrackedPath - just implement the config interface appropriately!
  static prepareTest(recordingObj: RecordedCoordSequenceSet, config: RecordingTestConfig): ProcessedSequenceTest {
    const testObj = new ProcessedSequenceTest();

    // Next:  drill down to the relevant part(s).
    const sourceTrackedPath = recordingObj.inputs[0].touchpoints[0].path;

    testObj.originalSamples = sourceTrackedPath.coords;
    testObj.originalSegments = sourceTrackedPath.segments;
    const lastSegment = testObj.originalSegments[testObj.originalSegments.length-1];

    // Build promises designed to reproduce the events at the correct times.
    testObj.samplePromises = testObj.originalSamples.map((sample) => {
      return timedPromise(() => {
        config.replaySample(sample);
      }, sample.t - testObj.originalSamples[0].t);
    });

    testObj.endPromise = timedPromise(() => {
      config.endSequence();
    }, lastSegment.lastCoord.t - testObj.originalSamples[0].t);

    // Wrap it all together with a nice little bow.
    testObj.compositePromise = Promise.all([testObj.endPromise].concat(testObj.samplePromises)).catch((reason) => {
      // Because we use a `setInterval` internally, we need cleanup if things go wrong.
      config.endSequence();
      throw reason;
    });

    return testObj;
  }
}