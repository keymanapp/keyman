import {
  InputEngineBase,
  SerializedGestureSource,
  GestureSource
} from '@keymanapp/gesture-recognizer';

import { RecordedCoordSequenceSet } from './inputRecording.js';
import { timedPromise } from '@keymanapp/web-utils';

export class HeadlessInputEngine<Type = any> extends InputEngineBase<Type> {
  private PATH_ID_SEED = 1;

  // Should generally keep a simple, parameterless default constructor.
  constructor() {
    super();
  }

  public preparePathPlayback(recordedPoint: SerializedGestureSource) {
    return this.playbackRecording({
      inputs: [
        recordedPoint
      ],
      config: null
    });
  }

  private prepareSourceStart(recordedSource: SerializedGestureSource) {
    const headSample = recordedSource.path.coords[0];

    const pathID = this.PATH_ID_SEED++;
    let replaySource = new GestureSource<Type>(pathID, null, recordedSource.isFromTouch);
    replaySource.update(headSample); // is included before the point is made available.

    const startPromise = timedPromise(headSample.t).then(() => {
      this.addTouchpoint(replaySource);
      this.emit('pointstart', replaySource);
    });

    return {
      promise: startPromise,
      source: replaySource
    }
  }

  private replaySourceSamples(replaySource: GestureSource<Type>, recording: SerializedGestureSource) {
    const originalSamples = recording.path.coords;
    const tailSamples = originalSamples.slice(1);

    const samplePromises = tailSamples.map((sample) => {
      return timedPromise(sample.t).then(() => {
        if(replaySource.isPathComplete) {
          return;
        }
        replaySource.update(sample);
      });
    });

    return samplePromises;
  }

  private playbackTerminations(replaySource: GestureSource<Type>, recording: SerializedGestureSource) {
    const sampleCount = recording.path.coords.length;
    const endTime = recording.path.coords[sampleCount-1].t;
    return timedPromise(endTime).then(() => {
      if(replaySource.isPathComplete) {
        return;
      }
      this.dropTouchpointWithId(replaySource.rawIdentifier);
      replaySource.terminate(recording.path.wasCancelled);
    });
  }

  async playbackRecording(recordedObj: RecordedCoordSequenceSet) {
    const playbackStartTuples = recordedObj.inputs.map((recording) => this.prepareSourceStart(recording));
    const playbackStarts = playbackStartTuples.map((tuple) => tuple.promise);
    const sources = playbackStartTuples.map((tuple) => tuple.source);

    const playbackMiddles = recordedObj.inputs.map((recording, index) => this.replaySourceSamples(sources[index], recording));

    const playbackTerminations = recordedObj.inputs.map((recording, index) => this.playbackTerminations(sources[index], recording));

    await Promise.all(([] as Promise<any>[])
      .concat(playbackStarts)
      .concat(playbackMiddles.reduce((fullSet, set) => { return fullSet.concat(set)}, []))
      .concat(playbackTerminations)
    );
  }
}