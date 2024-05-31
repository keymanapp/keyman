import {
  GestureDebugPath,
  GestureDebugSource,
  type InputSample
} from "@keymanapp/gesture-recognizer";

import { type HostFixtureLayoutController } from "./hostFixtureLayoutController.js";
import { type JSONObject } from "./jsonObject.js";
import { FixtureLayoutConfiguration } from "./fixtureLayoutConfiguration.js";
import { RecordedCoordSequenceSet } from "./inputRecording.js";
import { SequenceRecorder } from "./sequenceRecorder.js";

interface ChangedTouchData<Type> {
  sample: InputSample<Type>,
  identifier: number
};

type InputState = 'start' | 'move' | 'end';

/**
 * This class is designed to 'replay' a recorded InputSequence's DOM events to ensure
 * that the recognizer's DOM layer is working correctly.
 */
export class InputSequenceSimulator<HoveredItemType> {
  private controller: HostFixtureLayoutController;

  constructor(controller: HostFixtureLayoutController) {
    this.controller = controller;
  }

  getSampleClientPos(sample: JSONObject<InputSample<HoveredItemType>>): {clientX: number, clientY: number} {
    let targetRoot = this.controller.recognizer.config.targetRoot;
    let targetRect = targetRoot.getBoundingClientRect();

    return {
      clientX: sample.targetX + targetRect.left,
      clientY: sample.targetY + targetRect.top
    }
  }

  private buildSyntheticTouchEvent(name: string, dict: TouchEventInit): TouchEvent {
    let config = this.controller.recognizer.config;

    let baseDict = {
      srcElement: config.touchEventRoot,
      view: window
    } as any;

    if(dict.bubbles !== undefined) {
      baseDict.bubbles = dict.bubbles; // is an event read-only property; can't assign it later!
      delete dict.bubbles;
    }

    // Produces a hacky-but-sufficient implementation of TouchList for our purposes
    // in environments that avoid direct touch-support.
    const arrToTouchList = (arr: Touch[]): TouchList & { _arr: Touch[]} => {
      return {
        _arr: arr, // Obviously, this isn't a standard member of TouchList.
        length: arr.length,
        item: function(i: number) { return this._arr[i]; }
      }
    }

    //@ts-ignore
    dict.touches = arrToTouchList(dict.touches);
    //@ts-ignore
    dict.changedTouches = arrToTouchList(dict.changedTouches);

    let event = new Event(name, baseDict) as TouchEvent;

    // Since touch-related properties aren't expected by some browsers' Event constructors,
    // we must add them "in post" when testing in some non-touch environments.  For those,
    // the Event constructor will neglect to copy them from `baseDict`.
    Object.assign(event, dict);
    return event;
  }

  replayTouchSamples(changedTouchData: ChangedTouchData<HoveredItemType>[],
                    state: InputState,
                    recentTouches: Touch[],
                    targetElement?: HTMLElement): Touch[] {
    let config = this.controller.recognizer.config;

    let event: TouchEvent;

    const changedTouches = changedTouchData.map((data) => {
      const mappedSample = this.getSampleClientPos(data.sample);

      let touch: Touch;
      let touchDict = {
        identifier: data.identifier,
        target: targetElement || config.targetRoot,
        ...mappedSample
      };

      if(window['Touch'] !== undefined) {
        try {
          touch = new Touch(touchDict);
          return touch;
        } catch {}
      }

      // When not performing touch-emulation, some desktop browsers will leave `Touch` undefined.
      return touchDict as any;
    });

    // Now that we've removed the entries that match any changed touchpoints, filter out any null entries.
    let otherTouches = recentTouches.concat([]).filter((entry) => !!entry);
    otherTouches = otherTouches.filter((a) => !changedTouches.find((b) => a.identifier == b.identifier));

    let touchEventDict: TouchEventInit = {
      bubbles: true,
      // Ending touchpoints should NOT show up in `touches`.
      touches: state == 'end' ? otherTouches : changedTouches.concat(otherTouches),
      changedTouches: changedTouches,
    }

    let buildEvent = (type: string, dict: TouchEventInit) => {
      if(window['TouchEvent'] !== undefined) {
        // Nothing beats the real thing.
        try {
          return new TouchEvent(type, dict);
        } catch {};
      }

      // Effectively, an internal polyfill.  But, if at ALL possible, use a real version instead!
      return this.buildSyntheticTouchEvent(type, dict);
    }

    switch(state) {
      case 'start':
        event = buildEvent('touchstart', touchEventDict);
        break;
      case 'move':
        event = buildEvent('touchmove', touchEventDict);
        break;
      case 'end':
        event = buildEvent('touchend', touchEventDict);
        break;
    }

    config.targetRoot.dispatchEvent(event);

    return changedTouches;
  }

  replayMouseSample(sample: JSONObject<InputSample<HoveredItemType>>, state: string, targetElement?: HTMLElement) {
    let config = this.controller.recognizer.config;

    let event: MouseEvent;
    let eventDict: MouseEventInit = {
      bubbles: true,
      buttons: state == 'end' ? 0 : 1,
      ...this.getSampleClientPos(sample)
    };

    switch(state) {
      case 'start':
        event = new MouseEvent('mousedown', eventDict);
        break;
      case 'move':
        event = new MouseEvent('mousemove', eventDict);
        break;
      case 'end':
        event = new MouseEvent('mouseup', eventDict);
        break;
    }

    targetElement = targetElement || config.targetRoot;

    // Implies that `targetElement` is the target element; .target cannot be set separately
    // for mouse events, so we can't use config.mouseEventRoot.  Don't worry, the
    // listener will still receive the event b/c bubbling.
    targetElement.dispatchEvent(event);
  }

  /**
   * Given a previously-recorded sequence of sampled input coordinates, this function
   * reproduces their corresponding events and runs them through this instance's linked
   * gesture-recognizer instance.
   *
   * It will re-use the original fixture-layout configuration.
   * @param sequenceTestSpec A previously-recorded sequence of sampled input coordinates.
   * @param replayExecutor A middleman function to use when simulating the actual events.
   * @returns The final sample to be simulated.
   */
  private replayCore(sequenceTestSpec: RecordedCoordSequenceSet,
    replayExecutor: (func: () => void, timestamp: number) => void): number {
    let inputs = sequenceTestSpec.inputs;
    const config = sequenceTestSpec.config;

    this.controller.layoutConfiguration = new FixtureLayoutConfiguration(config);

    const touchpoints = inputs.map((input, index) => GestureDebugSource.deserialize(input, index));

    /**
     * For each corresponding recorded sequence, notes the index of the sequence's
     * next sample to be 'replayed'.  If the full sequence has already been
     * replayed, its corresponding entry will be set to Number.MAX_VALUE instead.
     */
    let sequenceProgress: number[] = new Array(inputs.length).fill(0);
    let sequenceTouches: Touch[] = new Array(inputs.length).fill(null);

    let lastTimestamp = null;

    while(sequenceProgress.find((number) => number != Number.MAX_VALUE) !== undefined) {
      // Determine the sequence that has the chronologically next-in-line sample
      // to reproduce.
      let minTimestamp = Number.MAX_VALUE;

      /**
       * Pulls double-duty as the index of the sequence owning said sample & as
       * the sequence's common 'identifier' value for emulated Touches.
       */
      let selectedSequences = [-1];

      for(let index=0; index < inputs.length; index++) {
        const touchpoint = GestureDebugSource.deserialize(inputs[index], index);
        const path = touchpoint.path as GestureDebugPath<any>;
        const indexInSequence = sequenceProgress[index];

        if(indexInSequence == Number.MAX_VALUE) {
          continue;
        }

        if(path.coords[indexInSequence].t < minTimestamp) {
          minTimestamp = path.coords[indexInSequence].t;
          selectedSequences = [index];
        } else if (path.coords[indexInSequence].t == minTimestamp) {
          selectedSequences.push(index);
        }
      }

      const preprocessing = selectedSequences.map((inputIndex) => {
        const touchpoint = touchpoints[inputIndex];
        const path = touchpoint.path as GestureDebugPath<any>;
        const indexInSequence = sequenceProgress[inputIndex];

        let appendEndEvent = false;
        if(indexInSequence + 1 >= path.coords.length) {
          sequenceProgress[inputIndex] = Number.MAX_VALUE;
          appendEndEvent = true;
        } else {
          sequenceProgress[inputIndex]++;
        }

        let state: string = "move";

        if(indexInSequence == 0) {
          state = "start";
        }

        const sample = path.coords[indexInSequence] as InputSample<HoveredItemType>;

        return {
          sample: sample,
          state: state,
          appendEndEvent: appendEndEvent,
          identifier: inputIndex
        };
      });

      // Assumption:  all are touch or all are mouse - no heterogenous mixtures.
      const sampleReplayer = touchpoints[0].isFromTouch
        ? (samples: ChangedTouchData<HoveredItemType>[], state: InputState) => {
          replayExecutor(() => {
            const replayedSamples = this.replayTouchSamples(samples, state, sequenceTouches);

            replayedSamples.forEach((touch, index) => {
              sequenceTouches[samples[index].identifier] = state == 'end' ? null : touch;
            });
          }, minTimestamp);
        }
        : (samples: ChangedTouchData<HoveredItemType>[], state: InputState) => {
          replayExecutor(() => {
            // There's only ever a single mouse contact point, so its emulation is far simpler.
            this.replayMouseSample(samples[0].sample, state);
          }, minTimestamp);
        };

      const startTouches = preprocessing.filter((value) => value.state == 'start');
      if(startTouches.length > 0) {
        sampleReplayer(startTouches, 'start');
      }

      const moveTouches = preprocessing.filter((value) => value.state == 'move');
      if(moveTouches.length > 0) {
        sampleReplayer(moveTouches, 'move');
      }

      const endTouches = preprocessing.filter((value) => value.appendEndEvent);
      if(endTouches.length > 0) {
        sampleReplayer(endTouches, 'end');
      }

      lastTimestamp = minTimestamp;
    }

    return lastTimestamp;
  }

  /**
   * Given a previously-recorded sequence of sampled input coordinates, this function
   * reproduces their corresponding events and runs them through this instance's linked
   * gesture-recognizer instance.  The original timestamps are ignored; only the relative
   * order of events will be enforced.
   *
   * It will re-use the original fixture-layout configuration.
   * @param sequenceTestSpec A previously-recorded sequence of sampled input coordinates.
   * @returns A recording of the reproduced version of that sequence.
   */
    replaySync(sequenceTestSpec: RecordedCoordSequenceSet): RecordedCoordSequenceSet {
    // We're going to record the test spec as we replay it, eventually returning the
    // final result.
    const recorder = new SequenceRecorder(this.controller);

    // We're operating synchronously, so just simulate the events on the spot.
    this.replayCore(sequenceTestSpec, (func) => { func() });

    // We technically don't have access to 'samples' for the actual recorded object.
    // Obtaining a 'deep copy' (though methodless) works around this nicely and
    // matches the original `sequenceTestSpec`'s format to boot.
    let recording = JSON.parse(recorder.recordingsToJSON()) as RecordedCoordSequenceSet;
    recorder.clear();
    return recording;
  }


  /**
   * Given a previously-recorded sequence of sampled input coordinates, this function
   * reproduces their corresponding events and runs them through this instance's linked
   * gesture-recognizer instance.  The original timestamps will be used; if a timer-stubbing
   * package (like SinonJS) is used, they should be reproduced to ~1ms precision.
   *
   * (Experimentally, the actual value tends to be Math.floor() - not Math.round().)
   *
   * It will re-use the original fixture-layout configuration.
   * @param sequenceTestSpec A previously-recorded sequence of sampled input coordinates.
   * @returns A Promise for a recording of the reproduced version of that sequence.
   */
  replayAsync(sequenceTestSpec: RecordedCoordSequenceSet): Promise<RecordedCoordSequenceSet> {
    // We're going to record the test spec as we replay it, eventually returning the
    // final result.
    const recorder = new SequenceRecorder(this.controller);

    // We're operating asynchronously, so we wrap each simulated event in a timeout.
    // NOTE:  this will be much less precise without stubbed timeout systems like the one
    // included within SinonJS.
    const finalTimestamp = this.replayCore(sequenceTestSpec, (func, timestamp) => {
      window.setTimeout(() => {
        func();
      }, timestamp);
    });

    return new Promise((resolve) => {
      window.setTimeout(() => {
        // We technically don't have access to 'samples' for the actual recorded object.
        // Obtaining a 'deep copy' (though methodless) works around this nicely and
        // matches the original `sequenceTestSpec`'s format to boot.
        let recording = JSON.parse(recorder.recordingsToJSON()) as RecordedCoordSequenceSet;
        recorder.clear();
        resolve(recording);
      }, finalTimestamp + 30);
    });
  }
}