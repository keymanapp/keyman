import {
  GestureSource,
  type InputSample
} from "@keymanapp/gesture-recognizer";

import { type HostFixtureLayoutController } from "./hostFixtureLayoutController.js";
import { type JSONObject } from "./jsonObject.js";
import { FixtureLayoutConfiguration } from "./fixtureLayoutConfiguration.js";
import { RecordedCoordSequenceSet } from "./inputRecording.js";
import { SequenceRecorder } from "./sequenceRecorder.js";

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
    const arrToTouchList = (arr: Touch[]): TouchList => {
      return {
        //@ts-ignore
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

  replayTouchSample(sample: JSONObject<InputSample<HoveredItemType>>,
                    state: string,
                    identifier: number,
                    otherTouches: Touch[],
                    targetElement?: HTMLElement): Touch {
    let config = this.controller.recognizer.config;

    let event: TouchEvent;
    const mappedSample = this.getSampleClientPos(sample);

    let touch: Touch;
    let touchDict = {identifier: identifier,
                      target: targetElement || config.targetRoot,
                      ...mappedSample};
    if(window['Touch'] !== undefined) {
      touch = new Touch(touchDict);
    } else {
      // When not performing touch-emulation, some desktop browsers will leave `Touch` undefined.
      touch = touchDict as any;
    }

    otherTouches = otherTouches || [];

    let touchEventDict: TouchEventInit = {
      bubbles: true,
      touches: [touch].concat(otherTouches),
      changedTouches: [touch],
    }

    let buildEvent = (type: string, dict: TouchEventInit) => {
      if(window['TouchEvent'] !== undefined) {
        // Nothing beats the real thing.
        return new TouchEvent(type, dict);
      } else {
        // Effectively, an internal polyfill.  But, if at ALL possible, use a real version instead!
        return this.buildSyntheticTouchEvent(type, dict);
      }
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

    return touch;
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
    replayExecutor: (func: () => void, sample?: InputSample<HoveredItemType>) => void): InputSample<HoveredItemType> {
    let inputs = sequenceTestSpec.inputs;
    const config = sequenceTestSpec.config;

    this.controller.layoutConfiguration = new FixtureLayoutConfiguration(config);

    /**
     * For each corresponding recorded sequence, notes the index of the sequence's
     * next sample to be 'replayed'.  If the full sequence has already been
     * replayed, its corresponding entry will be set to Number.MAX_VALUE instead.
     */
    let sequenceProgress: number[] = new Array(inputs.length).fill(0);
    let sequenceTouches: Touch[] = new Array(inputs.length).fill(null);

    let lastSample = null;

    while(sequenceProgress.find((number) => number != Number.MAX_VALUE) !== undefined) {
      // Determine the sequence that has the chronologically next-in-line sample
      // to reproduce.
      let minTimestamp = Number.MAX_VALUE;

      /**
       * Pulls double-duty as the index of the sequence owning said sample & as
       * the sequence's common 'identifier' value for emulated Touches.
       */
      let selectedSequence = -1;

      for(let index=0; index < inputs.length; index++) {
        // TODO:  does not iterate over all touchpoints.  Not that we can have more than one at present...
        const touchpoint = GestureSource.deserialize(inputs[index], index);
        const indexInSequence = sequenceProgress[index];

        if(indexInSequence == Number.MAX_VALUE) {
          continue;
        }

        if(touchpoint.path.coords[indexInSequence].t < minTimestamp) {
          minTimestamp = touchpoint.path.coords[indexInSequence].t;
          selectedSequence = index;
        }
      }

      const touchpoint = GestureSource.deserialize(inputs[selectedSequence], selectedSequence);
      const indexInSequence = sequenceProgress[selectedSequence];
      let state: string = "move";

      let appendEndEvent = false;
      if(indexInSequence + 1 >= touchpoint.path.coords.length) {
        sequenceProgress[selectedSequence] = Number.MAX_VALUE;
        appendEndEvent = true;
      } else {
        sequenceProgress[selectedSequence]++;
      }

      if(indexInSequence == 0) {
        state = "start";
      }

      const sample = touchpoint.path.coords[indexInSequence] as InputSample<HoveredItemType>;
      if(touchpoint.isFromTouch) {
        let otherTouches = [].concat(sequenceTouches);
        otherTouches.splice(selectedSequence, 1);
        // Now that we've removed the entry for the current touchpoint, filter out any null entries.
        otherTouches = otherTouches.filter((val) => !!val);

        replayExecutor(() => {
          sequenceTouches[selectedSequence] = this.replayTouchSample(sample, state, selectedSequence, otherTouches);
        }, sample);
        if(appendEndEvent) {
          // Synchronous in sync mode, but set with a timeout in async mode.
          replayExecutor(() => {
            this.replayTouchSample(sample, "end", selectedSequence, otherTouches);
          }, sample);
          sequenceTouches[selectedSequence] = null;
        }
      } else {
        replayExecutor(() => {
          this.replayMouseSample(sample, state);
        }, sample);
        if(appendEndEvent) {
          replayExecutor(() => {
            this.replayMouseSample(sample, "end");
          }, sample);
        }
      }

      lastSample = sample;
    }

    return lastSample;
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
    const finalSample = this.replayCore(sequenceTestSpec, (func, sample) => {
      window.setTimeout(() => {
        func();
      }, sample.t);
    });

    return new Promise((resolve) => {
      window.setTimeout(() => {
        // We technically don't have access to 'samples' for the actual recorded object.
        // Obtaining a 'deep copy' (though methodless) works around this nicely and
        // matches the original `sequenceTestSpec`'s format to boot.
        let recording = JSON.parse(recorder.recordingsToJSON()) as RecordedCoordSequenceSet;
        recorder.clear();
        resolve(recording);
      }, finalSample.t + 30);
    });
  }
}