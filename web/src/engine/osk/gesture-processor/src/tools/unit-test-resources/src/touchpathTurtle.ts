import { EventEmitter } from "eventemitter3";

import {
  type InputSample
} from "@keymanapp/gesture-recognizer"

interface EventMap<Type> {
  /**
   * Emits samples once they are fully specified.  Note that there may be a slight delay enforced -
   * this allows the 'hovered item' to be altered after any turtle pathing command before the
   * corresponding sample is published to the event.
   *
   * The slight delay also facilitates emitting the sample provided during initialization, simplifying
   * its integration with path-oriented unit tests.
   */
  'sample': (sample: InputSample<Type>) => void
}

/**
 * Designed to facilitate creation of 'synthetic' touchpath sample sequences for use
 * in unit tests.
 */
export class TouchpathTurtle<HoveredItemType> extends EventEmitter<EventMap<HoveredItemType>> {
  private pathSamples: InputSample<HoveredItemType>[];
  private pendingSample: InputSample<HoveredItemType> = null;

  constructor(obj: InputSample<HoveredItemType> | TouchpathTurtle<HoveredItemType>) {
    super();

    if(!(obj instanceof TouchpathTurtle)) {
      this.pathSamples = [];
      this.pendingSample = obj;
      return;
    }

    // Do deep-copying.
    this.pathSamples   = [].concat(obj.pathSamples);
    this.pendingSample = obj.pendingSample;
  }

  get location(): InputSample<HoveredItemType> {
    if(this.pendingSample) {
      return this.pendingSample;
    } else {
      return this.pathSamples[this.pathSamples.length-1];
    }
  }

  get path(): InputSample<HoveredItemType>[] {
    return [].concat(this.pathSamples);
  }

  get hoveredItem(): HoveredItemType {
    return this.location.item;
  }

  set hoveredItem(item: HoveredItemType) {
    if(this.pendingSample) {
      this.pendingSample.item = item;
    } else {
      this.pendingSample = {
        ...this.location,
        item: item
      }
    }
  }

  /**
   * Force-publishes the most recently-generated sample.  By default, the most recent sample
   * is left unpublished in case the its 'item' entry needs to be altered.
   */
  public commitPending() {
    const pending = this.pendingSample;
    if(pending) {
      this.pathSamples.push(pending);
      this.emit('sample', pending);
    }
    this.pendingSample = null;
    return pending;
  }

  protected trackSample(sample: InputSample<HoveredItemType>) {
    this.commitPending();
    this.pendingSample = sample;

    /*
     * Future subsegmentation hook notes:
     *
     * 1. Define a separate class... SubsegmentationMockingTurtle(?)...
     *    to fulfill any subsegmentation-algorithm-mocking needs by
     *    making them from the turtle itself (as the original did).
     * 2. Use this class internally, but restore the code removed in THIS
     *    commit (use `git blame` to confirm) in THAT class.
     *    - that class would be focused on managing the stats objects
     *    - it would also provide the original signatures for `wait` and
     *      `move`, returning Subsegmentation by wrapping THIS class's
     *      version of the methods & tracking 'start' and 'end' state
     *      CumulativePathStats as before.
     *
     * https://github.com/keymanapp/keyman/pull/7440/ should hopefully
     * maintain enough relevant preserved history to bypass the need for
     * `git blame`.
     */
  }

  wait(totalTime: number, sampleCount: number) {
    if(sampleCount <= 0 || totalTime < 0) {
      throw new Error("Invalid parameter value: totalTime may not be negative and sampleCount must be > 0!");
    }

    const startSample = this.location;
    const timeDelta = totalTime / sampleCount;

    // Base sample always exists in advance.
    for(let i = 1; i <= sampleCount; i++) {
      let sample = {...startSample};
      sample.t += timeDelta * i;
      this.trackSample(sample);
    }
  }

  /**
   * @param angleInDegrees   Measured clockwise from the DOM's <0, -1>.
   * @param distance         Net pixel distance to move
   * @param time             Total time to model for the move
   * @param sampleCount      Total number of steps to reach the destination
   */
  move(angleInDegrees: number, distance: number, time: number, sampleCount: number) {
    if(distance < 0 || time < 0 || sampleCount < 1) {
      // negative angles are actually fine, so we don't filter those.
      throw new Error("Invalid parameter value:  may not be negative!");
    }

    let angle = (90 - angleInDegrees) * Math.PI / 180;
    let xDistance = Math.cos(angle) * distance;
    let yDistance = -Math.sin(angle) * distance;

    const xTickDist = xDistance / sampleCount;
    const yTickDist = yDistance / sampleCount;
    const tTickDist = time / sampleCount;

    const startSample = this.location;

    // While _definitely_ not a perfect match for a real input recording... we'll just interpolate evenly.
    let currentSample = {...startSample};

    for(let i = 1; i <= sampleCount; i++) {
      currentSample = {
        targetX: startSample.targetX + xTickDist * i,
        targetY: startSample.targetY + yTickDist * i,
        t: startSample.t + tTickDist * i,
        item: startSample.item
      };

      this.trackSample(currentSample);
    }
  }
}