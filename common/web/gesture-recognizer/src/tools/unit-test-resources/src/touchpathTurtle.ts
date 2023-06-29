import {
  type InputSample
} from "@keymanapp/gesture-recognizer"

/**
 * Designed to facilitate creation of 'synthetic' touchpath sample sequences for use
 * in unit tests.
 */
export class TouchpathTurtle<HoveredItemType> {
  private pathSamples: InputSample<HoveredItemType>[];

  constructor(obj: InputSample<HoveredItemType> | TouchpathTurtle<HoveredItemType>) {
    if(!(obj instanceof TouchpathTurtle)) {
      this.pathSamples = [obj];
      return;
    }

    // Do deep-copying.
    this.pathSamples   = [].concat(obj.pathSamples);
  }

  get location(): InputSample<HoveredItemType> {
    return this.pathSamples[this.pathSamples.length-1];
  }

  get path(): InputSample<HoveredItemType>[] {
    return [].concat(this.pathSamples);
  }

  get hoveredItem(): HoveredItemType {
    return this.location.item;
  }

  set hoveredItem(item: HoveredItemType) {
    this.location.item = item;
  }

  protected trackSample(sample: InputSample<HoveredItemType>) {
    this.pathSamples.push(sample);

    /*
     * Future subsegmentation hook notes:
     *
     * 1. Have this class accept a callback that gets samples.
     * 2. Call it from this method.  That's why sample-adding has been
     *    centralized here.
     * 3. Define a separate class... SubsegmentationMockingTurtle(?)...
     *    to fulfill any subsegmentation-algorithm-mocking needs by
     *    making them from the turtle itself (as the original did).
     * 4. Use this class internally, but restore the code removed in THIS
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

  wait(totalTime: number, repeatInterval: number) {
    if(repeatInterval < 0 || totalTime < 0) {
      throw new Error("Invalid parameter value:  may not be negative!");
    }

    const startSample = this.location;

    // Base sample always exists in advance.
    for(let timeDelta = 0; timeDelta < totalTime; timeDelta += repeatInterval) {
      let sample = {...startSample};
      sample.t += timeDelta;
      this.trackSample(sample);
    }

    let currentSample = {...startSample};
    currentSample.t += totalTime;
    this.trackSample(currentSample);
  }

  move(angleInDegrees: number, distance: number, time: number, interval: number) {
    if(distance < 0 || time < 0 || interval < 0) {
      // negative angles are actually fine, so we don't filter those.
      throw new Error("Invalid parameter value:  may not be negative!");
    }

    // Angle is measured clockwise from the DOM's <0, -1>.
    let angle = (90 - angleInDegrees) * Math.PI / 180;
    let xDistance = Math.cos(angle) * distance;
    let yDistance = -Math.sin(angle) * distance;

    const xTickDist = xDistance * interval / time;
    const yTickDist = yDistance * interval / time;

    const startSample = this.location;

    // While _definitely_ not a perfect match for a real input recording... we'll just interpolate evenly.
    let currentSample = {...startSample};

    for(let timeDelta = 0; timeDelta < time; timeDelta += interval) {
      currentSample = {
        targetX: currentSample.targetX + xTickDist,
        targetY: currentSample.targetY + yTickDist,
        t: startSample.t + timeDelta
      };

      this.trackSample(currentSample);
    }

    currentSample = {
      targetX: startSample.targetX + xDistance,
      targetY: startSample.targetY + yDistance,
      t: startSample.t + time
    };
    this.trackSample(currentSample);
  }
}