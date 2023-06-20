import { EventEmitter } from "eventemitter3";

import {
  CumulativePathStats,
  type InputSample,
  type Subsegmentation
} from "@keymanapp/gesture-recognizer"

interface EventMap {
  'sample': (sample: InputSample) => void
}

/**
 * Designed to facilitate creation of 'synthetic' touchpath sample sequences for use
 * in unit tests.
 */
export class TouchpathTurtle extends EventEmitter<EventMap> {
  private readonly startSample: InputSample;
  private currentSample: InputSample;
  private currentStats: CumulativePathStats;
  private currentChop:  CumulativePathStats = null;

  private _pathSegments: Subsegmentation[] = [];

  constructor(obj: InputSample | TouchpathTurtle) {
    super();

    if(!(obj instanceof TouchpathTurtle)) {
      this.startSample = obj;
      this.currentSample = obj;
      this.currentStats = new CumulativePathStats(obj);

      this._pathSegments.push({
        stats: this.currentStats,
        baseAccumulation: null,
        endingAccumulation: this.currentStats
      });

      return;
    }

    // Do deep-copying.
    this.startSample   = obj.startSample;
    this.currentSample = obj.currentSample;
    this.currentStats  = new CumulativePathStats(obj.currentStats);
    this.currentChop   = new CumulativePathStats(obj.currentChop);
    this._pathSegments = [].concat(obj._pathSegments);
  }

  get pathComponents(): readonly Subsegmentation[] {
    return this._pathSegments;
  }

  get location(): InputSample {
    return this.currentSample;
  }

  wait(totalTime: number, repeatInterval: number): Subsegmentation {
    if(repeatInterval < 0 || totalTime < 0) {
      throw new Error("Invalid parameter value:  may not be negative!");
    }

    const segmentBaseStats = this.currentChop;
    const startSample = this.currentSample;

    // Base sample always exists in advance.
    let currentStats = this.currentStats;
    for(let timeDelta = 0; timeDelta < totalTime; timeDelta += repeatInterval) {
      let sample = {...startSample};
      sample.t += timeDelta;

      currentStats = currentStats.extend(sample);
    }

    this.currentSample = {...startSample};
    this.currentSample.t += totalTime;
    this.currentChop = currentStats;
    this.currentStats = currentStats.extend(this.currentSample);

    const pathComponent = {
      stats: this.currentStats.deaccumulate(segmentBaseStats),
      baseAccumulation: segmentBaseStats,
      endingAccumulation: this.currentStats
    }

    this._pathSegments.push(pathComponent);
    return pathComponent;
  }

  move(angleInDegrees: number, distance: number, time: number, interval: number): Subsegmentation {
    if(distance < 0 || time < 0 || interval < 0) {
      // negative angles are actually fine, so we don't filter those.
      throw new Error("Invalid parameter value:  may not be negative!");
    }

    // Angle is measured clockwise from the DOM's <0, -1>.
    let angle = angleInDegrees * Math.PI / 180;
    let xDistance = Math.cos(angle) * distance;
    let yDistance = -Math.sin(angle) * distance;

    const xTickDist = xDistance * interval / time;
    const yTickDist = yDistance * interval / time;

    const segmentBaseStats = this.currentChop;
    const startSample = this.currentSample;

    // While _definitely_ not a perfect match for a real input recording... we'll just interpolate evenly.
    let currentSample = {...startSample};
    let currentStats  = this.currentStats;

    for(let timeDelta = 0; timeDelta < time; timeDelta += interval) {
      currentSample = {
        targetX: currentSample.targetX + xTickDist,
        targetY: currentSample.targetY + yTickDist,
        t: currentSample.t + timeDelta
      };

      this.currentChop = currentStats;

      this.emit('sample', currentSample);
      currentStats = currentStats.extend(currentSample);
    }

    this.currentSample = {
      targetX: startSample.targetX + xDistance,
      targetY: startSample.targetY + yDistance,
      t: startSample.t + time
    };
    this.currentChop = currentStats;
    this.currentStats = currentStats.extend(this.currentSample);

    const pathComponent = {
      stats: this.currentStats.deaccumulate(segmentBaseStats),
      baseAccumulation: segmentBaseStats,
      endingAccumulation: this.currentStats
    }

    this._pathSegments.push(pathComponent);
    return pathComponent;
  }
}