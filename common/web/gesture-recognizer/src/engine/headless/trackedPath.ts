import EventEmitter from "eventemitter3";
import { InputSample } from "./inputSample.js";
import { Segment } from "./segment.js";
import { CumulativePathStats } from "./cumulativePathStats.js";

/**
 * Documents the expected typing of serialized versions of the `TrackedPoint` class.
 */
export type JSONTrackedPath = {
  coords: InputSample[]; // ensures type match with public class property.
  wasCancelled?: boolean;
}

interface EventMap {
  'step': (sample: InputSample) => void,
  'complete': () => void,
  'invalidated': () => void
  'segmentation': (segment: Segment) => void
}

/**
 * Models the path over time through coordinate space taken by a touchpoint during
 * its active lifetime.
 *
 *  _Supported events_:
 *
 * `'step'`: a new Event has been observed for this touchpoint, extending the path.
 * - Parameters:
 *   - `sample: InputSample` - the coordinate & timestamp of the new observation.
 *
 * `'complete'`: the touchpoint is no longer active; a touch-end has been observed.
 *   - Provides no parameters.
 *   - Will be the last event raised by its instance, after any final 'segmentation'
 *     events.
 *   - Still precedes resolution Promise fulfillment on the `Segment` provided by
 *     the most recently-preceding 'segmentation' event.
 *     - And possibly recognition Promise fulfillment.
 *
 * `'invalidated'`: the touchpoint is no longer active; the path has crossed
 * gesture-recognition boundaries and is no longer considered valid.
 *   - Provides no parameters.
 *   - Will precede the final 'segmentation' event for the 'end' segment
 *   - Will precede resolution Promise fulfillment on the `Segment` provided by
 *     the most recently-preceding 'segmentation' event.
 *     - And possibly recognition Promise fulfillment.
 */
export class TrackedPath extends EventEmitter<EventMap> {
  private samples: InputSample[] = [];

  private _isComplete: boolean = false;
  private wasCancelled?: boolean;

  private stats: CumulativePathStats;

  /**
   * Initializes an empty path intended for tracking a newly-activated touchpoint.
   */
  constructor();
  /**
   * Deserializes a TrackedPath instance from its corresponding JSON.parse() object.
   * @param jsonObj
   */
  constructor(jsonObj: JSONTrackedPath)
  constructor(jsonObj?: JSONTrackedPath) {
    super();

    if(jsonObj) {
      this.samples = [].concat(jsonObj.coords.map((obj) => ({...obj} as InputSample)));
      // If we're reconstructing this from a JSON.parse, it's a previously-recorded,
      // completed path.
      this._isComplete = true;
      this.wasCancelled = jsonObj.wasCancelled;
    }

    this.stats = new CumulativePathStats();
  }

  /**
   * Indicates whether or not the corresponding touchpoint is no longer active -
   * either due to cancellation or by the user's direct release of the touchpoint.
   */
  public get isComplete() {
    return this._isComplete;
  }

  /**
   * Extends the path with a newly-observed coordinate.
   * @param sample
   */
  extend(sample: InputSample) {
    if(this._isComplete) {
      throw new Error("Invalid state:  this TrackedPath has already terminated.");
    }

    // The tracked path should emit InputSample events before Segment events and
    // resolution of Segment Promises.
    this.samples.push(sample);
    this.stats = this.stats.extend(sample);
    this.emit('step', sample);
  }

  /**
   * Finalizes the path.
   * @param cancel Whether or not this finalization should trigger cancellation.
   */
  terminate(cancel: boolean = false) {
    if(this._isComplete) {
      throw new Error("Invalid state:  this TrackedPath has already terminated.");
    }
    this.wasCancelled = cancel;
    this._isComplete = true;

    // If cancelling, do so before finishing segments
    if(cancel) {
      this.emit('invalidated');
    }

    // If not cancelling, signal completion after finishing segments.
    if(!cancel) {
      this.emit('complete');
    }

    this.removeAllListeners();
  }

  /**
   * Returns all coordinate + timestamp pairings observed for the corresponding
   * touchpoint's path over its lifetime thus far.
   */
  public get coords(): readonly InputSample[] {
    return this.samples;
  }

  /**
   * Creates a serialization-friendly version of this instance for use by
   * `JSON.stringify`.
   */
  toJSON() {
    let jsonClone: JSONTrackedPath = {
      // Replicate array and its entries, but with certain fields of each entry missing.
      // No .clientX, no .clientY.
      coords: [].concat(this.samples.map((obj) => ({
        targetX: obj.targetX,
        targetY: obj.targetY,
        t:       obj.t
      }))),
      wasCancelled: this.wasCancelled
    }

    // Removes components of each sample that we don't want serialized.
    for(let sample of jsonClone.coords) {
      delete sample.clientX;
      delete sample.clientY;
    }

    return jsonClone;
  }
}