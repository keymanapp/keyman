import EventEmitter from "eventemitter3";
import { InputSample } from "./inputSample.js";
import { CumulativePathStats } from "./cumulativePathStats.js";
import { Mutable } from "../mutable.js";

/**
 * Documents the expected typing of serialized versions of the `GesturePath` class.
 */
export type SerializedGesturePath<Type, StateToken> = {
  coords: Mutable<InputSample<Type, StateToken>>[]; // ensures type match with public class property.
  wasCancelled?: boolean;
}

interface EventMap<Type, StateToken> {
  'step': (sample: InputSample<Type, StateToken>) => void,
  'complete': () => void,
  'invalidated': () => void
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
export class GesturePath<Type, StateToken = any> extends EventEmitter<EventMap<Type, StateToken>> {
  private samples: InputSample<Type, StateToken>[] = [];

  private _isComplete: boolean = false;
  private _wasCancelled?: boolean;

  private _stats: CumulativePathStats<Type>;

  public get stats() {
    // Is (practically) immutable, so it's safe to expose the instance directly.
    return this._stats;
  }

  /**
   * Initializes an empty path intended for tracking a newly-activated touchpoint.
   */
  constructor() {
    super();

    this._stats = new CumulativePathStats();
  }

  public clone(): GesturePath<Type, StateToken> {
    const instance = new GesturePath<Type, StateToken>();
    instance.samples = [].concat(this.samples);
    instance._isComplete = this._isComplete;
    instance._wasCancelled = this._wasCancelled;
    instance._stats = new CumulativePathStats<Type>(this._stats);

    return instance;
  }

  /**
   * Deserializes a GesturePath instance from its corresponding JSON.parse() object.
   * @param jsonObj
   */
  static deserialize<Type, StateToken>(jsonObj: SerializedGesturePath<Type, StateToken>): GesturePath<Type, StateToken> {
    const instance = new GesturePath<Type, StateToken>();

    instance.samples = [].concat(jsonObj.coords.map((obj) => ({...obj} as InputSample<Type, StateToken>)));
    instance._isComplete = true;
    instance._wasCancelled = jsonObj.wasCancelled;

    let stats = instance.samples.reduce((stats: CumulativePathStats<Type>, sample) => stats.extend(sample), new CumulativePathStats<Type>());
    instance._stats = stats;

    return instance;
  }

  /**
   * Indicates whether or not the corresponding touchpoint is no longer active -
   * either due to cancellation or by the user's direct release of the touchpoint.
   */
  public get isComplete() {
    return this._isComplete;
  }

  public get wasCancelled() {
    return this._wasCancelled;
  }

  /**
   * Extends the path with a newly-observed coordinate.
   * @param sample
   */
  extend(sample: InputSample<Type, StateToken>) {
    /* c8 ignore next 3 */
    if(this._isComplete) {
      throw new Error("Invalid state:  this GesturePath has already terminated.");
    }

    // The tracked path should emit InputSample events before Segment events and
    // resolution of Segment Promises.
    this.samples.push(sample);
    this._stats = this._stats.extend(sample);
    this.emit('step', sample);
  }

  /**
   * Finalizes the path.
   * @param cancel Whether or not this finalization should trigger cancellation.
   */
  terminate(cancel: boolean = false) {
    /* c8 ignore next 3 */
    if(this._isComplete) {
      throw new Error("Invalid state:  this GesturePath has already terminated.");
    }
    this._wasCancelled = cancel;
    this._isComplete = true;

    // If cancelling, do so before finishing segments
    if(cancel) {
      this.emit('invalidated');
    } else {
      // If not cancelling, signal completion after finishing segments.
      this.emit('complete');
    }

    this.removeAllListeners();
  }

  /**
   * Returns all coordinate + timestamp pairings observed for the corresponding
   * touchpoint's path over its lifetime thus far.
   */
  public get coords(): readonly InputSample<Type, StateToken>[] {
    return this.samples;
  }

  /**
   * Creates a serialization-friendly version of this instance for use by
   * `JSON.stringify`.
   */
  toJSON() {
    let jsonClone: SerializedGesturePath<Type, StateToken> = {
      // Replicate array and its entries, but with certain fields of each entry missing.
      // No .clientX, no .clientY.
      coords: [].concat(this.samples.map((obj) => ({
        targetX: obj.targetX,
        targetY: obj.targetY,
        t:       obj.t,
        item:    obj.item
      }))),
      wasCancelled: this._wasCancelled
    }

    // Removes components of each sample that we don't want serialized.
    for(let sample of jsonClone.coords) {
      delete sample.clientX;
      delete sample.clientY;

      // No point in serializing an `undefined` 'item' entry.
      if(sample.item === undefined) {
        delete sample.item;
      }
    }

    return jsonClone;
  }
}