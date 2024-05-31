import { InputSample } from "./inputSample.js";
import { CumulativePathStats } from "./cumulativePathStats.js";
import { Mutable } from "../mutable.js";
import { GesturePath } from "./gesturePath.js";

/**
 * Documents the expected typing of serialized versions of the `GesturePath` class.
 */
export type SerializedGesturePath<Type, StateToken> = {
  coords: Mutable<InputSample<Type, StateToken>>[]; // ensures type match with public class property.
  wasCancelled?: boolean;
  stats?: CumulativePathStats
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
export class GestureDebugPath<Type, StateToken = any> extends GesturePath<Type, StateToken> {
  private samples: InputSample<Type, StateToken>[] = [];

  public clone(): GestureDebugPath<Type, StateToken> {
    const instance = new GestureDebugPath<Type, StateToken>();
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
  static deserialize<Type, StateToken>(jsonObj: SerializedGesturePath<Type, StateToken>): GestureDebugPath<Type, StateToken> {
    const instance = new GestureDebugPath<Type, StateToken>();

    instance.samples = [].concat(jsonObj.coords.map((obj) => ({...obj} as InputSample<Type, StateToken>)));
    instance._isComplete = true;
    instance._wasCancelled = jsonObj.wasCancelled;

    let stats = instance.samples.reduce((stats: CumulativePathStats<Type>, sample) => stats.extend(sample), new CumulativePathStats<Type>());
    instance._stats = stats;

    return instance;
  }

  /**
   * Extends the path with a newly-observed coordinate.
   * @param sample
   */
  extend(sample: InputSample<Type, StateToken>) {
    /* c8 ignore next 3 */
    if(this.isComplete) {
      throw new Error("Invalid state:  this GesturePath has already terminated.");
    }

    // The tracked path should emit InputSample events before Segment events and
    // resolution of Segment Promises.
    this.samples.push(sample);
    super.extend(sample);
  }


  public translateCoordSystem(functor: (sample: InputSample<Type, StateToken>) => InputSample<Type, StateToken>) {
    super.translateCoordSystem(functor);

    for(let i=0; i < this.samples.length; i++) {
      this.samples[i] = functor(this.samples[i]);
    }
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
      wasCancelled: this.wasCancelled,
      stats: this.stats
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