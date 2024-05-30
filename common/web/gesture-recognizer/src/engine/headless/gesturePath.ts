import { EventEmitter } from "eventemitter3";
import { InputSample } from "./inputSample.js";
import { CumulativePathStats } from "./cumulativePathStats.js";
import { Mutable } from "../mutable.js";

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
  protected _isComplete: boolean = false;
  protected _wasCancelled?: boolean;

  protected _stats: CumulativePathStats<Type>;

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
    instance._isComplete = this._isComplete;
    instance._wasCancelled = this._wasCancelled;
    instance._stats = new CumulativePathStats<Type>(this._stats);

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
   * Builds a new instance with equal stats and with translated initialSample and
   * lastSample coordinates.  Further accumulation will be based upon the new
   * coordinate system as well.
   * @param functor
   */
  public translateCoordSystem(functor: (sample: InputSample<Type, StateToken>) => InputSample<Type, StateToken>) {
    this._stats = this._stats.translateCoordSystem(functor);
  }

  /**
   * Builds a new instance with its initial sample replaced and stats updated
   * to reflect the alternate starting position.
   *
   * Note that `rawDistance` adjustments are an approximation, not exact.  To
   * be precise, for stats representing two more more samples, the distance
   * between the original and new initial samples is added as a flat amount.
   * @param sample
   */
  public replaceInitialSample(sample: InputSample<Type, StateToken>) {
    this._stats = this._stats.replaceInitialSample(sample);
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
      return;
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

  public toJSON(): any {
    return {
      // Replicate array and its entries, but with certain fields of each entry missing.
      // No .clientX, no .clientY.
      stats: this.stats,
      wasCancelled: this.wasCancelled
    }
  }
}