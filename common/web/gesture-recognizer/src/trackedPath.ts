/// <reference path="inputSample.ts" />

namespace com.keyman.osk {
  /**
   * Documents the expected typing of serialized versions of the `TrackedPoint` class.
   */
  export type JSONTrackedPath = {
    coords: InputSample[]; // ensures type match with public class property.
    wasCancelled?: boolean;
    //segments: Segment[];
  }

  interface EventMap {
    'step': (sample: InputSample) => void,
    'complete': () => void,
    'invalidated': () => void
    // 'segmentation': (endingSegment: Segment, openingSegment: Segment) => void
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
   *
   * `'invalidated'`: the touchpoint is no longer active; the path has crossed
   * gesture-recognition boundaries and is no longer considered valid.
   *   - Provides no parameters.
   */
  export class TrackedPath extends EventEmitter<EventMap> {
    private samples: InputSample[] = [];
    private _segments: Segment[] = [];

    private readonly segmenter: PathSegmenter;

    private _isComplete: boolean = false;
    private wasCancelled?: boolean;

    // private _segments: Segment[];
    // public get segments(): readonly Segment[] { return this._segments; }

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
        this.samples = [...jsonObj.coords.map((obj) => ({...obj} as InputSample))];
        // If we're reconstructing this from a JSON.parse, it's a previously-recorded,
        // completed path.
        this._isComplete = true;
        this.wasCancelled = jsonObj.wasCancelled;
      }

      // Keep this as the _final_ statement in the constructor.  `PathSegmenter` will
      // need a reference to this instance, even if only via closure.
      // (Most likely; not yet done.) Kinda awkward, but it's useful for compartmentalization.
      this.segmenter = new PathSegmenter();
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
        throw "Invalid state:  this TrackedPath has already terminated.";
      }

      this.samples.push(sample);
      this.segmenter.add(sample);
      this.emit('step', sample);
    }

    /**
     * Finalizes the path.
     * @param cancel Whether or not this finalization should trigger cancellation.
     */
    terminate(cancel: boolean = false) {
      if(this._isComplete) {
        throw "Invalid state:  this TrackedPath has already terminated.";
      }
      this.wasCancelled = cancel;
      this._isComplete = true;
      this.segmenter.close();

      if(cancel) {
        this.emit('invalidated');
      } else {
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

    public get segments(): readonly Segment[] {
      return this._segments;
    }

    /**
     * Creates a serialization-friendly version of this instance for use by
     * `JSON.stringify`.
     */
    toJSON() {
      let jsonClone: JSONTrackedPath = {
        // Replicate array and its entries, but with certain fields of each entry missing.
        // No .clientX, no .clientY.
        coords: [...this.samples.map((obj) => ({
          targetX: obj.targetX,
          targetY: obj.targetY,
          t:       obj.t
        }))],
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

}