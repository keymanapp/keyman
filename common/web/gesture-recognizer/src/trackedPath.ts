import EventEmitter from "eventemitter3";
import { InputSample } from "./inputSample.js";
import { PathSegmenter } from "./pathSegmenter.js";
import { Segment } from "./segment.js";

/**
 * Documents the expected typing of serialized versions of the `TrackedPoint` class.
 */
export type JSONTrackedPath = {
  coords: InputSample[]; // ensures type match with public class property.
  wasCancelled?: boolean;
  segments: Segment[];
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
 *
 * `'segmentation'`:  a new segmentation boundary has been identified for the
 * ongoing touchpath.
 *   - Provides one parameter - a new `Segment` instance representing the
 *     still-in-construction part of the touchpath until this event is
 *     raised again. (That would indicate a new segmentation boundary
 *     marking the end of the first event's returned `Segment`.)
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
      this.samples = [].concat(jsonObj.coords.map((obj) => ({...obj} as InputSample)));
      // If we're reconstructing this from a JSON.parse, it's a previously-recorded,
      // completed path.
      this._isComplete = true;
      this.wasCancelled = jsonObj.wasCancelled;
    }

    // Keep this as the _final_ statement in the constructor.  `PathSegmenter` will
    // need a reference to this instance, even if only via closure.
    // (Most likely; not yet done.) Kinda awkward, but it's useful for compartmentalization.
    // - DO use 'via closure.'  That allows us to have the segment passing done via
    // `private` method.
    const segmentStartClosure = (segment: Segment) => {
      this._segments.push(segment);
      this.emit('segmentation', segment);
    }

    this.segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, segmentStartClosure);
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
    this.emit('step', sample);

    this.segmenter.add(sample);
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

    this.segmenter.close();

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
   * Returns the segmented form of the touchpath over its lifetime thus far.  It is
   * possible for certain parts of the path to go unrepresented if they are detected as
   * insignificant.
   *
   * Also of note:  for the common case, the final coordinate of one Segment will usually
   * be the initial point of the following Segment.  Usually, but not always.  This is
   * the only overlap that may occur.
   *
   * Note:  segment events and updates will always occur in the following order:
   *
   * 1. A segment's role is 'recognized' - its classification becomes known.
   * 2. The segment is 'resolved' - the segment is marked as completed.
   * 3. The next segment is added to this field, with on('segmentation') is raised
   *    for it.
   *
   * Note that it is possible for a segment to be 'recognized' and even 'resolved'
   * before its event is raised (thus, before it is added here) under some scenarios.
   * In particular, 'start' and 'end'-type segments are always 'recognized' and
   * 'resolved'.
   *
   * While the touchpath is active, there is a very high chance that the final
   * segment listed will not be resolved.  There will also be a distinct chance
   * that it is not yet recognized.
   *
   * The first Segment should always be a 'start', while the final Segment - once
   * _all_ input for the ongoing touchpath is complete - will be an 'end'.
   */
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
      coords: [].concat(this.samples.map((obj) => ({
        targetX: obj.targetX,
        targetY: obj.targetY,
        t:       obj.t
      }))),
      segments: [].concat(this.segments),
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