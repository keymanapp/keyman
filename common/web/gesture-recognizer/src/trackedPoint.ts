import { JSONTrackedPath, TrackedPath } from "./trackedPath.js";

/**
 * Documents the expected typing of serialized versions of the `TrackedPoint` class.
 */
export type JSONTrackedPoint = {
  isFromTouch: boolean;
  path: JSONTrackedPath;
  // identifier is not included b/c it's only needed during live processing.
}

/**
 * Represents one 'tracked point' involved in a potential / recognized gesture as tracked over time.
 * This 'tracked point' corresponds to one touch source as recognized by `Touch.identifier` or to
 * one 'cursor-point' as represented by mouse-based motion.
 */
export class TrackedPoint {
  /**
   * Indicates whether or not this tracked point's original source is a DOM `Touch`.
   */
  public readonly isFromTouch: boolean;

  /**
   * The numeric form of this point's identifier as seen in events (or as emulated for mouse events)
   */
  public readonly rawIdentifier: number;

  private _initialTarget: EventTarget;

  private _path: TrackedPath;

  /**
   * Tracks the coordinates and timestamps of each update for the lifetime of this `TrackedPoint`.
   */
  public get path(): TrackedPath {
    return this._path;
  }

  /**
   * Constructs a new TrackedPoint instance for tracking updates to an active input point over time.
   * @param identifier     The system identifier for the input point's events.
   * @param initialTarget  The initiating event's original target element
   * @param isFromTouch    `true` if sourced from a `TouchEvent`; `false` otherwise.
   */
  constructor(identifier: number,
              initialTarget: EventTarget,
              isFromTouch: boolean);
  /**
   * Deserializes a TrackedPoint instance from its serialized-JSON form.
   * @param identifier The unique identifier to assign to this instance.
   * @param parsedObj  The JSON representation to deserialize.
   */
  constructor(identifier: number,
    parsedObj: JSONTrackedPoint);
  constructor(identifier: number,
              obj: EventTarget | JSONTrackedPoint,
              isFromTouch?: boolean) {
    this.rawIdentifier = identifier;
    if(obj instanceof EventTarget) {
      this._initialTarget = obj;
      this.isFromTouch = isFromTouch;
      this._path = new TrackedPath();
    } else {
      // // TEMP:  conversion of old format.
      // if(obj['sequence']) {
      //   obj = obj['sequence'];
      // }
      // @ts-ignore
      this.isFromTouch = obj.isFromTouch;

      // TEMP:  conversion of old format
      // @ts-ignore
      let path = obj.path;
      // if(obj['samples']) {
      //   // @ts-ignore
      //   path = {
      //     coords: obj['samples']
      //   };
      // }

      this._path = new TrackedPath(path);
    }
  }

  /**
   * The event target for the first `Event` corresponding to this `TrackedPoint`.
   */
  public get initialTarget(): EventTarget {
    return this._initialTarget;
  }

  /**
   * Gets a fully-unique string-based identifier, even for edge cases where both mouse and touch input
   * are received simultaneously.
   */
  public get identifier(): string {
    const prefix = this.isFromTouch ? 'touch' : 'mouse';
    return `${prefix}:${this.rawIdentifier}`;
  }

  /**
   * Creates a serialization-friendly version of this instance for use by
   * `JSON.stringify`.
   */
  toJSON(): JSONTrackedPoint {
    let jsonClone: JSONTrackedPoint = {
      isFromTouch: this.isFromTouch,
      path: this.path.toJSON()
    }

    return jsonClone;
  }
}