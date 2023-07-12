import { InputSample } from "./inputSample.js";
import { JSONTrackedPath, TrackedPath } from "./trackedPath.js";

/**
 * Documents the expected typing of serialized versions of the `TrackedPoint` class.
 */
export type JSONTrackedPoint<HoveredItemType = any> = {
  isFromTouch: boolean;
  path: JSONTrackedPath<HoveredItemType>;
  initialHoveredItem: HoveredItemType
  // identifier is not included b/c it's only needed during live processing.
}

/**
 * Represents one 'tracked point' involved in a potential / recognized gesture as tracked over time.
 * This 'tracked point' corresponds to one touch source as recognized by `Touch.identifier` or to
 * one 'cursor-point' as represented by mouse-based motion.
 */
export class TrackedPoint<HoveredItemType> {
  /**
   * Indicates whether or not this tracked point's original source is a DOM `Touch`.
   */
  public readonly isFromTouch: boolean;

  /**
   * The numeric form of this point's identifier as seen in events (or as emulated for mouse events)
   */
  public readonly rawIdentifier: number;

  // A full, uninterrupted recording of all samples observed during the lifetime of the touchpoint.
  private _fullPath: TrackedPath<HoveredItemType>;

  // The portion of the touchpoint's lifetime currently under consideration for gestures.
  private _activePath: TrackedPath<HoveredItemType>

  // TODO:  consider preserving 'effective segmentation' caused by path-resetting during transition between gestures
  // when in debug/dev mode.  It's not the same as the old subsegmentation, but it may still be useful data for
  // debugging and/or automated-test development.

  private static _jsonIdSeed: -1;

  /**
   * Tracks the coordinates and timestamps of each update for the lifetime of this `TrackedPoint`.
   */
  public get fullPath(): TrackedPath<HoveredItemType> {
    return this._fullPath;
  }

  /**
   * Tracks the coordinates and timestamps of each update under consideration for this `TrackedPoint`.
   * Does not include any components preceding the most recent `resetPath()` call.
   */
  public get path(): TrackedPath<HoveredItemType> {
    return this._activePath;
  }

  private itemSource: InputSample<HoveredItemType>;

  /**
   * Constructs a new TrackedPoint instance for tracking updates to an active input point over time.
   * @param identifier     The system identifier for the input point's events.
   * @param initialHoveredItem  The initiating event's original target element
   * @param isFromTouch    `true` if sourced from a `TouchEvent`; `false` otherwise.
   */
  constructor(identifier: number, isFromTouch: boolean) {
    this.rawIdentifier = identifier;
    this.isFromTouch = isFromTouch;
    this._fullPath = new TrackedPath();
    this._activePath = this._fullPath;
  }

  /**
   * Deserializes a TrackedPoint instance from its serialized-JSON form.
   * @param jsonObj  The JSON representation to deserialize.
   * @param identifier The unique identifier to assign to this instance.
   */
  public static deserialize(jsonObj: JSONTrackedPoint, identifier: number) {
    const id = identifier !== undefined ? identifier : this._jsonIdSeed++;
    const isFromTouch = jsonObj.isFromTouch;
    const path = TrackedPath.deserialize(jsonObj.path);

    const instance = new TrackedPoint(id, isFromTouch);
    instance._fullPath = path;
    return instance;
  }

  public update(sample: InputSample<HoveredItemType>) {
    this._fullPath.extend(sample);
    if(this._activePath != this._fullPath) {
      this._activePath.extend(sample);
    }

    this.itemSource ||= sample;
  }

  /**
   * The identifying metadata returned by the configuration's specified `itemIdentifier` for
   * the target of the first `Event` that corresponded to this `TrackedPoint`.
   */
  public get initialHoveredItem(): HoveredItemType {
    return this.itemSource?.item;
  }

  /**
   * The identifying metadata returned by the configuration's specified `itemIdentifier` for
   * the target of the latest `Event` that corresponded to this `TrackedPoint`.
   */
  public get currentHoveredItem(): HoveredItemType {
    return this.path.coords[this.path.coords.length-1].item;
  }

  public resetPath(preserveInitial: boolean) {
    const lastSample = this._fullPath.latestInput;
    this._activePath = new TrackedPath<HoveredItemType>();
    if(lastSample) {
      this._activePath.extend(lastSample);
    }

    if(!preserveInitial) {
      this.itemSource = lastSample;
    }
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
      initialHoveredItem: this.initialHoveredItem,
      path: this.path.toJSON()
    }

    return jsonClone;
  }
}