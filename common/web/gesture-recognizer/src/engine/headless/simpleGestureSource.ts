import { InputSample } from "./inputSample.js";
import { SerializedGesturePath, GesturePath } from "./gesturePath.js";

/**
 * Documents the expected typing of serialized versions of the `SimpleGestureSource` class.
 */
export type SerializedSimpleGestureSource<HoveredItemType = any> = {
  isFromTouch: boolean;
  path: SerializedGesturePath<HoveredItemType>;
  // identifier is not included b/c it's only needed during live processing.
}

/**
 * Represents all metadata needed internally for tracking a single "touch contact point" / "touchpoint"
 * involved in a potential / recognized gesture as tracked over time.
 *
 * Each instance corresponds to one unique contact point as recognized by `Touch.identifier` or to
 * one 'cursor-point' as represented by mouse-based motion.
 *
 * Refer to https://developer.mozilla.org/en-US/docs/Web/API/Touch and
 * https://developer.mozilla.org/en-US/docs/Web/API/Navigator/maxTouchPoints re "touch contact point".
 *
 * May be one-to-many with recognized gestures:  a keyboard longpress interaction generally only has one
 * contact point but will have multiple realized gestures / components:
 * - longpress:  Enough time has elapsed
 * - subkey:  Subkey from the longpress subkey menu has been selected.
 *
 * Thus, it is a "gesture source".  This is the level needed to model a single contact point, while some
 * gestures expect multiple, hence "simple".
 *
 */
export class SimpleGestureSource<HoveredItemType> {
  /**
   * Indicates whether or not this tracked point's original source is a DOM `Touch`.
   */
  public readonly isFromTouch: boolean;

  /**
   * The numeric form of this point's identifier as seen in events (or as emulated for mouse events)
   */
  public readonly rawIdentifier: number;

  // A full, uninterrupted recording of all samples observed during the lifetime of the touchpoint.
  private _fullPath: GesturePath<HoveredItemType>;

  // The portion of the touchpoint's lifetime currently under consideration for gestures.
  private _activePath: GesturePath<HoveredItemType>;

  private _baseItem: HoveredItemType;

  private static _jsonIdSeed: -1;

  /**
   * Tracks the coordinates and timestamps of each update for the lifetime of this `SimpleGestureSource`.
   */
  public get fullPath(): GesturePath<HoveredItemType> {
    return this._fullPath;
  }

  /**
   * Tracks the coordinates and timestamps of each update for the lifetime of this `SimpleGestureSource`.
   */
  public get path(): GesturePath<HoveredItemType> {
    return this._activePath;
  }

  /**
   * Constructs a new SimpleGestureSource instance for tracking updates to an active input point over time.
   * @param identifier     The system identifier for the input point's events.
   * @param initialHoveredItem  The initiating event's original target element
   * @param isFromTouch    `true` if sourced from a `TouchEvent`; `false` otherwise.
   */
  constructor(identifier: number, isFromTouch: boolean) {
    this.rawIdentifier = identifier;
    this.isFromTouch = isFromTouch;
    this._fullPath = new GesturePath();
    this._activePath = this.fullPath;
  }

  /**
   * Deserializes a SimpleGestureSource instance from its serialized-JSON form.
   * @param jsonObj  The JSON representation to deserialize.
   * @param identifier The unique identifier to assign to this instance.
   */
  public static deserialize(jsonObj: SerializedSimpleGestureSource, identifier: number) {
    const id = identifier !== undefined ? identifier : this._jsonIdSeed++;
    const isFromTouch = jsonObj.isFromTouch;
    const path = GesturePath.deserialize(jsonObj.path);

    const instance = new SimpleGestureSource(id, isFromTouch);
    instance._fullPath = path;
    instance._activePath = path;
    return instance;
  }

  public update(sample: InputSample<HoveredItemType>) {
    this.fullPath.extend(sample);

    if(this.fullPath != this.path) {
      this.path.extend(sample);
    }

    this._baseItem ||= sample.item;
  }

  /**
   * The first path sample (coordinate) under consideration for this `SimpleGestureSource`.
   */
  public get baseItem(): HoveredItemType {
    return this._baseItem;
  }

  /**
   * The most recent path sample (coordinate) under consideration for this `SimpleGestureSource`.
   */
  public get currentSample(): InputSample<HoveredItemType> {
    return this.path.coords[this.path.coords.length-1];
  }

  public resetPath(preserveBaseSample: boolean) {
    const lastSample = this._fullPath.coords[this._fullPath.coords.length-1];
    this._activePath = new GesturePath<HoveredItemType>();
    if(lastSample) {
      this._activePath.extend(lastSample);
    }

    if(!preserveBaseSample) {
      this._baseItem = lastSample?.item;
    }
  }

  public terminate(cancel?: boolean) {
    this.path.terminate(cancel);
    if(this.path != this.fullPath) {
      this.fullPath.terminate(cancel);
    }
  }

  public get isPathComplete(): boolean {
    if(this.path.isComplete != this.fullPath.isComplete) {
      throw new Error("Unexpected state: desync between internal path tracking objects: path = " + this.path.isComplete);
    }
    return this.fullPath.isComplete;
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
  toJSON(): SerializedSimpleGestureSource {
    let jsonClone: SerializedSimpleGestureSource = {
      isFromTouch: this.isFromTouch,
      path: this.path.toJSON()
    }

    return jsonClone;
  }
}