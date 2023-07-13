import { InputSample } from "./inputSample.js";
import { SerializedGesturePath, GesturePath } from "./gesturePath.js";

/**
 * Documents the expected typing of serialized versions of the `SimpleGestureSource` class.
 */
export type SerializedSimpleGestureSource<HoveredItemType = any> = {
  isFromTouch: boolean;
  path: SerializedGesturePath<HoveredItemType>;
  initialHoveredItem: HoveredItemType
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

  private _path: GesturePath<HoveredItemType>;

  private static _jsonIdSeed: -1;

  /**
   * Tracks the coordinates and timestamps of each update for the lifetime of this `SimpleGestureSource`.
   */
  public get path(): GesturePath<HoveredItemType> {
    return this._path;
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
    this._path = new GesturePath();
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
    instance._path = path;
    return instance;
  }

  public update(sample: InputSample<HoveredItemType>) {
    this.path.extend(sample);
  }

  /**
   * The identifying metadata returned by the configuration's specified `itemIdentifier` for
   * the target of the first `Event` that corresponded to this `SimpleGestureSource`.
   */
  public get initialHoveredItem(): HoveredItemType {
    return this.path.coords[0].item;
  }

  /**
   * The identifying metadata returned by the configuration's specified `itemIdentifier` for
   * the target of the latest `Event` that corresponded to this `SimpleGestureSource`.
   */
  public get currentHoveredItem(): HoveredItemType {
    return this.path.coords[this.path.coords.length-1].item;
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
      initialHoveredItem: this.initialHoveredItem,
      path: this.path.toJSON()
    }

    return jsonClone;
  }
}