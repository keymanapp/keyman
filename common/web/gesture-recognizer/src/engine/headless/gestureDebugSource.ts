import { InputSample } from "./inputSample.js";
import { GestureRecognizerConfiguration, preprocessRecognizerConfig } from "../configuration/gestureRecognizerConfiguration.js";
import { Nonoptional } from "../nonoptional.js";
import { MatcherSelector } from "./gestures/matchers/matcherSelector.js";
import { SerializedGesturePath, GestureDebugPath } from "./gestureDebugPath.js";
import { GestureSource, SerializedGestureSource } from "./gestureSource.js";
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
export class GestureDebugSource<HoveredItemType, StateToken=any> extends GestureSource<HoveredItemType, StateToken, GestureDebugPath<HoveredItemType, StateToken>> {
  // Assertion:  must always contain an index 0 - the base recognizer config.
  private static _jsonIdSeed: -1;

  /**
   * Usable by the gesture-recognizer library's consumer to track a token identifying specific states
   * of the consuming system if desired.
   */
  public stateToken: StateToken = null;

  /**
   * Constructs a new GestureDebugSource instance for tracking updates to an active input point over time.
   * @param identifier     The system identifier for the input point's events.
   * @param initialHoveredItem  The initiating event's original target element
   * @param isFromTouch    `true` if sourced from a `TouchEvent`; `false` otherwise.
   */
  constructor(
    identifier: number,
    recognizerConfig: Nonoptional<GestureRecognizerConfiguration<HoveredItemType, StateToken>>
      | Nonoptional<GestureRecognizerConfiguration<HoveredItemType, StateToken>>[],
    isFromTouch: boolean
  ) {
    super(identifier, recognizerConfig, isFromTouch, GestureDebugPath);
  }

  protected initPath(): GestureDebugPath<HoveredItemType, StateToken> {
    return new GestureDebugPath();
  }

  /**
   * Deserializes a GestureSource instance from its serialized-JSON form.
   * @param jsonObj  The JSON representation to deserialize.
   * @param identifier The unique identifier to assign to this instance.
   */
  public static deserialize(jsonObj: SerializedGestureSource, identifier: number) {
    const id = identifier !== undefined ? identifier : this._jsonIdSeed++;
    const isFromTouch = jsonObj.isFromTouch;
    const path = GestureDebugPath.deserialize(jsonObj.path);

    const instance = new GestureDebugSource(id, null, isFromTouch);
    instance._path = path;
    return instance;
  }
}