// Note:  we may add properties in the future that aren't explicitly readonly;
// it's just that the ELEMENTS and zone definitions involved shouldn't be shifting
// after configuration.

import { InputSample } from "../headless/inputSample.js";
import { RecognitionZoneSource } from "./recognitionZoneSource.js";

// For example, customization of a longpress timer's length need not be readonly.
export interface GestureRecognizerConfiguration<HoveredItemType> {
  /**
   * Specifies the element that mouse input listeners should be attached to.  If
   * not specified, `eventRoot` will be set equal to `targetRoot`.
   */
  readonly mouseEventRoot?: HTMLElement;

  /**
   * Specifies the element that touch input listeners should be attached to.  If
   * not specified, `eventRoot` will be set equal to `targetRoot`.
   */
  readonly touchEventRoot?: HTMLElement;

  /**
   * Specifies the most specific common ancestor element of any event target
   * that the `InputEventEngine` should consider.
   */
  readonly targetRoot: HTMLElement;

  /**
   * A boundary constraining the legal coordinates for supported touchstart and mousedown
   * events.  If not specified, this will be set to `targetRoot`.
   */
  readonly inputStartBounds?: RecognitionZoneSource;

  /**
   * A boundary constraining the maximum range that an ongoing input may travel before it
   * is forceably canceled.  If not specified, this will be set to `targetRoot`.
   */
  readonly maxRoamingBounds?: RecognitionZoneSource;

  /**
   * A boundary constraining the "safe range" for ongoing touch events.  Events that leave a
   * safe boundary that did not start outside its respective "padded" bound will be canceled.
   *
   * If not specified, this will be based on the active viewport, padded internally by 2px on
   * all sides.
   */
  readonly safeBounds?: RecognitionZoneSource;

  /**
   * Used to define a "boundary" slightly more constrained than `safeBounds`.  Events that
   * start within this pixel range from a safe bound will disable that bound for the duration
   * of its corresponding input sequence.  May be a number or an array of 1, 2, or 4 numbers,
   * as with CSS styling.
   *
   * If not specified, this will default to a padding of 3px inside the standard safeBounds
   * unless `paddedSafeBounds` is defined.
   *
   * If `paddedSafeBounds` was specified initially, this will be set to `undefined`.
   */
  readonly safeBoundPadding?: number | number[];

  /**
   * Used to define when an input coordinate is "close" to `safeBounds` borders via exclusion.
   * If this is not defined while `safeBoundPadding` is, this will be built automatically to
   * match the spec set by `safeBoundPadding`.
   *
   * Defining this directly will cause `safeBoundPadding` to be ignored in favor of the bounds
   * set here.
   */
  readonly paddedSafeBounds?: RecognitionZoneSource;

  /**
   * Allows the gesture-recognizer client to specify the most relevant, identifying UI "item"
   * (as perceived by users / relevant for gesture discrimination) underneath the touchpoint's
   * current location based when processing input events.
   *
   * For applications in the DOM, simply returning `target` itself may be sufficient.
   * @param coord   The current touchpath coordinate; its .targetX and .targetY values should be
   *                interpreted as offsets from `targetRoot`.
   * @param target  The `EventTarget` (`Node` or `Element`) provided by the corresponding input event.
   * @returns
   */
  readonly itemIdentifier?: (coord: Omit<InputSample<any>, 'item'>, target: EventTarget) => HoveredItemType;
}