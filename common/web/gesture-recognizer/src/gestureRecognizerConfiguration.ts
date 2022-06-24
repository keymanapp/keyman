/// <reference path="recognitionZoneSource.ts" />
/// <reference path="viewportZoneSource.ts" />

namespace com.keyman.osk {
  export interface GestureRecognizerConfiguration {
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
     * A boundary constraining the "safe range" for ongoing touch events.  Events that started
     * within these bounds will be canceled whenever they exit the safe range.  Events starting
     * near or outside of them are given leeway, but only for relevant boundaries.
     *
     * If not specified, this will be based on the active viewport, padded internally by 2px on
     * all sides.
     */
    readonly safeBounds?: RecognitionZoneSource;

    // TBD:  rip this out.  It should be replaced with an implementation based on the previous
    // three properties WITHIN THIS PR.
    readonly coordConstrainedWithinInteractiveBounds: (coord: InputEventCoordinate) => boolean;
  }
}