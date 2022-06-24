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

    readonly coordConstrainedWithinInteractiveBounds: (coord: InputEventCoordinate) => boolean;
  }
}