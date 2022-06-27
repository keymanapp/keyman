namespace com.keyman.osk {
  export class ZoneBoundaryChecker {
    // This class exists for static methods & fields.
    private constructor() { }

    public static readonly FAR_LEFT:   0x0008 = 0x0008;
    public static readonly FAR_RIGHT:  0x0004 = 0x0004;
    public static readonly FAR_TOP:    0x0002 = 0x0002;
    public static readonly FAR_BOTTOM: 0x0001 = 0x0001;

    /**
     * Determines the relationship of an input coordinate to one of the gesture engine's
     * active recognition zones and returns a bitmask indicating which boundary (or
     * boundaries) the input coordinate lies outside of.
     *
     * @param coord An input coordinate
     * @param zone  An object defining a 'recognition zone' of the gesture engine.
     */
    getCoordZoneBitmask(coord: InputEventCoordinate, zone: RecognitionZoneSource) {
      const bounds = zone.getBoundingClientRect();

      let bitmask = 0;
      bitmask |= (coord.x < bounds.left)   ? ZoneBoundaryChecker.FAR_LEFT   : 0;
      bitmask |= (coord.x > bounds.right)  ? ZoneBoundaryChecker.FAR_RIGHT  : 0;
      bitmask |= (coord.y < bounds.top)    ? ZoneBoundaryChecker.FAR_TOP    : 0;
      bitmask |= (coord.y < bounds.bottom) ? ZoneBoundaryChecker.FAR_BOTTOM : 0;

      return bitmask;
    }
  }
}