namespace com.keyman.osk {

  export class PaddedZoneSource implements RecognitionZoneSource {
    private readonly root: RecognitionZoneSource;

    /**
     * [0]: y (top)
     * [1]: x (left)
     * [2]: height (top+bottom)
     * [3]: width (left+right)
     */
    private edgePadding: number[];

    /**
     * Provides a dynamic 'padded' recognition zone based upon offsetting from the borders
     * of the active page's viewport.
     *
     * Padding is defined using the standard CSS border & padding spec style:
     * - [a]:  equal and even padding on all sides
     * - [a, b]: top & bottom use `a`, left & right use `b`
     * - [a, b, c]: top uses `a`, left & right use `b`, bottom uses `c`
     * - [a, b, c, d]: top, right, bottom, then left.
     *
     * Positive padding reduces the size of the resulting zone; negative padding expands it.
     *
     * @param rootZoneSource The root zone source object/element to be 'padded'
     * @param edgePadding A set of 1 to 4 numbers defining padding per the standard CSS border & padding spec style.
     */
    public constructor(...edgePadding: number[])
    /**
     * Provides a dynamic 'padded' recognition zone based upon offsetting from the borders
     * of another defined zone.
     *
     * Padding is defined using the standard CSS border & padding spec style:
     * - [a]:  equal and even padding on all sides
     * - [a, b]: top & bottom use `a`, left & right use `b`
     * - [a, b, c]: top uses `a`, left & right use `b`, bottom uses `c`
     * - [a, b, c, d]: top, right, bottom, then left.
     *
     * Positive padding reduces the size of the resulting zone; negative padding expands it.
     *
     * @param rootZoneSource The root zone source object/element to be 'padded'
     * @param edgePadding A set of 1 to 4 numbers defining padding per the standard CSS border & padding spec style.
     */
    public constructor(rootZoneSource: RecognitionZoneSource, ...edgePadding: number[]);
    public constructor(rootZoneSource: RecognitionZoneSource | number, ...edgePadding: number[]) {
      // Disambiguate which constructor style was intended.
      if(typeof rootZoneSource == 'number') {
        edgePadding = [rootZoneSource, ...edgePadding];
        rootZoneSource = new ViewportZoneSource();
      }

      this.root = rootZoneSource;
      // In case it isn't yet defined.
      edgePadding = edgePadding || [0, 0, 0, 0];

      // Modeled after CSS styling definitions... just with preprocessed numbers, not strings.
      switch(edgePadding.length) {
        case 1:
          // all sides equal
          const val = edgePadding[0];
          this.edgePadding = [val, val, 2 * val, 2 * val];
          break;
        case 2:
          // top & bottom, left & right
          const yVal = edgePadding[0];
          const xVal = edgePadding[1];
          this.edgePadding = [yVal, xVal, 2 * yVal, 2 * xVal];
          break;
        case 3:
          // top, left & right, bottom
          this.edgePadding = [edgePadding[0],
                              edgePadding[1],
                              edgePadding[0] + edgePadding[2],
                              2*edgePadding[1]];
        case 4:
          // top, right, bottom, left
          this.edgePadding = [edgePadding[0],
                              edgePadding[3], // we want the `left` entry internally, not the `right`.
                              edgePadding[0] + edgePadding[2],
                              edgePadding[1] + edgePadding[3]];
          break;
        default:
          throw new Error("Invalid values for PaddedZoneSource's edgePadding - must be between 1 to 4 `number` values.");
      }
    }

    getBoundingClientRect(): DOMRect {
      const rootZone = this.root.getBoundingClientRect();

      return DOMRect.fromRect({
        y: rootZone.y + this.edgePadding[0],
        x: rootZone.x + this.edgePadding[1],
        height: rootZone.height - this.edgePadding[2],
        width: rootZone.width   - this.edgePadding[3]
      });
    }
  }
}