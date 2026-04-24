import { RecognitionZoneSource } from "./recognitionZoneSource.js";
import { ViewportZoneSource } from "./viewportZoneSource.js";

export class PaddedZoneSource implements RecognitionZoneSource {
  private readonly root: RecognitionZoneSource;

  private _edgePadding: {
    x: number,
    y: number,
    w: number,
    h: number
  };

  public get edgePadding() {
    return this._edgePadding;
  }

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
  public constructor(edgePadding: number[]);
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
  public constructor(rootZoneSource: RecognitionZoneSource, edgePadding?: number[]);
  public constructor(rootZoneSource: RecognitionZoneSource | number[], edgePadding?: number[]) {
    // Disambiguate which constructor style was intended.
    if(Array.isArray(rootZoneSource)) {
      edgePadding = rootZoneSource;
      rootZoneSource = new ViewportZoneSource();
    }

    this.root = rootZoneSource;
    // In case it isn't yet defined.
    edgePadding = edgePadding || [0, 0, 0, 0];

    this.updatePadding(edgePadding);
  }

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
  updatePadding(edgePadding: number[]) {

    // Modeled after CSS styling definitions... just with preprocessed numbers, not strings.
    switch(edgePadding.length) {
      case 1:
        // all sides equal
        const val = edgePadding[0];
        this._edgePadding = {
          x: val,
          y: val,
          w: 2 * val,
          h: 2 * val
        };
        break;
      case 2:
        // top & bottom, left & right
        this._edgePadding = {
          x: edgePadding[1],
          y: edgePadding[0],
          w: 2 * edgePadding[1],
          h: 2 * edgePadding[0]
        };
        break;
      case 3:
        // top, left & right, bottom
        this._edgePadding = {
          x: edgePadding[1],
          y: edgePadding[0],
          w: 2 * edgePadding[1],
          h: edgePadding[0] + edgePadding[2]
        };
        break;
      case 4:
        // top, right, bottom, left
        this._edgePadding = {
          x: edgePadding[3],
          y: edgePadding[0],
          w: edgePadding[1] + edgePadding[3],
          h: edgePadding[0] + edgePadding[2]
        }
        break;
      default:
        throw new Error("Invalid values for PaddedZoneSource's edgePadding - must be between 1 to 4 `number` values.");
    }
  }

  getBoundingClientRect(): DOMRect {
    const rootZone = this.root.getBoundingClientRect();

    // Chrome 35:  x, y do not exist on the returned rect, but left & top do.
    return new DOMRect(
      /*x:*/ rootZone.left + this.edgePadding.x,
      /*y:*/ rootZone.top + this.edgePadding.y,
      /*width:*/  rootZone.width  - this.edgePadding.w,
      /*height:*/ rootZone.height - this.edgePadding.h
    );
  }
}