import { GestureRecognizerConfiguration } from "./gestureRecognizerConfiguration.js";
import { InputSample } from "../headless/inputSample.js";
import { Nonoptional } from "../nonoptional.js";
import { RecognitionZoneSource } from "./recognitionZoneSource.js";

export class ZoneBoundaryChecker {
  // This class exists for static methods & fields.
  private constructor() { }

  public static readonly FAR_TOP   : 0x0008 = 0x0008;
  public static readonly FAR_LEFT  : 0x0004 = 0x0004;
  public static readonly FAR_BOTTOM: 0x0002 = 0x0002;
  public static readonly FAR_RIGHT : 0x0001 = 0x0001;

  /**
   * Determines the relationship of an input coordinate to one of the gesture engine's
   * active recognition zones and returns a bitmask indicating which boundary (or
   * boundaries) the input coordinate lies outside of.
   *
   * @param coord         An input coordinate
   * @param zone          An object defining a 'recognition zone' of the gesture engine.
   * @param ignoreBitmask A bitmask indicating select boundaries to ignore for the check.
   */
  static getCoordZoneBitmask(coord: InputSample<any>, zone: RecognitionZoneSource): number {
    const bounds = zone.getBoundingClientRect();

    let bitmask = 0;
    bitmask |= (coord.clientX < bounds.left)   ? ZoneBoundaryChecker.FAR_LEFT   : 0;
    bitmask |= (coord.clientX > bounds.right)  ? ZoneBoundaryChecker.FAR_RIGHT  : 0;
    bitmask |= (coord.clientY < bounds.top)    ? ZoneBoundaryChecker.FAR_TOP    : 0;
    bitmask |= (coord.clientY > bounds.bottom) ? ZoneBoundaryChecker.FAR_BOTTOM : 0;

    return bitmask; // returns zero if effectively 'within bounds'.
  }

  /**
   * Confirms whether or not the input coordinate lies within the accepted coordinate bounds
   * for a gesture input sequence's first coordinate.
   */
  static inputStartOutOfBoundsCheck(coord: InputSample<any>, config: Nonoptional<GestureRecognizerConfiguration<any>>): boolean {
    return !!this.getCoordZoneBitmask(coord, config.inputStartBounds); // true if out of bounds.
  }

  /**
   * Call this method to determine which safe-boundary edges, if any, the initial coordinate
   * indicates should be disabled for its sequence's future updates.
   *
   * This value should be provided as the third argument to `inputMoveCancellationCheck` for
   * updated input coordinates for the current input sequence.
   */
  static inputStartSafeBoundProximityCheck(coord: InputSample<any>, config: Nonoptional<GestureRecognizerConfiguration<any>>): number {
    return this.getCoordZoneBitmask(coord, config.paddedSafeBounds);
  }

  static inputMoveCancellationCheck(coord: InputSample<any>,
                                    config: Nonoptional<GestureRecognizerConfiguration<any>>,
                                    ignoredSafeBoundFlags?: number): boolean {
    ignoredSafeBoundFlags = ignoredSafeBoundFlags || 0;

    // If the coordinate lies outside the maximum supported range, fail the boundary check.
    if(!!(this.getCoordZoneBitmask(coord, config.maxRoamingBounds))) {
      return true;
    }

    let borderProximityBitmask = this.getCoordZoneBitmask(coord, config.safeBounds);

    // If the active input sequence started close enough to a safe zone border, we
    // disable that part of the said border for any cancellation checks.
    return !!(borderProximityBitmask & ~ignoredSafeBoundFlags);
  }
}