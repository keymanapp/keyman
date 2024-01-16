import { InputSample } from "@keymanapp/gesture-recognizer";

/**
 * The amount of coordinate 'noise' allowed during a scroll-enabled touch
 * before interpreting the currently-ongoing touch command as having scrolled.
 */
const HAS_SCROLLED_FUDGE_FACTOR = 10;

/**
 * This class was added to facilitate scroll handling for overflow-x elements, though it could
 * be extended in the future to accept overflow-y if needed.
 *
 * This is necessary because of the OSK's need to use `.preventDefault()` for stability; that
 * same method blocks native handling of overflow scrolling for touch browsers.
 */
export class BannerScrollState {
  totalLength = 0;

  baseCoord: InputSample<any>;
  curCoord: InputSample<any>;
  baseScrollLeft: number;

  constructor(coord: InputSample<any>, baseScrollLeft: number) {
    this.baseCoord = coord;
    this.curCoord = coord;
    this.baseScrollLeft = baseScrollLeft;

    this.totalLength = 0;
  }

  updateTo(coord: InputSample<any>): number {
    let prevCoord = this.curCoord;
    this.curCoord = coord;

    let delta = this.baseCoord.targetX - this.curCoord.targetX + this.baseScrollLeft;
    // Track the total amount of scrolling used, even if just a pixel-wide back and forth wiggle.
    this.totalLength += Math.abs(this.curCoord.targetX - prevCoord.targetX);

    return delta;
  }

  public get hasScrolled(): boolean {
    // Allow an accidental fudge-factor for overflow element noise during a touch, but not much.
    return this.totalLength > HAS_SCROLLED_FUDGE_FACTOR;
  }
}