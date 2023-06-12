/**
 * Represents the current location of the current cursor / touchpoint during
 * an ongoing OSK input event.  This class standardizes to .pageX (document)
 * coordinates, rather than .clientX (viewport) coordinates.
 */
export default class InputEventCoordinate {
  public readonly x: number;
  public readonly y: number;

  private readonly source: MouseEvent | TouchEvent;

  public constructor(x: number, y: number, source?: MouseEvent | TouchEvent) {
    this.x = x;
    this.y = y;

    if(source) {
      this.source = source;
    }
  }

  // Converts a MouseEvent or TouchEvent into the base coordinates needed
  // by the mouse-dragging operations.
  public static fromEvent(e: MouseEvent | TouchEvent) {
    let coordSource: MouseEvent | Touch;

    // Desktop Safari versions as recent as 14.1 do not support TouchEvents.
    // So, just in case, a two-fold conditional check to avoid issues with a direct
    // 'instanceof' against the type.
    if(window['TouchEvent'] && e instanceof TouchEvent) {
      coordSource = e.changedTouches[0];
    } else if(e['changedTouches']) {
      coordSource = e['changedTouches'][0] as Touch;
    } else {
      coordSource = e as MouseEvent;
    }

    // For MouseEvents, .pageX is slightly less supported in older browsers when
    // compared to .clientX.  They're about equally supported for TouchEvents.
    if (coordSource.pageX) {
      return new InputEventCoordinate(coordSource.pageX, coordSource.pageY, e);
    } else if (coordSource.clientX) {
      const x = coordSource.clientX + document.body.scrollLeft;
      const y = coordSource.clientY + document.body.scrollTop;

      return new InputEventCoordinate(x, y, e);
    } else {
      return new InputEventCoordinate(null, null, e);
    }
  }

  public get activeInputCount(): number {
    // May not be an ACTUAL touch event during unit tests.
    if(window['TouchEvent'] && this.source['touches'] !== undefined && this.source['touches'] !== null) {
      return this.source['touches'].length;
    } else {
      const event = this.source as MouseEvent;
      return event.buttons > 0 ? 1 : 0;
    }
  }

  public get target() {
    return this.source?.target;
  }

  public get isFromTouch(): boolean {
    return !this.isFromMouse;
  }

  public get isFromMouse(): boolean {
    return this.source instanceof MouseEvent;
  }
}