namespace com.keyman.osk {
  /**
   * Represents the current location of the current cursor / touchpoint during
   * an ongoing OSK input event.  This class standardizes to .pageX (document)
   * coordinates, rather than .clientX (viewport) coordinates.
   */
  export class InputEventCoordinate {
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
      if(e instanceof TouchEvent) {
        coordSource = e.changedTouches[0];
      } else {
        coordSource = e;
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
      if(this.source instanceof TouchEvent) {
        return this.source.touches.length;
      } else {
        return 1;
      }
    }

    public get target() {
      return this.source?.target;
    }
  }
}