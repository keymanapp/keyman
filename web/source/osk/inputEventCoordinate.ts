namespace com.keyman.osk {
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

    // Converts a MouseEvent into the base coordinates needed by the mouse-dragging operations.
    public static fromMouseEvent(e: MouseEvent) {
      if (e.pageX) {
        return new InputEventCoordinate(e.pageX, e.pageY, e);
      } else if (e.clientX) {
        const x = e.clientX + document.body.scrollLeft;
        const y = e.clientY + document.body.scrollTop;

        return new InputEventCoordinate(x, y, e);
      }
    }

    public static fromTouchEvent(e: TouchEvent) {
      let touch = e.changedTouches[0];

      if(touch.pageX) {
        return new InputEventCoordinate(touch.pageX, touch.pageY, e);
      } else if (touch.clientX) {
        const x = touch.clientX + document.body.scrollLeft;
        const y = touch.clientY + document.body.scrollTop;

        return new InputEventCoordinate(x, y, e);
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