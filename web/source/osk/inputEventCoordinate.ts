namespace com.keyman.osk {
  export class InputEventCoordinate {
    public readonly x: number;
    public readonly y: number

    public constructor(x: number, y: number) {
      this.x = x;
      this.y = y;
    }

    // Converts a MouseEvent into the base coordinates needed by the mouse-dragging operations.
    public static fromMouseEvent(e: MouseEvent) {
      if (e.pageX) {
        return new InputEventCoordinate(e.pageX, e.pageY);
      } else if (e.clientX) {
        const x = e.clientX + document.body.scrollLeft;
        const y = e.clientY + document.body.scrollTop;

        return new InputEventCoordinate(x, y);
      }
    }

    public static fromTouchEvent(e: TouchEvent) {
      let touch = e.changedTouches[0];

      if(touch.pageX) {
        return new InputEventCoordinate(touch.pageX, touch.pageY);
      } else if (touch.clientX) {
        const x = touch.clientX + document.body.scrollLeft;
        const y = touch.clientY + document.body.scrollTop;

        return new InputEventCoordinate(x, y);
      }
    }
  }
}