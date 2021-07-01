namespace com.keyman.osk.layouts {
  export class CustomizationCoordinate {
    public readonly x: number;
    public readonly y: number

    public constructor(x: number, y: number) {
      this.x = x;
      this.y = y;
    }

    public static fromEvent(e: MouseEvent) {
      if (e.pageX) {
        return new CustomizationCoordinate(e.pageX, e.pageY);
      } else if (e.clientX) {
        const x = e.clientX + document.body.scrollLeft;
        const y = e.clientY + document.body.scrollTop;

        return new CustomizationCoordinate(x, y);
      }
    }
  }
}