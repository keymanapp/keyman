namespace com.keyman.osk.layouts {
  export type MouseHandler = (this: GlobalEventHandlers, ev: MouseEvent) => any;

  /**
   * Used to store the page's original mouse handlers and properties
   * when temporarily overridden by OSK moving or resizing handlers due
   * to user interaction.
   */
  export class MouseStartSnapshot {
    private readonly _VPreviousMouseMove: MouseHandler;
    private readonly _VPreviousMouseUp: MouseHandler;
    private readonly _VPreviousCursor: string;
    private readonly _VPreviousMouseButton: number;

    constructor(e: MouseEvent) {
      this._VPreviousMouseMove = document.onmousemove;
      this._VPreviousMouseUp = document.onmouseup;

      this._VPreviousCursor = document.body.style.cursor;
      this._VPreviousMouseButton = (typeof(e.which)=='undefined' ? e.button : e.which);
    }

    restore() {
      document.onmousemove = this._VPreviousMouseMove;
      document.onmouseup = this._VPreviousMouseUp;

      if(document.body.style.cursor) {
        document.body.style.cursor = this._VPreviousCursor;
      }
    }

    matchesCausingClick(e: MouseEvent): boolean {
      return this._VPreviousMouseButton == (typeof(e.which)=='undefined' ? e.button : e.which);
    }
  }
}