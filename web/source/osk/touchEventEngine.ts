/// <reference path="inputEventEngine.ts" />

namespace com.keyman.osk {
  export class TouchEventEngine extends InputEventEngine {
    private readonly _touchStart: typeof TouchEventEngine.prototype.onTouchStart;
    private readonly _touchMove:  typeof TouchEventEngine.prototype.onTouchMove;
    private readonly _touchEnd:   typeof TouchEventEngine.prototype.onTouchEnd;

    public constructor(vkbd: VisualKeyboard) {
      super(vkbd);

      this._touchStart = this.onTouchStart.bind(this);
      this._touchMove  = this.onTouchMove.bind(this);
      this._touchEnd   = this.onTouchEnd.bind(this);
    }

    registerEventHandlers() {
      this.eventRoot.addEventListener('touchstart', this._touchStart);
      this.eventRoot.addEventListener('touchmove',  this._touchMove);
      this.eventRoot.addEventListener('touchend',   this._touchEnd);
    }

    unregisterEventHandlers() {
      this.eventRoot.removeEventListener('touchstart', this._touchStart);
      this.eventRoot.removeEventListener('touchmove',  this._touchMove);
      this.eventRoot.removeEventListener('touchend',   this._touchEnd);
    }

    private preventPropagation(e: TouchEvent) {
      // Standard event maintenance
      e.preventDefault();
      e.cancelBubble=true;

      if(typeof e.stopImmediatePropagation == 'function') {
        e.stopImmediatePropagation();
      } else if(typeof e.stopPropagation == 'function') {
        e.stopPropagation();
      }
    }

    onTouchStart(event: TouchEvent) {
      this.onInputStart(InputEventCoordinate.fromTouchEvent(event));
    }

    onTouchMove(event: TouchEvent) {
      this.preventPropagation(event);

      // This method is responsible for determining whether a touch 'moved'
      // or was 'cancelled'.
      
      // Determine the important geometric values involved
      const coord = InputEventCoordinate.fromTouchEvent(event);

      const _Box = this.vkbd.element.offsetParent as HTMLElement;
      const oskX = this.vkbd.element.offsetLeft + (_Box?.offsetLeft || 0);
      const oskY = this.vkbd.element.offsetTop  + (_Box?.offsetTop || 0);
      const width = this.vkbd.width;
      const height = this.vkbd.height;

      // Determine the out-of-bounds threshold at which touch-cancellation should automatically occur.
      // Assuming square key-squares, we'll use 1/3 the height of a row for bounds detection
      // for both dimensions.
      const rowCount = this.vkbd.currentLayer.rows.length;
      const buffer = (0.333 * this.vkbd.height / rowCount);

      // ... and begin!

      if(coord.x < oskX - buffer || coord.x > oskX + width + buffer) {
        this.onInputCancel(coord);
      } else if(coord.y < oskY - buffer || coord.y > oskY + height + buffer) {
        this.onInputCancel(coord);
      } else {
        this.onInputMove(coord);
      }
    }

    onTouchEnd(event: TouchEvent) {
      this.onInputEnd(InputEventCoordinate.fromTouchEvent(event));
    }
  }
}