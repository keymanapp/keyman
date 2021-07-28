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
      this.eventRoot.addEventListener('touchstart', this._touchStart, true);
      this.eventRoot.addEventListener('touchmove',  this._touchMove, false);
      // The listener below fails to capture when performing automated testing checks in Chrome emulation unless 'true'.
      this.eventRoot.addEventListener('touchend',   this._touchEnd, true);
    }

    unregisterEventHandlers() {
      this.eventRoot.removeEventListener('touchstart', this._touchStart, true);
      this.eventRoot.removeEventListener('touchmove',  this._touchMove, false);
      this.eventRoot.removeEventListener('touchend',   this._touchEnd, true);
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
      const coord = InputEventCoordinate.fromTouchEvent(event);

      if(this.vkbd.detectWithinBounds(coord)) {
        this.onInputMove(coord);
      } else {
        this.onInputMoveCancel(coord);
      }
    }

    onTouchEnd(event: TouchEvent) {
      this.onInputEnd(InputEventCoordinate.fromTouchEvent(event));
    }
  }
}