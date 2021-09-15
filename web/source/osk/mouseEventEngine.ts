/// <reference path="inputEventEngine.ts" />

namespace com.keyman.osk {
  export class MouseEventEngine extends InputEventEngine {
    private readonly _mouseStart: typeof MouseEventEngine.prototype.onMouseStart;
    private readonly _mouseMove:  typeof MouseEventEngine.prototype.onMouseMove;
    private readonly _mouseEnd:   typeof MouseEventEngine.prototype.onMouseEnd;

    private hasActiveClick: boolean = false;

    public constructor(vkbd: VisualKeyboard) {
      super(vkbd);

      this._mouseStart = this.onMouseStart.bind(this);
      this._mouseMove  = this.onMouseMove.bind(this);
      this._mouseEnd   = this.onMouseEnd.bind(this);
    }

    registerEventHandlers() {
      this.eventRoot.addEventListener('mousedown', this._mouseStart, true);
      this.eventRoot.addEventListener('mousemove',  this._mouseMove, false);
      // The listener below fails to capture when performing automated testing checks in Chrome emulation unless 'true'.
      this.eventRoot.addEventListener('mouseup',   this._mouseEnd, true);
    }

    unregisterEventHandlers() {
      this.eventRoot.removeEventListener('mousedown', this._mouseStart, true);
      this.eventRoot.removeEventListener('mousemove',  this._mouseMove, false);
      this.eventRoot.removeEventListener('mouseup',   this._mouseEnd, true);
    }

    private preventPropagation(e: MouseEvent) {
      // Standard event maintenance
      e.preventDefault();
      e.cancelBubble=true;
      e.returnValue=false; // I2409 - Avoid focus loss for visual keyboard events

      if(typeof e.stopImmediatePropagation == 'function') {
        e.stopImmediatePropagation();
      } else if(typeof e.stopPropagation == 'function') {
        e.stopPropagation();
      }
    }

    onMouseStart(event: MouseEvent) {
      this.preventPropagation(event);
      this.onInputStart(InputEventCoordinate.fromEvent(event));
      this.hasActiveClick = true;
    }

    onMouseMove(event: MouseEvent) {
      const coord = InputEventCoordinate.fromEvent(event);

      if(!event.buttons) {
        if(this.hasActiveClick) {
          this.hasActiveClick = false;
          this.onInputMoveCancel(coord);
        }
        return;
      } else if(!this.hasActiveClick) {
        // Can interfere with OSK drag-handlers (title bar, resize bar) otherwise.
        return;
      }

      this.preventPropagation(event);

      if(this.vkbd.detectWithinInteractiveBounds(coord)) {
        this.onInputMove(coord);
      } else {
        this.onInputMoveCancel(coord);
      }
    }

    onMouseEnd(event: MouseEvent) {
      if(!event.buttons) {
        this.hasActiveClick = false;
      }
      this.onInputEnd(InputEventCoordinate.fromEvent(event));
    }
  }
}