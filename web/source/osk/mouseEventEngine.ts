/// <reference path="inputEventEngine.ts" />

namespace com.keyman.osk {
  export class MouseEventEngine extends InputEventEngine {
    private readonly _mouseStart: typeof MouseEventEngine.prototype.onMouseStart;
    private readonly _mouseMove:  typeof MouseEventEngine.prototype.onMouseMove;
    private readonly _mouseEnd:   typeof MouseEventEngine.prototype.onMouseEnd;

    private vkbd: VisualKeyboard;
    private hasActiveClick: boolean = false;
    private ignoreSequence: boolean = false;

    public constructor(
      controller: any,
      eventRoot: HTMLElement,
      inputStartHandler:      InputHandler,
      inputMoveHandler:       InputHandler,
      inputMoveCancelHandler: InputHandler,
      inputEndHandler:        InputHandler
    ) {
      super(
        eventRoot,
        inputStartHandler,
        inputMoveHandler,
        inputMoveCancelHandler,
        inputEndHandler
      );

      if(controller instanceof VisualKeyboard) {
        this.vkbd = controller;
      }

      this._mouseStart = this.onMouseStart.bind(this);
      this._mouseMove  = this.onMouseMove.bind(this);
      this._mouseEnd   = this.onMouseEnd.bind(this);
    }

    public static forVisualKeyboard(vkbd: VisualKeyboard) {
      // document.body is the event root b/c we need to track the mouse if it leaves
      // the VisualKeyboard's hierarchy.
      return new MouseEventEngine(
        vkbd,
        document.body,
        vkbd.touch.bind(vkbd),
        vkbd.moveOver.bind(vkbd),
        vkbd.moveCancel.bind(vkbd),
        vkbd.release.bind(vkbd)
      );
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
      if(!this.vkbd.element.contains(event.target as Node)) {
        this.ignoreSequence = true;
        return;
      }

      this.preventPropagation(event);
      this.onInputStart(InputEventCoordinate.fromEvent(event));
      this.hasActiveClick = true;
    }

    onMouseMove(event: MouseEvent) {
      if(this.ignoreSequence) {
        return;
      }

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
      if(this.ignoreSequence) {
        this.ignoreSequence = false;
        return;
      }

      if(!event.buttons) {
        this.hasActiveClick = false;
      }
      this.onInputEnd(InputEventCoordinate.fromEvent(event));
    }
  }
}