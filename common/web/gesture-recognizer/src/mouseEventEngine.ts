/// <reference path="inputEventEngine.ts" />
/// <reference path="gestureRecognizerConfiguration.ts" />

namespace com.keyman.osk {
  export class MouseEventEngine extends InputEventEngine {
    private readonly _mouseStart: typeof MouseEventEngine.prototype.onMouseStart;
    private readonly _mouseMove:  typeof MouseEventEngine.prototype.onMouseMove;
    private readonly _mouseEnd:   typeof MouseEventEngine.prototype.onMouseEnd;

    private hasActiveClick: boolean = false;
    private disabledSafeBounds: number = 0;
    private ignoreSequence: boolean = false;

    public constructor(config: Nonoptional<GestureRecognizerConfiguration>) {
      super(config);

      this._mouseStart = this.onMouseStart.bind(this);
      this._mouseMove  = this.onMouseMove.bind(this);
      this._mouseEnd   = this.onMouseEnd.bind(this);
    }

    private get eventRoot(): HTMLElement {
      return this.config.mouseEventRoot;
    }

    // public static forVisualKeyboard(vkbd: VisualKeyboard) {
    //   const config: GestureRecognizerConfiguration = {
    //     targetRoot: vkbd.element,
    //     // document.body is the event root b/c we need to track the mouse if it leaves
    //     // the VisualKeyboard's hierarchy.
    //     eventRoot: document.body,
    //   };

    //   return new MouseEventEngine(config);
    // }

    // public static forPredictiveBanner(banner: SuggestionBanner, handlerRoot: SuggestionManager) {
    //   const config: GestureRecognizerConfiguration = {
    //     targetRoot: banner.getDiv(),
    //     // document.body is the event root b/c we need to track the mouse if it leaves
    //     // the VisualKeyboard's hierarchy.
    //     eventRoot: document.body,
    //   };

    //   return new MouseEventEngine(config);
    // }

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
      if(!this.config.targetRoot.contains(event.target as Node)) {
        this.ignoreSequence = true;
        return;
      }

      this.preventPropagation(event);
      const coord = InputEventCoordinate.fromEvent(event);

      if(!ZoneBoundaryChecker.inputStartOutOfBoundsCheck(coord, this.config)) {
        // If we started very close to a safe zone border, remember which one(s).
        // This is important for input-sequence cancellation check logic.
        this.disabledSafeBounds = ZoneBoundaryChecker.inputStartSafeBoundProximityCheck(coord, this.config);
      }

      this.onInputStart(coord);
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

      if(!ZoneBoundaryChecker.inputMoveCancellationCheck(coord, this.config, this.disabledSafeBounds)) {
        this.onInputMove(coord);
      } else {
        this.hasActiveClick = false;
        this.ignoreSequence = true;
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