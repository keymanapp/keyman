/// <reference path="inputEventEngine.ts" />
/// <reference path="gestureRecognizerConfiguration.ts" />

namespace com.keyman.osk {
  export class TouchEventEngine extends InputEventEngine {
    private readonly _touchStart: typeof TouchEventEngine.prototype.onTouchStart;
    private readonly _touchMove:  typeof TouchEventEngine.prototype.onTouchMove;
    private readonly _touchEnd:   typeof TouchEventEngine.prototype.onTouchEnd;

    public constructor(config: GestureRecognizerConfiguration) {
      super(config);

      this._touchStart = this.onTouchStart.bind(this);
      this._touchMove  = this.onTouchMove.bind(this);
      this._touchEnd   = this.onTouchEnd.bind(this);
    }

    private get eventRoot(): HTMLElement {
      return this.config.touchEventRoot;
    }

    // public static forVisualKeyboard(vkbd: VisualKeyboard) {
    //   let config: GestureRecognizerConfiguration = {
    //     targetRoot: vkbd.element,
    //     eventRoot: vkbd.element,
    //     inputStartHandler: vkbd.touch.bind(vkbd),
    //     inputMoveHandler: vkbd.moveOver.bind(vkbd),
    //     inputMoveCancelHandler: vkbd.moveCancel.bind(vkbd),
    //     inputEndHandler: vkbd.release.bind(vkbd),
    //     coordConstrainedWithinInteractiveBounds: vkbd.detectWithinInteractiveBounds.bind(vkbd)
    //   };

    //   return new TouchEventEngine(config);
    // }

    // public static forPredictiveBanner(banner: SuggestionBanner, handlerRoot: SuggestionManager) {
    //   const config: GestureRecognizerConfiguration = {
    //     targetRoot: banner.getDiv(),
    //     // document.body is the event root b/c we need to track the mouse if it leaves
    //     // the VisualKeyboard's hierarchy.
    //     eventRoot: banner.getDiv(),
    //     inputStartHandler: handlerRoot.touchStart.bind(handlerRoot),
    //     inputMoveHandler:  handlerRoot.touchMove.bind(handlerRoot),
    //     inputEndHandler:   handlerRoot.touchEnd.bind(handlerRoot),
    //     coordConstrainedWithinInteractiveBounds: function() { return true; }
    //   };

    //   return new TouchEventEngine(config);
    // }

    registerEventHandlers() {
      // The 'passive' property ensures we can prevent MouseEvent followups from TouchEvents.
      // It is only specified during `addEventListener`, not during `removeEventListener`.
      this.eventRoot.addEventListener('touchstart', this._touchStart, {capture: true, passive: false});
      this.eventRoot.addEventListener('touchmove',  this._touchMove, {capture: false, passive: false});
      // The listener below fails to capture when performing automated testing checks in Chrome emulation unless 'true'.
      this.eventRoot.addEventListener('touchend',   this._touchEnd, {capture: true, passive: false});
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
      this.preventPropagation(event);
      this.onInputStart(InputEventCoordinate.fromEvent(event));
    }

    onTouchMove(event: TouchEvent) {
      this.preventPropagation(event);
      const coord = InputEventCoordinate.fromEvent(event);

      if(this.config.coordConstrainedWithinInteractiveBounds(coord)) {
        this.onInputMove(coord);
      } else {
        this.onInputMoveCancel(coord);
      }
    }

    onTouchEnd(event: TouchEvent) {
      this.preventPropagation(event);
      this.onInputEnd(InputEventCoordinate.fromEvent(event));
    }
  }
}