/// <reference path="inputEventEngine.ts" />
/// <reference path="gestureRecognizerConfiguration.ts" />

namespace com.keyman.osk {
  export class TouchEventEngine extends InputEventEngine {
    private readonly _touchStart: typeof TouchEventEngine.prototype.onTouchStart;
    private readonly _touchMove:  typeof TouchEventEngine.prototype.onTouchMove;
    private readonly _touchEnd:   typeof TouchEventEngine.prototype.onTouchEnd;

    private safeBoundMaskMap: {[id: number]: number} = {};

    public constructor(config: Nonoptional<GestureRecognizerConfiguration>) {
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
    //   };

    //   return new TouchEventEngine(config);
    // }

    // public static forPredictiveBanner(banner: SuggestionBanner, handlerRoot: SuggestionManager) {
    //   const config: GestureRecognizerConfiguration = {
    //     targetRoot: banner.getDiv(),
    //     // document.body is the event root b/c we need to track the mouse if it leaves
    //     // the VisualKeyboard's hierarchy.
    //     eventRoot: banner.getDiv(),
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

    public cleanupSequenceWithId(identifier: number) {
      super.cleanupSequenceWithId(identifier);

      delete this.safeBoundMaskMap[identifier];
    }

    private buildSampleFromTouch(touch: Touch): InputSample {
      return this.buildSampleFor(touch.clientX, touch.clientY);
    }

    onTouchStart(event: TouchEvent) {
      // If it's not an event we'd consider handling, do not prevent event
      // propagation!  Just don't process it.
      if(!this.config.targetRoot.contains(event.target as Node)) {
        return;
      }

      this.preventPropagation(event);

      for(let i=0; i < event.changedTouches.length; i++) {
        const touch = event.changedTouches.item(i);
        const sample = this.buildSampleFromTouch(touch);

        if(!ZoneBoundaryChecker.inputStartOutOfBoundsCheck(sample, this.config)) {
          // If we started very close to a safe zone border, remember which one(s).
          // This is important for input-sequence cancellation check logic.
          this.safeBoundMaskMap[touch.identifier] = ZoneBoundaryChecker.inputStartSafeBoundProximityCheck(sample, this.config);
        } else {
          // This touchpoint shouldn't be considered; do not signal a touchstart for it.
          continue;
        }

        this.onInputStart(touch.identifier, sample, event.target);
      }
    }

    onTouchMove(event: TouchEvent) {
      for(let i=0; i < event.changedTouches.length; i++) {
        const touch = event.changedTouches.item(i);

        if(!this.hasActiveSequence(touch.identifier)) {
          continue;
        }

        this.preventPropagation(event);

        const sample = this.buildSampleFromTouch(touch);

        if(!ZoneBoundaryChecker.inputMoveCancellationCheck(sample, this.config, this.safeBoundMaskMap[touch.identifier])) {
          this.onInputMove(touch.identifier, sample);
        } else {
          this.onInputMoveCancel(touch.identifier);
        }
      }
    }

    onTouchEnd(event: TouchEvent) {
      for(let i=0; i < event.changedTouches.length; i++) {
        const touch = event.changedTouches.item(i);

        if(!this.hasActiveSequence(touch.identifier)) {
          continue;
        }

        this.preventPropagation(event);

        const sample = this.buildSampleFromTouch(touch);

        this.onInputEnd(touch.identifier, sample);
      }
    }
  }
}