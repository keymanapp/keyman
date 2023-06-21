import { GestureRecognizerConfiguration } from "./gestureRecognizerConfiguration.js";
import { InputEventEngine } from "./inputEventEngine.js";
import { InputSample } from "./inputSample.js";
import { Nonoptional } from "./nonoptional.js";
import { ZoneBoundaryChecker } from "./zoneBoundaryChecker.js";

export class TouchEventEngine extends InputEventEngine {
  private readonly _touchStart: typeof TouchEventEngine.prototype.onTouchStart;
  private readonly _touchMove:  typeof TouchEventEngine.prototype.onTouchMove;
  private readonly _touchEnd:   typeof TouchEventEngine.prototype.onTouchEnd;

  private safeBoundMaskMap: {[id: number]: number} = {};

  public constructor(config: Nonoptional<GestureRecognizerConfiguration>) {
    super(config);

    // We use this approach, rather than .bind, because _this_ version allows hook
    // insertion for unit tests via prototype manipulation.  The .bind version doesn't.
    this._touchStart = (event: TouchEvent) => this.onTouchStart(event);
    this._touchMove  = (event: TouchEvent) => this.onTouchMove(event);
    this._touchEnd   = (event: TouchEvent) => this.onTouchEnd(event);
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

  public dropTouchpointWithId(identifier: number) {
    super.dropTouchpointWithId(identifier);

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

      this.onInputStart(touch.identifier, sample, event.target, true);
    }
  }

  onTouchMove(event: TouchEvent) {
    let propagationActive = true;
    for(let i=0; i < event.changedTouches.length; i++) {
      const touch = event.changedTouches.item(i);

      if(!this.hasActiveTouchpoint(touch.identifier)) {
        continue;
      }

      if(propagationActive) {
        this.preventPropagation(event);
        propagationActive = false;
      }

      const sample = this.buildSampleFromTouch(touch);

      if(!ZoneBoundaryChecker.inputMoveCancellationCheck(sample, this.config, this.safeBoundMaskMap[touch.identifier])) {
        this.onInputMove(touch.identifier, sample);
      } else {
        this.onInputMoveCancel(touch.identifier, sample);
      }
    }
  }

  onTouchEnd(event: TouchEvent) {
    let propagationActive = true;
    for(let i=0; i < event.changedTouches.length; i++) {
      const touch = event.changedTouches.item(i);

      if(!this.hasActiveTouchpoint(touch.identifier)) {
        continue;
      }

      if(propagationActive) {
        this.preventPropagation(event);
        propagationActive = false;
      }

      this.onInputEnd(touch.identifier);
    }
  }
}