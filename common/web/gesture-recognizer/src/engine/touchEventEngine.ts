import { GestureRecognizerConfiguration } from "./configuration/gestureRecognizerConfiguration.js";
import { InputEventEngine } from "./inputEventEngine.js";
import { InputSample } from "./headless/inputSample.js";
import { Nonoptional } from "./nonoptional.js";
import { ZoneBoundaryChecker } from "./configuration/zoneBoundaryChecker.js";
import { GestureSource } from "./headless/gestureSource.js";

function touchListToArray(list: TouchList) {
  const arr: Touch[] = [];

  for(let i=0; i < list.length; i++) {
    arr.push(list.item(i));
  }

  return arr;
}
export class TouchEventEngine<HoveredItemType, StateToken = any> extends InputEventEngine<HoveredItemType, StateToken> {
  private readonly _touchStart: typeof TouchEventEngine.prototype.onTouchStart;
  private readonly _touchMove:  typeof TouchEventEngine.prototype.onTouchMove;
  private readonly _touchEnd:   typeof TouchEventEngine.prototype.onTouchEnd;

  private safeBoundMaskMap: {[id: number]: number} = {};

  public constructor(config: Nonoptional<GestureRecognizerConfiguration<HoveredItemType, StateToken>>) {
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

  public dropTouchpoint(source: GestureSource<HoveredItemType>) {
    super.dropTouchpoint(source);

    for(const key of Object.keys(this.safeBoundMaskMap)) {
      if(this.getTouchpointWithId(Number.parseInt(key, 10)) == source) {
        delete this.safeBoundMaskMap[key];
      }
    }
  }

  private buildSampleFromTouch(touch: Touch, timestamp: number) {
    // WILL be null for newly-starting `GestureSource`s / contact points.
    const source = this.getTouchpointWithId(touch.identifier);
    return this.buildSampleFor(touch.clientX, touch.clientY, touch.target, timestamp, source);
  }

  onTouchStart(event: TouchEvent) {
    // If it's not an event we'd consider handling, do not prevent event
    // propagation!  Just don't process it.
    if(!this.config.targetRoot.contains(event.target as Node)) {
      return;
    }

    this.preventPropagation(event);

    // In case a touch ID is reused, we can pre-emptively filter it for special cases to cancel the old version,
    // noting that it's included by a changedTouch.  (Only _new_ contact points are included in .changedTouches
    // during a touchstart.)
    const allTouches = touchListToArray(event.touches);
    const newTouches = touchListToArray(event.changedTouches);
    // Maintain all touches in the `.touches` array that are NOT marked as `.changedTouches` (and therefore, new)
    this.maintainTouchpointsWithIds(allTouches
      .filter((touch) => (newTouches.indexOf(touch) == -1))
      .map((touch) => touch.identifier)
    );

    // Ensure the same timestamp is used for all touches being updated.
    const timestamp = performance.now();

    // During a touch-start, only _new_ touch contact points are listed here;
    // we shouldn't signal "input start" for any previously-existing touch points,
    // so `.changedTouches` is the best way forward.
    for(let i=0; i < event.changedTouches.length; i++) {
      const touch = event.changedTouches.item(i);
      const sample = this.buildSampleFromTouch(touch, timestamp);

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
    // Ensure the same timestamp is used for all touches being updated.
    const timestamp = performance.now();

    this.maintainTouchpointsWithIds(touchListToArray(event.touches)
      .map((touch) => touch.identifier)
    );

    // Do not change to `changedTouches` - we need a sample for all active touches in order
    // to facilitate path-update synchronization for multi-touch gestures.
    //
    // May be worth doing changedTouches _first_ though.
    for(let i=0; i < event.touches.length; i++) {
      const touch = event.touches.item(i);

      if(!this.hasActiveTouchpoint(touch.identifier)) {
        continue;
      }

      if(propagationActive) {
        this.preventPropagation(event);
        propagationActive = false;
      }

      const config = this.getConfigForId(touch.identifier);

      const sample = this.buildSampleFromTouch(touch, timestamp);

      if(!ZoneBoundaryChecker.inputMoveCancellationCheck(sample, config, this.safeBoundMaskMap[touch.identifier])) {
        this.onInputMove(touch.identifier, sample, touch.target);
      } else {
        this.onInputMoveCancel(touch.identifier, sample, touch.target);
      }
    }
  }

  onTouchEnd(event: TouchEvent) {
    let propagationActive = true;

    // Only lists touch contact points that have been lifted; touchmove is raised separately if any movement occurred.
    for(let i=0; i < event.changedTouches.length; i++) {
      const touch = event.changedTouches.item(i);

      if(!this.hasActiveTouchpoint(touch.identifier)) {
        continue;
      }

      if(propagationActive) {
        this.preventPropagation(event);
        propagationActive = false;
      }

      this.onInputEnd(touch.identifier, event.target);
    }
  }
}