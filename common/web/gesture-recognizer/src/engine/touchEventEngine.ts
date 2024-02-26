import { GestureRecognizerConfiguration } from "./configuration/gestureRecognizerConfiguration.js";
import { InputEventEngine } from "./inputEventEngine.js";
import { InputSample } from "./headless/inputSample.js";
import { Nonoptional } from "./nonoptional.js";
import { ZoneBoundaryChecker } from "./configuration/zoneBoundaryChecker.js";
import { GestureSource } from "./headless/gestureSource.js";
import { ManagedPromise } from "@keymanapp/web-utils";
import { EventSequentializationQueue } from "./eventSequentializationQueue.js";
import { GesturePath } from "./index.js";

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

  protected readonly sequentializer = new EventSequentializationQueue();

  private safeBoundMaskMap: {[id: number]: number} = {};
  private pendingSourceIdentifiers: Map<number, Object> = new Map();
  private inputStartSignalMap: Map<GestureSource<HoveredItemType, StateToken>, ManagedPromise<void>> = new Map();

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
    if(e.cancelable) {
      // Chrome generates error-log messages if this is attempted while
      // the condition is false.
      e.preventDefault();
    }

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

  public unlockTouchpoint? = (touchpoint: GestureSource<HoveredItemType, StateToken, GesturePath<HoveredItemType, StateToken>>) => {
    const lock = this.inputStartSignalMap.get(touchpoint);
    if(lock) {
      lock.resolve();
      this.inputStartSignalMap.delete(touchpoint);
    }
  };

  public hasActiveTouchpoint(identifier: number): boolean {
    const baseResult = super.hasActiveTouchpoint(identifier);
    return baseResult || !!this.pendingSourceIdentifiers.has(identifier);
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

    this.sequentializer.queueEventFunctor(() => {
      // Maintain all touches in the `.touches` array that are NOT marked as `.changedTouches` (and therefore, new)
      this.maintainTouchpointsWithIds(allTouches
        .filter((touch1) => newTouches.findIndex(touch2 => touch1.identifier == touch2.identifier) == -1)
        .map((touch) => touch.identifier)
      );
    });

    this.sequentializer.queueEventFunctor(() => {
      // Ensure the same timestamp is used for all touches being updated.
      const timestamp = performance.now();
      let lastValidTouchpoint: GestureSource<HoveredItemType, StateToken> = null;
      let lastValidTouchId: number;
      const uniqueObject = {};

      // During a touch-start, only _new_ touch contact points are listed here;
      // we shouldn't signal "input start" for any previously-existing touch points,
      // so `.changedTouches` is the best way forward.
      for(let i=0; i < event.changedTouches.length; i++) {
        const touch = event.changedTouches.item(i);
        const touchId = touch.identifier;
        const sample = this.buildSampleFromTouch(touch, timestamp);

        this.pendingSourceIdentifiers.set(touchId, uniqueObject);

        if(!ZoneBoundaryChecker.inputStartOutOfBoundsCheck(sample, this.config)) {
          // If we started very close to a safe zone border, remember which one(s).
          // This is important for input-sequence cancellation check logic.
          this.safeBoundMaskMap[touchId] = ZoneBoundaryChecker.inputStartSafeBoundProximityCheck(sample, this.config);
        } else {
          // This touchpoint shouldn't be considered; do not signal a touchstart for it.
          continue;
        }

        lastValidTouchpoint = this.onInputStart(touchId, sample, event.target, true);
        lastValidTouchId = touchId;
      }

      if(lastValidTouchpoint) {
        // Ensure we only do the cleanup if and when it hasn't already been replaced by new events later.
        const cleanup = () => {
          if(this.pendingSourceIdentifiers.get(lastValidTouchId) == uniqueObject) {
            this.pendingSourceIdentifiers.delete(lastValidTouchId);
          }
        }

        lastValidTouchpoint.path.on('complete', cleanup);
        lastValidTouchpoint.path.on('invalidated', cleanup);

        // This 'lock' should only be released when the last simultaneously-registered touch is published via
        // gesture-recognizer event.
        let eventSignalPromise = new ManagedPromise<void>();
        this.inputStartSignalMap.set(lastValidTouchpoint, eventSignalPromise);

        return eventSignalPromise.corePromise;
      }
    });
  }

  onTouchMove(event: TouchEvent) {
    for(let i = 0; i < event.touches.length; i++) {
      const touch = event.touches.item(i);
      if(this.hasActiveTouchpoint(touch.identifier)) {
        this.preventPropagation(event);
        break;
      }
    }

    this.sequentializer.queueEventFunctor(() => {
      this.maintainTouchpointsWithIds(touchListToArray(event.touches)
        .map((touch) => touch.identifier)
      );
    });

    this.sequentializer.queueEventFunctor(() => {
      // Ensure the same timestamp is used for all touches being updated.
      const timestamp = performance.now();

      // Do not change to `changedTouches` - we need a sample for all active touches in order
      // to facilitate path-update synchronization for multi-touch gestures.
      //
      // May be worth doing changedTouches _first_ though.
      for(let i=0; i < event.touches.length; i++) {
        const touch = event.touches.item(i);

        if(!this.hasActiveTouchpoint(touch.identifier)) {
          continue;
        }

        const config = this.getConfigForId(touch.identifier);
        const sample = this.buildSampleFromTouch(touch, timestamp);

        if(!ZoneBoundaryChecker.inputMoveCancellationCheck(sample, config, this.safeBoundMaskMap[touch.identifier])) {
          this.onInputMove(touch.identifier, sample, touch.target);
        } else {
          this.onInputMoveCancel(touch.identifier, sample, touch.target);
        }
      }
    })

  }

  onTouchEnd(event: TouchEvent) {
    for(let i = 0; i < event.changedTouches.length; i++) {
      const touch = event.changedTouches.item(i);
      if(this.hasActiveTouchpoint(touch.identifier)) {
        this.preventPropagation(event);
        break;
      }
    }

    this.sequentializer.queueEventFunctor(() => {
      // Only lists touch contact points that have been lifted; touchmove is raised separately if any movement occurred.
      for(let i=0; i < event.changedTouches.length; i++) {
        const touch = event.changedTouches.item(i);

        if(!this.hasActiveTouchpoint(touch.identifier)) {
          continue;
        }

        this.onInputEnd(touch.identifier, event.target);
      }
    });
  }
}