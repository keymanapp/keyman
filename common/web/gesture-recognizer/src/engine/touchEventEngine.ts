import { GestureRecognizerConfiguration } from "./configuration/gestureRecognizerConfiguration.js";
import { InputEventEngine } from "./inputEventEngine.js";
import { InputSample } from "./headless/inputSample.js";
import { Nonoptional } from "./nonoptional.js";
import { ZoneBoundaryChecker } from "./configuration/zoneBoundaryChecker.js";
import { GestureSource } from "./headless/gestureSource.js";
import { ManagedPromise } from "@keymanapp/web-utils";

interface TouchpointTimestamps {
  trueStart: number;
  dispatch?: number;
  delta?: number;
}

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

  // ******** END OF DAY 2/22/2024 *******
  // This approach does not appear to be viable - it shifts too much during lag spikes / heavy typing.
  // Needs to be ON THE TOUCHPOINT, not the event ID.
  //
  // Idea:  closure-capture the object!  It's totally proper when the original event is live, of course.
  // Will need to tweak the sample-builder to reference the object.
  // *************************************

  /**
   * Maintains timing data about live, incoming events as they are seen, discarding them when the original
   * touch-end occurs (or when the corresponding GestureSource is terminated for other reasons).
   *
   * During heavy lag / with rapid typing, there is no guarantee that the corresponding GestureSource is actually being
   * actively processed, nor is there a guarantee that a actively-processed GestureSource still has corresponding data here.
   */
  private pendingTimestamps: Map<number, TouchpointTimestamps> = new Map();
  protected inputStartSignalMap: Map<GestureSource<HoveredItemType, StateToken>, ManagedPromise<void>> = new Map();

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

  public unlockTouchpoint(touchpoint: GestureSource<HoveredItemType, StateToken>) {
    const map = this.inputStartSignalMap;
    const lock = map.get(touchpoint);
    if(lock) {
      lock.resolve();
      map.delete(touchpoint);
    }
  }

  hasRegisteredTouchpoint(identifier: number): boolean {
    if(super.hasRegisteredTouchpoint(identifier)) {
      return true;
    } else {
      return !!this.pendingTimestamps.has(identifier);
    }
  }

  private buildSampleFromTouch(touch: Touch, timestamp: number, timeMeta: TouchpointTimestamps) {
    // WILL be null for newly-starting `GestureSource`s / contact points.
    const source = this.getTouchpointWithId(touch.identifier);

    // The actual timestamp passed in is from the observation's original time.
    // We adjust this timestamp forward by whatever lag was required to sequentialize the touchstart,
    // keeping all relative timings otherwise the same.
    const timestampDelta = timeMeta.delta;
    return this.buildSampleFor(touch.clientX, touch.clientY, touch.target, timestamp + timestampDelta, source);
  }

  onTouchStart(event: TouchEvent) {
    console.log('onTouchStart');
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

    // *** PROBLEM:  could accidentally cancel touches after it due to the delay it induces and how
    //               we store the touchpoint immediately instead of after a delay.

    this.sequentializer.queueEventFunctor(() => {
      // Maintain all touches in the `.touches` array that are NOT marked as `.changedTouches` (and therefore, new)
      this.maintainTouchpointsWithIds(allTouches
        .filter((touch1) => newTouches.findIndex(touch2 => touch1.identifier == touch2.identifier) == -1)
        .map((touch) => touch.identifier)
      );
    });

    // Ensure the same timestamp is used for all touches being updated.
    const timestamp = performance.now();

    // Used to closure-capture the timestamp object state at the time that the incoming event is
    // initially recorded.
    const timeMetaMap: TouchpointTimestamps[] = [];

    for(let i=0; i < event.changedTouches.length; i++) {
      const id = event.changedTouches.item(i).identifier;
      this.pendingTimestamps.set(id, {
        trueStart: timestamp
      });
      timeMetaMap[id] = this.pendingTimestamps.get(id);
      // this.markTrueStart(id, 0); // Only now should 'maintain touchpoint' stuff actually affect the touchpoint.
    }

    const initialStartSignal = () => {
      const dispatchTime = performance.now();
      let lastValidTouchpoint: GestureSource<HoveredItemType, StateToken> = null;

      // During a touch-start, only _new_ touch contact points are listed here;
      // we shouldn't signal "input start" for any previously-existing touch points,
      // so `.changedTouches` is the best way forward.
      for(let i=0; i < event.changedTouches.length; i++) {
        const touch = event.changedTouches.item(i);
        const eventId = touch.identifier;

        const timeMeta = timeMetaMap[eventId];
        timeMeta.dispatch = dispatchTime;
        timeMeta.delta = dispatchTime - timeMeta.trueStart;

        const sample = this.buildSampleFromTouch(touch, timestamp, timeMeta);

        if(!ZoneBoundaryChecker.inputStartOutOfBoundsCheck(sample, this.config)) {
          // If we started very close to a safe zone border, remember which one(s).
          // This is important for input-sequence cancellation check logic.
          this.safeBoundMaskMap[eventId] = ZoneBoundaryChecker.inputStartSafeBoundProximityCheck(sample, this.config);
        } else {
          // This touchpoint shouldn't be considered; do not signal a touchstart for it.
          continue;
        }

        lastValidTouchpoint = this.onInputStart(eventId, sample, event.target, true);

        // Ensure we only do the cleanup if and when it hasn't already been replaced by new events later.
        const cleanup = () => {
          if(this.pendingTimestamps.get(eventId) == timeMeta) {
            this.pendingTimestamps.delete(eventId);
          }
        }

        lastValidTouchpoint.path.on('complete', cleanup);
        lastValidTouchpoint.path.on('invalidated', cleanup);
      }

      // The 'lock' should only be released when the last simultaneously-registered touch is published.
      let eventSignalPromise = new ManagedPromise<void>();
      this.inputStartSignalMap.set(lastValidTouchpoint, eventSignalPromise);

      console.log('returning specialized promise');
      return eventSignalPromise.corePromise;
    }

    // Signals the new gesture-source and prevents any further input signals from processing until
    // the fate of the gesture source is clear:
    // - may auto-complete an existing gesture
    // - may need remapping due to said auto-complete triggering a state change (layer change in KMW)
    // - actual generation of the 'inputstart' event (if it remains independent from an ongoingly-multi-touch gesture model)
    this.sequentializer.queueEventFunctor(initialStartSignal);
  }

  onTouchMove(event: TouchEvent) {
    console.log('onTouchMove');

    // Used to closure-capture the timestamp object state at the time that the incoming event is
    // initially recorded.
    const timeMetaMap: TouchpointTimestamps[] = [];

    for(let i=0; i < event.touches.length; i++) {
      const touch = event.touches.item(i);
      if(this.hasRegisteredTouchpoint(touch.identifier) /* || hasPendingTouchpoint */) {
        this.preventPropagation(event);
      }
      timeMetaMap[touch.identifier] = this.pendingTimestamps.get(touch.identifier);
    }

    this.sequentializer.queueEventFunctor(() => {
      this.maintainTouchpointsWithIds(touchListToArray(event.touches)
        .map((touch) => touch.identifier)
      );
    });

    // Ensure the same timestamp is used for all touches being updated.
    const timestamp = performance.now();

    this.sequentializer.queueEventFunctor(() => {
      // Do not change to `changedTouches` - we need a sample for all active touches in order
      // to facilitate path-update synchronization for multi-touch gestures.
      //
      // May be worth doing changedTouches _first_ though.
      for(let i=0; i < event.touches.length; i++) {
        const touch = event.touches.item(i);
        const eventId = touch.identifier;
        const timeMeta = timeMetaMap[eventId];

        if(!this.hasRegisteredTouchpoint(eventId)) {
          continue;
        }

        const sample = this.buildSampleFromTouch(touch, timestamp, timeMeta);
        const config = this.getConfigForId(eventId);

        if(!ZoneBoundaryChecker.inputMoveCancellationCheck(sample, config, this.safeBoundMaskMap[eventId])) {
          this.onInputMove(eventId, sample, touch.target);
        } else {
          this.onInputMoveCancel(eventId, sample, touch.target);
        }
      }
    });
  }

  onTouchEnd(event: TouchEvent) {
    console.log('onTouchEnd');

    for(let i=0; i < event.changedTouches.length; i++) {
      const touch = event.changedTouches.item(i);
      if(!this.hasRegisteredTouchpoint(touch.identifier) /* || hasPendingTouchpoint */) {
        this.preventPropagation(event);
      }
    }

    this.sequentializer.queueEventFunctor(() => {
      // Only lists touch contact points that have been lifted; touchmove is raised separately if any movement occurred.
      for(let i=0; i < event.changedTouches.length; i++) {
        const touch = event.changedTouches.item(i);
        if(!this.hasRegisteredTouchpoint(touch.identifier)) {
          continue;
        }

        this.onInputEnd(touch.identifier, event.target);
      }
    });
  }
}