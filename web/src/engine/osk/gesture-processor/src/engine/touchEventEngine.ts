import { GestureRecognizerConfiguration } from "./configuration/gestureRecognizerConfiguration.js";
import { InputEventEngine } from "./inputEventEngine.js";
import { Nonoptional } from "./nonoptional.js";
import { ZoneBoundaryChecker } from "./configuration/zoneBoundaryChecker.js";
import { GestureSource } from "./headless/gestureSource.js";
import { ManagedPromise } from "@keymanapp/web-utils";
import { AsyncClosureDispatchQueue } from "./headless/asyncClosureDispatchQueue.js";
import { GesturePath } from "./index.js";

function touchListToArray(list: TouchList) {
  const arr: Touch[] = [];

  for(let i=0; i < list.length; i++) {
    arr.push(list.item(i));
  }

  return arr;
}
export class TouchEventEngine<ItemType, StateToken = any> extends InputEventEngine<ItemType, StateToken> {
  private readonly _touchStart: typeof TouchEventEngine.prototype.onTouchStart;
  private readonly _touchMove:  typeof TouchEventEngine.prototype.onTouchMove;
  private readonly _touchEnd:   typeof TouchEventEngine.prototype.onTouchEnd;

  protected readonly eventDispatcher = new AsyncClosureDispatchQueue();

  private safeBoundMaskMap: {[id: number]: number} = {};
  // This map works synchronously with the actual event handlers.
  private pendingSourcePromises: Map<number, ManagedPromise<GestureSource<ItemType, StateToken>>> = new Map();
  private inputStartSignalMap: Map<GestureSource<ItemType, StateToken>, ManagedPromise<void>> = new Map();

  public constructor(config: Nonoptional<GestureRecognizerConfiguration<ItemType, StateToken>>) {
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

  public dropTouchpoint(source: GestureSource<ItemType>) {
    super.dropTouchpoint(source);

    for(const key of Object.keys(this.safeBoundMaskMap)) {
      const keyVal = Number.parseInt(key, 10);
      if(this.getTouchpointWithId(keyVal) == source) {
        delete this.safeBoundMaskMap[keyVal];
      }
    }
  }

  public fulfillInputStart(touchpoint: GestureSource<ItemType, StateToken, GesturePath<ItemType, StateToken>>) {
    const lock = this.inputStartSignalMap.get(touchpoint);
    if(lock) {
      this.inputStartSignalMap.delete(touchpoint);
      lock.resolve();
    }
  };

  public hasActiveTouchpoint(identifier: number): boolean {
    const baseResult = super.hasActiveTouchpoint(identifier);
    return baseResult || !!this.pendingSourcePromises.has(identifier);
  }

  private buildSampleFromTouch(touch: Touch, timestamp: number, source: GestureSource<ItemType, StateToken>) {
    // WILL be null for newly-starting `GestureSource`s / contact points.
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
    const oldTouches = allTouches.filter((touch1) => {
      return newTouches.findIndex(touch2 => touch1.identifier == touch2.identifier) == -1;
    });

    // Any 'old touches' should have pre-existing entries in our promise-map that are still current, as
    // the promise-map is maintained 100% synchronously with incoming events.
    const oldSourcePromises = oldTouches.map((touch) => this.pendingSourcePromises.get(touch.identifier));

    this.eventDispatcher.runAsync(async () => {
      const oldSources = await Promise.all(oldSourcePromises);
      // Maintain all touches in the `.touches` array that are NOT marked as `.changedTouches` (and therefore, new)
      this.maintainTouchpoints(oldSources);

      return this.eventDispatcher.defaultWait;
    });

    /*
      We create Promises that can be set and retrieved synchronously with the actual event handlers
      in order to prevent issues from tricky asynchronous identifier-to-source mapping attempts.

      As these Promises are set (and thus, retrievable) synchronously with the actual event handlers,
      we can closure-capture them for use in the internally-asynchronous processing closures.

      `capturedSourcePromises` will be useful for closure-capture binding the new Promise(s) to
      the closure to be queued.  `this.pendingSourcePromises` facilitates similar closure-capture
      patterns within the touchMove and touchEnd handlers for their queued closures.
    */
    const capturedSourcePromises = new Map<number, ManagedPromise<GestureSource<ItemType, StateToken>>>();
    for(let i=0; i < event.changedTouches.length; i++) {
      const touch = event.changedTouches.item(i);
      const promise = new ManagedPromise<GestureSource<ItemType, StateToken>>();
      this.pendingSourcePromises.set(touch.identifier, promise);
      capturedSourcePromises.set(touch.identifier, promise);
    }

    /*
      When multiple touchpoints are active, we need to ensure a specific order of events.
      The easiest way to ensure the exact order involves programmatic delay of their
      processing, essentially "sequentializing" the events into a deterministic order.

      It also helps to ensure that any path updates are only emitted when all listeners
      for that path have been prepared - and other parts of the engine cause that to happen
      asynchronously in certain situations.  Within KMW, one such case is when a simple-tap
      with `nextLayer` defined is auto-completed by a new incoming touch, triggering an
      instant layer-change.
    */
    this.eventDispatcher.runAsync(() => {
      // Ensure the same timestamp is used for all touches being updated.
      const timestamp = performance.now();
      let touchpoint: GestureSource<ItemType, StateToken> = null;

      // During a touch-start, only _new_ touch contact points are listed here;
      // we shouldn't signal "input start" for any previously-existing touch points,
      // so `.changedTouches` is the best way forward.
      for(let i=0; i < event.changedTouches.length; i++) {
        const touch = event.changedTouches.item(i);
        const touchId = touch.identifier;
        const sample = this.buildSampleFromTouch(touch, timestamp, null);

        if(!ZoneBoundaryChecker.inputStartOutOfBoundsCheck(sample, this.config)) {
          // If we started very close to a safe zone border, remember which one(s).
          // This is important for input-sequence cancellation check logic.
          this.safeBoundMaskMap[touchId] = ZoneBoundaryChecker.inputStartSafeBoundProximityCheck(sample, this.config);
        } else {
          // This touchpoint shouldn't be considered; do not signal a touchstart for it.
          let sourcePromise = capturedSourcePromises.get(touchId);
          sourcePromise.resolve(null);
          continue;
        }

        touchpoint = this.onInputStart(touchId, sample, event.target, true);

        /*
          We use the closure-captured version bound to this specific closure, rather than the
          most recent one for the touch-identifier - under heavy rapid typing, it's possible that
          the touch-identifier has been reused.

          The resolved Promise may then be used to retrieve the correct source in the other event
          handlers' closures.
        */
        let sourcePromise = capturedSourcePromises.get(touchId);
        sourcePromise.resolve(touchpoint);

        /*
          Ensure we only do the cleanup if and when it hasn't already been replaced by new events later.

          Must be done for EACH source - we can't risk leaving a lingering entry once we've dismissed
          processing for the source.  Failure to do so may result in blocking touch events that should
          no longer be manipulated by this engine by affecting `hasActiveTouchpoint`.
        */
        const cleanup = () => {
          /*
            If delays accumulate significantly, it is possible that when this queued closure is run,
            a different touchpoint is reusing the same identifier.  Don't delete the entry if our
            entry has been replaced.
          */
          if(this.pendingSourcePromises.get(touchId) == sourcePromise) {
            this.pendingSourcePromises.delete(touchId);
          }
        }

        touchpoint.path.on('complete', cleanup);
        touchpoint.path.on('invalidated', cleanup);
      }

      if(touchpoint) {
        // This 'lock' should only be released when the last simultaneously-registered touch is published via
        // gesture-recognizer event.
        let eventSignalPromise = new ManagedPromise<void>();
        this.inputStartSignalMap.set(touchpoint, eventSignalPromise);

        return eventSignalPromise.corePromise;
      } else {
        return Promise.resolve();
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

    /*
      Using the Promise map built in touchStart, we can retrieve a Promise for the source linked
      to this event and closure-capture it for the closure queued below.
    */
    const capturedSourcePromises = new Map<number, Promise<GestureSource<ItemType, StateToken>>>();
    for(let i = 0; i < event.touches.length; i++) {
      const touchId = event.touches.item(i).identifier;
      // If the source's gesture is finalized or cancelled but touch events are ongoing,
      // with no delay between event and its processing, the map entry here will be cleared.
      capturedSourcePromises.set(touchId, this.pendingSourcePromises.get(touchId)?.corePromise);
    }

    this.eventDispatcher.runAsync(async () => {
      const touches = await Promise.all(capturedSourcePromises.values());
      this.maintainTouchpoints(touches);

      return this.eventDispatcher.defaultWait;
    });

    /*
      When multiple touchpoints are active, we need to ensure a specific order of events.
      The easiest way to ensure the exact order involves programmatic delay of their
      processing, essentially "sequentializing" the events into a deterministic order.

      It also helps to ensure that any path updates are only emitted when all listeners
      for that path have been prepared - and other parts of the engine cause that to happen
      asynchronously in certain situations.  Within KMW, one such case is when a simple-tap
      with `nextLayer` defined is auto-completed by a new incoming touch, triggering an
      instant layer-change.
    */
    this.eventDispatcher.runAsync(async () => {
      // Ensure the same timestamp is used for all touches being updated.
      const timestamp = performance.now();

      // Do not change to `changedTouches` - we need a sample for all active touches in order
      // to facilitate path-update synchronization for multi-touch gestures.
      //
      // May be worth doing changedTouches _first_ though.
      for(let i=0; i < event.touches.length; i++) {
        const touch = event.touches.item(i);
        const touchId = touch.identifier;


        // Only lists touch contact points that have been lifted; touchmove is
        // raised separately if any movement occurred.
        //
        // If the promise object could not be assigned, we `await undefined` -
        // which JS converts to `await Promise.resolve(undefined)`.  It's safe.
        const source = await capturedSourcePromises.get(touchId);
        if(!source || source.isPathComplete) {
          continue;
        }

        const config = source.currentRecognizerConfig;
        const sample = this.buildSampleFromTouch(touch, timestamp, source);

        if(!ZoneBoundaryChecker.inputMoveCancellationCheck(sample, config, this.safeBoundMaskMap[touchId])) {
          this.onInputMove(source, sample, touch.target);
        } else {
          this.onInputMoveCancel(source, sample, touch.target);
        }
      }

      /*
        Since we're operating within an async function, a Promise return-type
        is implied.  That cancels out the default wait, but we want to ensure
        that the default wait is applied here.
      */
      return this.eventDispatcher.defaultWait;
    });
  }

  onTouchEnd(event: TouchEvent) {
    for(let i = 0; i < event.changedTouches.length; i++) {
      const touch = event.changedTouches.item(i);
      if(this.hasActiveTouchpoint(touch.identifier)) {
        this.preventPropagation(event);
        break;
      }
    }

    /*
      Using the Promise map built in touchStart, we can retrieve a Promise for the source linked
      to this event and closure-capture it for the closure queued below.
    */
    const capturedSourcePromises = new Map<number, Promise<GestureSource<ItemType, StateToken>>>();
    // Any ending touches don't show up in event.touches - only in event.changedTouches!
    for(let i = 0; i < event.changedTouches.length; i++) {
      const touchId = event.changedTouches.item(i).identifier;
      // If the source's gesture is finalized or cancelled but touch events are ongoing,
      // with no delay between event and its processing, the map entry here will be cleared.
      const promiseToCapture = this.pendingSourcePromises.get(touchId)?.corePromise;
      capturedSourcePromises.set(touchId, promiseToCapture);
    }

    this.eventDispatcher.runAsync(async () => {
      // Only lists touch contact points that have been lifted; touchmove is
      // raised separately if any movement occurred.
      //
      // If the promise object could not be assigned, we `await undefined` -
      // which JS converts to `await Promise.resolve(undefined)`.  It's safe.
      for(let i=0; i < event.changedTouches.length; i++) {
        const touch = event.changedTouches.item(i);

        const source = await capturedSourcePromises.get(touch.identifier);
        if(!source || source.isPathComplete) {
          continue;
        }

        this.onInputEnd(source, event.target);
      }

      return this.eventDispatcher.defaultWait;
    });
  }
}