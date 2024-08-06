import { GestureRecognizerConfiguration } from "./configuration/gestureRecognizerConfiguration.js";
import { InputEventEngine } from "./inputEventEngine.js";
import { Nonoptional } from "./nonoptional.js";
import { ZoneBoundaryChecker } from "./configuration/zoneBoundaryChecker.js";
import { GestureSource } from "./headless/gestureSource.js";

// Does NOT use the AsyncClosureDispatchQueue... simply because there can only ever be one mouse touchpoint.
export class MouseEventEngine<ItemType, StateToken = any> extends InputEventEngine<ItemType, StateToken> {
  private readonly _mouseStart: typeof MouseEventEngine.prototype.onMouseStart;
  private readonly _mouseMove:  typeof MouseEventEngine.prototype.onMouseMove;
  private readonly _mouseEnd:   typeof MouseEventEngine.prototype.onMouseEnd;

  private hasActiveClick: boolean = false;
  private disabledSafeBounds: number = 0;

  private currentSource: GestureSource<ItemType, StateToken> = null;
  private readonly activeIdentifier = 0;

  public constructor(config: Nonoptional<GestureRecognizerConfiguration<ItemType, StateToken>>) {
    super(config);

    // We use this approach, rather than .bind, because _this_ version allows hook
    // insertion for unit tests via prototype manipulation.  The .bind version doesn't.
    this._mouseStart = (event: MouseEvent) => this.onMouseStart(event);
    this._mouseMove  = (event: MouseEvent) => this.onMouseMove(event);
    this._mouseEnd   = (event: MouseEvent) => this.onMouseEnd(event);
  }

  private get eventRoot(): HTMLElement {
    return this.config.mouseEventRoot;
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

  private buildSampleFromEvent(event: MouseEvent) {
    // WILL be null for newly-starting `GestureSource`s / contact points.
    return this.buildSampleFor(event.clientX, event.clientY, event.target, performance.now(), this.currentSource);
  }

  onMouseStart(event: MouseEvent) {
    // If it's not an event we'd consider handling, do not prevent event
    // propagation!  Just don't process it.
    if(!this.config.targetRoot.contains(event.target as Node)) {
      return;
    }

    this.preventPropagation(event);

    const sample = this.buildSampleFromEvent(event);

    if(!ZoneBoundaryChecker.inputStartOutOfBoundsCheck(sample, this.config)) {
      // If we started very close to a safe zone border, remember which one(s).
      // This is important for input-sequence cancellation check logic.
      this.disabledSafeBounds = ZoneBoundaryChecker.inputStartSafeBoundProximityCheck(sample, this.config);
    }

    const touchpoint = this.onInputStart(this.activeIdentifier, sample, event.target, false);
    this.currentSource = touchpoint;

    const cleanup = () => {
      this.currentSource = null;
    }

    touchpoint.path.on('complete', cleanup);
    touchpoint.path.on('invalidated', cleanup);
  }

  onMouseMove(event: MouseEvent) {
    const source = this.currentSource;
    if(!source) {
      return;
    }

    const sample = this.buildSampleFromEvent(event);

    if(!event.buttons) {
      if(this.hasActiveClick) {
        this.hasActiveClick = false;
        this.onInputMoveCancel(source, sample, event.target);
      }
      return;
    }

    this.preventPropagation(event);
    const config = source.currentRecognizerConfig;

    if(!ZoneBoundaryChecker.inputMoveCancellationCheck(sample, config, this.disabledSafeBounds)) {
      this.onInputMove(source, sample, event.target);
    } else {
      this.onInputMoveCancel(source, sample, event.target);
    }
  }

  onMouseEnd(event: MouseEvent) {
    const source = this.currentSource;
    if(!source) {
      return;
    }

    if(!event.buttons) {
      this.hasActiveClick = false;
    }

    this.onInputEnd(source, event.target);
  }
}