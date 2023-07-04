import InputEventEngine, { InputEventEngineConfig } from './inputEventEngine.js';
import InputEventCoordinate from '../inputEventCoordinate.js';

export default class MouseEventEngine extends InputEventEngine {
  private readonly _mouseStart: typeof MouseEventEngine.prototype.onMouseStart;
  private readonly _mouseMove:  typeof MouseEventEngine.prototype.onMouseMove;
  private readonly _mouseEnd:   typeof MouseEventEngine.prototype.onMouseEnd;

  private hasActiveClick: boolean = false;
  private ignoreSequence: boolean = false;

  public constructor(config: InputEventEngineConfig) {
    super(config);

    this._mouseStart = this.onMouseStart.bind(this);
    this._mouseMove  = this.onMouseMove.bind(this);
    this._mouseEnd   = this.onMouseEnd.bind(this);
  }

  registerEventHandlers() {
    this.config.eventRoot.addEventListener('mousedown', this._mouseStart, true);
    this.config.eventRoot.addEventListener('mousemove',  this._mouseMove, false);
    // The listener below fails to capture when performing automated testing checks in Chrome emulation unless 'true'.
    this.config.eventRoot.addEventListener('mouseup',   this._mouseEnd, true);
  }

  unregisterEventHandlers() {
    this.config.eventRoot.removeEventListener('mousedown', this._mouseStart, true);
    this.config.eventRoot.removeEventListener('mousemove',  this._mouseMove, false);
    this.config.eventRoot.removeEventListener('mouseup',   this._mouseEnd, true);
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

    if(this.config.coordConstrainedWithinInteractiveBounds(coord)) {
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