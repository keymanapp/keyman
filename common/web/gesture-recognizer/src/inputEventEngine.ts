import { EventEmitter } from "eventemitter3";
import { GestureRecognizerConfiguration } from "./gestureRecognizerConfiguration.js";
import { InputSample } from "./inputSample.js";
import { Nonoptional } from "./nonoptional.js";
import { TrackedPoint } from "./trackedPoint.js";
import { TouchEventEngine } from "./touchEventEngine.js";

interface EventMap {
  'pointstart': (input: TrackedPoint) => void
}

export abstract class InputEventEngine extends EventEmitter<EventMap> {
  protected readonly config: Nonoptional<GestureRecognizerConfiguration>;
  private _activeTouchpoints: TrackedPoint[] = [];

  public constructor(config: Nonoptional<GestureRecognizerConfiguration>) {
    super();
    this.config = config;
  }

  abstract registerEventHandlers(): void;
  abstract unregisterEventHandlers(): void;

  /**
   * @param identifier The identifier number corresponding to the input sequence.
   */
  hasActiveTouchpoint(identifier: number) {
    return this.getTouchpointWithId(identifier) !== undefined;
  }

  private getTouchpointWithId(identifier: number) {
    return this._activeTouchpoints.find((point) => point.rawIdentifier == identifier);
  }

  public dropTouchpointWithId(identifier: number) {
    this._activeTouchpoints = this._activeTouchpoints.filter((point) => point.rawIdentifier != identifier);
  }

  protected buildSampleFor(clientX: number, clientY: number): InputSample {
    const targetRect = this.config.targetRoot.getBoundingClientRect();
    return {
      clientX: clientX,
      clientY: clientY,
      targetX: clientX - targetRect.left,
      targetY: clientY - targetRect.top,
      t: performance.now()
    };
  }

  onInputStart(identifier: number, sample: InputSample, target: EventTarget) {
    const touchpoint = new TrackedPoint(identifier, target, this instanceof TouchEventEngine);
    touchpoint.path.extend(sample);

    this._activeTouchpoints.push(touchpoint);

    // External objects may desire to directly terminate handling of
    // input sequences under specific conditions.
    touchpoint.path.on('invalidated', () => {
      this.dropTouchpointWithId(identifier);
    });

    touchpoint.path.on('complete', () => {
      this.dropTouchpointWithId(identifier);
    });

    this.emit('pointstart', touchpoint);
  }

  onInputMove(identifier: number, sample: InputSample) {
    this.getTouchpointWithId(identifier)?.path.extend(sample);
  }

  onInputMoveCancel(identifier: number, sample: InputSample) {
    const touchpoint = this.getTouchpointWithId(identifier);
    touchpoint?.path.extend(sample);
    touchpoint?.path.terminate(true);
  }

  onInputEnd(identifier: number) {
    // We do not add extend the path here because any 'end' event immediately
    // follows a 'move' if it occurred simultaneously.
    this.getTouchpointWithId(identifier)?.path.terminate(false);
  }
}