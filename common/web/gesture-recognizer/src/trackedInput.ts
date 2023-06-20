import EventEmitter from "eventemitter3";
import { JSONTrackedPoint, TrackedPoint } from "./trackedPoint.js";

/**
 * Documents the expected typing of serialized versions of the `TrackedInput` class.
 */
export interface JSONTrackedInput {
  touchpoints: JSONTrackedPoint[];
  // gesture: Gesture;
}

interface EventMap {
  'end':    () => void;
  'cancel': () => void;
}


/**
 * Models a single ongoing input event, which may or may not involve multiple
 * touchpoints.
 *
 * _Supported events_:
 *
 * `'cancel'`:  all gesture recognition for this input is to be cancelled
 *                   and left incomplete.
 * - Provides no parameters.
 *
 * `'end'`:     all gesture recognition for this input is to be resolved.
 *   - Provides no parameters.
 */
export class TrackedInput extends EventEmitter<EventMap> {
  public readonly touchpoints: TrackedPoint[];

  // --- Future design aspects ---
  // private _gesture: Gesture;
  // public get gesture() { return this._gesture };

  private isActive = true;

  constructor(basePoint: TrackedPoint) {
    super();

    this.touchpoints = [ basePoint ];
    this._attachPointHooks(basePoint);
  }

  private _attachPointHooks(touchpoint: TrackedPoint) {
    touchpoint.path.on('complete', () => {
      this.isActive = false;
      this.emit('end');
      this.removeAllListeners();
    });

    touchpoint.path.on('invalidated', () => {
      this.isActive = false;
      this.emit('cancel');
      this.removeAllListeners();
    })
  }

  cancel() {
    if(this.isActive) {
      for(let point of this.touchpoints) {
        point.path.terminate(true);
      }
    }
  }

  end() {
    if(this.isActive) {
      for(let point of this.touchpoints) {
        point.path.terminate(false);
      }
    }
  }

  /**
   * Creates a serialization-friendly version of this instance for use by
   * `JSON.stringify`.
   */
  toJSON(): JSONTrackedInput {
    return {
      touchpoints: this.touchpoints.map((point) => point.toJSON())
    };
  }
}