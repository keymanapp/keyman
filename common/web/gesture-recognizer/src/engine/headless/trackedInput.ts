import EventEmitter from "eventemitter3";
import { JSONTrackedPoint, TrackedPoint } from "./trackedPoint.js";
import * as Gestures from "./gestures/index.js";

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
export class TrackedInput<HoveredItemType> extends EventEmitter<EventMap> {
  public readonly touchpoints: TrackedPoint<HoveredItemType>[];

  private _potentialGestures: Gestures.InputModel<HoveredItemType>[];

  private isActive = true;

  constructor(basePoint: TrackedPoint<HoveredItemType>) {
    super();

    this.touchpoints = [ basePoint ];
    this._attachPointHooks(basePoint);
  }

  private _attachPointHooks(touchpoint: TrackedPoint<HoveredItemType>) {
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

  get potentialGestures(): Gestures.InputModel<HoveredItemType>[] {
    return [].concat(this._potentialGestures);
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