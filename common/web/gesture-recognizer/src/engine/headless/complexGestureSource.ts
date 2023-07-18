import EventEmitter from "eventemitter3";
import { SerializedSimpleGestureSource, SimpleGestureSource } from "./simpleGestureSource.js";

/**
 * Documents the expected typing of serialized versions of the `ComplexGestureSource` class.
 */
export interface SerializedComplexGestureSource {
  touchpoints: SerializedSimpleGestureSource[];
  // gesture: Gesture;
}

interface EventMap<Type> {
  'newcontact': (contact: SimpleGestureSource<Type>) => void;
  'end':    () => void;
  'cancel': () => void;
}


/**
 * Models all ongoing contact that is considered part of the same single gesture
 * or sequence of chained Gestures over time.  This may or may not involve
 * multiple touch contact points / "SimpleGestureSource" instances.
 *
 * Note that multiple chained gestures may arise over the lifetime of a single
 * instance of this class.  For example, detecting a multitap requires
 * multiple contact points over time, possibly with each tap arising as a
 * potential 'last' tap gesture before new ones are received to continue the
 * sequence.
 *
 * _Supported events_:
 *
 * `'cancel'`:  all gesture recognition for this input is to be cancelled
 *              and left incomplete.
 * - Provides no parameters.
 *
 * `'end'`:     all gesture recognition for this input is to be resolved.
 *   - Provides no parameters.
 */
export class ComplexGestureSource<HoveredItemType> extends EventEmitter<EventMap<HoveredItemType>> {
  public readonly touchpoints: SimpleGestureSource<HoveredItemType>[];

  // --- Future design aspects ---
  // private _gesture: Gesture;
  // public get gesture() { return this._gesture };

  private isActive = true;

  constructor(basePoint: SimpleGestureSource<HoveredItemType>) {
    super();

    this.touchpoints = [ basePoint ];
    this._attachPointHooks(basePoint);
  }

  private _attachPointHooks(touchpoint: SimpleGestureSource<HoveredItemType>) {
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

  addTouchpoint(touchpoint: SimpleGestureSource<HoveredItemType>) {
    this.touchpoints.push(touchpoint);
    this._attachPointHooks(touchpoint);
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

  public get hasSyncedPaths(): boolean {
    if(this.touchpoints.length <= 1) {
      return true;
    } else {
      const timestamp = this.touchpoints[0].currentSample.t;

      for(let i=1; i < this.touchpoints.length; i++) {
        if(this.touchpoints[i].currentSample.t != timestamp) {
          return false;
        }
      }

      return true;
    }
  }

  /**
   * Creates a serialization-friendly version of this instance for use by
   * `JSON.stringify`.
   */
  toJSON(): SerializedComplexGestureSource {
    return {
      touchpoints: this.touchpoints.map((point) => point.toJSON())
    };
  }
}