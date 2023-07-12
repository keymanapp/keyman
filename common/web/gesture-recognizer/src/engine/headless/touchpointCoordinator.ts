import EventEmitter from "eventemitter3";
import { InputEngineBase } from "./inputEngineBase.js";
import { TrackedInput } from "./trackedInput.js";
import { TrackedPoint } from "./trackedPoint.js";

interface EventMap<HoveredItemType> {
  /**
   * Indicates that a new potential gesture has begun.
   * @param input
   * @returns
   */
  'inputstart': (input: TrackedInput<HoveredItemType>) => void;
}

/**
 * This class is responsible for interpreting the output of the various input-engine types
 * and facilitating the detection of related gestures.  Its role is to serve as a headless
 * version of the main `GestureRecognizer` class, avoiding its DOM and DOM-event dependencies.
 *
 * Of particular note: when a gesture involves multiple touchpoints - like a multitap - this class
 * is responsible for linking related touchpoints together for the detection of that gesture.
 */
export class TouchpointCoordinator<HoveredItemType> extends EventEmitter<EventMap<HoveredItemType>> {
  private inputEngines: InputEngineBase<HoveredItemType>[];

  private _activeTouchpointMap: {[id: string]: TrackedInput<HoveredItemType>} = {};
  private _activeInputs: TrackedInput<HoveredItemType>[] = [];

  public constructor() {
    super();
    this.inputEngines = [];
  }

  protected addEngine(engine: InputEngineBase<HoveredItemType>) {
    engine.on('pointstart', this.onNewTrackedPath);
    this.inputEngines.push(engine);
  }

  private readonly onNewTrackedPath = (touchpoint: TrackedPoint<HoveredItemType>) => {
    // Step 1:  do we have any other active inputs?  If so, how many?  Might this touchpoint fit well with one
    // of them?  What gestures might the touchpoint get folded into?

    // TODO:  something to define 'priority'
    // Probably don't need a priority queue here; there shouldn't be that many entries at once, at least in general use.
    let potentialGestureSets = this._activeInputs.map((input) => {
      return input.potentialGestures.map((gesture) => {
        return {
          input: input,
          gesture: gesture
        };
      });
    });
    let potentialGestures = potentialGestureSets.reduce((flattenedArray, set) => {
      return flattenedArray.concat(set)
    }, []);
    potentialGestures.sort((a, b) => a.gesture.priority - b.gesture.priority);
    for(let potentialGesture of potentialGestures) {
      // Possibilities:
      // 1.  A gesture says 'wait, there's another touchpath?  I should abort!'
      // 2.  A gesture says 'hey, I was waiting on that - gimme!
      // 3.  No existing gestures, so this loop is bypassed.
      // 4.  Existing gestures, but none of them care.
    }

    // Step ???: if the touchpoint does not belong with any other gestures, give it its own TrackedInput.
    const newInput = this.wrapPointWithInput(touchpoint);
    this.emit('inputstart', newInput);
  }

  private onPointUpdate(touchpoint: TrackedPoint<HoveredItemType>) {
    // stuff.
  }

  private wrapPointWithInput(touchpoint: TrackedPoint<HoveredItemType>): TrackedInput<HoveredItemType> {
    const newInput = new TrackedInput<HoveredItemType>(touchpoint);
    this._activeTouchpointMap[touchpoint.identifier] = newInput;

    // It will be possible for this._activeInputs[touchpoint.identifier] to change
    // during certain gestures, so use that rather than `newInput`.
    touchpoint.path.on('step', () => this.onPointUpdate(touchpoint));
    touchpoint.path.on('invalidated', () => {
      // TODO: on cancellation, is there any other cleanup to be done?

      // Also mark the touchpoint as no longer active.
      delete this._activeTouchpointMap[touchpoint.identifier];
    });
    touchpoint.path.on('complete', () => {
      // TODO: on cancellation, is there any other cleanup to be done?

      // Also mark the touchpoint as no longer active.
      delete this._activeTouchpointMap[touchpoint.identifier];
    });

    this._activeInputs.push(newInput);
    return newInput;
  }
}