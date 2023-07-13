import EventEmitter from "eventemitter3";
import { InputEngineBase } from "./inputEngineBase.js";
import { TrackedInput } from "./trackedInput.js";
import { SimpleGestureSource } from "./simpleGestureSource.js";

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

  private _activeInputs: {[id: string]: TrackedInput<HoveredItemType>} = {};

  public constructor() {
    super();
    this.inputEngines = [];
  }

  protected addEngine(engine: InputEngineBase<HoveredItemType>) {
    engine.on('pointstart', this.onNewTrackedPath);
    this.inputEngines.push(engine);
  }

  private readonly onNewTrackedPath = (touchpoint: SimpleGestureSource<HoveredItemType>) => {
    const newInput = new TrackedInput<HoveredItemType>(touchpoint);
    this._activeInputs[touchpoint.identifier] = newInput;

    this.emit('inputstart', newInput);
    return false;
  }
}