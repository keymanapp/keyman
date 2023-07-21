import EventEmitter from "eventemitter3";
import { InputEngineBase } from "./inputEngineBase.js";
import { ComplexGestureSource } from "./complexGestureSource.js";
import { SimpleGestureSource } from "./simpleGestureSource.js";

interface EventMap<HoveredItemType> {
  /**
   * Indicates that a new potential gesture has begun.
   * @param input
   * @returns
   */
  'inputstart': (input: ComplexGestureSource<HoveredItemType>) => void;
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

  private _activeSourcesMap: {[id: string]: ComplexGestureSource<HoveredItemType>} = {};
  private _activeSources: ComplexGestureSource<HoveredItemType>[] = [];

  public constructor() {
    super();
    this.inputEngines = [];
  }

  protected addEngine(engine: InputEngineBase<HoveredItemType>) {
    engine.on('pointstart', this.onNewTrackedPath);
    this.inputEngines.push(engine);
  }

  private readonly onNewTrackedPath = (touchpoint: SimpleGestureSource<HoveredItemType>) => {
    this.addSimpleSourceHooks(touchpoint);

    // ... stuff.

    // If no active ComplexGestureSource entries may match the incoming touchpoint, we have a new
    // ComplexGestureSource.
    const newInput = this.establishNewComplexSource(touchpoint);

    this.emit('inputstart', newInput);
    return false;
  }

  private doGestureUpdate(source: ComplexGestureSource<HoveredItemType>) {
    // Should probably ensure data-updates for multi-contact gestures are synchronized
    // before proceeding.  Single-contact cases are inherently synchronized, of course.
    //
    // Should a gesture type have geometric requirements on the current location of active
    // touchpaths, having a desync during a quick movement could cause the calculated
    // distance between the locations to be markedly different than expected.
    if(!source.hasSyncedPaths) {
      return;
    }

    // TODO: stuff.
  }

  private addSimpleSourceHooks(touchpoint: SimpleGestureSource<HoveredItemType>) {
    // It will be possible for this._activeInputs[touchpoint.identifier] to change during certain
    // gestures, so use that - within each handler - for lookups rather than the current `newInput`.

    // ----------

    touchpoint.path.on('step', () => this.doGestureUpdate(this._activeSourcesMap[touchpoint.identifier]));

    touchpoint.path.on('invalidated', () => {
      // TODO: on cancellation, is there any other cleanup to be done?

      // Also mark the touchpoint as no longer active.
      delete this._activeSourcesMap[touchpoint.identifier];
    });
    touchpoint.path.on('complete', () => {
      // TODO: on cancellation, is there any other cleanup to be done?

      // Also mark the touchpoint as no longer active.
      delete this._activeSourcesMap[touchpoint.identifier];
    });
  }

  private establishNewComplexSource(touchpoint: SimpleGestureSource<HoveredItemType>) {
    const newInput = new ComplexGestureSource<HoveredItemType>(touchpoint);
    this._activeSourcesMap[touchpoint.identifier] = newInput;
    this._activeSources.push(newInput);
    return newInput;
  }
}