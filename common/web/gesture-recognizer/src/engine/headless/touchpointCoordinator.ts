import EventEmitter from "eventemitter3";
import { InputEngineBase } from "./inputEngineBase.js";
import { SimpleGestureSource, SimpleGestureSourceSubview } from "./simpleGestureSource.js";

interface EventMap<HoveredItemType> {
  /**
   * Indicates that a new potential gesture has begun.
   * @param input
   * @returns
   */
  'inputstart': (input: SimpleGestureSource<HoveredItemType>) => void;
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

  private _activeSources: SimpleGestureSource<HoveredItemType>[] = [];

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

    // ... stuff

    this.emit('inputstart', touchpoint);
  }

  private doGestureUpdate(source: SimpleGestureSource<HoveredItemType>) {
    // Should probably ensure data-updates for multi-contact gestures are synchronized
    // before proceeding.  Single-contact cases are inherently synchronized, of course.
    //
    // Should a gesture type have geometric requirements on the current location of active
    // touchpaths, having a desync during a quick movement could cause the calculated
    // distance between the locations to be markedly different than expected.

    // TODO: stuff, including synchronization.  Probably do that on the caller,
    // rather than here?
  }

  private addSimpleSourceHooks(touchpoint: SimpleGestureSource<HoveredItemType>) {
    touchpoint.path.on('step', () => this.doGestureUpdate(touchpoint));

    touchpoint.path.on('invalidated', () => {
      // TODO: on cancellation, is there any other cleanup to be done?

      // Also mark the touchpoint as no longer active.
      let i = this._activeSources.indexOf(touchpoint);
      this._activeSources = this._activeSources.splice(i, 1);
    });
    touchpoint.path.on('complete', () => {
      // TODO: on cancellation, is there any other cleanup to be done?

      // Also mark the touchpoint as no longer active.
      let i = this._activeSources.indexOf(touchpoint);
      this._activeSources = this._activeSources.splice(i, 1);
    });
  }
}