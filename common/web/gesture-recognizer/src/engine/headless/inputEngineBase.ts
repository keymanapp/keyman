import EventEmitter from "eventemitter3";
import { GestureSource } from "./gestureSource.js";

interface EventMap<HoveredItemType> {
  /**
   * Indicates that a new, ongoing touchpoint or mouse interaction has begun.
   * @param input The instance that tracks all future updates over the lifetime of the touchpoint / mouse interaction.
   */
  'pointstart': (input: GestureSource<HoveredItemType>) => void;

  // // idea for line below: to help multitouch gestures keep touchpaths in sync, rather than updated separately
  // 'eventcomplete': () => void;
}

/**
 * This serves as an abstract, headless-capable base class for handling incoming touch-path data for
 * gesture recognition as it is either generated (in the DOM) or replayed during automated tests
 * (headlessly).
 */
export abstract class InputEngineBase<HoveredItemType> extends EventEmitter<EventMap<HoveredItemType>> {
  private _activeTouchpoints: GestureSource<HoveredItemType>[] = [];

  /**
   * @param identifier The identifier number corresponding to the input sequence.
   */
  hasActiveTouchpoint(identifier: number) {
    return this.getTouchpointWithId(identifier) !== undefined;
  }

  protected getTouchpointWithId(identifier: number) {
    return this._activeTouchpoints.find((point) => point.rawIdentifier == identifier);
  }

  public dropTouchpointWithId(identifier: number) {
    this._activeTouchpoints = this._activeTouchpoints.filter((point) => point.rawIdentifier != identifier);
  }

  protected addTouchpoint(touchpoint: GestureSource<HoveredItemType>) {
    this._activeTouchpoints.push(touchpoint);
  }
}