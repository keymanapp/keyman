import EventEmitter from "eventemitter3";
import { GestureSource } from "./gestureSource.js";

interface EventMap<HoveredItemType, StateToken> {
  /**
   * Indicates that a new, ongoing touchpoint or mouse interaction has begun.
   * @param input The instance that tracks all future updates over the lifetime of the touchpoint / mouse interaction.
   */
  'pointstart': (input: GestureSource<HoveredItemType, StateToken>) => void;

  // // idea for line below: to help multitouch gestures keep touchpaths in sync, rather than updated separately
  // 'eventcomplete': () => void;
}

/**
 * This serves as an abstract, headless-capable base class for handling incoming touch-path data for
 * gesture recognition as it is either generated (in the DOM) or replayed during automated tests
 * (headlessly).
 */
export abstract class InputEngineBase<HoveredItemType, StateToken = any> extends EventEmitter<EventMap<HoveredItemType, StateToken>> {
  private _activeTouchpoints: GestureSource<HoveredItemType>[] = [];

  public stateToken: StateToken;

  /**
   * @param identifier The identifier number corresponding to the input sequence.
   */
  hasActiveTouchpoint(identifier: number) {
    return this.getTouchpointWithId(identifier) !== undefined;
  }

  protected getTouchpointWithId(identifier: number) {
    return this._activeTouchpoints.find((point) => point.rawIdentifier == identifier);
  }

  /**
   * During the lifetime of a GestureSource (a continuous path for a single touchpoint),
   * it is possible that the legal area for the path may change.  This function allows
   * us to find the appropriate set of constraints for the path if any changes have been
   * requested - say, for a subkey menu after a longpress.
   * @param identifier
   * @returns
   */
  protected getConfigForId(identifier: number) {
    return this.getTouchpointWithId(identifier).currentRecognizerConfig;
  }

  protected getStateTokenForId(identifier: number) {
    return this.getTouchpointWithId(identifier).stateToken ?? null;
  }

  public dropTouchpointWithId(identifier: number) {
    this._activeTouchpoints = this._activeTouchpoints.filter((point) => point.rawIdentifier != identifier);
  }

  protected addTouchpoint(touchpoint: GestureSource<HoveredItemType, StateToken>) {
    this._activeTouchpoints.push(touchpoint);
  }
}