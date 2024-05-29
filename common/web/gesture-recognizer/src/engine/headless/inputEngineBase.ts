import { EventEmitter } from "eventemitter3";

import { GestureRecognizerConfiguration } from "../configuration/gestureRecognizerConfiguration.js";
import { Nonoptional } from "../nonoptional.js";
import { GestureDebugSource } from "./gestureDebugSource.js";
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

  // Touch interactions in the browser actually _re-use_ touch IDs once they lapse; the IDs are not lifetime-unique.
  // This gesture-engine desires lifetime-unique IDs, though, so we map them within this engine to remedy that problem.
  private readonly identifierMap: Record<number, number> = {};
  private static IDENTIFIER_SEED = 0;

  public stateToken: StateToken;

  protected readonly config: Nonoptional<GestureRecognizerConfiguration<HoveredItemType, StateToken>>;
  private sourceConstructor: typeof GestureSource<HoveredItemType, StateToken, any>;

  public constructor(config: Nonoptional<GestureRecognizerConfiguration<HoveredItemType, StateToken>>) {
    super();
    this.config = config;
    this.sourceConstructor = (config?.recordingMode ?? true) ? GestureDebugSource : GestureSource;
  }

  createTouchpoint(identifier: number, isFromTouch: boolean) {
    // IDs provided to `GestureSource` should be engine-unique.  Unfortunately, the base identifier patterns provided by
    // browsers don't do this, so we map the browser ID to an engine-internal one.
    const unique_id = InputEngineBase.IDENTIFIER_SEED++;

    this.identifierMap[identifier] = unique_id;

    // If debug mode is enabled, will enable persistent coordinate tracking.  Is off by default.
    const source = new this.sourceConstructor(unique_id, this.config, isFromTouch);
    source.stateToken = this.stateToken;

    // Do not add here; it needs special managing for unit tests.

    return source;
  }

  public fulfillInputStart(touchpoint: GestureSource<HoveredItemType, StateToken>) {}

  /**
   * Calls to this method will cancel any touchpoints whose internal IDs are _not_ included in the parameter.
   * Designed to facilitate recovery from error cases and peculiar states that sometimes arise when debugging.
   * @param identifiers
   */
  maintainTouchpoints(touchpoints: GestureSource<HoveredItemType, StateToken>[]) {
    touchpoints ||= [];
    this._activeTouchpoints
      .filter((source) => !touchpoints.includes(source))
      // Will trigger `.dropTouchpoint` later in the event chain.
      .forEach((source) => source.terminate(true));
  }

  /**
   * @param identifier The identifier number corresponding to the input sequence.
   */
  hasActiveTouchpoint(identifier: number) {
    return this.identifierMap[identifier] !== undefined;
  }

  /**
   * Retrieves the GestureSource (corresponding to a single touchpoint) corresponding
   * to the specified internal identifier.  Internal ID -> unique ID mapping is
   * performed here.
   * @param identifier
   * @returns
   */
  protected getTouchpointWithId(identifier: number) {
    const id = this.identifierMap[identifier];
    return this._activeTouchpoints.find((point) => point.rawIdentifier == id);
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
    // protected - so, used internally only within the input engines.
    // `getTouchpointWithId` will perform the internal -> external ID mapping.
    return this.getTouchpointWithId(identifier).currentRecognizerConfig;
  }

  protected getStateTokenForId(identifier: number) {
    // protected - so, used internally only within the input engines.
    // `getTouchpointWithId` will perform the internal -> external ID mapping.
    return this.getTouchpointWithId(identifier).stateToken ?? null;
  }

  protected dropTouchpoint(point: GestureSource<HoveredItemType>) {
    const id = point.rawIdentifier;

    this._activeTouchpoints = this._activeTouchpoints.filter((pt) => point != pt);
    for(const key of Object.keys(this.identifierMap)) {
      if(this.identifierMap[key] == id) {
        delete this.identifierMap[key];
      }
    }
  }

  protected addTouchpoint(touchpoint: GestureSource<HoveredItemType, StateToken>) {
    this._activeTouchpoints.push(touchpoint);
  }

  public get activeSources(): GestureSource<HoveredItemType, StateToken>[] {
    return [].concat(this._activeTouchpoints);
  }
}