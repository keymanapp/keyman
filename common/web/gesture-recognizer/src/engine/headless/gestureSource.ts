import { InputSample } from "./inputSample.js";
import { SerializedGesturePath, GesturePath } from "./gesturePath.js";
import { GestureRecognizerConfiguration, preprocessRecognizerConfig } from "../configuration/gestureRecognizerConfiguration.js";
import { Nonoptional } from "../nonoptional.js";

/**
 * Documents the expected typing of serialized versions of the `GestureSource` class.
 */
export type SerializedGestureSource<HoveredItemType = any> = {
  isFromTouch: boolean;
  path: SerializedGesturePath<HoveredItemType>;
  // identifier is not included b/c it's only needed during live processing.
}

/**
 * Represents all metadata needed internally for tracking a single "touch contact point" / "touchpoint"
 * involved in a potential / recognized gesture as tracked over time.
 *
 * Each instance corresponds to one unique contact point as recognized by `Touch.identifier` or to
 * one 'cursor-point' as represented by mouse-based motion.
 *
 * Refer to https://developer.mozilla.org/en-US/docs/Web/API/Touch and
 * https://developer.mozilla.org/en-US/docs/Web/API/Navigator/maxTouchPoints re "touch contact point".
 *
 * May be one-to-many with recognized gestures:  a keyboard longpress interaction generally only has one
 * contact point but will have multiple realized gestures / components:
 * - longpress:  Enough time has elapsed
 * - subkey:  Subkey from the longpress subkey menu has been selected.
 *
 * Thus, it is a "gesture source".  This is the level needed to model a single contact point, while some
 * gestures expect multiple, hence "simple".
 *
 */
export class GestureSource<HoveredItemType> {
  /**
   * Indicates whether or not this tracked point's original source is a DOM `Touch`.
   */
  public readonly isFromTouch: boolean;

  /**
   * The numeric form of this point's identifier as seen in events (or as emulated for mouse events)
   */
  public readonly rawIdentifier: number;

  // A full, uninterrupted recording of all samples observed during the lifetime of the touchpoint.
  protected _path: GesturePath<HoveredItemType>;

  protected _baseItem: HoveredItemType;

  private static _jsonIdSeed: -1;

  // Assertion:  must always contain an index 0 - the base recognizer config.
  protected recognizerConfigStack: Nonoptional<GestureRecognizerConfiguration<HoveredItemType>>[];

  /**
   * Tracks the coordinates and timestamps of each update for the lifetime of this `GestureSource`.
   */
  public get path(): GesturePath<HoveredItemType> {
    return this._path;
  }

  /**
   * Constructs a new GestureSource instance for tracking updates to an active input point over time.
   * @param identifier     The system identifier for the input point's events.
   * @param initialHoveredItem  The initiating event's original target element
   * @param isFromTouch    `true` if sourced from a `TouchEvent`; `false` otherwise.
   */
  constructor(
    identifier: number,
    recognizerConfig: Nonoptional<GestureRecognizerConfiguration<HoveredItemType>> | Nonoptional<GestureRecognizerConfiguration<HoveredItemType>>[],
    isFromTouch: boolean
  ) {
    this.rawIdentifier = identifier;
    this.isFromTouch = isFromTouch;
    this._path = new GesturePath();

    this.recognizerConfigStack = Array.isArray(recognizerConfig) ? recognizerConfig : [recognizerConfig];
  }

  /**
   * Deserializes a GestureSource instance from its serialized-JSON form.
   * @param jsonObj  The JSON representation to deserialize.
   * @param identifier The unique identifier to assign to this instance.
   */
  public static deserialize(jsonObj: SerializedGestureSource, identifier: number) {
    const id = identifier !== undefined ? identifier : this._jsonIdSeed++;
    const isFromTouch = jsonObj.isFromTouch;
    const path = GesturePath.deserialize(jsonObj.path);

    const instance = new GestureSource(id, null, isFromTouch);
    instance._path = path;
    return instance;
  }

  public update(sample: InputSample<HoveredItemType>) {
    this.path.extend(sample);
    this._baseItem ||= sample.item;
  }

  /**
   * The first path sample (coordinate) under consideration for this `GestureSource`.
   */
  public get baseItem(): HoveredItemType {
    return this._baseItem;
  }

  /**
   * The most recent path sample (coordinate) under consideration for this `GestureSource`.
   */
  public get currentSample(): InputSample<HoveredItemType> {
    return this.path.coords[this.path.coords.length-1];
  }

  /**
   * Creates a 'subview' of the current GestureSource.  It will be updated as the underlying
   * source continues to receive updates until disconnected.
   *
   * @param startAtEnd If `true`, the 'subview' will appear to start at the most recently-observed
   * path coordinate.  If `false`, it will have full knowledge of the current path.
   * @param preserveBaseItem If `true`, the 'subview' will denote its base item as the same
   * as its source.  If `false`, the base item for the 'subview' will be set to the `item` entry
   * from the most recently-observed path coordinate.
   * @returns
   */
  public constructSubview(startAtEnd: boolean, preserveBaseItem: boolean): GestureSourceSubview<HoveredItemType> {
    return new GestureSourceSubview(this, this.recognizerConfigStack, startAtEnd, preserveBaseItem);
  }

  /**
   * Terminates all tracking for the modeled contact point.  Passing `true` as a parameter will
   * treat the touchpath as if it were cancelled; `false` and `undefined` will treat it as if
   * the touchpath has completed its standard lifecycle.
   * @param cancel
   */
  public terminate(cancel?: boolean) {
    this.path.terminate(cancel);
  }

  /**
   * Denotes if the contact point's path either was cancelled or completed its standard
   * lifecycle.
   */
  public get isPathComplete(): boolean {
    return this.path.isComplete;
  }

  /**
   * Gets a fully-unique string-based identifier, even for edge cases where both mouse and touch input
   * are received simultaneously.
   */
  public get identifier(): string {
    const prefix = this.isFromTouch ? 'touch' : 'mouse';
    return `${prefix}:${this.rawIdentifier}`;
  }

  public pushRecognizerConfig(config: Omit<GestureRecognizerConfiguration<HoveredItemType>, 'touchEventRoot'| 'mouseEventRoot'>) {
    const configToProcess = {...config,
      mouseEventRoot: this.recognizerConfigStack[0].mouseEventRoot,
      touchEventRoot: this.recognizerConfigStack[0].touchEventRoot
    }
    this.recognizerConfigStack.push(preprocessRecognizerConfig(configToProcess));
  }

  public popRecognizerConfig() {
    if(this.recognizerConfigStack.length == 1) {
      throw new Error("Cannot 'pop' the original recognizer-configuration for this GestureSource.")
    }

    return this.recognizerConfigStack.pop();
  }

  public get currentRecognizerConfig() {
    return this.recognizerConfigStack[this.recognizerConfigStack.length-1];
  }

  /**
   * Creates a serialization-friendly version of this instance for use by
   * `JSON.stringify`.
   */
  /* c8 ignore start */
  toJSON(): SerializedGestureSource {
    let jsonClone: SerializedGestureSource = {
      isFromTouch: this.isFromTouch,
      path: this.path.toJSON()
    }

    return jsonClone;
    /* c8 ignore stop */
    /* c8 ignore next 2 */
    // esbuild or tsc seems to mangle the 'ignore stop' if put outside the ending brace.
  }
}

export class GestureSourceSubview<HoveredItemType> extends GestureSource<HoveredItemType> {
  private _baseSource: GestureSource<HoveredItemType>
  private _baseStartIndex: number;
  private subviewDisconnector: () => void;

  /**
   * Constructs a new "Subview" into an existing GestureSource instance.  Future updates of the base
   *  GestureSource will automatically be included until this instance's `disconnect` method is called.
   * @param identifier     The system identifier for the input point's events.
   * @param initialHoveredItem  The initiating event's original target element
   * @param isFromTouch    `true` if sourced from a `TouchEvent`; `false` otherwise.
   */
  constructor(
    source: GestureSource<HoveredItemType>,
    configStack: typeof GestureSource.prototype['recognizerConfigStack'],
    startAtEnd: boolean,
    preserveBaseItem: boolean
  ) {
    let mayUpdate = true;
    let start = 0;
    let length = source.path.coords.length;
    if(source instanceof GestureSourceSubview) {
      start = source._baseStartIndex;
      const expectedLength = start + length;
      // Check against the full remaining length of the original source; does
      // the subview provided to us include its source's most recent point?
      const sampleCountSinceStart = source.baseSource.path.coords.length;
      if(expectedLength != start + sampleCountSinceStart) {
        mayUpdate = false;
      }
    }

    super(source.rawIdentifier, configStack, source.isFromTouch);

    const baseSource = this._baseSource = source instanceof GestureSourceSubview ? source._baseSource : source;

    /**
     * Provides a coordinate-system translation for source subviews.
     * The base version still needs to use the original coord system, though.
     */
    const translateSample = (sample: InputSample<HoveredItemType>) => {
      const translation = this.recognizerTranslation;
      // Provide a coordinate-system translation for source subviews.
      // The base version still needs to use the original coord system, though.
      return {...sample, targetX: sample.targetX - translation.x, targetY: sample.targetY - translation.y};
    }

    // Note: we don't particularly need subviews to track the actual coords aside from
    // tracking related stats data.  But... we don't have an "off-switch" for that yet.
    let subpath: GesturePath<HoveredItemType>;

    // Will hold the last sample _even if_ we don't save every coord that comes through.
    const lastSample = source.path.stats.lastSample;

    // Are we 'chop'ping off the existing path or preserving it?  This sets the sample-copying
    // configuration accordingly.
    if(startAtEnd) {
      this._baseStartIndex = start = Math.max(start + length - 1, 0);
      length = length > 0 ? 1 : 0;
    } else {
      this._baseStartIndex = start;
    }

    subpath = new GesturePath<HoveredItemType>();
    for(let i=0; i < length; i++) {
      subpath.extend(translateSample(baseSource.path.coords[start + i]));
    }

    this._path = subpath;

    if(preserveBaseItem) {
      this._baseItem = source.baseItem;
    } else {
      this._baseItem = lastSample?.item;
    }

    if(mayUpdate) {
      // Ensure that this 'subview' is updated whenever the "source of truth" is.
      const completeHook    = ()       => this.path.terminate(false);
      const invalidatedHook = ()       => this.path.terminate(true);
      const stepHook        = (sample: InputSample<HoveredItemType>) => {
        super.update(translateSample(sample));
      };
      baseSource.path.on('complete',    completeHook);
      baseSource.path.on('invalidated', invalidatedHook);
      baseSource.path.on('step',        stepHook);

      // But make sure we can "disconnect" it later once the gesture being matched
      // with the subview has fully matched; it's good to have a snapshot left over.
      this.subviewDisconnector = () => {
        baseSource.path.off('complete',    completeHook);
        baseSource.path.off('invalidated', invalidatedHook);
        baseSource.path.off('step',        stepHook);
      }
    }
  }

  private get recognizerTranslation() {
    // Allowing a 'null' config greatly simplifies many of our unit-test specs.
    if(this.recognizerConfigStack.length == 1 || !this.currentRecognizerConfig) {
      return {
        x: 0,
        y: 0
      };
    }

    // Could compute all of this a single time & cache the value whenever a recognizer-config is pushed or popped.
    const currentRecognizer = this.currentRecognizerConfig;
    const currentClientRect = currentRecognizer.targetRoot.getBoundingClientRect();
    const baseClientRect = this.recognizerConfigStack[0].targetRoot.getBoundingClientRect();

    return {
      x: currentClientRect.x - baseClientRect.x,
      y: currentClientRect.y - baseClientRect.y
    }
  }

  /**
   * The original GestureSource this subview is based upon.  Note that the coordinate system may
   * differ if a gesture stage/component has occurred that triggered a change to the active
   * recognizer configuration.  (e.g. a subkey menu is being displayed for a longpress interaction)
   */
  public get baseSource() {
    return this._baseSource;
  }

  /**
   * This disconnects this subview from receiving further updates from the the underlying
   * source without causing it to be cancelled or treated as completed.
   */
  public disconnect() {
    if(this.subviewDisconnector) {
      this.subviewDisconnector();
      this.subviewDisconnector = null;
    }
  }

  public pushRecognizerConfig(config: Omit<GestureRecognizerConfiguration<HoveredItemType>, "touchEventRoot" | "mouseEventRoot">): void {
    throw new Error("Pushing and popping of recognizer configurations should only be called on the base GestureSource");
  }

  public popRecognizerConfig(): Nonoptional<GestureRecognizerConfiguration<HoveredItemType>> {
    throw new Error("Pushing and popping of recognizer configurations should only be called on the base GestureSource");
  }

  public update(sample: InputSample<HoveredItemType>): void {
    throw new Error("Updates should be provided through the base GestureSource.")
  }

  /**
   * Like `disconnect`, but this will also terminate the baseSource and prevent further
   * updates for the true, original `GestureSource` instance.  If the gesture-model
   * and gesture-matching algorithm has determined this should be called, full path-update
   * termination is correct, even if called against a subview into the instance.
   */
  public terminate(cancel?: boolean) {
    this.baseSource.terminate(cancel);
  }
}