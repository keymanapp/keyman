import { EventEmitter } from "eventemitter3";
import { InputEngineBase } from "./inputEngineBase.js";
import { GestureSource } from "./gestureSource.js";
import { MatcherSelection, MatcherSelector } from "./gestures/matchers/matcherSelector.js";
import { GestureSequence } from "./gestures/matchers/gestureSequence.js";
import { GestureModelDefs, getGestureModel, getGestureModelSet } from "./gestures/specs/gestureModelDefs.js";
import { GestureModel } from "./gestures/specs/gestureModel.js";
import { InputSample } from "./inputSample.js";
import { GestureDebugPath } from "./gestureDebugPath.js";
import { reportError } from "../reportError.js";

interface EventMap<HoveredItemType, StateToken> {
  /**
   * Indicates that a new potential gesture has begun.
   * @param input
   * @returns
   */
  'inputstart': (input: GestureSource<HoveredItemType, StateToken>) => void;

  'recognizedgesture': (sequence: GestureSequence<HoveredItemType, StateToken>) => void;
}

/**
 * This class is responsible for interpreting the output of the various input-engine types
 * and facilitating the detection of related gestures.  Its role is to serve as a headless
 * version of the main `GestureRecognizer` class, avoiding its DOM and DOM-event dependencies.
 *
 * Of particular note: when a gesture involves multiple touchpoints - like a multitap - this class
 * is responsible for linking related touchpoints together for the detection of that gesture.
 */
export class TouchpointCoordinator<HoveredItemType, StateToken=any> extends EventEmitter<EventMap<HoveredItemType, StateToken>> {
  private inputEngines: InputEngineBase<HoveredItemType, StateToken>[];
  private selectorStack: MatcherSelector<HoveredItemType, StateToken>[] = [new MatcherSelector()];

  private gestureModelDefinitions: GestureModelDefs<HoveredItemType, StateToken>;

  private _activeSources: GestureSource<HoveredItemType>[] = [];
  private _activeGestures: GestureSequence<HoveredItemType, StateToken>[] = [];

  private _stateToken: StateToken;

  private _history: (GestureSource<HoveredItemType> | GestureSequence<HoveredItemType, StateToken>)[] = [];
  private historyMax: number;

  public constructor(gestureModelDefinitions: GestureModelDefs<HoveredItemType, StateToken>, inputEngines?: InputEngineBase<HoveredItemType, StateToken>[], historyLength?: number) {
    super();

    this.historyMax = historyLength > 0 ? historyLength : 0;

    this.gestureModelDefinitions = gestureModelDefinitions;
    this.inputEngines = [];
    if(inputEngines) {
      for(let engine of inputEngines) {
        this.addEngine(engine);
      }
    }

    this.selectorStack[0].on('rejectionwithaction', this.modelResetHandler)
  }

  private readonly modelResetHandler = (
    selection: MatcherSelection<HoveredItemType, StateToken>,
    replaceModelWith: (model: GestureModel<HoveredItemType, StateToken>) => void
  ) => {
    const sourceIds = selection.matcher.allSourceIds;

    // If there's an active gesture that uses a source noted in the selection, it's the responsibility
    // of an existing GestureSequence to handle this one.  The handler should bypass it for this round.
    if(this.activeGestures.find((sequence) => {
      return sequence.allSourceIds.find((a) => sourceIds.indexOf(a) != -1);
    })) {
      return;
    }

    if(selection.result.action.type == 'replace') {
      replaceModelWith(getGestureModel(this.gestureModelDefinitions, selection.result.action.replace));
    } else {
      throw new Error("Missed a case in implementation!");
    }
  };

  public pushSelector(selector: MatcherSelector<HoveredItemType, StateToken>) {
    this.selectorStack.push(selector);
    selector.on('rejectionwithaction', this.modelResetHandler);
  }

  public sustainSelectorSubstack(selector: MatcherSelector<HoveredItemType, StateToken>) {
    if(!selector) {
      return [];
    }

    // If it's already been popped, just silently return.
    const index = this.selectorStack.indexOf(selector);
    if(index == -1) {
      return [];
    }

    /* c8 ignore start */
    if(this.selectorStack.length <= 1) {
      throw new Error("May not force the original, base gesture selector into sustain mode.");
    }
    /* c8 ignore end */

    let sustainedSources: GestureSource<HoveredItemType, any>[] = [];

    for(let i = index; i < this.selectorStack.length; i++) {
      selector = this.selectorStack[i];

      // If there are any models active with the `sustainWhenNested` property,
      // the following Promise resolves once those are also completed.
      sustainedSources = sustainedSources.concat(selector.cascadeTermination());
    }

    return sustainedSources;
  }

  public popSelector(selector: MatcherSelector<HoveredItemType, StateToken>) {
    if(!selector) {
      return;
    }

    // If it's already been popped, just silently return.
    const index = this.selectorStack.indexOf(selector);
    if(index == -1) {
      return;
    }

    /* c8 ignore start */
    if(this.selectorStack.length <= 1) {
      throw new Error("May not pop the original, base gesture selector.");
    }
    /* c8 ignore end */

    while(index < this.selectorStack.length) {
      selector = this.selectorStack[index];
      selector.off('rejectionwithaction', this.modelResetHandler);

      this.selectorStack.splice(index, 1);
    }

    // Should be fine as-is for now b/c modipress is always a base-selector gesture and is
    // the only thing modifying stateToken within KeymanWeb.  May need an async/await in
    // the future if other things become able to manipulate state tokens with this engine.

    // Make sure the current state token is set at this stage.
    this.currentSelector.stateToken = this.stateToken;
  }

  public selectorStackIncludes(selector: MatcherSelector<HoveredItemType, StateToken>): boolean {
    return this.selectorStack.includes(selector);
  }

  public get currentSelector() {
    return this.selectorStack[this.selectorStack.length-1];
  }

  private buildGestureMatchInspector(selector: MatcherSelector<HoveredItemType, StateToken>) {
    return (source: GestureSource<HoveredItemType, StateToken>) => {
      // Get the selectors at the time of the call, not at the time of the functor's construction.
      const selectorIndex = this.selectorStack.indexOf(selector);
      const selectors = this.selectorStack.slice(selectorIndex);

      return selectors.map((selector) => selector.potentialMatchersForSource(source).map((matcher) => matcher.model.id))
                      .reduce((flattened, entry) => flattened.concat(entry));
    };
  }

  protected addEngine(engine: InputEngineBase<HoveredItemType, StateToken>) {
    engine.on('pointstart', this.onNewTrackedPath);
    this.inputEngines.push(engine);
  }

  private recordHistory(gesture: typeof this._history[0]) {
    const histMax = this.historyMax;
    if(histMax > 0) {
      if(this._history.length == histMax) {
        this._history.shift();
      }
      this._history.push(gesture);
    }
  }

  private readonly onNewTrackedPath = async (touchpoint: GestureSource<HoveredItemType>) => {
    this.addSimpleSourceHooks(touchpoint);
    const modelDefs = this.gestureModelDefinitions;

    let potentialSelector: MatcherSelector<HoveredItemType, StateToken>;
    let selectionPromise: Promise<MatcherSelection<HoveredItemType, StateToken>>;
    do {
      potentialSelector = this.currentSelector;

      /* We wait for the source to fully pass through the gesture-model spin-up phase; there's
      * a chance that the new source will complete an existing gesture instantly without being
      * locked to it, resulting in activation of a different `stateToken`.
      *
      * This, in turn, can affect what the initial 'item' for the new gesture will be.
      */
      const modelingSpinupPromise = potentialSelector.matchGesture(touchpoint, getGestureModelSet(modelDefs, potentialSelector.baseGestureSetId));
      const modelingSpinupResults = await modelingSpinupPromise;

      if(modelingSpinupResults.sustainModeWithoutMatch) {

        const correctSample = (sample: InputSample<HoveredItemType, StateToken>) => {
          sample.stateToken = this.stateToken;
          sample.item = touchpoint.currentRecognizerConfig.itemIdentifier(sample, null);
        };

        /* May need to do a state-token change check & update the item; an `awaitNested` 'complete' action
         * may have been pending in the meantime that could have triggered a change.
         *
         * (The MatcherSelector's state token will not have been updated b/c it will have already been popped,
         * and because it's popped, it should not be responsible for managing the new GestureSource -
         * including shifts in state token.)
         *
         * Current actual use-case:  deferred modipress due to ongoing flick, auto-completed by new incoming touch.
         */
        if(touchpoint.path instanceof GestureDebugPath) {
          touchpoint.path.coords.forEach(correctSample);
        }

        // Don't forget to also correct the `stateToken` and `baseItem`!
        touchpoint.stateToken = this.stateToken;
        touchpoint.baseItem = touchpoint.path.stats.initialSample.item;

        // Also, in case a contact model's path-eval references data via stats...
        correctSample(touchpoint.path.stats.initialSample);
        correctSample(touchpoint.path.stats.lastSample);
        continue;
      } else {
        selectionPromise = modelingSpinupResults.selectionPromise;
        break;
      }

      // Can only happen if a `sustainWhenNested` model state is resolved, nested within
      // a gesture whose completion action requests `awaitNested`.
    } while(potentialSelector != this.currentSelector);

    const selector = this.currentSelector;

    touchpoint.setGestureMatchInspector(this.buildGestureMatchInspector(selector));

    const preGestureScribe = () => {
      this.recordHistory(touchpoint);
    }

    /*
      If there's an error in code receiving this event, we must not let that break the flow of
      event input processing - we may still have a locking Promise corresponding to our active
      GestureSource.  (See: next comment)
    */
    try {
      touchpoint.path.on('invalidated', preGestureScribe);
      this.emit('inputstart', touchpoint);
    } catch (err) {
      reportError("Error from 'inputstart' event listener", err);
    }

    /*
      If an `InputEventEngine` internally utilizes the `AsyncClosureDispatchQueue`, this is the point
      at which we are now safe to process further events.  The correct 'stateToken' has been identified
      and all GestureMatcher possibilities for the source have been launched; path updates may resume _and_
      new incoming paths may now be safely handled.  As such, we can now fulfill any Promise returned by
      a closure defined within its `inputStart` method for the `GestureSource` under consideration.

      It is quite important that we _do_ fulfill the `Promise` if it exists - further event processing will
      be blocked for such engines until this is done!  (Hence the try-catch above)
    */
    this.inputEngines.forEach((engine) => {
      engine.fulfillInputStart(touchpoint);
    });

    // ----------------------------------------

    // All gesture-matching is prepared; now we await the source's first gesture model match.
    const selection = await selectionPromise;

    // Any related 'push' mechanics that may still be lingering are currently handled by GestureSequence
    // during its 'completion' processing.  (See `GestureSequence.selectionHandler`.)
    if(!selection || selection.result.matched == false) {
      return;
    }

    // For multitouch gestures, only report the gesture **once**.
    const sourceIDs = selection.matcher.allSourceIds;
    for(let sequence of this._activeGestures) {
      if(!!sequence.allSourceIds.find((id1) => !!sourceIDs.find((id2) => id1 == id2))) {
        // We've already established (and thus, already reported) a GestureSequence for this selection.
        return;
      }
    }

    const gestureSequence = new GestureSequence(selection, modelDefs, this.currentSelector, this);
    this._activeGestures.push(gestureSequence);
    gestureSequence.on('complete', () => {
      // When the GestureSequence is fully complete and all related `firstSelectionPromise`s have
      // had the chance to resolve, drop the reference; prevent memory leakage.
      const index = this._activeGestures.indexOf(gestureSequence);
      if(index != -1) {
        this._activeGestures.splice(index, 1);
      }
    });

    // Could track sequences easily enough; the question is how to tell when to 'let go'.

    // No try-catch because only there's no critical code after it.
    if(!touchpoint.path.wasCancelled) {
      touchpoint.path.off('invalidated', preGestureScribe);
      gestureSequence.on('complete', () => this.recordHistory(gestureSequence));
    }
    this.emit('recognizedgesture', gestureSequence);
  }

  public get activeGestures(): GestureSequence<HoveredItemType, StateToken>[] {
    return [].concat(this._activeGestures);
  }

  public get activeSources(): GestureSource<HoveredItemType, StateToken>[] {
    return [].concat(this.inputEngines.map((engine) => engine.activeSources).reduce((merged, arr) => merged.concat(arr), []));
  }

  public get history() {
    return this._history;
  }

  public get historyJSON() {
    const sanitizingReplacer = function (key: string, value) {
      if(key == 'item') {
        // KMW 'key' elements involve circular refs.
        // Just return the key ID.  (Assumes use in KMW)
        return value?.id;
      } else {
        return value;
      }
    }

    return JSON.stringify(this.history, sanitizingReplacer, 2);
  }

  /**
   * The current 'state token' to be set for newly-starting gestures for use by gesture-recognizer
   * consumers, their item-identifier lookup functions, and their gesture model definitions.
   *
   * Use of this feature is intended to be strictly optional and only used in scenarios where
   * the recognizer's consumer needs some sort of system-state to be associated with ongoing gestures.
   */
  public get stateToken(): StateToken {
    return this._stateToken;
  }

  public set stateToken(token: StateToken) {
    this._stateToken = token;
    this.inputEngines.forEach((engine) => engine.stateToken = token);
    this.currentSelector.stateToken = token;
  }

  private addSimpleSourceHooks(touchpoint: GestureSource<HoveredItemType>) {

    touchpoint.path.on('invalidated', () => {
      // GestureSequence _should_ handle any other cleanup internally as fallout
      // from the path being cancelled.
      //
      // That said, it's handled asynchronously... but we can give a synchronous signal
      // through the next block of code, allowing cleanup to occur earlier during
      // recovery states.

      const owningSequence = this.activeGestures.find((entry) => entry.allSourceIds.includes(touchpoint.identifier));
      if(owningSequence) {
        owningSequence.cancel();
      }

      // To consider: should it specially mark if it 'completed' due to cancellation,
      // or is that safe to infer from the tracked GestureSource(s)?
      // Currently, we're going with the latter.

      // Also mark the touchpoint as no longer active.
      let i = this._activeSources.indexOf(touchpoint);
      this._activeSources = this._activeSources.splice(i, 1);
    });
    touchpoint.path.on('complete', () => {
      // Also mark the touchpoint as no longer active.
      let i = this._activeSources.indexOf(touchpoint);
      this._activeSources = this._activeSources.splice(i, 1);
    });
  }
}