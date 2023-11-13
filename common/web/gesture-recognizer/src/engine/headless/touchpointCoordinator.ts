import EventEmitter from "eventemitter3";
import { InputEngineBase } from "./inputEngineBase.js";
import { buildGestureMatchInspector, GestureSource } from "./gestureSource.js";
import { MatcherSelection, MatcherSelector } from "./gestures/matchers/matcherSelector.js";
import { GestureSequence } from "./gestures/matchers/gestureSequence.js";
import { GestureModelDefs, getGestureModel, getGestureModelSet } from "./gestures/specs/gestureModelDefs.js";
import { GestureModel } from "./gestures/specs/gestureModel.js";

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

  public constructor(gestureModelDefinitions: GestureModelDefs<HoveredItemType, StateToken>, inputEngines?: InputEngineBase<HoveredItemType, StateToken>[]) {
    super();

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

  public popSelector(selector: MatcherSelector<HoveredItemType, StateToken>) {
    /* c8 ignore start */
    if(this.selectorStack.length <= 1) {
      throw new Error("May not pop the original, base gesture selector.");
    }

    const index = this.selectorStack.indexOf(selector);
    if(index == -1) {
      throw new Error("This selector has not been pushed onto the 'setChange' stack.");
    }
    /* c8 ignore end */

    selector.off('rejectionwithaction', this.modelResetHandler);
    selector.cascadeTermination();

    this.selectorStack.splice(index, 1);
    // Make sure the current state token is set at this stage.
    this.currentSelector.stateToken = this.stateToken;
  }

  public selectorStackIncludes(selector: MatcherSelector<HoveredItemType, StateToken>): boolean {
    return this.selectorStack.includes(selector);
  }

  public get currentSelector() {
    return this.selectorStack[this.selectorStack.length-1];
  }

  protected addEngine(engine: InputEngineBase<HoveredItemType, StateToken>) {
    engine.on('pointstart', this.onNewTrackedPath);
    this.inputEngines.push(engine);
  }

  private readonly onNewTrackedPath = (touchpoint: GestureSource<HoveredItemType>) => {
    this.addSimpleSourceHooks(touchpoint);
    const modelDefs = this.gestureModelDefinitions;
    const selector = this.currentSelector;

    touchpoint.setGestureMatchInspector(buildGestureMatchInspector(selector));

    /* We wait for the source to fully pass through the gesture-model spin-up phase; there's
     * a chance that the new source will complete an existing gesture instantly without being
     * locked to it, resulting in activation of a different `stateToken`.
     *
     * This, in turn, can affect what the initial 'item' for the new gesture will be.
     */
    const modelingSpinupPromise = selector.matchGesture(touchpoint, getGestureModelSet(modelDefs, selector.baseGestureSetId));
    modelingSpinupPromise.then(async (selectionPromiseHost) => {
      this.emit('inputstart', touchpoint);

      const selection = await selectionPromiseHost.selectionPromise;

      // Any related 'push' mechanics that may still be lingering are currently handled by GestureSequence
      // during its 'completion' processing.  (See `GestureSequence.selectionHandler`.)
      if(selection.result.matched == false) {
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

      this.emit('recognizedgesture', gestureSequence);
    });
  }

  public get activeGestures(): GestureSequence<HoveredItemType, StateToken>[] {
    return [].concat(this._activeGestures);
  }

  public get activeSources(): GestureSource<HoveredItemType, StateToken>[] {
    return [].concat(this.inputEngines.map((engine) => engine.activeSources).reduce((merged, arr) => merged.concat(arr), []));
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