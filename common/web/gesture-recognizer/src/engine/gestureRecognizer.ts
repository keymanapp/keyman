import { GestureRecognizerConfiguration, preprocessRecognizerConfig } from "./configuration/gestureRecognizerConfiguration.js";
import { MouseEventEngine } from "./mouseEventEngine.js";
import { Nonoptional } from "./nonoptional.js";
import { TouchEventEngine } from "./touchEventEngine.js";
import { TouchpointCoordinator } from "./headless/touchpointCoordinator.js";
import { EMPTY_GESTURE_DEFS, GestureModelDefs } from "./headless/gestures/specs/index.js";

export class GestureRecognizer<HoveredItemType, StateToken = any> extends TouchpointCoordinator<HoveredItemType, StateToken> {
  public readonly config: Nonoptional<GestureRecognizerConfiguration<HoveredItemType, StateToken>>;

  private readonly mouseEngine: MouseEventEngine<HoveredItemType>;
  private readonly touchEngine: TouchEventEngine<HoveredItemType>;

  public constructor(gestureModelDefinitions: GestureModelDefs<HoveredItemType, StateToken>, config: GestureRecognizerConfiguration<HoveredItemType, StateToken>) {
    const preprocessedConfig = preprocessRecognizerConfig(config);

    // Possibly just a stop-gap measure... but this provides an empty gesture-spec set definition
    // that allows testing the path-constrainment functionality without invoking gesture-processing
    // overhead.
    gestureModelDefinitions = gestureModelDefinitions || EMPTY_GESTURE_DEFS;

    super(gestureModelDefinitions, null, preprocessedConfig.historyLength);
    this.config = preprocessedConfig;

    this.mouseEngine = new MouseEventEngine<HoveredItemType>(this.config);
    this.touchEngine = new TouchEventEngine<HoveredItemType>(this.config);

    this.mouseEngine.registerEventHandlers();
    this.touchEngine.registerEventHandlers();

    this.addEngine(this.mouseEngine);
    this.addEngine(this.touchEngine);
  }

  public destroy() {
    // When shutting down the gesture engine, we should go ahead and clear out all related
    // gesture-source tracking.
    this.activeGestures.forEach((sequence) => sequence.cancel());
    this.activeSources.forEach((source) => source.terminate(true));

    this.mouseEngine.unregisterEventHandlers();
    this.touchEngine.unregisterEventHandlers();

    // Because these two fields are marked readonly, we can't directly delete them.
    // Because they're private, we can't apply Mutable to make them deletable.
    // So... awkward cast + assignment it is.
    (this.mouseEngine as any) = null;
    (this.touchEngine as any) = null;
  }
}