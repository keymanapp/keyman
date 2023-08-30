import { GestureRecognizerConfiguration, preprocessRecognizerConfig } from "./configuration/gestureRecognizerConfiguration.js";
import { MouseEventEngine } from "./mouseEventEngine.js";
import { Nonoptional } from "./nonoptional.js";
import { TouchEventEngine } from "./touchEventEngine.js";
import { TouchpointCoordinator } from "./headless/touchpointCoordinator.js";

export class GestureRecognizer<HoveredItemType> extends TouchpointCoordinator<HoveredItemType> {
  public readonly config: Nonoptional<GestureRecognizerConfiguration<HoveredItemType>>;

  private readonly mouseEngine: MouseEventEngine<HoveredItemType>;
  private readonly touchEngine: TouchEventEngine<HoveredItemType>;

  public constructor(config: GestureRecognizerConfiguration<HoveredItemType>) {
    super();
    this.config = preprocessRecognizerConfig(config);

    this.mouseEngine = new MouseEventEngine<HoveredItemType>(this.config);
    this.touchEngine = new TouchEventEngine<HoveredItemType>(this.config);

    this.mouseEngine.registerEventHandlers();
    this.touchEngine.registerEventHandlers();

    this.addEngine(this.mouseEngine);
    this.addEngine(this.touchEngine);
  }

  public destroy() {
    this.mouseEngine.unregisterEventHandlers();
    this.touchEngine.unregisterEventHandlers();

    // Because these two fields are marked readonly, we can't directly delete them.
    // Because they're private, we can't apply Mutable to make them deletable.
    // So... awkward cast + assignment it is.
    (this.mouseEngine as any) = null;
    (this.touchEngine as any) = null;
  }
}