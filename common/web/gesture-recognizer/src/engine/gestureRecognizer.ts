import { GestureRecognizerConfiguration } from "./configuration/gestureRecognizerConfiguration.js";
import { MouseEventEngine } from "./mouseEventEngine.js";
import { Mutable } from "./mutable.js";
import { Nonoptional } from "./nonoptional.js";
import { PaddedZoneSource } from "./configuration/paddedZoneSource.js";
import { TouchEventEngine } from "./touchEventEngine.js";
import { TouchpointCoordinator } from "./headless/touchpointCoordinator.js";

export class GestureRecognizer<HoveredItemType> extends TouchpointCoordinator<HoveredItemType> {
  public readonly config: Nonoptional<GestureRecognizerConfiguration<HoveredItemType>>;

  private readonly mouseEngine: MouseEventEngine<HoveredItemType>;
  private readonly touchEngine: TouchEventEngine<HoveredItemType>;

  protected static preprocessConfig<HoveredItemType>(
    config: GestureRecognizerConfiguration<HoveredItemType>
  ): Nonoptional<GestureRecognizerConfiguration<HoveredItemType>> {
    // Allows configuration pre-processing during this method.
    let processingConfig: Mutable<Nonoptional<GestureRecognizerConfiguration<HoveredItemType>>> = {...config} as Nonoptional<GestureRecognizerConfiguration<HoveredItemType>>;
    processingConfig.mouseEventRoot = processingConfig.mouseEventRoot ?? processingConfig.targetRoot;
    processingConfig.touchEventRoot = processingConfig.touchEventRoot ?? processingConfig.targetRoot;

    processingConfig.inputStartBounds = processingConfig.inputStartBounds ?? processingConfig.targetRoot;
    processingConfig.maxRoamingBounds = processingConfig.maxRoamingBounds ?? processingConfig.targetRoot;
    processingConfig.safeBounds       = processingConfig.safeBounds       ?? new PaddedZoneSource([2]);

    processingConfig.itemIdentifier   = processingConfig.itemIdentifier   ?? (() => null);

    if(!config.paddedSafeBounds) {
      let paddingArray = config.safeBoundPadding;
      if(typeof paddingArray == 'number') {
        paddingArray = [ paddingArray ];
      }
      paddingArray = paddingArray ?? [3];

      processingConfig.paddedSafeBounds = new PaddedZoneSource(processingConfig.safeBounds, paddingArray);
    } else {
      // processingConfig.paddedSafeBounds is already set via the spread operator above.
      delete processingConfig.safeBoundPadding;
    }

    return processingConfig;
  }

  public constructor(config: GestureRecognizerConfiguration<HoveredItemType>) {
    super();
    this.config = GestureRecognizer.preprocessConfig(config);

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