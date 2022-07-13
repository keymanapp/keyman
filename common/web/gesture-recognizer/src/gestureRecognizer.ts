/// <reference path="gestureRecognizerConfiguration.ts" />
/// <reference path="mutable.ts" />
/// <reference path="includes/events.ts" />

namespace com.keyman.osk {
  export enum TrackedInputState {
    START = "start",
    END = "end",
    MOVE = "move",
    CANCEL = "cancel"
  };

  export class GestureRecognizer extends EventEmitter {
    public static readonly TRACKED_INPUT_UPDATE_EVENT_NAME = "trackedInputUpdate";

    protected readonly config: FinalizedGestureRecognizerConfiguration;

    private readonly mouseEngine: MouseEventEngine;
    private readonly touchEngine: TouchEventEngine;

    protected static preprocessConfig(config: GestureRecognizerConfiguration): FinalizedGestureRecognizerConfiguration {
      // Allows configuration pre-processing during this method.
      let processingConfig: Mutable<FinalizedGestureRecognizerConfiguration> = {...config} as FinalizedGestureRecognizerConfiguration;
      processingConfig.mouseEventRoot = processingConfig.mouseEventRoot ?? processingConfig.targetRoot;
      processingConfig.touchEventRoot = processingConfig.touchEventRoot ?? processingConfig.targetRoot;

      processingConfig.inputStartBounds = processingConfig.inputStartBounds ?? processingConfig.targetRoot;
      processingConfig.maxRoamingBounds = processingConfig.maxRoamingBounds ?? processingConfig.targetRoot;
      processingConfig.safeBounds       = processingConfig.safeBounds       ?? new ViewportZoneSource(2);
      if(typeof config.safeBoundPadding == 'number') {
        processingConfig.safeBoundPadding = [ config.safeBoundPadding ];
      }
      processingConfig.safeBoundPadding = processingConfig.safeBoundPadding ?? [3];

      processingConfig.paddedSafeBounds = new PaddedZoneSource(processingConfig.safeBounds, ...processingConfig.safeBoundPadding);

      return processingConfig;
    }

    public constructor(config: GestureRecognizerConfiguration) {
      super();
      this.config = GestureRecognizer.preprocessConfig(config);

      this.mouseEngine = new MouseEventEngine(this.config);
      this.touchEngine = new TouchEventEngine(this.config);

      this.mouseEngine.registerEventHandlers();
      this.touchEngine.registerEventHandlers();

      const _this = this;
      const forwardingUpdateHandler = (state, coord) => {
        this.emit(GestureRecognizer.TRACKED_INPUT_UPDATE_EVENT_NAME, state, coord);
        return false;
      }

      this.mouseEngine.on(InputEventEngine.INPUT_UPDATE_EVENT_NAME, forwardingUpdateHandler);
      this.touchEngine.on(InputEventEngine.INPUT_UPDATE_EVENT_NAME, forwardingUpdateHandler);
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
}