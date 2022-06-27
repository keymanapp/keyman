/// <reference path="gestureRecognizerConfiguration.ts" />
/// <reference path="includes/events.ts" />

namespace com.keyman.osk {
  type Mutable<Type> = {
    -readonly [Property in keyof Type]: Type[Property];
  };

  export enum TrackedInputState {
    START = "start",
    END = "end",
    MOVE = "move",
    CANCEL = "cancel"
  };

  export class GestureRecognizer extends EventEmitter {
    public static readonly TRACKED_INPUT_UPDATE_EVENT_NAME = "trackedInputUpdate";

    protected readonly config: FinalizedGestureRecognizerConfiguration;

    private readonly mouseEngine?: MouseEventEngine;
    private readonly touchEngine?: TouchEventEngine;

    protected static preprocessConfig(config: GestureRecognizerConfiguration): FinalizedGestureRecognizerConfiguration {
      // Allows configuration pre-processing during this method.
      let processingConfig: Mutable<FinalizedGestureRecognizerConfiguration> = Object.assign({}, config) as FinalizedGestureRecognizerConfiguration;
      processingConfig.mouseEventRoot = processingConfig.mouseEventRoot ?? processingConfig.targetRoot;
      processingConfig.touchEventRoot = processingConfig.touchEventRoot ?? processingConfig.targetRoot;

      processingConfig.inputStartBounds = processingConfig.inputStartBounds ?? processingConfig.targetRoot;
      processingConfig.maxRoamingBounds = processingConfig.maxRoamingBounds ?? processingConfig.targetRoot;
      processingConfig.safeBounds       = processingConfig.safeBounds       ?? new ViewportZoneSource(2);
      processingConfig.safeBoundPadding = processingConfig.safeBoundPadding ?? 3;

      processingConfig.paddedSafeBounds = new PaddedZoneSource(processingConfig.safeBounds, processingConfig.safeBoundPadding);

      return processingConfig;
    }

    public constructor(config: GestureRecognizerConfiguration) {
      super();
      this.config = GestureRecognizer.preprocessConfig(config);

      this.mouseEngine = new MouseEventEngine(config);
      this.touchEngine = new TouchEventEngine(config);

      this.mouseEngine?.registerEventHandlers();
      this.touchEngine?.registerEventHandlers();

      const _this = this;
      this.mouseEngine?.on(InputEventEngine.INPUT_UPDATE_EVENT_NAME, function(state, coord) {
        _this.emit(GestureRecognizer.TRACKED_INPUT_UPDATE_EVENT_NAME, state, coord);
        return false;
      });

      this.touchEngine?.on(InputEventEngine.INPUT_UPDATE_EVENT_NAME, function(state, coord) {
        _this.emit(GestureRecognizer.TRACKED_INPUT_UPDATE_EVENT_NAME, state, coord);
        return false;
      });
    }

    public destroy() {
      this.mouseEngine?.unregisterEventHandlers();
      this.touchEngine?.unregisterEventHandlers();

      (this.mouseEngine as any) = null;
      (this.touchEngine as any) = null;
    }
  }
}