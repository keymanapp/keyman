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

    protected readonly config: GestureRecognizerConfiguration;

    private readonly mouseEngine: MouseEventEngine;
    private readonly touchEngine: TouchEventEngine;

    protected static preprocessConfig(config: GestureRecognizerConfiguration): GestureRecognizerConfiguration {
      // Allows configuration pre-processing during this method.
      let processingConfig: Mutable<GestureRecognizerConfiguration> = {...config};
      processingConfig.mouseEventRoot = processingConfig.mouseEventRoot ?? processingConfig.targetRoot;
      processingConfig.touchEventRoot = processingConfig.touchEventRoot ?? processingConfig.targetRoot;

      return processingConfig;
    }

    public constructor(config: GestureRecognizerConfiguration) {
      super();
      config = GestureRecognizer.preprocessConfig(config);
      this.config = config;

      this.mouseEngine = new MouseEventEngine(config);
      this.touchEngine = new TouchEventEngine(config);

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