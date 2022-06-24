/// <reference path="inputEventCoordinate.ts" />
/// <reference path="gestureRecognizerConfiguration.ts" />
/// <reference path="includes/events.ts" />

namespace com.keyman.osk {
  type Mutable<Type> = {
    -readonly [Property in keyof Type]: Type[Property];
  };

  export type InputHandler = (coord: InputEventCoordinate) => void;

  export abstract class InputEventEngine extends EventEmitter {
    public static readonly INPUT_UPDATE_EVENT_NAME = "inputUpdate";

    protected readonly config: GestureRecognizerConfiguration;

    protected static preprocessConfig(config: GestureRecognizerConfiguration): GestureRecognizerConfiguration {
      // Allows configuration pre-processing during this method.
      let processingConfig: Mutable<GestureRecognizerConfiguration> = Object.assign({}, config);
      processingConfig.mouseEventRoot = processingConfig.mouseEventRoot ?? processingConfig.targetRoot;
      processingConfig.touchEventRoot = processingConfig.touchEventRoot ?? processingConfig.targetRoot;

      return processingConfig;
    }

    public constructor(config: GestureRecognizerConfiguration) {
      super();

      config = InputEventEngine.preprocessConfig(config);
      this.config = config;
    }

    abstract registerEventHandlers();
    abstract unregisterEventHandlers();

    onInputStart(coord: InputEventCoordinate) {
      this.emit(InputEventEngine.INPUT_UPDATE_EVENT_NAME, TrackedInputState.START, coord);
    }

    onInputMove(coord: InputEventCoordinate) {
      this.emit(InputEventEngine.INPUT_UPDATE_EVENT_NAME, TrackedInputState.MOVE, coord);
    }

    onInputMoveCancel(coord: InputEventCoordinate) {
      this.emit(InputEventEngine.INPUT_UPDATE_EVENT_NAME, TrackedInputState.CANCEL, coord);
    }

    onInputEnd(coord: InputEventCoordinate) {
      this.emit(InputEventEngine.INPUT_UPDATE_EVENT_NAME, TrackedInputState.END, coord);
    }
  }
}