/// <reference path="inputEventCoordinate.ts" />
/// <reference path="gestureRecognizerConfiguration.ts" />

namespace com.keyman.osk {
  type Mutable<Type> = {
    -readonly [Property in keyof Type]: Type[Property];
  };

  export type InputHandler = (coord: InputEventCoordinate) => void;

  export abstract class InputEventEngine {
    protected readonly config: GestureRecognizerConfiguration;

    protected static preprocessConfig(config: GestureRecognizerConfiguration): GestureRecognizerConfiguration {
      // Allows configuration pre-processing during this method.
      let processingConfig: Mutable<GestureRecognizerConfiguration> = Object.assign({}, config);
      processingConfig.mouseEventRoot = processingConfig.mouseEventRoot ?? processingConfig.targetRoot;
      processingConfig.touchEventRoot = processingConfig.touchEventRoot ?? processingConfig.targetRoot;

      return processingConfig;
    }

    public constructor(config: GestureRecognizerConfiguration) {
      config = InputEventEngine.preprocessConfig(config);
      this.config = config;
    }

    abstract registerEventHandlers();
    abstract unregisterEventHandlers();

    onInputStart(coord: InputEventCoordinate) {
      if(this.config.inputStartHandler) {
        this.config.inputStartHandler(coord);
      }
    }

    onInputMove(coord: InputEventCoordinate) {
      if(this.config.inputMoveHandler) {
        this.config.inputMoveHandler(coord);
      }
    }

    onInputMoveCancel(coord: InputEventCoordinate) {
      if(this.config.inputMoveCancelHandler) {
        this.config.inputMoveCancelHandler(coord);
      }
    }

    onInputEnd(coord: InputEventCoordinate) {
      if(this.config.inputEndHandler) {
        this.config.inputEndHandler(coord);
      }
    }
  }
}