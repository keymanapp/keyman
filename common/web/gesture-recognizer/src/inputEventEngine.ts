/// <reference path="inputEventCoordinate.ts" />
/// <reference path="gestureRecognizerConfiguration.ts" />
/// <reference path="includes/events.ts" />

namespace com.keyman.osk {
  export type InputHandler = (coord: InputEventCoordinate) => void;

  export abstract class InputEventEngine extends EventEmitter {
    public static readonly INPUT_UPDATE_EVENT_NAME = "inputUpdate";

    protected readonly config: Nonoptional<GestureRecognizerConfiguration>;

    public constructor(config: Nonoptional<GestureRecognizerConfiguration>) {
      super();
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