/// <reference path="inputEventCoordinate.ts" />

namespace com.keyman.osk {
  export type InputHandler = (coord: InputEventCoordinate) => void;

  export abstract class InputEventEngine {
    protected readonly eventRoot: HTMLElement;

    private inputStartHandler:  InputHandler;
    private inputMoveHandler:   InputHandler;
    private inputCancelHandler: InputHandler;
    private inputEndHandler:    InputHandler;

    public constructor(
      eventRoot: HTMLElement,
      inputStartHandler:      InputHandler,
      inputMoveHandler:       InputHandler,
      inputMoveCancelHandler: InputHandler,
      inputEndHandler:        InputHandler
    ) {
      this.eventRoot = eventRoot;

      this.inputStartHandler =  inputStartHandler;
      this.inputMoveHandler =   inputMoveHandler;
      this.inputCancelHandler = inputMoveCancelHandler;
      this.inputEndHandler =    inputEndHandler;
    }

    abstract registerEventHandlers();
    abstract unregisterEventHandlers();

    onInputStart(coord: InputEventCoordinate) {
      if(this.inputStartHandler) {
        this.inputStartHandler(coord);
      }
      //this.vkbd.touch(coord);
    }

    onInputMove(coord: InputEventCoordinate) {
      if(this.inputMoveHandler) {
        this.inputMoveHandler(coord);
      }
      //this.vkbd.moveOver(coord);
    }

    onInputMoveCancel(coord: InputEventCoordinate) {
      if(this.inputCancelHandler) {
        this.inputCancelHandler(coord);
      }
      //this.vkbd.moveCancel(coord);
    }

    onInputEnd(coord: InputEventCoordinate) {
      if(this.inputEndHandler) {
        this.inputEndHandler(coord);
      }
      //this.vkbd.release(coord);
    }
  }
}