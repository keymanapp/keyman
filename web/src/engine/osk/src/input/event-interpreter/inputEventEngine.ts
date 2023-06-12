import InputEventCoordinate from '../inputEventCoordinate.js';

export type InputHandler = (coord: InputEventCoordinate) => void;

export interface InputEventEngineConfig {
  /**
   * Specifies the element that input listeners should be attached to.
   */
  readonly eventRoot: HTMLElement;
  /**
   * Specifies the most specific common ancestor element of any event target
   * that the InputEventEngine should consider.
   */
  readonly targetRoot: HTMLElement;

  readonly coordConstrainedWithinInteractiveBounds: (coord: InputEventCoordinate) => boolean;

  readonly inputStartHandler?:      InputHandler;
  readonly inputMoveHandler?:       InputHandler;
  readonly inputMoveCancelHandler?: InputHandler;
  readonly inputEndHandler?:        InputHandler;
}

export default abstract class InputEventEngine {
  protected readonly config: InputEventEngineConfig;

  public constructor(config: InputEventEngineConfig) {
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