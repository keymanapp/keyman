/// <reference path="inputEventCoordinate.ts" />

namespace com.keyman.osk {
  export abstract class InputEventEngine {
    protected readonly vkbd: VisualKeyboard;
    protected readonly eventRoot: HTMLElement;

    public constructor(vkbd: VisualKeyboard, eventRoot: HTMLElement) {
      this.vkbd = vkbd;
      this.eventRoot = eventRoot;
    }

    abstract registerEventHandlers();
    abstract unregisterEventHandlers();

    onInputStart(coord: InputEventCoordinate) {
      this.vkbd.touch(coord);
    }

    onInputMove(coord: InputEventCoordinate) {
      this.vkbd.moveOver(coord);
    }

    onInputMoveCancel(coord: InputEventCoordinate) {
      this.vkbd.moveCancel(coord);
    }

    onInputEnd(coord: InputEventCoordinate) {
      this.vkbd.release(coord);
    }
  }
}