/// <reference path="inputEventCoordinate.ts" />

namespace com.keyman.osk {
  export abstract class InputEventEngine {
    protected readonly vkbd: VisualKeyboard;

    public constructor(vkbd: VisualKeyboard) {
      this.vkbd = vkbd;
    }

    protected get eventRoot(): HTMLDivElement {
      return this.vkbd.layerGroup.element;
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

    // onInputEndCancel(coord: InputEventCoordinate) {
    //   // TODO:
    // }
  }
}