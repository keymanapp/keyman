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

    }

    onInputMove(coord: InputEventCoordinate) {

    }

    onInputEnd(coord: InputEventCoordinate) {

    }

    onInputCancel(coord: InputEventCoordinate) {

    }
  }
}