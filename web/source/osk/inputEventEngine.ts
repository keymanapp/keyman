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

    protected detectWithinBounds(coord: InputEventCoordinate): boolean {
      // Determine the important geometric values involved
      const _Box = this.vkbd.element.offsetParent as HTMLElement;
      let oskX = this.vkbd.element.offsetLeft + (_Box?.offsetLeft || 0);
      let oskY = this.vkbd.element.offsetTop  + (_Box?.offsetTop || 0);

      // If the OSK is using fixed positioning (thus, viewport-relative), we need to
      // convert the 'clientX'-like values into 'pageX'-like values.
      if(this.vkbd.element.style.position == 'fixed' || _Box?.style.position == 'fixed') {
        oskX += window.pageXOffset;
        oskY += window.pageYOffset;
      }

      const width = this.vkbd.width;
      const height = this.vkbd.height;

      // Determine the out-of-bounds threshold at which touch-cancellation should automatically occur.
      // Assuming square key-squares, we'll use 1/3 the height of a row for bounds detection
      // for both dimensions.
      const rowCount = this.vkbd.currentLayer.rows.length;
      const buffer = (0.333 * this.vkbd.height / rowCount);

      // ... and begin!

      if(coord.x < oskX - buffer || coord.x > oskX + width + buffer) {
        return false;
      } else if(coord.y < oskY - buffer || coord.y > oskY + height + buffer) {
        return false;
      } else {
        return true;
      }
    }

    abstract registerEventHandlers();
    abstract unregisterEventHandlers();

    onInputStart(coord: InputEventCoordinate) {
      this.vkbd.touch(coord);
    }

    onInputMove(coord: InputEventCoordinate) {
      this.vkbd.moveOver(coord);
    }

    onInputEnd(coord: InputEventCoordinate) {
      this.vkbd.release(coord);
    }

    onInputCancel(coord: InputEventCoordinate) {
      this.vkbd.cancel(coord);
    }
  }
}