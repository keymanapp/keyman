/// <reference path="recognitionZoneSource.ts" />

namespace com.keyman.osk {
  export class ViewportZoneSource implements RecognitionZoneSource {

    /**
     * [0]: y (top)
     * [1]: x (left)
     * [2]: height (top+bottom)
     * [3]: width (left+right)
     */
    readonly edgePadding: number[];

    constructor(edgePadding?: number|number[]) {
      // In case it isn't yet defined.
      edgePadding = edgePadding || 0;

      if(typeof edgePadding == 'number') {
        this.edgePadding = [edgePadding, edgePadding, 2 * edgePadding, 2 * edgePadding];
        return;
      } else {
        // Modeled after CSS styling definitions... just with preprocessed numbers, not strings.
        switch(edgePadding.length) {
          case 1:
            const val = edgePadding[0];
            this.edgePadding = [val, val, 2 * val, 2 * val];
            break;
          case 2:
            const yVal = edgePadding[0];
            const xVal = edgePadding[1];
            this.edgePadding = [yVal, xVal, 2 * yVal, 2 * xVal];
            break;
          case 4:
            this.edgePadding = [edgePadding[0],
                                edgePadding[1],
                                edgePadding[0] + edgePadding[2],
                                edgePadding[1] + edgePadding[3]];
            break;
          default:
            throw new Error("Invalid parameter - must be an array of type `number` with length 1, 2, or 4.");
        }
      }
    }

    getBoundingClientRect(): DOMRect {
      // Viewport dimension detection is based on https://stackoverflow.com/a/8876069.
      const vw = Math.max(document.documentElement.clientWidth || 0, window.innerWidth || 0);
      const vh = Math.max(document.documentElement.clientHeight || 0, window.innerHeight || 0);

      return DOMRect.fromRect({
        y: this.edgePadding[0],
        x: this.edgePadding[1],
        height: vh - this.edgePadding[2],
        width:  vw - this.edgePadding[3]
      });
    }
  }
}