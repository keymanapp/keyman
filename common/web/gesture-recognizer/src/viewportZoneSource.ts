/// <reference path="recognitionZoneSource.ts" />

namespace com.keyman.osk {
  export class ViewportZoneSource implements RecognitionZoneSource {
    readonly edgePadding: number[];

    constructor(edgePadding?: number|number[]) {
      // In case it isn't yet defined.
      edgePadding = edgePadding || 0;

      if(typeof edgePadding == 'number') {
        this.edgePadding = [edgePadding, edgePadding, edgePadding, edgePadding];
        return;
      } else {
        // Modeled after CSS styling definitions... just with preprocessed numbers, not strings.
        switch(edgePadding.length) {
          case 1:
            const val = edgePadding[0];
            this.edgePadding = [val, val, val, val];
            break;
          case 2:
            const xVal = edgePadding[0];
            const yVal = edgePadding[1];
            this.edgePadding = [xVal, yVal, xVal, yVal];
            break;
          case 4:
            this.edgePadding = [... edgePadding]; // shallow copy.
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
        x: this.edgePadding[0],
        y: this.edgePadding[1],
        width:  vw - this.edgePadding[0] - this.edgePadding[2],
        height: vh - this.edgePadding[1] - this.edgePadding[3]
      });
    }
  }
}