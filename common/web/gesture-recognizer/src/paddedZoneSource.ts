namespace com.keyman.osk {

  export class PaddedZoneSource implements RecognitionZoneSource {
    private readonly root: RecognitionZoneSource;

    /**
     * [0]: x (left)
     * [1]: y (top)
     * [2]: width (left+right)
     * [3]: height (top+bottom)
     */
    private edgePadding: number[];

    // Positive values 'shrink' the new zone compared to the old zone, while negative ones
    // 'expand' it instead.
    constructor(rootZoneSource: RecognitionZoneSource, edgePadding?: number|number[]) {
      this.root = rootZoneSource;
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
            const xVal = edgePadding[0];
            const yVal = edgePadding[1];
            this.edgePadding = [xVal, yVal, 2 * xVal, 2 * yVal];
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
      const rootZone = this.root.getBoundingClientRect();

      return DOMRect.fromRect({
        x: rootZone.x + this.edgePadding[0],
        y: rootZone.y + this.edgePadding[1],
        width: rootZone.width   - this.edgePadding[2],
        height: rootZone.height - this.edgePadding[3]
      });
    }
  }
}