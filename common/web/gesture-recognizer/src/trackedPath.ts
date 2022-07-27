/// <reference path="inputSample.ts" />

namespace com.keyman.osk {
  export type JSONTrackedPath = {
    coords: InputSample[]; // ensures type match with public class property.
    wasCancelled?: boolean;
  }

  interface EventMap {
    'step': (sample: InputSample) => void,
    'complete': () => void,
    'invalidated': () => void
    // 'segmentation': (endingSegment: Segment, openingSegment: Segment) => void
  }

  export class TrackedPath extends EventEmitter<EventMap> {
    private samples: InputSample[] = [];
    private _isComplete: boolean = false;
    private wasCancelled?: boolean;

    constructor();
    /**
     * Deserializes a TrackedPath instance from its corresponding JSON.parse() object.
     * @param jsonObj
     */
    constructor(jsonObj: JSONTrackedPath)
    constructor(jsonObj?: JSONTrackedPath) {
      super();

      if(jsonObj) {
        this.samples = [...jsonObj.coords.map((obj) => ({...obj} as InputSample))];
        // If we're reconstructing this from a JSON.parse, it's a previously-recorded, completed path.
        this._isComplete = true;
        this.wasCancelled = jsonObj.wasCancelled;
      }
    }

    public get isComplete() {
      return this._isComplete;
    }

    extend(sample: InputSample) {
      if(this._isComplete) {
        throw "Invalid state:  this TrackedPath has already terminated.";
      }

      this.samples.push(sample);
      this.emit('step', sample);
    }

    terminate(cancel: boolean = false) {
      if(this._isComplete) {
        throw "Invalid state:  this TrackedPath has already terminated.";
      }
      this.wasCancelled = cancel;
      this._isComplete = true;

      if(cancel) {
        this.emit('invalidated');
      } else {
        this.emit('complete');
      }
      this.removeAllListeners();
    }

    public get coords(): readonly InputSample[] {
      return this.samples;
    }

    toJSON() {
      let jsonClone: JSONTrackedPath = {
        // Replicate array and its entries, but with certain fields of each entry missing.
        // No .clientX, no .clientY.
        coords: [...this.samples.map((obj) => ({
          targetX: obj.targetX,
          targetY: obj.targetY,
          t:       obj.t
        }))],
        wasCancelled: this.wasCancelled
      }

      // Removes components of each sample that we don't want serialized.
      for(let sample of jsonClone.coords) {
        delete sample.clientX;
        delete sample.clientY;
      }

      return jsonClone;
    }
  }

}