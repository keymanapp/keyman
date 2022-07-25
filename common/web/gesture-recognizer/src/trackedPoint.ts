/// <reference path="inputSample.ts" />

namespace com.keyman.osk {
  type JSONTrackedPoint = {
    isFromTouch: boolean;
    samples: TrackedPoint['path']; // ensures type match with public class property.
  }

  export class TrackedPoint {
    public readonly isFromTouch: boolean;
    public readonly rawIdentifier: number;

    private _initialTarget: EventTarget;

    private samples: InputSampleSequence = [];

    public get path(): readonly InputSample[] {
      return this.samples;
    }

    constructor(identifier: number,
                parsedObj: JSONTrackedPoint);
    constructor(identifier: number,
                initialTarget: EventTarget,
                isFromTouch: boolean);
    constructor(identifier: number,
                obj: EventTarget | JSONTrackedPoint,
                isFromTouch?: boolean) {
      this.rawIdentifier = identifier;
      if(obj instanceof EventTarget) {
        this._initialTarget = obj;
        this.isFromTouch = isFromTouch;
      } else {
        // @ts-ignore
        this.isFromTouch = obj.isFromTouch;
        // @ts-ignore
        this.samples = [...obj.samples.map((obj) => ({...obj} as InputSample))];
      }
    }

    public get initialTarget(): EventTarget {
      return this._initialTarget;
    }

    public get identifier(): string {
      const prefix = this.isFromTouch ? 'touch' : 'mouse';
      return `${prefix}:${this.rawIdentifier}`;
    }

    addSample(sample: InputSample) {
      this.samples.push(sample);
    }

    toJSON(): JSONTrackedPoint {
      let jsonClone: JSONTrackedPoint = {
        isFromTouch: this.isFromTouch,
        samples: [...this.samples.map((obj) => ({...obj} as InputSample))]
      }

      // Removes components of each sample that we don't want serialized.
      for(let sample of jsonClone.samples) {
        delete sample.clientX;
        delete sample.clientY;
      }

      return jsonClone;
    }
  }
}