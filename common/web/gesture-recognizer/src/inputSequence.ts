/// <reference path="inputSample.ts" />
namespace com.keyman.osk {
  export class InputSequence {
    public readonly isFromTouch: boolean;
    public readonly identifier: number;

    private _currentTarget: EventTarget;

    private _sequence: InputSampleSequence = [];

    constructor(identifier: number,
                initialTarget: EventTarget,
                isFromTouch: boolean) {
      this.identifier = identifier;
      this._currentTarget = initialTarget;
      this.isFromTouch = isFromTouch;
    }

    public get currentTarget(): EventTarget {
      return this._currentTarget;
    }

    addSample(sample: InputSample) {
      this._sequence.push(sample);
    }
  }
}