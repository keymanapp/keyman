/// <reference path="inputSample.ts" />
namespace com.keyman.osk {
  export class InputSequence {
    public readonly isFromTouch: boolean;
    public readonly identifier: number;

    private _currentTarget: EventTarget;
    private _isEditable: boolean = true;

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

    public get isEditable(): boolean {
      return this._isEditable;
    }

    public disableEditing() {
      this._isEditable = false;
    }

    addSample(sample: InputSample) {
      if(this.isEditable) {
        this._sequence.push(sample);
      }
    }
  }
}