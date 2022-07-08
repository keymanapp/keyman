/// <reference path="inputSample.ts" />
namespace com.keyman.osk {
  type JSONInputSequence = Omit<Omit<InputSequence, '_currentTarget'>, '_identifier'>;

  export class InputSequence {
    public readonly isFromTouch: boolean;
    public readonly identifier: number;

    private _currentTarget: EventTarget;

    private _sequence: InputSampleSequence = [];

    constructor(identifier: number,
                parsedObj: JSONInputSequence);
    constructor(identifier: number,
                initialTarget: EventTarget,
                isFromTouch: boolean);
    constructor(identifier: number,
                obj: EventTarget | JSONInputSequence,
                isFromTouch?: boolean) {
      this.identifier = identifier;
      if(obj instanceof EventTarget) {
        this._currentTarget = obj;
        this.isFromTouch = isFromTouch;
      } else {
        Object.assign(this, obj);
      }
    }

    public get currentTarget(): EventTarget {
      return this._currentTarget;
    }

    addSample(sample: InputSample) {
      this._sequence.push(sample);
    }

    // For use with JSON.stringify.
    static _replacer(key: string, value: any) {
      if(key == "_currentTarget") {
        return undefined;
      } else if(key == "identifier") {
        return undefined;
      } else {
        return value;
      }
    }
  }
}