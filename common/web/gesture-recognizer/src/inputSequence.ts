/// <reference path="inputSample.ts" />
namespace com.keyman.osk {
  type JSONInputSequence = Omit<Omit<InputSequence, '_currentTarget'>, '_identifier'>;

  export class InputSequence {
    public readonly isFromTouch: boolean;
    public readonly identifier: number;

    private _currentTarget: EventTarget;

    public readonly samples: InputSampleSequence = [];

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
        this.samples = [...obj.samples.map((obj) => ({...obj} as InputSample))];
      }
    }

    public get currentTarget(): EventTarget {
      return this._currentTarget;
    }

    addSample(sample: InputSample) {
      this.samples.push(sample);
    }

    toJSON() {
      let jsonClone = new InputSequence(this.identifier, this) as any;

      // Remove components that shouldn't be serialized.
      delete jsonClone._currentTarget;
      delete jsonClone.identifier;

      for(let sample of jsonClone.samples) {
        delete sample.clientX;
        delete sample.clientY;
      }

      return jsonClone as JSONInputSequence;
    }
  }
}