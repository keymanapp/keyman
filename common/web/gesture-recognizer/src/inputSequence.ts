/// <reference path="inputSample.ts" />
namespace com.keyman.osk {
  type JSONInputSequence = Omit<Omit<InputSequence, '_currentTarget'>, '_identifier'>;

  export class InputSequence {
    public readonly isFromTouch: boolean;
    public readonly identifier: number;

    private _currentTarget: EventTarget;

    private samples: InputSampleSequence = [];

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

    public get fullIdentifier(): string {
      const prefix = this.isFromTouch ? 'touch' : 'mouse';
      return `${prefix}:${this.identifier}`;
    }

    addSample(sample: InputSample) {
      this.samples.push(sample);
    }

    // For use with JSON.stringify.
    static _replacer(key: string, value: any) {
      if(key == "_currentTarget") { // No point in trying to save an HTMLElement to JSON.
        return undefined;
      } else if(key == "identifier") { // Only matters when receiving input, not when replaying it.
        return undefined;
      } else if(key == "clientX" || key == "clientY") { // We can reconstruct them from the `target[X|Y]` values.
        return undefined;
      } else {
        return value;
      }
    }
  }
}