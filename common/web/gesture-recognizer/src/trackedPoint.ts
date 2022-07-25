/// <reference path="inputSample.ts" />

namespace com.keyman.osk {
  type JSONTrackedPoint = {
    isFromTouch: boolean;
    samples: TrackedPoint['path']; // ensures type match with public class property.
  }

  /**
   * Represents one 'tracked point' involved in a potential / recognized gesture as tracked over time.
   * This 'tracked point' corresponds to one touch source as recognized by `Touch.identifier` or to
   * one 'cursor-point' as represented by mouse-based motion.
   */
  export class TrackedPoint {
    /**
     * Indicates whether or not this tracked point's original source is a DOM `Touch`.
     */
    public readonly isFromTouch: boolean;

    /**
     * The numeric form of this point's identifier as seen in events (or as emulated for mouse events)
     */
    public readonly rawIdentifier: number;

    private _initialTarget: EventTarget;

    private samples: InputSampleSequence = [];

    /**
     * Tracks the coordinates and timestamps of each update for the lifetime of this `TrackedPoint`.
     */
    public get path(): readonly InputSample[] {
      return this.samples;
    }

    /**
     * Constructs a new TrackedPoint instance for tracking updates to an active input point over time.
     * @param identifier     The system identifier for the input point's events.
     * @param initialTarget  The initiating event's original target element
     * @param isFromTouch    `true` if sourced from a `TouchEvent`; `false` otherwise.
     */
    constructor(identifier: number,
                initialTarget: EventTarget,
                isFromTouch: boolean);
    /**
     * Deserializes a TrackedPoint instance from its serialized-JSON form.
     * @param identifier The unique identifier to assign to this instance.
     * @param parsedObj  The JSON representation to deserialize.
     */
    constructor(identifier: number,
      parsedObj: JSONTrackedPoint);
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

    /**
     * Gets a fully-unique string-based identifier, even for edge cases where both mouse and touch input
     * are received simultaneously.
     */
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
        // Replicate array and its entries, but with certain fields of each entry missing.
        // No .clientX, no .clientY.
        samples: [...this.samples.map((obj) => ({
          targetX: obj.targetX,
          targetY: obj.targetY,
          t:       obj.t
        }))]
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