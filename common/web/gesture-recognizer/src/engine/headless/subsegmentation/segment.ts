import { RegressiblePathStats } from "../regressiblePathStats.js";
import { InputSample } from "../inputSample.js";
import { type SegmentClass } from "../segmentClassifier.js";

export interface JSONSegment {
  type: SegmentClass,
  duration: number,
  distance: number,
  speed: number,
  peakSpeed: number
  angle: number,
  cardinalDirection: string,
  initialCoord: InputSample<any>,
  lastCoord: InputSample<any>
}

/**
 * Denotes one "segment" of the ongoing touchpath.  These segments are then utilized as the basis
 * for detecting and synthesizing gestures.
 *
 * By first breaking the touchpath into gestures, we gain the ability to define something of a
 * finite-state-machine (FSM) model for each type of gesture we wish to support.  While technically
 * possible to do even without this step... we find that this makes such a goal far less complex.
 */
export class Segment {
  /**
   * Denotes the highest (mean + 1-sigma) speed seen among all subsegments comprising
   * this segment.
   *
   * This is likely not the maximum speed observed, as that observation is likely an outlier
   * and is not internally recorded.  We rebuild this from stats observations.
   *
   * Think of it as acting like a "smoothed peak speed".
   */
  private _peakSpeed: number;

  private _type?: SegmentClass;

  private _stats: RegressiblePathStats | JSONSegment;

  private _recognitionPromise: Promise<SegmentClass>;
  private _recognitionPromiseResolver: (type: SegmentClass | PromiseLike<SegmentClass>) => void;

  private _resolutionPromise: Promise<void>;
  private _resolutionPromiseResolver: () => void;

  private _isResolved: boolean = false;

  /**
   * Intended for internal-use only; this is utilized during the segmentation process.
   */
  public constructor();
  /**
   * Reconstructs an instance based upon a previously-serialized `.toJSON()` call on this
   * object.
   * @param serializedObj
   */
  public constructor(serializedObj: JSONSegment);
  public constructor(serializedObj?: JSONSegment) {
    if(serializedObj) {
      // We'll use the loaded object as our core reference.
      this._stats = serializedObj;
      this._peakSpeed = serializedObj.peakSpeed; // Is retrieved from `this`, not `this._stats`.
      this._type = serializedObj.type;           // This one too.

      // Ensure that the two promises are properly initialized for this construction type.
      this._recognitionPromise = Promise.resolve(serializedObj.type);
      this._resolutionPromise  = Promise.resolve();
    } else {
      this._peakSpeed = 0;

      this._recognitionPromise = new Promise<SegmentClass>((resolve) => {
        this._recognitionPromiseResolver = resolve;
      });

      this._resolutionPromise = new Promise<void>((resolve) => {
        this._resolutionPromiseResolver = () => {
          this._isResolved = true;
          resolve();
        }
      })
    }
  }

  /**
   * Indicates the likely "highest sustained" speed observed within the `Segment`'s path.
   *
   * To be more precise, the mean + 1-sigma (average + one standard deviation) of the speed
   * for the fastest component of the `Segment`.
   */
  get peakSpeed(): number {
    return this._peakSpeed;
  }

  protected setPeakSpeed(speed: number) {
    if(this._isResolved) {
      throw new Error("May not modify a resolved segment!");
    }
    this._peakSpeed = speed;
  }

  protected updateStats(totalStats: RegressiblePathStats) {
    if(this._isResolved) {
      throw new Error("May not modify a resolved segment!");
    }
    this._stats = totalStats;
  }

  protected classifyType(type: SegmentClass) {
    if(!this._stats) {
      throw new Error("Cannot recognize the segment - lacking critical metadata");
    }

    if(this._isResolved) {
      throw new Error("May not modify a resolved segment!");
    }

    if(this._type === undefined) {
      this._type = type;

      this._recognitionPromiseResolver(type);
    } else if(this._type != type) {
      throw new Error("May not change segment type once set!");
    }
  }

  /**
   * The starting point of this touchpath segment.
   */
  get initialCoord(): InputSample<any> {
    if(this._stats instanceof RegressiblePathStats) {
      return this._stats.initialSample;
    } else {
      return this._stats.initialCoord;
    }
  };

  /**
   * The ending point of this touchpath segment.
   */
  get lastCoord(): InputSample<any> {
    if(this._stats instanceof RegressiblePathStats) {
      return this._stats.lastSample;
    } else {
      return this._stats.lastCoord;
    }
  };

  /**
   * The duration of this touchpath segment, measured in ms.
   */
  get duration(): number {
    return this._stats.duration;
  }

  /**
   * The net distance traveled by this touchpath segment.  In other words,
   * the distance between `initialCoord` and `lastCoord` (ignoring time).
   */
  get distance(): number {
    if(this._stats instanceof RegressiblePathStats) {
      return this._stats.netDistance;
    } else {
      return this._stats.distance;
    }
  }

  /**
   * The average speed of this touchpath segment - distance / duration.
   */
  get speed(): number {
    return this._stats.speed;
  };

  /**
   * The angle traveled by this touchpath segment.  Measured in radians.
   */
  get angle(): number {
    return this._stats.angle;
  };

  /**
   * The 'directional bucket' for the detected angle.  This will
   * correspond to a cardinal or an intercardinal ('n', 'ne', etc.)
   */
  get direction(): string {
    return this._stats.cardinalDirection;
  };

  /**
   * The classification of this Segment:  'start', 'hold', 'move', or 'end'.
   *
   * May be null if the Segment has not yet been `recognized`.
   *
   * @see `SegmentClass`
   */
  get type(): SegmentClass {
    return this._type || null;
  }

  protected _isRecognized(): boolean {
    return this._type !== undefined;
  }

  /**
   * A `Promise` that resolves when the segment's classification is determined
   * - when we "recognize" its role in the touchpath.
   */
  get whenRecognized(): Promise<SegmentClass> {
    return this._recognitionPromise;
  }

  /**
   * A `Promise` that resolves when the segment and its role within the touchpath
   * are fully determined, as segmentation has noted the need for a new boundary -
   * and thus, distinct and different behavior after the endpoint of this segment
   * of the touchpath.
   */
  get whenResolved(): Promise<void> {
    return this._resolutionPromise;
  }

  protected resolve() {
    if(!this._stats) {
      throw new Error("Cannot resolve the segment - illegal state!");
    }
    this._resolutionPromiseResolver();
  }

  /**
   * Provides a human-readable yet serialization-friendly version of this instance.
   * @returns
   */
  public toJSON(): JSONSegment {
    const cleanSample: (sample: InputSample<any>) => InputSample<any> = (sample) => {
      const clone = {... sample};
      delete clone.clientX;
      delete clone.clientY;

      return clone;
    }

    return {
      type: this.type,
      duration: this.duration,
      cardinalDirection: this.direction,
      speed: this.speed,
      distance: this.distance,
      angle: this.angle,
      peakSpeed: this.peakSpeed,
      initialCoord: cleanSample(this.initialCoord),
      lastCoord: cleanSample(this.lastCoord)
    };
  }
}

/**
 * Provides an editable typing for Segment utilized during the segmentation process;
 * casting to the superclass provides an uneditable view into the Segment instead.
 *
 * This is intended for internal use only.
 */
export class SegmentImplementation extends Segment {
  constructor() {
    super();
  }

  public setPeakSpeed(speed: number) {
    super.setPeakSpeed(speed);
  }

  public updateStats(stats: RegressiblePathStats) {
    super.updateStats(stats);
  }

  public classifyType(type: SegmentClass) {
    super.classifyType(type);
  }

  public resolve() {
    super.resolve();
  }

  public get isRecognized() {
    return super._isRecognized();
  }
}