namespace com.keyman.osk {

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

    private _stats: CumulativePathStats;

    private _recognitionPromise: Promise<SegmentClass>;
    private _recognitionPromiseResolver: (type: SegmentClass | PromiseLike<SegmentClass>) => void;

    private _resolutionPromise: Promise<void>;
    private _resolutionPromiseResolver: () => void;

    public constructor() {
      this._peakSpeed = 0;

      this._recognitionPromise = new Promise<SegmentClass>((resolve) => {
        this._recognitionPromiseResolver = resolve;
      });

      this._resolutionPromise = new Promise<void>((resolve) => {
        this._resolutionPromiseResolver = resolve;
      })
    }

    get peakSpeed(): number {
      return this._peakSpeed;
    }

    protected setPeakSpeed(speed: number) {
      this._peakSpeed = speed;
    }

    protected updateStats(totalStats: CumulativePathStats) {
      this._stats = totalStats;
    }

    protected classifyType(type: SegmentClass) {
      if(!this._stats) {
        throw "Implementation error";
      }

      if(this._type === undefined) {
        this._type = type;

        this._recognitionPromiseResolver(type);
      } else if(this._type != type) {
        throw "May not change segment type once set!";
      }
    }

    get initialCoord(): InputSample {
      return this._stats.initialSample;
    };

    get lastCoord(): InputSample {
      return this._stats.lastSample;
    };

    get duration(): number {
      return this._stats.duration;
    }

    get speed(): number {
      return this._stats.speed;
    };

    get angle(): number {
      return this._stats.angle;
    };

    get direction(): string {
      return this._stats.cardinalDirection;
    };

    get type(): SegmentClass {
      return this._type;
    }

    get recognitionPromise(): Promise<SegmentClass> {
      return this._recognitionPromise;
    }

    get resolutionPromise(): Promise<void> {
      return this._resolutionPromise;
    }

    protected resolve() {
      if(!this._stats) {
        throw "Implementation error";
      }
      this._resolutionPromiseResolver();
    }
  }

  /**
   * Provides an editable typing for Segment; casting to the superclass provides an
   * uneditable version instead.
   */
  export class SegmentImplementation extends Segment {
    constructor() {
      super();
    }

    public setPeakSpeed(speed: number) {
      super.setPeakSpeed(speed);
    }

    public updateStats(stats: CumulativePathStats) {
      super.updateStats(stats);
    }

    public classifyType(type: SegmentClass) {
      super.classifyType(type);
    }

    public resolve() {
      super.resolve();
    }
  }
}