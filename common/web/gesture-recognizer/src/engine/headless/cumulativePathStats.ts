import { InputSample, isAnInputSample } from "./inputSample.js";

/**
 * Denotes one dimension utilized by touchpath input coordinates - 'x' and y' for space,
 * 't' for time.
 */
export type PathCoordAxis = 'x' | 'y' | 't';

/**
 * Denotes a pair of dimensions utilized by touchpath input coordinates.  The two axes
 * (see `PathCoordAxis`) must be specified in alphabetical order.
 */
export type PathCoordAxisPair = 'tx' | 'ty' | 'xy';

/**
 * Denotes one dimension or feature (velocity) that this class tracks statistics for.
 *
 * Sine and Cosine stats are currently excluded due to their necessary lack of statistical
 * independence.
 */
type StatAxis = PathCoordAxis;

/**
 * As the name suggests, this class facilitates tracking of cumulative mathematical values, etc
 * useful for interpretation of a contact point's path as it relates to gestures.
 *
 * Instances of this class may be considered immutable externally.
 *
 * A subclass with properties useful for path segmentation: `RegressiblePathStats`.
 */
export class CumulativePathStats<Type = any> {
  protected rawLinearSums  = {'x': 0, 'y': 0, 't': 0};

  // Handles raw-distance stuff.
  private coordArcSum: number = 0;

  /**
   * The base sample used to transpose all other received samples.  Use of this helps
   * avoid potential "catastrophic cancellation" effects that can occur when diffing
   * two numbers far from the sample-space's mathematical origin.
   *
   * Refer to https://en.wikipedia.org/wiki/Catastrophic_cancellation.
   */
  protected baseSample?: InputSample<Type>;

  /**
   * The initial sample included by this instance's computed stats.  Needed for
   * the 'directness' properties.
   */
  private _initialSample?: InputSample<Type>;

  private _lastSample?: InputSample<Type>;
  protected followingSample?: InputSample<Type>;
  private _sampleCount = 0;

  constructor();
  constructor(sample: InputSample<Type>);
  constructor(instance: CumulativePathStats<Type>);
  constructor(obj?: InputSample<Type> | CumulativePathStats<Type>)
  constructor(obj?: InputSample<Type> | CumulativePathStats<Type>) {
    if(!obj) {
      return;
    }

    // Will worry about JSON form later.
    if(obj instanceof CumulativePathStats) {
      Object.assign(this, obj);

      this.rawLinearSums = {...obj.rawLinearSums};
    } else if(isAnInputSample(obj)) {
      Object.assign(this, this.extend(obj));
      /* c8 ignore next 3 */
    } else {
      throw new Error("A constructor for this input pattern has not yet been implemented");
    }
  }

  /**
   * Statistically "observes" a new sample point on the touchpath, accumulating values
   * useful for provision of relevant statistical properties.
   * @param sample A newly-sampled point on the touchpath.
   * @returns A new, separate instance for the cumulative properties up to the
   *          newly-sampled point.
   */
  public extend(sample: InputSample<any>): CumulativePathStats<Type> {
    return this._extend(new CumulativePathStats(this), sample);
  }

  // Pattern exists to facilitate subclasses if needed in the future:  see #11079 and #11080.
  protected _extend(result: CumulativePathStats<Type>, sample: InputSample<any>) {
    if(!result._initialSample) {
      result._initialSample = sample;
      result.baseSample = sample;
    }

    const baseSample = result.baseSample;

    // Set _after_ deep-copying this for the result.
    this.followingSample = sample;

    // Helps prevent "catastrophic cancellation" issues from floating-point computation
    // for these statistical properties and properties based upon them.
    const x = sample.targetX - baseSample.targetX;
    const y = sample.targetY - baseSample.targetY;
    const t = sample.t - baseSample.t;

    result.rawLinearSums.x += x;
    result.rawLinearSums.y += y;
    result.rawLinearSums.t += t;

    if(this.lastSample) {
      // arc length stuff!
      const xDelta = sample.targetX - this.lastSample.targetX;
      const yDelta = sample.targetY - this.lastSample.targetY;

      const coordArcDeltaSq = xDelta * xDelta + yDelta * yDelta;
      const coordArcDelta = Math.sqrt(coordArcDeltaSq);

      result.coordArcSum     += coordArcDelta;
    }

    result._lastSample = sample;
    result.sampleCount = this.sampleCount + 1;

    return result;
  }

  /**
   * "De-accumulates" currently-accumulated values corresponding to the specified
   * subset, which should represent an earlier, previously-observed part of the path.
   * @param subsetStats The accumulated stats for the part of the path being removed
   *                    from this instance's current accumulation.
   * @returns
   */
  public deaccumulate(subsetStats?: CumulativePathStats<Type>): CumulativePathStats<Type> {
    const result = new CumulativePathStats(this);

    return this._deaccumulate(result, subsetStats);
  }

  protected _deaccumulate(result: CumulativePathStats<Type>, subsetStats?: CumulativePathStats<Type>): CumulativePathStats<Type> {
    // Possible addition:  use `this.buildRenormalized` on the returned version
    // if catastrophic cancellation effects (random, small floating point errors)
    // are not sufficiently mitigated & handled by the measures currently in place.
    //
    // Even then, we'd need to apply such generated objects carefully - we can't
    // re-merge the accumulated values or remap them to their old coordinate system
    // afterward.`buildRenormalize`'s remapping maneuver is a one-way stats-abuse trick.
    //
    // Hint: we'd need to pay attention to the "lingering segments" aspects in which
    // detected sub-segments might be "re-merged".
    // - Whenever they're merged & cleared, we should be clear to recentralize
    //   the cumulative stats that follow.  If any are still active, we can't
    //   recentralize.

    // We actually WILL accept a `null` argument; makes some of the segmentation
    // logic simpler.
    if(!subsetStats) {
      return result;
    }

    /* c8 ignore next 3 */
    if(!subsetStats.followingSample || !subsetStats.lastSample) {
      throw 'Invalid argument:  stats missing necessary tracking variable.';
    }

    for(let dim in result.rawLinearSums) {
      // TS refuses to infer beyond 'string' in a `let... in` construct; we can't
      // even assert it directly on `dim` via declaring it early!
      const d = dim as PathCoordAxis;
      result.rawLinearSums[d] -= subsetStats.rawLinearSums[d];
    }

    // arc length stuff!
    if(subsetStats.followingSample && subsetStats.lastSample) {
      const xDelta = subsetStats.followingSample.targetX - subsetStats.lastSample.targetX;
      const yDelta = subsetStats.followingSample.targetY - subsetStats.lastSample.targetY;

      const coordArcDeltaSq = xDelta * xDelta + yDelta * yDelta;
      const coordArcDelta = Math.sqrt(coordArcDeltaSq);

      // Due to how arc length stuff gets segmented.
      // There's the arc length within the prefix subset (operand 2 below) AND the part connecting it to the
      // 'remaining' subset (operand 1 below) before the portion wholly within what remains (the result)
      result.coordArcSum     -= coordArcDelta;
      result.coordArcSum     -= subsetStats.coordArcSum;
    }

    result.sampleCount -= subsetStats.sampleCount;

    // NOTE: baseSample MUST REMAIN THE SAME.  All math is based on the corresponding diff.
    // Though... very long touchpoint interactions could start being affected by that "catastrophic
    // cancellation" effect without further adjustment.  (If it matters, we'll get to that later.)
    // But _probably_ not; we don't go far beyond a couple of orders of magnitude from the origin in
    // ANY case except the timestamp (.t) - and even then, not far from the baseSample's timestamp value.

    // initialSample, though, we need to update b/c of the 'directness' properties.
    result._initialSample = subsetStats.followingSample;

    return result;
  }

  public translateCoordSystem(functor: (sample: InputSample<Type>) => InputSample<Type>): CumulativePathStats<Type> {
    const result = new CumulativePathStats(this);

    return this._translateCoordSystem(result, functor);
  }

  protected _translateCoordSystem(result: CumulativePathStats<Type>, functor: (sample: InputSample<Type>) => InputSample<Type>): CumulativePathStats<Type> {
    if(this.sampleCount == 0) {
      return result;
    }

    const singleSample = result.initialSample == result.lastSample;

    result._initialSample = functor(result.initialSample);
    result.baseSample = functor(result.baseSample);
    result._lastSample = singleSample ? result._initialSample : functor(result.lastSample);

    return result;
  }

  public replaceInitialSample(sample: InputSample<Type>): CumulativePathStats<Type> {
    let result = new CumulativePathStats(this);

    return this._replaceInitialSample(result, sample);
  }

  protected _replaceInitialSample(result: CumulativePathStats<Type>, sample: InputSample<Type>) {
    // if stats length == 0 or length == 1, is ezpz.  Could 'shortcut' things here.
    if(this.sampleCount == 0) {
      // Note:  if this error actually causes problems, 'silently failing' the call
      // by insta-returning should be "fine" as far as actual gesture processing goes.
      throw new Error("no sample available to replace");
      // return;
    }

    // Re: the block above... obviously, don't replace if there IS no initial sample yet.
    // It'll happen soon enough anyway.
    const originalSample = result.initialSample;
    result._initialSample = sample;

    if(this.sampleCount > 1) {
      // Works fine re: cata-cancellation - `this.baseSample.___` cancels out.
      const xDelta = sample.targetX - originalSample.targetX;
      const yDelta = sample.targetY - originalSample.targetY;
      const tDelta = sample.t       - originalSample.t;

      result.rawLinearSums.x += xDelta;
      result.rawLinearSums.y += yDelta;
      result.rawLinearSums.t += tDelta;

      /*
       * `rawDistance` tracking.  Note:  this is kind of an approximation, as
       * we aren't getting the true distance between the new first and the original
       * second point.  But... it should be "good enough".
       *
       * If need be, we could always track "second sample" to be more precise about things
       * here, though that would add a bit more logic overhead at low sample counts.
       * (Note the logic interactions inherent in firstSample, secondSample, and lastSample.)
       *
       * This concern should be a low-priority detail for now - at the time of writing,
       * rawDistance is currently only used by KeymanWeb for longpress up-flick thresholding,
       * and that codepath doesn't do path-start rewriting.
       */
      const coordArcDeltaSq = xDelta * xDelta + yDelta * yDelta;
      const coordArcDelta = Math.sqrt(coordArcDeltaSq);

      result.coordArcSum     += coordArcDelta;
    } else {
      result._lastSample = sample;
    }

    // Do NOT change sampleCount; we're replacing the original.
    return result;
  }

  public get lastSample() {
    return this._lastSample;
  }

  public get lastTimestamp(): number {
    return this.lastSample?.t;
  }

  public get sampleCount() {
    return this._sampleCount;
  }

  private set sampleCount(value: number) {
    this._sampleCount = value;
  }

  public get initialSample() {
    return this._initialSample;
  }

  /**
   * In order to mitigate the accumulation of small floating-point errors during the
   * various accumulations performed by this class, the domain of incoming values
   * is remapped near to the origin via axis-specific mapping constants.
   * @param dim
   * @returns
   */
  protected mappingConstant(dim: StatAxis) {
    if(!this.baseSample) {
      return undefined;
    }

    if(dim == 't') {
      return this.baseSample.t;
    } else if(dim == 'x') {
      return this.baseSample.targetX;
    } else if(dim == 'y') {
      return this.baseSample.targetY;
    } else {
      return 0;
    }
  }

  /**
   * Gets the statistical mean value of the samples observed during the represented
   * interval on the specified axis.
   * @param dim
   * @returns
   */
  public mean(dim: StatAxis) {
    // This external-facing version needs to provide values in 'external'-friendly
    // coordinate space.
    return this.rawLinearSums[dim] / this.sampleCount + this.mappingConstant(dim);
  }

  /**
   * Provides the direct Euclidean distance between the start and end points of the segment
   * (or curve) of the interval represented by this instance.
   *
   * This will likely not match the actual pixel distance traveled.
   */
  public get netDistance() {
    // No issue with a net distance of 0 due to a single point.
    if(!this.lastSample || !this.initialSample) {
      return 0;
    }

    const xDelta = this.lastSample.targetX - this.initialSample.targetX;
    const yDelta = this.lastSample.targetY - this.initialSample.targetY;

    return Math.sqrt(xDelta * xDelta + yDelta * yDelta);
  }

  /**
   * Gets the duration of the represented interval in milliseconds.
   */
  public get duration() {
    // no issue with a duration of zero from just one sample.
    if(!this.lastSample || !this.initialSample) {
      return 0;
    }
    return (this.lastSample.t - this.initialSample.t);
  }

  /**
   * Returns the angle (in radians) traveled by the corresponding segment clockwise
   * from the unit vector <0, -1> in the DOM (the unit "upward" direction).
   */
  public get angle() {
    if(this.sampleCount == 1 || !this.lastSample || !this.initialSample) {
      return undefined;
    } else if(this.netDistance < 1) {
      // < 1 px, thus sub-pixel, means we have nothing relevant enough to base an angle on.
      return undefined;
    }

    const xDelta = this.lastSample.targetX - this.initialSample.targetX;
    const yDelta = this.lastSample.targetY - this.initialSample.targetY;
    const yAngleDiff = Math.acos(-yDelta / this.netDistance);

    return xDelta < 0 ? (2 * Math.PI - yAngleDiff) : yAngleDiff;
  }

  /**
   * Returns the angle (in degrees) traveled by the corresponding segment clockwise
   * from the unit vector <0, -1> in the DOM (the unit "upward" direction).
   */
  public get angleInDegrees() {
    return this.angle * 180 / Math.PI;
  }

  /**
   * Returns the cardinal or intercardinal direction on the screen that most
   * closely matches the direction of movement represented by the represented
   * segment.
   *
   * @return A string one or two letters in length (e.g:  'n', 'sw'), or
             `undefined` if not enough data to determine a direction.
    */
  public get cardinalDirection() {
    if(this.sampleCount == 1 || !this.lastSample || !this.initialSample) {
      return undefined;
    }

    if(isNaN(this.angle) || this.angle === null || this.angle === undefined) {
      return undefined;
    }

    const buckets = ['n', 'ne', 'e', 'se', 's', 'sw', 'w', 'nw', 'n'];

    // We could be 'more efficient' and use radians here instead, but this
    // version helps a bit more with easy maintainability.
    const bucketIndex = Math.ceil((this.angleInDegrees - 22.5)/45);
    return buckets[bucketIndex];
  }

  /**
   * Measured in pixels per second.
   * @return a speed in pixels per millisecond.  May be 0 if no movement was observed
   * among the samples.
   */
  public get speed() {
    return this.duration ? this.netDistance / this.duration : 0;
  }

  /**
   * Provides the actual, pixel-based distance actually traveled by the represented segment.
   * May not be an integer (because diagonals are a thing).
   */
  public get rawDistance() {
    return this.coordArcSum;
  }

  /* c8 ignore start */
  /**
   * Provides a JSON.stringify()-friendly object with the properties most useful for
   * debugger-based inspection and/or console-logging statements.
   */
  public toJSON() {
    return {
      angle: this.angle,
      cardinal: this.cardinalDirection,
      netDistance: this.netDistance,
      duration: this.duration,
      sampleCount: this.sampleCount,
      rawDistance: this.rawDistance
    }
  }
  /* c8 ignore end */
}