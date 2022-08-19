namespace com.keyman.osk {
  /**
   * As the name suggests, this class exists to track cumulative mathematical values, etc
   * necessary to provide statistical information.  This information is used to facilitate
   * path segmentation.
   *
   * Instances of this class are immutable.
   */
  export class CumulativePathStats {
    // So... class-level "inner classes" are possible in TS... if defined via assignment to a field.
    static readonly regression = class RegressionFromSums {
      readonly independent: 'x' | 'y' | 't';
      readonly dependent:   'x' | 'y' | 't';
      readonly paired:      'tx' | 'ty' | 'xy';

      readonly accumulator: CumulativePathStats;

      constructor(mainStats: CumulativePathStats, dependentAxis: 'x' | 'y' | 't', independentAxis: 'x' | 'y' | 't') {
        if(dependentAxis == independentAxis) {
          throw "Two different axes must be specified for the regression object.";
        }

        this.accumulator = mainStats;

        this.dependent   = dependentAxis;
        this.independent = independentAxis;

        if(dependentAxis < independentAxis) {
          this.paired = dependentAxis.concat(independentAxis) as 'tx' | 'ty' | 'xy';
        } else {
          this.paired = independentAxis.concat(dependentAxis) as 'tx' | 'ty' | 'xy';
        }
      }

      get slope(): number {
        // The technical definition is the commented-out line, but the denominator component of both
        // cancels out - it's 'more efficient' to use the following line as a result.

        // this.accumulator.covariance(this.paired) / this.accumulator.variance(this.independent);
        const val = this.accumulator.crossSum(this.paired) / this.accumulator.squaredSum(this.independent);
        return val;
      }

      get intercept(): number {
        // Performing a regression based on these pre-summed values means that our obtained intercept is in
        // the mapped coordinate system.
        const mappedIntercept = this.accumulator.mappedMean(this.dependent) - this.slope * this.accumulator.mappedMean(this.independent);

        const val =  mappedIntercept + this.accumulator.mappingConstant(this.dependent) -
          this.slope * this.accumulator.mappingConstant(this.independent);

        return val;
      }

      get sumOfSquaredError(): number {
        return this.accumulator.squaredSum(this.dependent) - this.sumOfSquaredModeled;
      }

      get sumOfSquaredModeled(): number {
        return this.slope * this.accumulator.crossSum(this.paired);
      }

      get coefficientOfDetermination(): number {
        if(this.accumulator.squaredSum(this.dependent) == 0 || this.accumulator.squaredSum(this.independent) == 0) {
          return 1;
        }

        const acc = this.accumulator;
        const num = acc.crossSum(this.paired) * acc.crossSum(this.paired);
        const denom = acc.squaredSum(this.dependent) * acc.squaredSum(this.independent);

        return num / denom;
      }

      predictFromValue(value: number) {
        return this.slope * value + this.intercept;
      }
    }

    private rawLinearSums: {'x': number, 'y': number, 't': number} = {'x': 0, 'y': 0, 't': 0};

    private xCentroidSum: number = 0;
    private yCentroidSum: number = 0;

    private rawSquaredSums: {'x': number, 'y': number, 't': number} = {'x': 0, 'y': 0, 't': 0};

    private rawCrossSums: {'tx': number, 'ty': number, 'xy': number} = {'tx': 0, 'ty': 0, 'xy': 0};

    private coordArcSum: number = 0;

    private speedLinearSum: number = 0;
    private speedQuadSum:   number = 0;

    private cosLinearSum:   number = 0;
    private sinLinearSum:   number = 0;
    private arcSampleCount: number = 0;

    /**
     * The base sample used to transpose all other received samples.  Use of this helps
     * avoid potential "catastrophic cancellation" effects that can occur when diffing two
     * numbers far from the sample-space's mathematical origin.
     *
     * Refer to https://en.wikipedia.org/wiki/Catastrophic_cancellation.
     */
    private baseSample?: InputSample;

    /**
     * The initial sample included by this instance's computed stats.  Needed for
     * the 'directness' properties.
     */
    private initialSample?: InputSample;

    /*private*/ lastSample?: InputSample;
    private followingSample?: InputSample;
    private sampleCount = 0;

    constructor();
    constructor(sample: InputSample);
    constructor(instance: CumulativePathStats);
    constructor(obj?: InputSample | CumulativePathStats) {
      if(!obj) {
        return;
      }

      // Will worry about JSON form later.
      if(obj instanceof CumulativePathStats) {
        Object.assign(this, obj);

        this.rawLinearSums = {...obj.rawLinearSums};
        this.rawCrossSums  = {...obj.rawCrossSums};
        this.rawSquaredSums   = {...obj.rawSquaredSums};
      } else if(isAnInputSample(obj)) {
        Object.assign(this, this.extend(obj));
      }
    }

    /**
     * Statistically "observes" a new sample point on the touchpath, accumulating values
     * useful for provision of relevant statistical properties.
     * @param sample A newly-sampled point on the touchpath.
     * @returns A new, separate instance for the cumulative properties up to the
     *          newly-sampled point.
     */
    public extend(sample: InputSample): CumulativePathStats {
      if(!this.initialSample) {
        this.initialSample = sample;
        this.baseSample = sample;
      }
      const result = new CumulativePathStats(this);

      // Set _after_ deep-copying this for the result.
      this.followingSample = sample;

      // Helps prevent "catastrophic cancellation" issues from floating-point computation
      // for these statistical properties and properties based upon them.
      const x = sample.targetX - this.baseSample.targetX;
      const y = sample.targetY - this.baseSample.targetY;
      const t = sample.t - this.baseSample.t;

      result.rawLinearSums['x'] += x;
      result.rawLinearSums['y'] += y;
      result.rawLinearSums['t'] += t;

      result.rawCrossSums['tx'] += t * x;
      result.rawCrossSums['ty'] += t * y;
      result.rawCrossSums['xy'] += x * y;

      result.rawSquaredSums['x'] += x * x;
      result.rawSquaredSums['y'] += y * y;
      result.rawSquaredSums['t'] += t * t;

      if(this.lastSample) {
        // arc length stuff!
        const xDelta = sample.targetX - this.lastSample.targetX;
        const yDelta = sample.targetY - this.lastSample.targetY;
        const tDelta = sample.t       - this.lastSample.t;
        const tDeltaInSec = tDelta / 1000;

        const coordArcDeltaSq = xDelta * xDelta + yDelta * yDelta;
        const coordArcDelta = Math.sqrt(coordArcDeltaSq);

        result.coordArcSum     += Math.sqrt(coordArcDeltaSq);

        // Approximates weighting the time spent at each coord by splitting the time since
        // last event evenly for both coordinates.  Note:  does NOT shift based upon .baseSample!
        result.xCentroidSum += 0.5 * tDeltaInSec * (sample.targetX + this.lastSample.targetX);
        result.yCentroidSum += 0.5 * tDeltaInSec * (sample.targetY + this.lastSample.targetY);

        if(xDelta || yDelta) {
          // We wish to measure angle clockwise from <0, -1> in the DOM.  So, cos values should
          // align with that axis, while sin values should align with the positive x-axis.
          //
          // This provides a mathematical 'transformation' to the axes used by `atan2` in the
          // `angleMean` property.
          result.cosLinearSum   += -yDelta / coordArcDelta;
          result.sinLinearSum   +=  xDelta / coordArcDelta;
          result.arcSampleCount += 1;
        }

        if(tDeltaInSec) {
          result.speedLinearSum += Math.sqrt(coordArcDeltaSq) / tDeltaInSec;
          result.speedQuadSum   += coordArcDeltaSq / (tDeltaInSec * tDeltaInSec);
        }
      }

      result.lastSample = sample;
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
    public deaccumulate(subsetStats?: CumulativePathStats): CumulativePathStats {
      // Possible TODO:  Because of the properties of statistical variance & mean...
      // we could further prevent catastrophic cancellation by re-centering
      // all the linear, cross, and quad sums.
      // - mostly noteworthy for _long_ duration touches that wander long distances.
      // - basically, for cases that'd cause the floating-point error to exceed our
      //   test thresholds.  Re-centering would keep that error consistently below
      //   our thresholds.
      //
      // We could then take the new mean coordinates as a 'base sample'.
      // Kinda has to be the new mean b/c of the stats identities we'd be abusing,
      // but that's also the best catastrophic-cancellation prevention move we
      // could take.  So, this limitation's not really a negative.
      const result = new CumulativePathStats(this);

      // We actually WILL accept a `null` argument; makes some of the segmentation
      // logic simpler.
      if(!subsetStats) {
        return result;
      }

      if(!subsetStats.followingSample || !subsetStats.lastSample) {
        throw 'Invalid argument:  stats missing necessary tracking variable.';
      }

      for(let dim in result.rawLinearSums) {
        result.rawLinearSums[dim] -= subsetStats.rawLinearSums[dim];
      }

      for(let dimPair in result.rawCrossSums) {
        result.rawCrossSums[dimPair] -= subsetStats.rawCrossSums[dimPair];
      }

      for(let dim in result.rawSquaredSums) {
        result.rawSquaredSums[dim] -= subsetStats.rawSquaredSums[dim];
      }

      // arc length stuff!
      if(subsetStats.followingSample && subsetStats.lastSample) {
        const xDelta = subsetStats.followingSample.targetX - subsetStats.lastSample.targetX;
        const yDelta = subsetStats.followingSample.targetY - subsetStats.lastSample.targetY;
        const tDelta = subsetStats.followingSample.t       - subsetStats.lastSample.t;
        const tDeltaInSec = tDelta / 1000;

        const coordArcSq = xDelta * xDelta + yDelta * yDelta;

        // Due to how arc length stuff gets segmented.
        // There's the arc length within the prefix subset (operand 2 below) AND the part connecting it to the
        // 'remaining' subset (operand 1 below) before the portion wholly within what remains (the result)
        result.coordArcSum     -= Math.sqrt(coordArcSq);
        result.coordArcSum     -= subsetStats.coordArcSum;

        // Centroid sum management!
        // Same reasoning pattern as for the 'arc length stuff'.
        result.xCentroidSum -= 0.5 * tDeltaInSec * (subsetStats.followingSample.targetX + subsetStats.lastSample.targetX)
        result.xCentroidSum -= subsetStats.xCentroidSum;
        result.yCentroidSum -= 0.5 * tDeltaInSec * (subsetStats.followingSample.targetY + subsetStats.lastSample.targetY)
        result.yCentroidSum -= subsetStats.yCentroidSum;

        result.cosLinearSum   -= subsetStats.cosLinearSum;
        result.sinLinearSum   -= subsetStats.sinLinearSum;
        result.arcSampleCount -= subsetStats.arcSampleCount;

        result.speedLinearSum -= subsetStats.speedLinearSum;
        result.speedQuadSum   -= subsetStats.speedQuadSum;

        if(tDeltaInSec) {
          result.speedLinearSum -= Math.sqrt(coordArcSq) / tDeltaInSec;
          result.speedQuadSum   -= coordArcSq / (tDeltaInSec * tDeltaInSec);
        }
      }

      result.sampleCount -= subsetStats.sampleCount;

      // NOTE: baseSample MUST REMAIN THE SAME.  All math is based on the corresponding diff.
      // Though... very long touchpoint interactions could start being affected by that "catastrophic
      // cancellation" effect without further adjustment.  (If it matters, we'll get to that later.)
      // But _probably_ not; we don't go far beyond a couple of orders of magnitude from the origin in
      // ANY case except the timestamp (.t) - and even then, not far from the baseSample's timestamp value.

      // initialSample, though, we need to update b/c of the 'directness' properties.
      result.initialSample = subsetStats.followingSample;

      return result;
    }

    public get lastTimestamp(): number {
      return this.lastSample?.t;
    }

    public get count() {
      return this.sampleCount;
    }

    private mappingConstant(dim: 'x' | 'y' | 't') {
      if(!this.baseSample) {
        return undefined;
      }

      if(dim == 't') {
        return this.baseSample.t;
      } else if(dim == 'x') {
        return this.baseSample.targetX;
      } else {
        return this.baseSample.targetY;
      }
    }

    private mappedMean(dim: 'x' | 'y' | 't') {
      return this.rawLinearSums[dim] / this.sampleCount;
    }

    public get centroid(): {x: number, y: number} {
      if(this.sampleCount == 0) {
        return undefined;
      } else if(this.sampleCount == 1) {
        return {
          x: this.lastSample.targetX,
          y: this.lastSample.targetY
        };
      } else {
        const coeff = 1 / (this.duration); // * (this.sampleCount-1));
        return {
          x: this.xCentroidSum * coeff,
          y: this.yCentroidSum * coeff
        };
      }
    }

    public squaredSum(dim: 'x' | 'y' | 't') {
      const x2 = this.rawSquaredSums[dim];
      const x1 = this.rawLinearSums[dim];

      const val = x2 - x1 * x1 / this.sampleCount;

      return val > 1e-8 ? val : 0;
    }

    public crossSum(dimPair: 'tx' | 'ty' | 'xy') {
      const dim1 = dimPair.charAt(0);
      const dim2 = dimPair.charAt(1);

      let orderedDims: string = dimPair;
      if(dim2 < dim1) {
        orderedDims = dim2.concat(dim1);
      }

      const ab = this.rawCrossSums[orderedDims];
      const a  = this.rawLinearSums[dim1];
      const b  = this.rawLinearSums[dim2];

      const val = ab - a * b / this.sampleCount;

      return val > 1e-8 ? val : 0;
    }

    public covariance(dimPair: 'tx' | 'ty' | 'xy') {
      return this.crossSum(dimPair) / (this.sampleCount - 1);
    }

    public variance(dim: 'x' | 'y' | 't') {
      return this.squaredSum(dim) / (this.sampleCount - 1);
    }

    public buildRenormalized(): CumulativePathStats {
      let result = new CumulativePathStats(this);

      // By abusing the statistical identities for calculating various expressions
      // related to regression, we can re-center our mapped coordinate system on
      // our current mean.  We shouldn't do so too frequently, but this should help
      // moderate effects from catastrophic cancellation.

      let newBase: InputSample = {
        targetX: this.mappedMean['x'] + this.baseSample.targetX,
        targetY: this.mappedMean['y'] + this.baseSample.targetY,
        t:       this.mappedMean['t'] + this.baseSample.t
      };

      result.baseSample = newBase;

      for(const dimPair in result.rawCrossSums) {
        result.rawCrossSums[dimPair] = this.crossSum(dimPair as 'tx' | 'ty' | 'xy');
      }

      for(const dim in result.rawSquaredSums) {
        result.rawSquaredSums[dim] = this.squaredSum(dim as 'x' | 'y' | 't');
      }

      return result;
    }

    public fitRegression(dependent: 'x' | 'y' | 't', independent: 'x' | 'y' | 't') {
      return new CumulativePathStats.regression(this, dependent, independent);
    }

    public get netDistance() {
      // No issue with a net distance of 0 due to a single point.
      if(!this.lastSample || !this.initialSample) {
        return Number.NaN;
      }

      const xDelta = this.lastSample.targetX - this.initialSample.targetX;
      const yDelta = this.lastSample.targetY - this.initialSample.targetY;

      return Math.sqrt(xDelta * xDelta + yDelta * yDelta);
    }

    public get duration() {
      // no issue with a duration of zero from just one sample.
      if(!this.lastSample || !this.initialSample) {
        return Number.NaN;
      }
      return (this.lastSample.t - this.initialSample.t) * 0.001;
    }

    /**
     * Returns the angle (in radians) traveled by the corresponding segment clockwise
     * from the unit vector <0, -1> in the DOM (the unit "upward" direction).
     */
    public get angle() {
      if(this.sampleCount == 1 || !this.lastSample || !this.initialSample) {
        return Number.NaN;
      } else if(this.netDistance < 1) {
        // < 1 px, thus sub-pixel, means we have nothing relevant enough to base an angle on.
        return Number.NaN;
      }

      const xDelta = this.lastSample.targetX - this.initialSample.targetX;
      const yDelta = this.lastSample.targetY - this.initialSample.targetY;
      const yAngleDiff = Math.acos(-yDelta / this.netDistance);

      return xDelta < 0 ? (2 * Math.PI - yAngleDiff) : yAngleDiff;
    }

    public get angleInDegrees() {
      return this.angle * 180 / Math.PI;
    }

    public get cardinalDirection() {
      if(this.sampleCount == 1 || !this.lastSample || !this.initialSample) {
        return undefined;
      }

      const angle = this.angleInDegrees;
      const buckets = ['n', 'ne', 'e', 'se', 's', 'sw', 'w', 'nw'];

      for(let threshold = 22.5, bucketIndex = 0; threshold < 360; threshold += 45, bucketIndex += 1) {
        if(angle < threshold) {
          return buckets[bucketIndex];
        }
      }

      return 'n';
    }

    // px per s.
    public get speed() {
      // this.duration is already in seconds, not milliseconds.
      return this.duration ? this.netDistance / this.duration : Number.NaN;
    }

    // ... may not be "right".
    public get speedMean() {
      return this.speedLinearSum / (this.sampleCount-1);
    }

    public get speedVariance() {
      return this.speedQuadSum / (this.sampleCount-1) - (this.speedMean * this.speedMean);
    }

    /**
     * Returns the represented interval's 'mean angle' clockwise from the DOM's
     * <0, -1> (the unit vector toward the top of the screen) in radians.
     *
     * Uses the 'circular mean'.  Refer to https://en.wikipedia.org/wiki/Circular_mean.
     */
    public get angleMean() {
      if(this.arcSampleCount == 0) {
        return Number.NaN;
      }

      // Neato reference: https://rosettacode.org/wiki/Averages/Mean_angle
      // But we don't actually need to divide by sample count; `atan2` handles that!
      const sinMean = this.sinLinearSum;
      const cosMean = this.cosLinearSum;

      let angle = Math.atan2(sinMean, cosMean);  // result:  on the interval (-pi, pi]
      // Convert to [0, 2*pi).
      if(angle < 0) {
        angle = angle + 2 * Math.PI;
      }

      return angle;
    }

    /**
     * Provides the rSquared value needed internally for circular-statistic properties.
     *
     * Range:  floating-point values on the interval [0, 1].
     */
    private get angleRSquared() {
      // https://www.ebi.ac.uk/thornton-srv/software/PROCHECK/nmr_manual/man_cv.html may be a useful
      // reference for this tidbit.  The Wikipedia article's more dense... not that this link isn't
      // a bit dense itself.
      const rSquaredBase = this.cosLinearSum * this.cosLinearSum + this.sinLinearSum * this.sinLinearSum;
      return rSquaredBase / (this.arcSampleCount * this.arcSampleCount);
    }

    /**
     * The **circular variance** of the represented interval's angle observations.
     *
     * Refer to https://en.wikipedia.org/wiki/Directional_statistics#Variance.
     */
    public get angleVariance() {
      if(this.arcSampleCount == 0) {
        return Number.NaN;
      }
      return 1 - (this.angleRSquared);
    }

    /**
     * The **circular standard deviation** of the represented interval's angle observations.
     *
     * Refer to https://en.wikipedia.org/wiki/Directional_statistics#Standard_deviation.
     */
    public get angleDeviation() {
      if(this.arcSampleCount == 0) {
        return Number.NaN;
      }

      return Math.sqrt(-Math.log(this.angleRSquared));
    }

    public get rawDistance() {
      return this.coordArcSum;
    }

    // TODO:  is this actually ideal?  This was certainly useful for experimentation via interactive
    // debugging, but it may not be the best thing long-term.
    public toJSON() {
      return {
        angleMean: this.angleMean,
        angleMeanDegrees: this.angleMean * 180 / Math.PI,
        angleVariance: this.angleVariance,
        speedMean: this.speedMean,
        speedVariance: this.speedVariance,
        rawDistance: this.rawDistance,
        duration: this.duration,
        sampleCount: this.sampleCount
      }
    }
  }

}