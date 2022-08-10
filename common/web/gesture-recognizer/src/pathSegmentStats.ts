namespace com.keyman.osk {
  export class PathSegmentStats {
    private xCentroidSum: number = 0;
    private yCentroidSum: number = 0;

    private coordArcSum: number = 0;

    private speedLinearSum: number = 0;
    private speedQuadSum:   number = 0;

    private cosLinearSum:   number = 0;
    private sinLinearSum:   number = 0;
    private cosQuadSum:     number = 0;
    private sinQuadSum:     number = 0;
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

    private lastSample?: InputSample;
    private followingSample?: InputSample;
    private sampleCount = 0;

    constructor();
    constructor(sample: InputSample);
    constructor(instance: PathSegmentStats);
    constructor(obj?: InputSample | PathSegmentStats) {
      if(!obj) {
        return;
      }

      // Will worry about JSON form later.
      if(obj instanceof PathSegmentStats) {
        Object.assign(this, obj);
      } else if(isAnInputSample(obj)) {
        Object.assign(this, this.unionWith(obj));
      }
    }

    public unionWith(sample: InputSample): PathSegmentStats {
      if(!this.initialSample) {
        this.initialSample = sample;
        this.baseSample = sample;
      } else {
        this.followingSample = sample;
      }
      const result = new PathSegmentStats(this);

      // Helps prevent "catastrophic cancellation" issues from floating-point computation
      // for these statistical properties and properties based upon them.
      const x = sample.targetX - this.baseSample.targetX;
      const y = sample.targetY - this.baseSample.targetY;
      const t = sample.t - this.baseSample.t;

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
          const cos = -yDelta / coordArcDelta;  // alignment with <0, -1> in the DOM
          const sin =  xDelta / coordArcDelta;  // alignment with <1,  0> in the DOM
          result.cosLinearSum   += cos;
          result.sinLinearSum   += sin;

          result.cosQuadSum     += cos * cos;
          result.sinQuadSum     += sin * sin;

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

    public withoutPrefixSubset(subsetStats: PathSegmentStats): PathSegmentStats {
      const result = new PathSegmentStats(this);

      if(!subsetStats.followingSample || !subsetStats.lastSample) {
        throw 'Invalid argument:  stats missing necessary tracking variable.';
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
        result.sinQuadSum     -= subsetStats.sinQuadSum;
        result.cosQuadSum     -= subsetStats.cosQuadSum;

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

    public get directDistance() {
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
      } else if(this.directDistance < 1) {
        // < 1 px, thus sub-pixel, means we have nothing relevant enough to base an angle on.
        return Number.NaN;
      }

      const xDelta = this.lastSample.targetX - this.initialSample.targetX;
      const yDelta = this.lastSample.targetY - this.initialSample.targetY;
      const yAngleDiff = Math.acos(-yDelta / this.directDistance);

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
      return this.duration ? this.directDistance / this.duration : Number.NaN;
    }

    // ... may not be "right".
    public get speedMean() {
      return this.speedLinearSum / (this.sampleCount-1);
    }

    public get speedVariance() {
      return this.speedQuadSum / (this.sampleCount-1) - (this.speedMean * this.speedMean);
    }

    public get sinVariance() {
      const sinMean = this.sinLinearSum / this.arcSampleCount;
      return this.sinQuadSum / (this.arcSampleCount) - sinMean * sinMean;
    }

    public get cosVariance() {
      const cosMean = this.cosLinearSum / this.arcSampleCount;
      return this.cosQuadSum / this.arcSampleCount - cosMean * cosMean;
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
     * The **circular variance** of the represented interval's angle observations.
     *
     * Refer to https://en.wikipedia.org/wiki/Directional_statistics#Variance.
     */
    public get angleVariance() {
      if(this.arcSampleCount == 0) {
        return Number.NaN;
      }
      // https://www.ebi.ac.uk/thornton-srv/software/PROCHECK/nmr_manual/man_cv.html may be a useful
      // reference for this tidbit.  The Wikipedia article's more dense... not that this link isn't
      // a bit dense itself.
      const rSquared = this.cosLinearSum * this.cosLinearSum + this.sinLinearSum * this.sinLinearSum;
      return 1 - (rSquared / (this.arcSampleCount * this.arcSampleCount));
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

      // Excludes the divisor; we can add that in the following line.
      // https://www.ebi.ac.uk/thornton-srv/software/PROCHECK/nmr_manual/man_cv.html may be a useful
      // reference for this as well.
      const rSquared = this.cosLinearSum * this.cosLinearSum + this.sinLinearSum * this.sinLinearSum;
      return Math.sqrt(Math.log(this.arcSampleCount * this.arcSampleCount / rSquared));
    }

    public toJSON() {
      return {
        angleMean: this.angleMean,
        angleVariance: this.angleVariance,
        angleDeviation: this.angleDeviation,
        speedMean: this.speedMean,
        speedVariance: this.speedVariance,
        coordArcSum: this.coordArcSum,
        duration: this.duration,
        sampleCount: this.sampleCount
      }
    }
  }

}