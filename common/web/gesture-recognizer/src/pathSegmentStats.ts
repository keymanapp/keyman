namespace com.keyman.osk {
  export class PathSegmentStats {
    private static TIME_DIST_WEIGHT = .033; // Effect:  33ms ~= 1px distance.

    private xLinearSum: number = 0;
    private yLinearSum: number = 0;
    private tLinearSum: number = 0;

    private xCentroidSum: number = 0;
    private yCentroidSum: number = 0;

    private xQuadSum: number = 0;
    private yQuadSum: number = 0;
    private tQuadSum: number = 0;

    private xtCrossSum: number = 0;
    private ytCrossSum: number = 0;
    private xyCrossSum: number = 0;

    private coordArcSum: number = 0;
    private spacetimeArcSum: number = 0;

    private speedLinearSum: number = 0;
    private speedQuadSum:   number = 0;

    private cosLinearSum:   number = 0;
    private sinLinearSum:   number = 0;
    private sinCosCrossSum: number = 0;
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

    public lastSample?: InputSample;
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

      result.xLinearSum += x;
      result.yLinearSum += y;
      result.tLinearSum += t;

      result.xtCrossSum += x * t;
      result.ytCrossSum += y * t;
      result.xyCrossSum += x * y;

      result.xQuadSum += x * x;
      result.yQuadSum += y * y;
      result.tQuadSum += t * t;

      if(this.lastSample) {
        // arc length stuff!
        const xDelta = sample.targetX - this.lastSample.targetX;
        const yDelta = sample.targetY - this.lastSample.targetY;
        const tDelta = sample.t       - this.lastSample.t;
        const tDeltaInSec = tDelta / 1000;
        const weightedTDelta = tDelta * PathSegmentStats.TIME_DIST_WEIGHT;

        const coordArcDeltaSq = xDelta * xDelta + yDelta * yDelta;
        const coordArcDelta = Math.sqrt(coordArcDeltaSq);

        result.coordArcSum     += Math.sqrt(coordArcDeltaSq);
        result.spacetimeArcSum += Math.sqrt(coordArcDeltaSq + weightedTDelta * weightedTDelta);

        // Approximates weighting the time spent at each coord by splitting the time since
        // last event evenly for both coordinates.  Note:  does NOT shift based upon .baseSample!
        result.xCentroidSum += 0.5 * tDeltaInSec * (sample.targetX + this.lastSample.targetX);
        result.yCentroidSum += 0.5 * tDeltaInSec * (sample.targetY + this.lastSample.targetY);

        if(xDelta || yDelta) {
          const cos = -yDelta / coordArcDelta;  // alignment with <0, -1> in the DOM
          const sin =  xDelta / coordArcDelta;  // alignment with <1,  0> in the DOM
          result.cosLinearSum   += cos;
          result.sinLinearSum   += sin;

          result.sinCosCrossSum += sin * cos;
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

      result.xLinearSum -= subsetStats.xLinearSum;
      result.yLinearSum -= subsetStats.yLinearSum;
      result.tLinearSum -= subsetStats.tLinearSum;

      result.xtCrossSum -= subsetStats.xtCrossSum;
      result.ytCrossSum -= subsetStats.ytCrossSum;
      result.xyCrossSum -= subsetStats.xyCrossSum;

      result.xQuadSum -= subsetStats.xQuadSum;
      result.yQuadSum -= subsetStats.yQuadSum;
      result.tQuadSum -= subsetStats.tQuadSum;

      // arc length stuff!
      if(subsetStats.followingSample && subsetStats.lastSample) {
        const xDelta = subsetStats.followingSample.targetX - subsetStats.lastSample.targetX;
        const yDelta = subsetStats.followingSample.targetY - subsetStats.lastSample.targetY;
        const tDelta = subsetStats.followingSample.t       - subsetStats.lastSample.t;
        const weightedTDelta = tDelta * PathSegmentStats.TIME_DIST_WEIGHT;
        const tDeltaInSec = tDelta / 1000;

        const coordArcSq = xDelta * xDelta + yDelta * yDelta;

        // Due to how arc length stuff gets segmented.
        // There's the arc length within the prefix subset (operand 2 below) AND the part connecting it to the
        // 'remaining' subset (operand 1 below) before the portion wholly within what remains (the result)
        result.coordArcSum     -= Math.sqrt(coordArcSq);
        result.coordArcSum     -= subsetStats.coordArcSum;
        result.spacetimeArcSum -= Math.sqrt(coordArcSq + weightedTDelta * weightedTDelta);
        result.spacetimeArcSum -= subsetStats.spacetimeArcSum;

        // Centroid sum management!
        // Same reasoning pattern as for the 'arc length stuff'.
        result.xCentroidSum -= 0.5 * tDeltaInSec * (subsetStats.followingSample.targetX + subsetStats.lastSample.targetX)
        result.xCentroidSum -= subsetStats.xCentroidSum;
        result.yCentroidSum -= 0.5 * tDeltaInSec * (subsetStats.followingSample.targetY + subsetStats.lastSample.targetY)
        result.yCentroidSum -= subsetStats.yCentroidSum;

        result.cosLinearSum   -= subsetStats.cosLinearSum;
        result.sinLinearSum   -= subsetStats.sinLinearSum;
        result.sinCosCrossSum -= subsetStats.sinCosCrossSum;
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

    private get xSampleMean() {
      return this.xLinearSum / this.sampleCount;
    }

    private get ySampleMean() {
      return this.yLinearSum / this.sampleCount;
    }

    private get tSampleMean() {
      return this.tLinearSum / this.sampleCount;
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

    public get xtCovariance() {
      return this.xtCrossSum / this.sampleCount - (this.xSampleMean * this.tSampleMean);
    }

    public get ytCovariance() {
      return this.ytCrossSum / this.sampleCount - (this.ySampleMean * this.tSampleMean);
    }

    public get xyCovariance() {
      return this.xyCrossSum / this.sampleCount - (this.xSampleMean * this.ySampleMean);
    }

    public get xVariance() {
      return this.xQuadSum / this.sampleCount - (this.xSampleMean * this.xSampleMean);
    }

    public get yVariance() {
      return this.yQuadSum / this.sampleCount - (this.ySampleMean * this.ySampleMean);
    }

    public get tVariance() {
      return this.tQuadSum / this.sampleCount - (this.tSampleMean * this.tSampleMean);
    }

    public get xtCorrelation() {
      if(this.xVariance == 0) {
        return Number.NaN;
      }

      return this.xtCovariance / (Math.sqrt(this.xVariance * this.tVariance));
    }

    public get ytCorrelation() {
      if(this.yVariance == 0) {
        return Number.NaN;
      }

      return this.ytCovariance / (Math.sqrt(this.yVariance * this.tVariance));
    }

    public get xyCorrelation() {
      if(this.xVariance == 0 || this.yVariance == 0) {
        return Number.NaN;
      }

      return this.xyCovariance / (Math.sqrt(this.xVariance * this.yVariance));
    }

    public get movementRatio() {
      return this.coordArcSum / this.spacetimeArcSum;
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

    public get directnessRatio() {
      return this.directDistance / this.coordArcSum;
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

    public get angleMean() {
      if(this.arcSampleCount == 0) {
        return Number.NaN;
      }
      // Neato reference: https://rosettacode.org/wiki/Averages/Mean_angle
      // But we don't actually need to divide by sample count; `atan2` handles that!
      const sinMean = this.sinLinearSum / this.arcSampleCount;
      const cosMean = this.cosLinearSum / this.arcSampleCount;

      let angle = Math.atan2(sinMean, cosMean);  // result:  on the interval (-pi, pi]
      // Convert to [0, 2*pi).
      if(angle < 0) {
        angle = angle + 2 * Math.PI;
      }

      return angle;
    }

    // Based on https://www.ebi.ac.uk/thornton-srv/software/PROCHECK/nmr_manual/man_cv.html
    public get angleVariance() {
      if(this.arcSampleCount == 0) {
        return Number.NaN;
      }
      const rSquared = this.cosLinearSum * this.cosLinearSum + this.sinLinearSum * this.sinLinearSum;
      return 1 - (rSquared / (this.arcSampleCount * this.arcSampleCount));
    }

    public toJSON() {
      // return {
      //   xtCorrelation:     this.xtCorrelation,
      //   ytCorrelation:     this.ytCorrelation,
      //   xyCorrelation:     this.xyCorrelation,
      //   directnessRatio:   this.directnessRatio,
      //   directDistance:    this.directDistance,
      //   movementRatio:     this.movementRatio,
      //   angle:             this.angle,
      //   speed:             this.speed,
      //   cardinalDirection: this.cardinalDirection,
      //   centroid:          this.centroid,
      //   duration:          this.duration,
      //   // Probably doesn't need to be reported in the long run, but useful while we're still nailing down the math & such.
      //   sampleMean:        {x: this.baseSample.targetX + this.xSampleMean, y: this.baseSample.targetY + this.ySampleMean}
      // };
      return {
        angleMean: this.angleMean,
        angleVariance: this.angleVariance,
        speedMean: this.speedMean,
        speedVariance: this.speedVariance,
        coordArcSum: this.coordArcSum,
        asTheBirdFlies: this.directDistance,
        duration: this.duration,
        sampleCount: this.sampleCount
      }
    }
  }

}