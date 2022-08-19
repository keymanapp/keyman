/// <reference path="cumulativePathStats.ts" />

namespace com.keyman.osk {

  // https://en.wikipedia.org/wiki/F-distribution
  // Mostly used here to compare the sum-squared error components of segmented regressions
  // to the sum-squared "modeled" components of their overall variance.  Those are the two
  // "independent random variables" we're examining.  Variances (which are sums of squared
  // values themselves) tend to be chi-squared distributed, fulfilling the conditions.
  //
  // We do NOT want to be computing this thing at run-time.  (Just take one look at the
  // equations found in that Wikipedia article!)  Fortunately, it's very common in stats
  // circles to just... use a lookup table for finding p-values for things following
  // the f-distribution.
  //
  // For future non-stats-inclined maintainers:
  // One could say that the p-value = the chance that what we have just "randomly" happened
  // to occur without our expectations actually being met.  It's a super-common
  // stats term; they'd then say that if the p-value is sufficiently low, then we\
  // "reject the null hypothesis" - the theory that it actually DID happen randomly - in
  // favor of the high likelihood that we really are onto something.
  class FDistribution {

    /**
     * Indexing: [threshold-index][num-2][denom-1].
     *
     * If numerator has only 1 degree of freedom, that means we don't really have
     * segments so much as different averages between the two segments - there's
     * no slope on either side.  In other words, "just a bump".  No segmentation
     * there.
     *
     * 2 degrees of freedom:  no movement on the axis on only one side.
     *
     * threshold-index:
     * - p-value of .100:  0
     * - p-value of .050:  1
     *
     * The f-statistic value must match or exceed its corresponding entry in
     * the table below, based on the detected DoF (degrees of freedom) for the
     * 'numerator' and 'denominator' components.
     */
    private static readonly table = [
      // p = .100
      [
        // numerator: 2
        [
          // denom: 1-10
          49.50, 9.00, 5.46, 4.32, 3.78, 3.46, 3.26, 3.11, 3.01, 2.92,
          // denom: 11-20
           2.86, 2.81, 2.76, 2.73, 2.70, 2.67, 2.64, 2.62, 2.61, 2.59
          // we COULD continue with threshold-dropping after that,
          // but I doubt we'll need it in practicality.
          // At 30:     2.49
          // limit -> infinity:  2.30
        ],
        // numerator: 3
        [
          // denom: 1-10
          53.59, 9.16, 5.39, 4.19, 3.62, 3.29, 3.07, 2.92, 2.81, 2.73,
          // denom: 11-20
           2.66, 2.61, 2.56, 2.52, 2.49, 2.46, 2.44, 2.42, 2.40, 2.38
          // Likewise, from above.
          // At 30: 2.28
          // limit -> infinity: 2.08
        ]
      ],
      // p = .050
      [
        // numerator: 2
        [
          // denom: 1-10
          199.5, 19.0, 9.55, 6.94, 5.79, 5.14, 4.74, 4.46, 4.26, 4.10,
          // denom: 10-20
           3.98, 3.89, 3.81, 3.74, 3.68, 3.63, 3.59, 3.55, 3.52, 3.49
          // At 30: 3.32
          // limit -> infinity: 3.00
        ],
        // numerator: 3
        [
          // denom: 1-10
          215.7,19.16, 9.28, 6.59, 5.41, 4.76, 4.35, 4.07, 3.86, 3.71,
          // denom: 10-20
           3.59, 3.49, 3.41, 3.34, 3.29, 3.24, 3.20, 3.16, 3.13, 3.10
          // At 30: 2.92
          // limit -> infinity: 2.60
        ]
      ]
    ]

    /**
     * Determines a threshold tier for segmentation based on the segmented
     * regression f-statistic.
     * @param statistic
     * @param numDoF
     * @param denomDoF
     *
     * Tier 0:  don't segment
     * Tier 1:  segment if the other axis also says to segment
     *   - p-value < 0.100 on the tested axis
     * Tier 2:  segment regardless of what the other axis says
     *   - p-value < 0.050 on the tested axis
     */
    static thresholdTier(statistic: number, numDoF: number, denomDoF: number) {
      // Former case:  we'd never segment anyway
      // Latter case:  it's currently wrong to statistically test.
      //               The F-distribution is not defined for this case.
      if(numDoF < 2 || denomDoF < 1) {
        return 1;
      }

      if(numDoF > 3) {
        numDoF = 3;
      }

      const numIndex   = (numDoF > 3 ? 3 : numDoF) - 2;
      const denomIndex = (denomDoF > 20 ? 20 : denomDoF) - 1;

      const tier2Threshold = FDistribution.table[1][numIndex][denomIndex];
      if(statistic >= tier2Threshold) {
        return 0.05;
      }

      const tier1Threshold = FDistribution.table[0][numIndex][denomIndex];
      // If arising purely randomly isn't at least less than 10% likely,
      // we'll categorically say it happened randomly with full certainty (1).
      // Obviously not actually true, but it works for our thresholding.
      return statistic >= tier1Threshold ? 0.10 : 1;
    }
  }

  class Segmentation {
    public static readonly SPLIT_CRITERION_THRESHOLD = 1.5;

    readonly pre:   CumulativePathStats;
    readonly post:  CumulativePathStats;
    readonly union: CumulativePathStats;

    static readonly segmentationComparison = class SegmentedRegression {
      host: Segmentation;
      readonly independent: 'x' | 'y' | 't';
      readonly dependent:   'x' | 'y' | 't';
      readonly paired:      'tx' | 'ty' | 'xy';

      pre:   typeof CumulativePathStats.regression.prototype;
      post:  typeof CumulativePathStats.regression.prototype;
      union: typeof CumulativePathStats.regression.prototype;

      constructor(host: Segmentation, dependentAxis: 'x' | 'y' | 't', independentAxis: 'x' | 'y' | 't') {
        if(dependentAxis == independentAxis) {
          throw "Two different axes must be specified for the regression object.";
        }

        this.host = host;

        this.dependent   = dependentAxis;
        this.independent = independentAxis;

        if(dependentAxis < independentAxis) {
          this.paired = dependentAxis.concat(independentAxis) as 'tx' | 'ty' | 'xy';
        } else {
          this.paired = independentAxis.concat(dependentAxis) as 'tx' | 'ty' | 'xy';
        }

        this.pre   = this.host.pre  .fitRegression(dependentAxis, independentAxis);
        this.post  = this.host.post .fitRegression(dependentAxis, independentAxis);
        this.union = this.host.union.fitRegression(dependentAxis, independentAxis);
      }

      get remainingSumSquaredError(): number {
        return this.pre.sumOfSquaredError + this.post.sumOfSquaredError;
      }

      private get splitPoint() {
        let splitPoint = this.host.pre.lastSample;

        let indep = splitPoint.t;
        if(this.independent == 'x') {
          indep = splitPoint.targetX;
        } else if(this.independent == 'y') {
          indep = splitPoint.targetY;
        }

        let dep = splitPoint.t;
        if(this.dependent == 'x') {
          dep = splitPoint.targetX;
        } else if(this.dependent == 'y') {
          dep = splitPoint.targetY;
        }

        return {
          dep: dep,
          indep: indep
        };
      }

      get unsegmentedSumSquaredError(): number {
        // What was our remaining error for the unsegmented regression?
        let baseSSE = this.union.sumOfSquaredError;

        // We double-count the split point when segmenting, so we should add an
        // extra copy of its residual.
        const splitPoint = this.splitPoint;
        const splitPointError = this.splitPoint.dep - this.union.predictFromValue(splitPoint.indep);

        // This is the total sum-squared error to be handled by segmenting.
        return baseSSE + splitPointError * splitPointError;
      }

      get sumSquaredGainFromSegmentation(): number {
        return this.unsegmentedSumSquaredError - this.remainingSumSquaredError;
      }

      get coefficientOfDetermination(): number {
        if(this.host.union.squaredSum(this.dependent) == 0 || this.host.union.squaredSum(this.independent) == 0) {
          return 1;
        }

        return 1 - this.remainingSumSquaredError / this.host.union.squaredSum(this.dependent);
      }

      get fStat(): number {
        const val = this.sumSquaredGainFromSegmentation / this.remainingSumSquaredError;

        // We're fine with Infinity.  Just... not so much NaN.
        return isNaN(val) ? 0 : val;
      }

      get fDoF1(): number {
        let numDoF = 3;
        // Cases where there clearly may as well be 'no slope' at all.
        // This saves us a degree of freedom, which is VERY useful for segmenting
        // the boundary between a 'hold' and a 'move'.
        if(this.host.pre.squaredSum(this.dependent) < 1e-8) {
          numDoF--;
        }
        if(this.host.post.squaredSum(this.dependent) < 1e-8) {
          numDoF--;
        }

        return numDoF;
      }

      get fDoF2(): number {
        return this.host.union.count - 2 - this.fDoF1;
      }

      get certaintyThreshold() {
        return 1 - FDistribution.thresholdTier(this.fStat, this.fDoF1, this.fDoF2);
      }
    }

    constructor(pre: CumulativePathStats, post: CumulativePathStats, union: CumulativePathStats) {
      this.pre = pre;
      this.post = post;
      this.union = union;
    }

    public segReg(dependentAxis: 'x' | 'y' | 't', independentAxis: 'x' | 'y' | 't') {
      return new Segmentation.segmentationComparison(this, dependentAxis, independentAxis);
    }

    get segmentationMerited(): boolean {
      let totalThreshold = 0;
      const xTest = new Segmentation.segmentationComparison(this, 'x', 't');
      const yTest = new Segmentation.segmentationComparison(this, 'y', 't');

      // const xTestConfig = this.xtFTestConfiguration;
      // const yTestConfig = this.ytFTestConfiguration;

      totalThreshold += xTest.certaintyThreshold >= 0.95 ? 2 : (xTest.certaintyThreshold >= 0.90 ? 1 : 0) ;
      totalThreshold += yTest.certaintyThreshold >= 0.95 ? 2 : (yTest.certaintyThreshold >= 0.90 ? 1 : 0) ;

      return totalThreshold >= 2;
    }

    get mergeMerited(): boolean {
      // Because of caret-like motions, we need to text for regression on both axes.
      // I think?  Or does it really make any sort of difference?
      const xTest = new Segmentation.segmentationComparison(this, 'x', 'y');
      const yTest = new Segmentation.segmentationComparison(this, 'y', 'x');
      // const xTestConfig = this.xyFTestConfiguration;
      // const yTestConfig = this.yxFTestConfiguration;

      // If we don't get a p-value less than .100, then as far as x & y are concerned - and thus the user
      // is concerned - it's the same segment.  Speed may be different, but not the direction.
      if(xTest.certaintyThreshold > 0) {
        return false;
      }

      return yTest.certaintyThreshold == 0;
    }
  }

  class PotentialSegmentation extends Segmentation {
    readonly chopPoint: CumulativePathStats;
    readonly endOfPre:  CumulativePathStats;
    readonly baseChop:  CumulativePathStats;

    constructor(steppedStats: CumulativePathStats[],
                choppedStats: CumulativePathStats,
                splitIndex: number) {
      const pre        = steppedStats[splitIndex].deaccumulate(choppedStats);

      // Keep stats value components based on the final point of the 'pre' segment.
      const finalStats = steppedStats[steppedStats.length-1];
      const post       = finalStats.deaccumulate(steppedStats[splitIndex-1]);
      const union      = finalStats.deaccumulate(choppedStats);

      super(pre, post, union);
      this.baseChop  = choppedStats;
      this.chopPoint = steppedStats[splitIndex-1];
      this.endOfPre  = steppedStats[splitIndex];
    }
  }

  /* FIXME:  Note that this function is a temporary development stopgap and will likely shift
    * as development continues for a few reasons:
    * 1. In its current form, it'd be better to return a completed `Segment`; this is being used
    *    to finalize `Segment`s, after all.
    * 2. Except... we'll actually want it for uncompleted `Segment`s too, for the tail member of
    *    the public `path.segments` array, which'll need UPDATING, not replacement.
    * 3. In some cases, subsegmentation provides an advantage for constructing / updating `Segment`s.
    *    E.g: Flicks threshold based on top speed, and the faster subsegment's stats are far better
    *    for this than the combined interval's stats.
    */
    const mergeSubsegmentations = function(array: PotentialSegmentation[]) {
      if(array.length == 1) {
        return array[0].pre; // It's pre-calculated, so just use it.
      } else {
        const finalSubsegmentation = array[array.length-1];
        return finalSubsegmentation.endOfPre.deaccumulate(array[0].baseChop);
      }
    }

  export class PathSegmenter {
    /**
     * The minimum amount of time (in ms) to wait between sample repetitions
     * once inputs stop arriving, so long as the path is still active.
     */
    private readonly REPEAT_INTERVAL = 33;

    /**
     * The time-interval length (in ms) at the end of the path to consider
     * when determining whether or not to trigger path segmentation.
     */
    private readonly SLIDING_WINDOW_INTERVAL = 50;

    /**
     * Tracks the mathematical values used to provide path segment stats
     * at each point on the path.  Individual steps may be removed (and
     * batched into `choppedStats`) once their respective points on the
     * path have been fully processed.
     */
    private steppedCumulativeStats: CumulativePathStats[];

    private lingeringSubsegmentations: PotentialSegmentation[];

    // Currently used as an in-development diagnostic assist... but these
    // directly represent actual path segments as produced by the prototype
    // algorithm.  Just... the stats analysis of the path segment, without
    // obvious / public members to relevant coordinates.
    private _protoSegments: CumulativePathStats[] = [];
    private _protoSegmentSets: CumulativePathStats[][] = [];

    /**
     * Used to 'repeat' the most-recently observed incoming sample if no
     * other replaces it before it triggers.
     *
     * A repeating sample indicates lack of motion, which is valuable
     * information for stats-based segmentation.
     */
    private repeatTimer: number | NodeJS.Timeout;

    /**
     * The timestamp of observation of the most recently observed sample's
     * most recent repetition - even if it's only the first evaluation.
     */
    private repeatTimestamp: number;

    /**
     * Represents the cumulative statistics of all points on the path
     * that lie on already-fully-segmented parts of it.
     */
    private choppedStats: CumulativePathStats = null;
    private lastIntervalDuration = 0;

    constructor() {
      this.steppedCumulativeStats = [];
      this.lingeringSubsegmentations = [];
    }

    public add(sample: InputSample) {
      const repeater = (timeDelta: number) => {
        this.observe(sample, timeDelta);
      }

      if(this.repeatTimer) {
        // @ts-ignore
        clearInterval(this.repeatTimer);
        this.repeatTimer = null;
      }

      this.repeatTimer = setInterval(() => {
        const timeDelta = Date.now() - this.repeatTimestamp;
        repeater(timeDelta);
      }, this.REPEAT_INTERVAL);
      this.repeatTimestamp = Date.now();
      repeater(0);
    }

    public close() {
      // The Node clearTimeout & DOM clearTimeout appear to TS as overloads of each other,
      // and their type definitions will conflict.  A simple @ts-ignore will bypass this issue.
      // @ts-ignore
      clearInterval(this.repeatTimer);
      this.repeatTimer = null;

      // The way things are structured, finalization.pre = final segment.  It's some happy
      // 'fallout' from the implementation's design.
      let finalization = new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, this.steppedCumulativeStats.length-1);

      console.log("! Finalization !");
      this.filterSubsegmentation(finalization, true); // forces out the final segment.
                                                      // Hacky, but "enough" for now.

      // FIXME:  temporary statement to facilitate exploration, experimentation, & debugging
      console.log(this._protoSegments);
      console.log(this._protoSegmentSets);
      console.log(this._protoSegments.map((val) => (val.toJSON())));
    }

    private observe(sample: InputSample, timeDelta: number) {
      let cumulativeStats: CumulativePathStats;
      if(this.steppedCumulativeStats.length) {
        cumulativeStats = this.steppedCumulativeStats[this.steppedCumulativeStats.length-1];
      } else {
        cumulativeStats = new CumulativePathStats();
      }

      sample = {... sample};
      sample.t += timeDelta;
      const extendedStats = cumulativeStats.extend(sample);
      this.steppedCumulativeStats.push(extendedStats);

      this.attemptSegmentation();
    }

    private _debugLogSegmentationReport(candidateSplit: PotentialSegmentation) {
      console.log("------------------------------------------------------------------");

      // console.log("Angle variance ratio: " + candidateSplit.angleVarianceRatio);
      // console.log("Speed variance ratio: " + candidateSplit.speedVarianceRatio);

      console.log("Combined: ");
      console.log(candidateSplit.union.toJSON());
      console.log("Pre: ")
      console.log(candidateSplit.pre.toJSON());
      console.log("Post: ");
      console.log(candidateSplit.post.toJSON());

      console.log();

      const xF = candidateSplit.segReg('x', 't');
      const yF = candidateSplit.segReg('y', 't');
      console.log(`x F-test: F_(${xF.fDoF1}, ${xF.fDoF2}) = ${xF.fStat} @ ${xF.certaintyThreshold}`);
      console.log(`y F-test: F_(${yF.fDoF1}, ${yF.fDoF2}) = ${yF.fStat} @ ${yF.certaintyThreshold}`);

      console.log();

      console.log("Split object: ");
      console.log(candidateSplit);

      console.log("------------------------------------------------------------------");
        // END:  DO NOT RELEASE.
    }

    private attemptSegmentation() {
      const cumulativeStats = this.steppedCumulativeStats[this.steppedCumulativeStats.length - 1];
      const unsegmentedDuration = cumulativeStats.lastTimestamp - this.steppedCumulativeStats[0].lastTimestamp;

      if(unsegmentedDuration < this.SLIDING_WINDOW_INTERVAL * 2) {
        console.log("Interval too short for segmentation.");
        return;
      }

      let splitPoint = 0;
      // Do not consider the just-added `extendedStats` entry.
      //
      // Note:  even if we do reconsider the segmentation point... I don't think we
      // should reconsider anything earlier than where this marker falls.
      //
      // If we didn't segment earlier before, why would we suddenly do so now?
      for(let i = this.steppedCumulativeStats.length-2; i >=0; i--) {
        if(this.steppedCumulativeStats[i].lastTimestamp < cumulativeStats.lastTimestamp - this.SLIDING_WINDOW_INTERVAL) {
          splitPoint = i+1;
          break;
        }
      }

      // We split the cumulative stats on a specific point, which then resides on the edge
      // of both of the resulting intervals.
      let candidateSplit = new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, splitPoint);

      const lastIntervalDuration = this.lastIntervalDuration;
      this.lastIntervalDuration = unsegmentedDuration;

      const xF = candidateSplit.segReg('x', 't');
      const yF = candidateSplit.segReg('y', 't');
      if(!candidateSplit.segmentationMerited) {
        // // Debug logging statements:
        // console.log("Angle variance ratio: " + candidateSplit.angleVarianceRatio);
        // console.log("Speed variance ratio: " + candidateSplit.speedVarianceRatio);

        console.log(`x F-test: F_(${xF.fDoF1}, ${xF.fDoF2}) = ${xF.fStat} @ ${xF.certaintyThreshold}`);
        console.log(`y F-test: F_(${yF.fDoF1}, ${yF.fDoF2}) = ${yF.fStat} @ ${yF.certaintyThreshold}`);

        console.log("candidate split: " );
        console.log(candidateSplit);
        // QUESTION:  wait, what if we don't exit early?  Does that help 'wait' detection?
        // if(lastIntervalDuration >= this.SLIDING_WINDOW_INTERVAL * 2) {
        //   return;
        // }
      }

      // We either have the conditions to trigger segmentation or just became long enough to consider it.
      // If we're only just long enough to consider it, there may be a better segmentation point to start with.
      // Now... is there a better segmentation point?

      class SplitSearchState {
        readonly xTest: typeof Segmentation.segmentationComparison.prototype;
        readonly yTest: typeof Segmentation.segmentationComparison.prototype;
        readonly candidate: PotentialSegmentation;

        constructor(candidate?: PotentialSegmentation) {
          this.candidate = candidate;

          if(candidate == null) {
            return;
          }

          this.xTest = candidate.segReg('x', 't');
          this.yTest = candidate.segReg('y', 't');
        }

        get segRating() {
          if(this.candidate) {
            return Math.max(this.xTest.coefficientOfDetermination, this.yTest.coefficientOfDetermination);
          } else {
            return 0;
          }
        }

        get segmentationMerited() {
          return this.candidate?.segmentationMerited ?? false;
        }
      }

      let currentSplit = new SplitSearchState(candidateSplit);

      let leftSplit = new SplitSearchState(new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, splitPoint-1));
      let rightCandidate: PotentialSegmentation = null;
      if(splitPoint+1 < this.steppedCumulativeStats.length) {
        rightCandidate = new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, splitPoint+1);
      }
      let rightSplit = new SplitSearchState(rightCandidate);

      const criteria = [leftSplit.segRating, currentSplit.segRating, rightSplit.segRating];
      let sortedCriteria = [...criteria].sort();

      // TODO: if we're better on both axes on one side, let's start shifting.
      const delta = criteria.indexOf(sortedCriteria[2])-1;  // -1 if 'left' is best, 1 if 'right' is best.

      if(delta != 0) {
        // We can get better segmentation by shifting.  Proceed in the optimal direction.
        do {
          const nextSplitIndex = splitPoint + delta;
          if(nextSplitIndex >= this.steppedCumulativeStats.length || nextSplitIndex < 0) {
            break;
          }

          let nextCandidate = new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, splitPoint + delta);
          let nextSplit = new SplitSearchState(nextCandidate);

          // Prevent overly-short intervals / over-segmentation.
          if(nextCandidate.pre.duration * 1000 < this.SLIDING_WINDOW_INTERVAL / 2) {
            break;
          } else if(nextCandidate.post.duration * 1000 < this.SLIDING_WINDOW_INTERVAL / 2) {
            break;
          }

          // Not an improvement?  Guess we found the best spot.
          if(nextSplit.segRating <= currentSplit.segRating) {
            break;
          } else if(!nextSplit.segmentationMerited && currentSplit.segmentationMerited) {
            console.warn("aborting split-point relocation due to no longer segmenting");
            break;
          } else {
            splitPoint += delta;
            currentSplit = nextSplit;
            candidateSplit = nextCandidate;
          }
          // If we found a new best segmentation point, we then ask if we can get even better by shifting further.
        } while(true);
      }

      console.log("best split: ");
      console.log(candidateSplit);

      if(!candidateSplit.segmentationMerited) {
        return;
      }

      // First phase of segmentation:  complete!

      // But... there are some cases where we want to prevent segmentation from fully happening.
      // TODO:  That.

      // FIXME: DO NOT RELEASE.
      // This is exploratory / diagnostic code assisting development of the path segmentation
      // algorithm.
      this._debugLogSegmentationReport(candidateSplit);
      // END:  DO NOT RELEASE.

      this.steppedCumulativeStats = this.steppedCumulativeStats.slice(splitPoint);
      this.choppedStats = candidateSplit.chopPoint;
      this.lastIntervalDuration = candidateSplit.post.duration * 1000;

      this.filterSubsegmentation(candidateSplit);
    }

    // NOTE:  This function, as well as code called by it, are still very much still in prototyping.
    private filterSubsegmentation(subsegmentation: PotentialSegmentation, force?: boolean) {
      force = !!force;

      if(this.lingeringSubsegmentations.length) {
        // First:  check if the newly-finished subsegment should be merged with the lingering ones.
        const lastSubsegment = this.lingeringSubsegmentations[this.lingeringSubsegmentations.length-1];

        const tailUnion = mergeSubsegmentations([lastSubsegment, subsegmentation]);
        console.log("Lingering segment(s) considered for linking: ");
        console.log(this.lingeringSubsegmentations);
        if(!PathSegmenter.shouldMergeSubsegments(lastSubsegment.pre, subsegmentation.pre, tailUnion)) {
          // Emit as separate subsegment.
          const finishedSegment = mergeSubsegmentations(this.lingeringSubsegmentations);
          this._protoSegments.push(finishedSegment);
          this._protoSegmentSets.push(this.lingeringSubsegmentations.map((val) => val.pre));
          this.lingeringSubsegmentations = [];

          // IN DEVELOPMENT:  does this happen much?  If so... maybe we need intervening checks to pre-filter even
          // if not segmenting.
          // So far... not _much_, but I've seen it a few times.
          console.warn("Did not merge a lingering subsegment with the incoming one!");
        } else {
          console.log("Will merge in old subsegments!");
        }
      }

      console.log("Double-checking newly split subsegments for xy/yx correlation");
      if(!force && PathSegmenter.shouldMergeSubsegments(subsegmentation.pre, subsegmentation.post, subsegmentation.union)) {
        this.lingeringSubsegmentations.push(subsegmentation);
      } else {
        // Merge all as a completed segment!
        const finishedSegment = mergeSubsegmentations([...this.lingeringSubsegmentations, subsegmentation]);
        this._protoSegments.push(finishedSegment);
        this._protoSegmentSets.push([...this.lingeringSubsegmentations.map((val) => val.pre), subsegmentation.pre]);
        this.lingeringSubsegmentations = [];
      }
    }

    private static shouldMergeSubsegments(segment1: CumulativePathStats,
                                          segment2: CumulativePathStats,
                                          combined: CumulativePathStats): boolean {
      // // Known case #1:
      // //   Near-identical direction, but heavy speed difference.
      // //   Speed's still high enough to not be a 'wait'.
      // //   We may want to 'note' the higher speed; that may be relevant for flick detection.
      // // Known case #2:
      // //   Low-speed pivot; angle change caught on very low velocity for both subsegments.
      // //   ... or should this be merged?  Multiple mini-segments from 'wiggling' could just go ignored instead...

      if(segment1.speedMean < 80 && segment2.speedMean > 80) {
        console.log("desegmentation exception");
        return; // SUPER TEMP: needs further work; avoids the "low-speed pivot" case.
      }
      if(segment1.speedMean > 80 && segment2.speedMean < 80) {
        console.log("desegmentation exception");
        return;
      }

      const asSegmentation = new Segmentation(segment1, segment2, combined);

      console.log("Desegmentation under consideration: ");
      console.log(asSegmentation);

      const xF = asSegmentation.segReg('x', 'y');
      const yF = asSegmentation.segReg('y', 'x');
      console.log(`merger F-test (xy): F_(${xF.fDoF1}, ${xF.fDoF2}) = ${xF.fStat} @ ${xF.certaintyThreshold}`);
      console.log(`merger F-test (yx): F_(${yF.fDoF1}, ${yF.fDoF2}) = ${yF.fStat} @ ${yF.certaintyThreshold}`);
      console.log(`will remerge: ${asSegmentation.mergeMerited}`);

      return asSegmentation.mergeMerited;
    }
  }
}