/// <reference path="cumulativePathStats.ts" />

/* FIXME:  Too much `console`.
 *
 * There's an AWFUL LOT of `console.log`-ing in this file at present.
 * It was definitely useful for prototyping & development of its (and
 * of `cumulativePathStats`'s) code, but it's GOTTA be cleaned up
 * in the next PR... if not sooner.
 */

namespace com.keyman.osk {
  // Note:  the only `export`-ed class from this file is `PathSegmenter`, hence the helper
  // classes being placed within the same file.  Not to say we _couldn't_ put them elsewhere.

  // ------------

  // Mostly used here to compare the sum-squared error components of segmented regressions
  // to the sum-squared "modeled" components of their overall variance.  Those are the two
  // "independent random variables" we're examining.  Variances (which are sums of squared
  // values themselves) tend to be chi-squared distributed, fulfilling the conditions.
  // That's stats-speak for "what this module does with it has a solid theoretical basis."
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

  /**
   * Acts as a lookup table for null-hypothesis tests utilizing the F-distribution from the
   * domain of statistics.  Refer to https://en.wikipedia.org/wiki/F-distribution for details
   * on its properties.
   *
   * This is only a partial implementation; only a couple of numerator degrees-of-freedom are
   * worth our consideration, and there's only marginal gain to be found supporting more
   * denominator degrees-of-freedom than are already supported here.  Keeping it small also
   * helps with maintenance and readability.
   */
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
     * 'numerator' and 'denominator' components.  Exceeding the threshold specified
     * for the p = .100 section indicates less than a 10% probability of the statistic
     * being tested having arisen by purely random chance.  Failure to exceed even
     * that implies an unacceptably high chance of being "convenient but meaningless" -
     * the "null hypothesis", in stats speak.
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
     * Returns 0.050 if the statistic indicates a p-value of 0.050 or lower.
     * Failing that, returns 0.100 if a p-value of 0.100 or lower is indicated.
     * Otherwise, returns 1.0 - we have no basis to "reject the null hypothesis".
     */
    static thresholdTier(statistic: number, numDoF: number, denomDoF: number) {
      // Former case:  we'd never segment anyway
      // Latter case:  it's currently wrong to statistically test.
      //               The F-distribution is not defined for this case.
      if(numDoF < 2 || denomDoF < 1) {
        return 1;
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

  /**
   * Represents the result of a segmentation of the search path and the related
   * statistical accumulations needed to properly test the segmentation's
   * validity.
   */
  class Segmentation {
    public static readonly SPLIT_CRITERION_THRESHOLD = 1.5;

    /**
     * The properties of the "left-hand" / earlier half of the time interval
     * being segmented.
     */
    readonly pre:   CumulativePathStats;

    /**
     * The properties of the "right-hand" / later half of the time interval
     * being segmented.
     */
    readonly post:  CumulativePathStats;

    /**
     * The properties of the full interval being segmented, before the split.
     */
    readonly union: CumulativePathStats;

    /**
     * The full running accumulation for the ongoing touch path.  May include
     * accumulations from before the interval under examination.
     */
    readonly endpoint: CumulativePathStats;

    /**
     * Provides segmented-regression statistics & fitting values on the two specified axes /
     * dimensions based on the subsegments and their corresponding linear-regression
     * statistics.  All operations are O(1).
     */
    static readonly segmentationComparison = class SegmentedRegression {
      host: Segmentation;
      readonly independent: PathCoordAxis;
      readonly dependent:   PathCoordAxis;
      readonly paired:      'tx' | 'ty' | 'xy';

      /**
       * The linear regression for the underlying `pre` (earlier) subsegment.
       */
      pre:   typeof CumulativePathStats.regression.prototype;

      /**
       * The linear regression for the underlying `post` (later) subsegment.
       */
      post:  typeof CumulativePathStats.regression.prototype;

      /**
       * The linear, unsegmented regression for the underlying `union` (combined) subsegment.
       */
      union: typeof CumulativePathStats.regression.prototype;

      constructor(host: Segmentation, dependentAxis: PathCoordAxis, independentAxis: PathCoordAxis) {
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

      /**
       * The total summed squared-distances of the best fitting line from actually-observed values
       * for their corresponding subsegment; in other words, the "sum of the squared errors" when
       * utilizing segmented regression.
       *
       * Statistically, this is the portion of the dependent variable's variance (un-normalized)
       * that is unexplained even after dividing the interval into separate subsegments.
       */
      get remainingSumSquaredError(): number {
        return this.pre.sumOfSquaredError + this.post.sumOfSquaredError;
      }

      /**
       * Provides the coordinate that belongs to BOTH subsegments, as it ends one and begins
       * the other.
       */
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

      /**
       * The total summed squared-distances of the best fitting line from actually-observed values
       * when **not** utilizing segmented regression.  Double-counts the subsegment's common point, as
       * it is counted within both subsegments' sum-of-squared error term.
       */
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

      /**
       * Gets the amount of (unnormalized) variance not explained by a standard regression on
       * the full interval but succesfully explained by splitting the interval in twain via
       * segmented regression.
       */
      get sumSquaredGainFromSegmentation(): number {
        return this.unsegmentedSumSquaredError - this.remainingSumSquaredError;
      }

      /**
       * A statistical term that signals how successful the segmented regression is.  Always
       * has values on the interval [0, 1], with 1 being a perfect fit.
       */
      get coefficientOfDetermination(): number {
        if(this.host.union.squaredSum(this.dependent) == 0 || this.host.union.squaredSum(this.independent) == 0) {
          return 1;
        }

        return 1 - this.remainingSumSquaredError / this.host.union.squaredSum(this.dependent);
      }

      /**
       * Gets the raw statistic value needed to test for validity of segmentation based on the axes
       * under examination.  The higher the proportion of newly-explained variance to still-unexplained
       * variance is, the more confident we can _formally_ be in our segmentation.
       */
      get fStat(): number {
        const val = this.sumSquaredGainFromSegmentation / this.remainingSumSquaredError;

        // We're fine with Infinity.  Just... not so much NaN.
        return isNaN(val) ? 0 : val;
      }

      /**
       * Gets the degrees-of-freedom to be used for the numerator DoF parameter of
       * the F-distribution needed for validity testing.
       */
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

      /**
       * Gets the degrees-of-freedom to be used for the denominator DoF parameter of
       * the F-distribution needed for validity testing.
       */
      get fDoF2(): number {
        return this.host.union.sampleCount - 2 - this.fDoF1;
      }

      /**
       * States a statistically-founded level of confidence we may place in upholding
       * the represented segmentation, as based on the regressions and axes under examination.
       *
       * If we cannot hold any confidence whatsoever (due to being unable to "reject the
       * null hypothesis" from this specific regression), will return 0.
       */
      get certaintyThreshold() {
        return 1 - FDistribution.thresholdTier(this.fStat, this.fDoF1, this.fDoF2);
      }
    }

    constructor(pre: CumulativePathStats,
                post: CumulativePathStats,
                union: CumulativePathStats,
                cumulativeEndpoint: CumulativePathStats) {
      this.pre = pre;
      this.post = post;
      this.union = union;
      this.endpoint = cumulativeEndpoint;
    }

    /**
     * Provides a segmented-regression perspective for the represented interval and proposed
     * split point based upon the two specified axes.
     * @param dependent
     * @param independent
     * @returns
     */
    public segReg(dependentAxis: PathCoordAxis, independentAxis: PathCoordAxis) {
      return new Segmentation.segmentationComparison(this, dependentAxis, independentAxis);
    }

    /**
     * Defines our test for upholding a minimum of sub-segmentation based upon changes in the
     * interval's coordinates over time.  This may occur even with no change in spacial direction
     * if there is significant enough change in _speed_, as that "curves the line" when one axis
     * is time.
     */
    get segmentationMerited(): boolean {
      let totalThreshold = 0;
      const xTest = new Segmentation.segmentationComparison(this, 'x', 't');
      const yTest = new Segmentation.segmentationComparison(this, 'y', 't');

      // Our testing thresholds are for p=0.05 and p=0.10, which correspond to certainties of 95% and
      // 90% that our segmentation did not arrive from random chance based on the axis being tested.
      const p05 = 0.95; // 1 - 0.05
      const p10 = 0.9;  // 1 - 0.10

      totalThreshold += xTest.certaintyThreshold >= p05 ? 2 : (xTest.certaintyThreshold >= p10 ? 1 : 0) ;
      totalThreshold += yTest.certaintyThreshold >= p05 ? 2 : (yTest.certaintyThreshold >= p10 ? 1 : 0) ;

      return totalThreshold >= 2;
    }

    /**
     * Defines our test for considering two (or more) sub-segments as part of the same overall
     * segment.  This seeks to 'link' geometrically-related subsegments - especially those that
     * maintain the same direction but differ only in observed speed.
     */
    get mergeMerited(): boolean {
      // Because of caret-like motions (as in, in the '^' shape), we need to text for
      // regression on both axes.  One may have notably higher variance than the other.
      //
      // These tests ignore time, and therefore speed.  Only the raw geometry of the motion
      // matters for this test.
      const xTest = new Segmentation.segmentationComparison(this, 'x', 'y');
      const yTest = new Segmentation.segmentationComparison(this, 'y', 'x');

      // If we don't get a p-value less than .100, then as far as x & y are concerned - and thus the user
      // is concerned - it's the same segment.  Speed may be different, but not the direction.
      if(xTest.certaintyThreshold > 0) {
        return false;
      }

      return yTest.certaintyThreshold == 0;
    }
  }

  /**
   * A specialized form of the `Segmentation` class that assists in its
   * construction from the interval directly under examination by the
   * segmentation algorithm.
   */
  class PotentialSegmentation extends Segmentation {
    /**
     * The full running accumulation for the ongoing touch path until the point
     * before `post` / the later subsegment.  May include
     * accumulations from before the interval under examination.
     */
    readonly chopPoint: CumulativePathStats;

    /**
     * The full running accumulation for the ongoing touch path until the point
     * at the end of `pre` / the earlier subsegment.  May include
     * accumulations from before the interval under examination, and will
     * include accumulations from the first point represented by `post`.
     */
    readonly endOfPre:  CumulativePathStats;

    /**
     * The full running accumulation for the ongoing touch path until the point
     * before `pre` / the earlier subsegment.  Will only include
     * accumulations from before the interval under examination.
     */
    readonly baseChop:  CumulativePathStats;

    constructor(steppedStats: CumulativePathStats[],
                choppedStats: CumulativePathStats,
                splitIndex: number) {
      const pre        = steppedStats[splitIndex].deaccumulate(choppedStats);

      // Keep stats value components based on the final point of the 'pre' segment.
      const finalStats = steppedStats[steppedStats.length-1];
      const post       = finalStats.deaccumulate(steppedStats[splitIndex-1]);
      const union      = finalStats.deaccumulate(choppedStats);

      super(pre, post, union, finalStats);
      this.baseChop  = choppedStats;
      this.chopPoint = steppedStats[splitIndex-1];
      this.endOfPre  = steppedStats[splitIndex];
    }
  }

  /**
   * The core logic, algorithm, and manager for touchpath segmentation.
   */
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

    /**
     * Tracks all subsegments awaiting completion of their overall segment.
     */
    private lingeringSubsegmentations: PotentialSegmentation[];

    /**
     * TODO: These directly represent path segments produced by the prototype
     * algorithm.  Just... the stats analysis of the path segment, without
     * obvious / public members to relevant coordinates.
     *
     * In follow-up work, this should be used to build & to update
     * cleaner, public-facing segment objects for the touch path.
     */
    private _protoSegments: CumulativePathStats[] = [];

    /**
     * TODO: These directly represent the subsegments comprising their
     * corresponding path segments (in `_protoSegments`) produced by the
     * prototype algorithm.  Just... the stats analysis of each subsegment.
     *
     * In follow-up work, these should be used internally to build & update
     * the internals of TBD cleaner, public-facing segment objects for the
     * touch path.
     */
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

    constructor() {
      this.steppedCumulativeStats = [];
      this.lingeringSubsegmentations = [];
    }

    /**
     * Appends a new coordinate to the touch path and also kick-starts a timer for replicating it
     * should neither further inputs be received nor termination of the touchpoint be indicated.
     * @param sample
     */
    public add(sample: InputSample) {
      // Set up the input-repeater (in case we don't get further feedback but remain active)
      const repeater = (timeDelta: number) => {
        this.observe(sample, timeDelta);
      }

      // If we previously set up an input-repeater, cancel it.  We've got a more up-to-date
      // coordinate on the touchpath now.
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

    /**
     * Used to finalize all segmentation for the touchpath whenever termination of the corresponding
     * touchpoint has been terminated (mouse-up, touch-end).
     */
    public close() {
      // The Node clearTimeout & DOM clearTimeout appear to TS as overloads of each other,
      // and their type definitions will conflict.  A simple @ts-ignore will bypass this issue.
      // @ts-ignore
      clearInterval(this.repeatTimer);  // Cancels the input-repeater.
      this.repeatTimer = null;

      // The way things are structured, finalization.pre = final segment.  It's a bit hacky, but
      // it _is_ happy, intentional 'fallout' from the implementation's design.  `post` is effectively
      // a single-point 'subsegment' and will go effectively unutilized by the phase 2 subsegment linker.
      let finalization = new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, this.steppedCumulativeStats.length-1);

      console.log("! Finalization !");
      this.processSubsegmentation(finalization, true);

      // TODO:  these are temporary statements to facilitate exploration, experimentation, & debugging.
      //        We should be providing output to the touchpath object (`.path.segments`).
      //        But... that'll be left for a follow-up PR.
      console.log(this._protoSegments);
      console.log(this._protoSegmentSets);
      console.log(this._protoSegments.map((val) => (val.toJSON())));
    }

    /**
     * Adds a statistic 'observation' of the state of the touchpath.
     *
     * This is either to be called directly upon reception of a new input-coordinate or
     * upon replication of a prior input should the touchpath still be active without any
     * indication of motion.
     * @param sample
     * @param timeDelta
     */
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

      this.performSubsegmentation();
    }

    private _debugLogSegmentationReport(candidateSplit: PotentialSegmentation) {
      console.log("------------------------------------------------------------------");

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

    // The "reported via event" aspect mentioned below is necessary because this may be
    // called via setTimeout callback on a held touchpoint.  We need that `setTimeout`
    // (which replicates the most recently-seen input coordinate) in order to have good
    // sample-data for detecting the boundary between motion and lack thereof during
    // segmentation.
    /**
     * This is the "main method" for touchpath segmentation.  Should a new segment result
     * from its analysis, it will be reported via event.
     * @returns
     */
    private performSubsegmentation() {
      const cumulativeStats = this.steppedCumulativeStats[this.steppedCumulativeStats.length - 1];
      const unsegmentedDuration = cumulativeStats.lastTimestamp - this.steppedCumulativeStats[0].lastTimestamp;

      // STEP 1:  Determine the range of the sliding time window for the most recent samples.

      if(unsegmentedDuration < this.SLIDING_WINDOW_INTERVAL * 2) {
        console.log("Interval too short for segmentation.");
        return;
      }

      let splitPoint = 0;
      // Do not consider the just-added `extendedStats` entry when building the sliding
      // time window.
      for(let i = this.steppedCumulativeStats.length-2; i >=0; i--) {
        if(this.steppedCumulativeStats[i].lastTimestamp < cumulativeStats.lastTimestamp - this.SLIDING_WINDOW_INTERVAL) {
          splitPoint = i+1;
          break;
        }
      }


      // We split the cumulative stats on a specific point, which then resides on the edge
      // of both of the resulting intervals.  This completes "step 1".
      let candidateSplit = new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, splitPoint);

      // STEP 2:  given the proposed time window, do we have a basis for segmentation?  And is there a better
      // candidate split point nearby?

      // NOTE:  During initial development, I actually had a hard rule about not even searching if segmentation
      // failed on the initially-constructed sliding window.  Some analysis may be wise here, as it'd be nice
      // to optimize away the 'need to search' if we can find hard & fast rules about when we'll never need
      // to go looking.  But... being too aggressive can cause nasty problems.

      if(!candidateSplit.segmentationMerited) {
        // // Debug logging statements:
        const xTest = candidateSplit.segReg('x', 't');
        const yTest = candidateSplit.segReg('y', 't');
        console.log(`x F-test: F_(${xTest.fDoF1}, ${xTest.fDoF2}) = ${xTest.fStat} @ ${xTest.certaintyThreshold}`);
        console.log(`y F-test: F_(${yTest.fDoF1}, ${yTest.fDoF2}) = ${yTest.fStat} @ ${yTest.certaintyThreshold}`);

        console.log("candidate split: " );
        console.log(candidateSplit);
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

        // The value for an 'objective function' to optimize in our search for a better candidate.
        // The higher this is, the more we like its potential as the segmentation point.
        get segRating() {
          if(this.candidate) {
            return Math.max(this.xTest.coefficientOfDetermination, this.yTest.coefficientOfDetermination);
          } else {
            return 0;
          }
        }

        // But, if it turns out this potential point wouldn't actually result in segmentation, well...
        // "abandon ship".
        get segmentationMerited() {
          return this.candidate?.segmentationMerited ?? false;
        }
      }

      // Start:  split-point search

      /* We'll use our initial sliding window as a starting point.  It's close to the active location
       * of the touch, so we're unlikely to miss a proper segmentation point if it lies close.
       * We'll find the local maximum (best split point for the local part of the path)
       * via gradient descent.
       *
       * Note:  binary search is a 'bad idea', as the touchpath is likely not following a
       * monotonous path, mathematically speaking.  (It'd be possible to overshoot a "peak" or
       * "valley" quite easily.)
       */
      let currentSplit = new SplitSearchState(candidateSplit);

      let leftSplit = new SplitSearchState(new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, splitPoint-1));
      let rightCandidate: PotentialSegmentation = null;
      if(splitPoint+1 < this.steppedCumulativeStats.length) {
        rightCandidate = new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, splitPoint+1);
      }
      let rightSplit = new SplitSearchState(rightCandidate);

      // Step 2a:  detect which direction gives the best improvement in segmentation potential.
      const criteria = [leftSplit.segRating, currentSplit.segRating, rightSplit.segRating];
      let sortedCriteria = [...criteria].sort();

      const delta = criteria.indexOf(sortedCriteria[2])-1;  // -1 if 'left' is best, 1 if 'right' is best.

      // Step 2b: we've found the direction:  go searching!
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
            // Note:  this can happen if segmentation is triggered due to divergence on both axes
            // if one of them drops a threshold tier and the other fails to improve sufficiently.
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
      // Step 2 complete.

      // Step 3:  evaluate our candidate split-point - do we sub-segment?  If so, do so.
      console.log("best split: ");
      console.log(candidateSplit);

      if(!candidateSplit.segmentationMerited) {
        return;
      }

      // First phase of segmentation:  complete!

      // FIXME: DO NOT RELEASE.
      // This is exploratory / diagnostic code assisting development of the path segmentation
      // algorithm.
      this._debugLogSegmentationReport(candidateSplit);
      // END:  DO NOT RELEASE.

      this.steppedCumulativeStats = this.steppedCumulativeStats.slice(splitPoint);
      this.choppedStats = candidateSplit.chopPoint;

      // Step 4:  process the implications.

      // There are some cases where we want to prevent segmentation from fully happening.
      // The next method will both handle that and the signal of any completed segments that
      // may result.
      this.processSubsegmentation(candidateSplit);
    }

    /**
     * Given the results of the subsegmentation process, this method analyzes the results
     * in order to produce and update the detected segments.
     *
     * TODO:  Should produce actual `Segment` objects and trigger their emission
     * them via EventEmitter mechanics.
     * @param subsegmentation
     * @param finalize         Set to `true` for a terminating touchpath, indicating that
     *                         no further subsegmentation will occur for this touchpath.
     */
    private processSubsegmentation(subsegmentation: PotentialSegmentation, finalize?: boolean) {
      finalize = !!finalize;

      // This function makes a LOT of assumptions particular to this method and class.
      // It is not safe to make this class-level, and it's especially unsafe to make it `public`.
      const mergeSubsegmentationAccumulations = function(array: PotentialSegmentation[]) {
        if(array.length == 1) {
          return array[0].pre; // It's pre-calculated, so just use it.
        } else {
          const finalSubsegmentation = array[array.length-1];
          return finalSubsegmentation.endOfPre.deaccumulate(array[0].baseChop);
        }
      }

      // Step 1:  did we previously note that any previous subsegments are likely to be 'linked' to
      // the new, incoming left-hand subsegment?  If so, validate that expectation & act accordingly.

      let predecessor = subsegmentation.pre;
      let firstChopPoint = subsegmentation.baseChop;
      if(this.lingeringSubsegmentations.length) {
        // First:  check if the newly-finished subsegment should be merged with the lingering ones.
        const mergedPrecursors = mergeSubsegmentationAccumulations(this.lingeringSubsegmentations);

        const tailUnion = mergeSubsegmentationAccumulations([...this.lingeringSubsegmentations, subsegmentation]);
        console.log("Verifying linkage to pending merges: ");
        console.log(this.lingeringSubsegmentations);
        console.log("verification check:");
        console.log(mergedPrecursors);
        const precursorMergeSegmentation = new Segmentation(mergedPrecursors, subsegmentation.pre, tailUnion, subsegmentation.endOfPre);
        // Sometimes the start of a harsh turn seems like it's part of the same thing for a moment, but as it continues,
        // becomes something VERY different.  Validate that we should still merge the left-hand with its predecessors.
        if(!PathSegmenter.shouldLinkSubsegments(precursorMergeSegmentation)) {
          // Emit as separate subsegment.
          const finishedSegment = mergeSubsegmentationAccumulations(this.lingeringSubsegmentations);
          this._protoSegments.push(finishedSegment);
          this._protoSegmentSets.push(this.lingeringSubsegmentations.map((val) => val.pre));
          this.lingeringSubsegmentations = [];

          console.log("Proto-segments length: " + this._protoSegments.length);

          console.log("Did not merge a lingering subsegment with the incoming one!");
        } else {
          predecessor = tailUnion;
          firstChopPoint = this.lingeringSubsegmentations[0].baseChop;
          console.log("Will merge in old subsegments!");
        }
      }

      // Step 2:  okay, predecessor handling complete.  Now, do we think the newly-starting right-hand
      // subsegment is likely to be 'linked' to the newly-completed left-hand subsegment?

      console.log("Double-checking right-side split subsegment for xy/yx correlation with prior segment candidate(s)");
      const fullUnion = subsegmentation.endpoint.deaccumulate(firstChopPoint);
      const fullMergeSegmentation = new Segmentation(predecessor, subsegmentation.post, fullUnion, subsegmentation.endpoint);
      console.log("segmentation-prevention check:")
      console.log(fullMergeSegmentation);
      // if(!force && PathSegmenter.shouldMergeSubsegments(subsegmentation.pre, subsegmentation.post, subsegmentation.union)) {
      if(!finalize && PathSegmenter.shouldLinkSubsegments(fullMergeSegmentation)) {
        this.lingeringSubsegmentations.push(subsegmentation);
      } else {
        // Merge all as a completed segment!
        this._protoSegments.push(predecessor);
        this._protoSegmentSets.push([...this.lingeringSubsegmentations.map((val) => val.pre), subsegmentation.pre]);
        this.lingeringSubsegmentations = [];

        console.log("Proto-segments length: " + this._protoSegments.length);
      }
    }

    /**
     * Used by the 'filter' step (phase 2 of segmentation) to determine whether or not
     * two subsegments should be 'linked' as members of the same segment.  (That is, if
     * a user would likely consider the two subsegments as belonging to the "same arc
     * of motion".)
     * @param segmentation
     * @returns
     */
    private static shouldLinkSubsegments(segmentation: Segmentation): boolean {
      // // Known case #1:
      // //   Near-identical direction, but heavy speed difference.
      // //   Speed's still high enough to not be a 'wait'.
      // //   We may want to 'note' the higher speed; that may be relevant for flick detection.
      // // Known case #2:
      // //   Low-speed pivot; angle change caught on very low velocity for both subsegments.
      // //   ... or should this be merged?  Multiple mini-segments from 'wiggling' could just go ignored instead...

      // This is kinda plain and arbitrary, but it seems to work "well enough" for now,
      // at least at this stage of development.  (Most testing was done with Chrome emulation of
      // an iPhone SE.)

      // .mean('v') < 0.08:  the mean speed (taken timestamp to timestamp) does not exceed 0.08px/millisec.
      if(segmentation.pre.mean('v') < 0.08 && segmentation.post.mean('v') > 0.08) {
        console.log("desegmentation exception");
        return;
      }
      if(segmentation.pre.mean('v') > 0.08 && segmentation.post.mean('v') < 0.08) {
        console.log("desegmentation exception");
        return;
      }

      console.log("Desegmentation under consideration: ");
      console.log(segmentation);

      const xF = segmentation.segReg('x', 'y');
      const yF = segmentation.segReg('y', 'x');
      console.log(`merger F-test (xy): F_(${xF.fDoF1}, ${xF.fDoF2}) = ${xF.fStat} @ ${xF.certaintyThreshold}`);
      console.log(`merger F-test (yx): F_(${yF.fDoF1}, ${yF.fDoF2}) = ${yF.fStat} @ ${yF.certaintyThreshold}`);
      console.log(`will remerge: ${segmentation.mergeMerited}`);

      return segmentation.mergeMerited;
    }
  }
}