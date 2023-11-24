import { ConstructingSegment } from "./constructingSegment.js";
import { CumulativePathStats, PathCoordAxis, sigMinus } from "../cumulativePathStats.js";
import { InputSample } from "../inputSample.js";
import { Segment, SegmentImplementation } from "./segment.js";
import { SegmentClass, SegmentClassifier, SegmentClassifierConfig } from "../segmentClassifier.js";

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
 * Represents a "subsegmentation" - one part of a phase 1 segmentation split.
 */
export interface Subsegmentation {
  /**
   * The stats for observations comprising the subsegment.
   */
  stats: CumulativePathStats;

  /**
   * The cumulative stats up to the subsegment's endpoint, including
   * accumulated components that precede the subsegment entirely.
   */
  endingAccumulation: CumulativePathStats;

  /**
   * The cumulative stats up to the point before the subsegment's start
   * point, not including any accumulation for components within the subsegment.
   */
  baseAccumulation: CumulativePathStats;
}

/**
 * Represents the result of a segmentation of the search path and the related
 * statistical accumulations needed to properly test the segmentation's
 * validity.
 */
export class SegmentationSplit {
  public static readonly SPLIT_CRITERION_THRESHOLD = 1.5;

  /**
   * The properties of the "left-hand" / earlier half of the time interval
   * being segmented.
   */
  readonly pre:   Subsegmentation;

  /**
   * The properties of the "right-hand" / later half of the time interval
   * being segmented.
   */
  readonly post:  Subsegmentation;

  /**
   * The properties of the full interval being segmented, before the split.
   */
  readonly union: CumulativePathStats;

  /**
   * Provides segmented-regression statistics & fitting values on the two specified axes /
   * dimensions based on the subsegments and their corresponding linear-regression
   * statistics.  All operations are O(1).
   */
  static readonly segmentationComparison = class SegmentedRegression {
    host: SegmentationSplit;
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

    constructor(host: SegmentationSplit, dependentAxis: PathCoordAxis, independentAxis: PathCoordAxis) {
      if(dependentAxis == independentAxis) {
        throw new Error("Two different axes must be specified for the regression object.");
      }

      this.host = host;

      this.dependent   = dependentAxis;
      this.independent = independentAxis;

      if(dependentAxis < independentAxis) {
        this.paired = dependentAxis.concat(independentAxis) as 'tx' | 'ty' | 'xy';
      } else {
        this.paired = independentAxis.concat(dependentAxis) as 'tx' | 'ty' | 'xy';
      }

      this.pre   = this.host.pre  .stats.fitRegression(dependentAxis, independentAxis);
      this.post  = this.host.post .stats.fitRegression(dependentAxis, independentAxis);
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
    get splitPoint() {
      let splitPoint = this.host.pre.stats.lastSample;

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
      return sigMinus(this.unsegmentedSumSquaredError, this.remainingSumSquaredError);
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
      if(this.host.pre.stats.squaredSum(this.dependent) < 1e-8) {
        numDoF--;
      }
      if(this.host.post.stats.squaredSum(this.dependent) < 1e-8) {
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

    get prettyPrint(): string {
      return `F_(${this.fDoF1}, ${this.fDoF2}) = ${this.fStat} @ ${this.certaintyThreshold}`
    }
  }

  constructor(pre: Subsegmentation,
              post: Subsegmentation) {
    this.pre = pre;
    this.post = post;
    this.union = post.endingAccumulation.deaccumulate(pre.baseAccumulation);
  }

  static fromTrackedStats(steppedStats: CumulativePathStats[],
                          baseAccumulation: CumulativePathStats,
                          splitIndex: number) {
    let intervalEndStats = steppedStats[splitIndex];
    const pre: Subsegmentation = {
      stats:              intervalEndStats.deaccumulate(baseAccumulation),
      endingAccumulation: intervalEndStats,
      baseAccumulation:   baseAccumulation
    };

    // Keep stats value components based on the final point of the 'pre' segment.
    intervalEndStats = steppedStats[steppedStats.length-1];
    const postBaseAccumulation = steppedStats[splitIndex-1];
    const post: Subsegmentation = {
      stats:              intervalEndStats.deaccumulate(postBaseAccumulation),
      endingAccumulation: intervalEndStats,
      baseAccumulation:   postBaseAccumulation
    }

    return new SegmentationSplit(pre, post);
  }

  /**
   * Provides a segmented-regression perspective for the represented interval and proposed
   * split point based upon the two specified axes.
   * @param dependent
   * @param independent
   * @returns
   */
  public segReg(dependentAxis: PathCoordAxis, independentAxis: PathCoordAxis) {
    return new SegmentationSplit.segmentationComparison(this, dependentAxis, independentAxis);
  }

  /**
   * Defines our test for upholding a minimum of sub-segmentation based upon changes in the
   * interval's coordinates over time.  This may occur even with no change in spacial direction
   * if there is significant enough change in _speed_, as that "curves the line" when one axis
   * is time.
   */
  get segmentationMerited(): boolean {
    let totalThreshold = 0;
    const xTest = new SegmentationSplit.segmentationComparison(this, 'x', 't');
    const yTest = new SegmentationSplit.segmentationComparison(this, 'y', 't');

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
    const xTest = new SegmentationSplit.segmentationComparison(this, 'x', 'y');
    const yTest = new SegmentationSplit.segmentationComparison(this, 'y', 'x');

    // If we don't get a p-value less than .100, then as far as x & y are concerned - and thus the user
    // is concerned - it's the same segment.  Speed may be different, but not the direction.
    if(xTest.certaintyThreshold > 0) {
      return false;
    }

    return yTest.certaintyThreshold == 0;
  }

  public _debugLogSplitReport(demarcate?: boolean) {
    if(demarcate) {
      console.log("------------------------------------------------------------------");
    }

    console.log("Split object: ");
    console.log(this);

    console.log();

    const xF = this.segReg('x', 't');
    const yF = this.segReg('y', 't');
    console.log(`x F-test: ${xF.prettyPrint}`);
    console.log(`y F-test: ${yF.prettyPrint}`);

    console.log();

    if(demarcate) {
      console.log("------------------------------------------------------------------");
    }
  }

  public _debugLogAlignmentReport() {
    console.log("Desegmentation under consideration: ");
    console.log(this);

    console.log();

    const xF = this.segReg('x', 'y');
    const yF = this.segReg('y', 'x');
    console.log(`merger F-test (xy): ${xF.prettyPrint}`);
    console.log(`merger F-test (yx): ${yF.prettyPrint}`);
    console.log(`will remerge: ${this.mergeMerited}`);
  }
}

export interface SegmentationConfiguration extends SegmentClassifierConfig {
  // See `PathSegmenter.segmentationConfig`'s comment; we may wish to
  // define & provide extra configuration parameters here... or wherever
  // this type's final location ends up.
}

/**
 * The core logic, algorithm, and manager for touchpath segmentation and
 * subsegmentation.
 *
 * This class itself directly handles 'subsegmentation', as the first pass of
 * our algorithm errs on the side of segmenting too much in order to never
 * 'undersegment'.  It then delegates to other classes to recombine the
 * results as appropriate before producing completed `Segment`s.
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
   * Tracks the segment currently under construction.
   *
   * May also model an "empty" tail on the touchpath in order to track
   * which part of the path's accumulation is no longer under consideration.
   */
  private constructingSegment: ConstructingSegment;

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

  /**
   * A closure used to 'forward' generated Segments, generally to their public-facing
   * location on GesturePath.segments.
   */
  private readonly segmentForwarder: (segment: Segment) => void;

  /**
   * Denotes whether or not a first touchpath sample has been provided.
   */
  private hasStarted: boolean = false;

  // For consideration:  should REPEAT_INTERVAL, SLIDING_WINDOW_INTERVAL, and other
  // related values be defined here?
  //
  // Note:
  // - hardcoded 2 * SLIDING_WINDOW_INTERVAL:  minimum interval required for a
  //   subsegmentation attempt (to ensure sufficient observations on each side)
  // - hardcoded SLIDING_WINDOW_INTERVAL / 2:  minimum interval that must
  //   remain on each side after the sliding "optimum split point" search.
  //
  // NOTE: this will likely (eventually) be defined elsewhere, at a "higher level"
  // within this module / package.
  private segmentationConfig: SegmentationConfiguration;

  public static readonly DEFAULT_CONFIG: SegmentationConfiguration = {
    holdMinimumDuration: 100,
    holdMoveTolerance: 5
  }

  constructor(segmentationConfig: SegmentationConfiguration, segmentForwarder: (segment: Segment) => void) {
    this.steppedCumulativeStats = [];
    this.segmentForwarder = segmentForwarder;

    this.segmentationConfig = segmentationConfig;
  }

  /**
   * Appends a new coordinate to the touch path and also kick-starts a timer for replicating it
   * should neither further inputs be received nor termination of the touchpoint be indicated.
   * @param sample
   */
  public add(sample: InputSample<any>) {
    // If this is the first received input sample, generate & publish a "start" segment.
    // As ConstructingSegment is designed to work with -sequences- of samples, it's less
    // useful here... and unnecessary, as we already have all the info we need.
    if(!this.hasStarted) {
      this.hasStarted = true;

      const startSegment = new SegmentImplementation();
      startSegment.updateStats(new CumulativePathStats(sample));
      startSegment.classifyType(SegmentClass.START);
      startSegment.resolve();

      this.segmentForwarder(startSegment);
    }

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

    if(this.steppedCumulativeStats.length == 0) {
      return;
    }

    // Make sure that the final part of the touchpath is given a subsegment & then handled.
    const finalAccumulation = this.steppedCumulativeStats[this.steppedCumulativeStats.length - 1]
    let finalSubsegment: Subsegmentation = {
      stats:              finalAccumulation.deaccumulate(this.choppedStats),
      endingAccumulation: finalAccumulation,
      baseAccumulation:   this.choppedStats
    }

    // No need to check if this matches any predecessor subsegments; that already happened during
    // the last `performSubsegmentation` call.  There's no new data since then.
    // (Actually... this should already be in place now, after recent changes!)
    this.constructingSegment?.updatePendingSubsegment(finalSubsegment);
    this.finalizeSegment(); // also commits pending subsegment

    // Using the last-received sample, generate & publish an "end" segment.
    // As ConstructingSegment is designed to work with -sequences- of samples, it's less
    // useful here... and unnecessary, as we already have all the info we need.
    const endSegment = new SegmentImplementation();
    endSegment.updateStats(new CumulativePathStats(finalAccumulation.lastSample));
    endSegment.classifyType(SegmentClass.END);
    endSegment.resolve();

    this.segmentForwarder(endSegment);
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
  private observe(sample: InputSample<any>, timeDelta: number) {
    let cumulativeStats: CumulativePathStats;
    if(this.steppedCumulativeStats.length) {
      cumulativeStats = this.steppedCumulativeStats[this.steppedCumulativeStats.length-1];
    } else {
      cumulativeStats = new CumulativePathStats();
    }

    sample = {... sample, t: sample.t + timeDelta};
    const extendedStats = cumulativeStats.extend(sample);
    this.steppedCumulativeStats.push(extendedStats);

    this.performSubsegmentation();
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
    const unsegmentedForm: Subsegmentation = {
      stats: cumulativeStats.deaccumulate(this.choppedStats),
      endingAccumulation: cumulativeStats,
      baseAccumulation: this.choppedStats
    }

    // STEP 1:  Determine the range of the initial sliding time window for the most recent samples.

    if(unsegmentedDuration < this.SLIDING_WINDOW_INTERVAL * 2) {
      // If it returns false, that only makes the interval _even shorter_.
      if(!this.updateSegmentConstruction(unsegmentedForm)) {
        // Updates our internal state tracking; no infinite loop will result.
        this.performSubsegmentation();
        // return will automatically happen via fall-through.
      }
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
    let candidateSplit = SegmentationSplit.fromTrackedStats(this.steppedCumulativeStats, this.choppedStats, splitPoint);

    // STEP 2:  given the proposed time window, do we have a basis for segmentation?  And is there a better
    // candidate split point nearby?

    // We either have the conditions to trigger segmentation or just became long enough to consider it.
    // If we're only just long enough to consider it, there may be a better segmentation point to start with.
    // Now... is there a better segmentation point?  But first... how do we facilitate searching for one?

    // Start:  split-point search helper local-class.
    class SplitSearchState {
      readonly xTest: typeof SegmentationSplit.segmentationComparison.prototype;
      readonly yTest: typeof SegmentationSplit.segmentationComparison.prototype;
      readonly candidate: SegmentationSplit;

      constructor(candidate?: SegmentationSplit) {
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

    // NOTE:  During initial development, I actually had a hard rule about not even searching if segmentation
    // failed on the initially-constructed sliding window.  Some analysis may be wise here, as it'd be nice
    // to optimize away the 'need to search' if we can find hard & fast rules about when we'll never need
    // to go looking.  But... being too aggressive can cause nasty problems.

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

    let leftSplit = new SplitSearchState(SegmentationSplit.fromTrackedStats(this.steppedCumulativeStats, this.choppedStats, splitPoint-1));
    let rightCandidate: SegmentationSplit = null;
    if(splitPoint+1 < this.steppedCumulativeStats.length) {
      rightCandidate = SegmentationSplit.fromTrackedStats(this.steppedCumulativeStats, this.choppedStats, splitPoint+1);
    }
    let rightSplit = new SplitSearchState(rightCandidate);

    // Step 2a:  detect which direction gives the best improvement in segmentation potential.
    const criteria = [leftSplit.segRating, currentSplit.segRating, rightSplit.segRating];
    let sortedCriteria = [].concat(criteria).sort();

    const delta = criteria.indexOf(sortedCriteria[2])-1;  // -1 if 'left' is best, 1 if 'right' is best.

    // Step 2b: we've found the direction:  go searching!
    if(delta != 0) {
      // We can get better segmentation by shifting.  Proceed in the optimal direction.
      do {
        const nextSplitIndex = splitPoint + delta;
        if(nextSplitIndex >= this.steppedCumulativeStats.length || nextSplitIndex < 0) {
          break;
        }

        let nextCandidate = SegmentationSplit.fromTrackedStats(this.steppedCumulativeStats, this.choppedStats, splitPoint + delta);
        let nextSplit = new SplitSearchState(nextCandidate);

        // Prevent overly-short intervals / over-segmentation.
        if(nextCandidate.pre.stats.duration < this.SLIDING_WINDOW_INTERVAL / 2) {
          break;
        } else if(nextCandidate.post.stats.duration < this.SLIDING_WINDOW_INTERVAL / 2) {
          break;
        }

        // Not an improvement?  Guess we found the best spot.
        if(nextSplit.segRating <= currentSplit.segRating) {
          break;
        } else if(!nextSplit.segmentationMerited && currentSplit.segmentationMerited) {
          // Note:  this can happen if segmentation is triggered due to divergence on both axes
          // if one of them drops a threshold tier and the other fails to improve sufficiently.
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

    // Step 3:  evaluate validity of the left-hand subsegment reasonably continuing the in-construction segment.
    //          (In sort, subsegment "compatibility" with what came before.)
    //          A classic example case:  same direction, different speeds.
    //

    if(!this.updateSegmentConstruction(candidateSplit.pre)) {
      // It's quite possible that segmentation is possible in the leftover section post-reversion.
      // There is a pretty good chance that it'll instantly abort, but no guarantee.
      this.performSubsegmentation();
      return;
    }

    // First phase of segmentation:  complete!
    // Step 4:  basic bookkeeping.

    /*
      * NOTE:  this marks a very good location to call _debugLogSplitReport() if a deep-dive
      * inspection of the subsegmentation algorithm and its decisions is needed.
      */

    if(!candidateSplit.segmentationMerited) {
      if(!this.updateSegmentConstruction(unsegmentedForm)) {
        this.performSubsegmentation();
        //return will happen via fall-through here.
      }
      return;
    }

    // A successful update will ensure a valid .constructingSegment instance exists.
    this.commitSubsegmentation();

    // Step 5:  now that subsegmentation & its bookkeeping is done... process the implications.
    //          Does the right-hand (still-constructing) subsegment appear reasonable to
    //          'link' with its predecessors?  If not, finalize the predecessors.

    // As this occurs after a `commitPending`, there is currently no pending subsegment.
    // As such, this update will either:
    // - initialize a new pending subsegment for the in-construction Segment if compatible
    // - or finalize the Segment if incompatible, then use this subsegment to start a new Segment.
    // As a result, this update call will always succeed; there's no prior pending state that may be reverted to.
    this.updateSegmentConstruction(candidateSplit.post);
  }

  private finalizeSegment() {
    if(this.constructingSegment) {
      if(this.constructingSegment.subsegmentCount == 0 && !this.constructingSegment.hasPendingSubsegment) {
        throw new Error("Implementation error!");
      }
      this.constructingSegment.finalize();

      /*
        * NOTE:  if a deep-dive investigation is needed, it may prove helpful to emit each finalized
        * `constructingSegment` instance to the console here.  Like, _**precisely**_ here, immediately
        * after this multiline comment.
        *
        * From there, note that in many modern browsers, you can right-click a logged object and say
        * to "Store object as global variable", giving you console access to any such logged instances.
        *
        * `SubsegmentCompatibilityAnalyzer` is designed for ease-of-use with the members of
        * `ConstructingSegment.subsegmentations` via `SegmentationSplit`, facilitating interactive
        * inspection of which subsegments are and are not recombined into `Segment`s and why.
        */

      this.constructingSegment = null;
    }
  }

  /**
   * Updates the in-construction Segment with the specified subsegment's accumulation
   * data.  If no Segment is currently under construction, it also creates a new one.
   *
   * In the case that an update must be blocked due to a "pending commit" (from having
   * recognized the Segment while depending on the currently-constructing subsegment),
   * it will return `false` to signal that it has overriden the subsegmentation with
   * the most recently-valid prior version, committed that, and that the algorithm's
   * current analysis is no longer valid.
   * @param subsegment
   * @returns `false` if the caller should restart due to forced change of segmentation
   * state.
   */
  private updateSegmentConstruction(subsegment: Subsegmentation): boolean {
    let updateFlag: boolean;
    if(this.constructingSegment) {
      if(!this.constructingSegment.isCompatible(subsegment)) {
        // unknown:  is pending locked or not?
        updateFlag = false;
      } else {
        // Note that this call may semi-silently precommit the subsegment if this update is the
        // first to surpass the configured segment recognition timer threshold.
        //
        // Alternatively, if updating the pending subsegment results in incompatibility while
        // a pre-existing precomitted version does not, the update will fail.

        updateFlag = this.constructingSegment.updatePendingSubsegment(subsegment);
        if(updateFlag) {
          return true;
        }
      }
    }

    // If the segment under construction was registered as a 'hold' segment in the
    // middle of the subsegment, we now require the 'pending subsegment' to maintain
    // the same type.  But, reaching here means it's no longer compatible - so we
    // use the previously-registered subsegmentation here instead.
    if(updateFlag === false && this.constructingSegment.hasPrecommittedSubsegment) {
      this.commitSubsegmentation();

      // Signal our caller to refresh itself and restart segmentation for the round.
      // Kinda dirty, but it's 100% internal to this class, at least.
      return false;
    }

    this.constructingSegment?.clearPendingSubsegment();
    this.finalizeSegment();

    this.constructingSegment = new ConstructingSegment(subsegment, new SegmentClassifier(this.segmentationConfig));
    this.segmentForwarder(this.constructingSegment.pathSegment);

    return true;
  }

  private commitSubsegmentation() {
    this.constructingSegment.commitPendingSubsegment();

    // Based on what we just committed, we can find the split point that led to the subsegmentation commit:
    const splitPointAccumulation = this.constructingSegment.committedIntervalAsSubsegmentation.endingAccumulation;
    const splitPoint = this.steppedCumulativeStats.indexOf(splitPointAccumulation);

    // And given that split point, we can maintain our internal state accordingly.
    // The exact point of the split is duplicated; we remove accumulation from everything before it.
    this.choppedStats = this.steppedCumulativeStats[splitPoint-1];
    this.steppedCumulativeStats = this.steppedCumulativeStats.slice(splitPoint);
  }
}