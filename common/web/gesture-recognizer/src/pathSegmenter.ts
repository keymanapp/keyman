/// <reference path="cumulativePathStats.ts" />

namespace com.keyman.osk {
  class PotentialSegmentation {
    readonly pre:   CumulativePathStats;
    readonly post:  CumulativePathStats;
    readonly union: CumulativePathStats;
    readonly chopPoint: CumulativePathStats;

    constructor(steppedStats: CumulativePathStats[],
                choppedStats: CumulativePathStats,
                splitIndex: number) {
      this.chopPoint = steppedStats[splitIndex];
      this.pre       = steppedStats[splitIndex].deaccumulate(choppedStats);

      // Keep stats value components based on the final point of the 'pre' segment.
      const finalStats = steppedStats[steppedStats.length-1];
      this.post      = finalStats.deaccumulate(steppedStats[splitIndex-1]);
      this.union     = finalStats.deaccumulate(choppedStats);
    }

    // Note:  circular variance is defined separately from circular std. deviation.
    // They _are_ close, though.  But which is "best" to use here?
    // Even if sourced differently, the best initial guess is to compare variance
    // to variance.
    //
    // Also note that if there is no motion in either segmentation candidate, angleVariance
    // is undefined, thus NaN, mathematically.  Practically... no angle = no variance.
    public get angleVarianceRatio() {
      const preVariance  = (isNaN(this.pre.angleVariance)  ? 0 :  this.pre.angleVariance);
      const postVariance = (isNaN(this.post.angleVariance) ? 0 : this.post.angleVariance);

      return this.union.angleVariance / (preVariance + postVariance);
    }

    public get speedVarianceRatio() {
      const splitVariance = this.pre.speedVariance + this.post.speedVariance;
      return this.union.speedVariance / splitVariance;
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

    // Currently used as an in-development diagnostic assist... but these
    // directly represent actual path segments as produced by the prototype
    // algorithm.  Just... the stats analysis of the path segment, without
    // obvious / public members to relevant coordinates.
    private _protoSegments: CumulativePathStats[] = [];

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

      let intervalStats = this.steppedCumulativeStats[this.steppedCumulativeStats.length-1];
      if(this.choppedStats) {
        intervalStats = intervalStats.deaccumulate(this.choppedStats);
      }
      this._protoSegments.push(intervalStats);

      // FIXME:  temporary statement to facilitate exploration, experimentation, & debugging
      console.log(this._protoSegments);
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

      console.log("Angle variance ratio: " + candidateSplit.angleVarianceRatio);
      console.log("Speed variance ratio: " + candidateSplit.speedVarianceRatio);

      console.log("Combined: ");
      console.log(candidateSplit.union.toJSON());
      console.log(candidateSplit.union);
      console.log("Pre: ")
      console.log(candidateSplit.pre.toJSON());
      console.log(candidateSplit.pre);
      console.log("Post: ");
      console.log(candidateSplit.post.toJSON());
      console.log(candidateSplit.post);
      console.log("Angle variance ratio: " + candidateSplit.angleVarianceRatio);
      console.log("Speed variance ratio: " + candidateSplit.speedVarianceRatio);

      console.log();

      console.log("Prototype segment: ");
      console.log(candidateSplit.pre);

      console.log("------------------------------------------------------------------");
        // END:  DO NOT RELEASE.
    }

    private attemptSegmentation() {
      const extendedStats = this.steppedCumulativeStats[this.steppedCumulativeStats.length - 1];
      let preWindowEnd = 0;
      // Do not consider the just-added `extendedStats` entry.
      //
      // Note:  even if we do reconsider the segmentation point... I don't think we
      // should reconsider anything earlier than where this marker falls.
      //
      // If we didn't segment earlier before, why would we suddenly do so now?
      for(let i = this.steppedCumulativeStats.length-2; i >=0; i--) {
        if(this.steppedCumulativeStats[i].lastTimestamp + this.SLIDING_WINDOW_INTERVAL < extendedStats.lastTimestamp) {
          preWindowEnd = i;
          break;
        }
      }

      // Do not consider segmenting before at least two samples exist before the current
      // sliding time window.  (A minimum of two samples are needed for 'over time'
      // properties to have a chance at becoming 'defined'.)
      if(preWindowEnd > 0) {
        // We split the cumulative stats on a specific point, which then resides on the edge
        // of both of the resulting intervals.
        let candidateSplit = new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, preWindowEnd+1);

        // Run a comparison on various stats of the two.
        // Oh.  And _there's_ why - if we didn't check before b/c minimum time req't.
        if(candidateSplit.pre.duration * 1000 >= this.SLIDING_WINDOW_INTERVAL) { // sec vs millisec.
          let performSegmentation = false;

          /*
           * Okay, so this isn't probably quite the most statistically well-founded approach, but...
           *
           * A "variance ratio" of 1 indicates something of a break-even point; exceeding that threshold
           * means that the variations in value seen between the two potential segments are better
           * explained as being from two separate segments than from a single segment.  (Loosely speaking;
           * it'd take some effort to cement the statistical basis here; this is more 'inspired by' what
           * the values represent.)
           *
           * Of course... when it comes to speed, acceleration and such are factors.  It'd be all
           * too easy to split the slow and fast parts of an accelerating linear motion as two separate
           * pieces.  Requiring a higher degree of separation alleviates this - hence, the `/2` in the
           * condition below.  (That divisor's not the most statistically-based thing to do, but it
           * works well here.)
           *
           * So to reach the threshold set below, these three conditions will work:
           * - strong difference in angle between the segment candidates
           * - moderate difference in both angle and speed between the segment candidates (equal levels)
           * - very strong difference in speed between the segment candidates.
           */
          if(candidateSplit.angleVarianceRatio + candidateSplit.speedVarianceRatio / 2 > 1.5) {
            performSegmentation = true;
          }

          // Hmm.  Perhaps this should only serve as the "okay, let's segment" trigger... to then
          // find the BEST segmentation.

          // FIXME: DO NOT RELEASE.
          // This is exploratory / diagnostic code assisting development of the path segmentation
          // algorithm.
          if(performSegmentation) {
            this._debugLogSegmentationReport(candidateSplit);
          } else {
            console.log("Angle variance ratio: " + candidateSplit.angleVarianceRatio);
            console.log("Speed variance ratio: " + candidateSplit.speedVarianceRatio);
          }
          // END:  DO NOT RELEASE.

          if(performSegmentation) {
            this.steppedCumulativeStats = this.steppedCumulativeStats.slice(preWindowEnd+1);  // DO release this line.

            this._protoSegments.push(candidateSplit.pre);
            this.choppedStats = candidateSplit.chopPoint;
          }
        }
      }
    }
  }
}