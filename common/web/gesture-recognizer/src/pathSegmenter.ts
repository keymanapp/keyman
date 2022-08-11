/// <reference path="cumulativePathStats.ts" />

namespace com.keyman.osk {
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

      let preWindowEnd = 0;
      // Do not consider the just-added `extendedStats` entry.
      for(let i = this.steppedCumulativeStats.length-2; i >=0 ; i--) {
        if(this.steppedCumulativeStats[i].lastTimestamp + this.SLIDING_WINDOW_INTERVAL < sample.t) {
          preWindowEnd = i;
          break;
        }
      }

      // Do not consider segmenting before at least two samples exist before the current
      // sliding time window.  (A minimum of two samples are needed for 'over time'
      // properties to have a chance at becoming 'defined'.)
      //console.log(sample);
      if(preWindowEnd > 0) {
        // We split the cumulative stats on a specific point, which then resides on the edge
        // of both of the resulting intervals.
        const cumulativePreCandidate = this.steppedCumulativeStats[preWindowEnd+1];
        let preCandidate = cumulativePreCandidate;
        if(this.choppedStats) {
          preCandidate = preCandidate.deaccumulate(this.choppedStats);
        }
        let postCandidate = extendedStats.deaccumulate(this.steppedCumulativeStats[preWindowEnd]);

        let combined = extendedStats;
        if(this.choppedStats) {
          combined = combined.deaccumulate(this.choppedStats);
        }

        // Run a comparison on various stats of the two.
        if(preCandidate.duration * 1000 >= this.SLIDING_WINDOW_INTERVAL) { // sec vs millisec.
          let performSegmentation = false;

          // Note:  circular variance is defined separately from circular std. deviation.
          // They _are_ close, though.  But which is "best" to use here?
          // Even if sourced differently, the best initial guess is to compare variance
          // to variance.
          //
          // Also note that if there is no motion in either segmentation candidate, angleVariance
          // is undefined, thus NaN, mathematically.  Practically... no angle = no variance.
          let angleSplitVariance = (isNaN(preCandidate.angleVariance)  ? 0 :  preCandidate.angleVariance) +
                                   (isNaN(postCandidate.angleVariance) ? 0 : postCandidate.angleVariance);
          let angleVarianceRatio = combined.angleVariance / angleSplitVariance;

          const speedSplitVariance = preCandidate.speedVariance + postCandidate.speedVariance;
          const speedVarianceRatio = combined.speedVariance / speedSplitVariance;

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
          if(angleVarianceRatio + speedVarianceRatio / 2 > 1.5) {
            performSegmentation = true;
          }

          // Hmm.  Perhaps this should only serve as the "okay, let's segment" trigger... to then
          // find the BEST segmentation.

          // FIXME: DO NOT RELEASE.
          // This is exploratory / diagnostic code assisting development of the path segmentation
          // algorithm.
          if(performSegmentation) {
            console.log("------------------------------------------------------------------");
          }

          console.log("Angle variance ratio: " + angleVarianceRatio);
          console.log("Speed variance ratio: " + speedVarianceRatio);

          if(performSegmentation) {
            console.log("Combined: ");
            console.log(combined.toJSON());
            console.log(combined);
            console.log("Pre: ")
            console.log(preCandidate.toJSON());
            console.log(preCandidate);
            console.log("Post: ");
            console.log(postCandidate.toJSON());
            console.log(postCandidate);
            console.log("Angle variance ratio: " + angleVarianceRatio);
            console.log("Speed variance ratio: " + speedVarianceRatio);

            console.log();

            this.steppedCumulativeStats = this.steppedCumulativeStats.slice(preWindowEnd+1);  // DO release this line.
            console.log("Dropped samples: " + (preWindowEnd+1));
            console.log("Remaining samples: " + this.steppedCumulativeStats.length);

            console.log("Prototype segment: ");
            console.log(preCandidate);

            console.log("------------------------------------------------------------------");
            // END:  DO NOT RELEASE.

            this._protoSegments.push(preCandidate);
            this.choppedStats = cumulativePreCandidate;
          }
        }
      }
    }
  }
}