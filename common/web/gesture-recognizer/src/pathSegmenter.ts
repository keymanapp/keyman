/// <reference path="cumulativePathStats.ts" />

namespace com.keyman.osk {
  class PotentialSegmentation {
    public static readonly SPLIT_CRITERION_THRESHOLD = 1.5;

    readonly pre:   CumulativePathStats;
    readonly post:  CumulativePathStats;
    readonly union: CumulativePathStats;
    readonly chopPoint: CumulativePathStats;
    readonly baseChop:  CumulativePathStats;

    constructor(steppedStats: CumulativePathStats[],
                choppedStats: CumulativePathStats,
                splitIndex: number) {
      this.baseChop  = choppedStats;
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

    public get splitCriterion() {
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

      const angleComponent = isNaN(this.angleVarianceRatio) ? 0 : this.angleVarianceRatio;
      const speedComponent = isNaN(this.speedVarianceRatio) ? 0 : (this.speedVarianceRatio / 2);
      return angleComponent + speedComponent;
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

      this.filterSubsegmentation(finalization, true); // forces out the final segment.
                                                      // Hacky, but "enough" for now.

      // FIXME:  temporary statement to facilitate exploration, experimentation, & debugging
      console.log(this._protoSegments);
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
      const cumulativeStats = this.steppedCumulativeStats[this.steppedCumulativeStats.length - 1];
      const unsegmentedDuration = cumulativeStats.lastTimestamp - this.steppedCumulativeStats[0].lastTimestamp;

      if(unsegmentedDuration < this.SLIDING_WINDOW_INTERVAL * 2) {
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

      // Run a comparison on various stats of the two.
      // Oh.  And _there's_ why - if we didn't check before b/c minimum time req't.
      let performSegmentation = false;

      const initialSplitCriterion = candidateSplit.splitCriterion;
      if(initialSplitCriterion > PotentialSegmentation.SPLIT_CRITERION_THRESHOLD) {
        performSegmentation = true;
      }

      if(!performSegmentation) {
        // // Debug logging statements:
        console.log("Angle variance ratio: " + candidateSplit.angleVarianceRatio);
        console.log("Speed variance ratio: " + candidateSplit.speedVarianceRatio);
        return;
      }

      // We've met the conditions to trigger segmentation.  Now... is there a better segmentation point?
      //

      let currentSplitCriterion = initialSplitCriterion;
      let leftCandidate = new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, splitPoint-1);
      let rightCandidate = new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, splitPoint+1);

      const criteria = [leftCandidate.splitCriterion, candidateSplit.splitCriterion, rightCandidate.splitCriterion];
      let sortedCriteria = [...criteria].sort();

      const delta = criteria.indexOf(sortedCriteria[2])-1;  // -1 if 'left' is best, 1 if 'right' is best.

      if(delta != 0) {
        // We can get better segmentation by shifting.  Proceed in the optimal direction.
        do {
          let nextCandidate = new PotentialSegmentation(this.steppedCumulativeStats, this.choppedStats, splitPoint + delta);
          let nextSplitCriterion = nextCandidate.splitCriterion;

          // Prevent overly-short intervals / over-segmentation.
          if(nextCandidate.pre.duration * 1000 < this.SLIDING_WINDOW_INTERVAL / 2) {
            break;
          } else if(nextCandidate.post.duration * 1000 < this.SLIDING_WINDOW_INTERVAL / 2) {
            break;
          }

          // Not an improvement?  Guess we found the best spot.
          if(nextSplitCriterion < currentSplitCriterion) {
            break;
          } else {
            splitPoint += delta;
            currentSplitCriterion = nextSplitCriterion;
            candidateSplit = nextCandidate;
          }
          // If we found a new best segmentation point, we then ask if we can get even better by shifting further.
        } while(true);
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

      this.filterSubsegmentation(candidateSplit);
    }

    // NOTE:  This function, as well as code called by it, are still very much still in prototyping.
    private filterSubsegmentation(subsegmentation: PotentialSegmentation, force?: boolean) {
      force = !!force;
      const mergeSubsegmentations = function(array: PotentialSegmentation[]) {
        if(array.length == 1) {
          return array[0].pre; // It's pre-calculated, so just use it.
        } else {
          const finalSubsegmentation = array[array.length-1];
          return finalSubsegmentation.chopPoint.deaccumulate(array[0].baseChop);
        }
      }

      if(this.lingeringSubsegmentations.length) {
        // First:  check if the newly-finished subsegment should be merged with the lingering ones.
        const lastSubsegment = this.lingeringSubsegmentations[this.lingeringSubsegmentations.length-1];

        if(!PathSegmenter.shouldMergeSubsegments(lastSubsegment.pre, subsegmentation.pre)) {
          // Emit as separate subsegment.
          const finishedSegment = mergeSubsegmentations(this.lingeringSubsegmentations);
          this._protoSegments.push(finishedSegment);
          this.lingeringSubsegmentations = [];

          // IN DEVELOPMENT:  does this happen much?  If so... maybe we need intervening checks to pre-filter even
          // if not segmenting.
          console.warn("Did not merge a lingering subsegment with the incoming one!");
        } else {
          console.log("Will merge in old subsegments!");
        }
      }

      if(!force && PathSegmenter.shouldMergeSubsegments(subsegmentation.pre, subsegmentation.post)) {
        this.lingeringSubsegmentations.push(subsegmentation);
      } else {
        // Merge all as a completed segment!
        const finishedSegment = mergeSubsegmentations([...this.lingeringSubsegmentations, subsegmentation]);
        this._protoSegments.push(finishedSegment);
      }
    }

    private static shouldMergeSubsegments(segment1: CumulativePathStats, segment2: CumulativePathStats): boolean {
      // Known case #1:
      //   Almost identical direction, but heavy speed variance.
      //   Speed's still high enough to not be a 'wait'.
      // Known case #2:
      //   Low-speed pivot; angle change caught on very low velocity for both subsegments.
      return false;
    }
  }
}