/// <reference path="pathSegmentStats.ts" />

namespace com.keyman.osk {
  export class PathSegmenter {
    private readonly REPEAT_INTERVAL = 33;
    private readonly SLIDING_WINDOW_INTERVAL = 50;

    // May be best to keep an array of these, one per sample.
    // Can then diff the stats to determine better cut-offs.
    // Though... the whole arc-dist aspect will need a mite more help.
    // - chopping off from the end:  ez-pz.  Raw diff is great.
    //   - or, well, just use the appropriate one from mid-way.
    // - chopping off from the beginning:  need an extra sample reference.
    //   - .nextSample.
    //
    // The FINAL version, once resolved, may be published.
    // But until resolved, we probably want to keep an array.
    private _stats: PathSegmentStats[];

    // Currently used as an in-development diagnostic assist... but these
    // directly represent actual path segments as produced by the prototype
    // algorithm.  Just... the stats analysis of the path segment, without
    // obvious / public members to relevant coordinates.
    private _protoSegments: PathSegmentStats[] = [];

    private repeatTimer: number | NodeJS.Timeout;
    private repeatTimestamp: number;

    private choppedStats: PathSegmentStats = null;

    constructor() {
      this._stats = [];
    }

    public get stats(): readonly PathSegmentStats[] {
      return this._stats;
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

      let intervalStats = this.stats[this.stats.length-1];
      if(this.choppedStats) {
        intervalStats = intervalStats.withoutPrefixSubset(this.choppedStats);
      }
      this._protoSegments.push(intervalStats);
    }

    private observe(sample: InputSample, timeDelta: number) {
      let baseStats: PathSegmentStats;
      if(this.stats.length) {
        baseStats = this.stats[this.stats.length-1];
      } else {
        baseStats = new PathSegmentStats();
      }

      sample = {... sample};
      sample.t += timeDelta;
      const extendedStats = baseStats.unionWith(sample);
      this._stats.push(extendedStats);

      let preWindowEnd = 0;
      // Do not consider the just-added `extendedStats` entry.
      for(let i = this.stats.length-2; i >=0 ; i--) {
        if(this.stats[i].lastTimestamp + this.SLIDING_WINDOW_INTERVAL < sample.t) {
          preWindowEnd = i;
          break;
        }
      }

      // Do not consider segmenting before at least two samples exist before the current
      // sliding time window.  (A minimum of two samples are needed for 'over time'
      // properties to have a chance at becoming 'defined'.)
      //console.log(sample);
      if(preWindowEnd > 0) {
        const cumulativePreCandidate = this.stats[preWindowEnd+1];
        let preCandidate = cumulativePreCandidate;
        if(this.choppedStats) {
          preCandidate = preCandidate.withoutPrefixSubset(this.choppedStats);
        }
        let postCandidate = extendedStats.withoutPrefixSubset(this.stats[preWindowEnd]);

        let combined = extendedStats;
        if(this.choppedStats) {
          combined = combined.withoutPrefixSubset(this.choppedStats);
        }

        // Run a comparison on various stats of the two.
        if(preCandidate.duration * 1000 >= this.SLIDING_WINDOW_INTERVAL) { // sec vs millisec.
          let performSegmentation = false;

          let angleSplitVariance = preCandidate.angleVariance + postCandidate.angleVariance;
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

            this._stats = this._stats.slice(preWindowEnd+1);  // DO release this line.
            console.log("Dropped samples: " + (preWindowEnd+1));
            console.log("Remaining samples: " + this._stats.length);

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