namespace com.keyman.osk {
  /**
   * This class is responsible for managing the construction of public-facing Segments while keeping
   * all the internal parts... internal.  As such, it includes state management for parts of
   * PathSegmenter's operations.
   */
  export class ConstructingSegment {
    /**
     * Marks previously-accumulated stats on the touchpath for the portion that precedes
     * the in-construction Segment.
     */
    readonly baseAccumulation: CumulativePathStats;

    /**
     * Notes all subsegments identified as part of the overall segment being constructed.
     */
    private subsegmentations: Subsegmentation[] = [];

    /**
     * Marks a potential follow-up subsegment.  This portion is not automatically
     * committed during finalization.
     */
    private pendingSubsegmentation: Subsegmentation = null;

    constructor(preIntervalAccumulation: CumulativePathStats, initialPendingSubsegment: Subsegmentation) {
      this.baseAccumulation = preIntervalAccumulation;

      this.subsegmentations = [];
      this.pendingSubsegmentation = initialPendingSubsegment;
    }

    /**
     * Constructs a stats object for the interval ending with the specified accumulation
     * state and beginning at the start of the in-construction Segment.
     * @param accumulation
     * @returns
     */
    public buildIntervalFromBase(accumulation: CumulativePathStats) {
      return accumulation.deaccumulate(this.baseAccumulation);
    }

    /**
     * Returns the stats accumulation covering all currently-committed subsegments.
     * May be `null` if there are no committed subsegments.
     */
    public get committedInterval(): CumulativePathStats {
      return this.committedIntervalAsSubsegmentation.stats;
    }

    /**
     * Returns a merged form of all currently-committed subsegments in
     * Subsegmentation form.
     */
    public get committedIntervalAsSubsegmentation(): Subsegmentation {
      if(this.subsegmentations.length == 0) {
        return {
          stats: null,
          endingAccumulation: null,
          baseAccumulation: this.baseAccumulation
        };
      } else {
        const tailSub = this.subsegmentations[this.subsegmentations.length - 1];
        const tail = tailSub.endingAccumulation;

        return {
          stats: this.buildIntervalFromBase(tail),
          endingAccumulation: tail,
          baseAccumulation: this.baseAccumulation
        };
      }
    }

    public clearPendingPortion() {
      this.pendingSubsegmentation = null;
    }

    /**
     * Checks if the committed portion of the constructing segment are "compatible" with
     * a potential following subsegment.  Appending said subsegment may not change the
     * 'type' of segment being constructed or certain properties of it.
     *
     * @returns `true` if compatible, `false` if not.
     */
    public isCompatible(subsegment: Subsegmentation): boolean {
      if(this.subsegmentations.length == 0) {
        return true;
      }

      const committed = this.committedIntervalAsSubsegmentation;

      const preStats = committed.stats;
      const postStats = subsegment.stats;

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

      // TODO:  where to move logging, if doing so
      console.log("Verifying linkage to pending merges: ");
      console.log(this.subsegmentations);
      console.log("verification check:");
      console.log(this.committedIntervalAsSubsegmentation);
      // TODO:  end of "where to move"

      // .mean('v') < 0.08:  the mean speed (taken timestamp to timestamp) does not exceed 0.08px/millisec.
      if(preStats.mean('v') < 0.08 && postStats.mean('v') > 0.08) {
        console.log("desegmentation exception");
        return false;
      }
      if(preStats.mean('v') > 0.08 && postStats.mean('v') < 0.08) {
        console.log("desegmentation exception");
        return false;
      }

      if(preStats.cardinalDirection == postStats.cardinalDirection &&
        // TODO:  base this on 'detected as move', not 'not a confirmed hold'.
        preStats.mean('v') > 0.08 && postStats.mean('v') > 0.08) {
        // Same cardinal direction + recognized move?

        console.log("subsegments are both moving sufficiently quickly in the same direction:  will remerge");
        return true;
      }

      const segmentationSplit = new SegmentationSplit(committed, subsegment);

      // TODO:  remove for production / feature-branch merge
      segmentationSplit._debugLogAlignmentReport();

      return segmentationSplit.mergeMerited;
    }

    /**
     * Call this to denote the current state of an in-construction subsegment that **will** be included
     * in the final Segment once completed if it does not diverge.
     *
     * If the pending subsegment appears to belong to the current Segment, it contains valuable info
     * about the state of the as-of-yet unresolved Segment, including the most recent location
     * of the corresponding touchpoint.
     * @param subsegmentation
     */
    public updatePendingSubsegment(subsegmentation: Subsegmentation) {
      // TODO:  neat updates & events.
      this.pendingSubsegmentation = subsegmentation;
    }

    /**
     * Call to clear a previously-tracked in-construction subsegment that has diverged and is no
     * longer compatible.
     */
    public clearPendingSubsegment() {
      // Probably does not need to trigger an event; if called, there's a new, follow-up segment
      // coming that will maintain the current coordinate with its events instead.  Assuming
      // the need to rely on Segment events for that; even that's better off handled with path.coords
      // events instead.
      this.pendingSubsegmentation = null;
    }

    public commitPendingSubsegment() {
      if(this.pendingSubsegmentation) {
        this.subsegmentations.push(this.pendingSubsegmentation);
        this.pendingSubsegmentation = null;
      } else {
        throw "Illegal state - `commitPendingPortion` should never be called with nothing pending.";
      }
    }

    public finalize() {
      if(this.pendingSubsegmentation) {
        this.commitPendingSubsegment();
      }

      // TODO: fully 'recognize' the Segment.
    }
  }
}