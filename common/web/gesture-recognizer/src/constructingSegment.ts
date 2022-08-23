namespace com.keyman.osk {
  /**
   * This class is responsible for managing the construction of public-facing Segments while keeping
   * all the internal parts... internal.  As such, it includes state management for parts of
   * PathSegmenter's operations.
   */
  export class ConstructingSegment {
    /**
     * Marks the accumulated stats on the touchpath that precede the in-construction Segment.
     */
    private readonly preIntervalAccumulation: CumulativePathStats;

    /**
     * Notes all subsegments identified as part of the overall segment being constructed.
     */
    subsegmentations: Subsegmentation[] = [];  // TODO:  `private`?

    /**
     * Marks a potential follow-up subsegment.  This portion is not automatically
     * committed during finalization.
     */
    private pendingSubsegmentation: Subsegmentation = null;

    constructor(preIntervalAccumulation: CumulativePathStats,) {
      this.preIntervalAccumulation = preIntervalAccumulation;

      this.subsegmentations = [];
    }

    /**
     * Constructs a stats object for the interval ending with the specified accumulation
     * state and beginning at the start of the in-construction Segment.
     * @param accumulation
     * @returns
     */
    public buildIntervalFromBase(accumulation: CumulativePathStats) {
      return accumulation.deaccumulate(this.preIntervalAccumulation);
    }

    /**
     * Returns the stats accumulation covering all currently-committed subsegments.
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
          endingAccumulation: null
        };
      } else {
        const tailSub = this.subsegmentations[this.subsegmentations.length - 1];
        const tail = tailSub.endingAccumulation;

        return {
          stats: this.buildIntervalFromBase(tail),
          endingAccumulation: tail
        };
      }
    }

    public clearPendingPortion() {
      this.pendingSubsegmentation = null;
    }

    /**
     * Denotes the currently in-construction subsegment that **may** be included in the
     * Segment.
     *
     * If the pending subsegment appears to belong to the current Segment, it contains valuable info
     * about the state of the as-of-yet unresolved Segment, including the most recent location
     * of the corresponding touchpoint.
     * @param subsegmentation
     */
    public updatePendingPortion(subsegmentation: Subsegmentation) {
      // Would be awfully convenient if setting the value here auto-triggered an update of the
      // public-facing Segment.
      //
      // But, CURRENTLY, it's set here first, then a check for compatibility is run.
      this.pendingSubsegmentation = subsegmentation;
    }

    public commitPendingPortion() {
      if(this.pendingSubsegmentation) {
        this.subsegmentations.push(this.pendingSubsegmentation);
        this.clearPendingPortion();
      } else {
        throw "Illegal state - `commitPendingPortion` should never be called with nothing pending.";
      }
    }

    public finalize() {
      // does NOT include an uncommited pending portion!
      this.pendingSubsegmentation = null;
    }

    // Obviously, needs better specification.  'hold' / 'move' / 'unknown'?
    public type(): string {
      return undefined;
    }

    public get isEmpty(): boolean {
      if(this.subsegmentations.length != 0) {
        return false;
      } else {
        return this.pendingSubsegmentation == null
      }
    }
  }
}