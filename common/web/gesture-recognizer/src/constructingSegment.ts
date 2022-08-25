namespace com.keyman.osk {
  class CompatibilityAnalyzer {
    readonly classifier: SegmentClassifier;
    private split: SegmentationSplit;

    constructor(subsegmentationSplit: SegmentationSplit) {
      this.split = subsegmentationSplit;

      this.classifier = new SegmentClassifier();
    }

    private get preStats() {
      return this.split.pre.stats;
    }

    private get postStats() {
      return this.split.post.stats;
    }

    private get unionStats() {
      return this.split.union;
    }

    get directionCompatible(): boolean {
      if(!this.preStats ||this.regressionDirectionCompatible) {
        return true;
      } else if(this.cardinalDirectionCompatible && this.preStats.mean('v') > 0.08 && this.postStats.mean('v') > 0.08) {
          return true;
      } else {
        return false;
      }
    }

    get cardinalDirectionCompatible(): boolean {
      return !this.preStats ||this.preStats.cardinalDirection == this.postStats.cardinalDirection;
    }

    get regressionDirectionCompatible(): boolean {
      return !this.preStats || this.split.mergeMerited;
    }

    get classificationIfCompatible(): SegmentClass {
      // Get the baseline subsegment classification for each subsegment.
      let leftClass  = this.classifier.classifySubsegment(this.preStats);
      let rightClass = this.classifier.classifySubsegment(this.postStats);
      let unionClass = this.classifier.classifySubsegment(this.unionStats);

      // Choose the first non-null one as a fallback, then apply it.
      let fallbackClass = leftClass || rightClass || unionClass;

      leftClass  = leftClass || fallbackClass;
      rightClass = rightClass || fallbackClass;
      unionClass = unionClass || fallbackClass;

      // If all classes (post-fallback) match, that's the class if compatible.
      if(leftClass == rightClass && leftClass == unionClass) {
        return fallbackClass;  // can technically still be null.
      } else {
        return undefined;
      }
    }

    get isCompatible(): boolean {
      const commonClass = this.classificationIfCompatible;

      // If two adjacent hold subsegments also make a hold when combined...
      // just merge the two holds & call 'em compatible.
      if(!this.preStats || commonClass == 'hold') {
        return true;
      } else if(commonClass === undefined) {
        return false;
      }

      // if(null || 'move'):  as 'null' looks like a not-quite-there-yet 'move',
      // we treat it as such here.  Such subsegments are only compatible if
      // their directions are compatible.
      return this.directionCompatible;
    }
  }

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

    constructor(initialPendingSubsegment: Subsegmentation) {
      // Note:  may be null!  Occurs for the first processed subsegment.
      this.baseAccumulation = initialPendingSubsegment.baseAccumulation;

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
    public isCompatible(subsegmentation: Subsegmentation): boolean {
      return this.analyzeCompatibility(subsegmentation).isCompatible;
    }

    public analyzeCompatibility(subsegmentation: Subsegmentation): CompatibilityAnalyzer {
      const committed = this.committedIntervalAsSubsegmentation;
      const segmentationSplit = new SegmentationSplit(committed, subsegmentation);

      return new CompatibilityAnalyzer(segmentationSplit);
    }

    // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    // TODO:  remake into a function on the analyzer object.
    public debugLogCompabilityReport(subsegmentation: Subsegmentation) {
      const committed = this.committedIntervalAsSubsegmentation;
      const segmentationSplit = new SegmentationSplit(committed, subsegmentation);

      const analyzer = new CompatibilityAnalyzer(segmentationSplit);

      if(this.subsegmentations.length == 0) {
        console.log("No prior subsegments - thus, no compatibility conflicts are possible.");
      }

      // TODO:  Possible 'polished way' to provide feedback:
      //        Build an object with annotations for each condition under consideration.
      //        Object's then easily loggable & is even returnable.
      //        Split:  isCompatible vs analyzeCompatibility

      console.log("Verifying linkage to pending merges: ");
      console.log(this.subsegmentations);
      console.log("verification check:");
      console.log(this.committedIntervalAsSubsegmentation);

      // TODO: log report detalis

      segmentationSplit._debugLogAlignmentReport();
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