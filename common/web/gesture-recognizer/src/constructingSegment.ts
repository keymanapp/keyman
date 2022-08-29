namespace com.keyman.osk {
  // TODO NOTE:
  //
  // Maintain an access to the segment class and just raw-update its stats.  Don't go full encapsulation.
  // BUT have the object inherit from EventEmitter & support fields for the Promises.
  //
  // 1. The consumer needs a consistent reference for the object & event listening
  // 2. That object doesn't HAVE to be hyper-encapsulated.  Just a convenient "hook" point.
  // 3. We can maintain the Promises for recognized, resolved, etc HERE, making a copy of their
  //    references for the object.

  //
  // Or we can subclass a parent with protected funcs & make 'em public.  We 'publish' A, even implement
  // most of the stuff in there... but we instantiate B so that we can do maintenance.

  class CompatibilityAnalyzer {
    readonly classifier: SegmentClassifier;
    private split: SegmentationSplit;

    constructor(subsegmentationSplit: SegmentationSplit, classifier: SegmentClassifier) {
      this.split = subsegmentationSplit;

      this.classifier = classifier;
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
      if(!this.preStats || this.regressionDirectionCompatible) {
        return true;
      }

      let preMatchesMove  = this.classifier.classifySubsegment(this.preStats)  != SegmentClass.HOLD;
      let postMatchesMove = this.classifier.classifySubsegment(this.postStats) != SegmentClass.HOLD;

      if(preMatchesMove != postMatchesMove) {
        // If one subsegment appears to be a 'hold' while the other does not, well... holds
        // don't (practically) have a direction, which mismatches with the 'move' that does
        // have a direction.
        return false;
      } else if(!preMatchesMove) {
        // If both are 'hold' subsegments, both should be treated as directionless.
        return true;
      } else {
        // If both are 'move' / 'move'-like subsegments, only merge if their directional
        // classification falls into the same 'direction bucket'.
        return this.cardinalDirectionCompatible;
      }
    }

    get cardinalDirectionCompatible(): boolean {
      return !this.preStats || this.preStats.cardinalDirection == this.postStats.cardinalDirection;
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
    readonly classifier: SegmentClassifier;

    /**
     * Marks previously-accumulated stats on the touchpath for the portion that precedes
     * the in-construction Segment.
     */
    readonly baseAccumulation: CumulativePathStats;

    /**
     * Notes all subsegments identified as part of the overall segment being constructed.
     */
    // TODO:  Re-private this!
    /*private*/ subsegmentations: Subsegmentation[] = [];

    /**
     * Marks a potential follow-up subsegment.  This portion is not automatically
     * committed during finalization.
     */
    private pendingSubsegmentation: Subsegmentation = null;

    /**
     * A flag that is only set once the in-construction segment is first recognized as a 'wait'.
     * If set, this indicates that the current - and _only_ the current - `pendingSubsegmentation`
     * may only be replaced by an update that would still be compatible if the field's current
     * value were already committed.  (Goal: prevent the 'locked' pending section from becoming
     * not-committed in to the 'wait' role that triggered the 'lock'.)
     */
    private _pendingLocked: boolean = false;

    private _pathSegment: SegmentImplementation;

    constructor(initialPendingSubsegment: Subsegmentation, classifier: SegmentClassifier) {
      // Note:  may be null!  Occurs for the first processed subsegment.
      this.baseAccumulation = initialPendingSubsegment.baseAccumulation;
      this.classifier = classifier;

      this.subsegmentations = [];
      this.updatePendingSubsegment(initialPendingSubsegment);
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

    /**
     * Checks if the committed portion of the constructing segment are "compatible" with
     * a potential following subsegment.  Appending said subsegment may not change the
     * 'type' of segment being constructed or certain properties of it.
     *
     * @returns `true` if compatible, `false` if not.
     */
    public isCompatible(subsegmentation: Subsegmentation): boolean {
      const pendingAnalyzer = this.analyzeCompatibility(subsegmentation);
      const classification = pendingAnalyzer.classificationIfCompatible;

      // TODO:  does not sufficiently check against the pending section yet for the forced-break scenario!

      // this.publicSegment.type, if set, will match the previous round's .classificationIfCompatible.
      if(this.hasPrecommittedSubsegment && this.pathSegment.type && this.pathSegment.type != classification) {
        // TODO:  SPECIAL CASE:  'forced break'.
        return false;
      } else {
        return pendingAnalyzer.isCompatible;
      }
    }

    public analyzeCompatibility(subsegmentation: Subsegmentation): CompatibilityAnalyzer {
      const committed = this.committedIntervalAsSubsegmentation;
      const segmentationSplit = new SegmentationSplit(committed, subsegmentation);

      return new CompatibilityAnalyzer(segmentationSplit, this.classifier);
    }

    private wholeSegmentation(subsegmentation: Subsegmentation): Subsegmentation {
      const whole: Subsegmentation = {
        stats: subsegmentation.endingAccumulation.deaccumulate(this.baseAccumulation),
        baseAccumulation: this.baseAccumulation,
        endingAccumulation: subsegmentation.endingAccumulation
      };

      return whole;
    }

    // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    // TODO:  remake into a function on the analyzer object.
    public debugLogCompabilityReport(subsegmentation: Subsegmentation) {
      const committed = this.committedIntervalAsSubsegmentation;
      const segmentationSplit = new SegmentationSplit(committed, subsegmentation);

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

    public get pendingSubsegment(): Subsegmentation {
      return this.pendingSubsegmentation;
    }

    /**
     * A subsegment is pre-committed when it is required to successfully classify the in-construction
     * segment after reaching the configured time threshold for segment recognition - that is,
     * when previously-committed subsegments are insufficient to support the classification of a
     * 'recognized' Segment.
     *
     * In such a case, the precommitted portion must be maintained.  In the event that extending the
     * subsegment (on future updates) would render it incompatible, the subsegment must be forceably
     * segmented in order to commit the precommitted portion.
     */
    public get hasPrecommittedSubsegment() {
      return this._pendingLocked;
    }

    private set hasPrecommittedSubsegment(flag: boolean) {
      this._pendingLocked = flag;
    }

    /**
     * Call this to denote the current state of an in-construction subsegment that **will** be included
     * in the final Segment once completed if the pending subsegment does not diverge in a later update.
     *
     * If the pending subsegment appears to belong to the current Segment, it contains valuable info
     * about the state of the as-of-yet unresolved Segment, including the most recent location
     * of the corresponding touchpoint.
     *
     * @param subsegmentation
     * @returns `true` unless the subsegment
     */
    public updatePendingSubsegment(subsegmentation: Subsegmentation): boolean {
      const isFirstUpdate = !this.pendingSubsegmentation && this.subsegmentations.length == 0;
      const fullStatsWithIncoming = this.wholeSegmentation(subsegmentation).stats;

      // Do we have a locked pending section?  If so, check to be sure that an update won't break things.
      if(this.hasPrecommittedSubsegment) {
        const classification = this.classifier.classifySegment(fullStatsWithIncoming);

        // If the classification would change after updating the pending subsegment, block the update &
        // report update failure.
        if(this.pathSegment.type != classification) {
          return false;
        }
      }

      this.pendingSubsegmentation = subsegmentation;

      if(isFirstUpdate) {
        this.pathSegment = new SegmentImplementation();
      }

      this.pathSegment.updateStats(fullStatsWithIncoming);

      // Check the length of time that's elapsed.  If we've surpassed the recognition threshold,
      // it's time to commit to classifying the in-construction Segment.
      const alreadyElapsed = fullStatsWithIncoming.duration;
      const recognitionWaitTime = this.classifier.config.holdMinimumDuration - alreadyElapsed;

      // `undefined` if and only if still unrecognized.
      if(recognitionWaitTime <= 0 && this._pathSegment.type === undefined) {
        const classification = this.classifier.classifySegment(fullStatsWithIncoming);

        // Based on the specification for segment classification, there WILL be a classification
        // assigned here.  It's an implementation error if not.
        if(!classification) {
          throw "Implementation error - segment was not properly recognized";
        }

        // auto-resolve the recognition promise & 'lock' the classification (and also the portion
        // of the touchpath that triggered it).
        this.pathSegment.classifyType(classification);
        this.hasPrecommittedSubsegment = true;
      }

      return true;
    }

    /**
     * Call to clear a previously-tracked in-construction subsegment that has diverged and is no
     * longer compatible.
     */
    public clearPendingSubsegment() {
      if(this.hasPrecommittedSubsegment) {
        throw "Invalid state:  must fully commit a subsection due to a precommitted portion"!;
      }

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
        this.hasPrecommittedSubsegment = false;

        // Recognition check!
        if(!this.pathSegment.type) {
          const classification = this.classifier.classifySegment(this.committedInterval);
          if(classification) {
            this.pathSegment.classifyType(classification);
          }
        }
      } else {
        throw "Illegal state - `commitPendingPortion` should never be called with nothing pending.";
      }
    }

    public finalize() {
      if(this.pendingSubsegmentation) {
        this.commitPendingSubsegment();
      }

      // Fully 'recognize' the Segment if it somehow hasn't yet been recognized.
      if(this.pathSegment.type === undefined) {
        this.pathSegment.classifyType(this.classifier.classifySegment(this.committedInterval));
      }

      this.pathSegment.resolve();
    }

    public get pathSegment() {
      return this._pathSegment;
    }

    public set pathSegment(segment: SegmentImplementation) {
      this._pathSegment = segment;
    }
  }
}