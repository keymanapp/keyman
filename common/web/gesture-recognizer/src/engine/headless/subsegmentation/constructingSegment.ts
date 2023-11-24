import { SegmentClassifier } from "../segmentClassifier.js";
import { RegressiblePathStats } from "../regressiblePathStats.js";
import { SegmentationSplit, Subsegmentation } from "./pathSegmenter.js";
import { SegmentImplementation } from "./segment.js";
import { SubsegmentCompatibilityAnalyzer } from "./subsegmentCompatibilityAnalyzer.js";

/**
 * This class is responsible for managing the construction of public-facing Segments while keeping
 * all the internal parts... internal.  As such, it includes state management for parts of
 * PathSegmenter's operations.  Note that it makes many assumptions based upon its usage within
 * PathSegmenter.
 *
 * See also `SubsegmentCompatibilityAnalyzer`, which defines the criteria used within this class
 * for determining when to recombine subsegments and when to uphold segmentation decisions.
 */
export class ConstructingSegment {
  readonly classifier: SegmentClassifier;

  /**
   * Marks previously-accumulated stats on the touchpath for the portion that precedes
   * the in-construction Segment.
   */
  readonly baseAccumulation: RegressiblePathStats;

  /**
   * Notes all subsegments identified as part of the overall segment being constructed.
   */
  private subsegmentations: Subsegmentation[] = [];

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
  private buildIntervalFromBase(accumulation: RegressiblePathStats) {
    return accumulation.deaccumulate(this.baseAccumulation);
  }

  /**
   * Returns the stats accumulation covering all currently-committed subsegments.
   * May be `null` if there are no committed subsegments.
   */
  public get committedInterval(): RegressiblePathStats {
    return this.committedIntervalAsSubsegmentation.stats;
  }

  /**
   * Returns the number of already-committed subsegments that comprise the
   * in-construction Segment thus far.
   */
  public get subsegmentCount(): number {
    return this.subsegmentations.length;
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
    return pendingAnalyzer.isCompatible;
  }

  /**
   * Returns the analyzer instance used for the `isCompatible` check.  The returned
   * object provides additional helper properties useful for inspecting compatibility
   * logic.
   */
  public analyzeCompatibility(subsegmentation: Subsegmentation): SubsegmentCompatibilityAnalyzer {
    const committed = this.committedIntervalAsSubsegmentation;
    const segmentationSplit = new SegmentationSplit(committed, subsegmentation);

    return new SubsegmentCompatibilityAnalyzer(segmentationSplit, this.classifier);
  }

  /**
   * Returns the stats accumulation covering all currently-committed subsegments +
   * the submitted `Subsegmentation` object (as if it were committed).
   *
   * Assumes that its parameter immediately follows any previously-committed
   * subsegments.
   */
  private wholeSegmentation(subsegmentation: Subsegmentation): Subsegmentation {
    const whole: Subsegmentation = {
      stats: subsegmentation.endingAccumulation.deaccumulate(this.baseAccumulation),
      baseAccumulation: this.baseAccumulation,
      endingAccumulation: subsegmentation.endingAccumulation
    };

    return whole;
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

  public get hasPendingSubsegment() {
    return !!this.pendingSubsegmentation;
  }

  /**
   * Call this to denote the current state of an in-construction subsegment that **will** be included
   * in the final Segment once completed if the pending subsegment does not diverge in a later update.
   *
   * If the pending subsegment appears to belong to the current Segment, it contains valuable info
   * about the state of the as-of-yet unresolved Segment, including the most recent location
   * of the corresponding touchpoint.
   *
   * NOTE:  Assumes that .isCompatible has been checked first!  This method (currently) does not
   * perform the related check!
   * - It is currently called when and where relevant in PathSegmenter, the only thing currently
   *   calling this method.
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
    const recognitionFromMove = (this.pathSegment.distance > this.classifier.config.holdMoveTolerance);

    // `undefined` if and only if still unrecognized.
    if((recognitionFromMove || recognitionWaitTime <= 0) && !this._pathSegment.isRecognized) {
      const classification = this.classifier.classifySegment(fullStatsWithIncoming);

      // Based on the specification for segment classification, there WILL be a classification
      // assigned here.  It's an implementation error if not.
      if(!classification) {
        throw new Error("Implementation error - segment was not properly recognized");
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
      throw new Error("Invalid state:  must fully commit a subsection due to a precommitted portion!");
    }

    // Probably does not need to trigger an event; if called, there's a new, follow-up segment
    // coming that will maintain the current coordinate with its events instead.  Assuming
    // the need to rely on Segment events for that; even that's better off handled with path.coords
    // events instead.
    this.pendingSubsegmentation = null;

    // If there was a pending subsegment, we need to remove its components from the published segment stats.
    this.pathSegment.updateStats(this.committedInterval);
  }

  /**
   * Commits a currently-pending subsegment as part of the in-construction Segment.
   */
  public commitPendingSubsegment() {
    if(this.pendingSubsegmentation) {
      this.subsegmentations.push(this.pendingSubsegmentation);

      // Check the peak speed & update if needed.
      const commitStats = this.pendingSubsegmentation.stats;
      let peakSpeed = commitStats.mean('v') + Math.sqrt(commitStats.variance('v'));
      if(peakSpeed > this.pathSegment.peakSpeed) {
        this.pathSegment.setPeakSpeed(peakSpeed);
      }

      // Clear 'pending'-related fields.
      this.pendingSubsegmentation = null;
      this.hasPrecommittedSubsegment = false;

      // Recognition check!
      if(!this.pathSegment.isRecognized) {
        const classification = this.classifier.classifySegment(this.committedInterval);
        if(classification) {
          this.pathSegment.classifyType(classification);
        }
      }
    } else {
      throw new Error("Illegal state - `commitPendingPortion` should never be called with nothing pending.");
    }
  }

  /**
   * Finalizes the Segment, committing any pending portions, classifying ('recognizing') it if
   * necessary and resolving it.
   */
  public finalize() {
    if(this.pendingSubsegmentation) {
      this.commitPendingSubsegment();
    }

    // Fully 'recognize' the Segment if it somehow hasn't yet been recognized.
    if(!this.pathSegment.isRecognized) {
      this.pathSegment.classifyType(this.classifier.classifySegment(this.committedInterval));
    }

    this.pathSegment.resolve();
  }

  /**
   * The in-construction Segment, as published to `GesturePath.segments` & `GesturePath`'s
   * 'segmentation' event.
   */
  public get pathSegment() {
    return this._pathSegment;
  }

  private set pathSegment(segment: SegmentImplementation) {
    this._pathSegment = segment;
  }
}