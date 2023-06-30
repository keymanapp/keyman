import { SegmentationSplit } from "./pathSegmenter.js";
import { SegmentClass, SegmentClassifier } from "../segmentClassifier.js";

/**
 * This class fulfills two roles:
 *
 * 1.  The `.isCompatible()` function provides the core definition used by
 * `ConstructingSegment` to re-merge compatible subsegments, 'correcting' any
 * 'oversegmentation' that may have resulted from `PathSegmenter`'s algorithm.
 *
 * 2. This class's other properties and methods facilitate inspection of
 * the algorithm and its decision-making process during interactive debugging
 * sessions.
 *
 * `SegmentationSplit` objects are constructed from Subsegmentation objects,
 * which are made in abundance throughout the segmentation algorithms.  So, it
 * shouldn't be difficult to construct the necessary parameters to dynamically
 * construct instances of this class during an interactive debugging session.
 */
export class SubsegmentCompatibilityAnalyzer {
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

  /**
   * This property summarizes subsegment compatibility on the basis of direction.
   *
   * 1. If both subsegments are compatible as determined by geometry-based regression, we consider
   * them compatible.  (The first phase considers both geometry _and time_; we leave the "time"
   * part out here.)
   *
   * 2. If regression upholds the segmentation, but both subsegments are classified as
   * 'move'-compatible and their directions both fall within the same "direction bucket", we
   * consider them compatible.
   *
   * 3. If both subsegments are classified as 'hold's, angle doesn't matter - the user's intent
   * is "no motion".
   */
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

  /**
   * Indicates whether or not the two halves of the represented segmentation fall within
   * the same cardinal-direction "bucket".
   */
  get cardinalDirectionCompatible(): boolean {
    return !this.preStats || this.preStats.cardinalDirection == this.postStats.cardinalDirection;
  }

  /**
   * Indicates whether or not geometry-based regression of the two subsegments (without
   * respect to speed and/or time) provides sufficient evidence to uphold the segmentation.
   */
  get regressionDirectionCompatible(): boolean {
    return !this.preStats || this.split.mergeMerited;
  }

  /**
   * Indicates whether or not the classifications of the two subsegments is compatible;
   * if so, it returns the corresponding `SegmentClass` (or `null`, if no classification
   * may be committed yet).
   *
   * If the classifications are incompatible, ths function will return `undefined`
   * instead.
   */
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

  /**
   * The full, complete test for subsegment 'compatibility'.  If `true`, our
   * criteria indicate no merit in upholding the segmentation under review.
   * `false` indicates that the segmentation should be upheld instead.
   */
  get isCompatible(): boolean {
    const commonClass = this.classificationIfCompatible;

    // If two adjacent hold subsegments also make a hold when combined...
    // just merge the two holds & call 'em compatible.
    if(!this.preStats || commonClass == 'hold') {
      return true;
    } else if(commonClass === undefined) { // `null`: pending; `undefined`: incompatible
      return false;
    }

    // if(null || "move"):  as `null` "looks like" a not-quite-there-yet "move",
    // we treat it as such here.  Such subsegments are only compatible if
    // their directions are compatible.
    return this.directionCompatible;
  }

  /**
   * Intended only for use during interactive debugging sessions; prints useful logging
   * information to the developer console.
   */
  public debugLogCompabilityReport() {
    if(!this.preStats) {
      console.log("No prior subsegments - thus, no compatibility conflicts are possible.");
      return;
    }

    console.log("Regression-based compatibility check:")
    this.split._debugLogAlignmentReport();
  }
}