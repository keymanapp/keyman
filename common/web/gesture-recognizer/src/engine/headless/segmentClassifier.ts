import { CumulativePathStats } from "./cumulativePathStats.js";

/**
 * Nomenclature pending.
 *
 * This object defines the configured values to be used for classification of Segments.
 */
export interface SegmentClassifierConfig {
  /** The minimum duration required to classify a segment as a 'hold', in milliseconds. */
  holdMinimumDuration: number;

  /** The minimum number of "CSS pixels" the touchpoint must travel to break a 'hold'. */
  holdMoveTolerance: number;
}

export enum SegmentClass {
  START = 'start',
  END   = 'end',
  HOLD  = 'hold',
  MOVE  = 'move'
}

export class SegmentClassifier {
  readonly config: SegmentClassifierConfig;

  constructor(config: SegmentClassifierConfig) {
    this.config = config;
  }

  /**
   * The speed that must be traversed during the timespan allocated to a
   * minimum-duration 'wait' segment in order to match the maximum-allowed
   * 'wait' distance threshold.
   */
  private get breakevenSpeed(): number {
    return this.config.holdMoveTolerance / this.config.holdMinimumDuration;
  }

  /**
   * Given the cumulative stats for an observed Subsegment, determines its
   * segment-level classification - but only if we would commit to said
   * classification.
   * @param stats
   * @returns
   */
  public classifySubsegment(stats: CumulativePathStats): SegmentClass {
    const segmentClass = this.classifySegment(stats);

    if(segmentClass || segmentClass === null) {
      return segmentClass;
    }

    // For subsegment compatibility classification, we allow early 'hold' declaration if
    // the subsegment is "on pace" to become a 'hold' if the status quo is maintained.
    // If following a 'move', this is probably the first part of a 'hold', and that's nice
    // to capture.  (Or, if preceding a 'move', the last part of a 'hold'.)
    if(stats.speed <= this.breakevenSpeed) {
      return SegmentClass.HOLD;
      // Otherwise, there's a strong chance - but not a guarantee - of transitioning into a
      // 'move' classification.  It's best to blend with whichever neighboring subsegment
      // will take it, if any.
    } else {
      return null;
    }
  }

  /**
   *
   * @param stats Given the cumulative stats for a potential Segment, determines
   * the classification it would be assigned.  Will return `null` if it is not
   * yet possible to commit to a classification.
   * @returns
   */
  public classifySegment(stats: CumulativePathStats): SegmentClass {
    if(!stats) {
      return null;
    }
    // If the segment's net traveled distance exceeds the configured 'wait' distance
    // threshold, it must be classified as a 'move'.
    if(stats.netDistance > this.config.holdMoveTolerance) {
      return SegmentClass.MOVE;
      // If it does not, and the duration of the segment exceeds the configured minimum
      // 'hold' time threshold, it's therefore a 'hold'.
    } else if(stats.duration >= this.config.holdMinimumDuration) {
      return SegmentClass.HOLD;
      // Otherwise, it cannot be 'recognized' as a formal segment.
    } else {
      return null;
    }
  }
}