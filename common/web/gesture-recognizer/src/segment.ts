namespace com.keyman.osk {
  export class Segment {
    /**
     * Gets the accumulated stats across the full represented interval.
     *
     * This should be referenced for most statistical values that may be requested of the interval.
     */
    private stats: CumulativePathStats;

    // Does NOT track the subsegment stats.  That said, will maintain internal fields
    // with values sourced FROM subsegment stats as needed.

    /**
     * Denotes the highest (mean + 1-sigma) speed seen among all subsegments comprising
     * this segment.
     *
     * This is likely not the maximum speed observed, as that observation is likely an outlier
     * and is not internally recorded.  We rebuild this from stats observations.
     *
     * Think of it as acting like a "smoothed peak speed".
     */
    private _peakSpeed: number;

    get peakSpeed(): number {
      return this._peakSpeed;
    }

    // get initialCoord(): InputSample;
    // get lastCoord(): InputSample;

    // get meanSpeed(): number;

    // get angle();
    // get direction(): string; // (n, nw, ...)

    // In-dev notes:

    // can get speed from best entry in subsegment stats
    // subsegments should have consistent direction:  get direction from cumulative stats

    // interface to update subsegments?
    // as this'll be a public object, we prob don't want to public-expose the related method(s).
    // but we want a consistent object for the user / 'library consumer'.  How to?
    // Idea:  public interface
    // - private implementation
    // - (possible) safety wrapper for the private implementation
    //   - but, as far as Web (TS) goes, private-implementation parts won't be accessible.

    // segment 'type' / 'classification'

    // events?  or Promises?
    // - 'recognized' - from 'unknown' to something specific.
    // - 'resolved'   - when the segment is definitively finished.
    // Promises might actually be simpler to work with externally - a segment could, in theory, be auto-recognized.
  }
}