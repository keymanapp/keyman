/// <reference path="segmentStats.ts" />

namespace com.keyman.osk {
  export class Segment {
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
    private _stats: SegmentStats[];

    constructor() {
      this._stats = [];
    }

    public get stats(): readonly SegmentStats[] {
      return this._stats;
    }

    public add(sample: InputSample) {
      let baseStats: SegmentStats;
      if(this.stats.length > 0) {
        baseStats = this.stats[this.stats.length-1];
      } else {
        baseStats = new SegmentStats();
      }

      let extendedStats = baseStats.unionWith(sample);

      // FIXME:  VERY temp logging.
      console.log(extendedStats.toJSON());
      this._stats.push(extendedStats);
    }
  }
}