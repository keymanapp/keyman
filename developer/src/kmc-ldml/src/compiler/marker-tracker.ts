/**
 * Verb for MarkerTracker.add()
 */
export enum MarkerUse {
  /** outputs this marker into context (e.g. transform to= or key to=) */
  emit,
  /** consumes this marker out of the context (e.g. transform from=) */
  consume,
  /** matches the marker, but doesn't consume (e.g. display to=) */
  match,
  /** variable definition: might consume, emit, or match. */
  variable,
}

type MarkerSet = Set<string>;

/** Tracks usage of markers */
export class MarkerTracker {
  /** markers that were emitted */
  emitted: MarkerSet;
  /** markers that were consumed and removed from the context */
  consumed: MarkerSet;
  /** markers that were matched, but not necessarily consumed */
  matched: MarkerSet;
  /** all markers */
  all: MarkerSet;

  constructor() {
    this.emitted = new Set<string>();
    this.consumed = new Set<string>();
    this.matched = new Set<string>();
    this.all = new Set<string>();
  }

  /**
   *
   * @param verb what kind of use we are adding
   * @param markers list of markers to add
   */
  add(verb: MarkerUse, markers: string[]) {
    if (!markers.length) {
      return; // skip if empty
    }
    if (verb == MarkerUse.emit) {
      markers.forEach((m) => {
        this.emitted.add(m);
        this.all.add(m);
      });
    } else if (verb == MarkerUse.consume) {
      markers.forEach((m) => {
        this.consumed.add(m);
        this.all.add(m);
      });
    } else if (verb == MarkerUse.match) {
      markers.forEach((m) => {
        this.matched.add(m);
        this.all.add(m);
      });
    } else if (verb == MarkerUse.variable) {
      markers.forEach((m) => {
        // we don't know, so add it to all three
        this.matched.add(m);
        this.emitted.add(m);
        this.consumed.add(m);
        this.all.add(m);
      });
      /* c8 skip next 3 */
    } else {
      throw Error(`Internal error: unsupported verb ${verb} for match`);
    }
  }
}
