import { MarkerParser, VariableParser } from "@keymanapp/common-types";

/**
 * Verb for SubstitutionTracker.add()
 */
export enum SubstitutionUse {
  /** outputs this marker into context (e.g. transform to= or key to=) */
  emit,
  /** consumes this marker out of the context (e.g. transform from=) */
  consume,
  /** matches the marker, but doesn't consume (e.g. display to=) */
  match,
  /** variable definition: might consume, emit, or match. */
  variable,
}

type SubstitutionSet = Set<string>;

/** Tracks usage of markers */
export class SubstitutionTracker {
  /** markers that were emitted */
  emitted: SubstitutionSet;
  /** markers that were consumed and removed from the context */
  consumed: SubstitutionSet;
  /** markers that were matched, but not necessarily consumed */
  matched: SubstitutionSet;
  /** all markers */
  all: SubstitutionSet;

  constructor() {
    this.emitted = new Set<string>();
    this.consumed = new Set<string>();
    this.matched = new Set<string>();
    this.all = new Set<string>();
  }

  /**
   *
   * @param verb what kind of use we are adding
   * @param markers list of substitutions to add
   */
  add(verb: SubstitutionUse, markers: string[]) {
    if (!markers.length) {
      return; // skip if empty
    }
    if (verb == SubstitutionUse.emit) {
      markers.forEach((m) => {
        this.emitted.add(m);
        this.all.add(m);
      });
    } else if (verb == SubstitutionUse.consume) {
      markers.forEach((m) => {
        this.consumed.add(m);
        this.all.add(m);
      });
    } else if (verb == SubstitutionUse.match) {
      markers.forEach((m) => {
        this.matched.add(m);
        this.all.add(m);
      });
    } else if (verb == SubstitutionUse.variable) {
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

/** rollup of several substitution types */
export class Substitutions {
  addSetAndStringSubtitution(verb: SubstitutionUse, str?: string) {
    this.set.add(verb, VariableParser.allSetReferences(str));
    this.addStringAndMarkerSubstitution(verb, str);
  }
  /** add a string that can have string var substitutions or markers */
  addStringAndMarkerSubstitution(verb: SubstitutionUse, str?: string) {
    this.addMarkers(verb, str);
    this.addStringSubstitution(verb, str);
  }
  addStringSubstitution(verb: SubstitutionUse, str?: string) {
    this.string.add(verb, VariableParser.allStringReferences(str));
  }
  /** add a string that's just markers */
  addMarkers(verb: SubstitutionUse, str?: string) {
    this.markers.add(verb, MarkerParser.allReferences(str));
  }
  markers: SubstitutionTracker;
  set: SubstitutionTracker;
  string: SubstitutionTracker;
  uset: SubstitutionTracker;

  constructor() {
    this.markers = new SubstitutionTracker();
    this.set = new SubstitutionTracker();
    this.string = new SubstitutionTracker();
    this.uset = new SubstitutionTracker();
  }
}
