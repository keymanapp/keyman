import { LdmlKeyboardTypes } from "@keymanapp/common-types";
import { ObjectWithCompileContext } from "@keymanapp/developer-utils";

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

type SubstitutionSet = Map<string, ObjectWithCompileContext>;

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
    this.emitted =  new Map<string, ObjectWithCompileContext>();
    this.consumed = new Map<string, ObjectWithCompileContext>();
    this.matched =  new Map<string, ObjectWithCompileContext>();
    this.all =      new Map<string, ObjectWithCompileContext>();
  }

  /**
   *
   * @param verb what kind of use we are adding
   * @param markers list of substitutions to add
   */
  add(verb: SubstitutionUse, markers: string[], x?: ObjectWithCompileContext) {
    if (!markers.length) {
      return; // skip if empty
    }
    x = x || {}; // need at least an empty object
    if (verb == SubstitutionUse.emit) {
      markers.forEach((m) => {
        this.emitted.set(m, x);
        this.all.set(m,x);
      });
    } else if (verb == SubstitutionUse.consume) {
      markers.forEach((m) => {
        this.consumed.set(m,x);
        this.all.set(m,x);
      });
    } else if (verb == SubstitutionUse.match) {
      markers.forEach((m) => {
        this.matched.set(m,x);
        this.all.set(m,x);
      });
    } else if (verb == SubstitutionUse.variable) {
      markers.forEach((m) => {
        // we don't know, so add it to all three
        this.matched.set(m,x);
        this.emitted.set(m,x);
        this.consumed.set(m,x);
        this.all.set(m,x);
      });
      /* c8 skip next 3 */
    } else {
      throw Error(`Internal error: unsupported verb ${verb} for match`);
    }
  }
}

/** rollup of several substitution types */
export class Substitutions {
  addSetAndStringSubtitution(verb: SubstitutionUse, str?: string, x?: ObjectWithCompileContext) {
    this.set.add(verb, LdmlKeyboardTypes.VariableParser.allSetReferences(str), x);
    this.addStringAndMarkerSubstitution(verb, str, x);
  }
  /** add a string that can have string var substitutions or markers */
  addStringAndMarkerSubstitution(verb: SubstitutionUse, str?: string, x?: ObjectWithCompileContext) {
    this.addMarkers(verb, str, x);
    this.addStringSubstitution(verb, str, x);
  }
  addStringSubstitution(verb: SubstitutionUse, str?: string, x?: ObjectWithCompileContext) {
    this.string.add(verb, LdmlKeyboardTypes.VariableParser.allStringReferences(str), x);
  }
  /** add a string that's just markers */
  addMarkers(verb: SubstitutionUse, str?: string, x?: ObjectWithCompileContext) {
    this.markers.add(verb, LdmlKeyboardTypes.MarkerParser.allReferences(str), x);
    LdmlKeyboardTypes.MarkerParser.allBrokenReferences(str).forEach(m => this.badMarkers.set(m, x));
  }
  // all valid markers
  markers: SubstitutionTracker;
  // all invalid markers
  badMarkers: SubstitutionSet;
  // all valid set ids
  set: SubstitutionTracker;
  // all valid string ids
  string: SubstitutionTracker;
  // all valid uset ids
  uset: SubstitutionTracker;

  constructor() {
    this.markers = new SubstitutionTracker();
    this.set = new SubstitutionTracker();
    this.string = new SubstitutionTracker();
    this.uset = new SubstitutionTracker();
    this.badMarkers = new Map<string, ObjectWithCompileContext>();
  }
}
