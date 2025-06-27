import { LdmlKeyboardTypes } from "@keymanapp/common-types";
import { ObjectWithCompileContext } from '@keymanapp/common-types';

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
  add(verb: SubstitutionUse, markers: string[], compileContext?: ObjectWithCompileContext) {
    if (!markers.length) {
      return; // skip if empty
    }
    compileContext = compileContext || {}; // need at least an empty object
    if (verb == SubstitutionUse.emit) {
      markers.forEach((m) => {
        this.emitted.set(m, compileContext);
        this.all.set(m, compileContext);
      });
    } else if (verb == SubstitutionUse.consume) {
      markers.forEach((m) => {
        this.consumed.set(m, compileContext);
        this.all.set(m, compileContext);
      });
    } else if (verb == SubstitutionUse.match) {
      markers.forEach((m) => {
        this.matched.set(m, compileContext);
        this.all.set(m, compileContext);
      });
    } else if (verb == SubstitutionUse.variable) {
      markers.forEach((m) => {
        // we don't know, so add it to all three
        this.matched.set(m, compileContext);
        this.emitted.set(m, compileContext);
        this.consumed.set(m, compileContext);
        this.all.set(m, compileContext);
      });
      /* c8 skip next 3 */
    } else {
      throw Error(`Internal error: unsupported verb ${verb} for match`);
    }
  }
}

/** rollup of several substitution types */
export class Substitutions {
  addSetAndStringSubtitution(verb: SubstitutionUse, str?: string, compileContext?: ObjectWithCompileContext) {
    this.set.add(verb, LdmlKeyboardTypes.VariableParser.allSetReferences(str), compileContext);
    this.addStringAndMarkerSubstitution(verb, str, compileContext);
  }
  /** add a string that can have string var substitutions or markers */
  addStringAndMarkerSubstitution(verb: SubstitutionUse, str?: string, compileContext?: ObjectWithCompileContext) {
    this.addMarkers(verb, str, compileContext);
    this.addStringSubstitution(verb, str, compileContext);
  }
  addStringSubstitution(verb: SubstitutionUse, str?: string, compileContext?: ObjectWithCompileContext) {
    this.string.add(verb, LdmlKeyboardTypes.VariableParser.allStringReferences(str), compileContext);
  }
  /** add a string that's just markers */
  addMarkers(verb: SubstitutionUse, str?: string, compileContext?: ObjectWithCompileContext) {
    this.markers.add(verb, LdmlKeyboardTypes.MarkerParser.allReferences(str), compileContext);
    LdmlKeyboardTypes.MarkerParser.allBrokenReferences(str).forEach(m => this.badMarkers.set(m, compileContext));
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
