import { KMXPlusVersion, SectionIdent, constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LdmlKeyboardTypes } from '@keymanapp/common-types';
import { ObjectWithCompileContext } from "@keymanapp/common-types";
import { LDMLKeyboard, CompilerCallbacks } from '@keymanapp/developer-utils';
import { SectionCompiler } from "./section-compiler.js";
import Vars = KMXPlus.Vars;
import StringVarItem = KMXPlus.StringVarItem;
import SetVarItem = KMXPlus.SetVarItem;
import UnicodeSetItem = KMXPlus.UnicodeSetItem;
import DependencySections = KMXPlus.DependencySections;
import LDMLKeyboardXMLSourceFile = LDMLKeyboard.LDMLKeyboardXMLSourceFile;
import { LdmlCompilerMessages } from "./ldml-compiler-messages.js";
import { KeysCompiler } from "./keys.js";
import { TransformCompiler } from "./tran.js";
import { DispCompiler } from "./disp.js";
import { SubstitutionUse, Substitutions } from "./substitution-tracker.js";
export class VarsCompiler extends SectionCompiler {
  public get id() {
    return constants.section.vars;
  }

  public get dependencies(): Set<SectionIdent> {
    const defaults = new Set(<SectionIdent[]>[
      constants.section.strs,
      constants.section.elem,
      constants.section.list,
    ]);
    defaults.delete(this.id);
    return defaults;
  }

  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks, targetVersion: KMXPlusVersion) {
    super(source, callbacks, targetVersion);
  }

  public validate(): boolean {
    let valid = true;

    valid = this.validateAllSubstitutions() && valid; // accumulate validity

    return valid;
  }

  private validateIdentifier(id: string, compileContext?: ObjectWithCompileContext) {
    if(!id.match(LdmlKeyboardTypes.VariableParser.ID)) { // From <string> DTD
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidVariableIdentifier({id}, compileContext));
      return false;
    }
    return true;
  }

  private validateVars(st: Substitutions): boolean {
    let valid = true;
    const variables = this.keyboard3?.variables;

    // Check for duplicate ids
    const allIds = new Map<string,LDMLKeyboard.Variable>();
    const dups = new Set<string>();

    const allStrings = new Set();
    const allSets = new Set();
    const allUnicodeSets = new Set();

    /**
     * add an ID to check for duplicates
     * @param id id
     * @param x context
     */
    function addId(id: string, compileContext: LDMLKeyboard.Variable): void {
      if (allIds.has(id)) {
        dups.add(id);
      } else {
        allIds.set(id, compileContext);
      }
    }

    if (variables) {
      // Strings
      for (const e of variables.string) {
        const { id, value } = e;
        if(!this.validateIdentifier(id, e)) {
          valid = false;
          continue;
        }
        addId(id, e);
        const stringrefs = LdmlKeyboardTypes.VariableParser.allStringReferences(value);
        for(const ref of stringrefs) {
          if(!allStrings.has(ref)) {
            valid = false;
            this.callbacks.reportMessage(LdmlCompilerMessages.Error_MissingStringVariable({id: ref}, e));
            allStrings.add(ref); // avoids multiple reports of same missing variable
          }
        }
        st.string.add(SubstitutionUse.variable, stringrefs, e);
        allStrings.add(id);
        st.addMarkers(SubstitutionUse.variable, value, e);
      }
      // Sets
      for (const e of variables.set) {
        const { id, value } = e;
        if(!this.validateIdentifier(id, e)) {
          valid = false;
          continue;
        }
        addId(id, e);
        allSets.add(id);
        // check for illegal references, here.
        const stringrefs = LdmlKeyboardTypes.VariableParser.allStringReferences(value);
        st.string.add(SubstitutionUse.variable, stringrefs, e);

        // Now split into spaces.
        const items: string[] = LdmlKeyboardTypes.VariableParser.setSplitter(value);
        for (const item of items) {
          const setrefs = LdmlKeyboardTypes.VariableParser.allSetReferences(item);
          if (setrefs.length > 1) {
            // this is the form $[seta]$[setb]
            valid = false;
            this.callbacks.reportMessage(LdmlCompilerMessages.Error_NeedSpacesBetweenSetVariables({ item }, e));
          } else {
            st.set.add(SubstitutionUse.variable, setrefs, e); // the reference to a 'map'
          }
          // TODO-LDML: Are there other illegal cases here? what about "x$[set]"?
        }
      }
      // UnicodeSets
      for (const e of variables.uset) {
        const { id, value } = e;
        if(!this.validateIdentifier(id, e)) {
          valid = false;
          continue;
        }
        addId(id, e);
        allUnicodeSets.add(id);
        const stringrefs = LdmlKeyboardTypes.VariableParser.allStringReferences(value);
        st.string.add(SubstitutionUse.variable, stringrefs, e);
        const setrefs = LdmlKeyboardTypes.VariableParser.allSetReferences(value);
        for (const id2 of setrefs) {
          if (!allUnicodeSets.has(id2)) {
            valid = false;
            if (allSets.has(id2)) {
              // $[set] in a UnicodeSet must be another UnicodeSet.
              this.callbacks.reportMessage(LdmlCompilerMessages.Error_CantReferenceSetFromUnicodeSet({ id: id2 }, e));
            } else {
              st.uset.add(SubstitutionUse.variable, [id2], e);
            }
          }
        }
      }
    }
    // one report if any dups
    if (dups.size > 0) {
      for (const id of dups.values()) {
        this.callbacks.reportMessage(LdmlCompilerMessages.Error_DuplicateVariable({
          id
        }, allIds.get(id)));
      }
      valid = false;
    }

    // check for any missing vars
    for (const [id,x] of st.set.all.entries()) {
      // note: we check the uset list also. we don't know until later which was
      // intended. collisions are handled separately.
      if (!allSets.has(id) && !allUnicodeSets.has(id)) {
        valid = false;
        this.callbacks.reportMessage(LdmlCompilerMessages.Error_MissingSetVariable({ id }, x));
      }
    }
    for (const [id,x] of st.string.all.entries()) {
      if (!allStrings.has(id)) {
        valid = false;
        this.callbacks.reportMessage(LdmlCompilerMessages.Error_MissingStringVariable({ id }, x));
      }
    }
    for (const [id,x] of st.uset.all.entries()) {
      if (!allUnicodeSets.has(id)) {
        valid = false;
        this.callbacks.reportMessage(LdmlCompilerMessages.Error_MissingUnicodeSetVariable({ id }, x));
      }
    }

    return valid;
  }

  private collectSubstitutions(st: Substitutions): boolean {
    let valid = true;

    // call our friends to validate
    valid = this.validateSubstitutions(this.keyboard3, st) && valid; // accumulate validity
    valid = KeysCompiler.validateSubstitutions(this.keyboard3, st) && valid; // accumulate validity
    valid = TransformCompiler.validateSubstitutions(this.keyboard3, st) && valid; // accumulate validity
    valid = DispCompiler.validateSubstitutions(this.keyboard3, st) && valid; // accumulate validity
    return valid;
  }

  private validateAllSubstitutions(): boolean {
    const st = new Substitutions();
    let valid = this.collectSubstitutions(st);
    // see if there are any matched-but-not-emitted markers
    const matchedNotEmitted : Set<string> = new Set<string>();
    const mt = st.markers;
    for (const m of mt.matched.keys()) {
      if (m === LdmlKeyboardTypes.MarkerParser.ANY_MARKER_ID) continue; // match-all marker
      if (!mt.emitted.has(m)) {
        matchedNotEmitted.add(m);
      }
    }
    for (const m of mt.consumed.keys()) {
      if (m === LdmlKeyboardTypes.MarkerParser.ANY_MARKER_ID) continue; // match-all marker
      if (!mt.emitted.has(m)) {
        matchedNotEmitted.add(m);
      }
    }

    // report once
    if (matchedNotEmitted.size > 0) {
      const ids = Array.from(matchedNotEmitted.values()).sort();
      const x = mt.all.get(ids[0]); // get the FIRST object for context
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_MissingMarkers({ ids: ids.join(',')}, x));
      valid = false;
    }

    // now, validate the variables.
    valid = this.validateVars(st) && valid;

    if (!!st.badMarkers.size) {
      valid = false;
      for (const [id, x] of st.badMarkers.entries()) {
        this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidMarkerIdentifier({ id }, x));
      }
    }

    return valid;
  }

  validateSubstitutions(keyboard: LDMLKeyboard.LKKeyboard, st : Substitutions) : boolean {
    keyboard?.variables?.string?.forEach((string) => {
      const {value} = string;
      st.markers.add(SubstitutionUse.variable, LdmlKeyboardTypes.MarkerParser.allReferences(value), string);
    });
    // get markers mentioned in a set
    keyboard?.variables?.set?.forEach((set) => {
      const { value } = set;
      LdmlKeyboardTypes.VariableParser.setSplitter(value).forEach(v => st.markers.add(SubstitutionUse.match, LdmlKeyboardTypes.MarkerParser.allReferences(v), set));
    });
    return true;
  }

  public compile(sections: DependencySections): Vars {
    const result =  new Vars();

    const variables = this.keyboard3?.variables;
    // we always have vars, it's depended on by other sections
    // we already know the variables do not conflict with each other
    // need to add these one by one, because they depend on each other.
    // we don't add these in completely 'natural' order because xml2js has already lost that.
    // instead we go: strings, sets, then unicodeSets.

    // first, strings.
    variables?.string?.forEach((e) =>
      this.addString(result, e, sections));
    variables?.uset?.forEach((e) =>
      this.addUnicodeSet(result, e, sections));

    // reload markers - TODO-LDML: double work!
    const st = new Substitutions();
    this.collectSubstitutions(st);
    const mt = st.markers;

    // collect all markers, excluding the match-all
    const allMarkers : string[] = Array.from(mt.all.keys()).filter(m => m !== LdmlKeyboardTypes.MarkerParser.ANY_MARKER_ID).sort();
    result.markers = sections.list.allocList(allMarkers, {
      // pass the first object in the listr
      compileContext: mt.all.get(allMarkers[0]||'')
      // however, this is the string with the marker *id*. So, from the StrsCompiler
      // point of view, the value of this context would be if the marker ID had, say,
      // invalid Unicode in it for ERROR_IllegalCharacters - but, that wouldn't be a
      // valid Marker ID either.
      // So it's not likely that this context will be used, but it's available.
    }, sections);

    // sets need to be added late, because they can refer to markers
    variables?.set?.forEach((e) =>
      this.addSet(result, e, sections));

    return result.valid() ? result : null;
  }

  // routines for initializing Vars remain here, in the compiler.
  addString(result: Vars, e: LDMLKeyboard.LKString, sections: DependencySections): void {
    const { id } = e;
    let { value } = e;
    // fix any variables
    value = result.substituteStrings(value, sections);
    result.strings.push(new StringVarItem(id, value, sections, e));
  }
  addSet(result: Vars, e: LDMLKeyboard.LKSet, sections: DependencySections): void {
    const { id } = e;
    let { value } = e;
    // first substitute strings
    value = result.substituteStrings(value, sections);
    // OK to do this as a substitute, because we've already validated the set above.
    value = result.substituteSets(value, sections);
    // raw items - without marker substitution
    const rawItems: string[] = LdmlKeyboardTypes.VariableParser.setSplitter(value);
    // cooked items - has substutition of markers
    // this is not 'forMatch', all variables are to be assumed as string literals, not regex
    // content.
    const cookedItems: string[] = rawItems.map(v => result.substituteMarkerString(v, false));
    result.sets.push(new SetVarItem(id, cookedItems, sections, e));
  }
  addUnicodeSet(result: Vars, e: LDMLKeyboard.LKUSet, sections: DependencySections): void {
    const { id } = e;
    let { value } = e;
    value = result.substituteStrings(value, sections);
    value = result.substituteUnicodeSets(value, sections);
    result.usets.push(new UnicodeSetItem(id, value, sections, sections.usetparser, e));
  }
  // routines for using/substituting variables have been moved to the Vars class and its
  // properties
}
