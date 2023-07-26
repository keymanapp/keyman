import { SectionIdent, constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LDMLKeyboard, CompilerCallbacks } from '@keymanapp/common-types';
import { VariableParser } from '@keymanapp/common-types';
import { SectionCompiler } from "./section-compiler.js";
import Vars = KMXPlus.Vars;
import StringVarItem = KMXPlus.StringVarItem;
import SetVarItem = KMXPlus.SetVarItem;
import UnicodeSetItem = KMXPlus.UnicodeSetItem;
import DependencySections = KMXPlus.DependencySections;
import LDMLKeyboardXMLSourceFile = LDMLKeyboard.LDMLKeyboardXMLSourceFile;
import { CompilerMessages } from "./messages.js";
export class VarsCompiler extends SectionCompiler {
  public get id() {
    return constants.section.vars;
  }

  public get dependencies(): Set<SectionIdent> {
    const defaults = new Set(<SectionIdent[]>[
      constants.section.strs,
      constants.section.elem
    ]);
    defaults.delete(this.id);
    return defaults;
  }

  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
  }

  public validate(): boolean {
    let valid = true;
    // TODO-LDML scan for markers?

    // Check for duplicate ids
    const allIds = new Set();
    const dups = new Set();
    const variables = this.keyboard?.variables;

    if (!variables) return valid;

    const allStrings = new Set();
    const allSets = new Set();
    const allUnicodeSets = new Set();

    /**
     * add an ID to check for duplicates
     * @param id id
     */
    function addId(id : string) : void {
      if (allIds.has(id)) {
        dups.add(id);
      } else {
        allIds.add(id);
      }
    }

    // Strings
    for (const {id, value} of variables?.string) {
      addId(id);
      allStrings.add(id);
      const stringrefs = VariableParser.allStringReferences(value);
      for (const id2 of stringrefs) {
        if (!allStrings.has(id2)) {
          valid = false;
          this.callbacks.reportMessage(CompilerMessages.Error_MissingStringVariable({id: id2}));
        }
      }
    }
    // Sets
    for (const {id, value} of variables.set) {
      addId(id);
      allSets.add(id);
      // check for illegal references, here.
      const stringrefs = VariableParser.allStringReferences(value);
      for (const id2 of stringrefs) {
        if (!allStrings.has(id2)) {
          valid = false;
          this.callbacks.reportMessage(CompilerMessages.Error_MissingStringVariable({id: id2}));
        }
      }

      // Now split into spaces.
      const items : string[] = VariableParser.setSplitter(value);
      for (const item of items) {
        const setrefs = VariableParser.allSetReferences(item);
        if (setrefs.length > 1) {
          // this is the form $[seta]$[setb]
          valid = false;
          this.callbacks.reportMessage(CompilerMessages.Error_NeedSpacesBetweenSetVariables({item}));
        } else {
          for (const id2 of setrefs) {
            if (!allSets.has(id2)) {
              valid = false;
              this.callbacks.reportMessage(CompilerMessages.Error_MissingSetVariable({id: id2}));
            }
          }
        }
        // TODO-LDML: Are there other illegal cases here? what about "x$[set]"?
      }
    }
    // UnicodeSets
    for (const {id, value} of variables.unicodeSet) {
      addId(id);
      allUnicodeSets.add(id);
      const stringrefs = VariableParser.allStringReferences(value);
      for (const id2 of stringrefs) {
        if (!allStrings.has(id2)) {
          valid = false;
          this.callbacks.reportMessage(CompilerMessages.Error_MissingStringVariable({id: id2}));
        }
      }
      const setrefs = VariableParser.allSetReferences(value);
      for (const id2 of setrefs) {
        if (!allUnicodeSets.has(id2)) {
          valid = false;
          if (allSets.has(id2)) {
            // $[set] in a UnicodeSet must be another UnicodeSet.
            this.callbacks.reportMessage(CompilerMessages.Error_CantReferenceSetFromUnicodeSet({id: id2}));
          } else {
            this.callbacks.reportMessage(CompilerMessages.Error_MissingUnicodeSetVariable({id: id2}));
          }
        }
      }
    }

    // one report if any dups
    if (dups.size > 0) {
      this.callbacks.reportMessage(CompilerMessages.Error_DuplicateVariable({
        ids: Array.from(dups.values()).sort().join(', ')
      }));
      valid = false;
    }

    valid = valid && this.validateMarkers();

    return valid;
  }

  private validateMarkers(): boolean {
    return true;
  }

  public compile(sections: DependencySections): Vars {
    const result =  new Vars();

    const variables = this.keyboard?.variables;

    if (!variables) return result; // Empty vars, to simplify other sections

    // we already know the variables do not conflict with each other
    // need to add these one by one, because they depend on each other.
    // we don't add these in completely 'natural' order because xml2js has already lost that.
    // instead we go: strings, sets, then unicodeSets.

    // first, strings.
    variables?.string?.forEach((e) =>
      this.addString(result, e, sections));
    variables?.set?.forEach((e) =>
      this.addSet(result, e, sections));
    variables?.unicodeSet?.forEach((e) =>
      this.addUnicodeSet(result, e, sections));

    return result.valid() ? result : null;
  }

  // routines for initializing Vars remain here, in the compiler.
  addString(result: Vars, e: LDMLKeyboard.LKString, sections: DependencySections): void {
    const { id } = e;
    let { value } = e;
    // fix any variables
    value = result.substituteStrings(value, sections);
    result.strings.push(new StringVarItem(id, value, sections));
  }
  addSet(result: Vars, e: LDMLKeyboard.LKSet, sections: DependencySections): void {
    const { id } = e;
    let { value } = e;
    // first substitute strings
    value = result.substituteStrings(value, sections);
    // OK to do this as a substitute, because we've already validated the set above.
    value = result.substituteSets(value, sections);
    const items : string[] = VariableParser.setSplitter(value);
    result.sets.push(new SetVarItem(id, items, sections));
  }
  addUnicodeSet(result: Vars, e: LDMLKeyboard.LKUnicodeSet, sections: DependencySections): void {
    const { id } = e;
    let { value } = e;
    value = result.substituteStrings(value, sections);
    value = result.substituteUnicodeSets(value, sections);
    result.unicodeSets.push(new UnicodeSetItem(id, value, sections, sections.usetparser));
  }
  // routines for using/substituting variables have been moved to the Vars class and its
  // properties
}
