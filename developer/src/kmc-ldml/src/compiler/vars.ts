import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LDMLKeyboard, CompilerCallbacks } from '@keymanapp/common-types';
import { UnicodeSetParser } from '@keymanapp/common-types';
import { KmnCompiler } from  '@keymanapp/kmc-kmn';
import { SectionCompiler } from "./section-compiler.js";
import Vars = KMXPlus.Vars;
import StringVarItem = KMXPlus.StringVarItem;
import SetVarItem = KMXPlus.SetVarItem;
import UnicodeSetItem = KMXPlus.UnicodeSetItem;
import GlobalSections = KMXPlus.GlobalSections;
import LDMLKeyboardXMLSourceFile = LDMLKeyboard.LDMLKeyboardXMLSourceFile;
import { CompilerMessages } from "./messages.js";
import { VariableParser } from "../util/pattern-parser.js";
export class VarsCompiler extends SectionCompiler {
  public get id() {
    return constants.section.vars;
  }

  usetparser : UnicodeSetParser = null;

  public async init() : Promise<boolean> {
    const compiler = new KmnCompiler();
    const ok = await compiler.init(this.callbacks);
    if (ok) {
      this.usetparser = compiler;
    }
    return ok;
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

    const allStrings = new Set();
    const allSets = new Set();

    // not worrying about dups, will deal with that later

    // Strings
    for (const {id, value} of variables.string) {
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
      allSets.add(id);
      // For strings, we only care about illegal references, here.
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
          this.callbacks.reportMessage(CompilerMessages.Error_NeedSpacesBetweenSetVariables({item}));
        }
        for (const id2 of setrefs) {
          if (!allSets.has(id2)) {
            valid = false;
            this.callbacks.reportMessage(CompilerMessages.Error_MissingSetVariable({id: id2}));
          }
        }
        // TODO-LDML: Are there other illegal cases here? what about "x$[set]"?
      }
    }
    // UnicodeSets

    // Now check for dups
    const allVars = [
      variables?.string,
      variables?.set,
      variables?.unicodeSet];
    for (const vars of allVars) {
      for (const {id} of vars) {
        if (allIds.has(id)) {
          dups.add(id);
        } else {
          allIds.add(id);
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
    return valid;
  }

  public compile(sections: GlobalSections): Vars {
    const result =  new Vars();

    const variables = this.keyboard?.variables;

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

    if (!result.valid()) {
      return null;
    } else {
      return result;
    }
  }
  addString(result: Vars, e: LDMLKeyboard.LKString, sections: GlobalSections): void {
    const { id } = e;
    let { value } = e;
    // fix any variables
    value = this.substituteStrings(result, value, sections);
    result.strings.push(new StringVarItem(id, value, sections));
  }
  addSet(result: Vars, e: LDMLKeyboard.LKSet, sections: GlobalSections): void {
    const { id } = e;
    let { value } = e;
    // first substitute strings
    value = this.substituteStrings(result, value, sections);
    result.sets.push(new SetVarItem(id, VariableParser.setSplitter(value), sections));
  }
  addUnicodeSet(result: Vars, e: LDMLKeyboard.LKUnicodeSet, sections: GlobalSections): void {
    const { id } = e;
    let { value } = e;
    result.unicodeSets.push(new UnicodeSetItem(id, value, sections, this.usetparser));
  }
  substituteStrings(vars: Vars, str: string, sections: GlobalSections): string {
    return str.replaceAll(VariableParser.STRING_REFERENCE, (_entire, id) => {
      const val = this.findStringVariableValue(vars, id);
      if (val === null) {
        // Should have been caught during validation.
        throw Error(`Internal Error: reference to missing variable ${id}`);
      }
      return val;
    });
  }
  findStringVariableValue(vars: Vars, id: string): string {
    return this.findVariable(vars.strings, id)?.value?.value; // Unwrap: Variable, StrsItem
  }

  findVariable<T extends KMXPlus.VarsItem>(array: T[], id: string) : T {
    const v : T[] = array.filter(e => e.id.value === id);
    if (v.length === 0){
      return null;
    } else if (v.length !== 1) {
      throw Error(`Internal Error: Duplicate variable id ${id} crept into a variable list.`);
    } else {
      return v[0];
    }
  }
}
