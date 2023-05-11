// import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LDMLKeyboard, CompilerCallbacks } from '@keymanapp/common-types';
import { UnicodeSetParser } from '@keymanapp/common-types';
import { Compiler } from  '@keymanapp/kmc-kmn';
import { SectionCompiler } from "./section-compiler.js";
import Vars = KMXPlus.Vars;
import StringVarItem = KMXPlus.StringVarItem;
import SetVarItem = KMXPlus.SetVarItem;
import UnicodeSetItem = KMXPlus.UnicodeSetItem;
import GlobalSections = KMXPlus.GlobalSections;

import LDMLKeyboardXMLSourceFile = LDMLKeyboard.LDMLKeyboardXMLSourceFile;
// import LKTransform = LDMLKeyboard.LKTransform;
// import LKTransforms = LDMLKeyboard.LKTransforms;
import { CompilerMessages } from "./messages.js";

export class VarsCompiler extends SectionCompiler {
  usetparser : UnicodeSetParser = null;

  public async init() : Promise<boolean> {
    const compiler = new Compiler();
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
        ids: Array.of(dups.values()).join(', ')
      }));
      valid = false;
    }
    return valid;
  }

  public compile(sections: GlobalSections): Vars {
    const result =  new Vars();
    // TODO-LDML: need kmc-kmn compiler or uset

    const variables = this.keyboard?.variables;

    // we know the variables do not conflict with each other
    result.sets =
      variables?.set?.map(({ id, value }) =>
        new SetVarItem(id, value, sections));

    result.strings =
      variables?.string?.map(({ id, value }) =>
        new StringVarItem(id, value, sections));

    result.unicodeSets =
      variables?.unicodeSet?.map(({ id, value }) =>
        new UnicodeSetItem(id, value, sections, this.usetparser));

    return result;
  }
}
