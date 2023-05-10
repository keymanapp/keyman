// import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LDMLKeyboard, CompilerCallbacks } from '@keymanapp/common-types';
import { SectionCompiler } from "./section-compiler.js";
import Vars = KMXPlus.Vars;
import GlobalSections = KMXPlus.GlobalSections;

import LDMLKeyboardXMLSourceFile = LDMLKeyboard.LDMLKeyboardXMLSourceFile;
// import LKTransform = LDMLKeyboard.LKTransform;
// import LKTransforms = LDMLKeyboard.LKTransforms;
import { CompilerMessages } from "./messages.js";

export class VarsCompiler extends SectionCompiler {

  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
  }

  public validate(): boolean {
    let valid = true;
    // TODO-LDML markers?

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

    return result;
  }
}
