import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LDMLKeyboard } from "@keymanapp/common-types";
import { SectionCompiler } from "./section-compiler.js";

import LKBackspace = LDMLKeyboard.LKBackspace;
import LKBackspaces = LDMLKeyboard.LKBackspaces;
import GlobalSections = KMXPlus.GlobalSections;
import Bksp = KMXPlus.Bksp;
import BkspItem = KMXPlus.BkspItem;
import BkspItemFlags = KMXPlus.BkspItemFlags;

export class BkspCompiler extends SectionCompiler {

  public get id() {
    return constants.section.bksp;
  }

  public validate(): boolean {
    let valid = true;
    // TODO-LDML: linting here should check for identical before+from, but this involves a double-parse which is ugly
    // TODO-LDML: unicodesets means that either we fully parse them and verify conflicting rules or the linting is imperfect
    return valid;
  }

  private compileBackspace(sections: GlobalSections, backspace: LKBackspace): BkspItem {
    let result = new KMXPlus.BkspItem();
    result.from = sections.elem.allocElementString(sections.strs, backspace.from);
    result.to = sections.strs.allocString(backspace.to);
    result.before = sections.elem.allocElementString(sections.strs, backspace.before);
    result.flags = backspace.error == 'fail' ? BkspItemFlags.error : BkspItemFlags.none;
    return result;
  }

  private compileBackspaces(sections: GlobalSections, backspaces: LKBackspaces): Bksp {
    let result = new Bksp();

    if(backspaces?.backspace) {
      for(let backspace of backspaces.backspace) {
        result.items.push(this.compileBackspace(sections, backspace));
      }
    }

    return result;
  }

  public compile(sections: GlobalSections): Bksp {
    return this.compileBackspaces(sections, this.keyboard.backspaces);
  }
}
