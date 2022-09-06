import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Bksp, BkspItem, BkspItemFlags } from "../kmx/kmx-plus";
import { ElementString } from "../kmx/element-string";
import { LKBackspace, LKBackspaces } from "../ldml-keyboard/ldml-keyboard-xml";
import { SectionCompiler } from "./section-compiler";

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

  private compileBackspace(backspace: LKBackspace): BkspItem {
    let result = new BkspItem();
    result.from = new ElementString(backspace.from);
    result.to = backspace.to;
    result.before = new ElementString(backspace.before);
    result.flags = backspace.error == 'fail' ? BkspItemFlags.error : BkspItemFlags.none;
    return result;
  }

  private compileBackspaces(backspaces: LKBackspaces): Bksp {
    let result = new Bksp();

    if(backspaces?.backspace) {
      for(let backspace of backspaces.backspace) {
        result.items.push(this.compileBackspace(backspace));
      }
    }

    return result;
  }

  public compile(): Bksp {
    return this.compileBackspaces(this.keyboard.backspaces);
  }
}
