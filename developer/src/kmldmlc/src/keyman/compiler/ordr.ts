import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Ordr, OrdrItem } from "../kmx/kmx-plus";
import { ElementString } from "../kmx/element-string";
import { LKReorder, LKReorders } from "../ldml-keyboard/ldml-keyboard-xml";
import { SectionCompiler } from "./section-compiler";

export class OrdrCompiler extends SectionCompiler {

  public get id() {
    return constants.section.ordr;
  }

  public validate(): boolean {
    let valid = true;
    // TODO-LDML: linting here should check for identical before+from, but this involves a double-parse which is ugly
    // TODO-LDML: unicodesets means that either we fully parse them and verify conflicting rules or the linting is imperfect
    return valid;
  }

  private compileReorder(reorder: LKReorder): OrdrItem {
    let result = new OrdrItem();
    result.elements = new ElementString(reorder.from, reorder.order, reorder.tertiary, reorder.tertiary_base, reorder.prebase);
    result.before = new ElementString(reorder.before);
    return result;
  }

  private compileReorders(reorders: LKReorders): Ordr {
    let result = new Ordr();

    if(reorders?.reorder) {
      for(let reorder of reorders.reorder) {
        result.items.push(this.compileReorder(reorder));
      }
    }

    return result;
  }

  public compile(): Ordr {
    return this.compileReorders(this.keyboard.reorders);
  }
}
