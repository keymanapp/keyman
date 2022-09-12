import { constants } from "@keymanapp/ldml-keyboard-constants";
import { GlobalSections, Ordr, OrdrItem } from "../kmx/kmx-plus";
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

  private compileReorder(sections: GlobalSections, reorder: LKReorder): OrdrItem {
    let result = new OrdrItem();
    result.elements = sections.elem.allocElementString(sections.strs, reorder.from, reorder.order, reorder.tertiary, reorder.tertiary_base, reorder.prebase);
    result.before = sections.elem.allocElementString(sections.strs, reorder.before);
    return result;
  }

  private compileReorders(sections: GlobalSections, reorders: LKReorders): Ordr {
    let result = new Ordr();

    if(reorders?.reorder) {
      for(let reorder of reorders.reorder) {
        result.items.push(this.compileReorder(sections, reorder));
      }
    }

    return result;
  }

  public compile(sections: GlobalSections): Ordr {
    return this.compileReorders(sections, this.keyboard.reorders);
  }
}
