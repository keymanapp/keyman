import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus } from '@keymanapp/common-types';
import { SectionCompiler } from "./section-compiler.js";

import GlobalSections = KMXPlus.GlobalSections;
import Name = KMXPlus.Name;

export class NameCompiler extends SectionCompiler {

  public get id() {
    return constants.section.name;
  }

  public validate(): boolean {
    let valid = true;
    valid = (this.keyboard.names?.name?.length ?? 0) > 0;
    return valid;
  }

  public compile(sections: GlobalSections): Name {
    let result = new Name();
    result.names = this.keyboard.names?.name?.map(v => sections.strs.allocString(v.value)) ?? [];
    return result;
  }
}
