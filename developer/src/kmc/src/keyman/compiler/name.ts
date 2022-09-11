import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Name } from "../kmx/kmx-plus";
import { SectionCompiler } from "./section-compiler";

export class NameCompiler extends SectionCompiler {

  public get id() {
    return constants.section.name;
  }

  public validate(): boolean {
    let valid = true;
    valid = (this.keyboard.names?.name?.length ?? 0) > 0;
    return valid;
  }

  public compile(): Name {
    let result = new Name();
    result.names = this.keyboard.names?.name?.map(v => v.value) ?? [];
    return result;
  }
}
