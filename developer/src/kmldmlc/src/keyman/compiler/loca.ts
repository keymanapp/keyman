import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Loca } from "../kmx/kmx-plus";
import { SectionCompiler } from "./section-compiler";

export class LocaCompiler extends SectionCompiler {

  public get id() {
    return constants.section.loca;
  }

  public compile(): Loca {
    let result = new Loca();
    result.locales.push(this.source.keyboard.locale);
    return result;
  }
}
