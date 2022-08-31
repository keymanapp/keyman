import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Meta } from "../kmx/kmx-plus";
import { SectionCompiler } from "./section-compiler";

export class MetaCompiler extends SectionCompiler {

  public get id() {
    return constants.section.meta;
  }

  public validate(): boolean {
    //
    return true;
  }

  public compile(): Meta {
    let result = new Meta();
    result.name = this.source.keyboard.names?.name?.[0]?.value;
    result.author = this.source.keyboard.info?.author;
    result.conform = this.source.keyboard.conformsTo;
    result.layout = this.source.keyboard.info?.layout;
    result.normalization = this.source.keyboard.info?.normalization;
    result.indicator = this.source.keyboard.info?.indicator;
    result.settings = 0;
    return result;
  }
}