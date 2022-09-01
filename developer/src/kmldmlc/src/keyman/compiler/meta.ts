import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KeyboardSettings, Meta, Meta_NormalizationForm } from "../kmx/kmx-plus";
import { isValidEnumValue } from "../util/util";
import { CompilerErrors } from "./errors";
import { SectionCompiler } from "./section-compiler";

export class MetaCompiler extends SectionCompiler {

  public get id() {
    return constants.section.meta;
  }

  public validate(): boolean {
    let valid = true;

    const normalization = this.keyboard.info?.normalization;
    if(normalization !== undefined) {
      if(!isValidEnumValue(Meta_NormalizationForm, normalization)) {
        this.callbacks.reportMessage(CompilerErrors.InvalidNormalization({form: normalization}));
        valid = false;
      }
    }

    return valid;
  }

  public compile(): Meta {
    let result = new Meta();
    result.name = this.keyboard.names?.name?.[0]?.value;
    result.author = this.keyboard.info?.author;
    result.conform = this.keyboard.conformsTo;
    result.layout = this.keyboard.info?.layout;
    result.normalization = this.keyboard.info?.normalization as Meta_NormalizationForm;
    result.indicator = this.keyboard.info?.indicator;
    result.settings =
      (this.keyboard.settings?.fallback == "omit" ? KeyboardSettings.fallback : 0) |
      (this.keyboard.settings?.transformFailure == "omit" ? KeyboardSettings.transformFailure : 0) |
      (this.keyboard.settings?.transformPartial == "hide" ? KeyboardSettings.transformPartial : 0);
    return result;
  }
}