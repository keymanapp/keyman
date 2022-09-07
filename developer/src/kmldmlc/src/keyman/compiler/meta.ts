import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KeyboardSettings, Meta, Meta_NormalizationForm } from "../kmx/kmx-plus";
import { isValidEnumValue } from "../util/util";
import { CompilerMessages } from "./messages";
import { SectionCompiler } from "./section-compiler";
import * as semver from "semver";

export class MetaCompiler extends SectionCompiler {

  public get id() {
    return constants.section.meta;
  }

  public validate(): boolean {
    let valid = true;

    valid &&= this.validateNormalization(this.keyboard.info?.normalization);
    valid &&= this.validateVersion(this.keyboard.version?.number);

    return valid;
  }

  private validateVersion(versionNumber?: string) {
    if(versionNumber !== undefined) {
      if(versionNumber.match(/^[=v]/i)) {
        // semver ignores a preceding '=' or 'v'
        return false;
      }
      return !!semver.parse(versionNumber, {loose: false, includePrerelease: true});
    }
    return true;
  }

  private validateNormalization(normalization?: string) {
    if (normalization !== undefined) {
      if (!isValidEnumValue(Meta_NormalizationForm, normalization)) {
        this.callbacks.reportMessage(CompilerMessages.Error_InvalidNormalization({ form: normalization }));
        return false;
      }
    }
    return true;
  }

  public compile(): Meta {
    let result = new Meta();
    result.name = this.keyboard.names?.name?.[0]?.value;
    result.author = this.keyboard.info?.author;
    result.conform = this.keyboard.conformsTo;
    result.layout = this.keyboard.info?.layout;
    result.normalization = this.keyboard.info?.normalization as Meta_NormalizationForm;
    result.indicator = this.keyboard.info?.indicator;
    result.version = this.keyboard.version?.number ?? "0";
    result.settings =
      (this.keyboard.settings?.fallback == "omit" ? KeyboardSettings.fallback : 0) |
      (this.keyboard.settings?.transformFailure == "omit" ? KeyboardSettings.transformFailure : 0) |
      (this.keyboard.settings?.transformPartial == "hide" ? KeyboardSettings.transformPartial : 0);
    return result;
  }
}