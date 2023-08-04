import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus } from '@keymanapp/common-types';

import { isValidEnumValue } from "../util/util.js";
import { CompilerMessages } from "./messages.js";
import { SectionCompiler } from "./section-compiler.js";
import semver from "semver";

import DependencySections = KMXPlus.DependencySections;
import Meta = KMXPlus.Meta;
import Meta_NormalizationForm = KMXPlus.Meta_NormalizationForm;
import KeyboardSettings = KMXPlus.KeyboardSettings;

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
        this.callbacks.reportMessage(CompilerMessages.Error_InvalidVersion({ version: versionNumber }));
        return false;
      }
      if(!semver.parse(versionNumber, {loose: false})) {
        this.callbacks.reportMessage(CompilerMessages.Error_InvalidVersion({ version: versionNumber }));
        return false;
      }
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

  public compile(sections: DependencySections): Meta {
    let result = new Meta();
    result.author        = sections.strs.allocString(this.keyboard.info?.author);
    result.conform       = sections.strs.allocString(this.keyboard.conformsTo);
    result.layout        = sections.strs.allocString(this.keyboard.info?.layout);
    result.normalization = sections.strs.allocString(this.keyboard.info?.normalization);
    result.indicator     = sections.strs.allocString(this.keyboard.info?.indicator);
    result.version       = sections.strs.allocString(this.keyboard.version?.number ?? "0.0.0");
    result.settings =
      (this.keyboard.settings?.fallback == "omit" ? KeyboardSettings.fallback : 0) |
      (this.keyboard.settings?.transformFailure == "omit" ? KeyboardSettings.transformFailure : 0) |
      (this.keyboard.settings?.transformPartial == "hide" ? KeyboardSettings.transformPartial : 0);
    return result;
  }
}
