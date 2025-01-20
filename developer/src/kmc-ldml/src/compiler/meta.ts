import { SectionIdent, constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus } from '@keymanapp/common-types';

import { LdmlCompilerMessages } from "./ldml-compiler-messages.js";
import { SectionCompiler } from "./section-compiler.js";
import semver from "semver";

import DependencySections = KMXPlus.DependencySections;
import Meta = KMXPlus.Meta;
import KeyboardSettings = KMXPlus.KeyboardSettings;

export class MetaCompiler extends SectionCompiler {

  public get id() {
    return constants.section.meta;
  }

  public validate(): boolean {
    let valid = true;

    valid &&= this.validateNormalization(this.keyboard3.settings?.normalization);
    valid &&= this.validateVersion(this.keyboard3.version?.number);

    return valid;
  }

  private validateVersion(versionNumber?: string) {
    if(versionNumber !== undefined) {
      if(versionNumber.match(/^[=v]/i)) {
        // semver ignores a preceding '=' or 'v'
        this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidVersion({ version: versionNumber }));
        return false;
      }
      if(!semver.parse(versionNumber, {loose: false})) {
        this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidVersion({ version: versionNumber }));
        return false;
      }
    }
    return true;
  }

  private validateNormalization(normalization?: string) {
    if (normalization === 'disabled') {
      this.callbacks.reportMessage(LdmlCompilerMessages.Hint_NormalizationDisabled());
    }
    return true;
  }

  public get dependencies(): Set<SectionIdent> {
    const strsOnly = new Set(<SectionIdent[]>[constants.section.strs]);
    return strsOnly;
  }

  public compile(sections: DependencySections): Meta {
    let result = new Meta();
    result.author        = sections.strs.allocString(this.keyboard3.info?.author);
    result.conform       = sections.strs.allocString(this.keyboard3.conformsTo);
    result.layout        = sections.strs.allocString(this.keyboard3.info?.layout);
    result.name          = sections.strs.allocString(this.keyboard3.info?.name);
    result.indicator     = sections.strs.allocString(this.keyboard3.info?.indicator);
    result.version       = sections.strs.allocString(this.keyboard3.version?.number ?? "0.0.0");
    result.settings =
      (this.keyboard3.settings?.normalization == "disabled" ? KeyboardSettings.normalizationDisabled : 0);
    return result;
  }
}
