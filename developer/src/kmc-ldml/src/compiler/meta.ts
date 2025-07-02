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
        this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidVersion({ version: versionNumber }, this.keyboard3));
        return false;
      }
      if(!semver.parse(versionNumber, {loose: false})) {
        this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidVersion({ version: versionNumber }, this.keyboard3));
        return false;
      }
    }
    return true;
  }

  private validateNormalization(normalization?: string) {
    if (normalization === 'disabled') {
      this.callbacks.reportMessage(LdmlCompilerMessages.Hint_NormalizationDisabled(this.keyboard3.settings));
    }
    return true;
  }

  public get dependencies(): Set<SectionIdent> {
    const strsOnly = new Set(<SectionIdent[]>[constants.section.strs]);
    return strsOnly;
  }

  public compile(sections: DependencySections): Meta {
    const result = new Meta();
    result.author        = sections.strs.allocString(this.keyboard3.info?.author,
                                                 {x: this.keyboard3.info});
    result.conform       = sections.strs.allocString(this.keyboard3.conformsTo,
                                                 {x: this.keyboard3});
    result.layout        = sections.strs.allocString(this.keyboard3.info?.layout,
                                                 {x: this.keyboard3.info});
    result.name          = sections.strs.allocString(this.keyboard3.info?.name,
                                                 {x: this.keyboard3.info});
    result.indicator     = sections.strs.allocString(this.keyboard3.info?.indicator,
                                                 {x: this.keyboard3.info});
    result.version       = sections.strs.allocString(this.keyboard3.version?.number ?? "0.0.0",
                                                 {x: this.keyboard3.version});
    result.settings =
      (this.keyboard3.settings?.normalization == "disabled" ? KeyboardSettings.normalizationDisabled : 0);
    return result;
  }
}
