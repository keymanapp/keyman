import { constants } from "@keymanapp/ldml-keyboard-constants";
import { LDMLKeyboard } from '@keymanapp/developer-utils';
import { KMXPlus } from "@keymanapp/common-types";

import { LdmlCompilerMessages } from "./ldml-compiler-messages.js";
import { SectionCompiler } from "./section-compiler.js";

import DependencySections = KMXPlus.DependencySections;
import Disp = KMXPlus.Disp;
import DispItem = KMXPlus.DispItem;
import { SubstitutionUse, Substitutions } from "./substitution-tracker.js";

export class DispCompiler extends SectionCompiler {
  static validateSubstitutions(keyboard: LDMLKeyboard.LKKeyboard, st : Substitutions): boolean {
    keyboard.displays?.display?.forEach((e) => {
      const { display, output } = e;
      st.addStringAndMarkerSubstitution(SubstitutionUse.match, output, e);
      st.addStringSubstitution(SubstitutionUse.emit, display, e);
    });
    // no marker references in 'id'
    return true;
  }

  public get id() {
    return constants.section.disp;
  }

  public validate(): boolean {
    const valid = true;

    const tos = new Set();
    const ids = new Set();

    if (this.keyboard3.displays?.display) {
      for (const display of this.keyboard3.displays?.display) {
        const { output, keyId } = display;
        if ((output && keyId) || (!output && !keyId)) {
          this.callbacks.reportMessage(LdmlCompilerMessages.Error_DisplayNeedsToOrId({ display: display.display }, display));
          return false;
        } else if (output) {
          if (tos.has(output)) {
            this.callbacks.reportMessage(LdmlCompilerMessages.Error_DisplayIsRepeated({ display: display.display }, display));
            return false;
          } else {
            tos.add(output);
          }
        } else if (keyId) {
          if (ids.has(keyId)) {
            this.callbacks.reportMessage(LdmlCompilerMessages.Error_DisplayIsRepeated({ display: display.display }, display));
            return false;
          } else {
            ids.add(keyId);
          }
        }
      }
    }

    return valid;
  }

  public compile(sections: DependencySections): Disp {
    const result = new Disp();

    // displayOptions
    result.baseCharacter = sections.strs.allocString(this.keyboard3.displays?.displayOptions?.baseCharacter, { unescape: true, x: this.keyboard3?.displays?.displayOptions });

    // displays
    result.disps = this.keyboard3.displays?.display.map(display => ({
      to: sections.strs.allocString(display.output, {
        stringVariables: true,
        markers: true,
        unescape: true,
        x: display,
      }, sections),
      id: sections.strs.allocString(display.keyId, { x: display }), // not escaped, not substituted
      display: sections.strs.allocString(display.display, {
        stringVariables: true,
        unescape: true,
        x: display,
      }, sections),
    })) || []; // TODO-LDML: need coverage for the []

    result.disps.sort((a: DispItem, b: DispItem) => {
      // sort 'id' first (empty string will be lower)
      const idDiff = a.id.compareTo(b.id);
      if (idDiff != 0) {
        return idDiff;
      } else {
        // sort by 'to'
        return a.to.compareTo(b.to);
      }
    });

    return result;
  }
}
