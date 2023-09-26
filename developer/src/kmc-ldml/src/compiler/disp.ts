import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LDMLKeyboard, MarkerParser } from '@keymanapp/common-types';

import { CompilerMessages } from "./messages.js";
import { SectionCompiler } from "./section-compiler.js";

import DependencySections = KMXPlus.DependencySections;
import Disp = KMXPlus.Disp;
import DispItem = KMXPlus.DispItem;
import { MarkerTracker, MarkerUse } from "./marker-tracker.js";

export class DispCompiler extends SectionCompiler {
  static validateMarkers(keyboard: LDMLKeyboard.LKKeyboard, mt : MarkerTracker): boolean {
    keyboard.displays?.display?.forEach(({ to }) =>
      mt.add(MarkerUse.match, MarkerParser.allReferences(to)));
    // no marker references in 'id'
    return true;
  }

  public get id() {
    return constants.section.disp;
  }

  public validate(): boolean {
    let valid = true;

    const tos = new Set();
    const ids = new Set();

    if (this.keyboard3.displays?.display) {
      for (const { to, id } of this.keyboard3.displays?.display) {
        if ((to && id) || (!to && !id)) {
          this.callbacks.reportMessage(CompilerMessages.Error_DisplayNeedsToOrId({ to, id }));
          return false;
        } else if (to) {
          if (tos.has(to)) {
            this.callbacks.reportMessage(CompilerMessages.Error_DisplayIsRepeated({ to }));
            return false;
          } else {
            tos.add(to);
          }
        } else if (id) {
          if (ids.has(id)) {
            this.callbacks.reportMessage(CompilerMessages.Error_DisplayIsRepeated({ id }));
            return false;
          } else {
            ids.add(id);
          }
        }
      }
    }

    return valid;
  }

  public compile(sections: DependencySections): Disp {
    let result = new Disp();

    // displayOptions
    result.baseCharacter = sections.strs.allocAndUnescapeString(this.keyboard3.displays?.displayOptions?.baseCharacter);

    // TODO-LDML: substitute variables!

    // displays
    result.disps = this.keyboard3.displays?.display.map(display => ({
      to: sections.strs.allocAndUnescapeString(sections.vars.substituteMarkerString(display.to)),
      id: sections.strs.allocString(display.id), // not escaped, not substituted
      display: sections.strs.allocAndUnescapeString(display.display),
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
