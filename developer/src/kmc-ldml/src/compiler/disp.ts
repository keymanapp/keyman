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
    keyboard.displays?.display?.forEach(({ output }) =>
      mt.add(MarkerUse.match, MarkerParser.allReferences(output)));
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
      for (const { output, keyId } of this.keyboard3.displays?.display) {
        if ((output && keyId) || (!output && !keyId)) {
          this.callbacks.reportMessage(CompilerMessages.Error_DisplayNeedsToOrId({ output, keyId }));
          return false;
        } else if (output) {
          if (tos.has(output)) {
            this.callbacks.reportMessage(CompilerMessages.Error_DisplayIsRepeated({ output }));
            return false;
          } else {
            tos.add(output);
          }
        } else if (keyId) {
          if (ids.has(keyId)) {
            this.callbacks.reportMessage(CompilerMessages.Error_DisplayIsRepeated({ keyId }));
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
    let result = new Disp();

    // displayOptions
    result.baseCharacter = sections.strs.allocString(this.keyboard3.displays?.displayOptions?.baseCharacter, {unescape: true});

    // displays
    result.disps = this.keyboard3.displays?.display.map(display => ({
      to: sections.strs.allocString(display.output, {
        stringVariables: true,
        markers: true,
        unescape: true,
      }, sections),
      id: sections.strs.allocString(display.keyId), // not escaped, not substituted
      display: sections.strs.allocString(display.display, {
        stringVariables: true,
        unescape: true,
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
