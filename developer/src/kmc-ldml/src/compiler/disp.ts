import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LDMLKeyboard, MarkerParser } from '@keymanapp/common-types';

import { CompilerMessages } from "./messages.js";
import { SectionCompiler } from "./section-compiler.js";

import DependencySections = KMXPlus.DependencySections;
import Disp = KMXPlus.Disp;
import DispItem = KMXPlus.DispItem;

export class DispCompiler extends SectionCompiler {
  static validateMarkers(keyboard: LDMLKeyboard.LKKeyboard, emitMarkers: Set<string>, matchMarkers: Set<string>): boolean {
    keyboard.displays?.display?.forEach(({ to }) => {
      MarkerParser.allReferences(to).forEach(marker => matchMarkers.add(marker));
    });
    return true;
  }

  public get id() {
    return constants.section.disp;
  }

  public validate(): boolean {
    let valid = true;

    const tos = new Set();

    if (this.keyboard.displays?.display) {
      for (const { to } of this.keyboard.displays?.display) {
        if (tos.has(to)) {
          this.callbacks.reportMessage(CompilerMessages.Error_DisplayIsRepeated({ to }));
          return false;
        }
        tos.add(to);
      }
    }

    return valid;
  }

  public compile(sections: DependencySections): Disp {
    let result = new Disp();

    // displayOptions
    result.baseCharacter = sections.strs.allocAndUnescapeString(this.keyboard.displays?.displayOptions?.baseCharacter);

    // displays
    result.disps = this.keyboard.displays?.display.map(display => ({
      to: sections.strs.allocAndUnescapeString(display.to),
      display: sections.strs.allocAndUnescapeString(display.display),
    })) || []; // TODO-LDML: need coverage for the []

    result.disps.sort((a: DispItem, b: DispItem) => a.to.compareTo(b.to));

    return result;
  }
}
