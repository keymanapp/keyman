import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus } from '@keymanapp/common-types';

import { CompilerMessages } from "./messages.js";
import { SectionCompiler } from "./section-compiler.js";

import GlobalSections = KMXPlus.GlobalSections;
import Disp = KMXPlus.Disp;
import DispItem = KMXPlus.DispItem;

export class DispCompiler extends SectionCompiler {

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

  public compile(sections: GlobalSections): Disp {
    let result = new Disp();

    // displayOptions
    result.baseCharacter = sections.strs.allocString(this.keyboard.displays?.displayOptions?.baseCharacter);

    // displays
    result.disps = this.keyboard.displays?.display.map(display => ({
      to: sections.strs.allocString(display.to),
      display: sections.strs.allocString(display.display),
    })) || [];

    // TODO-LDML: Same function in comon/web/types/src
    function binaryStringCompare(a: string, b: string) : number {
      if(a < b) {
        return -1;
      } else if(a > b) {
        return 1;
      } else {
        return 0;
      }
    }

    result.disps.sort((a: DispItem, b: DispItem) => binaryStringCompare(a.to.value, b.to.value));

    return result;
  }
}
