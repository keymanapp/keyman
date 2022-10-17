import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, Constants } from '@keymanapp/common-types';
import { CompilerMessages } from "./messages.js";
import { SectionCompiler } from "./section-compiler.js";

import Vkey = KMXPlus.Vkey;
import LdmlVkeyNames = Constants.LdmlVkeyNames;

export class VkeyCompiler extends SectionCompiler {

  public get id() {
    return constants.section.vkey;
  }

  public get required(): boolean {
    return !!this.keyboard.vkeys;
  }

  public validate(): boolean {
    let valid = true;
    if(this.keyboard.vkeys) {
      let from: string[] = [], to: string[] = [];

      this.keyboard.vkeys.vkey.forEach(vk => {
        if(LdmlVkeyNames[vk.from] === undefined) {
          this.callbacks.reportMessage(CompilerMessages.Error_VkeyIsNotValid({vkey: vk.from}));
          valid = false;
        }

        if(LdmlVkeyNames[vk.to] === undefined) {
          this.callbacks.reportMessage(CompilerMessages.Error_VkeyIsNotValid({vkey: vk.to}));
          valid = false;
        }

        if(vk.from == vk.to) {
          this.callbacks.reportMessage(CompilerMessages.Hint_VkeyMapIsRedundant({vkey: vk.from}));
        }

        if(from.find(svk => svk == vk.from)) {
          this.callbacks.reportMessage(CompilerMessages.Error_VkeyMapIsRepeated({vkey: vk.from}));
          valid = false;
        }
        from.push(vk.from);

        if(to.find(svk => svk == vk.to)) {
          this.callbacks.reportMessage(CompilerMessages.Info_MultipleVkeyMapsHaveSameTarget({vkey: vk.to}));
        }
        to.push(vk.to);
      });
    }
    return valid;
  }

  public compile(): Vkey {
    let result = new Vkey();
    if(!this.keyboard.vkeys) {
      return result;
    }

    result.vkeys = this.keyboard.vkeys?.vkey.map(vk => {
      return {
        vkey: LdmlVkeyNames[vk.from],
        target: LdmlVkeyNames[vk.to]
      };
    });
    // Sort according to vkey binary order, per C7043
    result.vkeys.sort((a,b) => a.vkey - b.vkey);
    return result;
  }
}
