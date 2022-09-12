import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Vkey } from "../kmx/kmx-plus";
import { LdmlVkeyNames } from "../ldml-keyboard/virtual-key-constants";
import { CompilerMessages } from "./messages";
import { SectionCompiler } from "./section-compiler";

export class VkeyCompiler extends SectionCompiler {

  public get id() {
    return constants.section.vkey;
  }

  public get required(): boolean {
    return !!this.keyboard.vkeyMaps;
  }

  public validate(): boolean {
    let valid = true;
    if(this.keyboard.vkeyMaps) {
      let from: string[] = [], to: string[] = [];

      this.keyboard.vkeyMaps.vkeyMap.forEach(vk => {
        if(LdmlVkeyNames[vk.from] === undefined) {
          this.callbacks.reportMessage(CompilerMessages.Error_VkeyIsNotValid(vk.from));
          valid = false;
        }

        if(LdmlVkeyNames[vk.to] === undefined) {
          this.callbacks.reportMessage(CompilerMessages.Error_VkeyIsNotValid(vk.to));
          valid = false;
        }

        if(vk.from == vk.to) {
          this.callbacks.reportMessage(CompilerMessages.Hint_VkeyMapIsRedundant(vk.from));
        }

        if(from.find(svk => svk == vk.from)) {
          this.callbacks.reportMessage(CompilerMessages.Error_VkeyMapIsRepeated(vk.from));
          valid = false;
        }
        from.push(vk.from);

        if(to.find(svk => svk == vk.to)) {
          this.callbacks.reportMessage(CompilerMessages.Info_MultipleVkeyMapsHaveSameTarget(vk.to));
        }
        to.push(vk.to);
      });
    }
    return valid;
  }

  public compile(): Vkey {
    let result = new Vkey();
    if(!this.keyboard.vkeyMaps) {
      return result;
    }

    result.vkeys = this.keyboard.vkeyMaps?.vkeyMap.map(vk => {
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
