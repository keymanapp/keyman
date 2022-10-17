import { constants } from '@keymanapp/ldml-keyboard-constants';
import { LDMLKeyboard, KMXPlus, Constants } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { SectionCompiler } from "./section-compiler.js";

import GlobalSections = KMXPlus.GlobalSections;
import Keys = KMXPlus.Keys;
import USVirtualKeyMap = Constants.USVirtualKeyMap;

export class KeysCompiler extends SectionCompiler {

  public get id() {
    return constants.section.keys;
  }

  private validateHardwareLayer(layer: LDMLKeyboard.LKLayerMap) {
    let valid = true;
    if(layer.row.length > USVirtualKeyMap.length) {
      this.callbacks.reportMessage(CompilerMessages.Error_HardwareLayerHasTooManyRows());
      valid = false;
    }

    for(let y = 0; y < layer.row.length && y < USVirtualKeyMap.length; y++) {
      const keys = layer.row[y].keys.split(' ');

      if(keys.length > USVirtualKeyMap[y].length) {
        this.callbacks.reportMessage(CompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({row: y+1}));
        valid = false;
      }

      let x = -1;
      for(let key of keys) {
        x++;

        let keydef = this.keyboard.keys?.key?.find(x => x.id == key);
        if(!keydef) {
          this.callbacks.reportMessage(CompilerMessages.Error_KeyNotFoundInKeyBag({keyId: key, col: x+1, row: y+1, layer: layer.id, form: 'hardware'}));
          valid = false;
          continue;
        }
      }
    }

    return valid;
  }

  public validate() {
    let valid = true;
    if(!this.keyboard.layers?.[0]?.layer?.length) {
      valid = false;
      this.callbacks.reportMessage(CompilerMessages.Error_MustBeAtLeastOneLayerElement());
    }

    if(this.keyboard.layers?.[0]?.form == 'hardware') {
      for(let layer of this.keyboard.layers[0].layer) {
        valid = this.validateHardwareLayer(layer) && valid; // note: always validate even if previously invalid results found
      }
    }
    return valid;
  }

  public compile(sections: GlobalSections): Keys {
    // Use LayerMap + keys to generate compiled keys for hardware

    if(this.keyboard.layers?.[0]?.form == 'hardware') {
      for(let layer of this.keyboard.layers[0].layer) {
        let sect = this.compileHardwareLayer(sections, layer);
        return sect;
      }
    }

    // TODO: generate vkey mapping for touch-only keys

    return null;
  }

  private compileHardwareLayer(
    sections: GlobalSections,
    layer: LDMLKeyboard.LKLayerMap
  ): Keys {
    let result = new Keys();
    const mod = this.translateLayerIdToModifier(layer.id);

    let y = -1;
    for(let row of layer.row) {
      y++;

      const keys = row.keys.split(' ');
      let x = -1;
      for(let key of keys) {
        x++;

        let keydef = this.keyboard.keys?.key?.find(x => x.id == key);

        result.keys.push({
          vkey: USVirtualKeyMap[y][x],
          mod: mod,
          to: sections.strs.allocString(keydef.to),
          flags: 0 // Note: 'expand' is never set here, only by the .kmx builder
        });
      }
    }

    return result;
  }

  private translateLayerIdToModifier(id: string) {
    if(id == 'base') {
      return 0;
    }
    // TODO: other modifiers
    return 0;
  }
}
