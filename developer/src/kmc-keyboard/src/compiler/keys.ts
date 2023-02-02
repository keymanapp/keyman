import { constants } from '@keymanapp/ldml-keyboard-constants';
import { LDMLKeyboard, KMXPlus, Constants } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { SectionCompiler } from "./section-compiler.js";

import GlobalSections = KMXPlus.GlobalSections;
import Keys = KMXPlus.Keys;
import USVirtualKeyMap = Constants.USVirtualKeyMap;
import { calculateUniqueKeys, translateLayerAttrToModifier } from '../util/util.js';

export class KeysCompiler extends SectionCompiler {

  public get id() {
    return constants.section.keys;
  }

  private validateHardwareLayer(layer: LDMLKeyboard.LKLayer) {
    const uniqueKeys = calculateUniqueKeys([...this.keyboard.keys?.key]);
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

        let keydef = uniqueKeys.find(x => x.id == key);
        if(!keydef) {
          this.callbacks.reportMessage(CompilerMessages.Error_KeyNotFoundInKeyBag({keyId: key, col: x+1, row: y+1, layer: layer.id, form: 'hardware'}));
          valid = false;
          continue;
        }
        if (!keydef.to && !keydef.gap && !keydef.switch) {
          this.callbacks.reportMessage(CompilerMessages.Error_KeyMissingToGapOrSwitch({keyId: key}));
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

    // TODO-LDML: handle >1 layers!
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
      let sect = new Keys();
      for(let layer of this.keyboard.layers[0].layer) {
        this.compileHardwareLayer(sections, layer, sect);
      }
      return sect;
    }

    // TODO-LDML: generate vkey mapping for touch-only keys

    return null;
  }

  private compileHardwareLayer(
    sections: GlobalSections,
    layer: LDMLKeyboard.LKLayer,
    sect: Keys,
  ): Keys {
    const mod = translateLayerAttrToModifier(layer);

    let y = -1;
    for(let row of layer.row) {
      y++;

      const keys = row.keys.split(' ');
      let x = -1;
      for(let key of keys) {
        x++;

        let keydef = this.keyboard.keys?.key?.find(x => x.id == key);

        sect.keys.push({
          vkey: USVirtualKeyMap[y][x],
          mod: mod,
          to: sections.strs.allocAndUnescapeString(keydef.to),
          flags: 0 // Note: 'expand' is never set here, only by the .kmx builder
        });
      }
    }
    return sect;
  }
}
