import { constants } from '@keymanapp/ldml-keyboard-constants';
import { LDMLKeyboard, KMXPlus, Constants } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { SectionCompiler } from "./section-compiler.js";

import GlobalSections = KMXPlus.GlobalSections;
import Keys = KMXPlus.Keys;
import { calculateUniqueKeys, translateLayerAttrToModifier, validModifier } from '../util/util.js';

export class KeysCompiler extends SectionCompiler {

  public get id() {
    return constants.section.keys;
  }

  private validateHardwareLayer(hardware: string, layer: LDMLKeyboard.LKLayer) {
    let valid = true;

    const {modifier} = layer;
    if (!validModifier(modifier)) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidModifier({modifier, layer: layer.id}));
      valid = false;
    }

    const keymap = Constants.HardwareToKeymap.get(hardware);
    if (!keymap) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidHardware({hardware}));
      valid = false;
      return valid; // can't do anything else here
    }

    const uniqueKeys = calculateUniqueKeys([...this.keyboard.keys?.key]);
    if(layer.row.length > keymap.length) {
      this.callbacks.reportMessage(CompilerMessages.Error_HardwareLayerHasTooManyRows());
      valid = false;
    }

    for(let y = 0; y < layer.row.length && y < keymap.length; y++) {
      const keys = layer.row[y].keys.split(' ');

      if(keys.length > keymap[y].length) {
        this.callbacks.reportMessage(CompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({row: y+1, hardware}));
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

    const theLayers = this.keyboard.layers?.[0]; // TODO-LDML: handle >1 layers. #8160

    if(!theLayers?.layer?.length) {
      valid = false;
      this.callbacks.reportMessage(CompilerMessages.Error_MustBeAtLeastOneLayerElement());
    }

    if(theLayers?.form == 'hardware') {
      for(let layer of theLayers?.layer) {
        valid = this.validateHardwareLayer(theLayers?.hardware, layer) && valid; // note: always validate even if previously invalid results found
      }
    }
    return valid;
  }

  public compile(sections: GlobalSections): Keys {
    // Use LayerMap + keys to generate compiled keys for hardware
    const theLayers = this.keyboard.layers?.[0]; // TODO-LDML: handle >1 layers. #8160

    if(theLayers?.form == 'hardware') {
      let sect = new Keys();
      for(let layer of theLayers.layer) {
        this.compileHardwareLayer(sections, layer, sect, theLayers.hardware);
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
    hardware: string,
  ): Keys {
    const mod = translateLayerAttrToModifier(layer);
    const keymap = Constants.HardwareToKeymap.get(hardware);

    let y = -1;
    for(let row of layer.row) {
      y++;

      const keys = row.keys.split(' ');
      let x = -1;
      for(let key of keys) {
        x++;

        let keydef = this.keyboard.keys?.key?.find(x => x.id == key);

        sect.keys.push({
          vkey: keymap[y][x],
          mod: mod,
          to: sections.strs.allocAndUnescapeString(keydef.to),
          flags: 0 // Note: 'expand' is never set here, only by the .kmx builder
        });
      }
    }
    return sect;
  }
}
