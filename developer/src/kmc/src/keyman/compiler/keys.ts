import { constants } from '@keymanapp/ldml-keyboard-constants';
import { GlobalSections, Keys } from '../kmx/kmx-plus';
import * as LDMLKeyboard from '../ldml-keyboard/ldml-keyboard-xml';
import { USVirtualKeyMap } from "../ldml-keyboard/virtual-key-constants";
import { CompilerMessages } from './messages';

import { SectionCompiler } from "./section-compiler";

export class KeysCompiler extends SectionCompiler {

  public get id() {
    return constants.section.keys;
  }

  public compile(sections: GlobalSections): Keys {
    // Use LayerMap + keys to generate compiled keys for hardware

    if(this.keyboard.layerMaps?.[0]?.form == 'hardware') {
      for(let layer of this.keyboard.layerMaps[0].layerMap) {
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
      if(y > USVirtualKeyMap.length) {
        this.callbacks.reportMessage(CompilerMessages.Error_HardwareLayerHasTooManyRows());
        break;
      }

      const keys = row.keys.split(' ');
      let x = -1;
      for(let key of keys) {
        x++;
        if(x > USVirtualKeyMap[y].length) {
          this.callbacks.reportMessage(CompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({row: y+1}));
          break;
        }

        let keydef = this.keyboard.keys?.key?.find(x => x.id == key);
        if(!keydef) {
          this.callbacks.reportMessage(CompilerMessages.Error_KeyNotFoundInKeyBag({keyId: key, col: x+1, row: y+1, layer: layer.id, form: 'hardware'}));
        }

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