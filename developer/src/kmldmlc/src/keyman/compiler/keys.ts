import * as LDMLKeyboard from '../ldml-keyboard/ldml-keyboard-xml';
import { USVirtualKeyMap } from "../ldml-keyboard/us-virtual-keys";

import { SectionCompiler } from "./section-compiler";

export class KeysCompiler extends SectionCompiler {

  public execute() {
    // Use LayerMap + keys to generate compiled keys for hardware

    if(this.source.keyboard.layerMaps?.[0]?.form == 'hardware') {
      for(let layer of this.source.keyboard.layerMaps[0].layerMap) {
        this.compileHardwareLayer(layer);
      }
    }
  }

  private compileHardwareLayer(
    layer: LDMLKeyboard.LKLayerMap
  ) {
    const mod = this.translateLayerIdToModifier(layer.id);

    let y = -1;
    for(let row of layer.row) {
      y++;
      if(y > USVirtualKeyMap.length) {
        this.callbacks.reportMessage(0, `'hardware' layer has too many rows`);
        break;
      }

      const keys = row.keys.split(' ');
      let x = -1;
      for(let key of keys) {
        x++;
        if(x > USVirtualKeyMap[y].length) {
          this.callbacks.reportMessage(0, `Row #${y+1} on 'hardware' layer has too many keys`);
          break;
        }

        let keydef = this.source.keyboard.keys?.key?.find(x => x.id == key);
        if(!keydef) {
          this.callbacks.reportMessage(0,
            `Key ${key} in position #${x+1} on row #${y+1} of layer ${layer.id}, form 'hardware' not found in key bag`);
          continue;
        }

        this.kmx.kmxplus.keys.keys.push({
          vkey: USVirtualKeyMap[y][x],
          mod: mod,
          to: keydef.to,
          flags: 0 // Note: 'expand' is never set here, only by the .kmx builder
        });
      }
    }
  }

  private translateLayerIdToModifier(id: string) {
    if(id == 'base') {
      return 0;
    }
    // TODO: other modifiers
    return 0;
  }
}