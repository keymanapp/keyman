import { constants } from '@keymanapp/ldml-keyboard-constants';
import { /*LDMLKeyboard,*/ KMXPlus,/* Constants*/ } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { SectionCompiler } from "./section-compiler.js";

import GlobalSections = KMXPlus.GlobalSections;
import Layr = KMXPlus.Layr;
import LayrEntry = KMXPlus.LayrEntry;
import LayrList = KMXPlus.LayrList;
import LayrRow = KMXPlus.LayrRow;
// import USVirtualKeyMap = Constants.USVirtualKeyMap;

export class LayrCompiler extends SectionCompiler {

  public get id() {
    return constants.section.layr;
  }

  public validate() {
    let valid = true;
    if(!this.keyboard.layers?.[0]?.layer?.length) {
      valid = false;
      this.callbacks.reportMessage(CompilerMessages.Error_MustBeAtLeastOneLayerElement());
    }
    // TODO-LDML

    // if(this.keyboard.layers?.[0]?.form == 'hardware') {
    //   for(let layer of this.keyboard.layers[0].layer) {
    //     valid = this.validateHardwareLayer(layer) && valid; // note: always validate even if previously invalid results found
    //   }
    // }
    return valid;
  }

  public compile(sections: GlobalSections): Layr {
    // Use LayerMap + keys to generate compiled keys for hardware

    const sect = new Layr();
    // if(this.keyboard.layers?.[0]?.form == 'hardware') {
    //   for(let layer of this.keyboard.layers[0].layer) {
    //     let sect = this.compileHardwareLayer(sections, layer);
    //     return sect;
    //   }
    // }

    sect.lists = this.keyboard.layers.map((layers) => {
      const list : LayrList = {
        flags: 0, // flag
        hardware: sections.strs.allocString(layers.hardware),
        layerIndex: sect.layers.length,
        count: layers.layer.length,
      };
      if (layers.form === 'touch') {
        list.flags |= constants.layr_list_flags_touch;
      }
      // TODO-LDML: minDeviceWidth? modifiers?
      // push all layers
      layers.layer.forEach((layer) => {
        const entry : LayrEntry = {
          id: sections.strs.allocString(layer.id),
          modifier: sections.strs.allocString(layer.modifier),
          rowIndex: sect.rows.length,
          count: layer.row.length,
        };
        layer.row.forEach((row) => {
          const lrow : LayrRow = {
            keyIndex: sect.keys.length,
            count: 0,
          };
          row.keys.split(' ').forEach((keyid) => {
            sect.keys.push(keyid); // just the id… for now
            lrow.count ++; // increment key count
          });
          sect.rows.push(lrow);
        });
        sect.layers.push(entry);
      });
      return list;
    });

    return sect;
  }

  // private compileHardwareLayer(
  //   sections: GlobalSections,
  //   layer: LDMLKeyboard.LKLayer
  // ): Layr {
  //   let result = new Layr();
  //   const mod = this.translateLayerIdToModifier(layer.id);

  //   let y = -1;
  //   for(let row of layer.row) {
  //     y++;

  //     const keys = row.keys.split(' ');
  //     let x = -1;
  //     for(let key of keys) {
  //       x++;

  //       let keydef = this.keyboard.keys?.key?.find(x => x.id == key);

  //       result.keys.push({
  //         vkey: USVirtualKeyMap[y][x],
  //         mod: mod,
  //         to: sections.strs.allocString(keydef.to),
  //         flags: 0 // Note: 'expand' is never set here, only by the .kmx builder
  //       });
  //     }
  //   }

  //   return result;
  // }

  // private translateLayerIdToModifier(id: string) {
  //   if(id == 'base') {
  //     return 0;
  //   }
  //   // TODO: other modifiers
  //   return 0;
  // }
}
