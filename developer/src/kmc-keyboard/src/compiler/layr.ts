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
    return valid;
  }

  public compile(sections: GlobalSections): Layr {
    const sect = new Layr();

    sect.lists = this.keyboard.layers.map((layers) => {
      const list : LayrList = {
        flags: 0,
        hardware: sections.strs.allocString(layers.hardware),
        minDeviceWidth: layers.minDeviceWidth || 0,
        layers: layers.layer.map((layer) => {
          const entry : LayrEntry = {
            id: sections.strs.allocString(layer.id),
            modifier: sections.strs.allocString(layer.modifier),
            rows: layer.row.map((row) => {
              const erow : LayrRow = {
                keys: row.keys.split(' ').map((id) => sections.strs.allocString(id)),
              };
              return erow;
            }),
          };
          // TODO-LDML: modifiers
          return entry;
        }),
      };
      if (layers.form === 'touch') {
        list.flags |= constants.layr_list_flags_touch;
      }
      return list;
    });
    return sect;
  }
}
