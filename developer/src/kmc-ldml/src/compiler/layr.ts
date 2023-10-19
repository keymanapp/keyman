import { constants } from '@keymanapp/ldml-keyboard-constants';
import { KMXPlus } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { SectionCompiler } from "./section-compiler.js";
import { translateLayerAttrToModifier, validModifier } from '../util/util.js';


import DependencySections = KMXPlus.DependencySections;
import Layr = KMXPlus.Layr;
import LayrEntry = KMXPlus.LayrEntry;
import LayrList = KMXPlus.LayrList;
import LayrRow = KMXPlus.LayrRow;

export class LayrCompiler extends SectionCompiler {

  public get id() {
    return constants.section.layr;
  }

  public validate() {
    let valid = true;
    let totalLayerCount = 0;
    let hardwareLayers = 0;
    // let touchLayers = 0;
    this.keyboard3.layers?.forEach((layers) => {
      const { form } = layers;
      if (form === 'touch') {
        // touchLayers++;
        // multiple touch layers are OK
        totalLayerCount += layers.layer?.length;
        // TODO-LDML: check that widths are distinct
      } else {
        // hardware
        hardwareLayers++;
        if (hardwareLayers > 1) {
          valid = false;
          this.callbacks.reportMessage(CompilerMessages.Error_ExcessHardware({form}));
        }
      }
      layers.layer.forEach((layer) => {
        const { modifier, id } = layer;
        totalLayerCount++;
        if (!validModifier(modifier)) {
          this.callbacks.reportMessage(CompilerMessages.Error_InvalidModifier({ modifier, layer: id }));
          valid = false;
        }
      });
    });
    if (totalLayerCount === 0) { // TODO-LDML: does not validate touch layers yet
      // no layers seen anywhere
      valid = false;
      this.callbacks.reportMessage(CompilerMessages.Error_MustBeAtLeastOneLayerElement());
    }
    return valid;
  }

  public compile(sections: DependencySections): Layr {
    const sect = new Layr();

    sect.lists = this.keyboard3.layers.map((layers) => {
      const hardware = sections.strs.allocString(layers.form);
      // Already validated in validate
      const list: LayrList = {
        hardware,
        minDeviceWidth: layers.minDeviceWidth || 0,
        layers: layers.layer.map((layer) => {
          const entry: LayrEntry = {
            id: sections.strs.allocString(layer.id),
            mod: translateLayerAttrToModifier(layer),
            rows: layer.row.map((row) => {
              const erow: LayrRow = {
                keys: row.keys.split(' ').map((id) => sections.strs.allocString(id)),
              };
              return erow;
            }),
          };
          return entry;
        }),
      };
      return list;
    });
    return sect;
  }
}
