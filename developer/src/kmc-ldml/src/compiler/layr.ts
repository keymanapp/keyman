import { constants } from '@keymanapp/ldml-keyboard-constants';
import { KMXPlus } from '@keymanapp/common-types';
import { LdmlCompilerMessages } from './ldml-compiler-messages.js';
import { SectionCompiler } from "./section-compiler.js";
import { translateLayerAttrToModifier, validModifier } from '../util/util.js';


import DependencySections = KMXPlus.DependencySections;
import Layr = KMXPlus.Layr;
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
      const { formId } = layers;
      if (formId === 'touch') {
        // touchLayers++;
        // multiple touch layers are OK
        totalLayerCount += layers.layer?.length;
        // TODO-LDML: check that widths are distinct
      } else {
        // hardware
        hardwareLayers++;
        if (hardwareLayers > 1) {
          valid = false;
          this.callbacks.reportMessage(LdmlCompilerMessages.Error_ExcessHardware({formId}));
        }
      }
      layers.layer.forEach((layer) => {
        const { modifiers, id } = layer;
        totalLayerCount++;
        if (!validModifier(modifiers)) {
          this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidModifier({ modifiers, layer: id || '' }));
          valid = false;
        }
      });
    });
    if (totalLayerCount === 0) { // TODO-LDML: does not validate touch layers yet
      // no layers seen anywhere
      valid = false;
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_MustBeAtLeastOneLayerElement());
    }
    return valid;
  }

  public compile(sections: DependencySections): Layr {
    const sect = new Layr();

    sect.lists = this.keyboard3.layers.map((layers) => {
      const hardware = sections.strs.allocString(layers.formId);
      // Already validated in validate
      const layerEntries = [];
      for (const layer of layers.layer) {
        const rows = layer.row.map((row) => {
          const erow: LayrRow = {
            keys: row.keys.trim().split(/[ \t]+/).map((id) => sections.strs.allocString(id)),
          };
          return erow;
        });
        const mods = translateLayerAttrToModifier(layer);
        // push a layer entry for each modifier set
        for (const mod of mods) {
          layerEntries.push({
            id: sections.strs.allocString(layer.id),
            mod,
            rows,
          });
        }
      }
      const list: LayrList = {
        hardware,
        minDeviceWidth: layers.minDeviceWidth || 0,
        layers: layerEntries,
      };
      return list;
    });
    return sect;
  }
}
