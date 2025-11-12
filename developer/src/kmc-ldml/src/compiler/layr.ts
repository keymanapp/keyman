import { constants } from '@keymanapp/ldml-keyboard-constants';
import { KMXPlus } from '@keymanapp/common-types';
import { LdmlCompilerMessages } from './ldml-compiler-messages.js';
import { SectionCompiler } from "./section-compiler.js";
import { translateLayerAttrToModifier, validModifier } from '../util/util.js';


import DependencySections = KMXPlus.DependencySections;
import Layr = KMXPlus.Layr;
import LayrForm = KMXPlus.LayrForm;
import LayrRow = KMXPlus.LayrRow;

export class LayrCompiler extends SectionCompiler {

  public get id() {
    return constants.section.layr;
  }

  public validate() {
    let valid = true;
    let totalLayerCount = 0;
    let hardwareLayers = 0;
    let touchLayers = 0;
    const deviceWidths = new Set<number>();
    this.keyboard3.layers?.forEach((layers) => {
      const { formId } = layers;
      if (formId === 'touch') {
        touchLayers++;
        totalLayerCount += layers.layer?.length;
        const { minDeviceWidth } = layers;
        if (!minDeviceWidth ||
          minDeviceWidth < constants.layr_min_minDeviceWidth ||
          minDeviceWidth > constants.layr_max_minDeviceWidth ||
          Number.isNaN(Number(minDeviceWidth))) {
          valid = false;
          this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidLayerWidth({minDeviceWidth}, layers));
        } else if (deviceWidths.has(minDeviceWidth)) {
          valid = false;
          this.callbacks.reportMessage(LdmlCompilerMessages.Error_DuplicateLayerWidth({minDeviceWidth}, layers));
        } else {
          deviceWidths.add(minDeviceWidth);
        }
      } else {
        // hardware
        hardwareLayers++;
        if (hardwareLayers > 1) {
          valid = false;
          this.callbacks.reportMessage(LdmlCompilerMessages.Error_ExcessHardware({formId}, layers));
        }
      }
      layers.layer.forEach((layer) => {
        const { modifiers } = layer;
        totalLayerCount++;
        if (!validModifier(modifiers)) {
          this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidModifier({ modifiers }, layer));
          valid = false;
        }
      });
    });
    if (totalLayerCount === 0) { // TODO-LDML: does not validate touch layers yet
      // no layers seen anywhere
      valid = false;
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_MustBeAtLeastOneLayerElement(this.keyboard3));
    }
    return valid;
  }

  public compile(sections: DependencySections): Layr {
    const sect = new Layr();

    sect.forms = this.keyboard3.layers.map((layers) => {
      const hardware = sections.strs.allocString(layers.formId, {compileContext: layers});
      // Already validated in validate
      const layerEntries = [];
      for (const layer of layers.layer) {
        const rows = layer.row.map((row) => {
          const erow: LayrRow = {
            keys: row.keys.trim().split(/[ \t]+/).map((id) => sections.strs.allocString(id, { compileContext: row })),
          };
          // include linenumber info for row
          return SectionCompiler.copySymbols(erow, row);
        });
        const mods = translateLayerAttrToModifier(layer);
        // push a layer entry for each modifier set
        for (const mod of mods) {
          layerEntries.push({
            id: sections.strs.allocString(layer.id, {compileContext: layer}),
            mod,
            rows,
          });
        }
      }
      const form: LayrForm = {
        hardware,
        minDeviceWidth: layers.minDeviceWidth || 0,
        layers: layerEntries,
        baseLayout: sections.strs.allocString('', {compileContext: sect}), // TODO-EMBED-OSK-IN-KMX
        fontFaceName: sections.strs.allocString('', {compileContext: sect}),  // TODO-EMBED-OSK-IN-KMX
        fontSizePct: 100,  // TODO-EMBED-OSK-IN-KMX
        flags: 0,  // TODO-EMBED-OSK-IN-KMX
      };
      return form;
    });
    return sect;
  }
}
