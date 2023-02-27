import { constants } from '@keymanapp/ldml-keyboard-constants';
import { LDMLKeyboard, KMXPlus, Constants } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { SectionCompiler } from "./section-compiler.js";

import GlobalSections = KMXPlus.GlobalSections;
import Keys = KMXPlus.Keys;
import ListItem = KMXPlus.ListItem;
import KeysFlicks = KMXPlus.KeysFlicks;
import { allUsedKeyIdsInLayers, calculateUniqueKeys, translateLayerAttrToModifier, validModifier } from '../util/util.js';

export class KeysCompiler extends SectionCompiler {

  public get id() {
    return constants.section.keys;
  }

  public validate() {
    let valid = true;

    // general key-level validation here, only of used keys
    const usedKeys = allUsedKeyIdsInLayers(this.keyboard?.layers);
    const uniqueKeys = calculateUniqueKeys([...this.keyboard.keys?.key]);
    for (let key of uniqueKeys) {
      const {id, flicks} = key;
      if (!usedKeys.has(id)) {
        continue; // unused key, ignore
      }
      // TODO-LDML: further key-level validation here
      if (!flicks) {
        continue; // no flicks
      }
      const flickEntry = this.keyboard.keys?.flicks?.find(x => x.id === flicks);
      if (!flickEntry ) {
        valid = false;
        this.callbacks.reportMessage(CompilerMessages.Error_MissingFlicks({flicks, id}));
      }
    }

    // Kmap validation
    const theLayers = this.keyboard.layers?.[0]; // TODO-LDML: handle >1 layers. #8160

    if(!theLayers?.layer?.length) {
      valid = false;
      this.callbacks.reportMessage(CompilerMessages.Error_MustBeAtLeastOneLayerElement());
    }

    if(theLayers?.form == 'hardware') {
      for(let layer of theLayers?.layer) {
        valid = this.validateHardwareLayerForKmap(theLayers?.hardware, layer) && valid; // note: always validate even if previously invalid results found
      }
    }

    // TODO-LDML: some additional validation needed here?
    return valid;
  }

  public compile(sections: GlobalSections): Keys {
    if (!this.keyboard?.keys?.key && !this.keyboard?.keys?.flicks) {
      // short-circuit if no keys or flicks
      return null;
    }

    let sect = new Keys(sections.strs);

    // Load the flicks first
    this.loadFlicks(sections, sect);

    // Now, load the keys
    this.loadKeys(sections, sect);

    // Finally, kmap
    // Use LayerMap + keys to generate compiled keys for hardware
    const theLayers = this.keyboard.layers?.[0]; // TODO-LDML: handle >1 layers. #8160

    if(theLayers?.form == 'hardware') {
      for(let layer of theLayers.layer) {
        this.compileHardwareLayerToKmap(sections, layer, sect, theLayers.hardware);
      }
      return sect;
    }
    // TODO-LDML: generate vkey mapping for touch-only keys

    return sect;
  }

  public loadFlicks(sections: GlobalSections, sect: Keys) {
    for (let lkflicks of this.keyboard.keys.flicks) {
      let flicks: KeysFlicks = new KeysFlicks(sections.strs.allocString(lkflicks.id));

      for (let lkflick of lkflicks.flick) {
        let flags = 0;
        // TODO-LDML: single char
        const to = sections.strs.allocAndUnescapeString(lkflick.to);
        flags |= constants.keys_flick_flags_extend;
        let directions : ListItem = sections.list.allocListFromSpaces(sections.strs, lkflick.directions);
        flicks.flicks.push({
          directions,
          flags,
          to,
        });
      }

      sect.flicks.push(flicks);
    }
  }

  public loadKeys(sections: GlobalSections, sect: Keys) {
    const usedKeys = allUsedKeyIdsInLayers(this.keyboard?.layers);
    const uniqueKeys = calculateUniqueKeys([...this.keyboard.keys?.key]);

    for (let key of uniqueKeys) {
      if (!usedKeys.has(key.id)) {
        // TODO-LDML: linting for unused, non-implied and non-imported keys,
        continue; // unused key, skip
      }
      let flags = 0;
      const flicks = key.flicks;
      if (!!key.gap) {
        flags |= constants.keys_key_flags_gap;
      }
      if (key.transform === 'no') {
        flags |= constants.keys_key_flags_notransform;
      }
      const id = sections.strs.allocString(key.id);
      const longPress: ListItem = sections.list.allocListFromEscapedSpaces(sections.strs, key.longPress);
      const longPressDefault = sections.strs.allocAndUnescapeString(key.longPressDefault);
      const multiTap: ListItem = sections.list.allocListFromEscapedSpaces(sections.strs, key.multiTap);
      const keySwitch = sections.strs.allocString(key.switch); // 'switch' is a reserved word
      flags |= constants.keys_key_flags_extend;
      const to = sections.strs.allocAndUnescapeString(key.to); // TODO-LDML: single char
      const width = Math.ceil((key.width || 1) * 10.0);  // default, width=1
      sect.keys.push({
        flags,
        flicks,
        id,
        longPress,
        longPressDefault,
        multiTap,
        switch: keySwitch, // 'switch' is a reserved word
        to,
        width,
      });
    }
  }

  /**
   * TODO-LDML: from old 'keys'
   * Validate for purpose of kmap
   * @param hardware
   * @param layer
   * @returns
   */
  private validateHardwareLayerForKmap(hardware: string, layer: LDMLKeyboard.LKLayer) {
    let valid = true;

    const { modifier } = layer;
    if (!validModifier(modifier)) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidModifier({ modifier, layer: layer.id }));
      valid = false;
    }

    const keymap = Constants.HardwareToKeymap.get(hardware);
    if (!keymap) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidHardware({ hardware }));
      valid = false;
      return valid; // can't do anything else here
    }

    const uniqueKeys = calculateUniqueKeys([...this.keyboard.keys?.key]);
    if (layer.row.length > keymap.length) {
      this.callbacks.reportMessage(CompilerMessages.Error_HardwareLayerHasTooManyRows());
      valid = false;
    }

    for (let y = 0; y < layer.row.length && y < keymap.length; y++) {
      const keys = layer.row[y].keys.split(' ');

      if (keys.length > keymap[y].length) {
        this.callbacks.reportMessage(CompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({ row: y + 1, hardware }));
        valid = false;
      }

      let x = -1;
      for (let key of keys) {
        x++;

        let keydef = uniqueKeys.find(x => x.id == key);
        if (!keydef) {
          this.callbacks.reportMessage(CompilerMessages.Error_KeyNotFoundInKeyBag({ keyId: key, col: x + 1, row: y + 1, layer: layer.id, form: 'hardware' }));
          valid = false;
          continue;
        }
        if (!keydef.to && !keydef.gap && !keydef.switch) {
          this.callbacks.reportMessage(CompilerMessages.Error_KeyMissingToGapOrSwitch({ keyId: key }));
          valid = false;
          continue;
        }
      }
    }

    return valid;
  }


  private compileHardwareLayerToKmap(
    sections: GlobalSections,
    layer: LDMLKeyboard.LKLayer,
    sect: Keys,
    hardware: string,
  ): Keys {
    const mod = translateLayerAttrToModifier(layer);
    const keymap = Constants.HardwareToKeymap.get(hardware);

    let y = -1;
    for (let row of layer.row) {
      y++;

      const keys = row.keys.split(' ');
      let x = -1;
      for (let key of keys) {
        x++;

        // TODO-LDML: we already validated that the key exists, above.
        // So here we only need the ID?
        // let keydef = this.keyboard.keys?.key?.find(x => x.id == key);

        sect.kmap.push({
          vkey: keymap[y][x],
          mod: mod,
          key, // key id, to be changed into key index at finalization
        });
      }
    }
    return sect;
  }

}
