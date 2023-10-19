import { constants } from '@keymanapp/ldml-keyboard-constants';
import { LDMLKeyboard, KMXPlus, Constants, MarkerParser } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { SectionCompiler } from "./section-compiler.js";

import DependencySections = KMXPlus.DependencySections;
import Keys = KMXPlus.Keys;
import ListItem = KMXPlus.ListItem;
import KeysFlicks = KMXPlus.KeysFlicks;
import { allUsedKeyIdsInLayers, calculateUniqueKeys, translateLayerAttrToModifier, validModifier } from '../util/util.js';
import { MarkerTracker, MarkerUse } from './marker-tracker.js';

export class KeysCompiler extends SectionCompiler {
  static validateMarkers(
    keyboard: LDMLKeyboard.LKKeyboard,
    mt: MarkerTracker
  ): boolean {
    keyboard.keys?.key?.forEach(({ to }) =>
      mt.add(MarkerUse.emit, MarkerParser.allReferences(to))
    );
    return true;
  }

  public get id() {
    return constants.section.keys;
  }

  /**
   *
   * @returns just the non-touch layers.
   */
  public hardwareLayers() {
    return this.keyboard3.layers?.filter(({ form }) => form !== "touch");
  }

  public validate() {
    let valid = true;

    // There's no 'form' compiler.
    // We validate this here so that someone checks it.
    this.keyboard3.forms?.form?.forEach((form) => {
      if (!LDMLKeyboard.ImportStatus.isImpliedImport(form)) {
        // If it's not an implied import, give a warning.
        this.callbacks.reportMessage(CompilerMessages.Warn_CustomForm({ id: form.id }));
      }
    });

    // general key-level validation here, only of used keys
    const usedKeys = allUsedKeyIdsInLayers(this.keyboard3?.layers);
    const uniqueKeys = calculateUniqueKeys([...this.keyboard3.keys?.key]);
    for (let key of uniqueKeys) {
      const { id, flicks } = key;
      if (!usedKeys.has(id)) {
        continue; // unused key, ignore
      }
      // TODO-LDML: further key-level validation here
      if (!flicks) {
        continue; // no flicks
      }
      const flickEntry = this.keyboard3.keys?.flicks?.find(
        (x) => x.id === flicks
      );
      if (!flickEntry) {
        valid = false;
        this.callbacks.reportMessage(
          CompilerMessages.Error_MissingFlicks({ flicks, id })
        );
      }
    }

    // the layr compiler does more extensive validation of the layer attributes.

    // Kmap validation
    const hardwareLayers = this.hardwareLayers();

    if (hardwareLayers.length >= 1) {
      // validate all errors
      for (let layers of hardwareLayers) {
        for (let layer of layers.layer) {
          valid =
            this.validateHardwareLayerForKmap(layers.form, layer) && valid; // note: always validate even if previously invalid results found
        }
      }
      // TODO-LDML: } else { touch?
    }

    return valid;
  }

  public compile(sections: DependencySections): Keys {
    /* c8 ignore next 4 */
    if (!this.keyboard3?.keys?.key && !this.keyboard3?.keys?.flicks) {
      // short-circuit if no keys or flicks. Doesn't happen in practice due to implied import.
      return null;
    }

    let sect = new Keys(sections.strs);

    // Load the flicks first
    this.loadFlicks(sections, sect);

    // Now, load the keys
    this.loadKeys(sections, sect);

    // Finally, kmap
    // Use LayerMap + keys to generate compiled keys for hardware
    const hardwareLayers = this.hardwareLayers();
    /* c8 ignore next 3 */
    if (hardwareLayers.length > 1) {
      // validation should have already caught this
      throw Error(
        `Internal error: Expected 0 or 1 hardware layer, not ${hardwareLayers.length}`
      );
    } else if (hardwareLayers.length === 1) {
      const theLayers = hardwareLayers[0];
      const { form } = theLayers;
      for (let layer of theLayers.layer) {
        this.compileHardwareLayerToKmap(sections, layer, sect, form);
      }
    } // else: TODO-LDML do nothing if only touch layers

    return sect;
  }

  public loadFlicks(sections: DependencySections, sect: Keys) {
    for (let lkflicks of this.keyboard3.keys.flicks) {
      let flicks: KeysFlicks = new KeysFlicks(
        sections.strs.allocString(lkflicks.id)
      );

      for (let lkflick of lkflicks.flick) {
        let flags = 0;
        const to = sections.strs.allocString(lkflick.to, {
          stringVariables: true, markers: true, unescape: true, singleOk: true
        }, sections);
        if (!to.isOneChar) {
          flags |= constants.keys_flick_flags_extend;
        }
        let directions: ListItem = sections.list.allocListFromSpaces(
          lkflick.directions,
          {
            stringVariables: true, markers: true, unescape: true
          },
          sections);
        flicks.flicks.push({
          directions,
          flags,
          to,
        });
      }

      sect.flicks.push(flicks);
    }
  }

  public loadKeys(sections: DependencySections, sect: Keys) {
    const usedKeys = allUsedKeyIdsInLayers(this.keyboard3?.layers);
    const uniqueKeys = calculateUniqueKeys([...this.keyboard3.keys?.key]);

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
      if (key.transform === "no") {
        flags |= constants.keys_key_flags_notransform;
      }
      const id = sections.strs.allocString(key.id);
      const longPress: ListItem = sections.list.allocListFromSpaces(
        key.longPress, {
          stringVariables: true,
          markers: true,
          unescape: true,
        },
        sections);

      const longPressDefault = sections.strs.allocString(key.longPressDefault,
        {
          stringVariables: true,
          markers: true,
          unescape: true,
        },
        sections);

      const multiTap: ListItem = sections.list.allocListFromSpaces(
        key.multiTap,
        {
          stringVariables: true,
          markers: true,
          unescape: true,
        },
        sections);
      const keySwitch = sections.strs.allocString(key.switch); // 'switch' is a reserved word

      const toRaw = key.to;

      let toCooked = sections.vars.substituteStrings(toRaw, sections);
      toCooked = sections.vars.substituteMarkerString(toCooked);
      const to = sections.strs.allocString(key.to,
        {
          stringVariables: true,
          markers: true,
          unescape: true,
          singleOk: true
        },
        sections);
      if (!to.isOneChar) {
        flags |= constants.keys_key_flags_extend;
      }
      const width = Math.ceil((key.width || 1) * 10.0); // default, width=1
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

  private getKeymapFromForm(hardware : string, badScans?: Set<number>) : Constants.KeyMap {
    return KeysCompiler.getKeymapFromForms(this.keyboard3?.forms.form, hardware, badScans);
  }

  public static getKeymapFromForms(forms: LDMLKeyboard.LKForm[], hardware: string, badScans?: Set<number>): Constants.KeyMap {
    // seach in reverse form because of overrides
    const ldmlForm = [...forms].reverse().find((f) => f.id === hardware);
    if (!ldmlForm) {
      return undefined;
    }
    return KeysCompiler.getKeymapFromScancodes(ldmlForm, badScans);
  }

  public static getKeymapFromScancodes(ldmlForm: LDMLKeyboard.LKForm, badScans?: Set<number>) {
    const { scanCodes } = ldmlForm;
    const ldmlScan = scanCodes.map(o => o.codes.split(" ").map(n => Number.parseInt(n, 16)));
    const ldmlVkey = Constants.CLDRScanToKeyMap(ldmlScan, badScans);
    return ldmlVkey;
  }

  /**
   * TODO-LDML: from old 'keys'
   * Validate for purpose of kmap
   * @param hardware the 'form' parameter
   * @param layer
   * @returns
   */
  private validateHardwareLayerForKmap(
    hardware: string,
    layer: LDMLKeyboard.LKLayer
  ) {
    let valid = true;

    const { modifier } = layer;
    if (!validModifier(modifier)) {
      this.callbacks.reportMessage(
        CompilerMessages.Error_InvalidModifier({ modifier, layer: layer.id })
      );
      valid = false;
    }

    const badScans = new Set<number>();
    const keymap = this.getKeymapFromForm(hardware, badScans);
    if (!keymap) {
      this.callbacks.reportMessage(
        CompilerMessages.Error_InvalidHardware({ form: hardware })
      );
      valid = false;
      return valid;
    } else if (badScans.size !== 0) {
      const codes = Array.from(badScans.values()).map(n => Number(n).toString(16)).sort();
      this.callbacks.reportMessage(
        CompilerMessages.Error_InvalidScanCode({ form: hardware, codes })
      );
      valid = false;
      return valid;
    }

    const uniqueKeys = calculateUniqueKeys([...this.keyboard3.keys?.key]);
    if (layer.row.length > keymap.length) {
      this.callbacks.reportMessage(
        CompilerMessages.Error_HardwareLayerHasTooManyRows()
      );
      valid = false;
    }

    for (let y = 0; y < layer.row.length && y < keymap.length; y++) {
      const keys = layer.row[y].keys.split(" ");

      if (keys.length > keymap[y].length) {
        this.callbacks.reportMessage(
          CompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({
            row: y + 1,
            hardware,
            modifier,
          })
        );
        valid = false;
      }

      let x = -1;
      for (let key of keys) {
        x++;

        let keydef = uniqueKeys.find((x) => x.id == key);
        if (!keydef) {
          this.callbacks.reportMessage(
            CompilerMessages.Error_KeyNotFoundInKeyBag({
              keyId: key,
              col: x + 1,
              row: y + 1,
              layer: layer.id,
              form: "hardware",
            })
          );
          valid = false;
          continue;
        }
        if (!keydef.to && !keydef.gap && !keydef.switch) {
          this.callbacks.reportMessage(
            CompilerMessages.Error_KeyMissingToGapOrSwitch({ keyId: key })
          );
          valid = false;
          continue;
        }
      }
    }

    return valid;
  }

  private compileHardwareLayerToKmap(
    sections: DependencySections,
    layer: LDMLKeyboard.LKLayer,
    sect: Keys,
    hardware: string
  ): Keys {
    const mod = translateLayerAttrToModifier(layer);
    const keymap = this.getKeymapFromForm(hardware);

    let y = -1;
    for (let row of layer.row) {
      y++;

      const keys = row.keys.split(" ");
      let x = -1;
      for (let key of keys) {
        x++;

        // TODO-LDML: we already validated that the key exists, above.
        // So here we only need the ID?
        // let keydef = this.keyboard3.keys?.key?.find(x => x.id == key);

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
