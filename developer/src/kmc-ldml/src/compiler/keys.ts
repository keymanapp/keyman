import { constants } from '@keymanapp/ldml-keyboard-constants';
import { LDMLKeyboard, KMXPlus, Constants, MarkerParser } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { SectionCompiler } from "./section-compiler.js";

import DependencySections = KMXPlus.DependencySections;
import Keys = KMXPlus.Keys;
import ListItem = KMXPlus.ListItem;
import KeysFlicks = KMXPlus.KeysFlicks;
import { allUsedKeyIdsInFlick, allUsedKeyIdsInKey, allUsedKeyIdsInLayers, calculateUniqueKeys, hashFlicks, hashKeys, translateLayerAttrToModifier, validModifier } from '../util/util.js';
import { MarkerTracker, MarkerUse } from './marker-tracker.js';

export class KeysCompiler extends SectionCompiler {
  static validateMarkers(
    keyboard: LDMLKeyboard.LKKeyboard,
    mt: MarkerTracker
  ): boolean {
    // TODO-LDML: repetition

    const uniqueKeys = calculateUniqueKeys([...keyboard.keys?.key]);
    const keyBag = hashKeys(uniqueKeys); // for easier lookup
    // will be the set of ALL keys used in this keyboard
    const usedKeys = allUsedKeyIdsInLayers(keyboard?.layers);
    // save off the layer key IDs before we mutate the set
    const layerKeyIds = Array.from(usedKeys.values());
    const flickHash = hashFlicks(keyboard?.flicks?.flick); // for easier lookup
    const usedFlicks = KeysCompiler.getUsedFlicks(layerKeyIds, keyBag);
    KeysCompiler.addKeysFromFlicks(usedFlicks, flickHash, usedKeys);
    KeysCompiler.addUsedGestureKeys(layerKeyIds, keyBag, usedKeys);

    // process each key
    for (let keyId of usedKeys.values()) {
      const key = keyBag.get(keyId);
      if (!key) continue;
      mt.add(MarkerUse.emit, MarkerParser.allReferences(key.output));
    }
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
    return this.keyboard3.layers?.filter(({ formId }) => formId !== "touch");
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

    const keyBag = this.getKeyBag();
    // will be the set of ALL keys used in this keyboard
    const usedKeys = this.getLayerKeyIds();
    // save off the layer key IDs before we mutate the set
    const layerKeyIds = Array.from(usedKeys.values());
    const flickHash = this.getFlicks(); // for easier lookup
    const usedFlicks = KeysCompiler.getUsedFlicks(layerKeyIds, keyBag);

    // go through each layer key and collect flicks and gestures
    for(const keyId of layerKeyIds) {
      const key = keyBag.get(keyId);

      if (!key) {
        // Note: validateHardwareLayerForKmap(), below, will raise an error for hardware keys that are missing, with additional
        // context. For this section, we just skip missing keys.
        continue;
      }

      const { flickId } = key;
      if (flickId) {
        if (!flickHash.has(flickId)) {
          valid = false;
          this.callbacks.reportMessage(
            CompilerMessages.Error_MissingFlicks({ flickId, id: keyId })
          );
        }
      }
      const gestureKeys = allUsedKeyIdsInKey(key);

      for(const [gestureKeyId, attrs] of gestureKeys.entries()) {
        const gestureKey = keyBag.get(gestureKeyId);
        if (gestureKey == null) {
          // TODO-LDML: could keep track of already missing keys so we don't warn multiple times on gesture keys
          valid = false;
          this.callbacks.reportMessage(
            CompilerMessages.Error_GestureKeyNotFoundInKeyBag({keyId: gestureKeyId, parentKeyId: keyId, attribute: attrs.join(',')})
          );
        } else {
          usedKeys.add(gestureKeyId);
        }
      }
    }

    // now, check the flicks
    KeysCompiler.addKeysFromFlicks(usedFlicks, flickHash, usedKeys);
    // TODO-LDML: hint on unused flicks (that aren't imported)

    // Note: the layr compiler does more extensive validation of the layer attributes.

    // Kmap validation
    const hardwareLayers = this.hardwareLayers();

    if (hardwareLayers.length >= 1) {
      // validate all errors
      for (let layers of hardwareLayers) {
        for (let layer of layers.layer) {
          valid =
            this.validateHardwareLayerForKmap(layers.formId, layer, keyBag) && valid; // note: always validate even if previously invalid results found
        }
      }
      // TODO-LDML: } else { touch?
    }

    return valid;
  }

  static addKeysFromFlicks(usedFlicks: Set<string>, flickHash: Map<string, LDMLKeyboard.LKFlick>, usedKeys: Set<string>) {
    for (let flickId of usedFlicks.values()) {
      const flick = flickHash.get(flickId);
      if (!flick) continue;
      const flickKeys = allUsedKeyIdsInFlick(flick);
      flickKeys.forEach(keyId => usedKeys.add(keyId));
    }
  }

  private getFlicks() {
    return hashFlicks(this.keyboard3?.flicks?.flick);
  }

  /** a set with all key ids used in all layers */
  private getLayerKeyIds() {
    return allUsedKeyIdsInLayers(this.keyboard3?.layers);
  }

  /** the entire keybag (used or unused) as a hash */
  private getKeyBag() : Map<string, LDMLKeyboard.LKKey> {
    const uniqueKeys = calculateUniqueKeys([...this.keyboard3.keys?.key]);
    return hashKeys(uniqueKeys); // for easier lookup
  }

  public compile(sections: DependencySections): Keys {
    /* c8 ignore next 4 */
    if (!this.keyboard3?.keys?.key && !this.keyboard3?.keys?.flicks) {
      // short-circuit if no keys or flicks. Doesn't happen in practice due to implied import.
      return null;
    }

    let sect = new Keys(sections.strs);

    // TODO-LDML: some duplication with validate()
    const keyBag = this.getKeyBag();
    // We only want to include used keys in .kmx
    const usedKeys = this.getLayerKeyIds();
    // save off the layer key IDs before we mutate the set
    const layerKeyIds = Array.from(usedKeys.values());

    // Load the flicks first
    this.loadFlicks(sections, sect, keyBag, layerKeyIds, usedKeys);

    // add in the gesture keys
    KeysCompiler.addUsedGestureKeys(layerKeyIds, keyBag, usedKeys);

    // Now, load the keys into memory
    this.loadKeys(sections, sect, keyBag, layerKeyIds, usedKeys);

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
      const { formId } = theLayers;
      for (let layer of theLayers.layer) {
        this.compileHardwareLayerToKmap(sections, layer, sect, formId);
      }
    } // else: TODO-LDML do nothing if only touch layers

    return sect;
  }

  static addUsedGestureKeys(layerKeyIds: string[], keyBag: Map<string, LDMLKeyboard.LKKey>, usedKeys: Set<string>) {
    for (let keyId of layerKeyIds) {
      const key = keyBag.get(keyId);
      if (!key) continue;
      for (let gestureKeyId of allUsedKeyIdsInKey(key).keys()) {
        usedKeys.add(gestureKeyId);
      }
    }
  }

  /**
   *
   * @param keyBag the keybag as a hash
   * @param layerKeyIds list of keys from the layer, to extract used flicks
   * @param usedKeys will be populated with keys used in the flick
   */
  public loadFlicks(sections: DependencySections, sect: Keys,
    keyBag: Map<string, LDMLKeyboard.LKKey>, layerKeyIds: string[], usedKeys: Set<string>) {
    const flickHash = this.getFlicks(); // for easier lookup
    const usedFlicks = KeysCompiler.getUsedFlicks(layerKeyIds, keyBag);

    // only include used flicks in the table
    // this way, extra unused imported flicks are ignored
    // in id order, for now
    for (let flickId of Array.from(usedFlicks.values()).sort()) {
      const flick = flickHash.get(flickId);
      if (!flick) continue; // already reported by validate()

      // allocate the in-memory <flick id=…>
      let flicks: KeysFlicks = new KeysFlicks(
        sections.strs.allocString(flickId)
      );

      // add data from each segment
      for (let { keyId, directions } of flick.flickSegment) {
        const keyIdStr = sections.strs.allocString(keyId);
        let directionsList: ListItem = sections.list.allocListFromSpaces(
          directions,
          { },
          sections);
        flicks.flicks.push({
          directions: directionsList,
          keyId: keyIdStr,
        });
        usedKeys.add(keyId);
      }

      sect.flicks.push(flicks);
    }
  }

  static getUsedFlicks(layerKeyIds: string[], keyBag: Map<string, LDMLKeyboard.LKKey>) {
    const usedFlicks = new Set<string>();
    for (let keyId of layerKeyIds) {
      const key = keyBag.get(keyId);
      if (!key?.flickId) continue;
      usedFlicks.add(key.flickId);
    }
    return usedFlicks;
  }

  public loadKeys(sections: DependencySections, sect: Keys, keyBag: Map<string, LDMLKeyboard.LKKey>,
    layerKeyIds: string[], usedKeys: Set<string>) {

    // for each used key (whether from layer, gesture, etc.)
    // push these in id order, for tidiness
    for (let keyId of Array.from(usedKeys.values()).sort()) {
      const key = keyBag.get(keyId);
      if (!key) continue; // missing key

      let flags = 0;
      const { flickId, gap, longPressDefaultKeyId, longPressKeyIds, multiTapKeyIds, layerId, output } = key;
      if (!!gap) {
        flags |= constants.keys_key_flags_gap;
      }
      const id = sections.strs.allocString(key.id);
      const longPress: ListItem = sections.list.allocListFromSpaces(
        longPressKeyIds, {},
        sections);

      const longPressDefault = sections.strs.allocString(longPressDefaultKeyId,
        {},
        sections);

      const multiTap: ListItem = sections.list.allocListFromSpaces(
        multiTapKeyIds,
        {},
        sections);
      const keySwitch = sections.strs.allocString(layerId); // 'switch' is a reserved word

      const toRaw = output;

      let toCooked = sections.vars.substituteStrings(toRaw, sections);
      toCooked = sections.vars.substituteMarkerString(toCooked);
      const to = sections.strs.allocString(toCooked,
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
        flicks: flickId,
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
   * Validate for purpose of kmap
   * @param hardware the 'form' parameter
   * @param layer
   * @param keyHash the keybag's hash
   * @returns true if valid
   */
  private validateHardwareLayerForKmap(
    hardware: string,
    layer: LDMLKeyboard.LKLayer,
    keyHash: Map<string, LDMLKeyboard.LKKey>
  ): boolean {
    let valid = true;

    const { modifiers } = layer;
    if (!validModifier(modifiers)) {
      this.callbacks.reportMessage(
        CompilerMessages.Error_InvalidModifier({ modifiers, layer: layer.id })
      );
      valid = false;
    }

    const badScans = new Set<number>();
    const keymap = this.getKeymapFromForm(hardware, badScans);
    if (!keymap) {
      this.callbacks.reportMessage(
        CompilerMessages.Error_InvalidHardware({ formId: hardware })
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
            modifiers,
          })
        );
        valid = false;
      }

      let x = -1;
      for (let key of keys) {
        x++;

        let keydef = keyHash.get(key);
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
        if (!keydef.output && !keydef.gap && !keydef.layerId) {
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
