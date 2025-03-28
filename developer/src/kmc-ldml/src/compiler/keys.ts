import { SectionIdent, constants } from '@keymanapp/ldml-keyboard-constants';
import { KMXPlus, Constants } from '@keymanapp/common-types';
import { LDMLKeyboard } from '@keymanapp/developer-utils';
import { LdmlCompilerMessages } from './ldml-compiler-messages.js';
import { SectionCompiler } from "./section-compiler.js";

import DependencySections = KMXPlus.DependencySections;
import Keys = KMXPlus.Keys;
import KeysKeys = KMXPlus.KeysKeys;
import ListItem = KMXPlus.ListItem;
import KeysFlicks = KMXPlus.KeysFlicks;
import { allUsedKeyIdsInFlick, allUsedKeyIdsInKey, allUsedKeyIdsInLayers, calculateUniqueKeys, hashFlicks, hashKeys, translateLayerAttrToModifier, validModifier } from '../util/util.js';
import { SubstitutionUse, Substitutions } from './substitution-tracker.js';

/** reserved name for the special gap key. space is not allowed in key ids. */
const reserved_gap = "gap (reserved)";

export class KeysCompiler extends SectionCompiler {
  /** keys that are of a reserved type */
  public static RESERVED_KEY = Symbol('Reserved Key');
  static validateSubstitutions(
    keyboard: LDMLKeyboard.LKKeyboard,
    st: Substitutions
  ): boolean {
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

    // process each used key. unused keys don't get checked.
    for (let keyId of usedKeys.values()) {
      const key = keyBag.get(keyId);
      if (!key) continue; // key not found is handled elsewhere.
      st.addStringAndMarkerSubstitution(SubstitutionUse.emit, key.output);
    }
    return true;
  }

  public get id() {
    return constants.section.keys;
  }

  public get dependencies(): Set<SectionIdent> {
    const defaults = new Set(<SectionIdent[]>[
      constants.section.elem,
      constants.section.list,
      constants.section.meta,
      constants.section.strs,
      constants.section.vars,
    ]);
    return defaults;
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
        this.callbacks.reportMessage(LdmlCompilerMessages.Warn_CustomForm({ id: form.id }));
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
            LdmlCompilerMessages.Error_MissingFlicks({ flickId, id: keyId })
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
            LdmlCompilerMessages.Error_GestureKeyNotFoundInKeyBag({keyId: gestureKeyId, parentKeyId: keyId, attribute: attrs.join(',')})
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

    // Now load the reserved keys and slip them in here
    const reservedKeys = this.getReservedKeys(sections);
    for (const key of reservedKeys.values()) {
      sect.keys.push(key);
    }

    return sect;
  }

  /** list of reserved keys, for tests */
  public static readonly reserved_keys = [ reserved_gap ];
  /** count of reserved keys, for tests */
  public static readonly reserved_count = KeysCompiler.reserved_keys.length;

  /** mark as reserved */
  private static asReserved(k : KeysKeys) : KeysKeys {
    const o = k as any;
    o[KeysCompiler.RESERVED_KEY] = true;
    return k;
  }

  /** true if a reserved key */
  public static isReserved(k : KeysKeys) : boolean {
    const o = k as any;
    return !!o[KeysCompiler.RESERVED_KEY];
  }

  /** load up all reserved keys */
  getReservedKeys(sections: KMXPlus.DependencySections) : Map<String, KeysKeys> {
    const r = new Map<String, KeysKeys>();

    // set up some constants..
    const no_string = sections.strs.allocString('');
    const no_list = sections.list.allocList([], {}, sections);

    // now add the reserved key(s).
    r.set(reserved_gap, KeysCompiler.asReserved({
      flags: constants.keys_key_flags_gap | constants.keys_key_flags_extend,
      id: sections.strs.allocString(reserved_gap),
      flicks: '',
      longPress: no_list,
      longPressDefault: no_string,
      multiTap: no_list,
      switch: no_string,
      to: no_string,
      width: 10.0, // 10 * .1
    }));

    if (r.size !== KeysCompiler.reserved_count) {
      throw Error(`Internal Error: KeysCompiler.reserved_count=${KeysCompiler.reserved_count} != ${r.size} actual reserved keys.`);
    }

    return r;
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
          singleOk: true,
          nfd: true,
        },
        sections);
      if (!to.isOneChar) {
        flags |= constants.keys_key_flags_extend;
      }
      const width = Math.ceil((key.width || 1) * 10.0); // default, width=1
      sect.keys.push(SectionCompiler.copySymbols({
        flags,
        flicks: flickId,
        id,
        longPress,
        longPressDefault,
        multiTap,
        switch: keySwitch, // 'switch' is a reserved word
        to,
        width,
      }, key));
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
        LdmlCompilerMessages.Error_InvalidModifier({ modifiers, layer: layer.id })
      );
      valid = false;
    }

    const badScans = new Set<number>();
    const keymap = this.getKeymapFromForm(hardware, badScans);
    if (!keymap) {
      this.callbacks.reportMessage(
        LdmlCompilerMessages.Error_InvalidHardware({ formId: hardware })
      );
      valid = false;
      return valid;
    } else if (badScans.size !== 0) {
      const codes = Array.from(badScans.values()).map(n => Number(n).toString(16)).sort();
      this.callbacks.reportMessage(
        LdmlCompilerMessages.Error_InvalidScanCode({ form: hardware, codes })
      );
      valid = false;
      return valid;
    }

    if (layer.row.length > keymap.length) {
      this.callbacks.reportMessage(
        LdmlCompilerMessages.Error_HardwareLayerHasTooManyRows()
      );
      valid = false;
    }

    for (let y = 0; y < layer.row.length && y < keymap.length; y++) {
      const keys = layer.row[y].keys.split(" ");

      if (keys.length > keymap[y].length) {
        this.callbacks.reportMessage(
          LdmlCompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({
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
            LdmlCompilerMessages.Error_KeyNotFoundInKeyBag({
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
            LdmlCompilerMessages.Error_KeyMissingToGapOrSwitch({ keyId: key })
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
    const mods = translateLayerAttrToModifier(layer);
    const keymap = this.getKeymapFromForm(hardware);

    // Iterate over rows (y) and cols (x) of the scancodes table.
    // Any assigned keys will be used until we run out of keys in each row,
    // and run out of rows. The rest will be reserved_gap.

    for (let y = 0; y < keymap.length; y++) {
      let keys : string[];

      // if there are keys, use them.
      if (y < layer.row.length ) {
        const row = layer.row[y];
        keys = row.keys.split(" ");
      } else {
        keys = [];
      }

      // all columns in this row
      for (let x = 0; x < keymap[y].length; x++) {
        const vkey = keymap[y][x]; // from the scan table

        let key = reserved_gap; // unless there's a key in this row
        if (x < keys.length) {
          key = keys[x];
        }
        // push every combination
        for (const mod of mods) {
          sect.kmap.push({
            vkey,
            mod,
            key, // key id, to be changed into key index at finalization
          });
        }
      }
    }
    return sect;
  }
}
