/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Convert .keyman-touch-layout data into KMXPlus data for embedding into .kmx
 */
import { TouchLayout, KMXPlus, modifierStringToState, USVirtualKeyCodes } from "@keymanapp/common-types";
import { CompilerCallbacks, TouchLayoutFileReader, specialKeyCaps } from "@keymanapp/developer-utils";
import { KmnCompilerMessages } from "../kmn-compiler-messages.js";
import { constants } from "@keymanapp/ldml-keyboard-constants";

export class EmbedOskTouchLayoutInKmx {

  private keyIndex: number = 0;
  private vkDictionary: string[] = [];

  constructor(private callbacks: CompilerCallbacks) {
  }

  public loadTouchLayoutFile(filename: string): TouchLayout.TouchLayoutFile {
    const data = this.callbacks.loadFile(filename);
    if(!data) {
      this.callbacks.reportMessage(KmnCompilerMessages.Error_FileNotFound({filename}));
      return null;
    }

    const reader = new TouchLayoutFileReader();
    try {
      const touchLayout = reader.read(data);
      reader.validate(touchLayout);
      return touchLayout;
    } catch(e) {
      this.callbacks.reportMessage(KmnCompilerMessages.Error_InvalidTouchLayoutFile({filename, message: e?.message ?? 'unknown error'}));
      return null;
    }
  }

  public transformTouchLayoutToKmxPlus(kmx: KMXPlus.KMXPlusFile, touchLayout: TouchLayout.TouchLayoutFile, vkDictionary: string): void {
    // empty the keys into layer bags
    // build the layers
    // don't forget all the gestures
    // TODO-EMBED-OSK-IN-KMX: still need to implement VKDictionary

    // transformTouchLayoutPlatform(kmx, tl, 'desktop', tl.desktop); // probably not needed

    this.keyIndex = 0;
    this.vkDictionary = vkDictionary.split(/\s+/);
    kmx.kmxplus.disp.baseCharacter = kmx.kmxplus.strs.allocString('\u25cc');
    this.addPlatformFromTouchLayoutPlatform(kmx.kmxplus, 'tablet', touchLayout.tablet, 200); // anything larger than 200mm width
    this.addPlatformFromTouchLayoutPlatform(kmx.kmxplus, 'phone', touchLayout.phone, 1); // anything larger than 1mm width
  }

  private addPlatformFromTouchLayoutPlatform(kmxplus: KMXPlus.KMXPlusData, platformName: TouchLayout.TouchLayoutPlatformName, platform: TouchLayout.TouchLayoutPlatform, deviceWidth: number): void {
    if(!platform) {
      // The platform may not be used in the touch layout; this is fine
      return;
    }

    const newForm = new KMXPlus.LayrForm();
    newForm.baseLayout = kmxplus.strs.allocString('en-us'); // TODO-EMBED-OSK-IN-KMX: should this be null for touch?
    newForm.hardware = kmxplus.strs.allocString(KMXPlus.LayrFormHardware.touch);
    newForm.flags = platform.displayUnderlying ? KMXPlus.LayrFormFlags.showBaseLayout : 0;
    newForm.minDeviceWidth = deviceWidth;
    newForm.layers = [];

    // platform.defaultHint; TODO-EMBED-OSK-IN-KMX
    // platform.font; [ignore]
    // platform.fontsize; [ignore]

    for(const layer of platform.layer) {
      this.addLayerFromTouchLayoutLayer(kmxplus, newForm, platformName, layer);
    }

    kmxplus.layr.forms.push(newForm);
  }

  private addLayerFromTouchLayoutLayer(kmxplus: KMXPlus.KMXPlusData, newForm: KMXPlus.LayrForm, platform: TouchLayout.TouchLayoutPlatformName, layer: TouchLayout.TouchLayoutLayer): void {
    const newEntry = new KMXPlus.LayrEntry();
    newEntry.id = kmxplus.strs.allocString(layer.id);
    newEntry.mod = 0;
    for(const row of layer.row) {
      this.addRowFromTouchLayoutRow(kmxplus, newEntry, platform, layer, row);
    }
    newForm.layers.push(newEntry);
  }

  /**
   * Generates a unique key identifier from the touch layout platform, layer and
   * key id. The pattern returned is `platform~layer-id+modifier`; this pattern
   * (without the platform~ prefix) matches the identifiers used in KeymanWeb
   * and has been chosen for backward compatibility. These identifiers should be
   * stable across versions of Keyman.
   *
   * If a key identifier is repeated, then a `~n` suffix will be appended, where
   * `n` is an incrementing counter guaranteeing uniqueness.
   *
   * @param keys
   * @param id        the KeymanWeb key id, K_, T_, or U_
   * @param layer     the id of the layer in which the key is found
   * @param keyModifier  an overriding modifier state (see modifierNames in
   * developer/src/tike/xml/layoutbuilder/constants.js)
   * @returns
   */
  private generateUniqueKeyId(keys: KMXPlus.Keys, platform: string, layer: string, id: string, keyModifier: string) {
    // key id in KeymanWeb is `layer-id+override`
    // we need to pass in this data so that rules can still be applied
    // `+override` is the modifier for the key to be applied in the kmap -- not the layer
    const baseId = platform + '~' + layer + '-' + id + (keyModifier !== '' ? '+'+keyModifier : '');

    const key = keys.keys.find(item => item.id.value == baseId);
    if(!key) {
      return baseId;
    }

    this.keyIndex++;
    const resolvedId = baseId + '~' + this.keyIndex;
    this.callbacks.reportMessage(KmnCompilerMessages.Warn_TouchLayoutKeyIdUsedMoreThanOnceInALayer({id, layer, resolvedId}));
    return resolvedId;
  }

  private addRowFromTouchLayoutRow(kmxplus: KMXPlus.KMXPlusData, newEntry: KMXPlus.LayrEntry, platform: TouchLayout.TouchLayoutPlatformName, layer: TouchLayout.TouchLayoutLayer, row: TouchLayout.TouchLayoutRow): void {
    const newRow = new KMXPlus.LayrRow();

    for(const key of row.key) {
      this.addKeyFromTouchLayoutKey(kmxplus, newRow, platform, layer, key);
    }

    newEntry.rows.push(newRow);
  }

  private addKeyFromTouchLayoutKey(kmxplus: KMXPlus.DependencySections, newRow: KMXPlus.LayrRow, platform: TouchLayout.TouchLayoutPlatformName, layer: TouchLayout.TouchLayoutLayer, key: TouchLayout.TouchLayoutKey): void {
    const newKey = new KMXPlus.KeysKeys();

    newKey.id = kmxplus.strs.allocString(this.generateUniqueKeyId(kmxplus.keys, platform, layer.id, key.id, key.layer ?? ''));
    newKey.to = this.getKeyCap(kmxplus, newKey.id.value, key.id, key.text);
    newKey.flags = newKey.to.isOneChar ? 0 : KMXPlus.KeysKeysFlags.extend;

    if(key.flick) {
      const newFlicks = new KMXPlus.KeysFlicks(newKey.id);
      for(const direction of Object.keys(key.flick) as (keyof TouchLayout.TouchLayoutFlick)[]) {
        this.addFlickFromTouchLayoutFlick(kmxplus, newFlicks, direction, platform, layer, key.flick[direction]);
      }
      kmxplus.keys.flicks.push(newFlicks);
      newKey.flicks = newFlicks.id.value;
    } else {
      newKey.flicks = null;
    }

    if(key.sk && key.sk.length) {
      const { listItem, defaultId } = this.addKeysFromSubKeys(kmxplus, platform, layer, key.sk);
      newKey.longPress = listItem;
      newKey.longPressDefault = defaultId;
    } else {
      newKey.longPress = null;
      newKey.longPressDefault = kmxplus.strs.allocString('');
    }

    if(key.multitap && key.multitap.length) {
      const { listItem } = this.addKeysFromSubKeys(kmxplus, platform, layer, key.multitap);
      newKey.multiTap = listItem;
    } else {
      newKey.multiTap = null;
    }

    newKey.switch = kmxplus.strs.allocString(key.nextlayer || '');
    newKey.width = key.width ?? 100;

    if(key.hint) {
      const newHintDisp: KMXPlus.DispItem = {
        to: null, // not used in v19
        id: null, // not used in v19
        display: kmxplus.strs.allocString(key.hint),
        toId: newKey.id,
        flags:
          KMXPlus.DispItemFlags.hintNE |
          KMXPlus.DispItemFlags.isId, // TODO-EMBED-OSK-IN-KMX: Do we need to support special key caps here?
      };
      kmxplus.disp.disps.push(newHintDisp);
    }

          // TODO-EMBED-OSK-IN-KMX key.pad
          // TODO-EMBED-OSK-IN-KMX key.sp

    kmxplus.keys.keys.push(newKey);
    newRow.keys.push(newKey.id);

    this.addKmap(kmxplus, newKey, layer.id, key.id, key.layer, key.text);
  }

  private addKmap(kmxplus: KMXPlus.DependencySections, newKey: KMXPlus.KeysKeys, layerId: string, keyId: string, keyModifier: string, keyText: string) {
    const newKmap = new KMXPlus.KeysKmap();
    newKmap.key = newKey.id.value;
    newKmap.mod = modifierStringToState(keyModifier ?? layerId);
    newKmap.vkey = this.keyCodeToVkey(layerId, keyId, keyText);
    kmxplus.keys.kmap.push(newKmap);
  }

  private keyCodeToVkey(layerId: string, id: string, text: string) {
    id = id.toUpperCase();

    // K_ --> always look up from standard map, give warning if not found, return 0
    if(id.startsWith('K_')) {
      const result: number = (<any>USVirtualKeyCodes)[id];
      if(typeof result !== 'number') {
        this.callbacks.reportMessage(KmnCompilerMessages.Warn_TouchLayoutInvalidKeyId({layerId, id}));
        return 0;
      }
      if(result > 255) {
        // TODO-EMBED-OSK-IN-KMX: Do we need to do something with this? These are keys like K_LOPT,
        // and also what about K_ENTER, etc?
        return 0;
      }

      return result;
    }

    // T_ --> look up from VKDictionary, give warning if not found, return 0
    if(id.startsWith('T_')) {
      const index = this.vkDictionary.findIndex(key => key.toUpperCase() === id);
      if(index < 0) {
        if(text != '') {
          // Only warn if the key cap isn't blank
          this.callbacks.reportMessage(KmnCompilerMessages.Warn_TouchLayoutCustomKeyNotDefined({keyId: id, platformName: 'TODO-EMBED-OSK-IN-KMX', layerId, address: {keyIndex:0, rowIndex:0}}));
        }
        return 0;
      }

      return index + 256;
    }

    // U_ --> look up from VKDictionary, return 0 if not found, will map at runtime (no warning)
    if(id.startsWith('U_')) {
      const index = this.vkDictionary.findIndex(key => key.toUpperCase() === id);
      if(index < 0) {
        // will be mapped at runtime
        return 0;
      }

      return index + 256;
    }

    // TODO-EMBED-OSK-IN-KMX: warn on blank ids, missing required keys, error on missing layers

    // invalid ID, give warning, return 0
    this.callbacks.reportMessage(KmnCompilerMessages.Warn_TouchLayoutUnidentifiedKey({layerId, address:{keyIndex:0,rowIndex:0}})); // TODO-EMBED-OSK-IN-KMX: proper address
    return 0;
  }

  private addKeysFromSubKeys(kmxplus: KMXPlus.DependencySections, platform: TouchLayout.TouchLayoutPlatformName, layer: TouchLayout.TouchLayoutLayer, subKeys: TouchLayout.TouchLayoutSubKey[]) {
    let defaultId: KMXPlus.StrsItem = null;
    const ids: string[] = [];
    for(const subKey of subKeys) {
      const newKey = this.keyFromSubKey(kmxplus, platform, layer, subKey);
      kmxplus.keys.keys.push(newKey);
      if(subKey.default) {
        defaultId = newKey.id;
      }
      ids.push(newKey.id.value);
    }

    if(defaultId === null) {
      defaultId = kmxplus.strs.allocString(ids.length ? ids[0] : '');
    }

    const listItem = kmxplus.list.allocList(ids, {}, kmxplus);
    return { listItem, defaultId };
  }

  private addFlickFromTouchLayoutFlick(kmxplus: KMXPlus.DependencySections, flicks: KMXPlus.KeysFlicks, direction: string, platform: TouchLayout.TouchLayoutPlatformName, layer: TouchLayout.TouchLayoutLayer, subKey: TouchLayout.TouchLayoutSubKey): void {
    const newFlick = new KMXPlus.KeysFlick();
    newFlick.directions = kmxplus.list.allocList([direction], {}, kmxplus);

    const newKey = this.keyFromSubKey(kmxplus, platform, layer, subKey);
    kmxplus.keys.keys.push(newKey);

    newFlick.keyId = newKey.id;
    flicks.flicks.push(newFlick);
  }

  private keyFromSubKey(kmxplus: KMXPlus.DependencySections, platform: TouchLayout.TouchLayoutPlatformName, layer: TouchLayout.TouchLayoutLayer, subKey: TouchLayout.TouchLayoutSubKey) {
    const newKey = new KMXPlus.KeysKeys();
    newKey.id = kmxplus.strs.allocString(this.generateUniqueKeyId(kmxplus.keys, platform, layer.id, subKey.id, subKey.layer ?? ''));
    newKey.flicks = null;
    newKey.longPress = null;
    newKey.longPressDefault = kmxplus.strs.allocString('');
    newKey.multiTap = null;
    newKey.switch = kmxplus.strs.allocString(subKey.nextlayer || '');
    newKey.to = this.getKeyCap(kmxplus, newKey.id.value, subKey.id, subKey.text);
    newKey.flags = newKey.to.isOneChar ? 0 : KMXPlus.KeysKeysFlags.extend;
    newKey.width = 100;

    this.addKmap(kmxplus, newKey, layer.id, subKey.id, subKey.layer, subKey.text);

    return newKey;
  }

  private getKeyCap(kmxplus: KMXPlus.DependencySections, destId: string, sourceId: string, text: string) {
    text = text ?? '';
    if(text.match(/^\*(.+)\*$/)) {
      if(typeof((<any>specialKeyCaps)[text]) != 'number') {
        this.callbacks.reportMessage(KmnCompilerMessages.Warn_TouchLayoutSpecialLabelNotValid({id: sourceId, text}));
      } else {
        const keyCap = (<any>specialKeyCaps)[text];
        const newDisp: KMXPlus.DispItem = {
          to: null, // not used in v19
          id: null, // not used in v19
          display: kmxplus.strs.allocString(''),
          toId: kmxplus.strs.allocString(destId),
          flags:
            KMXPlus.DispItemFlags.hintPrimary |
            KMXPlus.DispItemFlags.isId |
            (keyCap << constants.disp_item_flags_shift_key_cap_type)
        }
        kmxplus.disp.disps.push(newDisp);
        // the base key cap text is empty because we use the `disp` structure
        // created above to present an iconic key cap
        return kmxplus.strs.allocString('');
      }
    } else if(text.trim() === '' && sourceId.startsWith('U_')) {
      // if key cap == U_xxxx[_yyyy], then we generate key cap from that
      const keyCap = this.unicodeKeyIdToString(sourceId);
      if(keyCap === null) {
        this.callbacks.reportMessage(KmnCompilerMessages.Warn_InvalidUnicodeKeyId({id: sourceId}));
      }
      return kmxplus.strs.allocString(keyCap ?? '', {singleOk: true});
    }

    return kmxplus.strs.allocString(text, {singleOk: true});
  }

  private unicodeKeyIdToString(id: string) {
    // Test for fall back to U_xxxxxx key id
    // For this first test, we ignore the keyCode and use the keyName
    if(!id || id.substring(0, 2) != 'U_') {
      return null;
    }

    let result = '';
    const codePoints = id.substring(2).split('_');
    for(const codePoint of codePoints) {
      const codePointValue = parseInt(codePoint, 16);
      if (isNaN(codePointValue) || (0x0 <= codePointValue && codePointValue <= 0x1F) || (0x80 <= codePointValue && codePointValue <= 0x9F)) {
        // Code points [U_0000 - U_001F] and [U_0080 - U_009F] refer to Unicode C0 and C1 control codes.
        // Check the codePoint number and do not allow output of these codes via U_xxxxxx shortcuts.
        // Also handles invalid identifiers (e.g. `U_ghij`) for which parseInt returns NaN

        // We'll still attempt to add valid chars
        continue;
      } else {
        result += String.fromCodePoint(codePointValue);
      }
    }
    return result ? result : null;
  }

  public unitTestEndpoints = {
    // public loadTouchLayoutFile: this.loadTouchLayoutFile.bind(this),
    // public transformTouchLayoutToKmxPlus: this.transformTouchLayoutToKmxPlus.bind(this),
    addPlatformFromTouchLayoutPlatform: this.addPlatformFromTouchLayoutPlatform.bind(this),
    addLayerFromTouchLayoutLayer: this.addLayerFromTouchLayoutLayer.bind(this),
    addRowFromTouchLayoutRow: this.addRowFromTouchLayoutRow.bind(this),
    addKeyFromTouchLayoutKey: this.addKeyFromTouchLayoutKey.bind(this),
    addKeysFromSubKeys: this.addKeysFromSubKeys.bind(this),
    addFlickFromTouchLayoutFlick: this.addFlickFromTouchLayoutFlick.bind(this),
    keyFromSubKey: this.keyFromSubKey.bind(this),
  }

}