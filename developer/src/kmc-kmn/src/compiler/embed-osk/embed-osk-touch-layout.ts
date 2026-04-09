/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Convert .keyman-touch-layout data into KMXPlus data for embedding into .kmx
 */
import { TouchLayout, KMXPlus, CharacterConstantString } from "@keymanapp/common-types";
import { CompilerCallbacks, TouchLayoutFileReader, oskFontMagicToken, specialKeyCaps } from "@keymanapp/developer-utils";
import { KmnCompilerMessages } from "../kmn-compiler-messages.js";
import { constants } from "@keymanapp/ldml-keyboard-constants";

export class EmbedOskTouchLayoutInKmx {

  private keyIndex: number = 0;

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

  public transformTouchLayoutToKmxPlus(kmx: KMXPlus.KMXPlusFile, touchLayout: TouchLayout.TouchLayoutFile): void {
    // empty the keys into layer bags
    // build the layers
    // don't forget all the gestures
    // TODO-EMBED-OSK-IN-KMX: still need to implement VKDictionary

    // transformTouchLayoutPlatform(kmx, tl, 'desktop', tl.desktop); // probably not needed

    this.keyIndex = 0;
    kmx.kmxplus.disp.baseCharacter = kmx.kmxplus.strs.allocString(CharacterConstantString.DOTTED_CIRCLE);
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
    newForm.fontFaceName = kmxplus.strs.allocString(oskFontMagicToken);
    newForm.fontSizePct = 100;
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

  private addLayerFromTouchLayoutLayer(kmxplus: KMXPlus.KMXPlusData, newForm: KMXPlus.LayrForm, platformName: TouchLayout.TouchLayoutPlatformName, layer: TouchLayout.TouchLayoutLayer): void {
    const newEntry = new KMXPlus.LayrEntry();
    newEntry.id = kmxplus.strs.allocString(layer.id);
    newEntry.mod = 0; // mod is unused for touch layouts
    for(const row of layer.row) {
      this.addRowFromTouchLayoutRow(kmxplus, newEntry, layer, row);
    }
    newForm.layers.push(newEntry);
  }

  private generateUniqueKeyId(id: string, layer: string) {
    this.keyIndex++;
    // TODO-EMBED-OSK-IN-KMX: report on uniqueness per form?
    return layer + '-' + id + '+' + this.keyIndex;
  }

  private addRowFromTouchLayoutRow(kmxplus: KMXPlus.KMXPlusData, newEntry: KMXPlus.LayrEntry, layer: TouchLayout.TouchLayoutLayer, row: TouchLayout.TouchLayoutRow): void {
    const newRow = new KMXPlus.LayrRow();

    for(const key of row.key) {
      this.addKeyFromTouchLayoutKey(kmxplus, newRow, layer, key);
    }

    newEntry.rows.push(newRow);
  }

  private addKeyFromTouchLayoutKey(kmxplus: KMXPlus.DependencySections, newRow: KMXPlus.LayrRow, layer: TouchLayout.TouchLayoutLayer, key: TouchLayout.TouchLayoutKey): void {
    const newKey = new KMXPlus.KeysKeys();

    newKey.id = kmxplus.strs.allocString(this.generateUniqueKeyId(key.id, key.layer ?? layer.id));
    newKey.to = this.getKeyCap(kmxplus, newKey.id.value, key.id, key.text);
    newKey.flags = newKey.to.isOneChar ? 0 : KMXPlus.KeysKeysFlags.extend;

    if(key.flick) {
      const newFlicks = new KMXPlus.KeysFlicks(newKey.id);
      for(const direction of Object.keys(key.flick) as (keyof TouchLayout.TouchLayoutFlick)[]) {
        this.addFlickFromTouchLayoutFlick(kmxplus, newFlicks, direction, layer, key.flick[direction]);
      }
      kmxplus.keys.flicks.push(newFlicks);
      newKey.flicks = newFlicks.id.value;
    } else {
      newKey.flicks = null;
    }

    if(key.sk && key.sk.length) {
      const { listItem, defaultId } = this.addKeysFromSubKeys(kmxplus, layer, key.sk);
      newKey.longPress = listItem;
      newKey.longPressDefault = defaultId;
    } else {
      newKey.longPress = null;
      newKey.longPressDefault = kmxplus.strs.allocString('');
    }

    if(key.multitap && key.multitap.length) {
      const { listItem } = this.addKeysFromSubKeys(kmxplus, layer, key.multitap);
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
  }

  private addKeysFromSubKeys(kmxplus: KMXPlus.DependencySections, layer: TouchLayout.TouchLayoutLayer, subKeys: TouchLayout.TouchLayoutSubKey[]) {
    let defaultId: KMXPlus.StrsItem = null;
    const ids: string[] = [];
    for(const subKey of subKeys) {
      const newKey = this.keyFromSubKey(kmxplus, layer, subKey);
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

  private addFlickFromTouchLayoutFlick(kmxplus: KMXPlus.DependencySections, flicks: KMXPlus.KeysFlicks, direction: string, layer: TouchLayout.TouchLayoutLayer, subKey: TouchLayout.TouchLayoutSubKey): void {
    const newFlick = new KMXPlus.KeysFlick();
    newFlick.directions = kmxplus.list.allocList([direction], {}, kmxplus);

    const newKey = this.keyFromSubKey(kmxplus, layer, subKey);
    kmxplus.keys.keys.push(newKey);

    newFlick.keyId = newKey.id;
    flicks.flicks.push(newFlick);
  }

  private keyFromSubKey(kmxplus: KMXPlus.DependencySections, layer: TouchLayout.TouchLayoutLayer, subKey: TouchLayout.TouchLayoutSubKey) {
    const newKey = new KMXPlus.KeysKeys();
    newKey.id = kmxplus.strs.allocString(this.generateUniqueKeyId(subKey.id, subKey.layer ?? layer.id));
    newKey.flicks = null;
    newKey.longPress = null;
    newKey.longPressDefault = kmxplus.strs.allocString('');
    newKey.multiTap = null;
    newKey.switch = kmxplus.strs.allocString(subKey.nextlayer || '');
    newKey.to = this.getKeyCap(kmxplus, newKey.id.value, subKey.id, subKey.text);
    newKey.flags = newKey.to.isOneChar ? 0 : KMXPlus.KeysKeysFlags.extend;
    newKey.width = 100;
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
      return kmxplus.strs.allocString(keyCap ?? '', {singleOk: true});
    }

    return kmxplus.strs.allocString(text, {singleOk: true});
  }

  private unicodeKeyIdToString(id: string) {
    // Test for fall back to U_xxxxxx key id
    // For this first test, we ignore the keyCode and use the keyName
    if(!id || id.substring(0, 2) != 'U_') {
      this.callbacks.reportMessage(KmnCompilerMessages.Warn_InvalidUnicodeKeyId_NaN({id, component: id}));
      return null;
    }

    let result = '';
    const codePoints = id.substring(2).split('_');
    for(const codePoint of codePoints) {
      if(!/^[0-9a-f]+$/i.test(codePoint) || codePoint.length > 6 || codePoint.length < 4) {
        // Cope with invalid identifiers (e.g. `U_ghij` or `U_24`)
        this.callbacks.reportMessage(KmnCompilerMessages.Warn_InvalidUnicodeKeyId_NaN({id, component: codePoint}));
        return null;
      }

      const codePointValue = parseInt(codePoint, 16);
      if (isNaN(codePointValue)) {
        // Any other out-of-range value for which parseInt returns NaN
        this.callbacks.reportMessage(KmnCompilerMessages.Warn_InvalidUnicodeKeyId_NaN({id, component: codePoint}));
        return null;
      }

      if((0x0 <= codePointValue && codePointValue <= 0x1F) || (0x7F <= codePointValue && codePointValue <= 0x9F)) {
        // Code points [U_0000 - U_001F] and [U_007F - U_009F] refer to Unicode C0 and C1 control codes.
        // Check the codePoint number and do not allow output of these codes via U_xxxxxx shortcuts.
        this.callbacks.reportMessage(KmnCompilerMessages.Warn_InvalidUnicodeKeyId_Control({id, component: codePoint}));
        return null;
      }

      if(0xFFFE <= (codePointValue & 0xFFFF) || (0xFDD0 <= codePointValue && codePointValue <= 0xFDDF)) {
        // U+FFFE + U+FFFF, U+1FFFE + U+1FFFF, etc are noncharacters, as are
        // U+FDD0-U+FDEF and should never be produced
        this.callbacks.reportMessage(KmnCompilerMessages.Warn_InvalidUnicodeKeyId_Noncharacter({id, component: codePoint}));
        return null;
      }

      if(0xD800 <= codePointValue && codePointValue <= 0xDFFF) {
        // surrogate pairs, use the code point instead
        this.callbacks.reportMessage(KmnCompilerMessages.Warn_InvalidUnicodeKeyId_SurrogatePair({id, component: codePoint}));
        return null;
      }

      if(codePointValue < 0 || codePointValue > 0x10FFFF) {
        // out of range
        this.callbacks.reportMessage(KmnCompilerMessages.Warn_InvalidUnicodeKeyId_OutOfRange({id, component: codePoint}));
        return null;
      }

      result += String.fromCodePoint(codePointValue);
    }

    return result;
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
    unicodeKeyIdToString: this.unicodeKeyIdToString.bind(this),
  }

}