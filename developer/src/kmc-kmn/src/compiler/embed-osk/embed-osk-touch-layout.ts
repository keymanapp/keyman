/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Convert .keyman-touch-layout data into KMXPlus data for embedding into .kmx
 */
import { TouchLayout, KMXPlus } from "@keymanapp/common-types";
import { CompilerCallbacks, TouchLayoutFileReader, oskFontMagicToken } from "@keymanapp/developer-utils";
import { KmnCompilerMessages } from "../kmn-compiler-messages.js";

export class EmbedOskTouchLayoutInKmx {

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
      // TODO-EMBED-OSK-IN-KMX report on e
      this.callbacks.reportMessage(KmnCompilerMessages.Error_InvalidTouchLayoutFile({filename}));
      return null;
    }
  }

  public transformTouchLayoutToKmxPlus(kmx: KMXPlus.KMXPlusFile, tl: TouchLayout.TouchLayoutFile): void {
    // empty the keys into layer bags
    // build the layers
    // don't forget all the gestures
    // what about the VKDictionary?

    // transformTouchLayoutPlatform(kmx, tl, 'desktop', tl.desktop); // probably not needed

    this.addPlatformFromTouchLayoutPlatform(kmx.kmxplus, 'tablet', tl.tablet, 200); // anything larger than 200mm width
    this.addPlatformFromTouchLayoutPlatform(kmx.kmxplus, 'phone', tl.phone, 1); // anything larger than 1mm width
  }

  private addPlatformFromTouchLayoutPlatform(kmxplus: KMXPlus.KMXPlusData, platformName: TouchLayout.TouchLayoutPlatformName, platform: TouchLayout.TouchLayoutPlatform, deviceWidth: number): void {
    if(!platform) {
      // The platform may not be used in the touch layout; this is fine
      return;
    }

    const form = new KMXPlus.LayrForm();
    form.baseLayout = kmxplus.strs.allocString('en-us'); // TODO-EMBED-OSK-IN-KMX: should this be null for touch?
    form.hardware = kmxplus.strs.allocString(KMXPlus.LayrFormHardware.touch);
    form.fontFaceName = kmxplus.strs.allocString(oskFontMagicToken);
    form.fontSizePct = 100;
    form.flags = platform.displayUnderlying ? KMXPlus.LayrFormFlags.showBaseLayout : 0;
    form.minDeviceWidth = deviceWidth;
    form.layers = [];

    // platform.defaultHint; TODO-EMBED-OSK-IN-KMX
    // platform.font; [ignore]
    // platform.fontsize; [ignore]

    for(const layer of platform.layer) {
      this.addLayerFromTouchLayoutLayer(kmxplus, form, platformName, layer);
    }

    kmxplus.layr.forms.push(form);
  }

  private addLayerFromTouchLayoutLayer(kmxplus: KMXPlus.KMXPlusData, form: KMXPlus.LayrForm, platformName: TouchLayout.TouchLayoutPlatformName, layer: TouchLayout.TouchLayoutLayer): void {
    const entry = new KMXPlus.LayrEntry();
    entry.id = kmxplus.strs.allocString(layer.id); // TODO-EMBED-OSK-IN-KMX: is this correct?
    entry.mod = 0;
    for(const row of layer.row) {
      this.addRowFromTouchLayoutRow(kmxplus, entry, this.keyIdPrefix(platformName, layer.id), row);
    }
    form.layers.push(entry);
  }

  private keyIdPrefix(platformName: TouchLayout.TouchLayoutPlatformName, layerId: string) {
    return platformName + '-' + layerId + '-';
  }

  private addRowFromTouchLayoutRow(kmxplus: KMXPlus.KMXPlusData, entry: KMXPlus.LayrEntry, idPrefix: string, row: TouchLayout.TouchLayoutRow): void {
    const er = new KMXPlus.LayrRow();

    for(const key of row.key) {
      this.addKeyFromTouchLayoutKey(kmxplus, er, idPrefix, key);
    }

    entry.rows.push(er);
  }

  private addKeyFromTouchLayoutKey(kmxplus: KMXPlus.DependencySections, er: KMXPlus.LayrRow, idPrefix: string, key: TouchLayout.TouchLayoutKey): void {
    const lk = new KMXPlus.KeysKeys();

    lk.id = kmxplus.strs.allocString(idPrefix + key.id);
    lk.to = this.getKeyCap(kmxplus, key.id, key.text); // kmxplus.strs.allocString(subKey.text);

    lk.flags = 0; // TODO-EMBED-OSK-IN-KMX: extend,gap // KMXPlus.KeysKeysFlags.

    if(key.flick) {
      const flicks = new KMXPlus.KeysFlicks(lk.id);
      for(const direction of Object.keys(key.flick) as (keyof TouchLayout.TouchLayoutFlick)[]) {
        this.addFlickFromTouchLayoutFlick(kmxplus, flicks, direction, key.flick[direction]);
      }
      kmxplus.keys.flicks.push(flicks);
      lk.flicks = flicks.id.value;
    } else {
      lk.flicks = null;
    }

    if(key.sk && key.sk.length) {
      const { listItem, defaultId } = this.addKeysFromSubKeys(kmxplus, key.sk);
      lk.longPress = listItem;
      lk.longPressDefault = defaultId;
    } else {
      lk.longPress = null;
      lk.longPressDefault = kmxplus.strs.allocString('');
    }

    if(key.multitap && key.multitap.length) {
      const { listItem } = this.addKeysFromSubKeys(kmxplus, key.multitap);
      lk.multiTap = listItem;
    } else {
      lk.multiTap = null;
    }

    lk.switch = kmxplus.strs.allocString(key.nextlayer || '');


    lk.width = key.width ?? 100;

    if(key.hint) {
      const hintDisp: KMXPlus.DispItem = {
        to: null, // not used in v19
        id: null, // not used in v19
        display: kmxplus.strs.allocString(key.hint),
        toId: lk.id,
        flags:
          KMXPlus.DispItemFlags.hintNE |
          KMXPlus.DispItemFlags.isId, // TODO-EMBED-OSK-IN-KMX: Do we need to support special key caps here?
      };
      kmxplus.disp.disps.push(hintDisp);
    }

          // TODO-EMBED-OSK-IN-KMX key.pad
          // TODO-EMBED-OSK-IN-KMX key.sp

    kmxplus.keys.keys.push(lk);
    er.keys.push(lk.id);
  }

  private addKeysFromSubKeys(kmxplus: KMXPlus.DependencySections, sk: TouchLayout.TouchLayoutSubKey[]) {
    let defaultId: KMXPlus.StrsItem = null;
    const ids: string[] = [];
    for(const subKey of sk) {
      const lk = this.keyFromSubKey(kmxplus, subKey);
      kmxplus.keys.keys.push(lk);
      if(subKey.default) {
        defaultId = lk.id;
      }
      ids.push(lk.id.value);
    }

    if(defaultId === null) {
      defaultId = kmxplus.strs.allocString(ids.length ? ids[0] : '');
    }

    const listItem = kmxplus.list.allocList(ids, {}, kmxplus);
    return { listItem, defaultId };
  }

  private addFlickFromTouchLayoutFlick(kmxplus: KMXPlus.DependencySections, flicks: KMXPlus.KeysFlicks, direction: string, subKey: TouchLayout.TouchLayoutSubKey): void {
    const flick = new KMXPlus.KeysFlick();
    flick.directions = kmxplus.list.allocList([direction], {}, kmxplus);
    flick.keyId = kmxplus.strs.allocString(subKey.id);

    const lk = this.keyFromSubKey(kmxplus, subKey);
    kmxplus.keys.keys.push(lk);
    flicks.flicks.push(flick);
  }

  private keyFromSubKey(kmxplus: KMXPlus.DependencySections, subKey: TouchLayout.TouchLayoutSubKey) {
    const lk = new KMXPlus.KeysKeys();
    lk.id = kmxplus.strs.allocString(subKey.id);
    lk.flags = 0;
    lk.flicks = null;
    lk.longPress = null;
    lk.longPressDefault = kmxplus.strs.allocString('');
    lk.multiTap = null;
    lk.switch = kmxplus.strs.allocString(subKey.nextlayer || '');
    // TODO: Disp
    lk.to = this.getKeyCap(kmxplus, subKey.id, subKey.text); // kmxplus.strs.allocString(subKey.text);
    lk.width = 100;
    return lk;
  }

  private getKeyCap(kmxplus: KMXPlus.DependencySections, id: string, text: string) {
    text = text ?? '';
    if(text.match(/^\*(.+)\*$/)) {
      const disp: KMXPlus.DispItem = {
        to: null, // not used in v19
        id: null, // not used in v19
        display: kmxplus.strs.allocString(''),
        toId: kmxplus.strs.allocString(id),
        flags:
          KMXPlus.DispItemFlags.hintPrimary |
          KMXPlus.DispItemFlags.isId |
          KMXPlus.DispItemFlags.keyCap123, //TODO-EMBED-OSK-IN-KMX
      }
      kmxplus.disp.disps.push(disp);
      return kmxplus.strs.allocString('');
    } else if(text.trim() === '' && id.startsWith('U_')) {
      // if key cap == U_xxxx[_yyyy], then we generate key cap from that
      return kmxplus.strs.allocString(this.unicodeKeyIdToString(id));
    } else {
      return kmxplus.strs.allocString(text);
    }
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