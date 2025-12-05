/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Export LDML data (https://www.unicode.org/reports/tr35/tr35-keyboards.html)
 * to .kvk format. This is an interim solution until Keyman Core supports
 * interrogation of the KMX+ data for OSK.
 */
import { KMXPlus, VisualKeyboard, translateLdmlModifiersToVisualKeyboardShift } from "@keymanapp/common-types";
import { CompilerCallbacks } from "@keymanapp/developer-utils";
import { LdmlCompilerMessages } from "./ldml-compiler-messages.js";
import { constants } from "@keymanapp/ldml-keyboard-constants";

// This is a partial polyfill for findLast, so not polluting Array.prototype
// https://medium.com/@stheodorejohn/findlast-method-polyfill-in-javascript-bridging-browser-gaps-c3baf6aabae1
// TODO: remove and replace with Array.prototype.findLast when it is
// well-supported
function findLast(arr: any, callback: any) {
  if (!arr) {
    return undefined;
  }
  const len = arr.length >>> 0;
  for (let i = len - 1; i >= 0; i--) {
    if (callback(arr[i], i, arr)) {
      return arr[i];
    }
  }
  return undefined;
}

export class LdmlKeyboardVisualKeyboardCompiler {
  public constructor(private callbacks: CompilerCallbacks) {
  }

  /**
   * Generate a visual keyboard
   * @param source      Compiled KMX+ data; note that this is modified to add
   *                    &VISUALKEYBOARD system store on success
   * @param keyboardId  Basename of keyboard, without file extension
   * @returns           Visual keyboard data on success, null on failure, or
   *                    false if no VK was generated for this keyboard
   */
  public compile(source: KMXPlus.KMXPlusData, keyboardId: string): VisualKeyboard.VisualKeyboard | boolean | null {
    const result = new VisualKeyboard.VisualKeyboard();

    /* TODO-LDML: consider VisualKeyboardHeaderFlags.kvkhUseUnderlying kvkhDisplayUnderlying kvkhAltGr kvkh102 */
    result.header.flags = 0;
    result.header.version = 0x0600;
    result.header.associatedKeyboard = keyboardId;
    result.header.ansiFont = {...VisualKeyboard.DEFAULT_KVK_FONT};
    result.header.unicodeFont = {...VisualKeyboard.DEFAULT_KVK_FONT};

    let hasVisualKeyboard = false;

    for(const layersForm of source.layr.forms) {
      const formId = layersForm.hardware.value;
      if(formId == 'touch') {
        continue;
      }

      for(const layer of layersForm.layers) {
        const res = this.compileHardwareLayer(source, result, layer, formId);
        if(res === false) {
          // failed to compile the layer
          return null;
        }
        if(res === null) {
          // not a supported layer type, but not an error
          continue;
        }
        hasVisualKeyboard = true;
      }
    }

    if(!hasVisualKeyboard) {
      return false;
    }

    return result;
  }

  private compileHardwareLayer(
    source: KMXPlus.KMXPlusData,
    vk: VisualKeyboard.VisualKeyboard,
    layer: KMXPlus.LayrEntry,
    hardware: string,
  ) {
    const layerId = layer.id.value;

    hardware = 'us'; // TODO-LDML: US Only. We need to clean this up for other hardware forms

    const shift = translateLdmlModifiersToVisualKeyboardShift(layer.mod);
    if(shift === null) {
      // Caps (num, scroll) is not a supported shift state in .kvk
      return null;
    }

    let result = true;
    let y = -1;
    for(const row of layer.rows) {
      y++;
      let x = -1;
      for(const key of row.keys) {
        x++;

        const keydef: KMXPlus.KeysKeys = findLast(source.keys?.keys, (kd: KMXPlus.KeysKeys) => kd.id.value == key.value);
        const kmap = source.keys.kmap.find(k => k.key == keydef.id.value && k.mod == layer.mod);
        const text = this.getDisplayFromKey(keydef, source) ?? null;

        if (!keydef || !kmap || text === null) {
          this.callbacks.reportMessage(
            LdmlCompilerMessages.Error_KeyNotFoundInKeyBag({
              keyId: key.value, layer: layerId, row: y, col: x, form: hardware
            }, row)
          );
          result = false;
        } else {
          vk.keys.push({
            flags: VisualKeyboard.VisualKeyboardKeyFlags.kvkkUnicode,
            shift,
            text,
            vkey: kmap.vkey
          });
        }
      }
    }
    return result;
  }

  private getDisplayFromKey(keydef: KMXPlus.KeysKeys, source: KMXPlus.KMXPlusData) {
    // if(source.disp.disps[0].toId)
    const display = source.disp?.disps?.find(d =>
      d.toId !== null
      ? (d.toId.value == keydef.id.value && d.flags & constants.disp_item_flags_is_id) ||
        (d.toId.value == keydef.to.value && !(d.flags & constants.disp_item_flags_is_id))
      : d.id.value == keydef.id.value || d.to.value == keydef.to.value);
    const value = display?.display.value ?? keydef.to.value;
    // strip markers from the output (these are valid in keydef.to, but not in display.display, nor in kvk)
    return value.replaceAll(/\uffff\u0008./g, '');
  }

}
