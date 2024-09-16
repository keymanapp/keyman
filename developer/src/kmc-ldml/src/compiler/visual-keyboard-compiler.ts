/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Export LDML data (https://www.unicode.org/reports/tr35/tr35-keyboards.html)
 * to .kvk format. This is an interim solution until Keyman Core supports
 * interrogation of the KMX+ data for OSK.
 */
import { ModifierKeyConstants, KMXPlus } from "@keymanapp/common-types";
import { VisualKeyboard, CompilerCallbacks } from "@keymanapp/common-types";
import { CompilerMessages } from "./messages.js";

// This is a partial polyfill for findLast, so not polluting Array.prototype
//
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


const LDML_MODIFIER_TO_KVK_MODIFIER = new Map<number, number>();
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.LCTRLFLAG,      VisualKeyboard.VisualKeyboardShiftState.KVKS_LCTRL);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.RCTRLFLAG,      VisualKeyboard.VisualKeyboardShiftState.KVKS_RCTRL);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.LALTFLAG,       VisualKeyboard.VisualKeyboardShiftState.KVKS_LALT);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.RALTFLAG,       VisualKeyboard.VisualKeyboardShiftState.KVKS_RALT);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.K_SHIFTFLAG,    VisualKeyboard.VisualKeyboardShiftState.KVKS_SHIFT);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.K_CTRLFLAG,     VisualKeyboard.VisualKeyboardShiftState.KVKS_CTRL);
LDML_MODIFIER_TO_KVK_MODIFIER.set(ModifierKeyConstants.K_ALTFLAG,      VisualKeyboard.VisualKeyboardShiftState.KVKS_ALT);

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
    let result = new VisualKeyboard.VisualKeyboard();

    /* TODO-LDML: consider VisualKeyboardHeaderFlags.kvkhUseUnderlying kvkhDisplayUnderlying kvkhAltGr kvkh102 */
    result.header.flags = 0;
    result.header.version = 0x0600;
    result.header.associatedKeyboard = keyboardId;
    result.header.ansiFont = {...VisualKeyboard.DEFAULT_KVK_FONT};
    result.header.unicodeFont = {...VisualKeyboard.DEFAULT_KVK_FONT};

    let hasVisualKeyboard = false;

    for(let layersList of source.layr.lists) {
      const formId = layersList.hardware.value;
      if(formId == 'touch') {
        continue;
      }

      for(let layer of layersList.layers) {
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

    const shift = this.translateLayerModifiersToVisualKeyboardShift(layer.mod);
    if(shift === null) {
      // Caps (num, scroll) is not a supported shift state in .kvk
      return null;
    }

    let result = true;
    let y = -1;
    for(let row of layer.rows) {
      y++;
      let x = -1;
      for(let key of row.keys) {
        x++;

        const keydef: KMXPlus.KeysKeys = findLast(source.keys?.keys, (kd: KMXPlus.KeysKeys) => kd.id.value == key.value);
        const kmap = source.keys.kmap.find(k => k.key == keydef.id.value && k.mod == layer.mod);
        const text = this.getDisplayFromKey(keydef, source) ?? null;

        if (!keydef || !kmap || text === null) {
          this.callbacks.reportMessage(
            CompilerMessages.Error_KeyNotFoundInKeyBag({ keyId: key.value, layer: layerId, row: y, col: x, form: hardware })
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
    const display = source.disp?.disps?.find(d => d.id.value == keydef.id.value || d.to.value == keydef.to.value);
    return display?.display.value ?? keydef.to.value;
  }

  private translateLayerModifiersToVisualKeyboardShift(modifiers: number): VisualKeyboard.VisualKeyboardShiftState {

    if(modifiers == 0) {
      return VisualKeyboard.VisualKeyboardShiftState.KVKS_NORMAL;
    }

    if(modifiers &
      (ModifierKeyConstants.CAPITALFLAG | ModifierKeyConstants.NUMLOCKFLAG | ModifierKeyConstants.SCROLLFLAG)
    ) {
      // Caps/Num/Scroll are not supported in .kvk, in combination or alone
      return null;
    }

    let shift: VisualKeyboard.VisualKeyboardShiftState = 0;

    for(const mod of LDML_MODIFIER_TO_KVK_MODIFIER.keys()) {
      if(modifiers & mod) {
        shift |= LDML_MODIFIER_TO_KVK_MODIFIER.get(mod);
      }
    }

    return shift;
  }
}
