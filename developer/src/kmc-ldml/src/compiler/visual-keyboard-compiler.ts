import { VisualKeyboard, LDMLKeyboard, CompilerCallbacks } from "@keymanapp/common-types";
import { KeysCompiler } from "./keys.js";
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

export class LdmlKeyboardVisualKeyboardCompiler {
  public constructor(private callbacks: CompilerCallbacks) {
  }

  public compile(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile): VisualKeyboard.VisualKeyboard {
    let result = new VisualKeyboard.VisualKeyboard();

    /* TODO-LDML: consider VisualKeyboardHeaderFlags.kvkhUseUnderlying kvkhDisplayUnderlying kvkhAltGr kvkh102 */
    result.header.flags = 0;
    result.header.version = 0x0600;

    /* TODO-LDML: consider associatedKeyboard: this _must_ be set to id (aka basename sans ext) of keyboard .kmx file */
    result.header.associatedKeyboard = '';
    result.header.ansiFont = {...VisualKeyboard.DEFAULT_KVK_FONT};
    result.header.unicodeFont = {...VisualKeyboard.DEFAULT_KVK_FONT};

    for(let layers of source.keyboard3.layers) {
      const { formId } = layers;
      for(let layer of layers.layer) {
        this.compileHardwareLayer(source, result, layer, formId);
      }
    }
    return result;
  }

  private compileHardwareLayer(
    source: LDMLKeyboard.LDMLKeyboardXMLSourceFile,
    vk: VisualKeyboard.VisualKeyboard,
    layer: LDMLKeyboard.LKLayer,
    hardware: string,
  ) {
    const layerId = layer.id;
    if (hardware === 'touch') {
      hardware = 'us'; // TODO-LDML: US Only. Do something different here?
    }
    const keymap = KeysCompiler.getKeymapFromForms(source.keyboard3?.forms?.form, hardware);
    if (!keymap) {
      this.callbacks.reportMessage(
        CompilerMessages.Error_InvalidHardware({ formId: hardware })
      );
      return;
    }
    const shift = this.translateLayerIdToVisualKeyboardShift(layer.id);

    let y = -1;
    for(let row of layer.row) {
      y++;

      const keys = row.keys.split(' ');
      let x = -1;
      for(let key of keys) {
        const keyId = key;
        x++;

        //@ts-ignore
        let keydef = findLast(source.keyboard3.keys?.key, x => x.id == key);

        if (!keydef) {
          this.callbacks.reportMessage(
            CompilerMessages.Error_KeyNotFoundInKeyBag({ keyId, layer: layerId, row: y, col: x, form: hardware })
          );
        } else {
          vk.keys.push({
            flags: VisualKeyboard.VisualKeyboardKeyFlags.kvkkUnicode,
            shift: shift,
            text: keydef.output, // TODO-LDML: displays
            vkey: keymap[y][x],
          });
        }
      }
    }
  }

  private translateLayerIdToVisualKeyboardShift(id: string) {
    if(id == 'base') {
      return 0;
    }
    // TODO-LDML: other modifiers
    return 0;
  }
}
