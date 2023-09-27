import { Constants, VisualKeyboard, LDMLKeyboard } from "@keymanapp/common-types";

export class LdmlKeyboardVisualKeyboardCompiler {
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
      for(let layer of layers.layer) {
        this.compileHardwareLayer(source, result, layer);
      }
    }
    return result;
  }

  private compileHardwareLayer(
    source: LDMLKeyboard.LDMLKeyboardXMLSourceFile,
    vk: VisualKeyboard.VisualKeyboard,
    layer: LDMLKeyboard.LKLayer
  ) {
    // TODO-LDML: consider consolidation with keys.ts?
    const shift = this.translateLayerIdToVisualKeyboardShift(layer.id);

    let y = -1;
    for(let row of layer.row) {
      y++;

      const keys = row.keys.split(' ');
      let x = -1;
      for(let key of keys) {
        x++;

        let keydef = source.keyboard3.keys?.key?.find(x => x.id == key);

        if (!keydef) {
          throw Error(`Internal Error: could not find key id="${key}" in layer "${layer.id || '<none>'}", row "${y}"`);
        }

        vk.keys.push({
          flags: VisualKeyboard.VisualKeyboardKeyFlags.kvkkUnicode,
          shift: shift,
          text: keydef.to, // TODO-LDML: displays
          vkey: Constants.USVirtualKeyMap[y][x] // TODO-LDML: #7965  US-only
        });
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
