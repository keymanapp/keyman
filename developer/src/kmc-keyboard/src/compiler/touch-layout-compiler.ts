import { TouchLayout, LDMLKeyboard } from "@keymanapp/common-types";

export class TouchLayoutCompiler {
  public compileToJavascript(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile): TouchLayout.TouchLayoutFile {
    let result: TouchLayout.TouchLayoutFile = {};

    // start with desktop to mimic vk emit
    result.desktop = {
      defaultHint: "none",  // TODO-LDML this should be optional
      layer: []
    };

    for(let layers of source.keyboard.layers) {
      for(let layer of layers.layer) {
        const resultLayer = this.compileHardwareLayer(source, result, layer);
        result.desktop.layer.push(resultLayer);
      }
    }
    return result;
  }

  private compileHardwareLayer(
    source: LDMLKeyboard.LDMLKeyboardXMLSourceFile,
    file: TouchLayout.TouchLayoutFile,
    layer: LDMLKeyboard.LKLayer
  ) {
    // TODO-LDML: consider consolidation with keys.ts?

    let fileLayer: TouchLayout.TouchLayoutLayer = {
      id: this.translateLayerIdToTouchLayoutShift(layer.id),
      row: []
    };

    let y = -1;
    for(let row of layer.row) {
      y++;

      let fileRow: TouchLayout.TouchLayoutRow = {id: y, key: []};
      fileLayer.row.push(fileRow);

      const keys = row.keys.split(' ');
      // let x = -1;
      for(let key of keys) {
        // x++;

        let keydef = source.keyboard.keys?.key?.find(x => x.id == key);

        let fileKey: TouchLayout.TouchLayoutKey = {
          id: 'K_???', // TODO-LDML: actual key identifier (K_ for default, T_ for others)
          text: keydef.to,
          // TODO-LDML: additional properties
        };
        fileRow.key.push(fileKey);
      }
    }

    return fileLayer;
  }

  private translateLayerIdToTouchLayoutShift(id: string) {
    if(id == 'base') {
      return 'default';
    }
    // TODO-LDML: other modifiers
    return 'default';
  }
}