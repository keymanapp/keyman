import { TouchLayout } from "@keymanapp/common-types";
import { LDMLKeyboard } from "@keymanapp/developer-utils";

export class TouchLayoutCompiler {
  public compileToJavascript(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile): TouchLayout.TouchLayoutFile {
    let result: TouchLayout.TouchLayoutFile = {};

    // start with desktop to mimic vk emit
    result.desktop = {
      defaultHint: "none",  // TODO-LDML this should be optional
      layer: []
    };

    for(let layers of source.keyboard3.layers) {
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
      id: this.translateLayerIdToTouchLayoutLayerId(layer.id, layer.modifiers),
      row: []
    };

    let y = -1;

    for(let row of layer.row) {
      y++;

      let fileRow: TouchLayout.TouchLayoutRow = {id: y, key: []};
      fileLayer.row.push(fileRow);

      const keys = row.keys.split(' ');
      for(let key of keys) {
        const keydef = source.keyboard3.keys?.key?.find(x => x.id == key);
        if(keydef) {
          const fileKey: TouchLayout.TouchLayoutKey = {
            id: this.translateKeyIdentifierToTouch(keydef.id) as TouchLayout.TouchLayoutKeyId,
            text: keydef.output || '',
            // TODO-LDML: additional properties
          };
          fileRow.key.push(fileKey);
        } else {
          // TODO-LDML: consider logging missing keys
        }
      }
    }

    return fileLayer;
  }

  private translateLayerIdToTouchLayoutLayerId(id: string, modifier: string): string {
    // Touch layout layers have a set of reserved names that correspond to
    // hardware modifiers. We want to map these identifiers first before falling
    // back to the layer ids

    // The set of recognized layer identifiers is:
    //
    // touch          | LDML
    // ---------------+-------------
    // default        | none
    // shift          | shift
    // caps           | caps
    // rightalt       | altR
    // rightalt-shift | altR shift
    //
    const map = {
      none:         'default',
      shift:        'shift',
      caps:         'caps',
      altR:         'rightalt',
      "altR shift": 'rightalt-shift'
    };

    // canonicalize modifier string, alphabetical
    // TODO-LDML: need to support multiple here
    if (modifier && modifier.indexOf(',') !== -1) {
      throw Error(`Internal error: TODO-LDML: multiple modifiers ${modifier} not yet supported.`);
    }
    modifier = (modifier||'').split(/\b/).sort().join(' ').trim();

    if(Object.hasOwn(map, modifier)) {
      return (map as any)[modifier];
    }

    // TODO-LDML: Other layer names will be used unmodified, is this sufficient?
    return id;
  }

  private translateKeyIdentifierToTouch(id: string): string {
    // Note: keys identifiers in kmx were traditionally case-insensitive, but we
    // are going to use them as case-insensitive for LDML keyboards. The set of
    // standard key identifiers a-z, A-Z, 0-9 will be used, where possible, and
    // all other keys will be mapped to `T_key`.

    if(id.match(/^[0-9a-zA-Z]$/)) {
      return 'K_'+id;
    }

    // Not a standard key
    return 'T_'+id;
  }
}
