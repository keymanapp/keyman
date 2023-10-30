import { ActiveLayer, type DeviceSpec, Keyboard, LayoutLayer } from '@keymanapp/keyboard-processor';

import OSKLayer from './oskLayer.js';
import VisualKeyboard from '../visualKeyboard.js';

export default class OSKLayerGroup {
  public readonly element: HTMLDivElement;
  public readonly layers: {[layerID: string]: OSKLayer} = {};

  public constructor(vkbd: VisualKeyboard, keyboard: Keyboard, formFactor: DeviceSpec.FormFactor) {
    let layout = keyboard.layout(formFactor);

    const lDiv = this.element = document.createElement('div');
    const ls=lDiv.style;

    // Set OSK box default style
    lDiv.className='kmw-key-layer-group';

    // Return empty DIV if no layout defined
    if(layout == null) {
      return;
    }

    // Set default OSK font size (Build 344, KMEW-90)
    let layoutFS = layout['fontsize'];
    if(typeof layoutFS == 'undefined' || layoutFS == null || layoutFS == '') {
      ls.fontSize='1em';
    } else {
      ls.fontSize=layout['fontsize'];
    }

    ls.width = '100%';
    ls.height = '100%';

    // Create a separate OSK div for each OSK layer, only one of which will ever be visible
    var n: number, i: number, j: number;
    var layers: LayoutLayer[];

    layers=layout['layer'];

    // Set key default attributes (must use exportable names!)
    var tKey=vkbd.getDefaultKeyObject();
    tKey['fontsize']=ls.fontSize;

    for(n=0; n<layers.length; n++) {
      let layer=layers[n] as ActiveLayer;
      const layerObj = new OSKLayer(vkbd, layout, layer);
      this.layers[layer.id] = layerObj;

      // Always make the 'default' layer visible by default.
      layerObj.element.style.display = (layer.id == 'default' ? 'block' : 'none');

      // Add layer to group
      lDiv.appendChild(layerObj.element);
    }
  }
}