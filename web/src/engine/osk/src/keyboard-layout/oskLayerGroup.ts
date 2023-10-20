import { ActiveLayer, type DeviceSpec, Keyboard, LayoutLayer, ActiveLayout } from '@keymanapp/keyboard-processor';

import { InputSample } from '@keymanapp/gesture-recognizer';

import { KeyElement } from '../keyElement.js';
import OSKLayer from './oskLayer.js';
import VisualKeyboard from '../visualKeyboard.js';
import OSKRow from './oskRow.js';

export default class OSKLayerGroup {
  public readonly element: HTMLDivElement;
  public readonly layers: {[layerID: string]: OSKLayer} = {};
  public readonly spec: ActiveLayout;

  public constructor(vkbd: VisualKeyboard, keyboard: Keyboard, formFactor: DeviceSpec.FormFactor) {
    let layout = keyboard.layout(formFactor);
    this.spec = layout;

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

      // Always make the first layer visible
      layerObj.element.style.display = (n==0 ? 'block' : 'none');

      // Add layer to group
      lDiv.appendChild(layerObj.element);
    }
  }

  findNearestKey(coord: Omit<InputSample<KeyElement>, 'item'>): KeyElement {
    if(!coord) {
      return null;
    }
    const layerId = coord.stateToken;
    if(!layerId) {
      throw new Error(`Layer id not set for input coordinate`);
    }

    const layer = this.layers[layerId];
    if(!layer) {
      throw new Error(`Layer id ${layerId} could not be found`);
    }

    // Kind of WET with VisualKeyboard.updateState, but oh well.
    let priorLayer: OSKLayer = null;
    if(layer.element.style.display == 'block') {
      priorLayer = layer;
    } else {
      for(let id in this.layers) {
        if(this.layers[id].element.style.display == 'block') {
          priorLayer = this.layers[id];
        }
        this.layers[id].element.style.display = 'none';
      }
    }
    layer.element.style.display = 'block';

    try {
      return this.nearestKey(coord, layer);
    } finally {
      if(layer != priorLayer) {
        layer.element.style.display = 'none';
        if(priorLayer) {
          priorLayer.element.style.display = 'block';
        }
      }
    }
  }

  private nearestKey(coord: Omit<InputSample<KeyElement>, 'item'>, layer: OSKLayer): KeyElement {
    const baseRect = this.element.getBoundingClientRect();

    let row: OSKRow = null;
    let bestMatchDistance = Number.MAX_VALUE;

    // Find the row that the touch-coordinate lies within.
    for(const r of layer.rows) {
      const rowRect = r.element.getBoundingClientRect();
      if(rowRect.top <= coord.clientY && coord.clientY < rowRect.bottom) {
        row = r;
        break;
      } else {
        const distance = rowRect.top > coord.clientY ? rowRect.top - coord.clientY : coord.clientY - rowRect.bottom;

        if(distance < bestMatchDistance) {
          bestMatchDistance = distance;
          row = r;
        }
      }
    }

    // Assertion:  row no longer `null`.

    // Warning: am not 100% sure that what follows is actually fully correct.

    // Find minimum distance from any key
    let closestKeyIndex = 0;
    let dx: number;
    let dxMax = 24;
    let dxMin = 100000;

    const x = coord.clientX;

    for (let k = 0; k < row.keys.length; k++) {
      // Second-biggest, though documentation suggests this is probably right.
      const keySquare = row.keys[k].square as HTMLElement; // gets the .kmw-key-square containing a key
      const squareRect = keySquare.getBoundingClientRect();

      // Find the actual key element.
      let childNode = keySquare.firstChild ? keySquare.firstChild as HTMLElement : keySquare;

      if (childNode.className !== undefined
        && (childNode.className.indexOf('key-hidden') >= 0
          || childNode.className.indexOf('key-blank') >= 0)) {
        continue;
      }
      const x1 = squareRect.left;
      const x2 = squareRect.right;
      if (x >= x1 && x <= x2) {
        // Within the key square
        return <KeyElement>childNode;
      }
      dx = x1 - x;
      if (dx >= 0 && dx < dxMin) {
        // To right of key
        closestKeyIndex = k; dxMin = dx;
      }
      dx = x - x2;
      if (dx >= 0 && dx < dxMin) {
        // To left of key
        closestKeyIndex = k; dxMin = dx;
      }
    }

    if (dxMin < 100000) {
      const t = <HTMLElement>row.keys[closestKeyIndex].square;
      const squareRect = t.getBoundingClientRect();

      const x1 = squareRect.left;
      const x2 = squareRect.right;

      // Limit extended touch area to the larger of 0.6 of key width and 24 px
      if (squareRect.width > 40) {
        dxMax = 0.6 * squareRect.width;
      }

      if (((x1 - x) >= 0 && (x1 - x) < dxMax) || ((x - x2) >= 0 && (x - x2) < dxMax)) {
        return <KeyElement>t.firstChild;
      }
    }
    return null;
  }
}