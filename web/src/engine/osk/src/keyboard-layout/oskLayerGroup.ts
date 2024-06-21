import { ActiveLayer, type DeviceSpec, Keyboard, LayoutLayer, ActiveLayout, ButtonClasses } from '@keymanapp/keyboard-processor';

import { InputSample } from '@keymanapp/gesture-recognizer';

import { KeyElement } from '../keyElement.js';
import OSKLayer, { LayerLayoutParams } from './oskLayer.js';
import VisualKeyboard from '../visualKeyboard.js';
import OSKBaseKey from './oskBaseKey.js';

const NEAREST_KEY_TOUCH_MARGIN_PERCENT = 0.06;

export default class OSKLayerGroup {
  public readonly element: HTMLDivElement;
  public readonly layers: {[layerID: string]: OSKLayer} = {};
  public readonly spec: ActiveLayout;

  // Exist as local copies of the VisualKeyboard values, updated via refreshLayout.
  private computedWidth: number;
  private computedHeight: number;

  private _activeLayerId: string = 'default';
  private _heightPadding: number;

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
    let n: number;
    var layers: LayoutLayer[];

    layers=layout['layer'];

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

  public get activeLayer(): OSKLayer {
    if(!this.activeLayerId) {
      return null;
    }

    return this.layers[this.activeLayerId];
  }

  public get activeLayerId(): string {
    return this._activeLayerId;
  }

  public set activeLayerId(id: string) {
    this._activeLayerId = id;

    for (let key of Object.keys(this.layers)) {
      const layer = this.layers[key];
      const layerElement = layer.element;
      if (layer.id == id) {
        layerElement.style.display = 'block';
      } else {
        layerElement.style.display = 'none';
      }
    }
  }

  /**
   * The core function referenced by the gesture engine for determining the key that
   * best matches the state of contact points and ongoing gestures.
   *
   * Calls to this function may temporarily change which layer is set for display,
   * as layout reflows are necessary for lookups in layers not currently set active.
   * Such changes layer will be reverted once the JS microtask queue regains control;
   * this delay is to prevent costly layout thrashing effects.
   * @param coord
   * @returns
   */
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

    return this.nearestKey(coord, layer);
  }

  /**
   * Temporarily enables the specified layer for page layout calculations and
   * queues an immediate reversion to the 'true' active layer at the earliest
   * opportunity on the JS microtask queue.
   * @param layer
   */
  public blinkLayer(arg: OSKLayer | string) {
    if(typeof arg === 'string') {
      const layerId = arg;
      arg = this.layers[layerId];
      if(!arg) {
        throw new Error(`Layer id ${layerId} could not be found`);
      }
    }

    const layer = arg;

    // Note:  we do NOT manipulate `._activeLayerId` here!  This is designed
    // explicitly to be temporary.
    if(layer.element.style.display != 'block') {
      for(let id in this.layers) {
        if(this.layers[id].element.style.display == 'block') {
          const priorLayer = this.layers[id];
          priorLayer.element.style.display = 'none';
        }
        this.layers[id].element.style.display = 'none';
      }
    }
    layer.element.style.display = 'block';

    /* As soon as control returns to the JS microtask queue, restore the original layer.
     * We want to avoid doing it sooner in case another lookup occurs before the standard
     * async reflow, as that could trigger expensive "layout thrashing" effects.
     *
     * In the case that a gesture-source's path needs to be remapped to a different layer,
     * multiple synchronous calls to this method may occur.  This is a pattern that may
     * result during input layer-remapping used to solve issues like #7173 and possibly
     * also during multitap operations.
     *
     * On "layout thrashing": https://webperf.tips/tip/layout-thrashing/
     */
    Promise.resolve().then(() => {
      const trueLayer = this.layers[this._activeLayerId];
      // If either condition holds, we have to trigger a layout reflow; it's the same cost
      // whether one changes or both do.
      if(layer.element.style.display == 'block' || trueLayer.element.style.display != 'block') {
        layer.element.style.display = 'none';
        trueLayer.element.style.display = 'block';
      }
    });
  }

  private nearestKey(coord: Omit<InputSample<KeyElement>, 'item'>, layer: OSKLayer): KeyElement {
    // If there are no rows, there are no keys; return instantly.
    if(layer.rows.length == 0) {
      return null;
    }

    // Our pre-processed layout info maps whatever shape the keyboard is in into a unit square.
    // So, we map our coord to find its location within that square.
    const proportionalCoords = {
      x: coord.targetX / this.computedWidth,
      y: coord.targetY / this.computedHeight
    };

    // If our computed width and/or height are 0, it's best to abort; key distance
    // calculations are not viable.
    if(!isFinite(proportionalCoords.x) || !isFinite(proportionalCoords.y)) {
      return null;
    }

    // Step 1:  find the nearest row.
    // Rows aren't variable-height - this value is "one size fits all."

    /*
      If 4 rows, y = .2 x 4 = .8 - still within the row with index 0 (spanning from 0 to .25)
                 y = .6 x 4 = 2.4 - within row with index 2 (third row, spanning .5 to .75)

      Assumes there is no fine-tuning of the row ranges to be done - each takes a perfect
      fraction of the overall layer height without any padding above or below.
    */
    const rowIndex = Math.max(0, Math.min(layer.rows.length-1, Math.floor(proportionalCoords.y * layer.rows.length)));
    const row = layer.rows[rowIndex];

    // Assertion:  row no longer `null`.
    // (We already prevented the no-rows available scenario, anyway.)

    // Step 2: Find minimum distance from any key
    // - If the coord is within a key's square, go ahead and return it.
    let closestKey: OSKBaseKey = null;
    // Is percentage-based!
    let minDistance = Number.MAX_VALUE;

    for (let key of row.keys) {
      const keySpec = key.spec;
      if(keySpec.sp == ButtonClasses.blank || keySpec.sp == ButtonClasses.spacer) {
        continue;
      }

      // Max distance from the key's center to consider, horizontally.
      const keyRadius = keySpec.proportionalWidth / 2;
      const distanceFromCenter = Math.abs(proportionalCoords.x - keySpec.proportionalX);

      // Find the actual key element.
      if(distanceFromCenter - keyRadius <= 0) {
        // As noted above:  if we land within a key's square, match instantly!
        return key.btn;
      } else {
        const distance = distanceFromCenter - keyRadius;
        if(distance < minDistance) {
          minDistance = distance;
          closestKey = key;
        }
      }
    }

    /*
      Step 3:  If the input coordinate wasn't within any valid key's "square",
      determine if the nearest valid key is acceptable - if it's within 60% of
      a standard key's width from the touch location.

      If the condition is not met, there are no valid keys within this row.
    */
    if (minDistance /* %age-based! */ <= NEAREST_KEY_TOUCH_MARGIN_PERCENT) {
      return closestKey.btn;
    }

    // Step 4:  no matches => return null.  The caller should be able to handle such cases,
    // anyway.
    return null;
  }

  public resetPrecalcFontSizes() {
    for(const layer of Object.values(this.layers)) {
      for(const row of layer.rows) {
        for(const key of row.keys) {
          key.resetFontPrecalc();
        }
      }
    }

    // This method is called whenever all related stylesheets are fully loaded and applied.
    // The actual padding data may not have been available until now.
    this._heightPadding = undefined;
  }

  public refreshLayout(layoutParams: LayerLayoutParams) {
    if(isNaN(layoutParams.keyboardWidth) || isNaN(layoutParams.keyboardHeight)) {
      // We're not in the DOM yet; we'll refresh properly once that changes.
      // Can be reached if the layerId is changed before the keyboard enters the DOM.
      return;
    }
    // Set layer-group copies of relevant computed-size values; they are used by nearest-key
    // detection.
    this.computedWidth = layoutParams.keyboardWidth;
    this.computedHeight = layoutParams.keyboardHeight;

    // Assumption:  this styling value will not change once the keyboard and
    // related stylesheets are loaded and applied.
    if(this._heightPadding === undefined) {
      // Should not trigger a new layout reflow; VisualKeyboard should have made no further DOM
      // style changes since the last one.

      // For touch-based OSK layouts, kmwosk.css may include top & bottom
      // padding on the layer-group element.
      const computedGroupStyle = getComputedStyle(this.element);

      // parseInt('') => NaN, which is falsy; we want to fallback to zero.
      let pt = parseInt(computedGroupStyle.paddingTop, 10) || 0;
      let pb = parseInt(computedGroupStyle.paddingBottom, 10) || 0;
      this._heightPadding = pt + pb;
    }

    if(this.activeLayer) {
      this.activeLayer.refreshLayout(layoutParams);
    }
  }

  public get verticalPadding() {
    return this._heightPadding ?? 0;
  }
}