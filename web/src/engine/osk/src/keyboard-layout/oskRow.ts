import { ActiveKey, ActiveLayer, ActiveRow } from '@keymanapp/keyboard-processor';

import OSKBaseKey from './oskBaseKey.js';
import { ParsedLengthStyle } from '../lengthStyle.js';
import VisualKeyboard from '../visualKeyboard.js';

/**
 * Models one row of one layer of the OSK (`VisualKeyboard`) for a keyboard.
 */
export default class OSKRow {
  public readonly element: HTMLDivElement;
  public readonly keys: OSKBaseKey[];
  public readonly heightFraction: number;
  public readonly spec: ActiveRow;

  public constructor(vkbd: VisualKeyboard,
                      layerSpec: ActiveLayer,
                      rowSpec: ActiveRow) {
    const rDiv = this.element = document.createElement('div');
    rDiv.className='kmw-key-row';

    // Calculate default row height
    this.heightFraction = 1 / layerSpec.row.length;

    // Apply defaults, setting the width and other undefined properties for each key
    const keys=rowSpec.key;
    this.spec = rowSpec;
    this.keys = [];

    // Calculate actual key widths by multiplying by the OSK's width and rounding appropriately,
    // adjusting the width of the last key to make the total exactly 100%.
    // Overwrite the previously-computed percent.
    // NB: the 'percent' suffix is historical, units are percent on desktop devices, but pixels on touch devices
    // All key widths and paddings are rounded for uniformity
    for(let j=0; j<keys.length; j++) {
      const key = keys[j];
      var keyObj = new OSKBaseKey(key as ActiveKey, layerSpec.id, this);

      var element = keyObj.construct(vkbd);
      this.keys.push(keyObj);

      rDiv.appendChild(element);
    }
  }

  public get displaysKeyCaps(): boolean {
    if(this.keys.length > 0) {
      return this.keys[0].displaysKeyCap;
    } else {
      return undefined;
    }
  }

  public set displaysKeyCaps(flag: boolean) {
    for(const key of this.keys) {
      key.displaysKeyCap = flag;
    }
  }

  public refreshLayout(vkbd: VisualKeyboard) {
    const rs = this.element.style;

    const rowHeight = vkbd.internalHeight.scaledBy(this.heightFraction);
    rs.maxHeight=rs.lineHeight=rs.height=rowHeight.styleString;

    // Only used for fixed-height scales at present.
    const padRatio = 0.15;

    const keyHeightBase = vkbd.usesFixedHeightScaling ? rowHeight : ParsedLengthStyle.forScalar(1);
    const padTop = keyHeightBase.scaledBy(padRatio / 2);
    const keyHeight = keyHeightBase.scaledBy(1 - padRatio);

    for(const key of this.keys) {
      const keySquare  = key.btn.parentElement;
      const keyElement = key.btn;

      // Set the kmw-key-square position
      const kss = keySquare.style;
      kss.height=kss.minHeight=keyHeightBase.styleString;

      const kes = keyElement.style;
      kes.top = padTop.styleString;
      kes.height=kes.lineHeight=kes.minHeight=keyHeight.styleString;

      if(keyElement.key) {
        keyElement.key.refreshLayout(vkbd);
      }
    }
  }
}