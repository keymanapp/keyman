import { ActiveKey, ActiveLayer, ActiveRow } from 'keyman/engine/keyboard';

import OSKBaseKey from './oskBaseKey.js';
import { ParsedLengthStyle } from '../lengthStyle.js';
import VisualKeyboard from '../visualKeyboard.js';
import OSKKey, { KeyLayoutParams } from './oskKey.js';
import { LayerLayoutParams } from './oskLayer.js';

/*
  The total proportion of key-square height used as key-button padding.
  The 'padding' is visible to users as the vertical space between keys
  and exists both in "fixed" and "absolute" sizing modes.
*/
export const KEY_BTN_Y_PAD_RATIO = 0.15;

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

  // Avoid any references to getComputedStyle, offset_, or other layout-reflow
  // dependent values.  Refer to https://gist.github.com/paulirish/5d52fb081b3570c81e3a.
  public refreshLayout(layoutParams: LayerLayoutParams) {
    const rs = this.element.style;

    const rowHeight = layoutParams.heightStyle.scaledBy(this.heightFraction);
    rs.maxHeight=rs.lineHeight=rs.height=rowHeight.styleString;

    const keyHeightBase = layoutParams.heightStyle.absolute ? rowHeight : ParsedLengthStyle.forScalar(1);
    const padTop = keyHeightBase.scaledBy(KEY_BTN_Y_PAD_RATIO / 2);
    const keyHeight = keyHeightBase.scaledBy(1 - KEY_BTN_Y_PAD_RATIO);

    // Update all key-square layouts.
    this.keys.forEach((key) => {
      const keySquare  = key.square;
      const keyElement = key.btn;

      // Set the kmw-key-square position
      const kss = keySquare.style;
      kss.height=kss.minHeight=keyHeightBase.styleString;

      const kes = keyElement.style;
      kes.top = padTop.styleString;
      kes.height=kes.lineHeight=kes.minHeight=keyHeight.styleString;
    });
  }

  private buildKeyLayout(layoutParams: LayerLayoutParams, key: OSKKey) {
    // Calculate changes to be made...
    const keyWidth = layoutParams.widthStyle.scaledBy(key.spec.proportionalWidth);

    // We maintain key-btn padding within the key-square - the latter `scaledBy`
    // adjusts for that, providing the final key-btn height.
    const keyHeight = layoutParams.heightStyle.scaledBy(this.heightFraction).scaledBy(1 - KEY_BTN_Y_PAD_RATIO);

    const keyStyle: KeyLayoutParams = {
      keyWidth:  keyWidth.val  * (keyWidth.absolute ? 1 : layoutParams.keyboardWidth),
      keyHeight: keyHeight.val * (keyHeight.absolute ? 1 : layoutParams.keyboardHeight),
      baseEmFontSize: layoutParams.baseEmFontSize,
      layoutFontSize: layoutParams.layoutFontSize
    };

    return keyStyle;
  }

  /**
   * Any style-caching behavior needed for use in layout manipulation should be
   * computed within this method, not within refreshLayout.  This is to prevent
   * unnecessary layout-reflow.
   * @param layoutParams
   * @returns
   */
  public detectStyles(layoutParams: LayerLayoutParams) {
    this.keys.forEach((key) => {
      key.detectStyles(this.buildKeyLayout(layoutParams, key));
    });
  }

  // Avoid any references to getComputedStyle, offset_, or other layout-reflow
  // dependent values.  Refer to https://gist.github.com/paulirish/5d52fb081b3570c81e3a.
  public refreshKeyLayouts(layoutParams: LayerLayoutParams) {
    this.keys.forEach((key) => {
      // Calculate changes to be made...
      const keyElement = key.btn;

      const widthStyle = layoutParams.widthStyle;
      const heightStyle = layoutParams.heightStyle;

      const keyWidth = widthStyle.scaledBy(key.spec.proportionalWidth);
      const keyPad =   widthStyle.scaledBy(key.spec.proportionalPad);

      // We maintain key-btn padding within the key-square - the latter `scaledBy`
      // adjusts for that, providing the final key-btn height.
      const keyHeight = heightStyle.scaledBy(this.heightFraction).scaledBy(1 - KEY_BTN_Y_PAD_RATIO);

      // Match the row height (if fixed-height) or use full row height (if percent-based)
      const styleHeight = heightStyle.absolute ? keyHeight.styleString : '100%';

      const keyStyle: KeyLayoutParams = this.buildKeyLayout(layoutParams, key);
      keyElement.key?.refreshLayout(keyStyle);

      key.square.style.width = keyWidth.styleString;
      key.square.style.marginLeft = keyPad.styleString;

      key.btn.style.width = widthStyle.absolute ? keyWidth.styleString : '100%';
      key.square.style.height = styleHeight;
    });
  }
}