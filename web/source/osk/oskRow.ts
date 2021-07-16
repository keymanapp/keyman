/// <reference path="oskBaseKey.ts" />

namespace com.keyman.osk {
  /**
   * Models one row of one layer of the OSK (`VisualKeyboard`) for a keyboard.
   */
  export class OSKRow {
    public readonly element: HTMLDivElement;
    public readonly keys: OSKBaseKey[];
    private readonly heightFraction: number;

    public constructor(vkbd: VisualKeyboard, 
                       layerSpec: keyboards.ActiveLayer,
                       rowSpec: keyboards.ActiveRow) {
      const rDiv = this.element = document.createElement('div');
      rDiv.className='kmw-key-row';

      // Calculate default row height
      this.heightFraction = 1 / layerSpec.row.length;

      // Apply defaults, setting the width and other undefined properties for each key
      const keys=rowSpec.key;
      this.keys = [];

      // Calculate actual key widths by multiplying by the OSK's width and rounding appropriately,
      // adjusting the width of the last key to make the total exactly 100%.
      // Overwrite the previously-computed percent.
      // NB: the 'percent' suffix is historical, units are percent on desktop devices, but pixels on touch devices
      // All key widths and paddings are rounded for uniformity
      for(let j=0; j<keys.length; j++) {
        const key = keys[j];
        var keyObj = new OSKBaseKey(key as OSKKeySpec, layerSpec.id, this.heightFraction);
        
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

      const height = vkbd.layoutHeight.scaledBy(this.heightFraction);
      if(vkbd.usesFixedHeightScaling) {
        rs.maxHeight=rs.lineHeight=rs.height=height.styleString;
      }

      // Only used for fixed-height scales at present.
      const rowPad = Math.round(0.15 * vkbd.height * this.heightFraction);

      for(const key of this.keys) {
        const keySquare  = key.btn.parentElement;
        const keyElement = key.btn;

        if(vkbd.usesFixedHeightScaling) {
          // Set the kmw-key-square position
          const kss = keySquare.style;
          const kes = keyElement.style;
          kss.height=kss.minHeight=height.styleString;

          kes.top = (rowPad/2) + 'px';
          kes.height=kes.lineHeight=kes.minHeight=(height.val-rowPad)+'px';
        }

        if(keyElement.key) {
          keyElement.key.refreshLayout(vkbd);
        }
      }
    }
  }
}