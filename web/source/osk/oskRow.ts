/// <reference path="oskBaseKey.ts" />

namespace com.keyman.osk {
  /**
   * Models one row of one layer of the OSK (`VisualKeyboard`) for a keyboard.
   */
  export class OSKRow {
    public readonly element: HTMLDivElement;
    public readonly keys: OSKBaseKey[];

    public constructor(vkbd: VisualKeyboard, 
                       layerSpec: keyboards.ActiveLayer,
                       rowSpec: keyboards.ActiveRow,
                       objectWidth: number,
                       doCalibration: boolean,
                       displayUnderlying: boolean) {
      const rDiv = this.element = document.createElement('div');
      rDiv.className='kmw-key-row';
      // The following event trap is needed to prevent loss of focus in IE9 when clicking on a key gap.
      // Unclear why normal _CreateElement prevention of loss of focus does not seem to work here.
      // Appending handler to event handler chain does not work (other event handling remains active).
      rDiv.onmousedown = function(e: MouseEvent) {
        if(e) {
          e.preventDefault();
        }
      }

      const rs=rDiv.style;

      // Calculate default row height
      const rowHeight = 100/layerSpec.row.length;

      // Set row height. (Phone and tablet heights are later recalculated
      // and set in px, allowing for viewport scaling.)
      rs.maxHeight=rs.height=rowHeight+'%';

      // Apply defaults, setting the width and other undefined properties for each key
      const keys=rowSpec.key;

      if(doCalibration) {
        // Calculate actual key widths by multiplying by the OSK's width and rounding appropriately,
        // adjusting the width of the last key to make the total exactly 100%.
        // Overwrite the previously-computed percent.
        // NB: the 'percent' suffix is historical, units are percent on desktop devices, but pixels on touch devices
        // All key widths and paddings are rounded for uniformity
        for(let j=0; j<keys.length; j++) {
          const key = keys[j];
          // TODO:  reinstate rounding?
          key['widthpc'] = key.proportionalWidth * objectWidth;
          key['padpc']   = key.proportionalPad   * objectWidth;
        }
      }

      //Create the key square (an outer DIV) for each key element with padding, and an inner DIV for the button (btn)
      var totalPercent=0;
      this.keys = [];

      for(let j=0; j<keys.length; j++) {
        const key=keys[j];

        var keyGenerator = new OSKBaseKey(key as OSKKeySpec, layerSpec.id);
        var keyTuple = keyGenerator.construct(vkbd, displayUnderlying, rs, totalPercent);
        this.keys.push(keyGenerator);

        rDiv.appendChild(keyTuple.element);
        totalPercent += keyTuple.percent;
      }
    }

    public refreshLayout(vkbd: VisualKeyboard, rowHeight: number, bottom: number, pad: number) {
      const rs = this.element.style;
      if(vkbd.usesFixedHeightScaling) {
        if(!vkbd.isStatic) {
          rs.bottom=bottom+'px';
        }
        rs.maxHeight=rs.lineHeight=rs.height=rowHeight+'px';
      }

      for(const key of this.keys) {
        const keySquare  = key.btn.parentElement;
        const keyElement = key.btn;

        if(vkbd.usesFixedHeightScaling) {
          // Set the kmw-key-square position
          const kss = keySquare.style;
          const kes = keyElement.style;
          kss.height=kss.minHeight=(rowHeight)+'px';

          kes.top = (pad/2) + 'px';
          kes.height=kes.lineHeight=kes.minHeight=(rowHeight-pad)+'px';
        }

        if(keyElement.key) {
          keyElement.key.refreshLayout(vkbd);
        }
      }
    }
  }
}