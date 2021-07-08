/// <reference path="oskRow.ts" />

namespace com.keyman.osk {
  export class OSKLayer {
    public readonly element: HTMLDivElement;
    public readonly rows: OSKRow[];

    public constructor(vkbd: VisualKeyboard,
                       layout: keyboards.ActiveLayout,
                       layer: keyboards.ActiveLayer,
                       objectWidth: number,
                       doCalibration: boolean) {
      const gDiv = this.element = document.createElement('div');
      const gs=gDiv.style;
      gDiv.className='kmw-key-layer';

      // Set font for layer if defined in layout
      if('font' in layout) {
        gs.fontFamily=layout['font'];
      } else {
        gs.fontFamily='';
      }

      gDiv['layer'] = gDiv['nextLayer'] = layer['id'];
      if(typeof layer['nextlayer'] == 'string') {
        gDiv['nextLayer']=layer['nextlayer'];
      }

      // Create a DIV for each row of the group
      let rows=layer['row'];
      this.rows = [];

      for(let i=0; i<rows.length; i++) {
        let rowObj = new OSKRow(vkbd, layer, rows[i], objectWidth, doCalibration, layout["displayUnderlying"]);
        gDiv.appendChild(rowObj.element);
        this.rows.push(rowObj);
      }
    }
  }
}