/// <reference path="oskRow.ts" />

namespace com.keyman.osk {
  export class OSKLayer {
    public readonly element: HTMLDivElement;
    public readonly rows: OSKRow[];
    public readonly spec: keyboards.ActiveLayer;
    public readonly nextlayer: string;

    public readonly globeKey:    OSKBaseKey;
    public readonly spaceBarKey: OSKBaseKey;
    public readonly hideKey:     OSKBaseKey;
    public readonly capsKey:     OSKBaseKey;
    public readonly numKey:      OSKBaseKey;
    public readonly scrollKey:   OSKBaseKey;

    public get id(): string {
      return this.spec.id;
    }

    public constructor(vkbd: VisualKeyboard,
                       layout: keyboards.ActiveLayout,
                       layer: keyboards.ActiveLayer,
                       objectWidth: number,
                       doCalibration: boolean) {
      this.spec = layer;
      
      const gDiv = this.element = document.createElement('div');
      const gs=gDiv.style;
      gDiv.className='kmw-key-layer';

      var nRows=layer['row'].length;
      if(nRows > 4 && vkbd.device.formFactor == 'phone') {
        gDiv.className = gDiv.className + ' kmw-5rows';
      }

      // Set font for layer if defined in layout
      if('font' in layout) {
        gs.fontFamily=layout['font'];
      } else {
        gs.fontFamily='';
      }

      this.nextlayer = gDiv['layer'] = layer['id'];
      if(typeof layer['nextlayer'] == 'string') {
        this.nextlayer = layer['nextlayer'];
      }

      // Create a DIV for each row of the group
      let rows=layer['row'];
      this.rows = [];

      for(let i=0; i<rows.length; i++) {
        let rowObj = new OSKRow(vkbd, layer, rows[i], objectWidth, doCalibration, layout["displayUnderlying"]);
        gDiv.appendChild(rowObj.element);
        this.rows.push(rowObj);
      }

      // Identify and save references to the language key, hide keyboard key, and space bar
      if(vkbd.device.touchable) {
        this.globeKey  = this.findKey('K_LOPT');
        this.hideKey   = this.findKey('K_ROPT');
      }

      // Define for both desktop and touchable OSK
      this.spaceBarKey = this.findKey('K_SPACE');
      this.capsKey     = this.findKey('K_CAPS');
      this.numKey      = this.findKey('K_NUMLOCK');
      this.scrollKey   = this.findKey('K_SCROLL');
    }

    /**
     *  Find the OSKBaseKey representing the specified
     *  key ID for the currently visible OSK layer
     *
     *  @param    {string}  keyId   key identifier
     *  @return   {Object}          Reference to key
     */
    public findKey(coreID: string): OSKBaseKey {
      for(const row of this.rows) {
        for(const key of row.keys) {
          if(key.getCoreId() == coreID) {
            return key;
          }
        }
      }
      return null;
    }

    public refreshLayout(vkbd: VisualKeyboard, paddedHeight: number, trueHeight: number) {
      // Check the heights of each row, in case different layers have different row counts.
      let nRows = this.rows.length;
      let rowHeight = Math.floor(trueHeight/(nRows == 0 ? 1 : nRows));

      if(vkbd.usesFixedHeightScaling) {
        this.element.style.height=(paddedHeight)+'px';


        if(vkbd.device.OS == 'Android' && 'devicePixelRatio' in window) {
          this.element.style.height = this.element.style.maxHeight = paddedHeight + 'px';
          rowHeight /= window.devicePixelRatio;
        }
      }

      // Sets the layers to the correct height
      let rowPad = Math.round(0.15*rowHeight);

      for(let nRow=0; nRow<nRows; nRow++) {
        let bottom = (nRows-nRow-1)*rowHeight+1;

        if(vkbd.usesFixedHeightScaling) {
          // Calculate the exact vertical coordinate of the row's center.
          this.spec.row[nRow].proportionalY = ((paddedHeight - bottom) - rowHeight/2) / paddedHeight;
        }

        const oskRow = this.rows[nRow];
        oskRow.refreshLayout(vkbd, rowHeight, bottom, rowPad);
      }
    }
  }
}