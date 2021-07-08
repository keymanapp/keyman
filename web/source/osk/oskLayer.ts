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
        this.globeKey  = this.getSpecialKey('K_LOPT');
        this.hideKey   = this.getSpecialKey('K_ROPT');
      }

      // Define for both desktop and touchable OSK
      this.spaceBarKey = this.getSpecialKey('K_SPACE');
    }

    /**
     *  Set the reference to a special function key for the
     *  currently visible OSK layer
     *
     *  @param    {string}  layerId layer identifier
     *  @param    {string}  keyId   key identifier
     *  @return   {Object}          Reference to key
     */
    private getSpecialKey(keyId: string): OSKBaseKey {
      for(let row of this.rows) {
        for(var k=0; k < row.keys.length; k++) {
          const key = row.keys[k];
          //let key = getKeyFrom(keys[k].firstChild);
          if(key && key.spec.id == keyId) {
            return key;
          }
        }
      }
    
      return null;
    }
  }
}