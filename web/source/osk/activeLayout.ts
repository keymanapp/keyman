namespace com.keyman.osk {
  class ActiveRow {
    // Identify key labels (e.g. *Shift*) that require the special OSK font
    static readonly SPECIAL_LABEL=/\*\w+\*/;

    // Defines key defaults
    static readonly DEFAULT_KEY = {
      text: '',
      width: '100',
      sp: '0',
      pad: '15'
    };
    
    constructor(row: LayoutRow, totalWidth: number) {
      // Apply defaults, setting the width and other undefined properties for each key
      let keys=row['key'];
      for(let j=0; j<keys.length; j++) {
        let key=keys[j];
        for(var tp in ActiveRow.DEFAULT_KEY) {
          if(typeof key[tp] != 'string') {
            key[tp]=ActiveRow.DEFAULT_KEY[tp];
          }
        }

        // Modify the key type for special keys with non-standard labels
        // to allow the keyboard font to ovveride the SpecialOSK font.
        // Blank keys are no longer reclassed - can use before/after CSS to add text
        switch(key['sp']) {
          case '1':
            if(!ActiveRow.SPECIAL_LABEL.test(key['text']) && key['text'] != '') {
              key['sp']='3';
            }
            break;
          case '2':
            if(!ActiveRow.SPECIAL_LABEL.test(key['text']) && key['text'] != '') {
              key['sp']='4';
            }
            break;
        }
      }

      // Calculate actual key widths by summing defined widths and scaling each key to %,
      // adjusting the width of the last key to make the total exactly 100%
      // Save each percentage key width as a separate member (do *not* overwrite layout specified width!)
      // NB: the 'percent' suffix is historical, units are percent on desktop devices, but pixels on touch devices
      // All key widths and paddings are rounded for uniformity
      var keyPercent: number, padPercent: number, totalPercent=0;
      for(let j=0; j<keys.length-1; j++) {
        keyPercent=Math.round(parseInt(keys[j]['width'],10)*objectWidth/totalWidth);
        keys[j]['widthpc']=keyPercent;
        padPercent=Math.round(parseInt(keys[j]['pad'],10)*objectWidth/totalWidth);
        keys[j]['padpc']=padPercent;
        totalPercent += padPercent+keyPercent;
      }

      // Allow for right OSK margin (15 layout units)
      totalPercent += Math.round(15*objectWidth/totalWidth);

      // If a single key, and padding is negative, add padding to right align the key
      if(keys.length == 1 && parseInt(keys[0]['pad'],10) < 0) {
        keyPercent=Math.round(parseInt(keys[0]['width'],10)*objectWidth/totalWidth);
        keys[0]['widthpc']=keyPercent;
        totalPercent += keyPercent;
        keys[0]['padpc']=(objectWidth-totalPercent);
      } else if(keys.length > 0) {
        let j=keys.length-1;
        padPercent=Math.round(parseInt(keys[j]['pad'],10)*objectWidth/totalWidth);
        keys[j]['padpc']=padPercent;
        totalPercent += padPercent;
        keys[j]['widthpc']=(objectWidth-totalPercent);
      }
    }
  }

  class ActiveLayer {
    rows: ActiveRow[] = [];
    spec: LayoutLayer;

    constructor(layer: LayoutLayer, formFactor: string) {
      this.spec = layer;

      layer.aligned=false;

      // Create a DIV for each row of the group
      let rows=layer['row'];

      // Calculate the maximum row width (in layout units)
      var totalWidth=0;
      for(let i=0; i<layer['row'].length; i++) {
        var width=0;
        let row=rows[i];
        let keys=row['key'];
        for(let j=0; j<keys.length; j++) {
          let key=keys[j];

          // Test for a trailing comma included in spec, added as null object by IE
          if(key == null) {
            keys.length = keys.length-1;
          } else {
            var kw, kp;
            kw = (typeof key['width'] == 'string' && key['width'] != '') ? parseInt(key['width'],10) : 100;
            if(isNaN(kw) || kw == 0) kw = 100;
            key['width'] = kw.toString();
            kp = (typeof key['pad'] == 'string' && key['pad'] != '') ? parseInt(key['pad'],10) : 15;
            if(isNaN(kp) || kp == 0) kp = 15;  // KMEW-119
            key['pad'] = kp.toString();
            width += kw + kp;
            //if(typeof key['width'] == 'string' && key['width'] != '') width += parseInt(key['width'],10); else width += 100;
            //if(typeof key['pad'] == 'string' && key['pad'] != '') width += parseInt(key['pad'],10); else width += 5;
          }
        }
        if(width > totalWidth) {
          totalWidth = width;
        }
      }

      // Add default right margin
      if(formFactor == 'desktop') {
        totalWidth += 5;  // KMEW-117
      } else {
        // TODO: Not entirely clear why this needs to be 15 instead of 5 on touch layouts.  We probably have
        // a miscalculation somewhere
        totalWidth += 15;
      }

      for(let i=0; i<rows.length; i++) {
        this.rows.push(new ActiveRow(rows[i], totalWidth));
      }
    }
  }

  export class ActiveLayout {
    layers: ActiveLayer[] = [];
    spec: LayoutFormFactor;

    /**
     * Create the OSK for a particular keyboard and device
     *
     * @param       {Object}              layout      OSK layout definition
     * @param       {string}              formFactor  layout form factor
     * @return      {Object}                          fully formatted OSK object
     */
    constructor(layout: LayoutFormFactor, formFactor: string) {
      this.spec = layout;

      if(layout == null) {
        throw new Error("Cannot build an ActiveLayout for a null specification.");
      }

      // Create a separate OSK div for each OSK layer, only one of which will ever be visible
      var n: number, i: number, j: number;
      var layers: LayoutLayer[], layer: LayoutLayer;
      var rows: LayoutRow[], row: LayoutRow;

      layers=layout['layer'];

      // ***Delete any empty rows at the end added by compiler bug...
      for(n=0; n<layers.length; n++) {
        layer=layers[n]; rows=layer['row'];
        for(i=rows.length; i>0; i--) {
          if(rows[i-1]['key'].length > 0) {
            break;
          }
        }

        if(i < rows.length) {
          rows.splice(i-rows.length,rows.length-i);
        }
      }
      // ...remove to here when compiler bug fixed ***

      // // Get the actual available document width and scale factor according to device type
      // var objectWidth : number;
      // if(formFactor == 'desktop') {
      //   objectWidth = 100; // As in, 100%.
      // } else {
      //   objectWidth = oskManager.getWidth();  // ... as in px.  Makes sense; we don't want random sub-pixel issues.
      // }

      for(n=0; n<layers.length; n++) {
        this.layers.push(new ActiveLayer(layers[n], formFactor));
      }
    }
  }
}