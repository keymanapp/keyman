namespace com.keyman.keyboards {
  type KeyDistribution = text.KeyDistribution;


  export class ActiveKey implements LayoutKey {

    static readonly DEFAULT_PAD=15;          // Padding to left of key, in virtual units
    static readonly DEFAULT_RIGHT_MARGIN=15; // Padding to right of right-most key, in virtual units
    static readonly DEFAULT_KEY_WIDTH=100;   // Width of a key, if not specified, in virtual units

    // Defines key defaults
    static readonly DEFAULT_KEY = {
      text: '',
      width: ActiveKey.DEFAULT_KEY_WIDTH.toString(),
      sp: '0',
      pad: ActiveKey.DEFAULT_PAD.toString()
    };

    id?: string;
    width?: string;
    pad?: string;
    layer: string;
    displayLayer: string;
    nextlayer: string;

    private baseKeyEvent: text.KeyEvent;
    isMnemonic: boolean = false;

    proportionalX: number;
    proportionalWidth: number;

    static polyfill(key: LayoutKey, layout: ActiveLayout, displayLayer: string) {
      // Add class functions to the existing layout object, allowing it to act as an ActiveLayout.
      let dummy = new ActiveKey();
      for(let prop in dummy) {
        if(!key.hasOwnProperty(prop)) {
          key[prop] = dummy[prop];
        }
      }

      // Ensure subkeys are also properly extended.
      if(key.sk) {
        for(let subkey of key.sk) {
          ActiveKey.polyfill(subkey, layout, displayLayer);
        }
      }

      let aKey = key as ActiveKey;
      aKey.displayLayer = displayLayer;
      aKey.layer = aKey.layer || displayLayer;

      // Compute the key's base KeyEvent properties for use in future event generation
      aKey.constructBaseKeyEvent(layout, displayLayer);
    }

    private constructBaseKeyEvent(layout: ActiveLayout, displayLayer: string) {
      // Get key name and keyboard shift state (needed only for default layouts and physical keyboard handling)
      // Note - virtual keys should be treated case-insensitive, so we force uppercasing here.
      let layer = this.layer || displayLayer || '';
      let keyName= this.id ? this.id.toUpperCase() : null;

      // Start:  mirrors _GetKeyEventProperties

      // Override key shift state if specified for key in layout (corrected for popup keys KMEW-93)
      let keyShiftState = text.KeyboardProcessor.getModifierState(layer);

      // First check the virtual key, and process shift, control, alt or function keys
      var Lkc: text.KeyEvent = {
        Ltarg: null, // set later, in constructKeyEvent.
        Lmodifiers: keyShiftState,
        Lstates: 0,
        Lcode: keyName ? text.Codes.keyCodes[keyName] : 0,
        LisVirtualKey: true,
        vkCode: 0,
        kName: keyName,
        kLayer: layer,
        kbdLayer: displayLayer,
        kNextLayer: this.nextlayer,
        device: null,
        isSynthetic: true
      };

      if(layout.keyboard) {
        let keyboard = layout.keyboard;

        // Include *limited* support for mnemonic keyboards (Sept 2012)
        // If a touch layout has been defined for a mnemonic keyout, do not perform mnemonic mapping for rules on touch devices.
        if(keyboard.isMnemonic && !(layout.isDefault && layout.formFactor != 'desktop')) {
          if(Lkc.Lcode != text.Codes.keyCodes['K_SPACE']) { // exception required, March 2013
            // Jan 2019 - interesting that 'K_SPACE' also affects the caps-state check...
            Lkc.vkCode = Lkc.Lcode;
            this.isMnemonic = true;
          }
        } else {
          Lkc.vkCode=Lkc.Lcode;
        }

        // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
        if(!keyboard.definesPositionalOrMnemonic) {
          // Not the best pattern, but currently safe - we don't look up any properties of any of the
          // arguments in this use case, and the object's scope is extremely limited.
          Lkc.Lcode = KeyMapping._USKeyCodeToCharCode(this.constructKeyEvent(null, null, null));
          Lkc.LisVirtualKey=false;
        }
      }

      this.baseKeyEvent = Lkc;
    }

    constructKeyEvent(keyboardProcessor: text.KeyboardProcessor, target: text.OutputTarget, device: text.EngineDeviceSpec): text.KeyEvent {
      // Make a deep copy of our preconstructed key event, filling it out from there.
      let Lkc = utils.deepCopy(this.baseKeyEvent);
      Lkc.Ltarg = target;
      Lkc.device = device;

      if(this.isMnemonic) {
        text.KeyboardProcessor.setMnemonicCode(Lkc, this.layer.indexOf('shift') != -1, keyboardProcessor ? keyboardProcessor.stateKeys['K_CAPS'] : false);
      }

      // Performs common pre-analysis for both 'native' and 'embedded' OSK key & subkey input events.
      // This part depends on the keyboard processor's active state.
      if(keyboardProcessor) {
        keyboardProcessor.setSyntheticEventDefaults(Lkc);
      }

      return Lkc;
    }
  }

  class ActiveRow implements LayoutRow {
    // Identify key labels (e.g. *Shift*) that require the special OSK font
    static readonly SPECIAL_LABEL=/\*\w+\*/;

    id: string;
    key: ActiveKey[];

    /**
     * Used for calculating fat-fingering offsets.
     */
    proportionalY: number;

    private constructor() {

    }

    static polyfill(row: LayoutRow, layout: ActiveLayout, displayLayer: string, totalWidth: number, proportionalY: number) {
      // Apply defaults, setting the width and other undefined properties for each key
      let keys=row['key'];
      for(let j=0; j<keys.length; j++) {
        let key=keys[j];
        for(var tp in ActiveKey.DEFAULT_KEY) {
          if(typeof key[tp] != 'string') {
            key[tp]=ActiveKey.DEFAULT_KEY[tp];
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

        ActiveKey.polyfill(key, layout, displayLayer);
      }

      /* The calculations here are effectively 'virtualized'.  When used with the OSK, the VisualKeyboard
       * will overwrite these values with their true runtime geometry.
       *
       * These calculations approximate those of the actual OSK (without fitting to a specific resolution)
       * and are intended for use with layout testing (while headless) in the future.
       */

      // Calculate percentage-based scalings by summing defined widths and scaling each key to %.
      // Save each percentage key width as a separate member (do *not* overwrite layout specified width!)
      var keyPercent: number, padPercent: number, totalPercent=0;
      for(let j=0; j<keys.length-1; j++) {
        keyPercent=parseInt(keys[j]['width'],10)/totalWidth;
        keys[j]['widthpc']=keyPercent;
        padPercent=parseInt(keys[j]['pad'],10)/totalWidth;
        keys[j]['padpc']=padPercent;

        // compute center's default x-coord (used in headless modes)
        (<ActiveKey> keys[j]).proportionalX = (totalPercent + padPercent + (keyPercent/2));
        (<ActiveKey> keys[j]).proportionalWidth = keyPercent;

        totalPercent += padPercent+keyPercent;
      }

      // Allow for right OSK margin (15 layout units)
      let rightMargin = ActiveKey.DEFAULT_RIGHT_MARGIN/totalWidth;
      totalPercent += rightMargin;

      // If a single key, and padding is negative, add padding to right align the key
      if(keys.length == 1 && parseInt(keys[0]['pad'],10) < 0) {
        keyPercent=parseInt(keys[0]['width'],10)/totalWidth;
        keys[0]['widthpc']=keyPercent;
        totalPercent += keyPercent;
        keys[0]['padpc']=1-totalPercent;

        // compute center's default x-coord (used in headless modes)
        (<ActiveKey> keys[0]).proportionalX = ((totalPercent - rightMargin) -  keyPercent/2);
        (<ActiveKey> keys[0]).proportionalWidth = keyPercent;

      } else if(keys.length > 0) {
        let j=keys.length-1;
        padPercent=parseInt(keys[j]['pad'],10)/totalWidth;
        keys[j]['padpc']=padPercent;
        totalPercent += padPercent;
        keys[j]['widthpc'] = keyPercent = 1-totalPercent;

        // compute center's default x-coord (used in headless modes)
        (<ActiveKey> keys[j]).proportionalX = (1 - rightMargin) - keyPercent/2;
        (<ActiveKey> keys[j]).proportionalWidth = keyPercent;
      }

      // Add class functions to the existing layout object, allowing it to act as an ActiveLayout.
      let dummy = new ActiveRow();
      for(let key in dummy) {
        if(!row.hasOwnProperty(key)) {
          row[key] = dummy[key];
        }
      }

      let aRow = row as ActiveRow;
      aRow.proportionalY = proportionalY;
    }

    populateKeyMap(map: {[keyId: string]: ActiveKey}) {
      this.key.forEach(function(key: ActiveKey) {
        if(key.id) {
          map[key.id] = key;
        }
      });
    }
  }

  export class ActiveLayer implements LayoutLayer {
    row: ActiveRow[];
    id: string;

    totalWidth: number;

    defaultKeyProportionalWidth: number;
    rowProportionalHeight: number;

    /**
     * Facilitates mapping key id strings to their specification objects.
     */
    keyMap: {[keyId: string]: ActiveKey};

    constructor() {

    }

    static polyfill(layer: LayoutLayer, layout: ActiveLayout) {
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
            kw = (typeof key['width'] == 'string' && key['width'] != '') ? parseInt(key['width'],10) : ActiveKey.DEFAULT_KEY_WIDTH;
            if(isNaN(kw) || kw == 0) kw = ActiveKey.DEFAULT_KEY_WIDTH;
            key['width'] = kw.toString();
            kp = (typeof key['pad'] == 'string' && key['pad'] != '') ? parseInt(key['pad'],10) : ActiveKey.DEFAULT_PAD;
            if(isNaN(kp) || kp == 0) kp = ActiveKey.DEFAULT_PAD;  // KMEW-119
            key['pad'] = kp.toString();
            width += kw + kp;
            //if(typeof key['width'] == 'string' && key['width'] != '') width += parseInt(key['width'],10); else width += DEFAULT_KEY_WIDTH;
            //if(typeof key['pad'] == 'string' && key['pad'] != '') width += parseInt(key['pad'],10); else width += 5;
          }
        }
        if(width > totalWidth) {
          totalWidth = width;
        }
      }

      // Add default right margin
      if(layout.formFactor == 'desktop') {
        totalWidth += 5; // TODO: resolve difference between touch and desktop; why don't we use ActiveKey.DEFAULT_RIGHT_MARGIN?
      } else {
        totalWidth += ActiveKey.DEFAULT_RIGHT_MARGIN;
      }

      let rowCount = layer.row.length;
      for(let i=0; i<rowCount; i++) {
        // Calculate proportional y-coord of row.  0 is at top with highest y-coord.
        let rowProportionalY = (i + 0.5) / rowCount;
        ActiveRow.polyfill(layer.row[i], layout, layer.id, totalWidth, rowProportionalY);
      }

      // Add class functions and properties to the existing layout object, allowing it to act as an ActiveLayout.
      let dummy = new ActiveLayer();
      for(let key in dummy) {
        if(!layer.hasOwnProperty(key)) {
          layer[key] = dummy[key];
        }
      }

      let aLayer = layer as ActiveLayer;
      aLayer.totalWidth = totalWidth;
      aLayer.defaultKeyProportionalWidth = parseInt(ActiveKey.DEFAULT_KEY.width, 10) / totalWidth;
      aLayer.rowProportionalHeight = 1.0 / rowCount;
      aLayer.keyMap = aLayer.constructKeyMap();
    }

    private constructKeyMap(): {[keyId: string]: ActiveKey} {
      let map: {[keyId: string]: ActiveKey} = {};
      this.row.forEach(function(row: ActiveRow) {
        row.populateKeyMap(map);
      });

      return map;
    }

    /**
     * Builds a sorted-order array of most likely keys to be intended for a given touch.
     * @param touchCoords A proportional (x, y) coordinate of the touch within the keyboard's geometry.
     *                           Should be within [0, 0] to [1, 1].
     * @param kbdScaleRatio The ratio of the keyboard's horizontal scale to its vertical scale.
     *                           For a 400 x 200 keyboard, should be 2.
     */
    getTouchProbabilities(touchCoords: {x: number, y: number}, kbdScaleRatio: number): KeyDistribution {
      let distribution = this.simpleTouchDistribution(touchCoords, kbdScaleRatio);
      let list: {keyId: string, p: number}[] = [];

      for(let key in distribution) {
        list.push({keyId: key, p: distribution[key]});
      }

      return list.sort(function(a, b) {
        return b.p - a.p; // Largest probability keys should be listed first.
      })
    }

    /**
     * Computes a probability distribution regarding the likelihood of a touch command being intended
     * for each of the layout's keys.
     * @param touchCoords A proportional (x, y) coordinate of the touch within the keyboard's geometry.
     *                           Should be within [0, 0] to [1, 1].
     * @param kbdScaleRatio The ratio of the keyboard's horizontal scale to its vertical scale.
     *                           For a 400 x 200 keyboard, should be 2.
     */
    simpleTouchDistribution(touchCoords: {x: number, y: number}, kbdScaleRatio: number): {[keyId: string]: number} {
      let keyDists = this.keyTouchDistances(touchCoords, kbdScaleRatio);
      let keyProbs: {[keyId: string]: number} = {};

      let totalMass = 0;

      // Should we wish to allow multiple different transforms for distance -> probability, use a function parameter in place
      // of the formula in the loop below.
      for(let key in keyDists) {
        totalMass += keyProbs[key] = 1 / (keyDists[key] + 1e-6); // Prevent div-by-0 errors.
      }

      for(let key in keyProbs) {
        keyProbs[key] /= totalMass;
      }

      return keyProbs;
    }

    /**
     * Computes a squared 'pseudo-distance' for the touch from each key.  (Not a proper metric.)
     * Intended for use in generating a probability distribution over the keys based on the touch input.
     * @param touchCoords A proportional (x, y) coordinate of the touch within the keyboard's geometry.
     *                           Should be within [0, 0] to [1, 1].
     * @param kbdScaleRatio The ratio of the keyboard's horizontal scale to its vertical scale.
     *                           For a 400 x 200 keyboard, should be 2.
     */
    private keyTouchDistances(touchCoords: {x: number, y: number}, kbdScaleRatio: number): {[keyId: string]: number} {
      let layer = this;

      let keyDists: {[keyId: string]: number} = {};

      // This double-nested loop computes a pseudo-distance for the touch from each key.  Quite useful for
      // generating a probability distribution.
      this.row.forEach(function(row: ActiveRow): void {
        row.key.forEach(function(key: ActiveKey): void {
          // If the key lacks an ID, just skip it.  Sometimes used for padding.
          if(!key.id) {
            return;
          }
          // These represent the within-key distance of the touch from the key's center.
          // Both should be on the interval [0, 0.5].
          let dx = Math.abs(touchCoords.x - key.proportionalX);
          let dy = Math.abs(touchCoords.y - row.proportionalY);

          // If the touch isn't within the key, these store the out-of-key distance
          // from the closest point on the key being checked.
          let distX: number, distY: number;

          if(dx > 0.5 * key.proportionalWidth) {
            distX = (dx - 0.5 * key.proportionalWidth);
            dx = 0.5;
          } else {
            distX = 0;
            dx /= key.proportionalWidth;
          }

          if(dy > 0.5 * layer.rowProportionalHeight) {
            distY = (dy - 0.5 * layer.rowProportionalHeight);
            dy = 0.5;
          } else {
            distY = 0;
            dy /= layer.rowProportionalHeight;
          }

          // Now that the differentials are computed, it's time to do distance scaling.
          //
          // For out-of-key distance, we scale the X component by the keyboard's aspect ratio
          // to get the actual out-of-key distance rather than proportional.
          distX *= kbdScaleRatio;

          // While the keys are rarely perfect squares, we map all within-key distance
          // to a square shape.  (ALT/CMD should seem as close to SPACE as a 'B'.)
          //
          // For that square, we take the rowHeight as its edge lengths.
          distX += dx * layer.rowProportionalHeight;
          distY += dy * layer.rowProportionalHeight;

          let distance = distX * distX + distY * distY;
          keyDists[key.id] = distance;
        });
      });

      return keyDists;
    }

    getKey(keyId: string) {
      // Keys usually are specified in a "long form" prefixed with their layer's ID.
      if(keyId.indexOf(this.id + '-') == 0) {
        keyId = keyId.replace(this.id + '-', '');
      }

      return this.keyMap[keyId];
    }
  }

  export class ActiveLayout implements LayoutFormFactor{
    layer: ActiveLayer[];
    font: string;
    keyLabels: boolean;
    isDefault?: boolean;
    keyboard: Keyboard;
    formFactor: text.FormFactor;

    /**
     * Facilitates mapping layer id strings to their specification objects.
     */
    layerMap: {[layerId: string]: ActiveLayer};

    private constructor() {

    }

    getLayer(layerId: string): ActiveLayer {
      return this.layerMap[layerId];
    }

    /**
     *
     * @param layout
     * @param formFactor
     */
    static polyfill(layout: LayoutFormFactor, keyboard: Keyboard, formFactor: text.FormFactor): ActiveLayout {
      if(layout == null) {
        throw new Error("Cannot build an ActiveLayout for a null specification.");
      }

      // Create a separate OSK div for each OSK layer, only one of which will ever be visible
      var n: number, i: number;
      var layers: LayoutLayer[], layer: LayoutLayer;
      let layerMap: {[layerId: string]: ActiveLayer} = {};
      var rows: LayoutRow[];

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

      // Add class functions to the existing layout object, allowing it to act as an ActiveLayout.
      let dummy = new ActiveLayout();
      for(let key in dummy) {
        if(!layout.hasOwnProperty(key)) {
          layout[key] = dummy[key];
        }
      }

      let aLayout = layout as ActiveLayout;
      aLayout.keyboard = keyboard;
      aLayout.formFactor = formFactor;

      for(n=0; n<layers.length; n++) {
        ActiveLayer.polyfill(layers[n], aLayout);
        layerMap[layers[n].id] = layers[n] as ActiveLayer;
      }

      aLayout.layerMap = layerMap;

      return aLayout;
    }
  }
}