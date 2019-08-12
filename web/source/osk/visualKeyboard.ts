/// <reference path="activeLayout.ts" />

namespace com.keyman.osk {
  let Codes = com.keyman.text.Codes;
  //#region Definition of the KeyElement merger type
  class KeyData {
    ['key']: OSKKey;
    ['keyId']: string;
    ['subKeys']?: OSKKeySpec[];
    
    constructor(keyData: OSKKey, keyId: string) {
      this['key'] = keyData;
      this['keyId'] = keyId;
    }
  }

  export type KeyElement = HTMLDivElement & KeyData;

  // Many thanks to https://www.typescriptlang.org/docs/handbook/advanced-types.html for this.
  function link(elem: HTMLDivElement, data: KeyData): KeyElement {
    let e = <KeyElement> elem;
    
    // Merges all properties and methods of KeyData onto the underlying HTMLDivElement, creating a merged class.
    for(let id in data) {
      if(!e.hasOwnProperty(id)) {
        (<any>e)[id] = (<any>data)[id];
      }
    }

    return e;
  }

  export function isKey(elem: Node): boolean {
    return elem && ('key' in elem) && ((<any> elem['key']) instanceof OSKKey);
  }

  export function getKeyFrom(elem: Node): KeyElement {
    if(isKey(elem)) {
      return <KeyElement> elem;
    } else {
      return null;
    }
  }
  //#endregion

  //#region OSK key objects and construction
  export class OSKKeySpec implements LayoutKey {
    id: string;
    text?: string;
    sp?: number | ButtonClass;
    width: string;
    layer?: string; // The key will derive its base modifiers from this property - may not equal the layer on which it is displayed.
    nextlayer?: string;
    pad?: string;
    widthpc?: number; // Added during OSK construction.
    padpc?: number; // Added during OSK construction.
    sk?: OSKKeySpec[];

    constructor(id: string, text?: string, width?: string, sp?: number | ButtonClass, nextlayer?: string, pad?: string) {
      this.id = id;
      this.text = text;
      this.width = width ? width : "50";
      this.sp = sp;
      this.nextlayer = nextlayer;
      this.pad = pad;
    }
  }

  export abstract class OSKKey {
    spec: OSKKeySpec;

    /** 
     * The layer of the OSK on which the key is displayed. 
     */
    readonly layer: string;

    constructor(spec: OSKKeySpec, layer: string) {
      this.spec = spec;
      this.layer = layer;
    }

    abstract getId(osk: VisualKeyboard): string;

    /**
     * Uses canvas.measureText to compute and return the width of the given text of given font in pixels.
     * 
     * @param {String} text The text to be rendered.
     * @param {String} style The CSSStyleDeclaration for an element to measure against, without modification.
     * 
     * @see https://stackoverflow.com/questions/118241/calculate-text-width-with-javascript/21015393#21015393
     * This version has been substantially modified to work for this particular application.
     */
    static getTextWidth(osk: VisualKeyboard, text: string, style: {fontFamily?: string, fontSize: string}) {
      // A final fallback - having the right font selected makes a world of difference.
      if(!style.fontFamily) {
        style.fontFamily = getComputedStyle(document.body).fontFamily;
      }

      if(!style.fontSize || style.fontSize == "") {
        style.fontSize = '1em';
      }

      let fontFamily = style.fontFamily;

      // Use of `getComputedStyle` is ideal, but in many of our use cases its preconditions are not met.
      // The following allows us to calculate the font size in those situations.
      let emScale = osk.getKeyEmFontSize();
      let fontSpec = (<KeymanBase> window['keyman']).util.getFontSizeStyle(style.fontSize);

      var fontSize: string;
      if(fontSpec.absolute) {
        // We've already got an exact size - use it!
        fontSize = fontSpec.val + 'px';
      } else {
        fontSize = fontSpec.val * emScale + 'px';
      }

      // re-use canvas object for better performance
      var canvas = OSKKey.getTextWidth['canvas'] || (OSKKey.getTextWidth['canvas'] = document.createElement("canvas"));
      var context = canvas.getContext("2d");
      context.font = fontSize + " " + fontFamily;
      var metrics = context.measureText(text);
      return metrics.width;
    }

    getKeyWidth(): number {
      let units = this.objectUnits();

      if(units == 'px') {
        // For mobile devices, we presently specify width directly in pixels.  Just use that!
        return this.spec['widthpc'];
      } else if(units == '%') {
        // For desktop devices, each key is given a %age of the total OSK width.  We'll need to compute an
        // approximation for that.  `this.kbdDiv` is the element controlling the OSK's width, set in px.
        // ... and since it's null whenever this method would be called during key construction, we simply
        // grab it from the cookie (or its default values) instead.
        let oskWidth = com.keyman.singleton.osk.getWidthFromCookie();

        // This is an approximation that tends to be a bit too large, but it's close enough to be useful.
        return Math.floor(oskWidth * this.spec['widthpc'] / 100);
      }
    }

    objectUnits(): string {
      // Returns a unit string corresponding to how the width for each key is specified.
      if((<KeymanBase>window['keyman']).util.device.formFactor == 'desktop') {
        return '%';
      } else {
        return 'px';
      }
    }

    /**
     * Replace default key names by special font codes for modifier keys
     *
     *  @param  {string}  oldText
     *  @return {string}
     **/
    protected renameSpecialKey(oldText: string): string {
      let keyman = (<KeymanBase>window['keyman'])
      // If a 'special key' mapping exists for the text, replace it with its corresponding special OSK character.
      return VisualKeyboard.specialCharacters[oldText] ? 
        String.fromCharCode(0XE000 + VisualKeyboard.specialCharacters[oldText]) :
        oldText;
    }

    // Produces a HTMLSpanElement with the key's actual text.
    protected generateKeyText(osk: VisualKeyboard): HTMLSpanElement {
      let util = (<KeymanBase>window['keyman']).util;
      let spec = this.spec;

      // Add OSK key labels
      var keyText;
      var t=util._CreateElement('span'), ts=t.style;
      if(spec['text'] == null || spec['text'] == '') {
        keyText='\xa0';  // default:  nbsp.
        if(typeof spec['id'] == 'string') {
          // If the ID's Unicode-based, just use that code.
          if(/^U_[0-9A-F]{4}$/i.test(spec['id'])) {
            keyText=String.fromCharCode(parseInt(spec['id'].substr(2),16));
          }
        }
      } else {
        keyText=spec['text'];
      }

      t.className='kmw-key-text';

      // Use special case lookup for modifier keys
      if(spec['sp'] == '1' || spec['sp'] == '2') {
        // Unique layer-based transformation.
        var tId=((spec['text'] == '*Tab*' && this.layer == 'shift') ? '*TabLeft*' : spec['text']);

        // Transforms our *___* special key codes into their corresponding PUA character codes for keyboard display.
        keyText=this.renameSpecialKey(tId);
      }

      // Grab our default for the key's font and font size.
      ts.fontSize=osk.fontSize;     //Build 344, KMEW-90

      //Override font spec if set for this key in the layout
      if(typeof spec['font'] == 'string' && spec['font'] != '') {
        ts.fontFamily=spec['font'];
      }

      if(typeof spec['fontsize'] == 'string' && spec['fontsize'] != 0) {
        ts.fontSize=spec['fontsize'];
      }

      let keyboardManager = (<KeymanBase>window['keyman']).keyboardManager;

      // For some reason, fonts will sometimes 'bug out' for the embedded iOS page if we
      // instead assign fontFamily to the existing style 'ts'.  (Occurs in iOS 12.)
      let styleSpec: {fontFamily?: string, fontSize: string} = {fontSize: ts.fontSize};

      if(ts.fontFamily) {
        styleSpec.fontFamily = ts.fontFamily;
      } else {
        styleSpec.fontFamily = osk.fontFamily; // Helps with style sheet calculations.
      }

      // Check the key's display width - does the key visualize well?
      var width: number = OSKKey.getTextWidth(osk, keyText, styleSpec);
      if(width == 0 && keyText != '' && keyText != '\xa0') {
        // Add the Unicode 'empty circle' as a base support for needy diacritics.
        keyText = '\u25cc' + keyText;

        if(keyboardManager.isRTL()) {
          // Add the RTL marker to ensure it displays properly.
          keyText = '\u200f' + keyText;
        }
        
        // Recompute the new width for use in autoscaling calculations below, just in case.
        width = OSKKey.getTextWidth(osk, keyText, styleSpec);
      }

      let fontSpec = util.getFontSizeStyle(ts.fontSize);
      let keyWidth = this.getKeyWidth();
      let maxProportion = 0.90;
      let proportion = (keyWidth * maxProportion) / width; // How much of the key does the text want to take?

      // Never upscale keys past the default - only downscale them.
      if(proportion < 1) {
        if(fontSpec.absolute) {
          ts.fontSize = proportion * fontSpec.val + 'px';
        } else {
          ts.fontSize = proportion * fontSpec.val + 'em';
        }
      }

      // Finalize the key's text.
      t.innerHTML = keyText;

      return t;
    }
  }

  export class OSKBaseKey extends OSKKey {
    constructor(spec: OSKKeySpec, layer: string) {
      super(spec, layer);
    }

    getId(osk: VisualKeyboard): string {
      // Define each key element id by layer id and key id (duplicate possible for SHIFT - does it matter?)
      return this.layer+'-'+this.spec.id;
    }

    // Produces a small reference label for the corresponding physical key on a US keyboard.
    private generateKeyCapLabel(): HTMLDivElement {
      // Create the default key cap labels (letter keys, etc.)
      var x = Codes.keyCodes[this.spec.id];
      switch(x) {
        // Converts the keyman key id code for common symbol keys into its representative ASCII code.
        // K_COLON -> K_BKQUOTE
        case 186: x=59; break;
        case 187: x=61; break;
        case 188: x=44; break;
        case 189: x=45; break;
        case 190: x=46; break;
        case 191: x=47; break;
        case 192: x=96; break;
        // K_LBRKT -> K_QUOTE
        case 219: x=91; break;
        case 220: x=92; break;
        case 221: x=93; break;
        case 222: x=39; break;
        default:
          // No other symbol character represents a base key on the standard QWERTY English layout.
          if(x < 48 || x > 90) {
            x=0;
          }
      }

      if(x > 0) {
        let q = (<KeymanBase>window['keyman']).util._CreateElement('div');
        q.className='kmw-key-label';
        q.innerHTML=String.fromCharCode(x);
        return q;
      } else {
        // Keyman-only virtual keys have no corresponding physical key.
        return null;
      }
    }

    private processSubkeys(btn: KeyElement) {
      // Add reference to subkey array if defined
      var bsn: number, bsk=btn['subKeys'] = this.spec['sk'];
      // Transform any special keys into their PUA representations.
      for(bsn=0; bsn<bsk.length; bsn++) {
        if(bsk[bsn]['sp'] == '1' || bsk[bsn]['sp'] == '2') {
          var oldText=bsk[bsn]['text'];
          bsk[bsn]['text']=this.renameSpecialKey(oldText);
        }
      }

      // If a subkey array is defined, add an icon
      var skIcon=(<KeymanBase>window['keyman']).util._CreateElement('div');
      skIcon.className='kmw-key-popup-icon';
      //kDiv.appendChild(skIcon);
      btn.appendChild(skIcon);
    }

    construct(osk: VisualKeyboard, layout: LayoutFormFactor, rowStyle: CSSStyleDeclaration, totalPercent: number): {element: HTMLDivElement, percent: number} {
      let util = (<KeymanBase>window['keyman']).util;
      let spec = this.spec;
      let isDesktop = util.device.formFactor == 'desktop'

      let kDiv=util._CreateElement('div');
      kDiv.className='kmw-key-square';

      let ks=kDiv.style;
      ks.width=this.objectGeometry(spec['widthpc']);

      let originalPercent = totalPercent;
      
      let btnEle=util._CreateElement('div');
      let btn = link(btnEle, new KeyData(this, spec['id']));

      // Set button class
      osk.setButtonClass(spec,btn,layout);

      // Set key and button positioning properties.
      if(!isDesktop) {
        // Regularize interkey spacing by rounding key width and padding (Build 390)
        ks.left=this.objectGeometry(totalPercent+spec['padpc']);
        ks.bottom=rowStyle.bottom;
        ks.height=rowStyle.height;  //must be specified in px for rest of layout to work correctly

        // Set distinct phone and tablet button position properties
        btn.style.left=ks.left;
        btn.style.width=ks.width;
      } else {
        ks.marginLeft=this.objectGeometry(spec['padpc']);
      }

      totalPercent=totalPercent+spec['padpc']+spec['widthpc'];

      // Add the (US English) keycap label for desktop OSK or if KDU flag is non-zero
      if(layout.keyLabels || isDesktop) {
        let keyCap = this.generateKeyCapLabel();

        if(keyCap) {
          btn.appendChild(keyCap);
        }
      }

      // Define each key element id by layer id and key id (duplicate possible for SHIFT - does it matter?)
      btn.id=this.getId(osk);

      // Define callbacks to handle key touches: iOS and Android tablets and phones
      // TODO: replace inline function calls??
      if(!util.device.touchable) {
        // Highlight key while mouse down or if moving back over originally selected key
        btn.onmouseover=btn.onmousedown=osk.mouseOverMouseDownHandler; // Build 360

        // Remove highlighting when key released or moving off selected element
        btn.onmouseup=btn.onmouseout=osk.mouseUpMouseOutHandler; //Build 360
      }

      // Make sure the key text is the element's first child - processSubkeys()
      // will add an extra element if subkeys exist, which can interfere with
      // keyboard/language name display on the space bar!
      btn.appendChild(this.generateKeyText(osk));

      // Handle subkey-related tasks.
      if(typeof(spec['sk']) != 'undefined' && spec['sk'] != null) {
        this.processSubkeys(btn);
      } else {
        btn['subKeys']=null;
      }
      
      // Add text to button and button to placeholder div
      kDiv.appendChild(btn);

      // Prevent user selection of key captions
      //t.style.webkitUserSelect='none';
      
      // The 'return value' of this process.
      return {element: kDiv, percent: totalPercent - originalPercent};
    }

    objectGeometry(v: number): string {
      let unit = this.objectUnits();
      if(unit == '%') {
        return v + unit;
      } else { // unit == 'px'
        return Math.round(v)+unit;
      }
    }
  }

  export class OSKSubKey extends OSKKey {
    constructor(spec: OSKKeySpec, layer: string) {
      super(spec, layer);
    }

    getId(osk: VisualKeyboard): string {
      let spec = this.spec;
      // Create (temporarily) unique ID by prefixing 'popup-' to actual key ID
      if(typeof(this.layer) == 'string' && this.layer != '') {
        return 'popup-'+this.layer+'-'+spec['id'];
      } else {
        // We only create subkeys when they're needed - the currently-active layer should be fine.
        return 'popup-' + osk.layerId + '-'+spec['id'];
      }
    }

    construct(osk: VisualKeyboard, baseKey: HTMLDivElement, topMargin: boolean): HTMLDivElement {
      let spec = this.spec;

      let kDiv=document.createElement('div');
      let tKey = osk.getDefaultKeyObject();
      let ks=kDiv.style;

      for(var tp in tKey) {
        if(typeof spec[tp] != 'string') {
          spec[tp]=tKey[tp];
        }
      }

      kDiv.className='kmw-key-square-ex';
      if(topMargin) {
        ks.marginTop='5px';
      }

      if(typeof spec['width'] != 'undefined') {
        ks.width=(parseInt(spec['width'],10)*baseKey.offsetWidth/100)+'px';
      } else {
        ks.width=baseKey.offsetWidth+'px';
      }
      ks.height=baseKey.offsetHeight+'px';

      let btnEle=document.createElement('div');
      let btn = link(btnEle, new KeyData(this, spec['id']));

      osk.setButtonClass(spec,btn);
      btn.id = this.getId(osk);

      // Must set button size (in px) dynamically, not from CSS
      let bs=btn.style;
      bs.height=ks.height;
      bs.width=ks.width;

      // Must set position explicitly, at least for Android
      bs.position='absolute';

      btn.appendChild(this.generateKeyText(osk));
      kDiv.appendChild(btn);

      return kDiv;
    }
  }

  //#endregion

  export class VisualKeyboard {
    // Defines the PUA code mapping for the various 'special' modifier/control keys on keyboards.
    static specialCharacters = {
      '*Shift*':    8,
      '*Enter*':    5,
      '*Tab*':      6,
      '*BkSp*':     4,
      '*Menu*':     11,
      '*Hide*':     10,
      '*Alt*':      25,
      '*Ctrl*':     1,
      '*Caps*':     3,
      '*ABC*':      16,
      '*abc*':      17,
      '*123*':      19,
      '*Symbol*':   21,
      '*Currency*': 20,
      '*Shifted*':  8, // set SHIFTED->9 for filled arrow icon
      '*AltGr*':    2,
      '*TabLeft*':  7,
      '*LAlt*':     0x56,
      '*RAlt*':     0x57,
      '*LCtrl*':    0x58,
      '*RCtrl*':    0x59,
      '*LAltCtrl*':       0x60,
      '*RAltCtrl*':       0x61,
      '*LAltCtrlShift*':  0x62,
      '*RAltCtrlShift*':  0x63,
      '*AltShift*':       0x64,
      '*CtrlShift*':      0x65,
      '*AltCtrlShift*':   0x66,
      '*LAltShift*':      0x67,
      '*RAltShift*':      0x68,
      '*LCtrlShift*':     0x69,
      '*RCtrlShift*':     0x70
    };

    /**
     * Contains layout properties corresponding to the OSK's layout.  Needs to be public
     * so that its geometry may be updated on rotations and keyboard resize events, as 
     * said geometry needs to be accurate for fat-finger probability calculations.
     */
    layout: ActiveLayout;
    layers: LayoutLayer[];
    layerId: string = "default";
    layerIndex: number;

    // Stores the base element for this instance of the visual keyboard.
    // Formerly known as osk._DivVKbd
    kbdDiv: HTMLDivElement;
    kbdHelpDiv: HTMLDivElement;
    styleSheet: HTMLStyleElement;

    // Style-related properties
    fontFamily: string;
    fontSize: string;

    // State-related properties
    ddOSK: boolean = false;
    popupVisible: boolean;
    keyPending: KeyElement;
    touchPending: Touch;
    deleteKey: KeyElement;
    deleting: number; // Tracks a timer id for repeated deletions.
    nextLayer: string;
    currentKey: string;

    // Special keys (for the currently-visible layer)
    lgKey: KeyElement;
    hkKey: KeyElement; // hide keyboard key
    spaceBar: KeyElement;

    // Touch-tracking properties
    touchX: number;
    touchY: number;
    touchCount: number;
    currentTarget: KeyElement;

    // Popup key management
    popupBaseKey: KeyElement;
    popupPending: boolean = false;
    subkeyDelayTimer: number;
    popupDelay: number = 500;
    menuEvent: KeyElement; // Used by embedded-mode.
    keytip: {key: KeyElement, state: boolean, element?: HTMLDivElement};
    popupCallout: HTMLDivElement;

    // Function fields (fleshed out by kmwnative.ts and/or kmwembedded.ts)
    touchHold: (key: KeyElement) => void;
    optionKey: (e: KeyElement, keyName: string, keyDown: boolean) => void;
    highlightSubKeys: (key: KeyElement, x: number, y: number) => void = this.highlightSubKeys || function(k,x,y) {};
    showKeyTip: (key: KeyElement, on: boolean) => void;
    drawPreview: (canvas: HTMLCanvasElement, w: number, h: number, edge: number) => void = this.drawPreview || function(c,w,h,e) {};
    createKeyTip: () => void;
    addCallout: (key: KeyElement) => HTMLDivElement = this.addCallout || function(key) {return null};
    waitForFonts: (kfd,ofd) => boolean = this.waitForFonts || function(kfd,ofd){return true;}; // Default is used by embedded.
    adjustHeights: () => boolean;

    //#region OSK constructor and helpers

    /**
     * @param       {Object}      PVK         Visual keyboard name
     * @param       {Object}      Lhelp       true if OSK defined for this keyboard
     * @param       {Object}      layout0 
     * @param       {Number}      kbdBitmask  Keyboard modifier bitmask
     * Description  Generates the base visual keyboard element, prepping for attachment to KMW
     */
    constructor(PVK, Lhelp, layout0: LayoutFormFactor, kbdBitmask: number) {
      let keyman = com.keyman.singleton;

      let util = keyman.util;
      let formFactor = util.device.formFactor;

      let layout=layout0;
      var Lkbd=util._CreateElement('div'), oskWidth;//s=Lkbd.style,
      var activeKeyboard = keyman.keyboardManager.activeKeyboard;

      // Build a layout using the default for the device
      if(typeof layout != 'object' || layout == null) {
        layout=Layouts.buildDefaultLayout(PVK,kbdBitmask, formFactor);
      }

      // Create the collection of HTML elements from the device-dependent layout object
      this.layout = ActiveLayout.polyfill(layout, formFactor);
      this.layers=layout['layer'];

      // Override font if specified by keyboard
      if('font' in layout) {
        this.fontFamily=layout['font']; 
      } else {
        this.fontFamily='';
      }

      // Set flag to add default (US English) key label if specified by keyboard
      layout.keyLabels = activeKeyboard && ((typeof(activeKeyboard['KDU']) != 'undefined') && activeKeyboard['KDU']);
      let divLayerContainer = this.deviceDependentLayout(layout, util.device.formFactor);

      this.ddOSK = true;

      // Append the OSK layer group container element to the containing element
      //osk.keyMap = divLayerContainer;
      Lkbd.appendChild(divLayerContainer);

      // Set base class - OS and keyboard added for Build 360
      this.kbdHelpDiv = this.kbdDiv = Lkbd;

      Lkbd.className=util.device.formFactor+' kmw-osk-inner-frame';
    }

    /**
     * Returns the default properties for a key object, used to construct
     * both a base keyboard key and popup keys
     *
     * @return    {Object}    An object that contains default key properties
     */
    getDefaultKeyObject(): OSKKeySpec {
      return new OSKKeySpec(undefined, '', ActiveKey.DEFAULT_KEY.width, ActiveKey.DEFAULT_KEY.sp as ButtonClass, null, ActiveKey.DEFAULT_KEY.pad);
    };

    /**
     * Create the OSK for a particular keyboard and device
     *
     * @param       {Object}              layout      OSK layout definition
     * @param       {string}              formFactor  layout form factor
     * @return      {Object}                          fully formatted OSK object
     */
    deviceDependentLayout(layout: LayoutFormFactor, formFactor: string): HTMLDivElement {
      let util = com.keyman.singleton.util;
      let oskManager = com.keyman.singleton.osk;
      let rowsPercent = 100;

      var lDiv=util._CreateElement('div'), ls=lDiv.style, totalHeight=0;

      // Set OSK box default style
      lDiv.className='kmw-key-layer-group';

      // Adjust OSK height for mobile and tablet devices TODO move outside this function???
      switch(formFactor) {
        case 'phone':
        case 'tablet':
          totalHeight=oskManager.getHeight();
          ls.height=totalHeight+'px';
          rowsPercent = Math.round(100*oskManager.getKeyboardHeight()/totalHeight );
          break;
      }

      // Return empty DIV if no layout defined
      if(layout == null) {
        return lDiv;
      }

      // Set default OSK font size (Build 344, KMEW-90)
      let layoutFS = layout['fontsize'];
      if(typeof layoutFS == 'undefined' || layoutFS == null || layoutFS == '') {
        ls.fontSize='1em';
      } else {
        ls.fontSize=layout['fontsize'];
      }

      this.fontSize=ls.fontSize;       //TODO: move outside function*********

      // Create a separate OSK div for each OSK layer, only one of which will ever be visible
      var n: number, i: number, j: number;
      var layers: LayoutLayer[], gDiv: HTMLDivElement;
      var rowHeight: number, rDiv: HTMLDivElement;
      var keys: LayoutKey[], key: LayoutKey, rs: CSSStyleDeclaration, gs: CSSStyleDeclaration;

      layers=layout['layer'];

      // Set key default attributes (must use exportable names!)
      var tKey=this.getDefaultKeyObject();
      tKey['fontsize']=ls.fontSize;

      // Identify key labels (e.g. *Shift*) that require the special OSK font
      var specialLabel=/\*\w+\*/;

      // ***Delete any empty rows at the end added by compiler bug...
      for(n=0; n<layers.length; n++) {
        let layer=layers[n];
        let rows=layer['row'];
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

      // Set the OSK row height, **assuming all layers have the same number of rows**

      // Calculate default row height
      rowHeight = rowsPercent/layers[0].row.length;

      // For desktop OSK, use a percentage of the OSK height
      if(formFactor == 'desktop') {
        rowHeight = rowsPercent/layers[0].row.length;
      }

      // Get the actual available document width and scale factor according to device type
      var objectWidth : number;
      if(formFactor == 'desktop') {
        objectWidth = 100;
      } else {
        objectWidth = oskManager.getWidth();
      }

      if(util.device.touchable) { //  /*&& ('ontouchstart' in window)*/ // Except Chrome emulation doesn't set this.
                                                                        // Not to mention, it's rather redundant.
        lDiv.addEventListener('touchstart', this.touch, true);
        // The listener below fails to capture when performing automated testing checks in Chrome emulation unless 'true'.
        lDiv.addEventListener('touchend', this.release,true); 
        lDiv.addEventListener('touchmove', this.moveOver,false);
        //lDiv.addEventListener('touchcancel', osk.cancel,false); //event never generated by iOS
      }

      for(n=0; n<layers.length; n++) {
        let layer=layers[n] as ActiveLayer;
        gDiv=util._CreateElement('div'), gs=gDiv.style;
        gDiv.className='kmw-key-layer';

        // Always make the first layer visible
        gs.display=(n==0?'block':'none');
        gs.height=ls.height;

        // Set font for layer if defined in layout
        if('font' in layout) gs.fontFamily=layout['font']; else gs.fontFamily='';

        gDiv['layer']=gDiv['nextLayer']=layer['id'];
        if(typeof layer['nextlayer'] == 'string') gDiv['nextLayer']=layer['nextlayer'];

        // Create a DIV for each row of the group
        let rows=layer['row'];

        for(i=0; i<rows.length; i++) {
          rDiv=util._CreateElement('div');
          rDiv.className='kmw-key-row';
          // The following event trap is needed to prevent loss of focus in IE9 when clicking on a key gap.
          // Unclear why normal _CreateElement prevention of loss of focus does not seem to work here.
          // Appending handler to event handler chain does not work (other event handling remains active).
          rDiv.onmousedown=util.mouseDownPreventDefaultHandler; // Build 360
          //util.attachDOMEvent(rDiv,'mousedown',function(e){if(e)e.preventDefault();

          let row=rows[i];
          rs=rDiv.style;

          // Set row height. (Phone and tablet heights are later recalculated
          // and set in px, allowing for viewport scaling.)
          rs.maxHeight=rs.height=rowHeight+'%';

          // Apply defaults, setting the width and other undefined properties for each key
          keys=row['key'];

          // Calculate actual key widths by multiplying by the OSK's width and rounding appropriately,
          // adjusting the width of the last key to make the total exactly 100%.
          // Overwrite the previously-computed percent.
          // NB: the 'percent' suffix is historical, units are percent on desktop devices, but pixels on touch devices
          // All key widths and paddings are rounded for uniformity
          var keyPercent: number, padPercent: number, totalPercent=0;
          for(j=0; j<keys.length-1; j++) {
            keyPercent = Math.round(keys[j]['widthpc'] * objectWidth); //Math.round(parseInt(keys[j]['width'],10)*objectWidth/totalWidth);
            keys[j]['widthpc']=keyPercent;
            padPercent = Math.round(keys[j]['padpc'] * objectWidth); // Math.round(parseInt(keys[j]['pad'],10)*objectWidth/totalWidth);
            keys[j]['padpc']=padPercent;

            // Recompute center's x-coord with exact, in-browser values.
            (<ActiveKey> keys[j]).proportionalX = (totalPercent + padPercent + (keyPercent/2))/objectWidth;
            (<ActiveKey> keys[j]).proportionalWidth = keyPercent / objectWidth;

            totalPercent += padPercent+keyPercent;
          }

          // Allow for right OSK margin (15 layout units)
          let rightMargin = Math.round(15*objectWidth/layer.totalWidth);
          totalPercent += rightMargin;

          // If a single key, and padding is negative, add padding to right align the key
          if(keys.length == 1 && parseInt(keys[0]['pad'],10) < 0) {
            keyPercent = Math.round(keys[0]['widthpc'] * objectWidth); //Math.round(parseInt(keys[0]['width'],10)*objectWidth/totalWidth);
            keys[0]['widthpc']=keyPercent;
            totalPercent += keyPercent;
            keys[0]['padpc']=(objectWidth-totalPercent);

            // Recompute center's x-coord with exact, in-browser values.
            (<ActiveKey> keys[0]).proportionalX = (totalPercent - rightMargin - keyPercent/2)/objectWidth;
            (<ActiveKey> keys[0]).proportionalWidth = keyPercent / objectWidth;
          } else if(keys.length > 0) {
            j=keys.length-1;
            padPercent = Math.round(keys[j]['padpc'] * objectWidth); //Math.round(parseInt(keys[j]['pad'],10)*objectWidth/totalWidth);
            keys[j]['padpc']=padPercent;
            totalPercent += padPercent;
            keys[j]['widthpc']= keyPercent = (objectWidth-totalPercent);

            // Recompute center's x-coord with exact, in-browser values.
            (<ActiveKey> keys[j]).proportionalX = (objectWidth - rightMargin - keyPercent/2)/objectWidth;
            (<ActiveKey> keys[j]).proportionalWidth = keyPercent / objectWidth;
          }

          //Create the key square (an outer DIV) for each key element with padding, and an inner DIV for the button (btn)
          totalPercent=0;
          for(j=0; j<keys.length; j++) {
            key=keys[j];
            
            var keyGenerator = new OSKBaseKey(key as OSKKeySpec, layer['id']);
            var keyTuple = keyGenerator.construct(this, layout, rs, totalPercent);

            rDiv.appendChild(keyTuple.element);
            totalPercent += keyTuple.percent;
          }
          // Add row to layer
          gDiv.appendChild(rDiv);
        }
        // Add layer to group
        lDiv.appendChild(gDiv);
      }
      return lDiv;
    }
    //#endregion

    //#region OSK touch handlers
    getTouchCoordinatesOnKeyboard(touch: Touch) {
      let keyman = com.keyman.singleton;
      
      // We need to compute the 'local', keyboard-based coordinates for the touch.
      let kbdCoords = keyman.util.getAbsolute(this.kbdDiv.firstChild as HTMLElement);
      let offsetCoords = {x: touch.pageX - kbdCoords.x, y: touch.pageY - kbdCoords.y};

      let layerGroup = this.kbdDiv.firstChild as HTMLDivElement;  // Always has proper dimensions, unlike kbdDiv itself.
      offsetCoords.x /= layerGroup.offsetWidth;
      offsetCoords.y /= layerGroup.offsetHeight;

      return offsetCoords;
    }

    getTouchProbabilities(touch: Touch): text.KeyDistribution {
      let keyman = com.keyman.singleton;
      if(!keyman.modelManager.mayCorrect) {
        return null;
      }
      
      let touchKbdPos = this.getTouchCoordinatesOnKeyboard(touch);
      let layerGroup = this.kbdDiv.firstChild as HTMLDivElement;  // Always has proper dimensions, unlike kbdDiv itself.
      return this.layout.layer[this.layerIndex].getTouchProbabilities(touchKbdPos, layerGroup.offsetWidth / layerGroup.offsetHeight);
    }

    /**
     * The main OSK touch start event handler
     *
     *  @param  {Event} e   touch start event object
     *
     */
    touch: (e: TouchEvent) => void = function(this: VisualKeyboard, e: TouchEvent) {
      let Processor = com.keyman.singleton.textProcessor;
      // Identify the key touched
      var t = <HTMLElement> e.changedTouches[0].target, key = this.keyTarget(t);

      // Save the touch point
      this.touchX = e.changedTouches[0].pageX;

      // Set the key for the new touch point to be current target, if defined
      this.currentTarget = key;

      // Prevent multi-touch if popup displayed
      var sk = document.getElementById('kmw-popup-keys');
      if((sk && sk.style.visibility == 'visible') || this.popupVisible) {
        return;
      }

      // Keep track of number of active (unreleased) touch points
      this.touchCount = e.touches.length;

      // Get nearest key if touching a hidden key or the end of a key row
      if((key && (key.className.indexOf('key-hidden') >= 0))
        || t.className.indexOf('kmw-key-row') >= 0) {
        key = this.findNearestKey(e,t);
      }
      // Do not do anything if no key identified!
      if(key == null) {
        return;
      }

      // Get key name (K_...) from element ID
      let keyName = key['keyId'];

      // Highlight the touched key
      this.highlightKey(key,true);

      // Special function keys need immediate action
      if(keyName == 'K_LOPT' || keyName == 'K_ROPT')      {
        window.setTimeout(function(this: VisualKeyboard){
          Processor.clickKey(key);
        }.bind(this),0);
        this.keyPending = null;
        this.touchPending = null;

        // Also backspace, to allow delete to repeat while key held
      } else if(keyName == 'K_BKSP') {
        let touchProbabilities = this.getTouchProbabilities(e.changedTouches[0]);
        // While we could inline the execution of the delete key here, we lose the ability to
        // record the backspace key if we do so.
        Processor.clickKey(key, e.changedTouches[0], this.layerId, touchProbabilities);
        this.deleteKey = key;
        this.deleting = window.setTimeout(this.repeatDelete,500);
        this.keyPending = null;
        this.touchPending = null;
      } else {
        if(this.keyPending) {
          this.highlightKey(this.keyPending, false);
          let touchProbabilities = this.getTouchProbabilities(this.touchPending);
          Processor.clickKey(this.keyPending, this.touchPending, this.layerId, touchProbabilities);
          this.clearPopup();
          // Decrement the number of unreleased touch points to prevent
          // sending the keystroke again when the key is actually released
          this.touchCount--;
        } else {
          // If this key has subkey, start timer to display subkeys after delay, set up release
          this.touchHold(key);
        }
        this.keyPending = key;
        this.touchPending = e.changedTouches[0];
      }
    }.bind(this);

    /**
     * OSK touch release event handler
     *
     *  @param  {Event} e   touch release event object
     *
     **/
    release: (e: TouchEvent) => void = function(this: VisualKeyboard, e: TouchEvent) {
      let Processor = com.keyman.singleton.textProcessor;

      // Prevent incorrect multi-touch behaviour if native or device popup visible
      var sk = document.getElementById('kmw-popup-keys'), t = this.currentTarget;

      if((sk && sk.style.visibility == 'visible') || this.popupVisible) {
        // Ignore release if a multiple touch
        if(e.touches.length > 0) {
          return;
        }

        // Cancel (but do not execute) pending key if neither a popup key or the base key
        if((t == null) || ((t.id.indexOf('popup') < 0) && (t.id != this.popupBaseKey.id))) {
          this.highlightKey(this.keyPending,false);
          this.clearPopup();
          this.keyPending = null;
          this.touchPending = null;
        }
      }

      // Handle menu key release event
      if(t && t.id) {
        this.optionKey(t, t.id, false);
      }

      // Test if moved off screen (effective release point must be corrected for touch point horizontal speed)
      // This is not completely effective and needs some tweaking, especially on Android
      var x = e.changedTouches[0].pageX;
      var beyondEdge = ((x < 2 && this.touchX > 5) || (x > window.innerWidth - 2 && this.touchX < window.innerWidth - 5));

      // Save then decrement current touch count
      var tc=this.touchCount;
      if(this.touchCount > 0) {
        this.touchCount--;
      }

      // Process and clear highlighting of pending target
      if(this.keyPending) {
        this.highlightKey(this.keyPending,false);

        // Output character unless moved off key
        if(this.keyPending.className.indexOf('hidden') < 0 && tc > 0 && !beyondEdge) {
          let touchProbabilities = this.getTouchProbabilities(e.changedTouches[0]);
          Processor.clickKey(this.keyPending, e.changedTouches[0], this.layerId, touchProbabilities);
        }
        this.clearPopup();
        this.keyPending = null;
        this.touchPending = null;
        // Always clear highlighting of current target on release (multi-touch)
      } else {
        var tt = e.changedTouches[0];
        t = this.keyTarget(tt.target);
        if(!t) {
          var t1 = document.elementFromPoint(tt.clientX,tt.clientY);
          t = this.findNearestKey(e, <HTMLElement> t1);
        }

        this.highlightKey(t,false);
      }

      // Clear repeated backspace if active
      if(this.deleting) {
        window.clearTimeout(this.deleting);
      }
      this.deleting = 0;
    }.bind(this);

    /**
     * OSK touch move event handler
     *
     *  @param  {Event} e   touch move event object
     *
     **/
    moveOver: (e: TouchEvent) => void = function(this: VisualKeyboard, e: TouchEvent) {
      let util = com.keyman.singleton.util;
      e.preventDefault();
      e.cancelBubble=true;

      if(typeof e.stopImmediatePropagation == 'function') {
        e.stopImmediatePropagation();
      } else if(typeof e.stopPropagation == 'function') {
        e.stopPropagation();
      }

      // Do not attempt to support reselection of target key for overlapped keystrokes
      if(e.touches.length > 1 || this.touchCount == 0) {
        return;
      }

      // Get touch position
      var x=typeof e.touches == 'object' ? e.touches[0].clientX : e.clientX,
          y=typeof e.touches == 'object' ? e.touches[0].clientY : e.clientY;

      // Move target key and highlighting
      var t = this.touchPending = e.changedTouches[0],
          t1 = <HTMLElement> document.elementFromPoint(x,y),
          key0 = this.keyPending,
          key1 = this.keyTarget(t1); // Not only gets base keys, but also gets popup keys!

      // Find the nearest key to the touch point if not on a visible key
      if((key1 && key1.className.indexOf('key-hidden') >= 0) ||
        (t1 && (!key1) && t1.className.indexOf('key-row') >= 0)) {
          key1 = this.findNearestKey(e,t1);
      }

      // Stop repeat if no longer on BKSP key
      if(key1 && (typeof key1.id == 'string') && (key1.id.indexOf('BKSP') < 0)) {
        if(this.deleting) {
          window.clearTimeout(this.deleting);
        }
        this.deleting = 0;
      }

      // Do not move over keys if device popup visible
      if(this.popupVisible) {
        if(key1 == null) {
          if(key0) {
            this.highlightKey(key0,false);
          }
          this.keyPending=null;
          this.touchPending=null;
        } else {
          if(key1 == this.popupBaseKey) {
            if(!util.hasClass(key1,'kmw-key-touched')) {
              this.highlightKey(key1,true);
            }
            this.keyPending = key1;
            this.touchPending = e.touches[0];
          } else {
            if(key0) {
              this.highlightKey(key0,false);
            }
            this.keyPending = null;
            this.touchPending = null;
          }
        }
        return;
      }

      var sk=document.getElementById('kmw-popup-keys');

      // Use the popup duplicate of the base key if a phone with a visible popup array
      if(sk && sk.style.visibility == 'visible' && util.device.formFactor == 'phone' && key1 == this.popupBaseKey) {
        key1 = <KeyElement> sk.childNodes[0].firstChild;
      }

      // Identify current touch position (to manage off-key release)
      this.currentTarget = key1;

      // Clear previous key highlighting
      if(key0 && key1 && (key1.id != key0.id)) {
        this.highlightKey(key0,false);
      }

      // If popup is visible, need to move over popup, not over main keyboard
      this.highlightSubKeys(key1,x,y);

      if(sk && sk.style.visibility == 'visible') {
        // Once a subkey array is displayed, do not allow changing the base key.
        // Keep that array visible and accept no other options until the touch ends.
        if(key1 && key1.id.indexOf('popup') < 0 && key1 != this.popupBaseKey) {
          return;
        }

        // Highlight the base key on devices that do not append it to the subkey array.
        if(key1 && key1 == this.popupBaseKey && key1.className.indexOf('kmw-key-touched') < 0) {
          this.highlightKey(key1,true);
        }
        // Cancel touch if moved up and off keyboard, unless popup keys visible
      } else {
        // _Box has (most of) the useful client values.
        let _Box = this.kbdDiv.offsetParent as HTMLElement; // == osk._Box
        let height = (this.kbdDiv.firstChild as HTMLElement).offsetHeight; // firstChild == layer-group, has height info.
        // We need to adjust the offset properties by any offsets related to the active banner.
        var yMin = Math.max(5, this.kbdDiv.offsetTop + _Box.offsetTop - 0.25*height);
        if(key0 && e.touches[0].pageY < yMin) {
          this.highlightKey(key0,false);
          this.showKeyTip(null,false);
          this.keyPending = null;
          this.touchPending = null;
        }
      }

      // Replace the target key, if any, by the new target key
      // Do not replace a null target, as that indicates the key has already been released
      if(key1 && this.keyPending) {
        this.keyPending = key1;
        this.touchPending = e.touches[0];
      }

      if(this.keyPending) {
        if(key0 != key1 || key1.className.indexOf('kmw-key-touched') < 0) {
          this.highlightKey(key1,true);
        }
      }

      if(key0 && key1 && (key1 != key0) && (key1.id != '')) {
        //  Display the touch-hold keys (after a pause)
        this.touchHold(key1);
        /*
        // Clear and restart the popup timer
        if(this.subkeyDelayTimer)
        {
          window.clearTimeout(this.subkeyDelayTimer);
          this.subkeyDelayTimer = null;
        }
        if(key1.subKeys != null)
        {
          this.subkeyDelayTimer = window.setTimeout(
            function()
            {
              this.clearPopup();
              this.showSubKeys(key1);
            }.bind(this),
            this.popupDelay);
        }
        */
      }
    }.bind(this);

    /**
     * Get the current key target from the touch point element within the key
     *
     * @param   {Object}  t   element at touch point
     * @return  {Object}      the key element (or null)
     **/
    keyTarget(target: HTMLElement | EventTarget): KeyElement {
      let keyman = com.keyman.singleton;
      let util = keyman.util;
      let t = <HTMLElement> target;

      try {
        if(t) {
          if(util.hasClass(t,'kmw-key')) {
            return getKeyFrom(t);
          }
          if(t.parentNode && util.hasClass(<HTMLElement> t.parentNode,'kmw-key')) {
            return getKeyFrom(t.parentNode);
          }
          if(t.firstChild && util.hasClass(<HTMLElement> t.firstChild,'kmw-key')) {
            return getKeyFrom(t.firstChild);
          }
        }
      } catch(ex) {}
      return null;
    }

    /**
     * Identify the key nearest to the touch point if at the end of a key row,
     * but return null more than about 0.6 key width from the nearest key.
     *
     *  @param  {Event}   e   touch event
     *  @param  {Object}  t   HTML object at touch point
     *  @return {Object}      nearest key to touch point
     *
     **/
    findNearestKey(e: TouchEvent, t: HTMLElement): KeyElement {
      if((!e) || (typeof e.changedTouches == 'undefined')
        || (e.changedTouches.length == 0)) {
        return null;
      }

      // Get touch point on screen
      var x = e.changedTouches[0].pageX;

      // Get key-row beneath touch point
      while(t && t.className !== undefined && t.className.indexOf('key-row') < 0) {
        t = <HTMLElement> t.parentNode;
      }
      if(!t) {
        return null;
      }

      // Find minimum distance from any key
      var k, k0=0, dx, dxMax=24, dxMin=100000, x1, x2;
      for(k = 0; k < t.childNodes.length; k++) {
        let childNode = t.childNodes[k] as HTMLElement;
        if(childNode.className !== undefined && childNode.className.indexOf('key-hidden') >= 0) {
          continue;
        }
        x1 = childNode.offsetLeft;
        x2 = x1 + childNode.offsetWidth;
        if(x >= x1 && x <= x2) {
          // Within the key square
          return <KeyElement> childNode.firstChild;
        }
        dx = x1 - x;
        if(dx >= 0 && dx < dxMin) {
          // To right of key
          k0 = k; dxMin = dx;
        }
        dx = x - x2;
        if(dx >= 0 && dx < dxMin) {
          // To left of key
          k0 = k; dxMin = dx;
        }
      }

      if(dxMin < 100000) {
        t = <HTMLElement> t.childNodes[k0];
        x1 = t.offsetLeft;
        x2 = x1 + t.offsetWidth;

        // Limit extended touch area to the larger of 0.6 of key width and 24 px
        if(t.offsetWidth > 40) {
          dxMax = 0.6 * t.offsetWidth;
        }

        if(((x1 - x) >= 0 && (x1 - x) < dxMax) || ((x - x2) >= 0 && (x - x2) < dxMax)) {
          return <KeyElement> t.firstChild;
        }
      }
      return null;
    }

    /**
     *  Repeat backspace as long as the backspace key is held down
     **/
    repeatDelete: () => void = function(this: VisualKeyboard) {
      let Processor = com.keyman.singleton.textProcessor;

      if(this.deleting) {
        Processor.clickKey(this.deleteKey);
        this.deleting = window.setTimeout(this.repeatDelete,100);
      }
    }.bind(this);
    //#endregion

    // cancel = function(e) {} //cancel event is never generated by iOS

    /**
     * Function     _UpdateVKShiftStyle
     * Scope        Private
     * @param       {string=}   layerId
     * Description  Updates the OSK's visual style for any toggled state keys
     */
    _UpdateVKShiftStyle(layerId?: string) {
      var i, n, layer=null, layerElement=null;
      let Processor = com.keyman.singleton.textProcessor;

      if(layerId) {
        for(n=0; n<this.layers.length; n++) {
          if(this.layers[n]['id'] == this.layerId) {
            break;
          }
        }

        return;  // Failed to find the requested layer.
      } else {
        n=this.layerIndex;
        layerId=this.layers[n]['id'];
      }

      layer=this.layers[n];
      
      // Set the on/off state of any visible state keys.
      var states = ['K_CAPS',      'K_NUMLOCK',  'K_SCROLL'];
      var keys   = [layer.capsKey, layer.numKey, layer.scrollKey];

      for(i=0; i < keys.length; i++) {
        // Skip any keys not in the OSK!
        if(keys[i] == null) {
          continue;
        }

        keys[i]['sp'] = Processor.stateKeys[states[i]] ? Layouts.buttonClasses['SHIFT-ON'] : Layouts.buttonClasses['SHIFT'];
        var btn = document.getElementById(layerId+'-'+states[i]);

        this.setButtonClass(keys[i], btn, this.layout);
      }
    }

    /**
     * Attach appropriate class to each key button, according to the layout
     *
     * @param       {Object}    key     key object
     * @param       {Object}    btn     button object
     * @param       {Object=}   layout  source layout description (optional, sometimes)
     */
    setButtonClass(key, btn, layout?) {
      let keyman = com.keyman.singleton;
      var n=0, keyTypes=['default','shift','shift-on','special','special-on','','','','deadkey','blank','hidden'];
      if(typeof key['dk'] == 'string' && key['dk'] == '1') { 
        n=8;
      }

      if(typeof key['sp'] == 'string') {
        n=parseInt(key['sp'],10);
      } 

      if(n < 0 || n > 10) {
        n=0;
      }

      layout = layout || this.layout;

      // Apply an overriding class for 5-row layouts
      var nRows=layout['layer'][0]['row'].length;
      if(nRows > 4 && keyman.util.device.formFactor == 'phone') {
        btn.className='kmw-key kmw-5rows kmw-key-'+keyTypes[n];
      } else {
        btn.className='kmw-key kmw-key-'+keyTypes[n];
      }
    }

    clearPopup() {
      let oskManager = com.keyman.singleton.osk;
      // Remove the displayed subkey array, if any, and cancel popup request
      var sk=document.getElementById('kmw-popup-keys');
      if(sk != null) {
        if(sk.shim) {
          oskManager._Box.removeChild(sk.shim);
        }
        sk.parentNode.removeChild(sk);
      }

      if(this.popupCallout) {
        oskManager._Box.removeChild(this.popupCallout);
      }
      this.popupCallout = null;

      if(this.subkeyDelayTimer) {
          window.clearTimeout(this.subkeyDelayTimer);
          this.subkeyDelayTimer = null;
      }
      this.popupBaseKey = null;
    }
    
    //#region 'native'-mode subkey handling
    /**
     * Display touch-hold array of 'sub-keys' above the currently touched key
     * @param       {Object}    e      primary key element
     */
    showSubKeys(e: KeyElement) {
      // Do not show subkeys if key already released
      if(this.keyPending == null) {
        return;
      }

      let keyman = com.keyman.singleton;
      let util = keyman.util;
      let device = util.device;
      
      // A tag we directly set on a key element during its construction.
      let subKeySpec: OSKKeySpec[] = e['subKeys'];

      // Create holder DIV for subkey array, and set styles.
      // A subkey array for Shift will only appear if extra layers exist

      // The holder is position:fixed, but the keys do not need to be, as no scrolling
      // is possible while the array is visible.  So it is simplest to let the keys have
      // position:static and display:inline-block
      var subKeys=document.createElement('DIV'),i;

      var tKey = this.getDefaultKeyObject();

      subKeys.id='kmw-popup-keys';
      this.popupBaseKey = e;

      // Does the popup array include the base key?   *** condition for phone only ***
      if(device.formFactor == 'phone') {
        this.prependBaseKey(e);
      }

      // Must set position dynamically, not in CSS
      var ss=subKeys.style;
      ss.bottom=(parseInt(e.style.bottom,10)+parseInt(e.style.height,10)+4)+'px';

      // Set key font according to layout, or defaulting to OSK font
      // (copied, not inherited, since OSK is not a parent of popup keys)
      ss.fontFamily=this.fontFamily;

      // Copy the font size from the parent key, allowing for style inheritance
      ss.fontSize=keyman.util.getStyleValue(e,'font-size');
      ss.visibility='hidden';

      var nKeys=subKeySpec.length,nRow,nRows,nCols;
      nRows=Math.min(Math.ceil(nKeys/9),2);
      nCols=Math.ceil(nKeys/nRows);
      if(nRows > 1) {
        ss.width=(nCols*e.offsetWidth+nCols*5)+'px';
      }

      // Add nested button elements for each sub-key
      for(i=0; i<nKeys; i++) {
        var needsTopMargin = false;
        let nRow=Math.floor(i/nCols);
        if(nRows > 1 && nRow > 0) {
          needsTopMargin = true;
        }

        let keyGenerator = new com.keyman.osk.OSKSubKey(subKeySpec[i], e['key'].layer);
        let kDiv = keyGenerator.construct(this, <HTMLDivElement> e, needsTopMargin);
        
        subKeys.appendChild(kDiv);
      }

      // Clear key preview if any
      this.showKeyTip(null,false);

      // Otherwise append the touch-hold (subkey) array to the OSK
      keyman.osk._Box.appendChild(subKeys);

      // And correct its position with respect to that element
      ss=subKeys.style;
      var x=dom.Utils.getAbsoluteX(e)+0.5*(e.offsetWidth-subKeys.offsetWidth), y,
        xMax=(util.landscapeView()?screen.height:screen.width)-subKeys.offsetWidth;

      if(x > xMax) {
        x=xMax;
      }
      if(x < 0) {
        x=0;
      }
      ss.left=x+'px';

      // Add the callout
      this.popupCallout = this.addCallout(e);

      // Make the popup keys visible
      ss.visibility='visible';

      // And add a filter to fade main keyboard
      subKeys.shim = document.createElement('DIV');
      subKeys.shim.id = 'kmw-popup-shim';
      keyman.osk._Box.appendChild(subKeys.shim);

      // Highlight the duplicated base key (if a phone)
      if(device.formFactor == 'phone') {
        var bk = <KeyElement> subKeys.childNodes[0].firstChild;
        this.keyPending = bk;
        this.highlightKey(bk,true);//bk.className = bk.className+' kmw-key-touched';
      }
    }

    /**
     * Prepend the base key to the touch-hold key array (for phones)
     *
     * @param {Object}  e   base key object
     */
    prependBaseKey(e: KeyElement) {
      // This is a tag we set on the key element during its construction.
      let subKeys: OSKKeySpec[] = e['subKeys'];
      let keyman = com.keyman.singleton;

      if(e && typeof(e.id) != 'undefined') {
        //TODO: refactor this, it's pretty messy...
        var i, 
          idx = e.id.split('-'), 
          baseId = e['keyId'],
          layer = e['key'].spec['layer'],
          sp = e['key'].spec['sp'],
          nextlayer = e['key'].spec['nextlayer'];

        if(typeof subKeys != 'undefined' && subKeys.length > 0 && (subKeys[0].id != baseId || subKeys[0].layer != layer)) {
          var eCopy = new OSKKeySpec(baseId, '', undefined, sp, nextlayer);  // {'id':baseId,'layer':'','key':undefined};
          if(layer != '') {
            eCopy['layer'] = layer;
          }

          for(i = 0; i < e.childNodes.length; i++) {
            if(keyman.util.hasClass(<HTMLElement> e.childNodes[i], 'kmw-key-text')) {
              break;
            }
          }
          if(i < e.childNodes.length) {
            eCopy['text'] = e.childNodes[i].textContent;
          }
          subKeys.splice(0, 0, eCopy);
        }
      }
    }
    //#endregion

    /**
     * Indicate the current language and keyboard on the space bar
     **/
    showLanguage() {
      let keyman = com.keyman.singleton;

      var lgName='',kbdName='';
      var activeStub = keyman.keyboardManager.activeStub;

      if(activeStub) {
        lgName=activeStub['KL'];
        kbdName=activeStub['KN'];
      } else if(keyman.getActiveLanguage(true)) {
        lgName=keyman.getActiveLanguage(true);
      } else {
        lgName='English';
      }

      try {
        var t=<HTMLElement> this.spaceBar.firstChild;
        let tParent = <HTMLElement> t.parentNode;
        if(typeof(tParent.className) == 'undefined' || tParent.className == '') {
          tParent.className='kmw-spacebar';
        } else if(tParent.className.indexOf('kmw-spacebar') == -1) {
          tParent.className +=' kmw-spacebar';
        }

        t.className='kmw-spacebar-caption';
        kbdName=kbdName.replace(/\s*keyboard\s*/i,'');

        // We use a separate variable here to keep down on MutationObserver messages in keymanweb.js code.
        var keyboardName = "";
        if(kbdName == lgName) {
          keyboardName=lgName;
        } else {
          keyboardName=lgName+' ('+kbdName+')';
        }
        // It sounds redundant, but this dramatically cuts down on browser DOM processing.
        if(t.innerHTML != keyboardName) {
          t.innerHTML = keyboardName;
        }
      }
      catch(ex){}
    }

    /**
     *  Add or remove a class from a keyboard key (when touched or clicked)
     *  or add a key preview for phone devices
     *
     *  @param    {Object}    key   key affected
     *  @param    {boolean}   on    add or remove highlighting
     **/
    highlightKey(key: KeyElement, on: boolean) {
      // Do not change element class unless a key
      if(!key || (key.className == '') || (key.className.indexOf('kmw-key-row') >= 0)) return;

      var classes=key.className, cs = ' kmw-key-touched';

      // For phones, use key preview rather than highlighting the key,
      // except for space, bksp, enter, shift and popup keys
      var usePreview = ((this.keytip != null)
        && (classes.indexOf('kmw-key-shift') < 0)
        && (classes.indexOf('kmw-spacebar') < 0)
        && (key.id.indexOf('popup') < 0 ));

      if(usePreview) {
        this.showKeyTip(key,on);
      } else {
        if(on && classes.indexOf(cs) < 0) {
          key.className=classes+cs;
          this.showKeyTip(null,false);     // Moved here by Serkan
        } else {
          key.className=classes.replace(cs,'');
        }
      }
    }

    //#region Mouse-event handling
    /**
     * Mouse down/mouse over event handler (desktop only)
     *
     * @param   {Event}  e  mouse over/mouse down event object
     */
    mouseOverMouseDownHandler = function(this: VisualKeyboard, e: MouseEvent) {
      let keyman = com.keyman.singleton;
      let util = keyman.util;

      var t = <HTMLElement> util.eventTarget(e);
      if(t === null || util.device.formFactor != 'desktop') {
        return;
      }

      if(t.nodeName == 'SPAN') {
        t = <HTMLElement> t.parentNode;
      }

      let key = this.keyTarget(t);

      if(util.eventType(e) == 'mousedown') {
        this.currentKey=key.id;
        util._CancelMouse(e);
        this.highlightKey(key, true);
      } else if(key.id == this.currentKey) {
        this.highlightKey(key, true);
      }
    }.bind(this);

    /**
     * Mouse up/mouse out event handler (desktop only)
     *
     * @param   {Event}  e  mouse up/out event object
     */
    mouseUpMouseOutHandler = function(this: VisualKeyboard, e: MouseEvent) {
      let keyman = com.keyman.singleton;
      let util = keyman.util;

      var t=<HTMLElement> util.eventTarget(e);
      if(t === null || util.device.formFactor != 'desktop') {
        return;
      }

      if(t.nodeName == 'SPAN') {
        t = <HTMLElement> t.parentNode;
      }

      let key = this.keyTarget(t);
      this.highlightKey(key, false);

      // Process as click if mouse button released anywhere over key
      if(util.eventType(e) == 'mouseup') {
        if(key.id == this.currentKey) {
          keyman.textProcessor.clickKey(key);
        }
        this.currentKey='';
      }
    }.bind(this);
    //#endregion

    getKeyEmFontSize() {
      let keyman = com.keyman.singleton;
      let util = keyman.util;

      if(util.device.formFactor == 'desktop') {
        let kbdFontSize = this.getFontSizeFromCookie();
        let keySquareScale = 0.8; // Set in kmwosk.css, is relative.
        return kbdFontSize * keySquareScale;
      } else {
        let emSizeStr = getComputedStyle(document.body).fontSize;
        let emSize = util.getFontSizeStyle(emSizeStr).val;
        let emScale = util.getFontSizeStyle(keyman.osk._Box).val;
        return emSize * emScale;
      }
    }

    getFontSizeFromCookie(): number {
      let keyman = com.keyman.singleton;
      let util = keyman.util;

      var c = util.loadCookie('KeymanWeb_OnScreenKeyboard');
      if(typeof(c) == 'undefined' || c == null) {
        return 16;
      }

      var newHeight=util.toNumber(c['height'],0.15*screen.height);
      if(newHeight > 0.5*screen.height) {
        newHeight=0.5*screen.height;
      }

      return (newHeight/8);
    }

        /**
     *  Set the reference to a special function key for the
     *  currently visible OSK layer
     *
     *  @param    {number}  nLayer  Index of visible layer
     *  @param    {string}  keyId   key identifier
     *  @return   {Object}          Reference to key
     */
    getSpecialKey(nLayer: number, keyId: string): KeyElement {
      let layers = this.kbdDiv.childNodes[0].childNodes;

      if(nLayer >= 0 && nLayer < layers.length) {
        // Special function keys will always be in bottom row (must modify code if not)
        let rows = layers[nLayer].childNodes;
        let keys = rows[rows.length-1].childNodes;
        for(var k=0; k<keys.length; k++) {
          let key = getKeyFrom(keys[k].firstChild);
          if(key && key['keyId'] == keyId) {
            return key;
          }
        }
      }
      return null;
    }

    show() {
      let device = com.keyman.singleton.util.device;
      var n,nLayer=-1, b = this.kbdDiv.childNodes[0].childNodes;

      for(n=0; n < b.length; n++) {
        let layerElement = <HTMLDivElement> b[n];
        if(layerElement['layer'] == this.layerId) {
          layerElement.style.display='block';
          //b[n].style.visibility='visible';
          this.nextLayer=this.layerId;
          this.layerIndex=nLayer=n;
          if(typeof this.layers[n]['nextlayer'] == 'string') {
            this.nextLayer=this.layers[n]['nextlayer'];
          }

          // If osk._Show has been called, there's probably been a change in modifier or state key state.  Keep it updated!
          this._UpdateVKShiftStyle();
        } else {
          layerElement.style.display='none';
          //layerElement.style.visibility='hidden';
        }
      }

      if(device.touchable) {
        // Identify and save references to the language key, hide keyboard key, and space bar
        this.lgKey=this.getSpecialKey(nLayer,'K_LOPT');     //TODO: should be saved with layer
        this.hkKey=this.getSpecialKey(nLayer,'K_ROPT');

        // Always adjust screen height if iPhone or iPod, to take account of viewport changes
        if(device.OS == 'iOS' && device.formFactor == 'phone') {
          this.adjustHeights();
        }
      }

      // Define for both desktop and touchable OSK
      this.spaceBar=this.getSpecialKey(nLayer,'K_SPACE'); //TODO: should be saved with layer
    }

    /**
     * Function     showLayer
     * Scope        Private
     * @param       {string}      id      ID of the layer to show
     * @return      {boolean}             true if the layer is shown, or false if it cannot be found
     * Description  Shows the layer identified by 'id' in the on screen keyboard
     */
    showLayer(id: string): boolean {
      let keyman = com.keyman.singleton;

      // Do not change layer unless needed (27/08/2015)
      if(id == this.layerId && keyman.util.device.formFactor != 'desktop') {
        // The layer's already shown, so report success.
        return true;
      }

      if(keyman.keyboardManager.activeKeyboard) {
        for(var i=0; i<this.layers.length; i++) {
          if(this.layers[i].id == id) {
            this.layerId=id;
            keyman.osk._Show();
            return true;
          }
        }
      }
      
      return false;
    }

    // /**
    //  * Function     _VKeyGetTarget
    //  * Scope        Private
    //  * @param       {Object}    e     OSK event
    //  * @return      {Object}          Target element for key in OSK
    //  * Description  Identify the OSK key clicked
    //  */
    // _VKeyGetTarget(e: Event) {
    //   var Ltarg;
    //   e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
    //   Ltarg = util.eventTarget(e);
    //   if (Ltarg == null) {
    //     return null;
    //   }
    //   if (Ltarg.nodeType == 3) { // defeat Safari bug
    //     Ltarg = Ltarg.parentNode;
    //   }
    //   if (Ltarg.tagName == 'SPAN') {
    //     Ltarg = Ltarg.parentNode;
    //   }
    //   return Ltarg;
    // }

    /**
     *  Append a style sheet for the current keyboard if needed for specifying an embedded font
     *  or to re-apply the default element font
     *
     **/
    appendStyleSheet() {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      var activeKeyboard = keymanweb.keyboardManager.activeKeyboard;
      var activeStub: com.keyman.KeyboardStub = keymanweb.keyboardManager.activeStub;

      // Do not do anything if a null stub
      if(activeStub == null) {
        return;
      }

      // First remove any existing keyboard style sheet
      if(this.styleSheet) {
        util.removeStyleSheet(this.styleSheet);
      }

      var i, kfd=activeStub['KFont'], ofd=activeStub['KOskFont'];

      // Add style sheets for embedded fonts if necessary (each font-face style will only be added once)
      util.addFontFaceStyleSheet(kfd);
      util.addFontFaceStyleSheet(ofd);

      // Temporarily hide duplicated elements on non-desktop browsers
      keymanweb.hideInputs();

      // Build the style string and append (or replace) the font style sheet
      // Note: Some browsers do not download the font-face font until it is applied,
      //       so must apply style before testing for font availability
      // Extended to allow keyboard-specific custom styles for Build 360
      var customStyle=this.addFontStyle(kfd,ofd);
      if( activeKeyboard != null && typeof(activeKeyboard['KCSS']) == 'string')  // KMEW-129
        customStyle=customStyle+activeKeyboard['KCSS'];

      this.styleSheet = util.addStyleSheet(customStyle); //Build 360

      // Wait until font is loaded then align duplicated input elements with page elements
      if(this.waitForFonts(kfd,ofd)) {
        keymanweb.alignInputs();
      }
    }

    /**
     *  Add or replace the style sheet used to set the font for input elements and OSK
     *
     *  @param  {Object}  kfd   KFont font descriptor
     *  @param  {Object}  ofd   OSK font descriptor (if any)
     *  @return {string}
     *
     **/
    addFontStyle(kfd, ofd): string {
      let keymanweb = com.keyman.singleton;

      // Get name of font to be applied
      var fn=keymanweb.baseFont;
      if(typeof(kfd) != 'undefined' && typeof(kfd['family']) != 'undefined') {
        fn=kfd['family'];
      }

      // Unquote font name in base font (if quoted)
      fn = fn.replace(/\u0022/g,'');

      // Set font family chain for mapped elements and remove any double quotes
      var rx=new RegExp('\\s?'+fn+',?'), ff=keymanweb.appliedFont.replace(/\u0022/g,'');

      // Remove base font name from chain if present
      ff = ff.replace(rx,'');
      ff = ff.replace(/,$/,'');

      // Then replace it at the head of the chain
      if(ff == '') {
        ff=fn;
      } else {
        ff=fn+','+ff;
      }

      // Re-insert quotes around individual font names
      ff = '"' + ff.replace(/\,\s?/g,'","') + '"';

      // Add to the stylesheet, quoted, and with !important to override any explicit style
      var s='.keymanweb-font{\nfont-family:' + ff + ' !important;\n}\n';

      // Set font family for OSK text
      if(typeof(ofd) != 'undefined') {
        s=s+'.kmw-key-text{\nfont-family:"'+ofd['family'].replace(/\u0022/g,'').replace(/,/g,'","')+'";\n}\n';
      } else if(typeof(kfd) != 'undefined') {
        s=s+'.kmw-key-text{\nfont-family:"'+kfd['family'].replace(/\u0022/g,'').replace(/,/g,'","')+'";\n}\n';
      }

      // Store the current font chain (with quote-delimited font names)
      keymanweb.appliedFont = ff;

      // Return the style string
      return s;
    }

    /**
     * Create copy of the OSK that can be used for embedding in documentation or help
     * The currently active keyboard will be returned if PInternalName is null
     *
     *  @param  {string}          PInternalName   internal name of keyboard, with or without Keyboard_ prefix
     *  @param  {number}          Pstatic         static keyboard flag  (unselectable elements)
     *  @param  {string=}         argFormFactor   layout form factor, defaulting to 'desktop'
     *  @param  {(string|number)=}  argLayerId    name or index of layer to show, defaulting to 'default'
     *  @return {Object}                          DIV object with filled keyboard layer content
     */
    buildDocumentationKeyboard(PInternalName,Pstatic,argFormFactor,argLayerId): HTMLElement { // I777
      let keymanweb = com.keyman.singleton;
      var PKbd=keymanweb.keyboardManager.activeKeyboard,Ln,
          formFactor=(typeof(argFormFactor) == 'undefined' ? 'desktop' : argFormFactor),
          layerId=(typeof(argLayerId) == 'undefined' ? 'default' : argLayerId);

      var keyboardsList = keymanweb.keyboardManager.keyboards;

      if(PInternalName != null) {
        var p=PInternalName.toLowerCase().replace('keyboard_','');

        for(Ln=0; Ln<keyboardsList.length; Ln++) {
          if(p == keyboardsList[Ln]['KI'].toLowerCase().replace('keyboard_','')) {
            PKbd=keyboardsList[Ln]; break;
          }
        }
      }

      if(!PKbd) {
        return null;
      }

      var layouts=PKbd['KVKL'], layout=null, PVK=PKbd['KV'];

      // Get the layout defined in the keyboard, or its nearest equivalent
      if(typeof layouts == 'object') {
        if(typeof(layouts[formFactor]) == 'object' && layouts[formFactor] != null) {
          layout=layouts[formFactor];
        } else if(formFactor == 'phone' && typeof(layouts['tablet']) == 'object' && layouts['tablet'] != null) {
          layout=layouts['tablet'];
        } else if(formFactor == 'tablet' && typeof(layouts['phone']) == 'object' && layouts['phone'] != null) {
          layout=layouts['phone'];
        } else if(typeof(layouts['desktop']) == 'object' && layouts['desktop'] != null) {
          layout=layouts['desktop'];
        }
      }

      // Else get a default layout for the device for this keyboard
      if(layout == null && PVK != null) {
        layout=Layouts.buildDefaultLayout(PVK,keymanweb.keyboardManager.getKeyboardModifierBitmask(PKbd),formFactor);
      }

      // Cannot create an OSK if no layout defined, just return empty DIV
      if(layout != null) {
        layout.keyLabels=((typeof(PKbd['KDU']) != 'undefined') && PKbd['KDU']);
      }

      // TODO:  Fix this method's link!
      let kbd=this.deviceDependentLayout(layout,formFactor);
      kbd.className=formFactor+'-static kmw-osk-inner-frame';

      // Select the layer to display, and adjust sizes
      if(layout != null) {
        var layer,row,key,Lr,Lk;
        for(Ln=0; Ln<layout.layer.length; Ln++) {
          layer=kbd.childNodes[Ln];
          for(Lr=0; Lr<layer.childNodes.length; Lr++) {
            row=layer.childNodes[Lr];
            for(Lk=0; Lk<row.childNodes.length; Lk++) {
              key=row.childNodes[Lk];
              key.style.height='100%';
            }
          }
          if(typeof(layerId) == 'number') {
            layer.style.display=(Ln == layerId && layerId >= 0 ? 'block' : 'none');
          } else if(typeof(layerId) == 'string') {
            layer.style.display=(layout.layer[Ln].id == layerId ? 'block' : 'none');
          } else {
            layer.style.display=(Ln == 0 ? 'block' : 'none');
          }
        }
      } else {
        kbd.innerHTML="<p style='color:#c40; font-size:0.5em;margin:10px;'>No "+formFactor+" layout is defined for "+PKbd['KN']+".</p>";
      }
      // Add a faint border
      kbd.style.border='1px solid #ccc';
      return kbd;
    }

    onHide() {
      // Remove highlighting from hide keyboard key, if applied
      if(this.hkKey) {
        this.highlightKey(this.hkKey,false);
      }
    }
  }
}