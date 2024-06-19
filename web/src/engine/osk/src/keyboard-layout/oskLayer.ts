import { ActiveLayer, ActiveLayout } from '@keymanapp/keyboard-processor';

import OSKRow from './oskRow.js';
import OSKBaseKey from './oskBaseKey.js';
import VisualKeyboard from '../visualKeyboard.js';
import { ParsedLengthStyle } from '../lengthStyle.js';

export interface LayerLayoutParams {
  keyboardWidth: number;
  keyboardHeight: number;
  widthStyle: ParsedLengthStyle;
  heightStyle: ParsedLengthStyle;
  baseEmFontSize: ParsedLengthStyle;
  layoutFontSize: ParsedLengthStyle;
  spacebarText: string;
}
export default class OSKLayer {
  public readonly element: HTMLDivElement;
  public readonly rows: OSKRow[];
  public readonly spec: ActiveLayer;
  public readonly nextlayer: string;

  public readonly globeKey:    OSKBaseKey;
  public readonly spaceBarKey: OSKBaseKey;
  public readonly hideKey:     OSKBaseKey;
  public readonly capsKey:     OSKBaseKey;
  public readonly numKey:      OSKBaseKey;
  public readonly scrollKey:   OSKBaseKey;

  private _rowHeight: number;

  public get rowHeight(): number {
    return this._rowHeight;
  }

  public get id(): string {
    return this.spec.id;
  }

  public constructor(vkbd: VisualKeyboard,
                      layout: ActiveLayout,
                      layer: ActiveLayer) {
    this.spec = layer;

    const gDiv = this.element = document.createElement('div');
    const gs=gDiv.style;
    gDiv.className='kmw-key-layer';

    var nRows=layer['row'].length;
    if(nRows > 4 && vkbd.device.formFactor == 'phone') {
      gDiv.className = gDiv.className + ' kmw-5rows';
    }

    // Set font for layer if defined in layout
    gs.fontFamily = 'font' in layout ? layout['font'] : '';

    this.nextlayer = layer.id;
    //@ts-ignore
    gDiv['layer'] = layer.id;
    // @ts-ignore
    if(typeof layer['nextlayer'] == 'string') {
      // The gDiv['nextLayer'] is no longer referenced in KMW 15.0+, but is
      // maintained for partial back-compat in case any site devs actually
      // relied on its value from prior versions.
      //
      // We won't pay attention to any mutations to the gDiv copy, though.
      // @ts-ignore
      gDiv['nextLayer'] = this.nextlayer = layer['nextlayer'];
    }

    // Create a DIV for each row of the group
    let rows=layer['row'];
    this.rows = [];

    for(let i=0; i<rows.length; i++) {
      let rowObj = new OSKRow(vkbd, layer, rows[i]);
      rowObj.displaysKeyCaps = layout["displayUnderlying"];
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

    if(this.spaceBarKey) {
      const spacebarLabel = this.spaceBarKey.label;
      let tButton = this.spaceBarKey.btn;

      if (typeof (tButton.className) == 'undefined' || tButton.className == '') {
        tButton.className = 'kmw-spacebar';
      } else if (tButton.className.indexOf('kmw-spacebar') == -1) {
        tButton.className += ' kmw-spacebar';
      }

      if (spacebarLabel.className != 'kmw-spacebar-caption') {
        spacebarLabel.className = 'kmw-spacebar-caption';
      }
    }
  }

  /**
   *  Find the OSKBaseKey representing the specified
   *  key ID for the currently visible OSK layer
   *
   *  @param    {string}  keyId   key identifier
   *  @return   {Object}          Reference to key
   */
  private findKey(keyId: string): OSKBaseKey {
    for(const row of this.rows) {
      for(const key of row.keys) {
        if(key.getBaseId() == keyId) {
          return key;
        }
      }
    }
    return null;
  }

  /**
   * Indicate the current language and keyboard on the space bar
   **/
  showLanguage(displayName: string) {
    if(!this.spaceBarKey) {
      return;
    }

    try {
      const spacebarLabel = this.spaceBarKey.label;

      // The key can read the text from here during the display update without
      // triggering a reflow.
      this.spaceBarKey.spec.text = displayName;

      // It sounds redundant, but this dramatically cuts down on browser DOM processing;
      // but sometimes innerText is reported empty when it actually isn't, so set it
      // anyway in that case (Safari, iOS 14.4)
      if (spacebarLabel.innerText != displayName || displayName == '') {
        spacebarLabel.innerText = displayName;
      }
    }
    catch (ex) { }
  }

  public refreshLayout(layoutParams: LayerLayoutParams) {
    // Do all layout-reflow / style-refresh dependent precalculations here,
    // before we perform any DOM manipulation.
    this.rows.forEach((row) => row.detectStyles(layoutParams));

    // Hereafter, avoid any references to getComputedStyle, offset_, or other
    // layout-reflow dependent values.  Refer to
    // https://gist.github.com/paulirish/5d52fb081b3570c81e3a.

    // Check the heights of each row, in case different layers have different row counts.
    const layerHeight = layoutParams.keyboardHeight;
    const nRows = this.rows.length;
    const rowHeight = this._rowHeight = Math.floor(layerHeight/(nRows == 0 ? 1 : nRows));

    const usesFixedWidthScaling = layoutParams.widthStyle.absolute;
    if(usesFixedWidthScaling) {
      this.element.style.height=(layerHeight)+'px';
    }

    this.showLanguage(layoutParams.spacebarText);

    // Update row layout properties
    for(let nRow=0; nRow<nRows; nRow++) {
      const oskRow = this.rows[nRow];
      const bottom = (nRows-nRow-1)*rowHeight+1;

      if(usesFixedWidthScaling) {
        // Calculate the exact vertical coordinate of the row's center.
        this.spec.row[nRow].proportionalY = ((layerHeight - bottom) - rowHeight/2) / layerHeight;
      }

      oskRow.refreshLayout(layoutParams);
      if(nRow == nRows-1) {
        oskRow.element.style.bottom = '1px';
      }
    }

    for(const row of this.rows) {
      row.refreshKeyLayouts(layoutParams);
    }
  }
}
