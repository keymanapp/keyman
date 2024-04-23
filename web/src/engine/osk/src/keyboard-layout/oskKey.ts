import { ActiveKey, ActiveSubKey, ButtonClass, ButtonClasses, DeviceSpec } from '@keymanapp/keyboard-processor';

// At present, we don't use @keymanapp/keyman.  Just `keyman`.  (Refer to <root>/web/package.json.)
import specialChars from '../specialCharacters.js';
import buttonClassNames from '../buttonClassNames.js';

import { KeyElement } from '../keyElement.js';
import VisualKeyboard from '../visualKeyboard.js';
import { getTextMetrics } from './getTextMetrics.js';
import { ParsedLengthStyle } from '../lengthStyle.js';

export interface KeyLayoutParams {
  keyWidth: number;
  keyHeight: number;
  baseEmFontSize: ParsedLengthStyle;
  layoutFontSize: ParsedLengthStyle;
}

/**
 * Replace default key names by special font codes for modifier keys
 *
 *  @param  {string}  oldText
 *  @return {string}
 **/
export function renameSpecialKey(oldText: string, vkbd: VisualKeyboard): string {
 // If a 'special key' mapping exists for the text, replace it with its corresponding special OSK character.
 switch(oldText) {
   case '*ZWNJ*':
     // Default ZWNJ symbol comes from iOS.  We'd rather match the system defaults where
     // possible / available though, and there's a different standard symbol on Android.
     oldText = vkbd.device.OS == DeviceSpec.OperatingSystem.Android ?
       '*ZWNJAndroid*' :
       '*ZWNJiOS*';
     break;
   case '*Enter*':
     oldText = vkbd.isRTL ? '*RTLEnter*' : '*LTREnter*';
     break;
   case '*BkSp*':
     oldText = vkbd.isRTL ? '*RTLBkSp*' : '*LTRBkSp*';
     break;
   default:
     // do nothing.
 }

 let specialCodePUA = 0XE000 + specialChars[oldText];

 return specialChars[oldText] ?
   String.fromCharCode(specialCodePUA) :
   oldText;
}

export default abstract class OSKKey {
  // Only set here to act as an alias for code built against legacy versions.
  static readonly specialCharacters = specialChars;

  static readonly BUTTON_CLASSES = buttonClassNames;

  static readonly HIGHLIGHT_CLASS = 'kmw-key-touched';
  readonly spec: ActiveKey | ActiveSubKey;

  btn: KeyElement;
  label: HTMLSpanElement;
  square: HTMLDivElement;

  private _fontSize: ParsedLengthStyle;
  private _fontFamily: string;

  /**
   * The layer of the OSK on which the key is displayed.
   */
  readonly layer: string;

  constructor(spec: ActiveKey | ActiveSubKey, layer: string) {
    this.spec = spec;
    this.layer = layer;
  }

  abstract getId(): string;

  /**
   * Attach appropriate class to each key button, according to the layout
   *
   * @param       {Object=}   layout  source layout description (optional, sometimes)
   */
  public setButtonClass() {
    let key = this.spec;
    let btn = this.btn;

    var n=0;
    if(typeof key['dk'] == 'string' && key['dk'] == '1') {
      n=8;
    }

    n = key['sp'] ?? n;

    if(n < 0 || n > 10) {
      n=0;
    }

    btn.className='kmw-key kmw-key-'+ buttonClassNames[n];
  }

  /**
   * For keys with button classes that support toggle states, this method
   * may be used to toggle which state the key's button class is in.
   * -  shift  <=>  shift-on
   * - special <=> special-on
   * @param {boolean=} flag The new toggle state
   */
  public setToggleState(flag?: boolean) {
    let btnClassId: number;

    btnClassId = this.spec['sp'];

    // 1 + 2:   shift  +  shift-on
    // 3 + 4:  special + special-on
    switch(buttonClassNames[btnClassId]) {
      case 'shift':
      case 'shift-on':
        if(flag === undefined) {
          flag = buttonClassNames[btnClassId] == 'shift';
        }

        this.spec['sp'] = 1 + (flag ? 1 : 0) as ButtonClass;
        break;
      // Added in 15.0:  special key highlight toggling.
      // Was _intended_ in earlier versions, but not actually implemented.
      case 'special':
      case 'special-on':
        if(flag === undefined) {
          flag = buttonClassNames[btnClassId] == 'special';
        }

        this.spec['sp'] = 3 + (flag ? 1 : 0) as ButtonClass;
        break;
      default:
        return;
    }

    this.setButtonClass();
  }

  // "Frame key" - generally refers to non-linguistic keys on the keyboard
  public isFrameKey(): boolean {
    let classIndex = this.spec['sp'] || 0;
    switch(buttonClassNames[classIndex]) {
      case 'default':
      case 'deadkey':
        // Note:  will (generally) include the spacebar.
        return false;
      default:
        return true;
    }
  }

  public allowsKeyTip(): boolean {
    if(this.isFrameKey()) {
      return false;
    } else {
      return !this.btn.classList.contains('kmw-spacebar');
    }
  }

  public highlight(on: boolean) {
    var classes=this.btn.classList;

    if(on) {
      if(!classes.contains(OSKKey.HIGHLIGHT_CLASS)) {
        classes.add(OSKKey.HIGHLIGHT_CLASS);
      }
    } else {
      classes.remove(OSKKey.HIGHLIGHT_CLASS);
    }
  }

  /**
   * Calculate the font size required for a key cap, scaling to fit longer text
   * @param text
   * @param layoutParams   specification for the key
   * @param scale     additional scaling to apply for the font-size calculation (used by keytips)
   * @returns         font size as a style string
   */
  getIdealFontSize(text: string, layoutParams: KeyLayoutParams, scale?: number): ParsedLengthStyle {
    // Fallback in case not all style info is currently ready.
    if(!this._fontFamily) {
      return new ParsedLengthStyle('1em');
    }

    scale ??= 1;

    const keyWidth = layoutParams.keyWidth;
    const keyHeight = layoutParams.keyHeight;
    const emScale = layoutParams.baseEmFontSize.scaledBy(layoutParams.layoutFontSize.val);

    // Among other things, ensures we use SpecialOSK styling for special key text.
    // It's set on the key-span, not on the button.
    //
    // Also helps ensure that the stub's font-family name is used for keys, should
    // that mismatch the font-family name specified within the keyboard's touch layout.

    let originalSize = this._fontSize;
    if(!originalSize.absolute) {
      originalSize = emScale.scaledBy(originalSize.val);
    }

    const style = {
      fontFamily: this._fontFamily,
      fontSize: originalSize.styleString,
      height: layoutParams.keyHeight
    }

    let metrics = getTextMetrics(text, emScale.scaledBy(scale).val, style);

    const MAX_X_PROPORTION = 0.90;
    const MAX_Y_PROPORTION = 0.90;
    const X_PADDING = 2;

    var fontHeight: number;
    if(metrics.fontBoundingBoxAscent) {
      fontHeight = metrics.fontBoundingBoxAscent + metrics.fontBoundingBoxDescent;
    }

    // Don't add extra padding to height - multiplying with MAX_Y_PROPORTION already gives
    // padding
    let textHeight = fontHeight ?? 0;
    let xProportion = (keyWidth * MAX_X_PROPORTION) / (metrics.width + X_PADDING); // How much of the key does the text want to take?
    let yProportion = textHeight && keyHeight ? (keyHeight * MAX_Y_PROPORTION) / textHeight : undefined;

    var proportion: number = xProportion;
    if(yProportion && yProportion < xProportion) {
      proportion = yProportion;
    }

    // Never upscale keys past the default * the specified scale - only downscale them.
    // Proportion < 1:  ratio of key width to (padded [loosely speaking]) text width
    //                  maxProportion determines the 'padding' involved.
    return ParsedLengthStyle.forScalar(scale * Math.min(proportion, 1));
  }

  public get keyText(): string {
    const spec = this.spec;
    const DEFAULT_BLANK = '\xa0';

    // Add OSK key labels
    let keyText = null;
    if(spec['text'] == null || spec['text'] == '') {
      // U_ codes are handled during keyboard pre-processing.
      keyText = DEFAULT_BLANK;
    } else {
      keyText=spec['text'];

      // Unique layer-based transformation:  SHIFT-TAB uses a different glyph.
      if(keyText == '*Tab*' && this.layer == 'shift') {
        keyText = '*TabLeft*';
      }
    }

    return keyText;
  }

  // Produces a HTMLSpanElement with the key's actual text.
  protected generateKeyText(vkbd: VisualKeyboard): HTMLSpanElement {
    const spec = this.spec;

    let t = document.createElement('span'), ts=t.style;
    t.className='kmw-key-text';

    // Add OSK key labels
    let keyText = this.keyText;
    let specialText = renameSpecialKey(keyText, vkbd);
    if(specialText != keyText) {
      // The keyboard wants to use the code for a special glyph defined by the SpecialOSK font.
      keyText = specialText;
      spec['font'] = "SpecialOSK";
    }

    //Override font spec if set for this key in the layout
    if(typeof spec['font'] == 'string' && spec['font'] != '') {
      ts.fontFamily=spec['font'];
    }

    if(typeof spec['fontsize'] == 'string' && spec['fontsize'] != '') {
      ts.fontSize=spec['fontsize'];
    }

    // For some reason, fonts will sometimes 'bug out' for the embedded iOS page if we
    // instead assign fontFamily to the existing style 'ts'.  (Occurs in iOS 12.)
    let styleSpec: {fontFamily?: string, fontSize: string} = {fontSize: ts.fontSize};

    if(ts.fontFamily) {
      styleSpec.fontFamily = ts.fontFamily;
    } else {
      styleSpec.fontFamily = vkbd.fontFamily; // Helps with style sheet calculations.
    }

    // Check the key's display width - does the key visualize well?
    if(vkbd.isRTL) {
      // Add the RTL marker to ensure it displays properly.
      keyText = '\u200f' + keyText;
    }

    // Finalize the key's text.
    t.innerText = keyText;

    return t;
  }

  public resetFontPrecalc() {
    this._fontFamily = undefined;
    this._fontSize = undefined;
    this.label.style.fontSize = '';
  }

  /**
   * Any style-caching behavior needed for use in layout manipulation should be
   * computed within this method, not within refreshLayout.  This is to prevent
   * unnecessary layout-reflow.
   * @param layoutParams
   * @returns
   */
  public detectStyles(layoutParams: KeyLayoutParams): void {
    // Avoid doing any font-size related calculations if there's no text to display.
    if(this.spec.sp == ButtonClasses.spacer || this.spec.sp == ButtonClasses.blank) {
      return;
    }

    // Attempt to detect static but key-specific style properties if they haven't yet
    // been detected.
    if(this._fontFamily === undefined) {
      const lblStyle = getComputedStyle(this.label);

      // Abort if the element is not currently in the DOM; we can't get any info this way.
      if(!lblStyle.fontFamily) {
        return;
      }
      this._fontFamily = lblStyle.fontFamily;

      // Detect any difference in base (em) font size and that which is computed for the key itself.
      const computedFontSize = new ParsedLengthStyle(lblStyle.fontSize);
      const layoutFontSize = layoutParams.layoutFontSize;
      if(layoutFontSize.absolute) {
        // rather than just straight-up taking .layoutFontSize
        this._fontSize = computedFontSize;
      } else {
        const baseEmFontSize = layoutParams.baseEmFontSize;
        const baseFontSize = layoutFontSize.scaledBy(baseEmFontSize.val);
        const localFontScaling = computedFontSize.val / baseFontSize.val;
        this._fontSize = ParsedLengthStyle.forScalar(localFontScaling);
      }
    }
  }

  // Avoid any references to getComputedStyle, offset_, or other layout-reflow
  // dependent values.  Refer to https://gist.github.com/paulirish/5d52fb081b3570c81e3a.
  public refreshLayout(layoutParams: KeyLayoutParams) {
    // space bar may not define the text span!
    if(this.label) {
      if(!this.label.classList.contains('kmw-spacebar-caption')) {
        // Do not use `this.keyText` - it holds *___* codes for special keys, not the actual glyph!
        const keyCapText = this.label.textContent;
        const fontSize = this.getIdealFontSize(keyCapText, layoutParams);
        this.label.style.fontSize = fontSize.styleString;
      } else {
        // Spacebar text, on the other hand, is available via this.keyText.
        // Using this field helps prevent layout reflow during updates.
        const fontSize = this.getIdealFontSize(this.keyText, layoutParams);

        // Since the kmw-spacebar-caption version uses !important, we must specify
        // it directly on the element too; otherwise, scaling gets ignored.
        this.label.style.setProperty("font-size", fontSize.styleString, "important");
      }
    }
  }
}
