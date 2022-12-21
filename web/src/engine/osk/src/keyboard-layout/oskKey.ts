import { ActiveKey, ButtonClass, DeviceSpec, LayoutKey } from '@keymanapp/keyboard-processor';
// At present, we don't use @keymanapp/keyman.  Just `keyman`.  (Refer to <root>/web/package.json.)
import { getAbsoluteX, getAbsoluteY } from 'keyman/engine/dom-utils';

import { getFontSizeStyle } from '../fontSizeUtils.js';
import InputEventCoordinate from '../input/inputEventCoordinate.js';
import specialChars from '../specialCharacters.js';
import buttonClassNames from '../buttonClassNames.js';

import { KeyElement } from '../keyElement.js';
import VisualKeyboard from '../visualKeyboard.js';
import { getTextMetrics } from './getTextMetrics.js';

export class OSKKeySpec implements LayoutKey {
  id: string;

  // Only set (within @keymanapp/keyboard-processor) for keys actually specified in a loaded layout
  baseKeyID?: string;
  coreID?: string;
  elementID?: string;

  text?: string;
  sp?: ButtonClass;
  width: number;
  layer?: string; // The key will derive its base modifiers from this property - may not equal the layer on which it is displayed.
  nextlayer?: string;
  pad?: number;
  sk?: OSKKeySpec[];
  default?: boolean;

  constructor(id: string, text?: string, width?: number, sp?: ButtonClass, nextlayer?: string, pad?: number) {
    this.id = id;
    this.text = text;
    this.width = width ? width : 50;
    this.sp = sp;
    this.nextlayer = nextlayer;
    this.pad = pad;
  }
}

export default abstract class OSKKey {
  // Only set here to act as an alias for code built against legacy versions.
  static readonly specialCharacters = specialChars;

  static readonly BUTTON_CLASSES = buttonClassNames;

  static readonly HIGHLIGHT_CLASS = 'kmw-key-touched';
  readonly spec: OSKKeySpec;

  btn: KeyElement;
  label: HTMLSpanElement;
  square: HTMLDivElement;

  /**
   * The layer of the OSK on which the key is displayed.
   */
  readonly layer: string;

  constructor(spec: OSKKeySpec, layer: string) {
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
   * @param vkbd
   * @param style     specification for the desired base font size
   * @param override  if true, don't use the font spec from the button, just use the passed in spec
   * @returns         font size as a style string
   */
  getIdealFontSize(vkbd: VisualKeyboard, text: string, style: {height?: string, fontFamily?: string, fontSize: string}, override?: boolean): string {
    let buttonStyle = getComputedStyle(this.btn);
    let keyWidth = parseFloat(buttonStyle.width);
    let emScale = 1;

    const originalSize = getFontSizeStyle(style.fontSize || '1em');

    // Not yet available; it'll be handled in a later layout pass.
    if(!buttonStyle.fontSize) {
      // NOTE:  preserves old behavior for use in documentation keyboards, for now.
      // Once we no longer need to maintain this code block, we can drop all current
      // method parameters safely.
      //
      // Recompute the new width for use in autoscaling calculations below, just in case.
      emScale = vkbd.getKeyEmFontSize();
      keyWidth = this.getKeyWidth(vkbd);
    } else if(!override) {
      // When available, just use computedStyle instead.
      style = buttonStyle;
    }

    let fontSpec = getFontSizeStyle(style.fontSize || '1em');
    let metrics = getTextMetrics(text, emScale, style);

    const MAX_X_PROPORTION = 0.90;
    const MAX_Y_PROPORTION = 0.90;
    const X_PADDING = 2;
    const Y_PADDING = 2;

    var fontHeight: number, keyHeight: number;
    if(metrics.fontBoundingBoxAscent) {
      fontHeight = metrics.fontBoundingBoxAscent + metrics.fontBoundingBoxDescent;
    }

    let textHeight = fontHeight ? fontHeight + Y_PADDING : 0;
    if(style.height && style.height.indexOf('px') != -1) {
      keyHeight = Number.parseFloat(style.height.substring(0, style.height.indexOf('px')));
    }

    let xProportion = (keyWidth * MAX_X_PROPORTION) / (metrics.width + X_PADDING); // How much of the key does the text want to take?
    let yProportion = textHeight && keyHeight ? (keyHeight * MAX_Y_PROPORTION) / textHeight : undefined;

    var proportion: number = xProportion;
    if(yProportion && yProportion < xProportion) {
      proportion = yProportion;
    }

    // Never upscale keys past the default - only downscale them.
    // Proportion < 1:  ratio of key width to (padded [loosely speaking]) text width
    //                  maxProportion determines the 'padding' involved.
    //
    if(proportion < 1) {
      if(originalSize.absolute) {
        return proportion * fontSpec.val + 'px';
      } else {
        return proportion * originalSize.val + 'em';
      }
    } else {
      if(originalSize.absolute) {
        return fontSpec.val + 'px';
      } else {
        return originalSize.val + 'em';
      }
    }
  }

  getKeyWidth(vkbd: VisualKeyboard): number {
    let key = this.spec as ActiveKey;
    return key.proportionalWidth * vkbd.width;
  }

  /**
   * Replace default key names by special font codes for modifier keys
   *
   *  @param  {string}  oldText
   *  @return {string}
   **/
  protected renameSpecialKey(oldText: string, vkbd: VisualKeyboard): string {
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

  public get keyText(): string {
    const spec = this.spec;
    const DEFAULT_BLANK = '\xa0';

    // Add OSK key labels
    let keyText = null;
    if(spec['text'] == null || spec['text'] == '') {
      if(typeof spec['id'] == 'string') {
        // If the ID's Unicode-based, just use that code.
        keyText = ActiveKey.unicodeIDToText(spec['id']);
      }

      keyText = keyText || DEFAULT_BLANK;
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
    let specialText = this.renameSpecialKey(keyText, vkbd);
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
    let emScale = vkbd.getKeyEmFontSize();
    var width: number = getTextMetrics(keyText, emScale, styleSpec).width;
    if(width == 0 && keyText != '' && keyText != '\xa0') {
      // Add the Unicode 'empty circle' as a base support for needy diacritics.

      // Disabled by mcdurdin 2020-10-19; dotted circle display is inconsistent on iOS/Safari
      // at least and doesn't combine with diacritic marks. For consistent display, it may be
      // necessary to build a custom font that does not depend on renderer choices for base
      // mark display -- e.g. create marks with custom base included, potentially even on PUA
      // code points and use those in rendering the OSK. See #3039 for more details.
      // keyText = '\u25cc' + keyText;

      if(vkbd.isRTL) {
        // Add the RTL marker to ensure it displays properly.
        keyText = '\u200f' + keyText;
      }
    }

    ts.fontSize = this.getIdealFontSize(vkbd, keyText, styleSpec);

    // Finalize the key's text.
    t.innerText = keyText;

    return t;
  }

  public isUnderTouch(input: InputEventCoordinate): boolean {
    let x = input.x;
    let y = input.y;

    let btn = this.btn;
    let x0 = getAbsoluteX(btn);
    let y0 = getAbsoluteY(btn);
    let x1 = x0 + btn.offsetWidth;
    let y1 = y0 + btn.offsetHeight;

    return (x > x0 && x < x1 && y > y0 && y < y1);
  }

  public refreshLayout(vkbd: VisualKeyboard) {
    // space bar may not define the text span!
    if(this.label) {
      if(!this.label.classList.contains('kmw-spacebar-caption')) {
        this.label.style.fontSize = this.getIdealFontSize(vkbd, this.keyText, this.btn.style);
      } else {
        // Remove any custom setting placed on it before recomputing its inherited style info.
        this.label.style.fontSize = '';
        const fontSize = this.getIdealFontSize(vkbd, this.label.textContent, getComputedStyle(this.label), true);

        // Since the kmw-spacebar-caption version uses !important, we must specify
        // it directly on the element too; otherwise, scaling gets ignored.
        this.label.style.setProperty("font-size", fontSize, "important");
      }
    }
  }
}
