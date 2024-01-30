import { ActiveKey, ActiveSubKey, ButtonClass, DeviceSpec, LayoutKey } from '@keymanapp/keyboard-processor';
import { TouchLayout } from '@keymanapp/common-types';
import TouchLayoutFlick = TouchLayout.TouchLayoutFlick;

// At present, we don't use @keymanapp/keyman.  Just `keyman`.  (Refer to <root>/web/package.json.)
import { getAbsoluteX, getAbsoluteY } from 'keyman/engine/dom-utils';

import { getFontSizeStyle } from '../fontSizeUtils.js';
import specialChars from '../specialCharacters.js';
import buttonClassNames from '../buttonClassNames.js';

import { KeyElement } from '../keyElement.js';
import VisualKeyboard from '../visualKeyboard.js';
import { getTextMetrics } from './getTextMetrics.js';

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
   * @param vkbd
   * @param text
   * @param style     specification for the desired base font size
   * @param override  if true, don't use the font spec from the button, just use the passed in spec
   * @returns         font size as a style string
   */
  getIdealFontSize(vkbd: VisualKeyboard, text: string, style: {height?: string, fontFamily?: string, fontSize: string}, override?: boolean): string {
    let buttonStyle: typeof style & {width?: string} = getComputedStyle(this.btn);
    let keyWidth = parseFloat(buttonStyle.width);
    let emScale = vkbd.getKeyEmFontSize();

    // Among other things, ensures we use SpecialOSK styling for special key text.
    // It's set on the key-span, not on the button.
    const capFont = !this.label ? undefined: getComputedStyle(this.label).fontFamily;
    if(capFont) {
      buttonStyle = {
        fontFamily: capFont,
        fontSize: buttonStyle.fontSize,
        height: buttonStyle.height
      }
    }

    const originalSize = getFontSizeStyle(style.fontSize || '1em');

    // Not yet available; it'll be handled in a later layout pass.
    if(!override) {
      // When available, just use computedStyle instead.
      style = buttonStyle;
    }

    let fontSpec = getFontSizeStyle(style.fontSize || '1em');
    let metrics = getTextMetrics(text, emScale, style);

    const MAX_X_PROPORTION = 0.90;
    const MAX_Y_PROPORTION = 0.90;
    const X_PADDING = 2;

    var fontHeight: number, keyHeight: number;
    if(metrics.fontBoundingBoxAscent) {
      fontHeight = metrics.fontBoundingBoxAscent + metrics.fontBoundingBoxDescent;
    }

    // Don't add extra padding to height - multiplying with MAX_Y_PROPORTION already gives
    // padding
    let textHeight = fontHeight ?? 0;
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

  public refreshLayout(vkbd: VisualKeyboard) {
    // space bar may not define the text span!
    if(this.label) {
      if(!this.label.classList.contains('kmw-spacebar-caption')) {
        // Do not use `this.keyText` - it holds *___* codes for special keys, not the actual glyph!
        const keyCapText = this.label.textContent;
        this.label.style.fontSize = this.getIdealFontSize(vkbd, keyCapText, this.btn.style);
      } else {
        // Remove any custom setting placed on it before recomputing its inherited style info.
        this.label.style.fontSize = '';
        const fontSize = this.getIdealFontSize(vkbd, this.label.textContent, this.btn.style);

        // Since the kmw-spacebar-caption version uses !important, we must specify
        // it directly on the element too; otherwise, scaling gets ignored.
        this.label.style.setProperty("font-size", fontSize, "important");
      }
    }
  }
}
