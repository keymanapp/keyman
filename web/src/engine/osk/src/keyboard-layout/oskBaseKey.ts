import { ActiveKey, Codes, DeviceSpec } from '@keymanapp/keyboard-processor';
import { landscapeView } from 'keyman/engine/dom-utils';

import OSKKey, { KeyLayoutParams, renameSpecialKey } from './oskKey.js';
import { KeyData, KeyElement, link } from '../keyElement.js';
import OSKRow from './oskRow.js';
import VisualKeyboard from '../visualKeyboard.js';
import { ParsedLengthStyle } from '../lengthStyle.js';
import { GesturePreviewHost } from './gesturePreviewHost.js';

export default class OSKBaseKey extends OSKKey {
  private capLabel: HTMLDivElement;
  private previewHost: GesturePreviewHost;
  private preview: HTMLDivElement;

  public readonly row: OSKRow;

  constructor(spec: ActiveKey, layer: string, row: OSKRow) {
    super(spec, layer);
    this.row = row;
  }

  getId(): string {
    // Define each key element id by layer id and key id (duplicate possible for SHIFT - does it matter?)
    return this.spec.elementID;
  }

  getCoreId(): string {
    return this.spec.coreID;
  }

  getBaseId(): string {
    return this.spec.baseKeyID;
  }

  // Produces a small reference label for the corresponding physical key on a US keyboard.
  private generateKeyCapLabel(): HTMLDivElement {
    // Create the default key cap labels (letter keys, etc.)
    var x = Codes.keyCodes[this.spec.baseKeyID];
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

    let q = document.createElement('div');
    q.className='kmw-key-label';
    if(x > 0) {
      q.innerText=String.fromCharCode(x);
    } else {
      // Keyman-only virtual keys have no corresponding physical key.
      // So, no text for the key-cap.
    }
    return q;
  }

  private processSubkeys(btn: KeyElement, vkbd: VisualKeyboard) {
    // Add reference to subkey array if defined
    var bsn: number, bsk=btn['subKeys'] = this.spec['sk'];
    // Transform any special keys into their PUA representations.
    for(bsn=0; bsn<bsk.length; bsn++) {
      if(bsk[bsn]['sp'] == 1 || bsk[bsn]['sp'] == 2) {
        var oldText=bsk[bsn]['text'];
        bsk[bsn]['text']=renameSpecialKey(oldText, vkbd);
      }

      // If a subkey doesn't have a defined layer property, copy it from the base key's layer by default.
      if(!bsk[bsn].layer) {
        bsk[bsn].layer = btn.key.layer
      }
    }
  }

  construct(vkbd: VisualKeyboard): HTMLDivElement {
    let spec = this.spec;

    let kDiv = document.createElement('div');
    kDiv.className='kmw-key-square';

    let btnEle = document.createElement('div');
    let btn = this.btn = link(btnEle, new KeyData(this, spec['id']));

    // Set button class
    this.setButtonClass();

    // Add the (US English) keycap label for layouts requesting display of underlying keys
    let keyCap = this.capLabel = this.generateKeyCapLabel();
    btn.appendChild(keyCap);

    // Define each key element id by layer id and key id (duplicate possible for SHIFT - does it matter?)
    btn.id=this.getId();

    // Make sure the key text is the element's first child - processSubkeys()
    // will add an extra element if subkeys exist, which can interfere with
    // keyboard/language name display on the space bar!
    btn.appendChild(this.label = this.generateKeyText(vkbd));

    // Handle subkey-related tasks.
    if(typeof(spec['sk']) != 'undefined' && spec['sk'] != null) {
      this.processSubkeys(btn, vkbd);
    } else {
      btn['subKeys']=null;
    }

    // If a subkey array is defined, add an icon
    const skIcon = this.generateHint(vkbd);
    btn.appendChild(skIcon);

    // Add text to button and button to placeholder div
    kDiv.appendChild(btn);

    this.preview = document.createElement('div');
    this.preview.style.display = 'none';
    btn.appendChild(this.preview);

    // The 'return value' of this process.
    return this.square = kDiv;
  }

  public generateHint(vkbd: VisualKeyboard): HTMLDivElement {
    // If a hint is defined, add an icon
    const skIcon = document.createElement('div');
    // Ensure that we use the keyboard's text font for hints.
    skIcon.className='kmw-key-popup-icon';

    const hintSpec = this.spec.hintSrc;
    if(!hintSpec) {
      return skIcon;
    }

    if(hintSpec.font && hintSpec.font != 'SpecialOSK') {
      skIcon.style.fontFamily = hintSpec.font;
    } else {
      skIcon.classList.add('kmw-key-text');
    }

    if(hintSpec.fontsize) {
      const parsed = new ParsedLengthStyle(hintSpec.fontsize);
      // From kmwosk.css: .kmw-key-popup-icon { font-size: 0.5em }
      // The spec says to overwrite that, but we still want half-size compared to the text
      // as a key-cap.
      skIcon.style.fontSize = parsed.scaledBy(0.5).styleString;
    }

    // If the base key itself is the source of the hint text, we use `hint` directly.
    // Otherwise, we present the source subkey's key cap as the hint.
    const baseText = hintSpec == this.spec ? this.spec.hint : hintSpec.text
    const text = renameSpecialKey(baseText, vkbd);
    if(text == '\u2022') {
      // The original, pre-17.0 longpress dot-hint used bold-face styling.
      skIcon.style.fontWeight='bold';
    }

    if(baseText != text) {
      // if the text is from a *Special* shorthand, always use our special-key OSK font.
      skIcon.style.fontFamily = 'SpecialOSK';
    }

    skIcon.textContent = text;

    return skIcon;
  }

  public setPreview(previewHost: GesturePreviewHost) {
    const oldPreview = this.preview;

    if(previewHost) {
      this.previewHost = previewHost;
      this.preview = this.previewHost.element;
    } else {
      this.previewHost = null;
      this.preview = document.createElement('div');
      this.preview.style.display = 'none';
    }

    previewHost?.setCancellationHandler(() => {
      this.setPreview(null);
    });

    this.btn.replaceChild(this.preview, oldPreview);
  }

  public refreshLayout(layoutParams: KeyLayoutParams) {
    super.refreshLayout(layoutParams);  // key labels in particular.

    const emFont = layoutParams.baseEmFontSize;
    // Rescale keycap labels on small phones
    if(emFont.val < 12) {
      this.capLabel.style.fontSize = '6px';
    } else {
      // The default value set within kmwosk.css.
      this.capLabel.style.fontSize = ParsedLengthStyle.forScalar(0.5).styleString;
    }
  }

  public get displaysKeyCap(): boolean {
    return this.capLabel && this.capLabel.style.display == 'block';
  }

  public set displaysKeyCap(flag: boolean) {
    if(!this.capLabel) {
      throw new Error("Key element not yet constructed; cannot display key cap");
    }
    this.capLabel.style.display = flag ? 'block' : 'none';
  }
}
