import OSKBaseKey from '../../../keyboard-layout/oskBaseKey.js';
import { KeyElement } from '../../../keyElement.js';
import KeyTipInterface from '../../../keytip.interface.js';
import VisualKeyboard from '../../../visualKeyboard.js';
import { GesturePreviewHost } from '../../../keyboard-layout/gesturePreviewHost.js';
import { ParsedLengthStyle } from '../../../lengthStyle.js';

const CSS_PREFIX = 'kmw-';
const DEFAULT_TIP_ORIENTATION: PhoneKeyTipOrientation = 'top';

export type PhoneKeyTipOrientation = 'top' | 'bottom';

export default class KeyTip implements KeyTipInterface {
  public readonly element: HTMLDivElement;
  public key: KeyElement;
  public state: boolean = false;

  private orientation: PhoneKeyTipOrientation = DEFAULT_TIP_ORIENTATION;

  //  -----
  // |     | <-- tip
  // |  x  | <-- preview
  // |_   _|
  //  |   |
  //  |   |  <-- cap
  //  |___|

  private readonly cap: HTMLDivElement;
  private readonly tip: HTMLDivElement;
  private previewHost: GesturePreviewHost;
  private preview: HTMLDivElement;
  private readonly vkbd: VisualKeyboard;

  private readonly constrain: boolean;
  private readonly reorient: (orientation: PhoneKeyTipOrientation) => void;

  /**
   *
   * @param constrain keep the keytip within the bounds of the overall OSK.
   *                  Will probably be handled via function in a later pass.
   */
  constructor(vkbd: VisualKeyboard, constrain: boolean) {
    this.vkbd = vkbd;
    let tipElement = this.element=document.createElement('div');
    tipElement.className='kmw-keytip';
    tipElement.id = 'kmw-keytip';

    // The following style is critical, so do not rely on external CSS
    tipElement.style.pointerEvents='none';
    tipElement.style.display='none';

    tipElement.appendChild(this.tip = document.createElement('div'));
    tipElement.appendChild(this.cap = document.createElement('div'));
    this.tip.appendChild(this.preview = document.createElement('div'));

    this.tip.className = 'kmw-keytip-tip';
    this.cap.className = 'kmw-keytip-cap';

    this.constrain = constrain;

    this.reorient = (orientation: PhoneKeyTipOrientation) => {
      this.orientation = orientation;
      this.show(this.key, this.state, this.previewHost);
    }
  }

  show(key: KeyElement, on: boolean, previewHost: GesturePreviewHost) {
    const vkbd = this.vkbd;

    // During quick input sequences - especially during a multitap-modipress - it's possible
    // for a user to request a preview for a key from a layer that is currently active, but
    // currently not visible due to need previously-requested layout calcs for a different layer.
    if(on) {
      // Necessary for `key.offsetParent` and client-rect methods referenced below.
      // Will not unnecessarily force reflow if the layer is already in proper document flow,
      // but otherwise restores it.
      vkbd.layerGroup.blinkLayer(key.key.spec.displayLayer);
    }

    // Create and display the preview
    // If !key.offsetParent, the OSK is probably hidden.  Either way, it's a half-
    // decent null-guard check.
    if(on && key.offsetParent) {
      // The key element is positioned relative to its key-square, which is,
      // in turn, relative to its row.  Rows take 100% width, so this is sufficient.
      //
      let rowElement = (key.key as OSKBaseKey).row.element;

      // May need adjustment for borders if ever enabled for the desktop form-factor target.
      let rkey = key.getClientRects()[0], rrow = rowElement.getClientRects()[0];
      let xLeft = rkey.left - rrow.left,
          xWidth = rkey.width,
          xHeight = rkey.height,
          kc = key.key.label,
          previewFontScale = 1.8;

      let kts = this.element.style;

      // Roughly matches how the subkey positioning is set.
      const _Box = vkbd.topContainer as HTMLDivElement;
      const _BoxRect = _Box.getBoundingClientRect();
      const keyRect = key.getBoundingClientRect();

      let y: number;
      const orientation = this.orientation;
      const distFromTop = keyRect.bottom - _BoxRect.top;
      y = (distFromTop + (orientation == 'top' ? 1 : -1));
      let ySubPixelPadding = y - Math.floor(y);

      // Canvas dimensions must be set explicitly to prevent clipping
      // This gives us exactly the same number of pixels on left and right
      let canvasWidth = xWidth + Math.ceil(xWidth * 0.3) * 2;
      let canvasHeight = Math.ceil(2.3 * xHeight) + (ySubPixelPadding); //

      if(orientation == 'bottom') {
        y += canvasHeight - xHeight;
      }

      kts.top = 'auto';
      const unselectedOrientation = orientation == 'top' ? 'bottom' : 'top';
      this.tip.classList.remove(`${CSS_PREFIX}${unselectedOrientation}`);
      this.tip.classList.add(`${CSS_PREFIX}${orientation}`);

      kts.bottom = Math.floor(_BoxRect.height - y) + 'px';
      kts.textAlign = 'center';
      kts.overflow = 'visible';
      kts.width = canvasWidth+'px';
      kts.height = canvasHeight+'px';

      // Some keyboards (such as `balochi_scientific`) do not _package_ a font but
      // specify an extremely common one, such as Arial.  In such cases, .kmw-key-text
      // custom styling doesn't exist, relying on the layer object to simply specify
      // the font-family.
      const layerFontFamily = this.vkbd.currentLayer.element.style.fontFamily;
      const ckts = getComputedStyle(vkbd.element);
      kts.fontFamily = key.key.spec.font || layerFontFamily || ckts.fontFamily;

      var px=parseInt(ckts.fontSize,10);
      if(px == Number.NaN) {
        px = 0;
      }

      if(px != 0) {
        let scaleStyle = {
          keyWidth: 1.6 * xWidth,
          keyHeight: 1.6 * xHeight, // as opposed to the canvas height of 2.3 * xHeight.
          baseEmFontSize: vkbd.getKeyEmFontSize(),
          layoutFontSize: new ParsedLengthStyle(vkbd.kbdDiv.style.fontSize)
        };

        kts.fontSize = key.key.getIdealFontSize(key.key.keyText, scaleStyle, previewFontScale).styleString;
      }

      // Adjust shape if at edges
      var xOverflow = (canvasWidth - xWidth) / 2;
      if(xLeft < xOverflow) {
        this.cap.style.left = '1px';
        xLeft += xOverflow - 1;
      } else if(xLeft > window.innerWidth - xWidth - xOverflow) {
        this.cap.style.left = (canvasWidth - xWidth - 1) + 'px';
        xLeft -= xOverflow - 1;
      } else {
        this.cap.style.left = xOverflow + 'px';
      }

      kts.left=(xLeft - xOverflow) + 'px';

      let cs = getComputedStyle(this.element);
      let oskHeight = _BoxRect.height;
      let bottomY = parseFloat(cs.bottom);
      let tipHeight = parseFloat(cs.height);
      let halfHeight = Math.ceil(canvasHeight / 2);

      this.cap.style.width = xWidth + 'px';
      this.tip.style.height = halfHeight + 'px';

      const capOffset = 3;
      const capStart = (halfHeight - capOffset) + 'px';
      if(orientation == 'top') {
        this.cap.style.top = capStart;
        this.cap.style.bottom = '';
      } else {
        this.cap.style.top = '';
        this.cap.style.bottom = capStart;
      }
      const defaultCapHeight = (distFromTop - Math.floor(y) + canvasHeight - (orientation == 'top' ? halfHeight : -capOffset * 2));
      this.cap.style.height = defaultCapHeight + 'px';

      if(this.constrain && tipHeight + bottomY > oskHeight) {
        const delta = tipHeight + bottomY - oskHeight;
        kts.height = (canvasHeight-delta) + 'px';
        const hx = Math.max(0, (canvasHeight-delta)-(canvasHeight/2) + 2);
        this.cap.style.height = hx + 'px';
      } else if(bottomY < 0) { // we'll assume that we always constrain at the OSK's bottom.
        kts.bottom = '0px';
        this.cap.style.height = Math.max(0, defaultCapHeight + bottomY) + 'px';
      }

      kts.display = 'block';

      if(this.previewHost == previewHost) {
        return;
      }

      const oldHost = this.preview;

      if(this.previewHost) {
        this.previewHost.off('preferredOrientation', this.reorient);
      }
      this.previewHost = previewHost;

      if(previewHost) {
        this.previewHost.on('preferredOrientation', this.reorient);
        this.preview = this.previewHost.element;
        this.tip.replaceChild(this.preview, oldHost);
        previewHost.setCancellationHandler(() => this.show(null, false, null));
        previewHost.on('startFade', () => {
          this.element.classList.remove('kmw-preview-fade');
          // Note:  a reflow is needed to reset the transition animation.
          this.element.offsetWidth;
          this.element.classList.add('kmw-preview-fade');
        });
      }
    } else { // Hide the key preview
      this.element.style.display = 'none';
      this.previewHost?.off('preferredOrientation', this.reorient);
      this.previewHost = null;
      const oldPreview = this.preview;
      this.preview = document.createElement('div');
      this.tip.replaceChild(this.preview, oldPreview);
      this.element.classList.remove('kmw-preview-fade');

      this.orientation = DEFAULT_TIP_ORIENTATION;
    }

    // Save the key preview state
    this.key = key;
    this.state = on;
  }
}