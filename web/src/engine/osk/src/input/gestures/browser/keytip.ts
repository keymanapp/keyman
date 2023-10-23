import OSKBaseKey from '../../../keyboard-layout/oskBaseKey.js';
import { KeyElement } from '../../../keyElement.js';
import KeyTipInterface from '../../../keytip.interface.js';
import VisualKeyboard from '../../../visualKeyboard.js';
import { GesturePreviewHost } from '../../../keyboard-layout/gesturePreviewHost.js';

export default class KeyTip implements KeyTipInterface {
  public readonly element: HTMLDivElement;
  public key: KeyElement;
  public state: boolean = false;

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

  private readonly constrain: boolean;

  /**
   *
   * @param constrain keep the keytip within the bounds of the overall OSK.
   *                  Will probably be handled via function in a later pass.
   */
  constructor(constrain: boolean) {
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
  }

  show(key: KeyElement, on: boolean, vkbd: VisualKeyboard) {
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
      let y = (keyRect.bottom - _BoxRect.top + 1);
      let ySubPixelPadding = y - Math.floor(y);

      // Canvas dimensions must be set explicitly to prevent clipping
      // This gives us exactly the same number of pixels on left and right
      let canvasWidth = xWidth + Math.ceil(xWidth * 0.3) * 2;
      let canvasHeight = Math.ceil(2.3 * xHeight) + (ySubPixelPadding); //

      kts.top = 'auto';
      kts.bottom = Math.floor(_BoxRect.height - y) + 'px';
      kts.textAlign = 'center';
      kts.overflow = 'visible';
      kts.width = canvasWidth+'px';
      kts.height = canvasHeight+'px';

      const ckts = getComputedStyle(vkbd.element);
      kts.fontFamily = ckts.fontFamily;

      var px=parseInt(ckts.fontSize,10);
      if(px == Number.NaN) {
        px = 0;
      }

      if(px != 0) {
        let popupFS = previewFontScale * px;
        let scaleStyle = {
          fontFamily: kts.fontFamily,
          fontSize: popupFS + 'px',
          height: 1.6 * xHeight + 'px' // as opposed to the canvas height of 2.3 * xHeight.
        };

        kts.fontSize = key.key.getIdealFontSize(vkbd, key.key.keyText, scaleStyle, true);
      }

      const oldHost = this.preview;
      this.previewHost = new GesturePreviewHost(key, true);
      this.preview = this.previewHost.element;
      this.tip.replaceChild(this.preview, oldHost);

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

      this.cap.style.top = (halfHeight - 3) + 'px';
      this.cap.style.height = (keyRect.bottom - _BoxRect.top - Math.floor(y - canvasHeight) - (halfHeight)) + 'px'; //(halfHeight + 3 + ySubPixelPadding) + 'px';

      if(this.constrain && tipHeight + bottomY > oskHeight) {
        const delta = tipHeight + bottomY - oskHeight;
        kts.height = (canvasHeight-delta) + 'px';
        const hx = Math.max(0, (canvasHeight-delta)-(canvasHeight/2) + 2);
        this.cap.style.height = hx + 'px';
      }

      kts.display = 'block';
    } else { // Hide the key preview
      this.element.style.display = 'none';
      this.previewHost = null;
      const oldPreview = this.preview;
      this.preview = document.createElement('div');
      this.tip.replaceChild(this.preview, oldPreview);
    }

    // Save the key preview state
    this.key = key;
    this.state = on;
  }
}