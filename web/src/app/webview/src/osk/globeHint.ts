import {
  type GlobeHint as GlobeHintInterface,
  type KeyElement,
  type OSKBaseKey,
  ParsedLengthStyle,
  type VisualKeyboard
} from "keyman/engine/osk";

import { SingleInputCapture } from "./singleInputCapture.js";

/**
 * The GlobeHint is a specialized key tip that is presented above
 * the globe key on first use of Keyman, to show the user how to
 * switch input methods. The label is localizable text.
 */
export class GlobeHint implements GlobeHintInterface {
  public readonly element: HTMLDivElement;
  public state: boolean = false;

  //  -----
  // |     | <-- tip
  // |  x  | <-- label
  // |_   _|
  //   \ /  <-- cap
  //    v

  private readonly cap: HTMLDivElement;
  private readonly tip: HTMLDivElement;
  private readonly label: HTMLSpanElement;

  private readonly vkbd: VisualKeyboard;

  private docInputCatch?: SingleInputCapture;
  private tipInputTrap?: SingleInputCapture;

  private delayedDismissTimerHandle: number;

  constructor(vkbd: VisualKeyboard) {
    this.vkbd = vkbd;

    let tipElement = this.element=document.createElement('div');
    tipElement.className = 'kmw-globehint';
    tipElement.id = 'kmw-globehint';

    // The following style is critical, so do not rely on external CSS
    tipElement.style.pointerEvents = 'none';
    tipElement.style.display = 'none';

    tipElement.appendChild(this.tip = document.createElement('div'));
    tipElement.appendChild(this.cap = document.createElement('div'));
    this.tip.appendChild(this.label = document.createElement('span'));

    this.tip.className = 'kmw-globehint-tip';
    this.cap.className = 'kmw-globehint-cap';
    this.label.className = 'kmw-globehint-label';

    this.text = "Tap here to change keyboard.";
  }

  get text(): string {
    return this.label.innerText;
  }

  set text(val: string) {
    this.label.innerText = val;
  }

  private _show(key: KeyElement, on: boolean) {
    //let keyman = com.keyman.singleton;

    // Create and display the preview
    // If !key.offsetParent, the OSK is probably hidden.  Either way, it's a half-
    // decent null-guard check.
    if(!on || !key.offsetParent) {
      this.element.style.display = 'none';
    } else {
      // The key element is positioned relative to its key-square, which is,
      // in turn, relative to its row.  Rows take 100% width, so this is sufficient.
      //
      let rowElement = (key.key as OSKBaseKey).row.element;

      // May need adjustment for borders if ever enabled for the desktop form-factor target.
      let rkey = key.getClientRects()[0], rrow = rowElement.getClientRects()[0];
      let xLeft = rkey.left - rrow.left,
          xWidth = rkey.width,
          xHeight = rkey.height;

      let center = rkey.left + rkey.width/2;

      let bubbleStyle = this.element.style;

      // Roughly matches how the subkey positioning is set.
      const _Box = this.vkbd.element.parentNode as HTMLDivElement;
      const _BoxRect = _Box.getBoundingClientRect();
      const keyRect = key.getBoundingClientRect();
      let y = (keyRect.bottom - _BoxRect.top + 1);

      // Width dimensions must be set explicitly to prevent clipping.
      // We'll assume that the globe key is always positioned on the bottom row.
      let oskBaseFontSize = new ParsedLengthStyle(this.vkbd.fontSize.styleString);
      let bubbleWidth = Math.ceil(xWidth * 3);

      bubbleStyle.bottom = Math.floor(this.vkbd.height - y) + 'px';
      // CSS already defines transform: translateX(-50%) - this centers the element.
      bubbleStyle.left = center + 'px';

      this.tip.style.bottom = (rrow.height - 1) + 'px';
      this.tip.style.width = bubbleWidth + 'px';
      this.tip.style.pointerEvents = on ? 'auto' : 'none';

      // Adjust shape if at edges

      // how much width lies outside the range of the base key, per side
      const xOverflow = (bubbleWidth - xWidth) / 2;

      const capWidth = xWidth * 2 / 3;

      const leftEdgeMargin  = xLeft;
      const rightEdgeMargin = window.innerWidth - (xLeft + xWidth);

      if(leftEdgeMargin < xOverflow) {
        // The overflow would be clipped by the left edge when centered,
        // so we "offset" this part to keep it within screen bounds.
        this.tip.style.transform = 'translateX(' + (xOverflow - leftEdgeMargin + 1) + 'px)';
      } else if(rightEdgeMargin < xOverflow) {
        // The overflow would be clipped by the right edge when centered,
        // so we "offset" this part to keep it within screen bounds.
        this.tip.style.transform = 'translateX(-' + (xOverflow - rightEdgeMargin + 1) + 'px)';
      }

      if(!oskBaseFontSize.absolute) {
        this.element.style.fontSize = this.vkbd.fontSize.styleString;
      }

      let capHeight = xHeight / 3;

      this.cap.style.bottom = (rrow.height - capHeight) + 'px';
      this.cap.style.width = '0px';
      this.cap.style.height = '0px';
      this.cap.style.borderLeftWidth   = (capWidth / 2) + 'px';
      this.cap.style.borderRightWidth  = (capWidth / 2) + 'px';
      this.cap.style.borderTopWidth    = capHeight + 'px';

      bubbleStyle.display = 'block';
    }

    // Save the key preview state
    this.state = on;
  }

  private clearCatchers() {
    // disable input-capture system
    if(this.docInputCatch) {
      this.docInputCatch.cancel();
      this.docInputCatch = null;
    }

    if(this.tipInputTrap) {
      this.tipInputTrap.cancel();
      this.tipInputTrap = null;
    }
  }

  public show(key: KeyElement, onDismiss?: () => void) {
    this._show(key, true);

    const captureHandler = () => {
      // Prevent duplicated dismissal calls that'd otherwise result when the tip itself receives a touch.
      window.clearTimeout(this.delayedDismissTimerHandle);
      this.hide(key);
      onDismiss();
    };

    if(onDismiss) {
      // enable input-capture system
      this.tipInputTrap = new SingleInputCapture(this.tip, true, captureHandler);
      this.docInputCatch = new SingleInputCapture(document.body, false, () => {
        this.delayedDismissTimerHandle = window.setTimeout(() => {
          captureHandler();
        }, 1); // we need to delay processing so that events directly on the tip get priority first!
      });
    } else {
      // disable any previously-set input-captures as the incoming
      // (thus, most-recent) call didn't request one.
      this.clearCatchers();
    }
  }

  public hide(key: KeyElement) {
    this._show(key, false);

    this.clearCatchers();
  }
}