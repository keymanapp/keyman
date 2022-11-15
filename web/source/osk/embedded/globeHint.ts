///<reference path="singleInputCapture.ts" />

namespace com.keyman.osk.embedded {
  // Consider it a "specialized key tip".
  export class GlobeHint implements osk.GlobeHint {
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

    private inputTrap?: SingleInputCapture;

    constructor(vkbd: VisualKeyboard) {
      this.vkbd = vkbd;

      let tipElement = this.element=document.createElement('div');
      tipElement.className='kmw-globehint';
      tipElement.id = 'kmw-globehint';

      // The following style is critical, so do not rely on external CSS
      tipElement.style.pointerEvents='none';
      tipElement.style.display='none';

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
      let keyman = com.keyman.singleton;

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
        let oskBaseFontSize = new ParsedLengthStyle(keyman.osk.baseFontSize);
        let bubbleWidth = Math.ceil(xWidth * 3);

        // The base OSK font-size is typically set in 'em', and we want to scale up
        // the bubble accordingly for devices where it'd display too small otherwise.
        if(!oskBaseFontSize.absolute) {
          bubbleWidth *= oskBaseFontSize.val;
        }

        bubbleStyle.bottom = Math.floor(keyman.osk.computedHeight - y) + 'px';
        // CSS already defines transform: translateX(-50%) - this centers the element.
        bubbleStyle.left = center + 'px';

        this.tip.style.bottom = (rrow.height - 1) + 'px';
        this.tip.style.width = bubbleWidth + 'px';

        // Adjust shape if at edges

        // how much width lies outside the range of the base key, per side
        var xOverflow = (bubbleWidth - xWidth) / 2;

        let capWidth = xWidth * 2 / 3;

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
          this.element.style.fontSize = keyman.osk.baseFontSize;
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

    public show(key: KeyElement, onDismiss?: () => void) {
      this._show(key, true);

      if(onDismiss) {
        // enable input-capture system
        this.inputTrap = new SingleInputCapture(() => {
          this._show(key, false);
          onDismiss();
        });
      }
    }

    public hide(key: KeyElement) {
      this._show(key, false);

      // disable input-capture system
      if(this.inputTrap) {
        this.inputTrap.cancel();
        this.inputTrap = null;
      }
    }
  }
}