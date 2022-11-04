namespace com.keyman.osk.embedded {
  export class GlobeHint {
    public readonly element: HTMLDivElement;
    public key: KeyElement;
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

    constructor() {
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

      this.label.innerText = "Tap here to change keyboard.";
    }

    show(key: KeyElement, on: boolean, vkbd: VisualKeyboard) {
      let keyman = com.keyman.singleton;

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
            xHeight = rkey.height;

        let kts = this.element.style;

        // Roughly matches how the subkey positioning is set.
        const _Box = vkbd.element.parentNode as HTMLDivElement;
        const _BoxRect = _Box.getBoundingClientRect();
        const keyRect = key.getBoundingClientRect();
        let y = (keyRect.bottom - _BoxRect.top + 1);
        let ySubPixelPadding = y - Math.floor(y);

        // Canvas dimensions must be set explicitly to prevent clipping
        // This gives us exactly the same number of pixels on left and right
        let canvasWidth = xWidth + Math.ceil(xWidth * 1) * 2;
        let canvasHeight = Math.ceil(3.0 * xHeight) + (ySubPixelPadding); //

        kts.top = 'auto';
        kts.bottom = Math.floor(keyman.osk.computedHeight - y) + 'px';
        kts.textAlign = 'center';
        kts.overflow = 'visible';
        kts.width = canvasWidth+'px';
        kts.height = canvasHeight+'px';

        // Adjust shape if at edges

        // how much width lies outside the range of the base key, per side
        var xOverflow = (canvasWidth - xWidth) / 2;

        let capWidth = xWidth * 2 / 3;
        var capAdjustment = (xWidth - capWidth) / 2;
        // if the overflow would be clipped by the left edge
        if(xLeft < xOverflow) {
          this.cap.style.left = (1 + capAdjustment) + 'px';
          xLeft += xOverflow - 1;
          // if the overflow would be clipped by the right edge
        } else if(xLeft > window.innerWidth - xWidth - xOverflow) {
          this.cap.style.left = (canvasWidth - xWidth - 1 + capAdjustment) + 'px';
          xLeft -= xOverflow - 1;
          // standard
        } else {
          this.cap.style.left = (xOverflow + capAdjustment) + 'px';
        }

        kts.left=(xLeft - xOverflow) + 'px';

        let cs = getComputedStyle(this.element);
        let oskHeight = keyman.osk.computedHeight;
        let bottomY = parseFloat(cs.bottom);
        let tipHeight = parseFloat(cs.height);

        let capHeight = xHeight + 3;
        let noncapHeight = tipHeight - capHeight;

        this.tip.style.height = noncapHeight + 'px';

        let finalBaseCapHeight = (keyRect.bottom - _BoxRect.top - Math.floor(y - canvasHeight) - (capHeight));
        let finalCapCalloutHeight = finalBaseCapHeight / 4;
        this.cap.style.top = (noncapHeight - 1) + 'px';
        this.cap.style.width = '0px'; //capWidth + 'px';
        this.cap.style.height = '0px'; //finalCapHeight + 'px';\
        this.cap.style.borderLeftWidth   = (capWidth / 2) + 'px';
        this.cap.style.borderRightWidth  = (capWidth / 2) + 'px';
        this.cap.style.borderTopWidth    = finalCapCalloutHeight + 'px';
        //this.cap.style.borderBottomWidth = (finalCapHeight / 2) + 'px';

        kts.display = 'block';
      } else { // Hide the key preview
        this.element.style.display = 'none';
      }

      // Save the key preview state
      this.key = key;
      this.state = on;
    }
  }
}