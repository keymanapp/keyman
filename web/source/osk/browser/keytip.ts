namespace com.keyman.osk.browser {
  export class KeyTip implements com.keyman.osk.KeyTip {
    public readonly element: HTMLDivElement;
    public key: KeyElement;
    public state: boolean = false;

    //  -----
    // |     | <-- tip
    // |  x  | <-- label
    // |_   _|
    //  |   |
    //  |   |  <-- cap
    //  |___|

    private readonly cap: HTMLDivElement;
    private readonly tip: HTMLDivElement;
    private readonly label: HTMLSpanElement;

    private readonly constrain: boolean;

    // constrain:  keep the keytip within the bounds of the overall OSK.
    // Will probably be handled via function in a later pass.
    constructor(constrain: boolean) {
      let tipElement = this.element=document.createElement('div');
      tipElement.className='kmw-keytip';
      tipElement.id = 'kmw-keytip';

      // The following style is critical, so do not rely on external CSS
      tipElement.style.pointerEvents='none';
      tipElement.style.display='none';

      tipElement.appendChild(this.tip = document.createElement('div'));
      tipElement.appendChild(this.cap = document.createElement('div'));
      this.tip.appendChild(this.label = document.createElement('span'));

      this.tip.className = 'kmw-keytip-tip';
      this.cap.className = 'kmw-keytip-cap';
      this.label.className = 'kmw-keytip-label';

      this.constrain = constrain;
    }

    show(key: KeyElement, on: boolean, vkbd: VisualKeyboard) {
      let keyman = com.keyman.singleton;
      let util = keyman.util;

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

        // Canvas dimensions must be set explicitly to prevent clipping
        let canvasWidth = 1.6 * xWidth;
        let canvasHeight = 2.3 * xHeight;

        let kts = this.element.style;
        kts.top = 'auto';

        // Matches how the subkey positioning is set.
        const _Box = vkbd.element.parentNode as HTMLDivElement;
        kts.bottom = (_Box.offsetHeight - rowElement.offsetHeight - rowElement.offsetTop + key.offsetTop) + 'px';
        kts.textAlign = 'center';
        kts.overflow = 'visible';
        kts.fontFamily = util.getStyleValue(kc,'font-family');
        kts.width = canvasWidth+'px';
        kts.height = canvasHeight+'px';

        var px=util.getStyleInt(kc, 'font-size');
        if(px != 0) {
          let popupFS = previewFontScale * px;
          let scaleStyle = {
            fontFamily: kts.fontFamily,
            fontSize: popupFS + 'px',
            height: 1.6 * xHeight + 'px' // as opposed to the canvas height of 2.3 * xHeight.
          };

          kts.fontSize = key.key.getIdealFontSize(vkbd, scaleStyle);
        }

        this.label.textContent = kc.textContent;

        // Adjust shape if at edges
        var xOverflow = (canvasWidth - xWidth) / 2;
        if(xLeft < xOverflow) {
          this.cap.style.left = '0px';
          xLeft += xOverflow;
        } else if(xLeft > window.innerWidth - xWidth - xOverflow) {
          this.cap.style.left = (canvasWidth - xWidth) + 'px';
          xLeft -= xOverflow;
        } else {
          this.cap.style.left = xOverflow + 'px';
        }

        kts.left=(xLeft - xOverflow) + 'px';

        let cs = getComputedStyle(this.element);
        let oskHeight = keyman.osk.computedHeight;
        let bottomY = parseInt(cs.bottom, 10);
        let tipHeight = parseInt(cs.height, 10);

        this.cap.style.width = xWidth + 'px';
        this.tip.style.height = (canvasHeight / 2) + 'px';
        this.cap.style.top = (canvasHeight / 2) + 'px';
        this.cap.style.height = (canvasHeight / 2 - 1) + 'px';

        if(this.constrain && tipHeight + bottomY > oskHeight) {
          const delta = tipHeight + bottomY - oskHeight;
          kts.height = (canvasHeight-delta) + 'px';
          const hx = Math.max(0, (canvasHeight-delta)-(canvasHeight/2));
          this.cap.style.height = hx + 'px';
        }

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