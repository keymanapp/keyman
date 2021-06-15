namespace com.keyman.osk.browser {
  export class KeyTip {
    public readonly element: HTMLDivElement;
    public key: KeyElement;
    public state: boolean = false;

    private canvas: HTMLCanvasElement;
    private label: HTMLSpanElement;

    constructor() {
      let tipElement = this.element=document.createElement('div');
      tipElement.className='kmw-keytip';
      tipElement.id = 'kmw-keytip';

      // The following style is critical, so do not rely on external CSS
      tipElement.style.pointerEvents='none';

      // Add CANVAS element for outline and SPAN for key label
      tipElement.appendChild(this.canvas = document.createElement('canvas'));
      tipElement.appendChild(this.label = document.createElement('span'));
    }

    show(key: KeyElement, on: boolean, vkbd: VisualKeyboard) {
      let keyman = com.keyman.singleton;
      let util = keyman.util;

      // Create and display the preview
      if(on) {
        var xLeft = dom.Utils.getAbsoluteX(key),
            xWidth = key.offsetWidth,
            xHeight = key.offsetHeight,
            kc = key.key.label,
            edge = 0,
            previewFontScale = 1.8;

        // Find key text element
        for(var i=0; i<key.childNodes.length; i++) {
          kc = <HTMLElement> key.childNodes[i];
          if(kc.classList.contains('kmw-key-text')) {
            break;
          }
        }

        // Canvas dimensions must be set explicitly to prevent clipping
        this.canvas.width = 1.6 * xWidth;
        this.canvas.height = 2.3 * xHeight;

        let kts = this.element.style;
        kts.top = 'auto';
        // Matches how the subkey positioning is set.
        kts.bottom = (parseInt(key.style.bottom, 10))+'px';
        kts.textAlign = 'center';
        kts.overflow = 'visible';
        kts.fontFamily = util.getStyleValue(kc,'font-family');
        kts.width = this.canvas.width+'px';
        kts.height = this.canvas.height+'px';

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

        let ktls = this.label.style;
        ktls.display = 'block';
        ktls.position = 'absolute';
        ktls.textAlign = 'center';
        ktls.width='100%';
        ktls.top = '2%';
        ktls.bottom = 'auto';

        // Adjust canvas shape if at edges
        var xOverflow = (this.canvas.width - xWidth) / 2;
        if(xLeft < xOverflow) {
          edge = -1;
          xLeft += xOverflow;
        } else if(xLeft > window.innerWidth - xWidth - xOverflow) {
          edge = 1;
          xLeft -= xOverflow;
        }

        // For now, should only be true (in production) when keyman.isEmbedded == true.
        let constrainPopup = keyman.isEmbedded;

        let cs = getComputedStyle(this.element);
        let oskHeight = keyman.osk.getHeight();
        let bottomY = parseInt(cs.bottom, 10);
        let tipHeight = parseInt(cs.height, 10);

        let delta = 0;
        if(tipHeight + bottomY > oskHeight && constrainPopup) {
          delta = tipHeight + bottomY - oskHeight;
          this.canvas.height = this.canvas.height - delta;
          kts.height = this.canvas.height + 'px';
        }

        this.drawPreview(this.canvas, vkbd.device, xWidth, xHeight, edge, delta);

        kts.left=(xLeft - xOverflow) + 'px';
        kts.display = 'block';
      } else { // Hide the key preview
        this.element.style.display = 'none';
      }

      // Save the key preview state
      this.key = key;
      this.state = on;
    }

    /**
     * Draw key preview in element using CANVAS
     *  @param  {Object}  canvas CANVAS element
     *  @param  {number}  w width of touched key, px
     *  @param  {number}  h height of touched key, px
     *  @param  {number}  edge  -1 left edge, 1 right edge, else 0
     */
    drawPreview(canvas: HTMLCanvasElement, device: com.keyman.Device, w: number, h: number, edge: number, delta?: number) {
      delta = delta || 0;

      var ctx = canvas.getContext('2d'), dx = (canvas.width - w)/2, hMax = canvas.height + delta,
          w0 = 0, w1 = dx, w2 = w + dx, w3 = w + 2 * dx,
          h1 = 0.5 * hMax, h2 = 0.6 * hMax, h3 = hMax, r = 8;

      let hBoundedMax = canvas.height;

      h2 = h2 > hBoundedMax ? hBoundedMax : h2;
      h3 = hMax > hBoundedMax ? hBoundedMax : h3;

      if(device.OS == 'Android') {
        r = 3;
      }

      // Adjust the preview shape at the edge of the keyboard
      switch(edge) {
        case -1:
          w1 -= dx;
          w2 -= dx;
          break;
        case 1:
          w1 += dx;
          w2 += dx;
          break;
      }

      // Clear the canvas
      ctx.clearRect(0,0,canvas.width,canvas.height);

      // Define appearance of preview (cannot be done directly in CSS)
      if(device.OS == 'Android') {
        var wx=(w1+w2)/2;
        w1 = w2 = wx;
      }
      ctx.fillStyle = device.styles.popupCanvasBackgroundColor;
      ctx.lineWidth = 1;
      ctx.strokeStyle = '#cccccc';

      // Draw outline
      ctx.save();
      ctx.beginPath();
      ctx.moveTo(w0+r,0);
      ctx.arcTo(w3,0,w3,r,r);
      if(device.OS == 'Android') {
        ctx.arcTo(w3,h1,w2,h2,r);
        ctx.arcTo(w2,h2,w1,h2,r);
      } else {
        let lowerR = 0;
        if(h3 > h2) {
          lowerR = h3-h2 > r ? r : h3-h2;
        }
        ctx.arcTo(w3,h1,w2,h2,r);
        ctx.arcTo(w2,h2,w2-lowerR,h3,lowerR);
        ctx.arcTo(w2,h3,w1,h3,lowerR);
        ctx.arcTo(w1,h3,w1,h2-lowerR,lowerR);
      }
      ctx.arcTo(w1,h2,w0,h1-r,r);
      ctx.arcTo(w0,h1,w0,r,r);
      ctx.arcTo(w0,0,w0+r,0,r);
      ctx.fill();
      ctx.stroke();
      ctx.restore();
    };
  }
}