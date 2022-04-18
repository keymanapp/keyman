namespace com.keyman.osk.embedded {
  export class KeyTip implements com.keyman.osk.KeyTip {
    public key: KeyElement;
    public state: boolean = false;

    private showPreview: (x: number, y: number, width: number, height: number, text: string) => void;
    private clearPreview: () => void;

    constructor(showPreview: typeof KeyTip.prototype.showPreview,
                clearPreview: () => void) {

      if(showPreview == null || typeof showPreview == 'function') {
        this.showPreview = showPreview;
      }
      if(clearPreview == null || typeof clearPreview == 'function') {
        this.clearPreview = clearPreview;
      }
    }

    show(key: KeyElement, on: boolean, vkbd: VisualKeyboard) {
      let util = com.keyman.singleton.util;

      if(on && this.showPreview) {
        var xBase = dom.Utils.getAbsoluteX(key) - dom.Utils.getAbsoluteX(vkbd.kbdDiv) + key.offsetWidth/2,
            yBase = dom.Utils.getAbsoluteY(key) /*- dom.Utils.getAbsoluteY(this.kbdDiv) + bannerHeight*/,
            kc;

        // Find key text element
        for(var i=0; i<key.childNodes.length; i++) {
          kc = key.childNodes[i];
          if(util.hasClass(kc,'kmw-key-text')) {
            break;
          }
        }

        if(key.className.indexOf('kmw-key-default') >= 0 && key.id.indexOf('K_SPACE') < 0) {
          this.showPreview(xBase, yBase, key.offsetWidth, key.offsetHeight, kc.innerHTML);
        }
      } else if(!on && this.clearPreview) {
        if(vkbd.touchCount == 0 || key == null) {
          this.clearPreview();
        }
      }

      this.key = key;
      this.state = on;
    }
  }
}