import { getAbsoluteX, getAbsoluteY } from "keyman/engine/dom-utils";

import {
  type KeyTip as KeyTipInterface,
  type KeyElement,
  type VisualKeyboard
} from "keyman/engine/osk";

export class KeyTip implements KeyTipInterface {
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
    if(on && this.showPreview) {
      let xBase = getAbsoluteX(key) - getAbsoluteX(vkbd.kbdDiv) + key.offsetWidth/2,
          yBase = getAbsoluteY(key) /*- getAbsoluteY(this.kbdDiv) + bannerHeight*/;
      let kc: Element;

      // Find key text element
      for(let i=0; i<key.childNodes.length; i++) {
        kc = key.childNodes[i] as Element;
        if(kc.classList.contains('kmw-key-text')) {
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