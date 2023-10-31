import { ActiveKey } from "@keymanapp/keyboard-processor";
import { KeyElement } from "../keyElement.js";

const FLICK_DIRS = ['n', 'ne', 'e', 'se', 's', 'sw', 'w', 'nw'] as const;

const FLICK_PREVIEW_CHAR = {
  n: '\ufe3f',
  s: '\ufe40',
  nw: '\u2329',
  w: '\u2329',
  sw: '\u232a',
  ne: '\u232a',
  e: '\u232a',
  se: '\u2329'
}

export class GesturePreviewHost {
  private readonly div: HTMLDivElement;
  private readonly label: HTMLSpanElement;
  private readonly previewImgContainer: HTMLDivElement;

  private flickPreviews = new Map<string, HTMLDivElement>;
  private hintLabel: HTMLDivElement = null;

  private onCancel: () => void;

  get element(): HTMLDivElement {
    return this.div;
  }

  constructor(key: KeyElement, isPhone: boolean) {
    const keySpec = key.key.spec;

    const base = this.div = document.createElement('div');
    base.className='kmw-gesture-preview';
    base.id = 'kmw-gesture-preview';

    base.style.pointerEvents='none';

    // We want this to be distinct from the base element so that we can scroll it;
    // this matters greatly for doing flick things.
    const previewImgContainer = this.previewImgContainer = document.createElement('div');
    this.previewImgContainer.id = 'kmw-preview-img-container';

    const label = this.label = document.createElement('span');
    label.className='kmw-gesture-base-label kmw-key-text';
    label.id = 'kmw-gesture-base-label';
    previewImgContainer.appendChild(label);

    // Re-use the text value from the base key's label.
    label.textContent = key.key.label.textContent;

    this.div.appendChild(this.previewImgContainer);

    if(keySpec.flick) {
      const flickSpec = keySpec.flick || {};

      for(const dir of FLICK_DIRS) {
        if(flickSpec[dir]) {
          const index = FLICK_DIRS.indexOf(dir);
          const isDiag = (index % 2) == 1;

          const arrowEle = document.createElement('div');
          arrowEle.className = 'kmw-flick-preview';
          arrowEle.textContent = FLICK_PREVIEW_CHAR[dir];

          let angle: number;

          // The different characters selected here are because of how each is
          // spaced on its line; a pure rotation of a single variant fails to
          // align the rendered glyphs of opposite sides correctly.

          if(dir.includes('w')) {
            arrowEle.style.left = isDiag ? '15%' : '5%';
            angle = (index - 6) * 45;
            arrowEle.style.marginTop = isDiag ? '0px' : '-1px';
          } else if(dir.includes('e')) {
            arrowEle.style.right = isDiag ? '15%' : '5%';
            angle = (index - 2) * 45;
            arrowEle.style.marginTop = isDiag ? '0px' : '-1px';
          } else {
            arrowEle.style.left = '50%';
            arrowEle.style.transform = 'translateX(-50%)';
            angle = 0;
          }

          const isSouthward = dir.includes('s');
          if(angle && isSouthward) {
            angle += 180;
          }

          // The two glyphs below may not render identically to their left & right
          // variants on certain devices, unfortunately.
          if(dir.includes('n')) {
            arrowEle.style.top = isDiag ? '10%' : '0%';
          } else if(isSouthward) {
            arrowEle.style.bottom = isDiag ? '10%' : '0%';
          } else {
            arrowEle.style.top = '50%';
            arrowEle.style.transform = 'translateY(-50%) ';
          }

          arrowEle.style.transform = arrowEle.style.transform + `rotate(${angle}deg)`;

          this.flickPreviews.set(dir, arrowEle);
          previewImgContainer.appendChild(arrowEle);
        }
      }
    }

    // const hintLabel = this.hintLabel = document.createElement('div');
    // hintLabel.className='kmw-key-popup-icon';
    // hintLabel.textContent = keySpec == keySpec.hintSrc ? keySpec.hint : keySpec.hintSrc?.text;
    // hintLabel.style.fontWeight= hintLabel.textContent == '\u2022' ? 'bold' : '';

    // // Default positioning puts it far too close to the flick-preview bit.
    // let yAdjustment = 0;
    // hintLabel.style.marginTop = `-${yAdjustment}px`;

    // // b/c multitap's border forces position shifting
    // let xAdjustment = 0;
    // hintLabel.style.marginRight = `-${xAdjustment}px`;

    // base.appendChild(hintLabel);
  }

  public cancel() {
    this.onCancel?.();
    this.onCancel = null;
  }

  public setCancellationHandler(handler: () => void) {
    this.onCancel = handler;
  }

  // These may not exist like this longterm.
  private clearFlick() {
    for(const pair of this.flickPreviews.entries()) {
      pair[1].classList.add('flick-clear');
    }
  }

  private clearHint() {
    this.hintLabel?.classList.add('hint-clear');
  }

  public clearAll() {
    this.clearFlick();
    this.clearHint();
  }
}