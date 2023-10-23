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
  private lpPreview: HTMLDivElement = null;
  private readonly mtStyling: boolean;

  get element(): HTMLDivElement {
    return this.div;
  }

  constructor(key: KeyElement, isPhone: boolean) {
    // Temporary "force all to be on" switch.  Is within constructor so it can
    // update during a demo.
    const DEMO_ALL = false || window['GESTURE_DEMO'];

    const keySpec = key.key.spec;

    const base = this.div = document.createElement('div');
    base.className='kmw-gesture-preview';
    base.id = 'kmw-gesture-preview';

    base.style.pointerEvents='none';

    // Should probably just be the base element at this point...
    // Ah well, only figured this out after experimentation.
    const previewImgContainer = this.previewImgContainer = document.createElement('div');
    this.previewImgContainer.id = 'kmw-preview-img-container';

    const label = this.label = document.createElement('span');
    label.className='kmw-gesture-base-label kmw-key-text';
    label.id = 'kmw-gesture-base-label';
    previewImgContainer.appendChild(label);

    // Re-use the text value from the base key's label.
    label.textContent = key.key.label.textContent;

    this.mtStyling = DEMO_ALL || keySpec.multitap;
    if(this.mtStyling) {
      // Shifts the layout to provide a rough multitap visualization
      this.previewImgContainer.className = 'kmw-multitap-preview'; // to indicate multitap presence.
    }

    this.div.appendChild(this.previewImgContainer);

    if(DEMO_ALL || keySpec.flick) {
      const flickSpec = keySpec.flick || {};

      for(const dir of FLICK_DIRS) {
        if(DEMO_ALL || (flickSpec[dir])) {
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

    // const neFlick = DEMO_ALL || keySpec.flick && keySpec.flick.ne;
    if(DEMO_ALL || keySpec.sk) {
      const skIcon = this.lpPreview = document.createElement('div');
      skIcon.className='kmw-key-popup-icon';

      // Default positioning puts it far too close to the flick-preview bit.
      let yAdjustment = DEMO_ALL || keySpec.multitap ? 1 : 0;
      yAdjustment += isPhone ? 2 : 0;
      skIcon.style.marginTop = `-${yAdjustment}px`;

      // b/c multitap's border forces position shifting
      let xAdjustment = DEMO_ALL || keySpec.multitap ? 3 : 0;
      skIcon.style.marginRight = `-${xAdjustment}px`;

      previewImgContainer.appendChild(skIcon);
    }
  }

  // These may not exist like this longterm.
  private clearMultitap() {
    if(this.mtStyling) {
      this.previewImgContainer.classList.add('multitap-clear');
    }
  }

  private clearFlick() {
    for(const pair of this.flickPreviews.entries()) {
      pair[1].classList.add('flick-clear');
    }
  }

  private clearLongpress() {
    this.lpPreview?.classList.add('longpress-clear');
  }

  private clearAll() {
    this.clearMultitap();
    this.clearFlick();
    this.clearLongpress();
  }
}