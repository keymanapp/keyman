import { ActiveKey } from "@keymanapp/keyboard-processor";
import { KeyElement } from "../keyElement.js";

const FLICK_DIRS = ['n', 'ne', 'e', 'se', 's', 'sw', 'w', 'nw'] as const;

export class GesturePreviewHost {
  private readonly div: HTMLDivElement;
  private readonly label: HTMLSpanElement;

  private flickPreviews: HTMLDivElement[] = [];

  get element(): HTMLDivElement {
    return this.div;
  }

  constructor(key: KeyElement) {
    const keySpec = key.key.spec;

    const base = this.div = document.createElement('div');
    base.className='kmw-gesture-preview';
    base.id = 'kmw-gesture-preview';

    base.style.pointerEvents='none';
    const label = this.label = document.createElement('span');
    label.className='kmw-gesture-base-label kmw-key-text';
    label.id = 'kmw-gesture-base-label';
    base.appendChild(label);

    // Re-use the text value from the base key's label.
    label.textContent = key.key.label.textContent;

    for(const dir of FLICK_DIRS) {
      const index = FLICK_DIRS.indexOf(dir);
      const isDiag = (index % 2) == 1;

      const arrowEle = document.createElement('div');
      arrowEle.className = 'kmw-flick-preview';
      let angle: number;

      if(dir.includes('w')) {
        arrowEle.style.left = isDiag ? '15%' : '5%';
        arrowEle.textContent = '\u2329';
        angle = (index - 6) * 45;
      } else if(dir.includes('e')) {
        arrowEle.style.right = isDiag ? '15%' : '5%';
        arrowEle.textContent = '\u232a';
        angle = (index - 2) * 45;
      } else {
        arrowEle.style.left = '50%';
        arrowEle.style.transform = 'translateX(-50%)';
        angle = 0;
      }

      if(dir.includes('n')) {
        arrowEle.style.top = isDiag ? '5%' : '-5%';
        arrowEle.textContent ||= '\ufe3f';
      } else if(dir.includes('s')) {
        arrowEle.style.bottom = isDiag ? '5%' : '-5%';
        arrowEle.textContent ||= '\ufe40';
      } else {
        arrowEle.style.top = '50%';
        arrowEle.style.transform = 'translateY(-50%) ';
      }

      // const angle = (index - 2) * 45;
      arrowEle.style.transform = arrowEle.style.transform + `rotate(${angle}deg)`;

      this.flickPreviews.push(arrowEle);
      this.div.appendChild(arrowEle);
    }
  }
}