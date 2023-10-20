import { ActiveKey } from "@keymanapp/keyboard-processor";
import { KeyElement } from "../keyElement.js";

export class GesturePreviewHost {
  private readonly div: HTMLDivElement;
  private readonly label: HTMLSpanElement;

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
    label.id = label.className;
    base.appendChild(label);

    // Re-use the text value from the base key's label.
    label.textContent = key.key.label.textContent;
  }
}