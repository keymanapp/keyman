import { type KeyElement } from "./keyElement.js";

export default interface GlobeHint {
  text: string;
  state: boolean;
  element?: HTMLDivElement;

  show(key: KeyElement, onDismiss?: () => void);
  hide(key: KeyElement);
}