import { type KeyElement } from "./keyElement.js";

export interface GlobeHint {
  text: string;
  state: boolean;
  element?: HTMLDivElement;

  show(key: KeyElement, onDismiss?: () => void): void;
  hide(key: KeyElement): void;
}