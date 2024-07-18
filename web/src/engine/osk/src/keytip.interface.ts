import { GesturePreviewHost } from "./keyboard-layout/gesturePreviewHost.js";
import { KeyElement } from "./keyElement.js";

export default interface KeyTip {
  key: KeyElement;
  state: boolean;
  element?: HTMLDivElement;

  show(key: KeyElement, on: boolean, previewHost: GesturePreviewHost): void;
}
