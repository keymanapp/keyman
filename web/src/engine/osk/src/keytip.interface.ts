import { GesturePreviewHost } from "./keyboard-layout/gesturePreviewHost.js";
import { KeyElement } from "./keyElement.js";
import VisualKeyboard from "./visualKeyboard.js";

export default interface KeyTip {
  key: KeyElement;
  state: boolean;
  element?: HTMLDivElement;

  show(key: KeyElement, on: boolean, vkbd: VisualKeyboard, previewHost: GesturePreviewHost);
}
