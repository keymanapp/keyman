import { KeyElement } from "./keyElement.js";
import VisualKeyboard from "./visualKeyboard.js";

export default interface KeyTip {
  key: KeyElement;
  state: boolean;
  element?: HTMLDivElement;

  show(key: KeyElement, on: boolean, vkbd: VisualKeyboard);
}
