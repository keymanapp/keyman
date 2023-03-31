namespace com.keyman.osk {
  export interface KeyTip {
    key: KeyElement;
    state: boolean;
    element?: HTMLDivElement;

    show(key: KeyElement, on: boolean, vkbd: VisualKeyboard);
  }
}