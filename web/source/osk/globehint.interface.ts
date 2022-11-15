namespace com.keyman.osk {
  export interface GlobeHint {
    state: boolean;
    element?: HTMLDivElement;

    show(key: KeyElement);
    hide(key: KeyElement);
  }
}