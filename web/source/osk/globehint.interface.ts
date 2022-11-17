namespace com.keyman.osk {
  export interface GlobeHint {
    text: string;
    state: boolean;
    element?: HTMLDivElement;

    show(key: KeyElement, onDismiss?: () => void);
    hide(key: KeyElement);
  }
}