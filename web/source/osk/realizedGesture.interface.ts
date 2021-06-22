namespace com.keyman.osk {
  export interface RealizedGesture {
    readonly baseKey: KeyElement;
    readonly promise: Promise<text.KeyEvent>;

    clear(): void;
    isVisible(): boolean;
    updateTouch(touch: Touch): void;
  }
}