namespace com.keyman.osk {
  export interface PendingGesture {
    readonly baseKey: KeyElement;
    readonly promise: Promise<RealizedGesture>;

    cancel(): void;
    resolve?(): void;
  }
}