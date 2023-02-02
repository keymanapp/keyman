namespace com.keyman.osk {
  export interface OSKViewComponent {
    readonly element: HTMLElement;
    readonly layoutHeight: ParsedLengthStyle;
    refreshLayout(): void;
  }
}