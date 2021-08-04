namespace com.keyman.osk {
  export interface OSKViewComponent {
    readonly layoutHeight: ParsedLengthStyle;
    refreshLayout(): void;
  }
}