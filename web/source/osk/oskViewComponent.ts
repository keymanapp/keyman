namespace com.keyman.osk {
  export interface OSKViewComponent {
    get layoutHeight(): ParsedLengthStyle;
    refreshLayout(): void;
  }
}