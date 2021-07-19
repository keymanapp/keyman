/// <reference path="resizeBar.ts" />
/// <reference path="titleBar.ts" />

namespace com.keyman.osk.layouts {
  export class TargetedFloatLayout {
    titleBar: layouts.TitleBar;
    resizeBar: layouts.ResizeBar;

    public constructor() {
      this.titleBar = new layouts.TitleBar();
      this.resizeBar = new layouts.ResizeBar();
    }
  }
}