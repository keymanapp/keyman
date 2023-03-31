/// <reference path="keyboardView.interface.ts" />

namespace com.keyman.osk {
  export class EmptyView implements KeyboardView {
    readonly element: HTMLDivElement;
    
    constructor() {
      let Ldiv = this.element = document.createElement('div');
      Ldiv.style.userSelect = 'none';
      Ldiv.className='kmw-osk-none';
    }

    // No operations needed; this is a stand-in for the desktop OSK when no keyboard is active.
    public postInsert() { }
    public updateState() { }

    public refreshLayout() { }

    public get layoutHeight(): ParsedLengthStyle {
      return ParsedLengthStyle.inPixels(0);
    }
  }
}