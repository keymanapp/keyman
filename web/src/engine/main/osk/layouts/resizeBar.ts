/// <reference path="../oskViewComponent.ts" />

namespace com.keyman.osk.layouts {
  export class ResizeBar implements OSKViewComponent {
    private _element: HTMLDivElement;
    private _resizeHandle: HTMLDivElement;

    private static readonly DISPLAY_HEIGHT = ParsedLengthStyle.inPixels(16); // As set in kmwosk.css

    private mouseCancellingHandler: (ev: MouseEvent) => boolean = function(ev: MouseEvent) {
      ev.preventDefault();
      ev.cancelBubble = true;
      return false;
    };

    public constructor(dragHandler?: MouseDragOperation) {
      this._element = this.buildResizeBar();

      if(dragHandler) {
        this._resizeHandle.onmousedown = dragHandler.mouseDownHandler;
      }
    }

    public get layoutHeight(): ParsedLengthStyle {
      return ResizeBar.DISPLAY_HEIGHT;
    }

    public get element(): HTMLDivElement {
      return this._element;
    }

    public get handle(): HTMLDivElement {
      return this._resizeHandle;
    }

    public allowResizing(flag: boolean) {
      this._resizeHandle.style.display = flag ? 'block' : 'none';
    }

    private markUnselectable(e: HTMLElement) {
      e.style.MozUserSelect="none";
      e.style.KhtmlUserSelect="none";
      e.style.UserSelect="none";
      e.style.WebkitUserSelect="none";
    }

    /**
     * Create a bottom bar with a resizing icon for the desktop OSK
     */
    buildResizeBar(): HTMLDivElement {
      let util = com.keyman.singleton.util;
      let osk = com.keyman.singleton.osk;

      var bar = document.createElement('div');
      this.markUnselectable(bar);
      bar.className='kmw-footer';
      bar.onmousedown = this.mouseCancellingHandler;

      // Add caption
      var Ltitle=document.createElement('div');
      this.markUnselectable(Ltitle);
      Ltitle.className='kmw-footer-caption';
      Ltitle.innerHTML='<a href="https://keyman.com/developer/keymanweb/">KeymanWeb</a>';
      Ltitle.id='keymanweb-osk-footer-caption';

      // Display build number on shift+double click
      util.attachDOMEvent(Ltitle,'dblclick', function(e) {
        if(e && e.shiftKey) {
          osk.showBuild();
        }
        return false;
      }.bind(this),false);

      bar.appendChild(Ltitle);

      var Limg = document.createElement('div');
      this.markUnselectable(Limg);
      Limg.className='kmw-footer-resize';
      bar.appendChild(Limg);
      this._resizeHandle=Limg;

      return bar;
    }

    public refreshLayout() {
      // The title bar is adaptable as it is and needs no adjustments.
    }
  }
}