namespace com.keyman.osk.layouts {
  export class TargetedResizeBar {
    private _element: HTMLDivElement;
    private _resizeHandle: HTMLDivElement;

    private mouseCancellingHandler: (ev: MouseEvent) => boolean = function(ev: MouseEvent) {
      ev.preventDefault();
      ev.cancelBubble = true;
      return false;
    };

    public constructor() {
      this._element = this.buildResizeBar();
    }

    public get element(): HTMLDivElement {
      return this._element;
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

      // Prevent selection of caption (IE - set by class for other browsers)
      if('onselectstart' in Ltitle) {
        Ltitle.onselectstart = util.selectStartHandler; //IE (Build 360)
      }

      bar.appendChild(Ltitle);

      var Limg = document.createElement('div');
      this.markUnselectable(Limg);
      Limg.className='kmw-footer-resize';
      Limg.onmousedown=osk._VResizeMouseDown;
      Limg.onmouseover=Limg.onmouseout=osk._VResizeMouseOut;
      bar.appendChild(Limg);
      this._resizeHandle=Limg;
      //TODO: the image never appears in IE8, have no idea why!
      return bar;
    }
  }
}