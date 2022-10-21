/// <reference path="mouseDragOperation.ts" />
/// <reference path="../oskViewComponent.ts" />

namespace com.keyman.osk.layouts {
  export class TitleBar implements OSKViewComponent {
    private _element: HTMLDivElement;
    private _unpinButton: HTMLDivElement;
    private _closeButton: HTMLDivElement;
    private _helpButton: HTMLDivElement;
    private _configButton: HTMLDivElement;
    private _caption: HTMLSpanElement;

    private static readonly DISPLAY_HEIGHT = ParsedLengthStyle.inPixels(20); // As set in kmwosk.css

    public constructor(dragHandler?: MouseDragOperation) {
      this._element = this.buildTitleBar();

      if(dragHandler) {
        this.element.onmousedown = dragHandler.mouseDownHandler;
      }
    }

    public get layoutHeight(): ParsedLengthStyle {
      return TitleBar.DISPLAY_HEIGHT;
    }

    private mouseCancellingHandler: (ev: MouseEvent) => boolean = function(ev: MouseEvent) {
      ev.preventDefault();
      ev.cancelBubble = true;
      return false;
    };

    public get element(): HTMLDivElement {
      return this._element;
    }

    public setPinCJKOffset() {
      this._unpinButton.style.left = '15px';
    }

    public showPin(visible: boolean) {
      this._unpinButton.style.display = visible ? 'block' : 'none';
    }

    public setTitle(str: string) {
      this._caption.innerHTML = str;
    }

    public setTitleFromKeyboard(keyboard: keyboards.Keyboard) {
      let title = "<span style='font-weight:bold'>" + keyboard?.name + '</span>';  // I1972  // I2186
      this._caption.innerHTML = title;
    }

    public attachHandlers(osk: OSKView) {
      let util = com.keyman.singleton.util;

      this._helpButton.onclick = function() {
        var p={};
        util.callEvent('osk.helpclick',p);
        if(window.event) {
          window.event.returnValue=false;
        }
        return false;
      }

      this._configButton.onclick = function() {
        var p={};
        util.callEvent('osk.configclick',p);
        if(window.event) {
          window.event.returnValue=false;
        }
        return false;
      }

      this._closeButton.onclick = function () {
        osk.startHide(true);
        return false;
      };

      if(osk instanceof FloatingOSKView) {
        const _osk = osk as FloatingOSKView;
        this._unpinButton.onclick = function () {
          _osk.restorePosition(true);
          return false;
        }
      }
    }

    /**
     * Create a control bar with title and buttons for the desktop OSK
     */
    buildTitleBar(): HTMLDivElement {
      let bar = document.createElement('div');
      this.markUnselectable(bar);
      bar.id='keymanweb_title_bar';
      bar.className='kmw-title-bar';

      var Ltitle = this._caption = document.createElement('span');
      this.markUnselectable(Ltitle);
      Ltitle.className='kmw-title-bar-caption';
      Ltitle.style.color='#fff';
      bar.appendChild(Ltitle);

      var Limg = this._closeButton = this.buildCloseButton();
      bar.appendChild(Limg);

      Limg = this._helpButton = this.buildHelpButton()
      bar.appendChild(Limg);

      Limg = this._configButton = this.buildConfigButton();
      bar.appendChild(Limg);

      Limg = this._unpinButton = this.buildUnpinButton();
      bar.appendChild(Limg);

      return bar;
    }

    private markUnselectable(e: HTMLElement) {
      e.style.MozUserSelect="none";
      e.style.KhtmlUserSelect="none";
      e.style.UserSelect="none";
      e.style.WebkitUserSelect="none";
    }

    private buildCloseButton(): HTMLDivElement {
      var Limg = document.createElement('div');
      this.markUnselectable(Limg);

      Limg.id='kmw-close-button';
      Limg.className='kmw-title-bar-image';
      Limg.onmousedown = this.mouseCancellingHandler;

      return Limg;
    }

    private buildHelpButton(): HTMLDivElement {
      let Limg = document.createElement('div');
      this.markUnselectable(Limg);
      Limg.id='kmw-help-image';
      Limg.className='kmw-title-bar-image';
      Limg.title='KeymanWeb Help';
      Limg.onmousedown = this.mouseCancellingHandler;
      return Limg;
    }

    private buildConfigButton(): HTMLDivElement {
      let Limg = document.createElement('div');
      this.markUnselectable(Limg);

      Limg.id='kmw-config-image';
      Limg.className='kmw-title-bar-image';
      Limg.title='KeymanWeb Configuration Options';
      Limg.onmousedown = this.mouseCancellingHandler;

      return Limg;
    }

    /**
     * Builds an 'unpin' button for restoring OSK to default location, handle mousedown and click events
     */
    private buildUnpinButton(): HTMLDivElement {
      let Limg = document.createElement('div');  //I2186
      this.markUnselectable(Limg);

      Limg.id = 'kmw-pin-image';
      Limg.className = 'kmw-title-bar-image';
      Limg.title='Pin the On Screen Keyboard to its default location on the active text box';

      Limg.onmousedown = this.mouseCancellingHandler;

      return Limg;
    }

    public refreshLayout() {
      // The title bar is adaptable as it is and needs no adjustments.
    }
  }
}