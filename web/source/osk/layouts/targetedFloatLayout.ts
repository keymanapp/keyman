/// <reference path="resizeBar.ts" />
/// <reference path="titleBar.ts" />
/// <reference path="mouseStartSnapshot.ts" />
/// <reference path="customizationCoordinate.ts" />

namespace com.keyman.osk.layouts {
  export class TargetedFloatLayout {
    titleBar: layouts.TitleBar;
    resizeBar: layouts.ResizeBar;

    private oskView: OSKManager;

    // OSK resizing-event state fields
    private _resizeStart: CustomizationCoordinate;
    private _VOriginalWidth: number;
    private _VOriginalHeight: number;

    // Resize-event temporary storage
    private _mouseStartSnapshot: MouseStartSnapshot;
    private _moveHandler: MouseDragOperation;

    public constructor() {
      this.titleBar = new layouts.TitleBar(this.titleDragHandler);
      this.resizeBar = new layouts.ResizeBar();

      // Attach handlers to the title bar and resize bar as appropriate.
      this.resizeBar.handle.onmousedown = this._VResizeMouseDown.bind(this);
    }

    public get isMovingOrResizing(): boolean {
      return this._moveHandler.isActive || !!this._mouseStartSnapshot;
    }

    public get movementEnabled(): boolean {
      return this.titleDragHandler.enabled;
    }

    public set movementEnabled(flag: boolean) {
      this.titleDragHandler.enabled = flag;
    }

    public get isBeingMoved(): boolean {
      return this.titleDragHandler.isActive;
    }

    attachToView(view: OSKManager) {
      if(this.oskView) {
        throw new Error("This layout is already attached to an OSK.");
      }

      this.oskView = view;
      this.titleDragHandler.enabled = !view.noDrag;
    }


    private get titleDragHandler(): MouseDragOperation {
      const _this = this;

      if(this._moveHandler) {
        return this._moveHandler;
      }

      this._moveHandler = new class extends MouseDragOperation {
        startX: number;
        startY: number;

        constructor() {
          super('move'); // The type of cursor to use while 'active'.
        }

        onDragStart() {
          if(!_this.oskView) {
            return;
          }

          this.startX = _this.oskView._Box.offsetLeft;
          this.startY = _this.oskView._Box.offsetTop;

          let keymanweb = com.keyman.singleton;
          if(keymanweb.isCJK()) {
            _this.titleBar.setPinCJKOffset();
          }

          keymanweb.uiManager.justActivated = true;
        }

        // Note:  _this.oskView may not be initialized yet.
        onDragMove(cumulativeX: number, cumulativeY: number) {
          if(!_this.oskView) {
            return;
          }

          _this.titleBar.showPin(true);
          _this.oskView.userPositioned = true;

          _this.oskView._Box.style.left = (this.startX + cumulativeX) + 'px';
          _this.oskView._Box.style.top  = (this.startY + cumulativeY) + 'px';

          var r=_this.oskView.getRect();
          _this.oskView.setSize(r.width, r.height, true);
          _this.oskView.x = r.left;
          _this.oskView.y = r.top;
        }

        onDragRelease() {
          if(!_this.oskView) {
            return;
          }

          let keymanweb = com.keyman.singleton;

          keymanweb.domManager.focusLastActiveElement();

          keymanweb.uiManager.justActivated = false;
          keymanweb.uiManager.setActivatingUI(false);

          if(_this.oskView.vkbd) {
            _this.oskView.vkbd.currentKey=null;
          }

          _this.oskView.userPositioned = true;
          _this.oskView.doResizeMove();
          _this.oskView.saveCookie();
        }
      }

      return this._moveHandler;
    }

    /**
     * Function     _VResizeMouseDown
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Process resizing of KMW UI
     */
    private _VResizeMouseDown(e: MouseEvent) {
      if(!e) {
        return true;
      }

      this._resizeStart = CustomizationCoordinate.fromEvent(e);

      if(!this._mouseStartSnapshot) { // I1472 - Dragging off edge of browser window causes muckup
        this._mouseStartSnapshot = new MouseStartSnapshot(e);
      }

      this._VOriginalWidth = this.oskView.vkbd.kbdDiv.offsetWidth;
      this._VOriginalHeight = this.oskView.vkbd.kbdDiv.offsetHeight;

      document.onmousemove = this._VResizeMouseMove.bind(this);
      document.onmouseup = this._VResizeMouseUp.bind(this);

      if(document.body.style.cursor) {
        document.body.style.cursor = 'se-resize';
      }

      e.preventDefault();
      e.cancelBubble = true;

      let keymanweb = com.keyman.singleton;

      keymanweb.uiManager.justActivated = true;
      return false;
    }

    /**
     * Function     _VResizeMouseMove
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Process mouse movement during resizing of OSK
     */
    private _VResizeMouseMove(e: MouseEvent) {
      if(!e) {
        return true;
      }

      e.preventDefault();
      e.cancelBubble = true;

      if(!this._mouseStartSnapshot.matchesCausingClick(e)) { // I1472 - Dragging off edge of browser window causes muckup
        return this._VResizeMouseUp(e);
      } else {
        const coord = CustomizationCoordinate.fromEvent(e);
        let deltaX = coord.x - this._resizeStart.x;
        let deltaY = coord.y - this._resizeStart.y;
        var newWidth  = (this._VOriginalWidth   + deltaX),
            newHeight = (this._VOriginalHeight  + deltaY);
            
        // Set the smallest and largest OSK size
        if(newWidth < 0.2*screen.width) {
          newWidth = 0.2*screen.width;
        }
        if(newHeight < 0.1*screen.height) {
          newHeight = 0.1*screen.height;
        }
        if(newWidth > 0.9*screen.width) {
          newWidth=0.9*screen.width;
        }
        if(newHeight > 0.5*screen.height) {
          newWidth=0.5*screen.height;
        }

        // Explicitly set OSK width, height,  and font size - cannot safely rely on scaling from font
        this.oskView.setSize(newWidth, newHeight);

        return false;
      }
    }

    /**
     * Function     _VResizeMoveMouseUp
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Process mouse up during resizing of KMW UI
     */
    private _VResizeMouseUp(e: MouseEvent) {
      if(!e) {
        return true;
      }

      this._mouseStartSnapshot.restore();
      this._mouseStartSnapshot = null;
      
      if(this.oskView.vkbd) {
        this.oskView.vkbd.currentKey=null;
      }

      let keymanweb = com.keyman.singleton;

      keymanweb.domManager.focusLastActiveElement();

      keymanweb.uiManager.justActivated = false;
      keymanweb.uiManager.setActivatingUI(false);

      if(this.oskView.vkbd) {
        this._VOriginalWidth = this.oskView.vkbd.kbdDiv.offsetWidth;
        this._VOriginalHeight = this.oskView.vkbd.kbdDiv.offsetHeight;
      }

      this.oskView.doResizeMove();
      this.oskView.saveCookie();

      e.preventDefault();
      e.cancelBubble = true;
      return false;
    }
  }
}