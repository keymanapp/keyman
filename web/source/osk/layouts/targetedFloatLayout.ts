/// <reference path="resizeBar.ts" />
/// <reference path="titleBar.ts" />
/// <reference path="mouseDragOperation.ts" />

namespace com.keyman.osk.layouts {
  export class TargetedFloatLayout {
    titleBar: layouts.TitleBar;
    resizeBar: layouts.ResizeBar;

    private oskView: FloatingOSKView;

    // Encapsulations of the drag behaviors for OSK movement & resizing
    private _moveHandler: MouseDragOperation;
    private _resizeHandler: MouseDragOperation;

    public constructor() {
      this.titleBar = new layouts.TitleBar(this.titleDragHandler);
      this.resizeBar = new layouts.ResizeBar(this.resizeDragHandler);
    }

    public get movementEnabled(): boolean {
      return this.titleDragHandler.enabled;
    }

    public set movementEnabled(flag: boolean) {
      this.titleDragHandler.enabled = flag;
      this.titleBar.showPin(flag && this.oskView.userPositioned);
    }

    public get resizingEnabled(): boolean {
      return this.resizeDragHandler.enabled;
    }

    public set resizingEnabled(flag: boolean) {
      this.resizeDragHandler.enabled = flag;
      this.resizeBar.allowResizing(flag);
    }

    public get isBeingMoved(): boolean {
      return this.titleDragHandler.isActive;
    }

    public get isBeingResized(): boolean {
      return this.resizeDragHandler.isActive;
    }

    attachToView(view: FloatingOSKView) {
      this.oskView = view;
      this.titleBar.attachHandlers(view);
      this.titleDragHandler.enabled = !view.noDrag;
      this.resizeDragHandler.enabled = true; // by default.
    }

    private get titleDragHandler(): MouseDragOperation {
      const layout = this;

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
          if(!layout.oskView) {
            return;
          }

          this.startX = layout.oskView._Box.offsetLeft;
          this.startY = layout.oskView._Box.offsetTop;

          let keymanweb = com.keyman.singleton;
          if(keymanweb.isCJK()) {
            layout.titleBar.setPinCJKOffset();
          }

          keymanweb.uiManager.justActivated = true;
        }

        // Note:  _this.oskView may not be initialized yet.
        onDragMove(cumulativeX: number, cumulativeY: number) {
          if(!layout.oskView) {
            return;
          }

          layout.titleBar.showPin(true);
          layout.oskView.userPositioned = true;

          layout.oskView._Box.style.left = (this.startX + cumulativeX) + 'px';
          layout.oskView._Box.style.top  = (this.startY + cumulativeY) + 'px';

          var r=layout.oskView.getRect();
          layout.oskView.setSize(r.width, r.height, true);
          layout.oskView.x = r.left;
          layout.oskView.y = r.top;
        }

        onDragRelease() {
          if(!layout.oskView) {
            return;
          }

          let keymanweb = com.keyman.singleton;

          keymanweb.domManager.focusLastActiveElement();

          keymanweb.uiManager.justActivated = false;
          keymanweb.uiManager.setActivatingUI(false);

          if(layout.oskView.vkbd) {
            layout.oskView.vkbd.currentKey=null;
          }

          layout.oskView.userPositioned = true;
          layout.oskView.doResizeMove();
          layout.oskView.saveCookie();
        }
      }

      return this._moveHandler;
    }

    private get resizeDragHandler(): MouseDragOperation {
      const layout = this;

      if(this._resizeHandler) {
        return this._resizeHandler;
      }

      this._resizeHandler = new class extends MouseDragOperation {
        startWidth: number;
        startHeight: number;

        constructor() {
          super('se-resize'); // The type of cursor to use while 'active'.
        }

        onDragStart() {
          if(!layout.oskView) {
            return;
          }

          this.startWidth = layout.oskView.computedWidth;
          this.startHeight = layout.oskView.computedHeight;

          let keymanweb = com.keyman.singleton;

          keymanweb.uiManager.justActivated = true;
        }

        // Note:  _this.oskView may not be initialized yet.
        onDragMove(cumulativeX: number, cumulativeY: number) {
          if(!layout.oskView) {
            return;
          }

          let newWidth  = this.startWidth  + cumulativeX;
          let newHeight = this.startHeight + cumulativeY;

          // Set the smallest and largest OSK size
          if(newWidth < 0.2*screen.width) {
            newWidth = 0.2*screen.width;
          }
          if(newHeight < 0.1*screen.height) {
            newHeight = 0.1*screen.height;
          }
          if(newWidth > 0.9*screen.width) {
            newWidth = 0.9*screen.width;
          }
          if(newHeight > 0.5*screen.height) {
            newHeight = 0.5*screen.height;
          }

          // Explicitly set OSK width, height,  and font size - cannot safely rely on scaling from font
          layout.oskView.setSize(newWidth, newHeight, true);
        }

        onDragRelease() {
          if(!layout.oskView) {
            return;
          }

          let keymanweb = com.keyman.singleton;

          keymanweb.domManager.focusLastActiveElement();

          keymanweb.uiManager.justActivated = false;
          keymanweb.uiManager.setActivatingUI(false);

          if(layout.oskView.vkbd) {
            layout.oskView.vkbd.currentKey=null;
          }

          if(layout.oskView.vkbd) {
            this.startWidth  = layout.oskView.computedWidth;
            this.startHeight = layout.oskView.computedHeight;
          }
          layout.oskView.refreshLayout(); // Finalize the resize.
          layout.oskView.doResizeMove();
          layout.oskView.saveCookie();
        }
      }

      return this._resizeHandler;
    }
  }
}