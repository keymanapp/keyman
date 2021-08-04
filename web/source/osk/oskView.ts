// Includes the banner
/// <reference path="./bannerManager.ts" />
// Generates the visual keyboard specific to each keyboard.  (class="kmw-osk-inner-frame")
/// <reference path="visualKeyboard.ts" />
// Models keyboards that present a help page, rather than a standard OSK.
/// <reference path="helpPageView.ts" />
/// <reference path="emptyView.ts" />

namespace com.keyman.osk {
  export abstract class OSKView {
    _Box: HTMLDivElement;

    headerView:   OSKViewComponent;
    bannerView:   BannerManager; // Which implements OSKViewComponent
    keyboardView: KeyboardView;  // Which implements OSKViewComponent
    footerView:   OSKViewComponent;

    public get vkbd(): VisualKeyboard {
      if(this.keyboardView instanceof VisualKeyboard) {
        return this.keyboardView;
      } else {
        return null;
      }
    }

    public get banner(): BannerManager {  // Maintains old reference point used by embedding apps.
      return this.bannerView;
    }

    /**
     * The configured width for this OSKManager.  May be `undefined` or `null`
     * to allow automatic width scaling. 
     */
     private _width: ParsedLengthStyle;

     /**
      * The configured height for this OSKManager.  May be `undefined` or `null`
      * to allow automatic height scaling. 
      */
     private _height: ParsedLengthStyle;
 
     /**
      * The computed width for this OSKManager.  May be null if auto sizing
      * is allowed and the OSKManager is not currently in the DOM hierarchy.
      */
     private _computedWidth: number;
 
     /**
     * The computed height for this OSKManager.  May be null if auto sizing
     * is allowed and the OSKManager is not currently in the DOM hierarchy.
     */
     private _computedHeight: number;
 
     /**
      * The base font size to use for hosted `Banner`s and `VisualKeyboard`
      * instances.
      */
     private _baseFontSize: ParsedLengthStyle;
 
     private needsLayout: boolean = true;

    /**
     * The configured width for this VisualKeyboard.  May be `undefined` or `null`
     * to allow automatic width scaling. 
     */
    get width(): LengthStyle {
      return this._width;
    }

    /**
     * The configured height for this VisualKeyboard.  May be `undefined` or `null`
     * to allow automatic height scaling. 
     */
    get height(): LengthStyle {
      return this._height;
    }

    /**
     * The computed width for this VisualKeyboard.  May be null if auto sizing
     * is allowed and the VisualKeyboard is not currently in the DOM hierarchy.
     */
    get computedWidth(): number {
      // Computed during layout operations; allows caching instead of continuous recomputation.
      if(this.needsLayout) {
        this.refreshLayout();
      }
      return this._computedWidth;
    }

    /**
     * The computed height for this VisualKeyboard.  May be null if auto sizing
     * is allowed and the VisualKeyboard is not currently in the DOM hierarchy.
     */
    get computedHeight(): number {
      // Computed during layout operations; allows caching instead of continuous recomputation.
      if(this.needsLayout) {
        this.refreshLayout();
      }
      return this._computedHeight;
    }

    /**
     * The top-level style string for the font size used by the predictive banner 
     * and the primary keyboard visualization elements.
     */
    get baseFontSize(): string {
      return this.parsedBaseFontSize.styleString;
    }

    protected get parsedBaseFontSize(): ParsedLengthStyle {
      if(!this._baseFontSize) {
        let keymanweb = com.keyman.singleton;
        let device = keymanweb.util.device;
        this._baseFontSize = this.defaultFontSize(device, keymanweb.isEmbedded);
      }

      return this._baseFontSize;
    }

    public defaultFontSize(device: Device, isEmbedded: boolean): ParsedLengthStyle {
      if(device.touchable) {
        var fontScale: number = 1;
        if(device.formFactor == 'phone') {
          fontScale = 1.6 * (isEmbedded ? 0.65 : 0.6) * 1.2;  // Combines original scaling factor with one previously applied to the layer group.
        } else {
          // The following is a *temporary* fix for small format tablets, e.g. PendoPad
          var pixelRatio = 1;
          if(device.OS == 'Android' && 'devicePixelRatio' in window) {
            pixelRatio = window.devicePixelRatio;
          }

          let defaultHeight = this.bannerView.height + this.getDefaultKeyboardHeight();
          if(device.OS == 'Android' && device.formFactor == 'tablet' && defaultHeight < 300 * pixelRatio) {
            fontScale *= 1.2;
          } else {
            fontScale *= 2; //'2.5em';
          }
        }

        // Finalize the font size parameter.
        return ParsedLengthStyle.special(fontScale, 'em');
      } else {
        return this.computedHeight ? ParsedLengthStyle.inPixels(this.computedHeight / 8) : undefined;
      } 
    }

    /* private */ computeFrameHeight(): number {
      return (this.headerView?.layoutHeight.val || 0) + (this.footerView?.layoutHeight.val || 0);
    }

    /*private*/ setSize(width?: number, height?: number, pending?: boolean) {
      let mutatedFlag = false;

      if(width && height) {
        mutatedFlag = !this._width || !this._height;

        const parsedWidth = ParsedLengthStyle.inPixels(width);
        const parsedHeight = ParsedLengthStyle.inPixels(height);

        mutatedFlag = mutatedFlag || parsedWidth.styleString  != this._width.styleString;
        mutatedFlag = mutatedFlag || parsedHeight.styleString != this._height.styleString;

        this._width = parsedWidth;
        this._height = parsedHeight;
      }

      this.needsLayout = this.needsLayout || mutatedFlag;

      if(this.vkbd) {
        let availableHeight = height - this.computeFrameHeight();
        if(this.bannerView.height > 0) {
          availableHeight -= this.bannerView.height + 5;
        }
        this.vkbd.setSize(width, availableHeight, pending);
      }
    }

    public refreshLayout(): void {
      // Step 1:  have the necessary conditions been met?
      const fixedSize = this.width && this.height && this.width.absolute && this.height.absolute;
      const computedStyle = getComputedStyle(this._Box);
      const isInDOM = computedStyle.height != '' && computedStyle.height != 'auto';

      // Step 2:  determine basic layout geometry
      if(fixedSize) {
        this._computedWidth  = this.width.val;
        this._computedHeight = this.height.val;
      } else if(isInDOM) {
        this._computedWidth   = parseInt(computedStyle.width, 10);
        this._computedHeight  = parseInt(computedStyle.height, 10);
      } else {
        // Cannot perform layout operations!
        return;
      }

      // Must be set before any references to the .computedWidth and .computedHeight properties!
      this.needsLayout = false;

      // Step 3:  perform layout operations.
      if(this.vkbd) {
        let availableHeight = this.computedHeight - this.computeFrameHeight();

        // +5:  from kmw-banner-bar's 'top' attribute when active
        if(this.bannerView.height > 0) {
          availableHeight -= this.bannerView.height + 5;
        }
        this.vkbd.setSize(this.computedWidth, availableHeight);
        this.vkbd.refreshLayout();

        if(this.vkbd.usesFixedHeightScaling) {
          var b: HTMLElement = this._Box, bs=b.style;
          bs.height=bs.maxHeight=this.computedHeight+'px';
        }
      }
    }

    public refreshLayoutIfNeeded() {
      if(this.needsLayout) {
        this.refreshLayout();
      }
    }

    public abstract getDefaultKeyboardHeight(): number;
  }
}