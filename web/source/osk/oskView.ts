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

    protected device: com.keyman.utils.DeviceSpec;

    private keyboard: keyboards.Keyboard;

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
        this._baseFontSize = this.defaultFontSize(this.device, keymanweb.isEmbedded);
      }

      return this._baseFontSize;
    }

    public defaultFontSize(device: utils.DeviceSpec, isEmbedded: boolean): ParsedLengthStyle {
      if(device.touchable) {
        var fontScale: number = 1;
        if(device.formFactor == 'phone') {
          fontScale = 1.6 * (isEmbedded ? 0.65 : 0.6) * 1.2;  // Combines original scaling factor with one previously applied to the layer group.
        } else {
          // The following is a *temporary* fix for small format tablets, e.g. PendoPad
          var pixelRatio = 1;
          if(device.OS == 'android' && 'devicePixelRatio' in window) {
            pixelRatio = window.devicePixelRatio;
          }

          let defaultHeight = this.bannerView.height + this.getDefaultKeyboardHeight();
          if(device.OS == 'android' && device.formFactor == 'tablet' && defaultHeight < 300 * pixelRatio) {
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

    public get activeKeyboard(): keyboards.Keyboard {
      return this.keyboard;
    }

    public set activeKeyboard(keyboard: keyboards.Keyboard) {
      this.keyboard = keyboard;
      this.loadActiveKeyboard();
    }

    constructor(deviceSpec: com.keyman.utils.DeviceSpec) {
      this.device = deviceSpec;

      // OSK initialization - create DIV and set default styles
      this._Box = document.createElement('div');   // Container for OSK (Help DIV, displayed when user clicks Help icon)
      this._Box.style.userSelect = 'none';

      // Initializes the two constant OSKComponentView fields.
      this.bannerView   = new BannerManager();
      this.keyboardView = null;

      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      // Install the default OSK stylesheet
      util.linkStyleSheet(keymanweb.getStyleSheetPath('kmwosk.css'));
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

    public abstract getDefaultWidth(): number;
    public abstract getDefaultKeyboardHeight(): number;

    /**
     * Function     _Load
     * Scope        Private
     * Description  OSK initialization when keyboard selected
     */
    _Load() { // Load Help - maintained only temporarily.
      let keymanweb = com.keyman.singleton;
      this.activeKeyboard = keymanweb.core.activeKeyboard;
    }

    protected abstract postKeyboardLoad(): void;

    private loadActiveKeyboard() {
      let device = this.device;

      var s = this._Box.style;
      s.zIndex='9999'; s.display='none'; s.width= device.touchable ? '100%' : 'auto';
      s.position = (device.formFactor == 'desktop' ? 'absolute' : 'fixed');

      if(this.vkbd) {
        this.vkbd.shutdown();
      }
      this.keyboardView = null;

      // Instantly resets the OSK container, erasing / delinking the previously-loaded keyboard.
      this._Box.innerHTML = '';

      // Any event-cancelers would go here, after the innerHTML reset.

      // Add header element to OSK only for desktop browsers
      if(this.headerView) {
        this._Box.appendChild(this.headerView.element);
      }

      // Add suggestion banner bar to OSK
      this._Box.appendChild(this.banner.element);
      this.banner.element.style.fontSize = this.baseFontSize;

      let kbdView: KeyboardView = this.keyboardView = this._GenerateKeyboardView(this.activeKeyboard);
      this._Box.appendChild(kbdView.element);
      if(kbdView instanceof VisualKeyboard) {
        kbdView.fontSize = this.parsedBaseFontSize;
      }
      kbdView.postInsert();

      // Add footer element to OSK only for desktop browsers
      if(this.footerView) {
        this._Box.appendChild(this.footerView.element);
      }

      // Initializes the size of a touch keyboard.
      if(this.vkbd && device.touchable) {
        let targetOSKHeight = this.vkbd.computedAdjustedOskHeight(this.getDefaultKeyboardHeight());
        this.setSize(this.getDefaultWidth(), targetOSKHeight + this.banner.height);
      }
      // END:  construction of the actual internal layout for the overall OSK

      // Correct the classname for the (inner) OSK frame (Build 360)
      var kbdID: string = (this.activeKeyboard ? this.activeKeyboard.id.replace('Keyboard_','') : '');

      if(kbdID.indexOf('::') != -1) { // We used to also test if we were in embedded mode, but... whatever.
        // De-namespaces the ID for use with CSS classes.
        // Assumes that keyboard IDs may not contain the ':' symbol.
        kbdID = kbdID.substring(kbdID.indexOf('::') + 2);
      }

      const kbdClassSuffix = ' kmw-keyboard-' + kbdID;
      kbdView.element.className = kbdView.element.className + kbdClassSuffix;

      this.banner.appendStyles();

      if(this.vkbd) {
        // Create the key preview (for phones)
        this.vkbd.createKeyTip();

        // Append a stylesheet for this keyboard for keyboard specific styles
        // or if needed to specify an embedded font
        this.vkbd.appendStyleSheet();
      }

      this.postKeyboardLoad();
    }

    private layerChangeHandler: text.SystemStoreMutationHandler = function(this: OSKManager,
      source: text.MutableSystemStore,
      newValue: string) {
      // This handler is also triggered on state-key state changes (K_CAPS) that 
      // may not actually change the layer.
      if(this.vkbd) {
        this.vkbd._UpdateVKShiftStyle();
      }

      if(source.value != newValue) {
        // Prevents console errors when a keyboard only displays help.
        // Can occur when using SHIFT with sil_euro_latin on a desktop form-factor.
        if(this.vkbd) {
          this.vkbd.layerId = newValue;
        }
        this._Show();
      }
    }.bind(this);

    private _GenerateKeyboardView(keyboard: keyboards.Keyboard): KeyboardView {
      let device = com.keyman.singleton.util.device;

      if(this.vkbd) {
        this.vkbd.shutdown();
      }

      this._Box.className = "";

      // Case 1:  since we hide the system keyboard on touch devices, we need
      //          to display SOMETHING that can accept input.
      if(keyboard == null && !device.touchable) {
        // We do not (currently) allow selecting the default system keyboard on
        // touch form-factors.  Likely b/c mnemonic difficulties.
        return new EmptyView();
      } else {
        // Generate a visual keyboard from the layout (or layout default)
        // Condition is false if no key definitions exist, formFactor == desktop, AND help text exists.  All three.
        if(keyboard && keyboard.layout(device.formFactor as utils.FormFactor)) {
          return this._GenerateVisualKeyboard(keyboard);
        } else if(!keyboard /* && device.touchable (implied) */) {
          // Show a basic, "hollow" OSK that at least allows input, since we're
          // on a touch device and hiding the system keyboard
          return this._GenerateVisualKeyboard(null);
        } else {
          // A keyboard help-page or help-text is still a visualization, even not a standard OSK.
          return new HelpPageView(keyboard);
        }
      }
    }

    /**
     * Function     _GenerateVisualKeyboard
     * Scope        Private
     * @param       {Object}      keyboard    The keyboard to visualize
     * Description  Generates the visual keyboard element and attaches it to KMW
     */
    private _GenerateVisualKeyboard(keyboard: keyboards.Keyboard): VisualKeyboard {
      let device = com.keyman.singleton.util.device;

      // Root element sets its own classes, one of which is 'kmw-osk-inner-frame'.
      let vkbd = new VisualKeyboard(keyboard, device);

      // Ensure the OSK's current layer is kept up to date.
      let core = com.keyman.singleton.core; // Note:  will eventually be a class field.
      core.keyboardProcessor.layerStore.handler = this.layerChangeHandler;

      // Set box class - OS and keyboard added for Build 360
      this._Box.className=device.formFactor+' '+ device.OS.toLowerCase() + ' kmw-osk-frame';

      // Add primary keyboard element to OSK
      return vkbd;
    }
  }
}