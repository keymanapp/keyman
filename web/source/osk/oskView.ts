// Includes the banner
/// <reference path="./bannerManager.ts" />

// Generates the visual keyboard specific to each keyboard.  (class="kmw-osk-inner-frame")
/// <reference path="visualKeyboard.ts" />
// Models keyboards that present a help page, rather than a standard OSK.
/// <reference path="helpPageView.ts" />
/// <reference path="emptyView.ts" />

namespace com.keyman.osk {
  export type OSKRect = {
    'left'?: number,
    'top'?: number,
    'width'?: number,
    'height'?: number,
    'nosize'?: boolean,
    'nomove'?: boolean
  };

  export abstract class OSKView {
    _Box: HTMLDivElement;

    headerView:   OSKViewComponent;
    bannerView:   BannerManager; // Which implements OSKViewComponent
    keyboardView: KeyboardView;  // Which implements OSKViewComponent
    footerView:   OSKViewComponent;

    protected device: com.keyman.utils.DeviceSpec;

    private _boxBaseMouseDown:        (e: MouseEvent) => boolean; 
    private _boxBaseTouchStart:       (e: TouchEvent) => boolean;
    private _boxBaseTouchEventCancel: (e: TouchEvent) => boolean;

    private keyboard: keyboards.Keyboard;
    
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

      this.setBaseMouseEventListeners();
      if(deviceSpec.touchable) {
        this.setBaseTouchEventListeners();
      }

      // Register a listener for model change events so that we can hot-swap the banner as needed.
      // Handled here b/c banner changes may trigger a need to re-layout the OSK.
      const _this = this;
      keymanweb.core.languageProcessor.on('statechange', 
                                          function(state: text.prediction.StateChangeEnum) {
        let currentType = _this.bannerView.activeType;
        _this.bannerView.selectBanner(state);

        if(currentType != _this.bannerView.activeType) {
          _this.refreshLayout();
        }

        return true;
      });
    }

    private setBaseMouseEventListeners() {
      let keymanweb = com.keyman.singleton;

      this._boxBaseMouseDown = function(e) {
        keymanweb.uiManager.setActivatingUI(true);
        return false;
      }

      this._Box.addEventListener('mousedown', this._boxBaseMouseDown, false);
    }

    private removeBaseMouseEventListeners() {
      if(this._boxBaseMouseDown) {
        this._Box.removeEventListener('mousedown', this._boxBaseMouseDown, false);
        this._boxBaseMouseDown = null;
      }
    }

    private setBaseTouchEventListeners() {
      // And to prevent touch event default behaviour on mobile devices
      let keymanweb = com.keyman.singleton;

      var cancelEventFunc = this._boxBaseTouchEventCancel = function(e) {
        if(e.cancelable) {
          e.preventDefault();
        }
        e.stopPropagation();
        return false;
      };

      this._boxBaseTouchStart = function(e) {
        keymanweb.uiManager.setActivatingUI(true);
        return cancelEventFunc(e);
      }

      this._Box.addEventListener('touchstart', this._boxBaseTouchStart, false);
      this._Box.addEventListener('touchmove',  this._boxBaseTouchEventCancel, false);
      this._Box.addEventListener('touchend',  this._boxBaseTouchEventCancel, false);
      this._Box.addEventListener('touchcancel',  this._boxBaseTouchEventCancel, false);
    }

    private removeBaseTouchEventListeners() {
      if(!this._boxBaseTouchEventCancel) {
        return;
      }

      this._Box.removeEventListener('touchstart', this._boxBaseTouchStart, false);
      this._Box.removeEventListener('touchmove',  this._boxBaseTouchEventCancel, false);
      this._Box.removeEventListener('touchend',  this._boxBaseTouchEventCancel, false);
      this._Box.removeEventListener('touchcancel',  this._boxBaseTouchEventCancel, false);

      this._boxBaseTouchEventCancel = null;
      this._boxBaseTouchStart = null;
    }

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
      this.headerView?.refreshLayout();
      this.bannerView.refreshLayout();
      this.footerView?.refreshLayout();

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

    private layerChangeHandler: text.SystemStoreMutationHandler = function(this: OSKView,
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
          // Ensure that the layer's spacebar is properly captioned.
          this.vkbd.showLanguage();
        }

        // Ensure the keyboard view is modeling the correct state.  (Correct layer, etc.)
        this.keyboardView.updateState();
        this.refreshLayoutIfNeeded();
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

    // Corresponds to the desktop OSK's _Show, but acts as a core, common method 
    // usable by all display patterns.
    protected makeVisible() {
      // Do not try to display/render the OSK if undefined or no keyboard is loaded.
      if(!this._Box || !this.keyboardView) {
        return;
      }

      // Ensure the keyboard view is modeling the correct state.  (Correct layer, etc.)
      this.keyboardView.updateState();

      this._Box.style.display='block'; // Is 'none' when hidden.

      // First thing after it's made visible.
      this.refreshLayoutIfNeeded();

      if(this.keyboardView instanceof VisualKeyboard) {
        this.keyboardView.showLanguage();
      }

      // If OSK still hidden, make visible only after all calculation finished
      if(this._Box.style.visibility == 'hidden') {
        let _this = this;
        window.setTimeout(function() {
          _this._Box.style.visibility = 'visible';
        }, 0);
      }
    }

    protected makeHidden(hiddenByUser: boolean) {
      // Save current size if visible
      if(this._Box && this._Box.style.display == 'block' && this.keyboardView instanceof VisualKeyboard) {
        this.keyboardView.refit();
      }

      if(this._Box) {
        this._Box.style.display = 'none';
      }
    }

    ['shutdown']() {
      // Disable the OSK's event handlers.
      this.removeBaseMouseEventListeners();
      this.removeBaseTouchEventListeners();

      // Remove the OSK's elements from the document, allowing them to be properly cleaned up.
      // Necessary for clean engine testing.
      var _box = this._Box;
      if(_box.parentElement) {
        _box.parentElement.removeChild(_box);
      }
    }

    /**
     * Function     getRect
     * Scope        Public
     * @return      {Object.<string,number>}   Array object with position and size of OSK container
     * Description  Get rectangle containing KMW Virtual Keyboard
     */
     ['getRect'](): OSKRect {		// I2405
      var p: OSKRect = {};

      // Always return these based upon _Box; using this.vkbd will fail to account for banner and/or
      // the desktop OSK border.
      p['left'] = p.left = dom.Utils.getAbsoluteX(this._Box);
      p['top']  = p.top  = dom.Utils.getAbsoluteY(this._Box);

      p['width'] = this.computedWidth;
      p['height'] = this.computedHeight;
      return p;
    }

    /* ---- Legacy interfacing methods and fields ----
     *
     * The endgoal is to eliminate the need for these entirely, but extra work and care
     * will be necessary to achieve said endgoal for these methods.
     *
     * The simplest way forward is to maintain them, then resolve them independently,
     * one at a time.
     */
  
    /*
     * Display KMW OSK at specified position (returns nothing)
     * 
     * The positioning parameters only make sense for the FloatingOSKView type;
     * other implementations should use no parameters whatsoever.
     *
     * @param       {number=}     Px      x-coordinate for OSK rectangle
     * @param       {number=}     Py      y-coordinate for OSK rectangle
     */
    public abstract _Show(Px?: number, Py?: number);

    /**
     * Hide Keymanweb On Screen Keyboard
     *
     * @param       {boolean}   hiddenByUser    Distinguish between hiding on loss of focus and explicit hiding by user
     */
    public abstract _Hide(hiddenByUser: boolean);

    /**
     * Display build number
     * 
     * In the future, this should raise an event that the consuming KeymanWeb
     * engine may listen for & respond to, rather than having it integrated
     * as part of the OSK itself. 
     */
    showBuild() {
      let keymanweb = com.keyman.singleton;
      keymanweb.util.internalAlert('KeymanWeb Version '+keymanweb['version']+'.'+keymanweb['build']+'<br /><br />'
          +'<span style="font-size:0.8em">Copyright &copy; 2017 SIL International</span>');
    }

    /**
     * Display list of installed keyboards in pop-up menu
     * 
     * In the future, this language menu should be defined as a UI module like the standard
     * desktop UI modules.  The globe key should then trigger an event to _request_ that the
     * consuming engine display the active UI module's menu.
     * 
     **/
    showLanguageMenu() {
      let menu = new LanguageMenu(com.keyman.singleton);
      menu.show();
    }

    /**
     * Function     hideNow
     * Scope        Private
     * Description  Hide the OSK unconditionally and immediately, cancel any pending transition
     * 
     * Usages:
     * - during rotations to temporarily hide the OSK during layout ops
     * - when controls lose focus (N/A to embedded mode)
     * 
     * Somewhat conflated with the _Show / _Hide methods, which often serve more as an
     * "enable" vs "disable" feature on the OSK - though that distinction isn't super-clear.
     * 
     * Definitely needs clearer design & modeling, at the least.
     */
    hideNow: () => void = function(this: OSKView) { // I3363 (Build 301)
      this._Box.removeEventListener('transitionend', this.hideNow, false);
      this._Box.removeEventListener('webkitTransitionEnd', this.hideNow, false);

      if(document.body.className.indexOf('osk-always-visible') >= 0) {
        return;
      }

      var os=this._Box.style;
      os.display='none';
      os.opacity='1';
      this._Visible=false;
      os.transition=os.msTransition=os.MozTransition=os.WebkitTransition='';

      if(this.vkbd) {
        this.vkbd.onHide();
      }
    }.bind(this);

    // OSK state fields
    //
    // They're not very well defined or encapsulated; there's definitely room for more
    // "polish" here.
    _Visible: boolean = false;
    _Enabled: boolean = true;

    /**
     * Function     enabled
     * Scope        Public
     * @return      {boolean|number}    True if KMW OSK enabled
     * Description  Test if KMW OSK is enabled
     */
     ['isEnabled'](): boolean {
      return this._Enabled;
    }

    /**
     * Function     isVisible
     * Scope        Public
     * @return      {boolean|number}    True if KMW OSK visible
     * Description  Test if KMW OSK is actually visible
     * Note that this will usually return false after any UI event that results in (temporary) loss of input focus
     */
    ['isVisible'](): boolean {
      return this._Visible;
    }

    /**
     * Function     hide
     * Scope        Public
     * Description  Prevent display of OSK window on focus
     */
     ['hide']() {
      this._Enabled = false;
      this._Hide(true);
    }

    /**
     * Description  Display KMW OSK (at position set in callback to UI)
     * Function     show
     * Scope        Public
     * @param       {(boolean|number)=}      bShow     True to display, False to hide, omitted to toggle
     */
     ['show'](bShow: boolean) {
      if(arguments.length > 0) {
        this._Enabled=bShow;
        if(bShow) {
          this._Show();
        } else {
          this._Hide(true);
        }
      } else {
        if(this._Visible) {
          this._Hide(true);
        } else {
          this._Show();
        }
      }
    }

    /**
     * Function     addEventListener
     * Scope        Public
     * @param       {string}            event     event name
     * @param       {function(Object)}  func      event handler
     * @return      {boolean}
     * Description  Wrapper function to add and identify OSK-specific event handlers
     */
    ['addEventListener'](event: string, func: (obj) => boolean) {
      return com.keyman.singleton.util.addEventListener('osk.'+event, func);
    }
  }
}