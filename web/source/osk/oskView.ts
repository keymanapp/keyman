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

  export enum ActivationMode {
    static      = "static",  // For use by documentation keyboards, eventually.
    manual      = "manual",
    automatic = "automatic"
  }

  export abstract class OSKView {
    _Box: HTMLDivElement;

    headerView:   OSKViewComponent;
    bannerView:   BannerManager; // Which implements OSKViewComponent
    keyboardView: KeyboardView;  // Which implements OSKViewComponent
    footerView:   OSKViewComponent;

    protected device: com.keyman.utils.DeviceSpec;
    protected readonly hostDevice: com.keyman.utils.DeviceSpec;

    private _boxBaseMouseDown:        (e: MouseEvent) => boolean;
    private _boxBaseTouchStart:       (e: TouchEvent) => boolean;
    private _boxBaseTouchEventCancel: (e: TouchEvent) => boolean;

    private keyboard: keyboards.Keyboard;
    private lgMenu?: LanguageMenu; // only used on non-embedded paths.

    private _target:  text.OutputTarget;

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

    //
    private _activationMode: ActivationMode = ActivationMode.automatic;
    private _displayIfActive: boolean = true;

    private _animatedHideTimeout: number;

    constructor(deviceSpec: com.keyman.utils.DeviceSpec, hostDevice?: com.keyman.utils.DeviceSpec) {
      this.device = deviceSpec;

      if(!hostDevice) {
        hostDevice = deviceSpec;
      }
      this.hostDevice = hostDevice;

      // OSK initialization - create DIV and set default styles
      this._Box = document.createElement('div');   // Container for OSK (Help DIV, displayed when user clicks Help icon)
      this._Box.style.userSelect = 'none';

      // Initializes the two constant OSKComponentView fields.
      this.bannerView   = new BannerManager(this.hostDevice);
      this.keyboardView = null;

      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      // Install the default OSK stylesheet
      util.linkStyleSheet(keymanweb.getStyleSheetPath('kmwosk.css'));

      this.setBaseMouseEventListeners();
      if(hostDevice.touchable) {
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

    /**
     * Gets and sets the IME-like interface (`OutputTarget`) to be affected by events from
     * the OSK.
     *
     * If `activationMode` is `'conditional'`, this property's state controls the visibility
     * of the OSKView.
     */
    public get activeTarget(): text.OutputTarget {
      return this._target;
    }

    public set activeTarget(targ: text.OutputTarget) {
      // If already null & set to null again, take no action.
      if(this._target == null && targ == null) {
        return;
      }

      this._target = targ;
      this.commonCheckAndDisplay();
    }


    public get targetDevice(): com.keyman.utils.DeviceSpec {
      return this.device;
    }

    public set targetDevice(spec: com.keyman.utils.DeviceSpec) {
      if(this.allowsDeviceChange(spec)) {
        this.device = spec;
        this.loadActiveKeyboard();
      } else {
        console.error("May not change target device for this OSKView type.");
      }
    }

    protected allowsDeviceChange(newSpec: com.keyman.utils.DeviceSpec): boolean {
      return false;
    }

    /**
     * Determines the activation state model used to control presentation of the OSK.
     * - `'conditional'`:  Only displays if `activeTarget` is non-null - if there is an active
     * target that can receive the OSK's context-manipulation events.
     * - `'manual'`:  Display is directly controlled by manipulating the value of `displayIfActive`.
     *   It may be displayed while `activeTarget` is `null`.
     * - `'static'`:  The OSK should be permanently displayed and may never be hidden.
     */
    get activationMode(): ActivationMode {
      if(!this._activationMode) {
        this._activationMode = ActivationMode.automatic;
      }

      return this._activationMode;
    }

    set activationMode(mode: ActivationMode) {
      this._activationMode = mode;
      this.commonCheckAndDisplay();
    }

    /**
     * Implementation of the activation modeling described in the documentation for
     * `activationMode`.
     */
    protected get activationConditionsMet(): boolean {
      switch(this.activationMode) {
        case 'manual':
          return true;
        case 'static':
          return true;
        case 'automatic':
          return !!this.activeTarget;
        default:
          console.error("Unexpected activation mode set for the OSK.");
          return false;
      }
    }

    /**
     * A property denoting whether or not the OSK should be presented if it meets its
     * activation conditions.
     *
     * When `activationMode == 'manual'`, `displayIfActive == true` is the lone
     * activation condition.
     *
     * Note: cannot be set to `false` if `activationMode == 'static'`.
     */
    get displayIfActive(): boolean {
      return this._displayIfActive;
    }

    set displayIfActive(flag: boolean) {
      if(this.displayIfActive == flag) {
        return;
      }

      // if is touch device or is CJK keyboard, this.displayIfActive must remain true.
      if(this.keyboard?.isCJK && !flag) {
        console.warn("Cannot hide display of OSK for CJK keyboards.");
        flag = true;
      } else if(this.hostDevice.touchable && !flag) {
        console.warn("Cannot hide display of OSK when hosted on touch-based devices.");
        flag = true;
      } else if(this.activationMode == 'static') {
        // Silently fail; it's a documentation keyboard.
        // This is the primary difference between 'manual' and 'static'.
        flag = true;
      }

      this._displayIfActive = flag;
      this.commonCheckAndDisplay();
    }

    /**
     * Used by the activation & visibility properties as a common helper; all of their
     * setters rely on this function to manage presentation (showing / hiding) of the OSK.
     */
    private commonCheckAndDisplay() {
      if(this.activationConditionsMet && this.displayIfActive) {
        this.present();
      } else {
        this.startHide(false);
      }
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
    get width(): ParsedLengthStyle {
      return this._width;
    }

    /**
     * The configured height for this VisualKeyboard.  May be `undefined` or `null`
     * to allow automatic height scaling.
     */
    get height(): ParsedLengthStyle {
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
      return this.parsedBaseFontSize?.styleString || '';
    }

    protected get parsedBaseFontSize(): ParsedLengthStyle {
      if(!this._baseFontSize) {
        let keymanweb = com.keyman.singleton;
        this._baseFontSize = OSKView.defaultFontSize(this.device, this.computedHeight, keymanweb.isEmbedded);
      }

      return this._baseFontSize;
    }

    public static defaultFontSize(device: utils.DeviceSpec, computedHeight: number, isEmbedded: boolean): ParsedLengthStyle {
      if(device.touchable) {
        const fontScale = device.formFactor == 'phone'
          ? 1.6 * (isEmbedded ? 0.65 : 0.6) * 1.2  // Combines original scaling factor with one previously applied to the layer group.
          : 2; // iPad or Android tablet
        return ParsedLengthStyle.special(fontScale, 'em');
      } else {
        return computedHeight ? ParsedLengthStyle.inPixels(computedHeight / 8) : undefined;
      }
    }

    public get activeKeyboard(): keyboards.Keyboard {
      return this.keyboard;
    }

    public set activeKeyboard(keyboard: keyboards.Keyboard) {
      this.keyboard = keyboard;
      this.loadActiveKeyboard();

      if(this.keyboard?.isCJK) {
        this.displayIfActive = true;
      }
    }

    private computeFrameHeight(): number {
      return (this.headerView?.layoutHeight.val || 0) + (this.footerView?.layoutHeight.val || 0);
    }

    setSize(width?: number | LengthStyle, height?: number | LengthStyle, pending?: boolean) {
      let mutatedFlag = false;

      let parsedWidth: ParsedLengthStyle;
      let parsedHeight: ParsedLengthStyle;

      if(!width && width !== 0) {
        return;
      }

      if(!height && height !== 0) {
        return;
      }

      if(Number.isFinite(width as number)) {
        parsedWidth = ParsedLengthStyle.inPixels(width as number);
      } else {
        parsedWidth = new ParsedLengthStyle(width as LengthStyle);
      }

      if(Number.isFinite(height as number)) {
        parsedHeight = ParsedLengthStyle.inPixels(height as number);
      } else {
        parsedHeight = new ParsedLengthStyle(height as LengthStyle);
      }

      if(width && height) {
        mutatedFlag = !this._width || !this._height;

        mutatedFlag = mutatedFlag || parsedWidth.styleString  != this._width.styleString;
        mutatedFlag = mutatedFlag || parsedHeight.styleString != this._height.styleString;

        this._width = parsedWidth;
        this._height = parsedHeight;
      }

      this.needsLayout = this.needsLayout || mutatedFlag;
      this.refreshLayoutIfNeeded(pending);
    }

    public setNeedsLayout() {
      this.needsLayout = true;
    }

    public refreshLayout(pending?: boolean): void {
      if(!this.keyboardView) {
        return;
      }

      // Step 1:  have the necessary conditions been met?
      const hasDimensions = this.width && this.height;
      const fixedSize = hasDimensions && this.width.absolute && this.height.absolute;
      const computedStyle = getComputedStyle(this._Box);
      const isInDOM = computedStyle.height != '' && computedStyle.height != 'auto';

      // Step 2:  determine basic layout geometry
      if(fixedSize) {
        this._computedWidth  = this.width.val;
        this._computedHeight = this.height.val;
      } else if(isInDOM && hasDimensions) {
        const parent = this._Box.offsetParent as HTMLElement;
        this._computedWidth  = this.width.val  * (this.width.absolute  ? 1 : parent.offsetWidth);
        this._computedHeight = this.height.val * (this.height.absolute ? 1 : parent.offsetHeight);
      } else {
        // Cannot perform layout operations!
        return;
      }

      // Must be set before any references to the .computedWidth and .computedHeight properties!
      this.needsLayout = false;

      // Step 3:  perform layout operations.
      this.banner.element.style.fontSize = this.baseFontSize;
      if(this.vkbd) {
        this.vkbd.fontSize = this.parsedBaseFontSize;
      }

      if(!pending) {
        this.headerView?.refreshLayout();
        this.bannerView.refreshLayout();
        this.footerView?.refreshLayout();
      }

      if(this.vkbd) {
        let availableHeight = this.computedHeight - this.computeFrameHeight();

        // +5:  from kmw-banner-bar's 'top' attribute when active
        if(this.bannerView.height > 0) {
          availableHeight -= this.bannerView.height + 5;
        }
        this.vkbd.setSize(this.computedWidth, availableHeight, pending);

        const bs = this._Box.style;
        // OSK size settings can only be reliably applied to standard VisualKeyboard
        // visualizations, not to help text or empty views.
        bs.width  = bs.maxWidth  = this.computedWidth + 'px';
        bs.height = bs.maxHeight = this.computedHeight + 'px';

        // Ensure that the layer's spacebar is properly captioned.
        this.vkbd.showLanguage();
      } else {
        const bs = this._Box.style;
        bs.width  = 'auto';
        bs.height = 'auto';
        bs.maxWidth = bs.maxHeight = '';
      }
    }

    public refreshLayoutIfNeeded(pending?: boolean) {
      if(this.needsLayout) {
        this.refreshLayout(pending);
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

    protected abstract setBoxStyling(): void;

    private loadActiveKeyboard() {
      this.setBoxStyling();

      if(this.vkbd) {
        this.vkbd.shutdown();
      }
      this.keyboardView = null;
      this.needsLayout = true;

      // Instantly resets the OSK container, erasing / delinking the previously-loaded keyboard.
      this._Box.innerHTML = '';

      // Any event-cancelers would go here, after the innerHTML reset.

      // Add header element to OSK only for desktop browsers
      if(this.headerView) {
        this._Box.appendChild(this.headerView.element);
      }

      // Add suggestion banner bar to OSK
      this._Box.appendChild(this.banner.element);

      let kbdView: KeyboardView = this.keyboardView = this._GenerateKeyboardView(this.activeKeyboard);
      this._Box.appendChild(kbdView.element);
      kbdView.postInsert();

      // Add footer element to OSK only for desktop browsers
      if(this.footerView) {
        this._Box.appendChild(this.footerView.element);
      }
      // END:  construction of the actual internal layout for the overall OSK

      this.banner.appendStyles();

      if(this.vkbd) {
        // Create the key preview (for phones)
        this.vkbd.createKeyTip();
        // Create the globe hint (for embedded contexts; has a stub for other contexts)
        this.vkbd.createGlobeHint();

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
        this.vkbd._UpdateVKShiftStyle(newValue);
      }

      if((this.vkbd && this.vkbd.layerId != newValue) || source.value != newValue) {
        // Prevents console errors when a keyboard only displays help.
        // Can occur when using SHIFT with sil_euro_latin on a desktop form-factor.
        if(this.vkbd) {
          this.vkbd.layerId = newValue;
          // Ensure that the layer's spacebar is properly captioned.
          this.vkbd.showLanguage();
        }

        // Ensure the keyboard view is modeling the correct state.  (Correct layer, etc.)
        this.keyboardView.updateState();
        // We need to recalc the font size here because the layer did not have
        // calculated dimensions available before it was visible
        this.refreshLayout();
      }
    }.bind(this);

    private _GenerateKeyboardView(keyboard: keyboards.Keyboard): KeyboardView {
      let device = this.device;

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
      let device = this.device;

      // Root element sets its own classes, one of which is 'kmw-osk-inner-frame'.
      let vkbd = new VisualKeyboard(keyboard, device, this.hostDevice);

      // Ensure the OSK's current layer is kept up to date.
      let core = com.keyman.singleton.core; // Note:  will eventually be a class field.
      core.keyboardProcessor.layerStore.handler = this.layerChangeHandler;

      // Set box class - OS and keyboard added for Build 360
      this._Box.className=device.formFactor+' '+ device.OS.toLowerCase() + ' kmw-osk-frame';

      // Add primary keyboard element to OSK
      return vkbd;
    }

    /**
     * The main function for presenting the OSKView.
     *
     * This includes:
     * - refreshing its layout
     * - displaying it
     * - positioning it
     */
    public present(): void {
      // Do not try to display OSK if no active element
      if(!this.mayShow()) {
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

      this._Visible=true;

      /* In case it's still '0' from a hide() operation.
       *
       * (Opacity is only modified when device.touchable = true,
       * though a couple of extra conditions may apply.)
       */
      this._Box.style.opacity = '1';

      // If OSK still hidden, make visible only after all calculation finished
      if(this._Box.style.visibility == 'hidden') {
        let _this = this;
        window.setTimeout(function() {
          _this._Box.style.visibility = 'visible';
        }, 0);
      }

      this.setDisplayPositioning();
    }

    /**
     * Method usable by subclasses of OSKView to control that OSKView type's
     * positioning behavior when needed by the present() method.
     */
    protected abstract setDisplayPositioning();

    /**
     * Method used to start a potentially-asynchronous hide of the OSK.
     * @param hiddenByUser `true` if this hide operation was directly requested by the user.
     */
    public startHide(hiddenByUser: boolean): void {
      if(!this.mayHide(hiddenByUser)) {
        return;
      }

      if(hiddenByUser) {
        // The one location outside of the `displayIfActive` property that bypasses the setter.
        // Avoids needless recursion that could be triggered by it, as we're already in the
        // process of hiding the OSK anyway.
        this._displayIfActive = ((this.keyboard.isCJK || this.hostDevice.touchable)? true : false); // I3363 (Build 301)
      }

      let promise: Promise<boolean> = null;
      if(this._Box && this.hostDevice.touchable && !(this.keyboardView instanceof EmptyView)) {
        /**
         * Note:  this refactored code appears to reflect a currently-dead code path.  14.0's
         * equivalent is either extremely niche or is actually inaccessible.
         */
        promise = this.useHideAnimation();
      } else {
        promise = Promise.resolve(true);
      }

      const _this = this;
      promise.then(function(shouldHide: boolean) {
        if(shouldHide) {
          _this.finalizeHide();
        }
      });

      // Allow UI to execute code when hiding the OSK
      var p={};
      p['HiddenByUser']=hiddenByUser;
      this.doHide(p);

      // If hidden by the UI, be sure to restore the focus
      if(hiddenByUser && this.activeTarget instanceof dom.targets.OutputTarget) {
        this.activeTarget?.focus();
      }
    }

    /**
     * Performs the _actual_ logic and functionality involved in hiding the OSK.
     */
    protected finalizeHide() {
      if(document.body.className.indexOf('osk-always-visible') >= 0) {
        return;
      }

      if(this._Box) {
        let bs=this._Box.style;
        bs.display = 'none';
        bs.transition = '';
        bs.opacity = '1';
        this._Visible=false;
      }

      if(this.vkbd) {
        this.vkbd.onHide();
      }
    }

    /**
     *
     * @returns `false` if the OSK is in an invalid state for being presented to the user.
     */
    protected mayShow(): boolean {
      if(!this.activationConditionsMet) {
        return false;
      }

      // Never display the OSK for desktop browsers unless KMW element is focused, and a keyboard selected
      if(!this.keyboardView || this.keyboardView instanceof EmptyView || !this.displayIfActive) {
        return false;
      }

      if(!this._Box) {
        return false;
      }

      return true;
    }

    /**
     *
     * @param hiddenByUser
     * @returns `false` if the OSK is in an invalid state for being hidden from the user.
     */
    protected mayHide(hiddenByUser: boolean): boolean {
      if(this.activationMode != 'automatic' && this.displayIfActive) {
        return false;
      }

      if(!hiddenByUser && this.hostDevice.formFactor == 'desktop') {
        //Allow desktop OSK to remain visible on blur if body class set
        if(document.body.className.indexOf('osk-always-visible') >= 0) {
          return false;
        }
      }

      return true;
    }

    /**
     * Applies CSS styling and handling needed to perform a fade animation when
     * hiding the OSK.
     *
     * Note:  currently reflects an effectively-dead code path, though this is
     * likely not intentional.  Other parts of the KMW engine seem to call hideNow()
     * synchronously after each and every part of the engine that calls this function,
     * cancelling the Promise.
     *
     * @returns A Promise denoting either cancellation of the hide (`false`) or
     * completion of the hide & its animation (`true`)
     */

    protected useHideAnimation(): Promise<boolean> {
      const os = this._Box.style;
      const _this = this;

      return new Promise<boolean>(function(resolve) {
        const cleanup = function() {
          // TODO(lowpri): attach event listeners on create and leave them there
          _this._Box.removeEventListener('transitionend', cleanup, false);
          _this._Box.removeEventListener('webkitTransitionEnd', cleanup, false);
          _this._Box.removeEventListener('transitioncancel', cleanup, false);
          _this._Box.removeEventListener('webkitTransitionCancel', cleanup, false);
          if(_this._animatedHideTimeout != 0) {
            window.clearTimeout(_this._animatedHideTimeout);
          }
          _this._animatedHideTimeout = 0;

          if(_this._Visible && _this.activationConditionsMet) {
            // Leave opacity alone and clear transition if another element activated
            os.transition='';
            os.opacity='1';
            resolve(false);
            return false;
          } else {
            resolve(true);
            return true;
          }
        }, startup = function() {
          _this._Box.removeEventListener('transitionrun', startup, false);
          _this._Box.removeEventListener('webkitTransitionRun', startup, false);
          _this._Box.addEventListener('transitionend', cleanup, false);
          _this._Box.addEventListener('webkitTransitionEnd', cleanup, false);
          _this._Box.addEventListener('transitioncancel', cleanup, false);
          _this._Box.addEventListener('webkitTransitionCancel', cleanup, false);
        };

        _this._Box.addEventListener('transitionrun', startup, false);
        _this._Box.addEventListener('webkitTransitionRun', startup, false);

        os.transition='opacity 0.5s linear 0';
        os.opacity='0';

        // Cannot hide the OSK smoothly using a transitioned drop, since for
        // position:fixed elements transitioning is incompatible with translate3d(),
        // and also does not work with top, bottom or height styles.
        // Opacity can be transitioned and is probably the simplest alternative.
        // We must condition on osk._Visible in case focus has since been moved to another
        // input (in which case osk._Visible will be non-zero)
        _this._animatedHideTimeout = window.setTimeout(cleanup,
          200); // Wait a bit before starting, to allow for moving to another element
      });
    }

    /**
     * Used to synchronously hide the OSK, cancelling any async hide animations that have
     * not started and immediately completing the hide of any hide ops pending completion
     * of their animation.
     */
    public hideNow() {
      if(!this.mayHide(false) || !this._Box) {
        return;
      }

      // Two possible uses for _animatedHideResolver:
      // - _animatedHideTimeout is set:   animation is waiting to start
      // - _animatedHideTimeout is null:  animation has already started.

      // Was an animated hide waiting to start?  Just cancel it.
      if(this._animatedHideTimeout) {
        window.clearTimeout(this._animatedHideTimeout);
        this._animatedHideTimeout = 0;
      }

      // Was an animated hide already in progress?  If so, just trigger it early.
      const os = this._Box.style;
      os.transition='';
      os.opacity='0';
      this.finalizeHide();
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
          +'<span style="font-size:0.8em">Copyright &copy; 2021 SIL International</span>');
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
      if(this.hostDevice.touchable) {
        this.lgMenu = new LanguageMenu(com.keyman.singleton);
        this.lgMenu.show();
      }
    }

    hideLanguageMenu() {
      this.lgMenu?.hide();
      this.lgMenu = null;
    }

    // OSK state fields & events
    //
    // These are relatively stable and may be preserved as they are.
    _Visible: boolean = false;

    /**
     * Function     enabled
     * Scope        Public
     * @return      {boolean|number}    True if KMW OSK enabled
     * Description  Test if KMW OSK is enabled
     */
     ['isEnabled'](): boolean {
      return this.displayIfActive;
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
      this.displayIfActive = false;
      this.startHide(true);
    }

    /**
     * Description  Display KMW OSK (at position set in callback to UI)
     * Function     show
     * Scope        Public
     * @param       {(boolean|number)=}      bShow     True to display, False to hide, omitted to toggle
     */
     ['show'](bShow?: boolean) {
      if(arguments.length > 0) {
        this.displayIfActive = bShow;
      } else {
        if(this.activationConditionsMet) {
          this.displayIfActive = !this.displayIfActive;
        }
      }
    }

    /**
     * Allow UI to respond to OSK being shown (passing position and properties)
     *
     * @param       {Object=}       p     object with coordinates and userdefined flag
     * @return      {boolean}
     *
     */
     doShow(p) {
      return com.keyman.singleton.util.callEvent('osk.show',p);
    }

    /**
     * Allow UI to update respond to OSK being hidden
     *
     * @param       {Object=}       p     object with coordinates and userdefined flag
     * @return      {boolean}
     *
     */
    doHide(p) {
      return com.keyman.singleton.util.callEvent('osk.hide',p);
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
      // As the following title bar buttons (for desktop / FloatingOSKView) do nothing unless
      // a site designer uses these events, we disable / hide them until an event is attached.
      let titleBar = this.headerView;
      if(titleBar && titleBar instanceof layouts.TitleBar) {
        switch(event) {
          case 'configclick':
            titleBar.configEnabled = true;
            break;
          case 'helpclick':
            titleBar.helpEnabled = true;
            break;
        }
      }

      return com.keyman.singleton.util.addEventListener('osk.'+event, func);
    }
  }
}