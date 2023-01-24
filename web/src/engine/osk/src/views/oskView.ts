import EventEmitter, { ArgumentMap } from 'eventemitter3';

import BannerView, { BannerController } from '../banner/bannerView.js';
import { SuggestionBanner } from '../banner/banner.js';
import OSKViewComponent from '../components/oskViewComponent.interface.js';
import EmptyView from '../components/emptyView.js';
import HelpPageView from '../components/helpPageView.js';
import KeyboardView from '../components/keyboardView.interface.js';
import VisualKeyboard from '../visualKeyboard.js';
import { LengthStyle, ParsedLengthStyle } from '../lengthStyle.js';

import DeviceSpec from '@keymanapp/web-utils/build/obj/deviceSpec.js';
import Keyboard from '@keymanapp/keyboard-processor/build/obj/keyboards/keyboard.js';

import { getAbsoluteX, getAbsoluteY } from 'keyman/build/engine/dom-utils/obj/getAbsolute.js';
import createUnselectableElement from 'keyman/build/engine/dom-utils/obj/createUnselectableElement.js';
import KeyEvent from '@keymanapp/keyboard-processor/build/obj/text/keyEvent.js';
import TitleBar from '../components/titleBar.js';
import { StylesheetManager } from 'keyman/build/engine/dom-utils/obj/stylesheets.js';
import Configuration from '../config/viewConfiguration.js';
import KeyboardProperties from '../keyboardProperties.js';
import Activator from './activator.js';
import ManagedPromise from '../managedPromise.js';
import TouchEventPromiseMap from './touchEventPromiseMap.js';

// These will likely be eliminated from THIS file at some point.
import type { SystemStoreMutationHandler, MutableSystemStore } from '@keymanapp/keyboard-processor/build/obj/text/systemStores.js';
import { StateChangeEnum } from '@keymanapp/input-processor/build/obj/text/prediction/languageProcessor.js';
import PredictionContext from '@keymanapp/input-processor/build/obj/text/prediction/predictionContext.js';

export type OSKPos = {'left'?: number, 'top'?: number};

export type OSKRect = {
  'left'?: number,
  'top'?: number,
  'width'?: number,
  'height'?: number,
  'nosize'?: boolean,
  'nomove'?: boolean
};

export interface LegacyOSKEventMap {
  'osk.configclick'(obj: {});
  'osk.helpclick'(obj: {});
  'osk.resizemove'(obj: {});
  'osk.show'(obj: {});
  'osk.hide'(obj: {});
}

interface EventMap {
  /**
   * Designed to pass key events off to any consuming modules/libraries.
   *
   * Note:  the following code block was originally used to integrate with the keyboard & input
   * processors, but it requires entanglement with components external to this OSK module.
   *
   ```
    let keyman = com.keyman.singleton;
    var Lelem = keyman.domManager.lastActiveElement;

    if(Lelem != null) {
      // Handle any DOM state management related to click inputs.
      let outputTarget = dom.Utils.getOutputTarget(Lelem);
      keyman.domManager.initActiveElement(Lelem);

      // Clear any cached codepoint data; we can rebuild it if it's unchanged.
      outputTarget.invalidateSelection();
      // Deadkey matching continues to be troublesome.
      // Deleting matched deadkeys here seems to correct some of the issues.   (JD 6/6/14)
      outputTarget.deadkeys().deleteMatched();      // Delete any matched deadkeys before continuing

      if(!keyman.isEmbedded) {
        keyman.uiManager.setActivatingUI(true);
        com.keyman.dom.DOMEventHandlers.states._IgnoreNextSelChange = 100;
        keyman.domManager.focusLastActiveElement();
        com.keyman.dom.DOMEventHandlers.states._IgnoreNextSelChange = 0;
      }

      let retVal = !!keyman.core.processKeyEvent(Lkc, outputTarget);

      // Now that processing is done, we can do a bit of post-processing, too.
      keyman.uiManager.setActivatingUI(false);	// I2498 - KeymanWeb OSK does not accept clicks in FF when using automatic UI
      return retVal;
    } else {
      return true;
    }
   ```
   */
  'keyEvent': (event: KeyEvent) => void,

  /**
   * Allows passing data needed for legacy-style events as specified by our published Keyman Engine for Web API.
   *
   * @param eventName The full name of the event ('osk.' + specific event)
   * @param args The event's argument object.
   */
  legacyevent<T extends keyof LegacyOSKEventMap>(eventName: T, arg: {});
  // TSC fell over when trying to emit via this definition.  VS code worked decently well with it, though!  :(
  //legacyevent<T extends keyof LegacyOSKEventMap>(eventName: T, ...args: ArgumentMap<LegacyOSKEventMap>[Extract<T, keyof LegacyOSKEventMap>]): void;

  onshow(): void;
  /**
   *
  ```
    // If hidden by the UI, be sure to restore the focus
    if(hiddenByUser && this.activeTarget) {
      this.activeTarget?.focus();
    }
  ```
   */
  onhide(hiddenByUser: boolean): void;

  /**
   ```
// Display list of installed keyboards in pop-up menu

// In the future, this language menu should be defined as a UI module like the standard
// desktop UI modules.  The globe key should then trigger an event to _request_ that the
// consuming engine display the active UI module's menu.

showLanguageMenu() {
  if(this.hostDevice.touchable) {
    this.lgMenu = new LanguageMenu(com.keyman.singleton);
    this.lgMenu.show();
  }
}
  ```
   */
  shouldShowLanguageMenu: (e: HTMLElement) => void;
  shouldHideLanguageMenu: () => void;

  /**
   * This event is raised when the OSK's 'config' button is clicked.
   * Adding a listener for the event will cause the 'config' button to be displayed for
   * FloatingOSKView instances.
   */
  showConfig: () => void;

  /**
   * This event is raised when the OSK's 'help' button is clicked.
   * Adding a listener for the event will cause the 'help' button to be displayed for
   * FloatingOSKView instances.
   */
  showHelp: () => void;

  /**
   * Signals the special command to display the engine's version + build number.
   *
   * The eventual handler should be of the following form:
   *
   ```
  showBuild() {
    let keymanweb = com.keyman.singleton;

    keymanweb.util.internalAlert('KeymanWeb Version '+keymanweb['version']+'.'+keymanweb['build']+'<br /><br />'
        +'<span style="font-size:0.8em">Copyright &copy; 2021 SIL International</span>');
  }
   ```
   *
   */
  showBuild: () => void;

  /**
   * Signals that the OSK is being moved by the user via a drag operation.
   *
   * The provided Promise will resolve once the drag operation is complete.
   *
   * Former handling, before this was an event:
   ```
  // On event start
  let keymanweb = com.keyman.singleton;
  keymanweb.uiManager.justActivated = true;

  // On promise resolution
  keymanweb.domManager.focusLastActiveElement();

  keymanweb.uiManager.justActivated = false;
  keymanweb.uiManager.setActivatingUI(false);

  // Alternate case using the same event:
  if(isVisible && activeTarget) {
    this.activeTarget.focus();  // I2036 - OSK does not unpin to correct location
  }
  // If too tough to disambiguate, can always make a separate event.
   ```
   */
  dragMove: (promise: Promise<void>) => void;

  /**
   * Signals that the OSK is being resized via a drag operation (on a resize 'handle').
   *
   * The provided Promise will resolve once the resize operation is complete.
   *
   *
   * Former handling, before this was an event:
   ```
   // On event start
   let keymanweb = com.keyman.singleton;
   keymanweb.uiManager.justActivated = true;

   // On promise resolution
   keymanweb.domManager.focusLastActiveElement();

   keymanweb.uiManager.justActivated = false;
   keymanweb.uiManager.setActivatingUI(false);
   ```
   */
  resizeMove: (promise: Promise<void>) => void;


  /**
   * Signals that either the mouse or an active touchpoint is interacting with the OSK.
   *
   * The provided `Promise` will resolve once the corresponding interaction is complete.
   * Note that for touch events, more than one touchpoint may coexist, each with its own
   * corresponding call of this event and corresponding `Promise`.
   *
   * Former handling, before this was an event:
   ```
   // On event start
   keymanweb.uiManager.setActivatingUI(true);

   // On promise resolution
   // noop // did not bother with `setActivatingUI(false)`.  Possible bug?
   ```
   */
  pointerInteraction: (promise: Promise<void>) => void;
}

export default abstract class OSKView extends EventEmitter<EventMap> {
  _Box: HTMLDivElement;

  headerView:   OSKViewComponent;
  bannerView:   BannerView; // Which implements OSKViewComponent
  keyboardView: KeyboardView;  // Which implements OSKViewComponent
  footerView:   OSKViewComponent;

  private _bannerController: BannerController;

  private kbdStyleSheetManager: StylesheetManager;
  private uiStyleSheetManager: StylesheetManager;

  private config: Configuration;

  private _boxBaseMouseDown:        (e: MouseEvent) => boolean;
  private _boxBaseTouchStart:       (e: TouchEvent) => boolean;
  private _boxBaseTouchEventCancel: (e: TouchEvent) => boolean;

  private keyboardData: {
    keyboard: Keyboard,
    metadata: KeyboardProperties
  };

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

  private _animatedHideTimeout: number;

  private mouseEnterPromise?: ManagedPromise<void>;
  private touchEventPromiseManager = new TouchEventPromiseMap();

  constructor(configuration: Configuration) {
    super();

    // Clone the config; do not allow object references to be altered later.
    this.config = configuration = {...configuration};
    this.config.commonStyleSheetRefs = [...configuration.commonStyleSheetRefs];

    this.config.device = configuration.device || configuration.hostDevice;

    this.config.isEmbedded = configuration.isEmbedded || false;
    this.config.activator.on('activate', this.activationListener);

    // OSK initialization - create DIV and set default styles
    this._Box = createUnselectableElement('div');   // Container for OSK (Help DIV, displayed when user clicks Help icon)
    this.kbdStyleSheetManager = new StylesheetManager(this._Box);
    this.uiStyleSheetManager = new StylesheetManager(this._Box);

    // Initializes the two constant OSKComponentView fields.
    this.bannerView   = new BannerView();
    this.bannerView.events.on('bannerchange', () => this.refreshLayout());

    this._bannerController = new BannerController(this.bannerView, this.hostDevice);

    this.keyboardView = null;

    this.setBaseMouseEventListeners();
    if(this.hostDevice.touchable) {
      this.setBaseTouchEventListeners();
    }
  }

  protected get configuration(): Configuration {
    return this.config;
  }

  public get bannerController(): BannerController {
    return this.bannerController;
  }

  public get hostDevice(): DeviceSpec {
    return this.config.hostDevice;
  }

  public get fontRootPath(): string {
    return this.config.fontRootPath;
  }

  public get isEmbedded(): boolean {
    return this.config.isEmbedded;
  }

  /**
   * Function     _VKbdMouseEnter
   * Scope        Private
   * @param       {Object}      e      event
   * Description  Activate the KMW UI when mouse enters the OSK element hierarchy
   */
  private _VKbdMouseEnter: (e: MouseEvent) => void;

  /**
   * Function     _VKbdMouseLeave
   * Scope        Private
   * @param       {Object}      e      event
   * Description  Cancel activation of KMW UI when mouse leaves the OSK element hierarchy
   */
  private _VKbdMouseLeave: (e: MouseEvent) => void;

  private setBaseMouseEventListeners() {
    this._Box.onmouseenter = this._VKbdMouseEnter = (e) => {
      if(this.mouseEnterPromise) {
        // The chain was somehow interrupted, with the mouseleave never occurring!
        this.mouseEnterPromise.resolve();
      }

      this.mouseEnterPromise = new ManagedPromise<void>();
      this.emit('pointerInteraction', this.mouseEnterPromise.corePromise);
    };

    this._Box.onmouseleave = this._VKbdMouseLeave = (e) => {
      this.mouseEnterPromise.resolve();
      this.mouseEnterPromise = null;
      // com.keyman.singleton.uiManager.setActivatingUI(false);
    };
  }

  private removeBaseMouseEventListeners() {
    this._Box.onmouseenter = null;
    this._Box.onmouseleave = null;
  }

  private setBaseTouchEventListeners() {
    // To prevent touch event default behaviour on mobile devices
    let commonPrevention = function(e: TouchEvent) {
      if(e.cancelable) {
        e.preventDefault();
      }
      e.stopPropagation();
      return false;
    }

    this._boxBaseTouchEventCancel = (e) => {
      this.touchEventPromiseManager.maintainTouches(e.touches);
      return commonPrevention(e);
    };

    this._boxBaseTouchStart = (e) => {
      for(let i = 0; i < e.changedTouches.length; i++) {
        let promise = this.touchEventPromiseManager.promiseForTouchpoint(e.changedTouches[i].identifier);
        this.emit('pointerInteraction', promise.corePromise);
      }

      this.touchEventPromiseManager.maintainTouches(e.touches);
      return commonPrevention(e);
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

  // TODO:  activeTarget has been 'moved' to activationModel.activationCondition (for TwoStateActivation instances).
  //        Loosely speaking, anyway.



  public get targetDevice(): DeviceSpec {
    return this.config.device;
  }

  public set targetDevice(spec: DeviceSpec) {
    if(this.allowsDeviceChange(spec)) {
      this.config.device = spec;
      this.loadActiveKeyboard();
    } else {
      console.error("May not change target device for this OSKView type.");
    }
  }

  protected allowsDeviceChange(newSpec: DeviceSpec): boolean {
    return false;
  }

  /**
   * Gets and sets the activation state model used to control presentation of the OSK.
   */
  get activationModel(): Activator {
    return this.config.activator;
  }

  set activationModel(model: Activator) {
    if(!model) {
      throw new Error("The activation model may not be set to null or undefined!");
    }

    this.config.activator.off('activate', this.activationListener);
    model.on('activate', this.activationListener);

    this.config.activator = model;

    this.commonCheckAndDisplay();
  }

  public get mayDisable(): boolean {
    if(this.hostDevice.touchable) {
      return false;
    }

    if(this.activeKeyboard?.keyboard.isCJK) {
      return false;
    }

    return true;
  }

  private readonly activationListener = (flag: boolean) => {
    // CJK override:  may not be disabled, as the CJK elements are required.
    if(!this.mayDisable && !this.activationModel.enabled) {
      this.activationModel.off('activate', this.activationListener);
      try {
        this.activationModel.enabled = true;
      } finally {
        this.activationModel.on('activate', this.activationListener);
      }
    }
    this.commonCheckAndDisplay();
  };

  /**
   * A property denoting whether or not the OSK will be presented when it meets all
   * other activation conditions.
   *
   * Is equivalent to `.activationModel.enabled`.
   */
  get displayIfActive(): boolean {
    return this.activationModel.enabled;
  }

  /**
   * Used by the activation model's event listenerss and properties as a common helper;
   * they rely on this function to manage presentation (showing / hiding) of the OSK.
   */
  private commonCheckAndDisplay() {
    if(this.activationModel.activate) {
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

  public get banner(): BannerView {  // Maintains old reference point used by embedding apps.
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
      this._baseFontSize = OSKView.defaultFontSize(this.targetDevice, this.computedHeight, this.isEmbedded);
    }

    return this._baseFontSize;
  }

  public static defaultFontSize(device: DeviceSpec, computedHeight: number, isEmbedded: boolean): ParsedLengthStyle {
    if(device.touchable) {
      const fontScale = device.formFactor == 'phone'
        ? 1.6 * (isEmbedded ? 0.65 : 0.6) * 1.2  // Combines original scaling factor with one previously applied to the layer group.
        : 2; // iPad or Android tablet
      return ParsedLengthStyle.special(fontScale, 'em');
    } else {
      return computedHeight ? ParsedLengthStyle.inPixels(computedHeight / 8) : undefined;
    }
  }

  public get activeKeyboard(): {
    keyboard: Keyboard,
    metadata: KeyboardProperties
  } {
    return this.keyboardData;
  }

  public set activeKeyboard(keyboardData: {
    keyboard: Keyboard,
    metadata: KeyboardProperties
  }) {
    this.keyboardData = keyboardData;
    this.loadActiveKeyboard();

    if(this.keyboardData?.keyboard.isCJK) {
      this.activationModel.enabled = true;
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
      // Note:  %-based auto-detect for dimensions currently has some issues; the stylesheets load
      // asynchronously, causing the format to be VERY off before the stylesheets fully load.
      //
      // Depending on initial effects, changes to the OSK size could cause changes to the _parent_ size,
      // too... so this potential bit likely needs something of a redesign.
      const parent = this._Box.parentElement as HTMLElement;
      this._computedWidth  = this.width.val  * (this.width.absolute  ? 1 : parent.offsetWidth);
      this._computedHeight = this.height.val * (this.height.absolute ? 1 : parent.offsetHeight);
    } else if(!hasDimensions) {
      // Cannot perform layout operations!
      console.warn("Unable to properly perform layout - size specifications have not yet been set.");
      return;
    } else {
      console.warn("Unable to properly perform layout - specification uses a relative spec, thus relies upon insertion into the DOM for layout.");
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

  // /**
  //  * Function     _Load
  //  * Scope        Private
  //  * Description  OSK initialization when keyboard selected
  //  */
  // _Load() { // Load Help - maintained only temporarily.
  //   let keymanweb = com.keyman.singleton;
  //   this.activeKeyboard = keymanweb.core.activeKeyboard;
  // }

  public postKeyboardLoad() {
    this._Visible = false;  // I3363 (Build 301)

    // Perform any needed restructuring and/or layout tweaks (depending on the OSKView type).
    this.postKeyboardAdjustments();

    if(this.displayIfActive) {
      this.present();
    }
  }

  protected abstract postKeyboardAdjustments(): void;

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

    // Install the default OSK stylesheet - but don't have it managed by the stylesheet manager.
    // We wish to maintain kmwosk.css whenever keyboard-specific styles are reset/removed.
    for(let sheetHref of this.configuration.commonStyleSheetRefs) {
      this.uiStyleSheetManager.linkExternalSheet(sheetHref);
    }

    // Any event-cancelers would go here, after the innerHTML reset.

    // Add header element to OSK only for desktop browsers
    if(this.headerView) {
      this._Box.appendChild(this.headerView.element);
    }

    // Add suggestion banner bar to OSK
    this._Box.appendChild(this.banner.element);

    if(this.bannerView.banner) {
      this.banner.banner.configureForKeyboard(this.keyboardData.keyboard, this.keyboardData.metadata);
    }

    let kbdView: KeyboardView = this.keyboardView = this._GenerateKeyboardView(this.keyboardData.keyboard, this.keyboardData.metadata);
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

  private _GenerateKeyboardView(keyboard: Keyboard, keyboardMetadata: KeyboardProperties): KeyboardView {
    let device = this.targetDevice;

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
      if(keyboard && keyboard.layout(device.formFactor as DeviceSpec.FormFactor)) {
        return this._GenerateVisualKeyboard(keyboard, keyboardMetadata);
      } else if(!keyboard /* && device.touchable (implied) */ || !keyboardMetadata) {
        // Show a basic, "hollow" OSK that at least allows input, since we're
        // on a touch device and hiding the system keyboard
        return this._GenerateVisualKeyboard(null, null);
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
  private _GenerateVisualKeyboard(keyboard: Keyboard, keyboardMetadata: KeyboardProperties): VisualKeyboard {
    let device = this.targetDevice;

    // Root element sets its own classes, one of which is 'kmw-osk-inner-frame'.
    let vkbd = new VisualKeyboard({
      keyboard: keyboard,
      keyboardMetadata: keyboardMetadata,
      device: device,
      hostDevice: this.hostDevice,
      topContainer: this._Box,
      styleSheetManager: this.kbdStyleSheetManager,
      fontRootPath: this.fontRootPath
    });

    vkbd.on('keyEvent', (keyEvent) => this.emit('keyEvent', keyEvent));
    vkbd.on('globeKey', (keyElement) => this.emit('shouldShowLanguageMenu', keyElement));

    // Set box class - OS and keyboard added for Build 360
    this._Box.className=device.formFactor+' '+ device.OS.toLowerCase() + ' kmw-osk-frame';

    // Add primary keyboard element to OSK
    return vkbd;
  }

  /**
   * This function may be provided to event sources to trigger changes in keyboard layer.
   * It is pre-bound to its OSKView instance.
   *
  ```
    {
      let core = com.keyman.singleton.core;
      core.keyboardProcessor.layerStore.handler = this.layerChangeHandler;
    }
  ```
   *
   * @param source
   * @param newValue
   * @returns
   */
  public layerChangeHandler: SystemStoreMutationHandler = (source: MutableSystemStore,
    newValue: string) => {
    // This handler is also triggered on state-key state changes (K_CAPS) that
    // may not actually change the layer.
    if(this.vkbd) {
      this.vkbd._UpdateVKShiftStyle(newValue);
    }

    if((this.vkbd && this.vkbd.layerId != newValue) || source.value != newValue) {
      // Prevents console errors when a keyboard only displays help.
      // Can occur when using SHIFT with sil_euro_latin on a desktop form-factor.
      //
      // Also, only change the layer ID itself if there is an actual corresponding layer
      // in the OSK.
      if(this.vkbd?.layerGroup.layers[newValue]) {
        this.vkbd.layerId = newValue;
        // Ensure that the layer's spacebar is properly captioned.
        this.vkbd.showLanguage();
      }

      // Ensure the keyboard view is modeling the correct state.  (Correct layer, etc.)
      this.keyboardView.updateState(); // will also need the stateKeys.
      // We need to recalc the font size here because the layer did not have
      // calculated dimensions available before it was visible
      this.refreshLayout();
    }

    return false;
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
    this.keyboardView.updateState(); // get current state keys!

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
      this.activationModel.enabled = ((this.keyboardData.keyboard.isCJK || this.hostDevice.touchable) ? true : false); // I3363 (Build 301)
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
    this.doHide(hiddenByUser);
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
    if(!this.activationModel.conditionsMet) {
      return false;
    }

    // Never display the OSK for desktop browsers unless KMW element is focused, and a keyboard selected
    if(!this.keyboardView || this.keyboardView instanceof EmptyView || !this.activationModel.enabled) {
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
    if(this.activationModel.conditionsMet && !this.mayDisable) {
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

        if(_this._Visible && _this.activationModel.conditionsMet) {
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

    this.kbdStyleSheetManager.unlinkAll();
    this.uiStyleSheetManager.unlinkAll();
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
    p['left'] = p.left = getAbsoluteX(this._Box);
    p['top']  = p.top  = getAbsoluteY(this._Box);

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

  // hideLanguageMenu() {
  //   this.lgMenu?.hide();
  //   this.lgMenu = null;
  // }

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
    this.activationModel.enabled = false;
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
      this.activationModel.enabled = bShow;
    } else {
      if(this.activationModel.conditionsMet) {
        this.activationModel.enabled = !this.activationModel.enabled;
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
    this.emit('onshow');
    this.emit('legacyevent', 'osk.show', p);
  }

  /**
   * Allow UI to update respond to OSK being hidden
   *
   * @param       {boolean}       p     object with coordinates and userdefined flag
   * @return      {void}
   *
   */
  doHide(hiddenByUser: boolean) {
    this.emit('onhide', hiddenByUser);

    const p={};
    p['HiddenByUser']=hiddenByUser;
    this.emit('legacyevent', 'osk.hide', p);
  }

  /**
   * Function     addEventListener
   * Scope        Public
   * @param       {string}            event     event name
   * @param       {function(Object)}  func      event handler
   * @return      {boolean}
   * Description  Wrapper function to add and identify OSK-specific event handlers
   */
  addEventListener<T extends keyof LegacyOSKEventMap>(event: T, fn: (arg: {}) => void): void {
  // addEventListener<T extends keyof LegacyOSKEventMap>(event: T, fn: (...args: ArgumentMap<LegacyOSKEventMap>[Extract<T, keyof LegacyOSKEventMap>]) => void): void {
    // might be a bit too much for the type system to fully infer due to the nested event maps.
    this.emit('legacyevent', event, fn);

    //return com.keyman.singleton.util.addEventListener('osk.'+event, func);
  }

  // Allows interception of attaching event handlers.
  on<T extends keyof EventMap>(event: T, fn: (...args: ArgumentMap<EventMap>[Extract<T, keyof EventMap>]) => void, context?: any): this {
    super.on(event, fn, context);
    this.onListenedEvent(event);
    return this;
  }

  once<T extends keyof EventMap>(event: T, fn: (...args: ArgumentMap<EventMap>[Extract<T, keyof EventMap>]) => void, context?: any): this {
    super.once(event, fn, context);
    this.onListenedEvent(event);
    return this;
  }

  private onListenedEvent(eventName: keyof EventMap) {
    // As the following title bar buttons (for desktop / FloatingOSKView) do nothing unless
    // a site designer uses these events, we disable / hide them until an event is attached.
    let titleBar = this.headerView;
    if(titleBar && titleBar instanceof TitleBar) {
      switch(eventName) {
        case 'showConfig':
          titleBar.configEnabled = true;
          break;
        case 'showHelp':
          titleBar.helpEnabled = true;
          break;
        default:
          return;
      }
    }
  }
}