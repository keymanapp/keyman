import { EventEmitter } from 'eventemitter3';

import { BannerView } from '../banner/bannerView.js';
import { BannerController } from '../banner/bannerController.js';
import OSKViewComponent from '../components/oskViewComponent.interface.js';
import EmptyView from '../components/emptyView.js';
import HelpPageView from '../components/helpPageView.js';
import KeyboardView from '../components/keyboardView.interface.js';
import VisualKeyboard from '../visualKeyboard.js';
import { LengthStyle, ParsedLengthStyle } from '../lengthStyle.js';
import { type KeyElement } from '../keyElement.js';

import {
  Codes,
  DeviceSpec,
  Keyboard,
  KeyboardProperties,
  ManagedPromise,
  type MinimalCodesInterface
} from '@keymanapp/keyboard-processor';
import { createUnselectableElement, getAbsoluteX, getAbsoluteY, StylesheetManager } from 'keyman/engine/dom-utils';
import { EventListener, LegacyEventEmitter } from 'keyman/engine/events';
import { type MutableSystemStore, type SystemStoreMutationHandler } from 'keyman/engine/js-processor';

import Configuration from '../config/viewConfiguration.js';
import Activator, { StaticActivator } from './activator.js';
import TouchEventPromiseMap from './touchEventPromiseMap.js';
import { KeyEventHandler, KeyEventSourceInterface } from './keyEventSource.interface.js';
import { DEFAULT_GESTURE_PARAMS, GestureParams } from '../input/gestures/specsForLayout.js';

// These will likely be eliminated from THIS file at some point.\

export type OSKPos = {'left'?: number, 'top'?: number};

export type OSKRect = {
  'left'?: number,
  'top'?: number,
  'width'?: number,
  'height'?: number,
  'nosize'?: boolean,
  'nomove'?: boolean
};

/**
 * Definition for OSK events documented at
 * https://help.keyman.com/DEVELOPER/ENGINE/WEB/16.0/reference/events/.
 */
export interface LegacyOSKEventMap {
  'configclick'(obj: {}): void;
  'helpclick'(obj: {}): void;
  'resizemove'(obj: {}): void;
  'show'(obj: {
    x?: number,
    y?: number,
    userLocated?: boolean
  }): void;
  'hide'(obj: {
    HiddenByUser?: boolean
  }): void;
}

/**
 * For now, these will serve as undocumented, internal events.  We need a proper
 * design round and discussion before we consider promoting them to long-term,
 * documented official API events.
 */
export interface EventMap {
  /**
   * Designed to pass key events off to any consuming modules/libraries.
   *
   * Note:  the following code block was originally used to integrate with the keyboard & input
   * processors, but it requires entanglement with components external to this OSK module.
   */
  'keyevent': KeyEventHandler,

  /**
   * Indicates that the globe key has either been pressed (`on` == `true`)
   * or released (`on` == `false`).
   */
  globekey: (e: KeyElement, on: boolean) => void;

  /**
   * A virtual keystroke corresponding to a "hide" command has been received.
   */
  hiderequested: (key: KeyElement) => void;

  /**
   * Signals the special command to display the engine's version + build number.
   */
  showbuild: () => void;

  // While the next two are near-duplicates of the legacy event `resizemove`, these
  // have the advantage of providing a Promise for the end of the ongoing user
  // interaction.  We need that Promise for focus-management.

  /**
   * Signals that the OSK is being moved by the user via a drag operation.
   *
   * The provided Promise will resolve once the drag operation is complete.
   *
   * Note that position-restoration (unpinning the OSK) is treated as a drag-move
   * event.  It resolves near-instantly.
   */
  dragmove: (promise: Promise<void>) => void;

  /**
   * Signals that the OSK is being resized via a drag operation (on a resize 'handle').
   *
   * The provided Promise will resolve once the resize operation is complete.
   */
  resizemove: (promise: Promise<void>) => void;

  /**
   * Signals that either the mouse or an active touchpoint is interacting with the OSK.
   *
   * The provided `Promise` will resolve once the corresponding interaction is complete.
   * Note that for touch events, more than one touchpoint may coexist, each with its own
   * corresponding call of this event and corresponding `Promise`.
   */
  pointerinteraction: (promise: Promise<void>) => void;
}

export function getResourcePath(config: Configuration) {
  let resourcePathExt = 'osk/';
  if(config.isEmbedded) {
    resourcePathExt = '';
  }
  return `${config.pathConfig.resources}/${resourcePathExt}`
}

export default abstract class OSKView
  extends EventEmitter<EventMap>
  implements MinimalCodesInterface, KeyEventSourceInterface<EventMap> {
  _Box: HTMLDivElement;
  readonly legacyEvents = new LegacyEventEmitter<LegacyOSKEventMap>();

  // #region Key code definition aliases for legacy keyboards (that expect window['keyman']['osk'].___)
  get keyCodes() {
    return Codes.keyCodes;
  }

  get modifierCodes() {
    return Codes.modifierCodes;
  }

  get modifierBitmasks() {
    return Codes.modifierBitmasks;
  }

  get stateBitmasks() {
    return Codes.stateBitmasks;
  }
  // #endregion

  headerView:   OSKViewComponent;
  bannerView:   BannerView; // Which implements OSKViewComponent
  keyboardView: KeyboardView;  // Which implements OSKViewComponent
  footerView:   OSKViewComponent;

  private _bannerController: BannerController;

  private kbdStyleSheetManager: StylesheetManager;
  private uiStyleSheetManager: StylesheetManager;

  private config: Configuration;
  private deferLayout: boolean;

  private _boxBaseTouchStart:       (e: TouchEvent) => boolean;
  private _boxBaseTouchEventCancel: (e: TouchEvent) => boolean;

  private keyboardData: {
    keyboard: Keyboard,
    metadata: KeyboardProperties
  };

  /**
   * Provides the current parameterization for timings and distances used by
   * any gesture-supporting keyboards.  Changing properties of its objects will
   * automatically update keyboards to use the new configuration.
   *
   * If `gestureParams` was set in the configuration object passed in at
   * construction time, this will be the same instance.
   */
  get gestureParams(): GestureParams {
    return this.config.gestureParams;
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

  private _animatedHideTimeout: number;

  private mouseEnterPromise?: ManagedPromise<void>;
  private touchEventPromiseManager = new TouchEventPromiseMap();

  static readonly STYLESHEET_FILES = ['kmwosk.css', 'globe-hint.css'];

  constructor(configuration: Configuration) {
    super();

    // Clone the config; do not allow object references to be altered later.
    this.config = configuration = {...configuration};
    // If gesture parameters were not provided in advance, initialize them from defaults.
    this.config.gestureParams ||= DEFAULT_GESTURE_PARAMS;

    // `undefined` is falsy, but we want a `true` default behavior for this config property.
    if(this.config.allowHideAnimations === undefined) {
      this.config.allowHideAnimations = true;
    }

    this.config.device = configuration.device || configuration.hostDevice;

    this.config.isEmbedded = configuration.isEmbedded || false;
    this.config.embeddedGestureConfig = configuration.embeddedGestureConfig || {};
    this.config.activator.on('activate', this.activationListener);

    // OSK initialization - create DIV and set default styles
    this._Box = createUnselectableElement('div');   // Container for OSK (Help DIV, displayed when user clicks Help icon)
    this.kbdStyleSheetManager = new StylesheetManager(this._Box, this.config.doCacheBusting || false);
    this.uiStyleSheetManager = new StylesheetManager(this._Box);

    // Initializes the two constant OSKComponentView fields.
    this.bannerView   = new BannerView();
    this.bannerView.events.on('bannerchange', () => this.refreshLayout());
    this._Box.appendChild(this.bannerView.element);

    this._bannerController = new BannerController(this.bannerView, this.hostDevice, this.config.predictionContextManager);

    this.keyboardView = this._GenerateKeyboardView(null, null);
    this._Box.appendChild(this.keyboardView.element);

    // Install the default OSK stylesheets - but don't have it managed by the keyboard-specific stylesheet manager.
    // We wish to maintain kmwosk.css whenever keyboard-specific styles are reset/removed.
    // Temp-hack:  embedded products prefer their stylesheet, etc linkages without the /osk path component.
    const resourcePath = getResourcePath(this.config);

    for(let sheetFile of OSKView.STYLESHEET_FILES) {
      const sheetHref = `${resourcePath}${sheetFile}`;
      this.uiStyleSheetManager.linkExternalSheet(sheetHref);
    }

    this.setBaseMouseEventListeners();
    if(this.hostDevice.touchable) {
      this.setBaseTouchEventListeners();
    }

    this._Box.style.display = 'none';
  }

  protected get configuration(): Configuration {
    return this.config;
  }

  public get bannerController(): BannerController {
    return this._bannerController;
  }

  public get hostDevice(): DeviceSpec {
    return this.config.hostDevice;
  }

  public get fontRootPath(): string {
    return this.config.pathConfig.fonts;
  }

  public get isEmbedded(): boolean {
    return this.config.isEmbedded;
  }

  private setBaseMouseEventListeners() {
    this._Box.onmouseenter = (e) => {
      if(this.mouseEnterPromise) {
        // The chain was somehow interrupted, with the mouseleave never occurring!
        this.mouseEnterPromise.resolve();
      }

      this.mouseEnterPromise = new ManagedPromise<void>();
      this.emit('pointerinteraction', this.mouseEnterPromise.corePromise);
    };

    this._Box.onmouseleave = (e) => {
      this.mouseEnterPromise.resolve();
      this.mouseEnterPromise = null;
      // focusAssistant.setMaintainingFocus(false);
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
        this.emit('pointerinteraction', promise.corePromise);
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
    if(this.activationModel.activate && this.activeKeyboard) {
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

  public batchLayoutAfter(closure: () => void) {
    /*
      Is there already an ongoing batch?  If so, just run the closure and don't
      adjust the tracking variables.  The outermost call will finalize layout.
    */
    if(this.deferLayout) {
      closure();
      return;
    }

    try {
      this.deferLayout = true;
      if(this.vkbd) {
        this.vkbd.deferLayout = true;
      }
      closure();
    } finally {
      this.deferLayout = false;
      if(this.vkbd) {
        this.vkbd.deferLayout = false;
      }
      this.refreshLayout();
    }
  }

  public refreshLayout(pending?: boolean): void {
    if(!this.keyboardView || this.deferLayout) {
      return;
    }

    // Step 1:  have the necessary conditions been met?
    const hasDimensions = this.width && this.height;

    if(!hasDimensions) {
      // If dimensions haven't been set yet, we have no basis for layout calculations.
      // We do not emit a warning here; if we did, at the time of writing this, we'd
      // consistently get Sentry events from the Keyman mobile apps.
      //
      // See #9206 & https://github.com/keymanapp/keyman/pull/9206#issuecomment-1627917615
      // for context and history.
      return;
    }

    const fixedSize = this.width.absolute && this.height.absolute;
    const computedStyle = getComputedStyle(this._Box);
    const isInDOM = computedStyle.height != '' && computedStyle.height != 'auto';

    // Step 2:  determine basic layout geometry
    if(fixedSize) {
      this._computedWidth  = this.width.val;
      this._computedHeight = this.height.val;
    } else if(isInDOM) {
      // Note:  %-based auto-detect for dimensions currently has some issues; the stylesheets load
      // asynchronously, causing the format to be VERY off before the stylesheets fully load.
      //
      // Depending on initial effects, changes to the OSK size could cause changes to the _parent_ size,
      // too... so this potential bit likely needs something of a redesign.
      const parent = this._Box.parentElement as HTMLElement;
      this._computedWidth  = this.width.val  * (this.width.absolute  ? 1 : parent.offsetWidth);
      this._computedHeight = this.height.val * (this.height.absolute ? 1 : parent.offsetHeight);
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
      this.bannerView.width = this.computedWidth;
      this.bannerView.refreshLayout();
      this.footerView?.refreshLayout();
    }

    if(this.vkbd) {
      let availableHeight = this.computedHeight - this.computeFrameHeight();

      // +5:  from kmw-banner-bar's 'top' attribute when active
      if(this.bannerView.height > 0) {
        availableHeight -= this.bannerView.height + 5;
      }
      // Triggers the VisualKeyboard.refreshLayout() method, which includes a showLanguage() call.
      this.vkbd.setSize(this.computedWidth, availableHeight, pending);

      const bs = this._Box.style;
      // OSK size settings can only be reliably applied to standard VisualKeyboard
      // visualizations, not to help text or empty views.
      bs.width  = bs.maxWidth  = this.computedWidth + 'px';
      bs.height = bs.maxHeight = this.computedHeight + 'px';
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
    this.needsLayout = true;
    // Save references to the old kbd & its styles for shutdown after replacement.
    const oldKbd = this.keyboardView;
    const oldKbdStyleManager = this.kbdStyleSheetManager;

    // Create new ones for the new, incoming kbd.
    this.kbdStyleSheetManager = new StylesheetManager(this._Box, this.config.doCacheBusting || false);
    const kbdView = this.keyboardView = this._GenerateKeyboardView(this.keyboardData?.keyboard, this.keyboardData?.metadata);

    // Perform the replacement.
    this._Box.replaceChild(kbdView.element, oldKbd.element);
    kbdView.postInsert();
    this.bannerController?.configureForKeyboard(this.keyboardData?.keyboard, this.keyboardData?.metadata);

    // Now that the swap has occurred, it's safe to shutdown the old VisualKeyboard and any related stylesheets.
    if(oldKbd instanceof VisualKeyboard) {
      oldKbd.shutdown();
    }
    oldKbdStyleManager.unlinkAll();

    // END:  construction of the actual internal layout for the overall OSK
    // Footer element management is handled within FloatingOSKView.

    this.banner.appendStyles();

    if(this.vkbd) {
      // Create the key preview (for phones)
      this.vkbd.createKeyTip();

      // Create the globe hint (for embedded contexts; has a stub for other contexts)
      const globeHint = this.vkbd.createGlobeHint();
      if(globeHint) {
        this._Box.appendChild(globeHint.element);
      }

      // Append a stylesheet for this keyboard for keyboard specific styles
      // or if needed to specify an embedded font
      this.vkbd.appendStyleSheet();
    }

    this.postKeyboardLoad();
  }

  private _GenerateKeyboardView(keyboard: Keyboard, keyboardMetadata: KeyboardProperties): KeyboardView {
    let device = this.targetDevice;

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

    const resourcePath = getResourcePath(this.config);

    // Root element sets its own classes, one of which is 'kmw-osk-inner-frame'.
    let vkbd = new VisualKeyboard({
      keyboard: keyboard,
      keyboardMetadata: keyboardMetadata,
      device: device,
      hostDevice: this.hostDevice,
      topContainer: this._Box,
      styleSheetManager: this.kbdStyleSheetManager,
      pathConfig: this.config.pathConfig,
      embeddedGestureConfig: this.config.embeddedGestureConfig,
      isEmbedded: this.config.isEmbedded,
      specialFont: {
        family: 'SpecialOSK',
        files: [`${resourcePath}/keymanweb-osk.ttf`],
        path: '' // Not actually used.
      },
      gestureParams: this.config.gestureParams
    });

    vkbd.on('keyevent', (keyEvent, callback) => this.emit('keyevent', keyEvent, callback));
    vkbd.on('globekey', (keyElement, on) => this.emit('globekey', keyElement, on));
    vkbd.on('hiderequested', (keyElement) => {
      this.doHide(true);
      this.emit('hiderequested', keyElement);
    });

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
      if(this.vkbd?.layerGroup.getLayer(newValue) && !this.vkbd?.layerLocked) {
        // triggers state-update + layer refresh automatically.
        this.vkbd.layerId = newValue;
      }
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

    // Each subclass is responsible for raising the 'show' event on its own, since
    // certain ones supply extra information in their event param object.
  }

  /**
   * Method usable by subclasses of OSKView to control that OSKView type's
   * positioning behavior when needed by the present() method.
   */
  protected abstract setDisplayPositioning(): void;

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
    if(this._Box && this.hostDevice.touchable && !(this.keyboardView instanceof EmptyView) && this.config.allowHideAnimations) {
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
    if (document.body.className.indexOf('osk-always-visible') >= 0) {
      if (this.hostDevice.formFactor == 'desktop') {
        return;
      }
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

    if(this.activationModel instanceof StaticActivator) {
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

    this.bannerController.shutdown();
  }

  /**
   * Function     getRect
   * Scope        Public
   * @return      {Object.<string,number>}   Array object with position and size of OSK container
   * Description  Get rectangle containing KMW Virtual Keyboard
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/osk/getRect
   */
  public getRect(): OSKRect {		// I2405
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

  // OSK state fields & events
  //
  // These are relatively stable and may be preserved as they are.
  _Visible: boolean = false;

  /**
   * Function     enabled
   * Scope        Public
   * @return      {boolean|number}    True if KMW OSK enabled
   * Description  Test if KMW OSK is enabled
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/osk/isEnabled
   */
  public isEnabled(): boolean {
    return this.displayIfActive;
  }

  /**
   * Function     isVisible
   * Scope        Public
   * @return      {boolean|number}    True if KMW OSK visible
   * Description  Test if KMW OSK is actually visible
   * Note that this will usually return false after any UI event that results in (temporary) loss of input focus
   *
   * https://help.keyman.com/developer/engine/web/current-version/reference/osk/isVisible
   */
  public isVisible(): boolean {
    return this._Visible;
  }

  /**
   * Function     hide
   * Scope        Public
   * Description  Prevent display of OSK window on focus
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/osk/hide
   */
  public hide() {
    this.activationModel.enabled = false;
    this.startHide(true);
  }

  /**
   * Description  Display KMW OSK (at position set in callback to UI)
   * Function     show
   * Scope        Public
   * @param       {(boolean|number)=}      bShow     True to display, False to hide, omitted to toggle
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/osk/show
   */
  public show(bShow?: boolean) {
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
  doShow(p: {
    x: number,
    y: number,
    userLocated: boolean
  }) {
    // Newer style 'doShow' emitted from .present by default.
    this.legacyEvents.callEvent('show', p);
  }

  /**
   * Allow UI to update respond to OSK being hidden
   *
   * @param       {boolean}       p     object with coordinates and userdefined flag
   * @return      {void}
   *
   */
  doHide(hiddenByUser: boolean) {
    const p={
      HiddenByUser: hiddenByUser
    };
    this.legacyEvents.callEvent('hide', p);
  }

  /**
   * Function     addEventListener
   * Scope        Public
   * @param       {string}            event     event name
   * @param       {function(Object)}  func      event handler
   * Description  Wrapper function to add and identify OSK-specific event handlers
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/osk/addEventListener
   */
  public addEventListener<T extends keyof LegacyOSKEventMap>(
    event: T,
    fn: EventListener<LegacyOSKEventMap, T>
  ): void {
    this.legacyEvents.addEventListener(event, fn);
  }

  /**
   * Function     removeEventListener
   * Scope        Public
   * @param       {string}            event     event name
   * @param       {function(Object)}  func      event handler
   * Description  Wrapper function to remove previously-added OSK-specific event handlers
   *
   * See https://help.keyman.com/developer/engine/web/current-version/reference/osk/removeEventListener
   */
  public removeEventListener<T extends keyof LegacyOSKEventMap>(
    event: T,
    fn: EventListener<LegacyOSKEventMap, T>
  ): void {
    this.legacyEvents.removeEventListener(event, fn);
  }
}