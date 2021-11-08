/// <reference path="preProcessor.ts" />
/// <reference path="lengthStyle.ts" />
/// <reference path="utils.ts" />
/// <reference path="oskLayerGroup.ts" />
/// <reference path="keytip.interface.ts" />
/// <reference path="browser/keytip.ts" />
/// <reference path="browser/pendingLongpress.ts" />
/// <reference path="keyboardView.interface.ts" />
/// <reference path="touchEventEngine.ts" />
/// <reference path="mouseEventEngine.ts" />

namespace com.keyman.osk {
  interface BoundingRect {
    left: number,
    right: number,
    top: number,
    bottom: number
  };

  export class VisualKeyboard implements KeyboardView {
    // Legacy alias, maintaining a reference for code built against older
    // versions of KMW.
    static specialCharacters = OSKKey.specialCharacters;

    /**
     * Contains layout properties corresponding to the OSK's layout.  Needs to be public
     * so that its geometry may be updated on rotations and keyboard resize events, as
     * said geometry needs to be accurate for fat-finger probability calculations.
     */
    kbdLayout: keyboards.ActiveLayout;
    layerGroup: OSKLayerGroup;

    private _layerId: string = "default";
    layerIndex: number = 0; // the index of the default layer
    readonly isRTL: boolean;

    device: com.keyman.utils.DeviceSpec;
    hostDevice: com.keyman.utils.DeviceSpec;

    inputEngine: InputEventEngine;

    isStatic: boolean = false;
    _fixedWidthScaling:  boolean = false;
    _fixedHeightScaling: boolean = true;

    // Stores the base element for this instance of the visual keyboard.
    // Formerly known as osk._DivVKbd
    kbdDiv: HTMLDivElement;
    styleSheet: HTMLStyleElement;

    /**
     * The configured width for this VisualKeyboard.  May be `undefined` or `null`
     * to allow automatic width scaling.
     */
    private _width: number;

    /**
     * The configured height for this VisualKeyboard.  May be `undefined` or `null`
     * to allow automatic height scaling.
     */
    private _height: number;

    /**
     * The computed width for this VisualKeyboard.  May be null if auto sizing
     * is allowed and the VisualKeyboard is not currently in the DOM hierarchy.
     */
    private _computedWidth: number;

    /**
     * The computed height for this VisualKeyboard.  May be null if auto sizing
     * is allowed and the VisualKeyboard is not currently in the DOM hierarchy.
     */
    private _computedHeight: number;

    // Style-related properties
    fontFamily: string;
    private _fontSize: ParsedLengthStyle;
    // fontSize: string;

    // State-related properties
    keyPending: KeyElement;
    touchPending: InputEventCoordinate;
    deleteKey: KeyElement;
    deleting: number; // Tracks a timer id for repeated deletions.
    nextLayer: string;
    currentKey: string;

    // Touch-tracking properties
    initTouchCoord: InputEventCoordinate;
    touchCount: number;
    currentTarget: KeyElement;

    // Used by embedded-mode's globe key
    menuEvent: KeyElement; // Used by embedded-mode.

    // Popup key management
    keytip: KeyTip;
    pendingSubkey: PendingGesture;
    subkeyGesture: RealizedGesture;

    get layerId(): string {
      return this._layerId;
    }

    set layerId(value: string) {
      this._layerId = value;
    }

    get currentLayer(): OSKLayer {
      return this.layerId ? this.layerGroup?.layers[this.layerId] : null;
    }

    // Special keys (for the currently-visible layer)
    get lgKey(): KeyElement { // currently, must be visible for the touch language menu.
      return this.currentLayer?.globeKey?.btn;
    }

    private get hkKey(): KeyElement { // hide keyboard key
      return this.currentLayer?.hideKey?.btn;
    }

    public get spaceBar(): KeyElement { // also referenced by the touch language menu.
      return this.currentLayer?.spaceBarKey?.btn;
    }

    //#region OSK constructor and helpers

    /**
     * @param       {Object}      PVK         Visual keyboard name
     * @param       {Object}      Lhelp       true if OSK defined for this keyboard
     * @param       {Object}      layout0
     * @param       {Number}      kbdBitmask  Keyboard modifier bitmask
     * Description  Generates the base visual keyboard element, prepping for attachment to KMW
     */
    constructor(keyboard: keyboards.Keyboard, device: com.keyman.utils.DeviceSpec, hostDevice?: com.keyman.utils.DeviceSpec, isStatic?: boolean) {
      this.device = device;
      this.hostDevice = hostDevice || device;
      if (isStatic) {
        this.isStatic = isStatic;
      }

      this._fixedWidthScaling  = this.device.touchable && !this.isStatic;
      this._fixedHeightScaling = this.device.touchable && !this.isStatic;

      // Create the collection of HTML elements from the device-dependent layout object
      var Lkbd = document.createElement('div');
      let layout: keyboards.ActiveLayout;
      if (keyboard) {
        layout = this.kbdLayout = keyboard.layout(device.formFactor as utils.FormFactor);
        this.isRTL = keyboard.isRTL;
      } else {
        // This COULD be called with no backing keyboard; KMW will try to force-show the OSK even without
        // a backing keyboard on mobile, using the most generic default layout as the OSK's base.
        //
        // In KMW's current state, it'd take a major break, though - Processor always has an activeKeyboard,
        // even if it's "hollow".
        let rawLayout = keyboards.Layouts.buildDefaultLayout(null, null, device.formFactor);
        layout = this.kbdLayout = keyboards.ActiveLayout.polyfill(rawLayout, null, device.formFactor as utils.FormFactor);
        this.isRTL = false;
      }

      // Override font if specified by keyboard
      if ('font' in layout) {
        this.fontFamily = layout['font'];
      } else {
        this.fontFamily = '';
      }

      // Now to build the actual layout.
      const formFactor = device.formFactor as utils.FormFactor;
      let layoutKeyboard = keyboard;
      if (!layoutKeyboard) {
        // May occasionally be null in embedded contexts; have seen this when iOS engine sets
        // keyboard height during change of keyboards.
        layoutKeyboard = new keyboards.Keyboard(null);
      }

      this.layerGroup = new OSKLayerGroup(this, layoutKeyboard, formFactor);

      // Now that we've properly processed the keyboard's layout, mark it as calibrated.
      // TODO:  drop the whole 'calibration' thing.  The newer layout system supersedes the
      // need for it.  (Is no longer really used, so the drop ought be clean.)
      layoutKeyboard.markLayoutCalibrated(formFactor);

      // Append the OSK layer group container element to the containing element
      //osk.keyMap = divLayerContainer;
      Lkbd.appendChild(this.layerGroup.element);

      // Set base class - OS and keyboard added for Build 360
      this.kbdDiv = Lkbd;

      // For 'live' touch keyboards, attach touch-based event handling.
      // Needs to occur AFTER this.kbdDiv is initialized.
      if (!this.isStatic) {
        if (this.hostDevice.touchable) {
          this.inputEngine = TouchEventEngine.forVisualKeyboard(this);
        } else {
          this.inputEngine = MouseEventEngine.forVisualKeyboard(this);
        }
        this.inputEngine.registerEventHandlers();
      }

      Lkbd.className = device.formFactor + ' kmw-osk-inner-frame';
    }

    public get element(): HTMLDivElement {
      return this.kbdDiv;
    }

    public postInsert(): void { }

    /**
     * The configured width for this VisualKeyboard.  May be `undefined` or `null`
     * to allow automatic width scaling.
     */
    get width(): number {
      return this._width;
    }

    /**
     * The configured height for this VisualKeyboard.  May be `undefined` or `null`
     * to allow automatic height scaling.
     */
    get height(): number {
      return this._height;
    }

    get layoutWidth(): ParsedLengthStyle {
      if (this.usesFixedWidthScaling) {
        let baseWidth = this.width;
        let cs = getComputedStyle(this.element);
        if (cs.border) {
          let borderWidth = new ParsedLengthStyle(cs.borderWidth).val;
          baseWidth -= borderWidth * 2;
        }
        return ParsedLengthStyle.inPixels(baseWidth);
      } else {
        return ParsedLengthStyle.forScalar(1);
      }
    }

    get layoutHeight(): ParsedLengthStyle {
      if (this.usesFixedHeightScaling) {
        let baseHeight = this.height;
        let cs = getComputedStyle(this.element);
        if (cs.border) {
          let borderHeight = new ParsedLengthStyle(cs.borderWidth).val;
          baseHeight -= borderHeight * 2;
        }
        return ParsedLengthStyle.inPixels(baseHeight);
      } else {
        return ParsedLengthStyle.forScalar(1);
      }
    }

    get fontSize(): ParsedLengthStyle {
      if (!this._fontSize) {
        this._fontSize = new ParsedLengthStyle('1em');
      }
      return this._fontSize;
    }

    set fontSize(value: ParsedLengthStyle) {
      this._fontSize = value;
      this.kbdDiv.style.fontSize = value.styleString;
    }

    /**
     * Uses fixed scaling for widths of internal elements, rather than relative,
     * percent-based scaling.
     */
    public get usesFixedWidthScaling(): boolean {
      return this._fixedWidthScaling;
    }

    public set usesFixedWidthScaling(val: boolean) {
      this._fixedWidthScaling = val;
    }

    /**
     * Uses fixed scaling for heights of internal elements, rather than relative,
     * percent-based scaling.
     */
    public get usesFixedHeightScaling(): boolean {
      return this._fixedHeightScaling;
    }

    public set usesFixedHeightScaling(val: boolean) {
      this._fixedHeightScaling = val;
    }

    /**
     * Denotes if the VisualKeyboard or its containing OSKView / OSKManager uses
     * fixed positioning.
     */
    public get usesFixedPositioning(): boolean {
      let node: HTMLElement = this.element;
      while (node) {
        if (getComputedStyle(node).position == 'fixed') {
          return true;
        } else {
          node = node.offsetParent as HTMLElement;
        }
      }

      return false;
    }

    /**
     * Sets & tracks the size of the VisualKeyboard's primary element.
     * @param width
     * @param height
     * @param pending Set to `true` if called during a resizing interaction
     */
    public setSize(width?: number, height?: number, pending?: boolean) {
      this._width = width;
      this._height = height;

      if (this.kbdDiv) {
        this.kbdDiv.style.width = width ? this._width + 'px' : '';
        this.kbdDiv.style.height = height ? this._height + 'px' : '';

        if (!this.device.touchable && height) {
          this.fontSize = new ParsedLengthStyle((this._height / 8) + 'px');
        }

        if (!pending) {
          this.refreshLayout();
        }
      }
    }

    /**
     * Returns the default properties for a key object, used to construct
     * both a base keyboard key and popup keys
     *
     * @return    {Object}    An object that contains default key properties
     */
    getDefaultKeyObject(): OSKKeySpec {
      return new OSKKeySpec(undefined, '', keyboards.ActiveKey.DEFAULT_KEY.width, keyboards.ActiveKey.DEFAULT_KEY.sp as keyboards.ButtonClass,
        null, keyboards.ActiveKey.DEFAULT_KEY.pad);
    };
    //#endregion

    //#region OSK touch handlers
    getTouchCoordinatesOnKeyboard(input: InputEventCoordinate) {
      let keyman = com.keyman.singleton;

      // We need to compute the 'local', keyboard-based coordinates for the touch.
      let kbdCoords = keyman.util.getAbsolute(this.kbdDiv as HTMLElement);
      let offsetCoords = { x: input.x - kbdCoords.x, y: input.y - kbdCoords.y };

      // The layer group's element always has the proper width setting, unlike kbdDiv itself.
      offsetCoords.x /= this.layerGroup.element.offsetWidth;
      offsetCoords.y /= this.kbdDiv.offsetHeight;

      return offsetCoords;
    }

    getTouchProbabilities(input: InputEventCoordinate): text.KeyDistribution {
      let keyman = com.keyman.singleton;
      if (!keyman.core.languageProcessor.mayCorrect) {
        return null;
      }

      // Note:  if subkeys are active, they will still be displayed at this time.
      // TODO:  In such cases, we should build an ActiveLayout (of sorts) for subkey displays,
      //        update their geometries to the actual display values, and use the results here.
      let touchKbdPos = this.getTouchCoordinatesOnKeyboard(input);
      let layerGroup = this.layerGroup.element;  // Always has proper dimensions, unlike kbdDiv itself.
      let width = layerGroup.offsetWidth, height = this.kbdDiv.offsetHeight;
      // Prevent NaN breakages.
      if (!width || !height) {
        return null;
      }

      let kbdAspectRatio = layerGroup.offsetWidth / this.kbdDiv.offsetHeight;
      let baseKeyProbabilities = this.kbdLayout.getLayer(this.layerId).getTouchProbabilities(touchKbdPos, kbdAspectRatio);

      if (!this.subkeyGesture || !this.subkeyGesture.baseKey.key) {
        return baseKeyProbabilities;
      } else {
        // A temp-hack, as this was noted just before 14.0's release.
        // Since a more... comprehensive solution would be way too complex this late in the game,
        // this provides a half-decent stopgap measure.
        //
        // Will not correct to nearby subkeys; only includes the selected subkey and its base keys.
        // Still, better than ignoring them both for whatever base key is beneath the final cursor location.
        let baseMass = 1.0;

        let baseKeyMass = 1.0;
        let baseKeyID = this.subkeyGesture.baseKey.key.spec.coreID;

        let popupKeyMass = 0.0;
        let popupKeyID: string = null;

        // Note:  when embedded on Android (as of 14.0, at least), we don't get access to this.
        // Just the base key.
        if (this.keyPending && this.keyPending.key) {
          popupKeyMass = 3.0;
          popupKeyID = this.keyPending.key.spec.coreID;
        }

        // If the base key appears in the subkey array and was selected, merge the probability masses.
        if (popupKeyID == baseKeyID) {
          baseKeyMass += popupKeyMass;
          popupKeyMass = 0;
        } else {
          // We namespace it so that lookup operations know to find it via its base key.
          popupKeyID = `${baseKeyID}::${popupKeyID}`;
        }

        // Compute the normalization factor
        let totalMass = baseMass + baseKeyMass + popupKeyMass;
        let scalar = 1.0 / totalMass;

        // Prevent duplicate entries in the final map & normalize the remaining entries!
        for (let i = 0; i < baseKeyProbabilities.length; i++) {
          let entry = baseKeyProbabilities[i];
          if (entry.keyId == baseKeyID) {
            baseKeyMass += entry.p * scalar;
            baseKeyProbabilities.splice(i, 1);
            i--;
          } else if (entry.keyId == popupKeyID) {
            popupKeyMass = + entry.p * scalar;
            baseKeyProbabilities.splice(i, 1);
            i--;
          } else {
            entry.p *= scalar;
          }
        }

        let finalArray: { keyId: string, p: number }[] = [];

        if (popupKeyMass > 0) {
          finalArray.push({ keyId: popupKeyID, p: popupKeyMass * scalar });
        }

        finalArray.push({ keyId: baseKeyID, p: baseKeyMass * scalar });

        finalArray = finalArray.concat(baseKeyProbabilities);
        return finalArray;
      }
    }

    //#region Input handling start

    /**
     * Determines a "fuzzy boundary" area around the OSK within which active mouse and
     * touch events will be maintained, even if their coordinates lie outside of the OSK's
     * true visual bounds.
     * @returns A `BoundingRect`, in `.pageX` / `.pageY` coordinates.
     */
    private getInteractiveBoundingRect(): BoundingRect {
      // Determine the important geometric values involved
      const _Box = this.element.offsetParent as HTMLElement;
      let oskX = this.element.offsetLeft + (_Box?.offsetLeft || 0);
      let oskY = this.element.offsetTop + (_Box?.offsetTop || 0);

      // Determine the out-of-bounds threshold at which touch-cancellation should automatically occur.
      // Assuming square key-squares, we'll use 1/3 the height of a row for bounds detection
      // for both dimensions.
      const rowCount = this.currentLayer.rows.length;
      const buffer = (0.333 * this.height / rowCount);

      // Determine the OSK's boundaries and the boundaries of the page / view.
      // These values are needed in .pageX / .pageY coordinates for the final calcs.
      let boundingRect: BoundingRect = {
        left: oskX - buffer,
        right: oskX + this.width + buffer,
        top: oskY - buffer,
        bottom: oskY + this.height + buffer
      };

      // If the OSK is using fixed positioning (thus, viewport-relative), we need to
      // convert the 'clientX'-like values into 'pageX'-like values.
      if (this.usesFixedPositioning) {
        boundingRect.left += window.pageXOffset;
        boundingRect.right += window.pageXOffset;
        boundingRect.top += window.pageYOffset;
        boundingRect.bottom += window.pageYOffset;
      }

      return boundingRect;
    }

    /**
     * Adjusts a potential "interactive boundary" definition by enforcing an
     * "event cancellation zone" near screen boundaries that are not directly adjacent
     * to the ongoing input event's initial coordinate.
     *
     * This facilitates modeling of conventional cancellation gestures where a user would
     * drag the mouse or touch point off the OSK, as mouse and touch event handlers receive
     * no input beyond screen boundaries.
     *
     * @param baseBounds The baseline interactive bounding area to be adjusted
     * @param startCoord The initial coordinate of a currently-ongoing input event
     * @returns
     */
    private applyScreenMarginBoundsThresholding(baseBounds: BoundingRect,
      startCoord: InputEventCoordinate): BoundingRect {
      // Determine the needed linear translation to screen coordinates.
      const xDelta = window.screenLeft - window.pageXOffset;
      const yDelta = window.screenTop - window.pageYOffset;

      let adjustedBounds: BoundingRect = { ...baseBounds };

      // Also translate the initial touch's screen coord, as it affects our bounding box logic.
      const initScreenCoord = new InputEventCoordinate(startCoord.x + xDelta, startCoord.y + yDelta);

      // Detection:  is the OSK aligned with any screen boundaries?
      // If so, create a 'fuzzy' zone around the edges not near the initial touch point that allow
      // move-based cancellation.

      // If the initial input screen-coord is at least 5 pixels from the screen's left AND
      // the OSK's left boundary is within 2 pixels from the screen's left...
      if (initScreenCoord.x >= 5 && baseBounds.left + xDelta <= 2) {
        adjustedBounds.left = 2 - xDelta; // new `leftBound` is set to 2 pixels from the screen's left.
      }

      if (initScreenCoord.x <= screen.width - 5 && baseBounds.right + xDelta >= screen.width - 2) {
        adjustedBounds.right = (screen.width - 2) - xDelta; // new `rightBound` 2px from screen's right.
      }

      if (initScreenCoord.y >= 5 && baseBounds.top + yDelta <= 2) {
        adjustedBounds.top = 2 - yDelta;
      }

      if (initScreenCoord.y <= screen.height - 5 && baseBounds.bottom + yDelta >= screen.height - 2) {
        adjustedBounds.bottom = (screen.height - 2) - yDelta;
      }

      return adjustedBounds;
    }

    detectWithinInteractiveBounds(coord: InputEventCoordinate): boolean {
      // Shortcuts the method during unit testing, as we don't currently
      // provide coordinate values in its synthetic events.
      if (coord.x === null && coord.y === null) {
        return true;
      }

      const baseBoundingRect = this.getInteractiveBoundingRect();
      let adjustedBoundingRect = baseBoundingRect;
      if(this.initTouchCoord) {
        this.applyScreenMarginBoundsThresholding(baseBoundingRect, this.initTouchCoord);
      }

      // Now to check where the input coordinate lies in relation to the final bounding box!

      if (coord.x < adjustedBoundingRect.left || coord.x > adjustedBoundingRect.right) {
        return false;
      } else if (coord.y < adjustedBoundingRect.top || coord.y > adjustedBoundingRect.bottom) {
        return false;
      } else {
        return true;
      }
    }

    /**
     * The main OSK touch start event handler
     *
     *  @param  {Event} e   touch start event object
     *
     */
    touch(input: InputEventCoordinate) {
      // Identify the key touched
      var t = <HTMLElement>input.target, key = this.keyTarget(t);

      // Save the touch point, which is used for quick-display of popup keys (defined in highlightSubKeys)
      this.initTouchCoord = input;

      // Set the key for the new touch point to be current target, if defined
      this.currentTarget = key;

      // Clear repeated backspace if active, preventing 'sticky' behavior.
      this.cancelDelete();

      // Prevent multi-touch if popup displayed
      if (this.subkeyGesture && this.subkeyGesture.isVisible()) {
        return;
      }

      // Keep track of number of active (unreleased) touch points
      this.touchCount = input.activeInputCount;

      // Get nearest key if touching a hidden key or the end of a key row
      if ((key && ((key.className.indexOf('key-hidden') >= 0) || (key.className.indexOf('key-blank') >= 0)))
        || t.className.indexOf('kmw-key-row') >= 0) {

        // Perform "fudged" selection ops if and only if we're not sure about the precision of the
        // input source.  Mouse-based selection IS precise, so no need for "fudging" there.
        if (!input.isFromMouse) {
          key = this.findNearestKey(input, t);
        }
      }
      // Do not do anything if no key identified!
      if (key == null) {
        return;
      }

      // Get key name (K_...) from element ID
      let keyName = key['keyId'];

      // Highlight the touched key
      this.highlightKey(key, true);

      // Special function keys need immediate action
      if (keyName == 'K_LOPT' || keyName == 'K_ROPT') {
        window.setTimeout(function (this: VisualKeyboard) {
          this.modelKeyClick(key);
          // Because we immediately process the key, we need to re-highlight it after the click.
          this.highlightKey(key, true);
          // Highlighting'll be cleared automatically later.
        }.bind(this), 0);
        this.keyPending = null;
        this.touchPending = null;

        // Also backspace, to allow delete to repeat while key held
      } else if (keyName == 'K_BKSP') {
        // While we could inline the execution of the delete key here, we lose the ability to
        // record the backspace key if we do so.
        this.modelKeyClick(key, input);
        this.deleteKey = key;
        this.deleting = window.setTimeout(this.repeatDelete, 500);
        this.keyPending = null;
        this.touchPending = null;
      } else {
        if (this.keyPending) {
          this.highlightKey(this.keyPending, false);

          if (this.subkeyGesture && this.subkeyGesture instanceof browser.SubkeyPopup) {
            let subkeyPopup = this.subkeyGesture as browser.SubkeyPopup;
            subkeyPopup.updateTouch(input);
            subkeyPopup.finalize(input);
          } else {
            this.modelKeyClick(this.keyPending, this.touchPending);
          }
          // Decrement the number of unreleased touch points to prevent
          // sending the keystroke again when the key is actually released
          this.touchCount--;
        } else {
          // If this key has subkey, start timer to display subkeys after delay, set up release
          this.initGestures(key, input);
        }
        this.keyPending = key;
        this.touchPending = input;
      }
    }

    /**
     * OSK touch release event handler
     *
     *  @param  {Event} e   touch release event object
     *
     **/
    release(input: InputEventCoordinate): void {
      // Prevent incorrect multi-touch behaviour if native or device popup visible
      var t = this.currentTarget;

      // Clear repeated backspace if active, preventing 'sticky' behavior.
      this.cancelDelete();

      if ((this.subkeyGesture && this.subkeyGesture.isVisible())) {
        // Ignore release if a multiple touch
        if (input.activeInputCount > 0) {
          return;
        }

        if (this.subkeyGesture instanceof browser.SubkeyPopup) {
          let subkeyPopup = this.subkeyGesture as browser.SubkeyPopup;
          subkeyPopup.finalize(input);
        }
        this.highlightKey(this.keyPending, false);
        this.keyPending = null;
        this.touchPending = null;

        return;
      }

      // Handle menu key release event
      if (t && t.id) {
        this.optionKey(t, t.id, false);
      }

      // Test if moved off screen (effective release point must be corrected for touch point horizontal speed)
      // This is not completely effective and needs some tweaking, especially on Android
      if (!this.detectWithinInteractiveBounds(input)) {
        this.moveCancel(input);
        this.touchCount--;
        return;
      }

      // Save then decrement current touch count
      var tc = this.touchCount;
      if (this.touchCount > 0) {
        this.touchCount--;
      }

      // Process and clear highlighting of pending target
      if (this.keyPending) {
        this.highlightKey(this.keyPending, false);
        // Output character unless moved off key
        if (this.keyPending.className.indexOf('hidden') < 0 && tc > 0) {
          this.modelKeyClick(this.keyPending, input);
        }
        this.clearPopup();
        this.keyPending = null;
        this.touchPending = null;
        // Always clear highlighting of current target on release (multi-touch)
      } else {
        var tt = input;
        t = this.keyTarget(tt.target);
        if (!t) {
          // Operates relative to the viewport, not based on the actual coordinate on the page.
          var t1 = document.elementFromPoint(input.x - window.pageXOffset, input.y - window.pageYOffset);
          t = this.findNearestKey(input, <HTMLElement>t1);
        }

        this.highlightKey(t, false);
      }
    }

    moveCancel(input: InputEventCoordinate): void {
      // Do not attempt to support reselection of target key for overlapped keystrokes.
      // Perform _after_ ensuring possible sticky keys have been cancelled.
      if (input.activeInputCount > 1) {
        return;
      }

      // Update all gesture tracking.  The function returns true if further input processing
      // should be blocked.  (Keeps the subkey array operating when the input coordinate has
      // moved outside the OSK's boundaries.)
      if (this.updateGestures(null, this.keyPending, input)) {
        return;
      }

      this.cancelDelete();

      this.highlightKey(this.keyPending, false);
      this.showKeyTip(null, false);
      this.clearPopup();
      this.keyPending = null;
      this.touchPending = null;
    }

    /**
     * OSK touch move event handler
     *
     *  @param  {Event} e   touch move event object
     *
     **/
    moveOver(input: InputEventCoordinate): void {
      // Shouldn't be possible, but just in case.
      if (this.touchCount == 0) {
        this.cancelDelete();
        return;
      }

      // Get touch position
      const x = input.x - window.pageXOffset;
      const y = input.y - window.pageYOffset;

      // Move target key and highlighting
      this.touchPending = input;
      // Operates on viewport-based coordinates, not page-based.
      var t1 = <HTMLElement>document.elementFromPoint(x, y);
      const key0 = this.keyPending;
      let key1 = this.keyTarget(t1); // Not only gets base keys, but also gets popup keys!

      // Find the nearest key to the touch point if not on a visible key
      if ((key1 && key1.className.indexOf('key-hidden') >= 0) ||
        (t1 && (!key1) && t1.className.indexOf('key-row') >= 0)) {
        key1 = this.findNearestKey(input, t1);
      }

      // Cancels BKSP if it's not the key.  (Note... could also cancel BKSP if the ongoing
      // input is cancelled, regardless of key, just to be safe.)

      // Stop repeat if no longer on BKSP key
      if (key1 && (typeof key1.id == 'string') && (key1.id.indexOf('-K_BKSP') < 0)) {
        this.cancelDelete();
      }

      // Cancels if it's a multitouch attempt.

      // Do not attempt to support reselection of target key for overlapped keystrokes.
      // Perform _after_ ensuring possible sticky keys have been cancelled.
      if (input.activeInputCount > 1) {
        return;
      }

      // Gesture-updates should probably be a separate call from other touch-move aspects.

      // Update all gesture tracking.  The function returns true if further input processing
      // should be blocked.
      if (this.updateGestures(key1, key0, input)) {
        return;
      }

      // Identify current touch position (to manage off-key release)
      this.currentTarget = key1;

      // Only NOW do we denote the newly-selected key as the currently-focused key.

      // Replace the target key, if any, by the new target key
      // Do not replace a null target, as that indicates the key has already been released
      if (key1 && this.keyPending) {
        this.highlightKey(key0, false);
        this.keyPending = key1;
        this.touchPending = input;
      }

      if (key0 && key1 && (key1 != key0) && (key1.id != '')) {
        // While there may not be an active subkey menu, we should probably update which base key
        // is being highlighted by the current touch & start a pending longpress for it.
        this.clearPopup();
        this.initGestures(key1, input);
      }

      if (this.keyPending) {
        if (key0 != key1 || key1.className.indexOf('kmw-key-touched') < 0) {
          this.highlightKey(key1, true);
        }
      }
    }

    //#endregion

    /**
     * Get the current key target from the touch point element within the key
     *
     * @param   {Object}  t   element at touch point
     * @return  {Object}      the key element (or null)
     **/
    keyTarget(target: HTMLElement | EventTarget): KeyElement {
      let t = <HTMLElement>target;

      try {
        if (t) {
          if (t.classList.contains('kmw-key')) {
            return getKeyFrom(t);
          }
          if (t.parentNode && (t.parentNode as HTMLElement).classList.contains('kmw-key')) {
            return getKeyFrom(t.parentNode);
          }
          if (t.firstChild && (t.firstChild as HTMLElement).classList.contains('kmw-key')) {
            return getKeyFrom(t.firstChild);
          }
        }
      } catch (ex) { }
      return null;
    }

    /**
     * Identify the key nearest to the touch point if at the end of a key row,
     * but return null more than about 0.6 key width from the nearest key.
     *
     *  @param  {Event}   e   touch event
     *  @param  {Object}  t   HTML object at touch point
     *  @return {Object}      nearest key to touch point
     *
     **/
    findNearestKey(input: InputEventCoordinate, t: HTMLElement): KeyElement {
      if (!input) {
        return null;
      }

      // Get touch point on screen
      var x = input.x;

      // Get key-row beneath touch point
      while (t && t.className !== undefined && t.className.indexOf('key-row') < 0) {
        t = <HTMLElement>t.parentNode;
      }
      if (!t) {
        return null;
      }

      // Find minimum distance from any key
      var k, k0 = 0, dx, dxMax = 24, dxMin = 100000, x1, x2;
      for (k = 0; k < t.childNodes.length; k++) {
        let keySquare = t.childNodes[k] as HTMLElement; // gets the .kmw-key-square containing a key
        // Find the actual key element.
        let childNode = keySquare.firstChild ? keySquare.firstChild as HTMLElement : keySquare;

        if (childNode.className !== undefined
          && (childNode.className.indexOf('key-hidden') >= 0
            || childNode.className.indexOf('key-blank') >= 0)) {
          continue;
        }
        x1 = keySquare.offsetLeft;
        x2 = x1 + keySquare.offsetWidth;
        if (x >= x1 && x <= x2) {
          // Within the key square
          return <KeyElement>childNode;
        }
        dx = x1 - x;
        if (dx >= 0 && dx < dxMin) {
          // To right of key
          k0 = k; dxMin = dx;
        }
        dx = x - x2;
        if (dx >= 0 && dx < dxMin) {
          // To left of key
          k0 = k; dxMin = dx;
        }
      }

      if (dxMin < 100000) {
        t = <HTMLElement>t.childNodes[k0];
        x1 = t.offsetLeft;
        x2 = x1 + t.offsetWidth;

        // Limit extended touch area to the larger of 0.6 of key width and 24 px
        if (t.offsetWidth > 40) {
          dxMax = 0.6 * t.offsetWidth;
        }

        if (((x1 - x) >= 0 && (x1 - x) < dxMax) || ((x - x2) >= 0 && (x - x2) < dxMax)) {
          return <KeyElement>t.firstChild;
        }
      }
      return null;
    }

    /**
     *  Repeat backspace as long as the backspace key is held down
     **/
    repeatDelete: () => void = function (this: VisualKeyboard) {
      if (this.deleting) {
        this.modelKeyClick(this.deleteKey);
        this.deleting = window.setTimeout(this.repeatDelete, 100);
      }
    }.bind(this);

    /**
     * Cancels any active repeatDelete() timeouts, ensuring that
     * repeating backspace operations are properly terminated.
     */
    cancelDelete() {
      // Clears the delete-repeating timeout.
      if (this.deleting) {
        window.clearTimeout(this.deleting);
      }
      this.deleting = 0;
    }
    //#endregion

    modelKeyClick(e: osk.KeyElement, input?: InputEventCoordinate) {
      let keyEvent = this.initKeyEvent(e, input);

      // TODO:  convert into an actual event, raised by the VisualKeyboard.
      //        Its code is intended to lie outside of the OSK-Core library/module.
      PreProcessor.raiseKeyEvent(keyEvent);
    }

    initKeyEvent(e: osk.KeyElement, input?: InputEventCoordinate) {
      // Turn off key highlighting (or preview)
      this.highlightKey(e, false);

      // Future note:  we need to refactor osk.OSKKeySpec to instead be a 'tag field' for
      // keyboards.ActiveKey.  (Prob with generics, allowing the Web-only parts to
      // be fully specified within the tag.)
      //
      // Would avoid the type shenanigans needed here because of our current type-abuse setup
      // for key spec tracking.
      let keySpec = (e['key'] ? e['key'].spec : null) as unknown as keyboards.ActiveKey;
      if (!keySpec) {
        console.error("OSK key with ID '" + e.id + "', keyID '" + e.keyId + "' missing needed specification");
        return null;
      }

      // Return the event object.
      return this.keyEventFromSpec(keySpec, input);
    }

    keyEventFromSpec(keySpec: keyboards.ActiveKey, input?: InputEventCoordinate) {
      let core = com.keyman.singleton.core; // only singleton-based ref currently needed here.

      // Start:  mirrors _GetKeyEventProperties

      // First check the virtual key, and process shift, control, alt or function keys
      let Lkc = keySpec.constructKeyEvent(core.keyboardProcessor, this.device);

      // End - mirrors _GetKeyEventProperties

      if (core.languageProcessor.isActive && input) {
        Lkc.source = input;
        Lkc.keyDistribution = this.getTouchProbabilities(input);
      }

      // Return the event object.
      return Lkc;
    }

    // cancel = function(e) {} //cancel event is never generated by iOS

    /**
     * Function     _UpdateVKShiftStyle
     * Scope        Private
     * @param       {string=}   layerId
     * Description  Updates the OSK's visual style for any toggled state keys
     */
    _UpdateVKShiftStyle(layerId?: string) {
      var i;
      let core = com.keyman.singleton.core;

      if (!layerId) {
        layerId = this.layerId;
      }

      const layer = this.layerGroup.layers[layerId];
      if (!layer) {
        return;
      }

      // So... through KMW 14, we actually never tracked the capsKey, numKey, and scrollKey
      // properly for keyboard-defined layouts - only _default_, desktop-style layouts.
      //
      // We _could_ remedy this, but then... touch keyboards like khmer_angkor actually
      // repurpose certain state keys, and in an inconsistent manner at that.
      // Considering the potential complexity of touch layouts, with multiple possible
      // layer-shift keys, it's likely best to just leave things as they are for now.
      if (!core.activeKeyboard?.usesDesktopLayoutOnDevice(this.device)) {
        return;
      }

      // Set the on/off state of any visible state keys.
      const states = ['K_CAPS', 'K_NUMLOCK', 'K_SCROLL'];
      const keys = [layer.capsKey, layer.numKey, layer.scrollKey];

      for (i = 0; i < keys.length; i++) {
        // Skip any keys not in the OSK!
        if (keys[i] == null) {
          continue;
        }

        keys[i].setToggleState(core.keyboardProcessor.stateKeys[states[i]]);
      }
    }

    clearPopup() {
      // Remove the displayed subkey array, if any, and cancel popup request
      if (this.subkeyGesture) {
        this.subkeyGesture.clear();
        this.subkeyGesture = null;
      }

      if (this.pendingSubkey) {
        this.pendingSubkey.cancel();
        this.pendingSubkey = null;
      }
    }

    //#endregion

    /**
     * Indicate the current language and keyboard on the space bar
     **/
    showLanguage() {
      let keyman = com.keyman.singleton;

      let displayName: string = undefined;
      let activeStub = keyman.keyboardManager.activeStub;

      if (activeStub) {
        if (activeStub['displayName'] != null) {
          displayName = activeStub['displayName'];
        } else {
          let
            lgName: string = activeStub['KL'],
            kbdName: string = activeStub['KN'];
          kbdName = kbdName.replace(/\s*keyboard\s*/i, '');
          switch (keyman.options['spacebarText']) {
            case SpacebarText.KEYBOARD:
              displayName = kbdName;
              break;
            case SpacebarText.LANGUAGE:
              displayName = lgName;
              break;
            case SpacebarText.LANGUAGE_KEYBOARD:
              displayName = (kbdName == lgName) ? lgName : lgName + ' - ' + kbdName;
              break;
            case SpacebarText.BLANK:
              displayName = '';
              break;
            default:
              displayName = kbdName;
          }
        }
      } else {
        displayName = '(System keyboard)';
      }

      try {
        var t = <HTMLElement>this.spaceBar.key.label;
        let tParent = <HTMLElement>t.parentNode;
        if (typeof (tParent.className) == 'undefined' || tParent.className == '') {
          tParent.className = 'kmw-spacebar';
        } else if (tParent.className.indexOf('kmw-spacebar') == -1) {
          tParent.className += ' kmw-spacebar';
        }

        if (t.className != 'kmw-spacebar-caption') {
          t.className = 'kmw-spacebar-caption';
        }

        // It sounds redundant, but this dramatically cuts down on browser DOM processing;
        // but sometimes innerText is reported empty when it actually isn't, so set it
        // anyway in that case (Safari, iOS 14.4)
        if (t.innerText != displayName || displayName == '') {
          t.innerText = displayName;
        }
      }
      catch (ex) { }
    }

    /**
     *  Add or remove a class from a keyboard key (when touched or clicked)
     *  or add a key preview for phone devices
     *
     *  @param    {Object}    key   key affected
     *  @param    {boolean}   on    add or remove highlighting
     **/
    highlightKey(key: KeyElement, on: boolean) {
      // Do not change element class unless a key
      if (!key || !key.key || (key.className == '') || (key.className.indexOf('kmw-key-row') >= 0)) return;

      // For phones, use key preview rather than highlighting the key,
      var usePreview = (this.keytip != null) && key.key.allowsKeyTip();

      if (usePreview) {
        this.showKeyTip(key, on);
      } else {
        if (on) {
          // May be called on already-unhighlighted keys, so we don't remove the tip here.
          this.showKeyTip(null, false);
        }
        key.key.highlight(on);
      }
    }

    /**
     * Use of `getComputedStyle` is ideal, but in many of our use cases its preconditions are not met.
     * This function allows us to calculate the font size in those situations.
     */
    getKeyEmFontSize(): number {
      if (!this.fontSize) {
        return 0;
      }

      if (this.device.formFactor == 'desktop') {
        let keySquareScale = 0.8; // Set in kmwosk.css, is relative.
        return this.fontSize.scaledBy(keySquareScale).val;
      } else {
        let emSizeStr = getComputedStyle(document.body).fontSize;
        let emSize = getFontSizeStyle(emSizeStr).val;

        var emScale = 1;
        if (!this.isStatic) {
          // Double-check against the font scaling applied to the _Box element.
          if (this.fontSize.absolute) {
            return this.fontSize.val;
          } else {
            emScale = this.fontSize.val;
          }
        }
        return emSize * emScale;
      }
    }

    updateState() {
      // May happen for desktop-oriented keyboards that neglect to specify a touch layout.
      // See `test_chirality.js` from the unit-test keyboard suite, which tests keystrokes
      // using modifiers that lack corresponding visual-layout representation.
      if (!this.currentLayer) {
        return;
      }

      var n, b = this.kbdDiv.childNodes[0].childNodes;
      this.nextLayer = this.layerId;

      if (this.currentLayer.nextlayer) {
        this.nextLayer = this.currentLayer.nextlayer;
      }

      for (n = 0; n < b.length; n++) {
        let layerElement = <HTMLDivElement>b[n];
        if (layerElement['layer'] == this.layerId) {
          layerElement.style.display = 'block';
          //b[n].style.visibility='visible';

          // Most functions that call this one often indicate a change in modifier
          // or state key state.  Keep it updated!
          this._UpdateVKShiftStyle();
        } else {
          layerElement.style.display = 'none';
          //layerElement.style.visibility='hidden';
        }
      }
    }

    /**
     * Used to refresh the VisualKeyboard's geometric layout and key sizes
     * when needed.
     */
    refreshLayout() {
      let keyman = com.keyman.singleton;
      let device = this.device;

      var fs = 1.0;
      // TODO: Logically, this should be needed for Android, too - may need to be changed for the next version!
      if (device.OS == utils.OperatingSystem.iOS && !keyman.isEmbedded) {
        fs = fs / keyman.util.getViewportScale();
      }

      let paddedHeight: number;
      if (this.height) {
        paddedHeight = this.computedAdjustedOskHeight(this.height);
      }

      let b = this.layerGroup.element as HTMLElement;
      let gs = this.kbdDiv.style;
      let bs = b.style;
      if (this.usesFixedHeightScaling) {
        // Sets the layer group to the correct height.
        gs.height = gs.maxHeight = paddedHeight + 'px';
      }

      // The font-scaling applied on the layer group.
      gs.fontSize = this.fontSize.styleString;
      bs.fontSize = ParsedLengthStyle.forScalar(fs).styleString;

      // Needs the refreshed layout info to work correctly.
      for (const layerId in this.layerGroup.layers) {
        const layer = this.layerGroup.layers[layerId];
        layer.refreshLayout(this, paddedHeight, this.height);
      }

      // NEW CODE ------

      // Step 1:  have the necessary conditions been met?
      const fixedSize = this.width && this.height;
      const computedStyle = getComputedStyle(this.kbdDiv);
      const isInDOM = computedStyle.height != '' && computedStyle.height != 'auto';

      // Step 2:  determine basic layout geometry
      if (fixedSize) {
        this._computedWidth = this.width;
        this._computedHeight = this.height;
      } else if (isInDOM) {
        this._computedWidth = parseInt(computedStyle.width, 10);
        if (!this._computedWidth) {
          // For touch keyboards, the width _was_ specified on the layer group,
          // not the root element (`kbdDiv`).
          const groupStyle = getComputedStyle(this.kbdDiv.firstElementChild);
          this._computedWidth = parseInt(groupStyle.width, 10);
        }
        this._computedHeight = parseInt(computedStyle.height, 10);
      } else {
        // Cannot perform layout operations!
        return;
      }

      // Step 3:  perform layout operations.  (Handled by 'old code' section below.)

      // END NEW CODE -----------

      // Needs the refreshed layout info to work correctly.
      for (const layerId in this.layerGroup.layers) {
        const layer = this.layerGroup.layers[layerId];
        layer.refreshLayout(this, paddedHeight, this._computedHeight);
      }
    }

    /*private*/ computedAdjustedOskHeight(allottedHeight: number): number {
      if (!this.layerGroup) {
        return allottedHeight;
      }

      const layers = this.layerGroup.layers;
      let oskHeight = 0;

      // In case the keyboard's layers have differing row counts, we check them all for the maximum needed oskHeight.
      for (const layerID in layers) {
        const layer = layers[layerID];
        let nRows = layer.rows.length;
        let rowHeight = Math.floor(allottedHeight / (nRows == 0 ? 1 : nRows));
        let layerHeight = nRows * rowHeight;

        if (layerHeight > oskHeight) {
          oskHeight = layerHeight;
        }
      }

      // This isn't set anywhere else; it's a legacy part of the original methods.
      const oskPad = 0;
      let oskPaddedHeight = oskHeight + oskPad;

      return oskPaddedHeight;
    }

    /**
     *  Append a style sheet for the current keyboard if needed for specifying an embedded font
     *  or to re-apply the default element font
     *
     **/
    appendStyleSheet() {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      var activeKeyboard = keymanweb.core.activeKeyboard;
      var activeStub: com.keyman.keyboards.KeyboardStub = keymanweb.keyboardManager.activeStub;

      // Do not do anything if a null stub
      if (activeStub == null) {
        return;
      }

      // First remove any existing keyboard style sheet
      if (this.styleSheet) {
        util.removeStyleSheet(this.styleSheet);
      }

      var i, kfd = activeStub['KFont'], ofd = activeStub['KOskFont'];

      // Add style sheets for embedded fonts if necessary (each font-face style will only be added once)
      util.addFontFaceStyleSheet(kfd);
      util.addFontFaceStyleSheet(ofd);

      // Temporarily hide duplicated elements on non-desktop browsers
      keymanweb.hideInputs();

      // Build the style string and append (or replace) the font style sheet
      // Note: Some browsers do not download the font-face font until it is applied,
      //       so must apply style before testing for font availability
      // Extended to allow keyboard-specific custom styles for Build 360
      var customStyle = this.addFontStyle(kfd, ofd);
      if (activeKeyboard != null && typeof (activeKeyboard.oskStyling) == 'string')  // KMEW-129
        customStyle = customStyle + activeKeyboard.oskStyling;

      this.styleSheet = util.addStyleSheet(customStyle); //Build 360

      // Wait until font is loaded then align duplicated input elements with page elements
      if (this.waitForFonts(kfd, ofd)) {
        keymanweb.alignInputs();
      }
    }

    /**
     *  Add or replace the style sheet used to set the font for input elements and OSK
     *
     *  @param  {Object}  kfd   KFont font descriptor
     *  @param  {Object}  ofd   OSK font descriptor (if any)
     *  @return {string}
     *
     **/
    addFontStyle(kfd, ofd): string {
      let keymanweb = com.keyman.singleton;

      // Get name of font to be applied
      var fn = keymanweb.baseFont;
      if (typeof (kfd) != 'undefined' && typeof (kfd['family']) != 'undefined') {
        fn = kfd['family'];
      }

      // Unquote font name in base font (if quoted)
      fn = fn.replace(/\u0022/g, '');

      // Set font family chain for mapped elements and remove any double quotes
      var rx = new RegExp('\\s?' + fn + ',?'), ff = keymanweb.appliedFont.replace(/\u0022/g, '');

      // Remove base font name from chain if present
      ff = ff.replace(rx, '');
      ff = ff.replace(/,$/, '');

      // Then replace it at the head of the chain
      if (ff == '') {
        ff = fn;
      } else {
        ff = fn + ',' + ff;
      }

      // Re-insert quotes around individual font names
      ff = '"' + ff.replace(/\,\s?/g, '","') + '"';

      // Add to the stylesheet, quoted, and with !important to override any explicit style
      var s = '.keymanweb-font{\nfont-family:' + ff + ' !important;\n}\n';

      // Set font family for OSK text
      if (typeof (ofd) != 'undefined') {
        s = s + '.kmw-key-text{\nfont-family:"' + ofd['family'].replace(/\u0022/g, '').replace(/,/g, '","') + '";\n}\n';
      } else if (typeof (kfd) != 'undefined') {
        s = s + '.kmw-key-text{\nfont-family:"' + kfd['family'].replace(/\u0022/g, '').replace(/,/g, '","') + '";\n}\n';
      }

      // Store the current font chain (with quote-delimited font names)
      keymanweb.appliedFont = ff;

      // Return the style string
      return s;
    }

    /**
     * Create copy of the OSK that can be used for embedding in documentation or help
     * The currently active keyboard will be returned if PInternalName is null
     *
     *  @param  {Object}            PKbd            the keyboard object to be displayed
     *  @param  {string=}           argFormFactor   layout form factor, defaulting to 'desktop'
     *  @param  {(string|number)=}  argLayerId      name or index of layer to show, defaulting to 'default'
     *  @param  {number}            height          Target height for the rendered keyboard
     *                                              (currently required for legacy reasons)
     *  @return {Object}                            DIV object with filled keyboard layer content
     */
    static buildDocumentationKeyboard(PKbd: com.keyman.keyboards.Keyboard, argFormFactor, argLayerId, height: number): HTMLElement { // I777
      if (!PKbd) {
        return null;
      }

      var formFactor = (typeof (argFormFactor) == 'undefined' ? 'desktop' : argFormFactor),
        layerId = (typeof (argLayerId) == 'undefined' ? 'default' : argLayerId),
        device = new Device();

      // Device emulation for target documentation.
      device.formFactor = formFactor;
      if (formFactor != 'desktop') {
        device.OS = 'iOS';
        device.touchable = true;
      } else {
        device.OS = 'windows';
        device.touchable = false;
      }

      let layout = PKbd.layout(formFactor);

      let kbdObj = new VisualKeyboard(PKbd, device.coreSpec, device.coreSpec, true);

      // The 'documentation' format uses the base element's child as the actual display base.
      // Since there's no backing kmw-osk-frame, we do need the static-class kmw-osk-inner-frame
      // to perform background styling on our behalf.  We'll trust the actual, live keyboard rules
      // for the other elements, which in turn needs the non-static variant of the CSS rules.
      kbdObj.layerGroup.element.className = kbdObj.kbdDiv.className + ' ' + device.formFactor
        + '-static ' + device.OS.toLowerCase();

      let kbd = kbdObj.kbdDiv.childNodes[0] as HTMLDivElement; // Gets the layer group.

      // Select the layer to display, and adjust sizes
      if (layout != null) {
        kbdObj.layerId = layerId;
        kbdObj.updateState();
        // This still feels fairly hacky... but something IS needed to constrain the height.
        // There are plans to address related concerns through some of the later aspects of
        // the Web OSK-Core design.
        kbdObj.setSize(800, height); // Probably need something for width, too, rather than
        // assuming 100%.
        kbdObj.refreshLayout(); // Necessary for the row heights to be properly set!
        // Relocates the font size definition from the main VisualKeyboard wrapper, since we don't return the whole thing.
        kbd.style.fontSize = kbdObj.kbdDiv.style.fontSize;
        kbd.style.height = kbdObj.kbdDiv.style.height;
        kbd.style.maxHeight = kbdObj.kbdDiv.style.maxHeight;
      } else {
        kbd.innerHTML = "<p style='color:#c40; font-size:0.5em;margin:10px;'>No " + formFactor + " layout is defined for " + PKbd.name + ".</p>";
      }
      // Add a faint border
      kbd.style.border = '1px solid #ccc';

      // Once the element is inserted into the DOM, refresh the layout so that proper text scaling may apply.
      const refreshInterval = window.setInterval(function () {
        let computedStyle = getComputedStyle(kbd);
        if (computedStyle.fontSize) {
          if (kbd.style.fontSize) {
            // Preserve the new setting (provided by CSS)
            kbdObj.fontSize = new ParsedLengthStyle(kbd.style.fontSize);
          }
          kbdObj.refreshLayout();
          window.clearInterval(refreshInterval);
        }
      }, 10);

      return kbd;
    }

    onHide() {
      // Remove highlighting from hide keyboard key, if applied
      if (this.hkKey) {
        this.highlightKey(this.hkKey, false);
      }
    }

    /**
     * Starts an implementation-specific longpress gesture.  Separately implemented for
     * in-browser and embedded modes.
     * @param key The base key of the longpress.
     * @returns
     */
    startLongpress(key: KeyElement): PendingGesture {
      let _this = this;

      // First-level object/Promise:  will produce a subkey popup when the longpress gesture completes.
      // 'Returns' a second-level object/Promise:  resolves when a subkey is selected or is cancelled.
      let pendingLongpress = new browser.PendingLongpress(this, key);
      pendingLongpress.promise.then(function (subkeyPopup) {
        // In-browser-specific handling.
        if (subkeyPopup) {
          // Append the touch-hold (subkey) array to the OSK
          let keyman = com.keyman.singleton;
          keyman.osk._Box.appendChild(subkeyPopup.element);
          keyman.osk._Box.appendChild(subkeyPopup.shim);

          // Must be placed after its `.element` has been inserted into the DOM.
          subkeyPopup.reposition(_this);
        }
      });

      return pendingLongpress;
    }

    /**
     * Initializes all supported gestures given a base key and the triggering touch coordinates.
     * @param key     The gesture's base key
     * @param touch   The starting touch coordinates for the gesture
     * @returns
     */
    initGestures(key: KeyElement, input: InputEventCoordinate) {
      if (key['subKeys']) {
        let _this = this;

        let pendingLongpress = this.startLongpress(key);
        if (pendingLongpress == null) {
          return;
        }
        this.pendingSubkey = pendingLongpress;

        pendingLongpress.promise.then(function (subkeyPopup) {
          if (_this.pendingSubkey == pendingLongpress) {
            _this.pendingSubkey = null;
          }

          if (subkeyPopup) {
            // Clear key preview if any
            _this.showKeyTip(null, false);

            _this.subkeyGesture = subkeyPopup;
            subkeyPopup.promise.then(function (keyEvent: text.KeyEvent) {
              // Allow active cancellation, even if the source should allow passive.
              // It's an easy and cheap null guard.
              if (keyEvent) {
                PreProcessor.raiseKeyEvent(keyEvent);
              }
              _this.clearPopup();
            });
          }
        });
      }
    }

    /**
     * Updates all currently-pending and activated gestures.
     *
     * @param currentKey    The key currently underneath the most recent touch coordinate
     * @param previousKey   The previously-selected key
     * @param input         The current mouse or touch coordinate for the gesture
     * @returns true if should fully capture input, false if input should 'fall through'.
     */
    updateGestures(currentKey: KeyElement, previousKey: KeyElement, input: InputEventCoordinate): boolean {
      let key0 = previousKey;
      let key1 = currentKey;

      // Clear previous key highlighting, allow subkey controller to highlight as appropriate.
      if (this.subkeyGesture) {
        if (key0) {
          key0.key.highlight(false);
        }
        this.subkeyGesture.updateTouch(input);

        this.keyPending = null;
        this.touchPending = null;

        return true;
      }

      this.currentTarget = null;

      // If popup is visible, need to move over popup, not over main keyboard
      // Could be turned into a browser-longpress specific implementation within browser.PendingLongpress?
      if (key1 && key1['subKeys'] != null && this.initTouchCoord) {
        // Show popup keys immediately if touch moved up towards key array (KMEW-100, Build 353)
        if ((this.initTouchCoord.y - input.y > 5) && this.pendingSubkey && this.pendingSubkey instanceof browser.PendingLongpress) {
          this.pendingSubkey.resolve();
        }
      }

      // If there is an active popup menu (which can occur from the previous block),
      // a subkey popup exists; do not allow base key output.
      if (this.subkeyGesture) {
        return true;
      }

      return false;
    }

    optionKey(e: KeyElement, keyName: string, keyDown: boolean) {
      let keyman = com.keyman.singleton;
      let oskManager = keyman.osk;
      if (keyDown) {
        if (keyName.indexOf('K_LOPT') >= 0) {
          oskManager.showLanguageMenu();
        } else if (keyName.indexOf('K_ROPT') >= 0) {
          keyman.uiManager.setActivatingUI(false);
          oskManager.startHide(true);
          let active = keyman.domManager.activeElement;
          if (dom.Utils.instanceof(active, "TouchAliasElement")) {
            (active as dom.TouchAliasElement).hideCaret();
          }
          keyman.domManager.lastActiveElement = null;
        }
      }
    };

    /**
     * Add (or remove) the keytip preview (if KeymanWeb on a phone device)
     *
     * @param   {Object}  key   HTML key element
     * @param   {boolean} on    show or hide
     */
    showKeyTip(key: KeyElement, on: boolean) {
      var tip = this.keytip;

      // Do not change the key preview unless key or state has changed
      if (tip == null || (key == tip.key && on == tip.state)) {
        return;
      }

      let sk = this.subkeyGesture;
      let popup = (sk && sk.isVisible());

      // If popup keys are active, do not show the key tip.
      on = popup ? false : on;

      tip.show(key, on, this);
    };

    /**
     *  Create a key preview element for phone devices
     */
    createKeyTip() {
      let keyman = com.keyman.singleton;

      if (this.device.formFactor == 'phone') {
        if (this.keytip == null) {
          // For now, should only be true (in production) when keyman.isEmbedded == true.
          let constrainPopup = keyman.isEmbedded;
          this.keytip = new browser.KeyTip(constrainPopup);
        }

        // Always append to _Box (since cleared during OSK Load)
        if (this.keytip && this.keytip.element) {
          keyman.osk._Box.appendChild(this.keytip.element);
        }
      }
    };

    /**
     * Wait until font is loaded before applying stylesheet - test each 100 ms
     * @param   {Object}  kfd   main font descriptor
     * @param   {Object}  ofd   secondary font descriptor (OSK only)
     * @return  {boolean}
     */
    waitForFonts(kfd, ofd) {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      let fontDefined = !!(kfd && kfd['files']);
      kfd = fontDefined ? kfd : undefined;

      let oskFontDefined = !!(ofd && ofd['files']);
      ofd = oskFontDefined ? ofd : undefined;

      // Automatically 'ready' if the descriptor is explicitly `undefined`.
      // Thus, also covers the case where both are undefined.
      var kReady = util.checkFontDescriptor(kfd), oReady = util.checkFontDescriptor(ofd);
      if (kReady && oReady) {
        return true;
      }

      keymanweb.fontCheckTimer = window.setInterval(function () {
        if (util.checkFontDescriptor(kfd) && util.checkFontDescriptor(ofd)) {
          window.clearInterval(keymanweb.fontCheckTimer);
          keymanweb.fontCheckTimer = null;
          keymanweb.alignInputs();
        }
      }, 100);

      // Align anyway as best as can if font appears to remain uninstalled after 5 seconds
      window.setTimeout(function () {
        if (keymanweb.fontCheckTimer) {
          window.clearInterval(keymanweb.fontCheckTimer);
          keymanweb.fontCheckTimer = null;
          keymanweb.alignInputs();
          // Don't notify - this is a management issue, not anything the user needs to deal with
          // TODO: Consider having an icon in the OSK with a bubble that indicates missing font
          //util.alert('Unable to download the font normally used with '+ks['KN']+'.');
        }
      }, 5000);
      return false;
    };

    shutdown() {
      let keyman = com.keyman.singleton;

      // Prevents style-sheet pollution from multiple keyboard swaps.
      if (this.styleSheet) {
        keyman.util.removeStyleSheet(this.styleSheet);
      }

      if(this.inputEngine) {
        this.inputEngine.unregisterEventHandlers();
      }
    }
  }
}
