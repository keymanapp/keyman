/// <reference path="preProcessor.ts" />
/// <reference path="utils.ts" />
/// <reference path="oskBaseKey.ts" />
/// <reference path="keytip.interface.ts" />
/// <reference path="browser/keytip.ts" />
/// <reference path="browser/pendingLongpress.ts" />
/// <reference path="keyboardView.interface.ts" />

namespace com.keyman.osk {
  export class VisualKeyboard implements KeyboardView {
    // Legacy alias, maintaining a reference for code built against older
    // versions of KMW.
    static specialCharacters = OSKKey.specialCharacters;

    /**
     * Contains layout properties corresponding to the OSK's layout.  Needs to be public
     * so that its geometry may be updated on rotations and keyboard resize events, as
     * said geometry needs to be accurate for fat-finger probability calculations.
     */
    layout: keyboards.ActiveLayout;
    layers: keyboards.LayoutLayer[];
    private _layerId: string = "default";
    readonly isRTL: boolean;
    layerIndex: number;

    device: Device;
    isStatic: boolean = false;

    // Stores the base element for this instance of the visual keyboard.
    // Formerly known as osk._DivVKbd
    kbdDiv: HTMLDivElement;
    styleSheet: HTMLStyleElement;

    _width: number;
    _height: number;

    // Style-related properties
    fontFamily: string;
    fontSize: string;

    // State-related properties
    keyPending: KeyElement;
    touchPending: Touch;
    deleteKey: KeyElement;
    deleting: number; // Tracks a timer id for repeated deletions.
    nextLayer: string;
    currentKey: string;

    // Special keys (for the currently-visible layer)
    lgKey: KeyElement;
    hkKey: KeyElement; // hide keyboard key
    spaceBar: KeyElement;

    // Touch-tracking properties
    touchX: number;
    touchY: number;
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

    //#region OSK constructor and helpers

    /**
     * @param       {Object}      PVK         Visual keyboard name
     * @param       {Object}      Lhelp       true if OSK defined for this keyboard
     * @param       {Object}      layout0
     * @param       {Number}      kbdBitmask  Keyboard modifier bitmask
     * Description  Generates the base visual keyboard element, prepping for attachment to KMW
     */
    constructor(keyboard: keyboards.Keyboard, device: Device, isStatic?: boolean) {
      this.device = device;
      if(isStatic) {
        this.isStatic = isStatic;
      }

      // Create the collection of HTML elements from the device-dependent layout object
      var Lkbd=document.createElement('div');
      let layout: keyboards.ActiveLayout;
      if(keyboard) {
        layout = this.layout = keyboard.layout(device.formFactor as utils.FormFactor);
        this.isRTL = keyboard.isRTL;
      } else {
        // This COULD be called with no backing keyboard; KMW will try to force-show the OSK even without
        // a backing keyboard on mobile, using the most generic default layout as the OSK's base.
        //
        // In KMW's current state, it'd take a major break, though - Processor always has an activeKeyboard,
        // even if it's "hollow".
        let rawLayout = keyboards.Layouts.buildDefaultLayout(null, null, device.formFactor);
        layout = this.layout = keyboards.ActiveLayout.polyfill(rawLayout, null, device.formFactor as utils.FormFactor);
        this.isRTL = false;
      }
      this.layers=layout['layer'];

      // Override font if specified by keyboard
      if('font' in layout) {
        this.fontFamily=layout['font'];
      } else {
        this.fontFamily='';
      }

      let divLayerContainer = this.deviceDependentLayout(keyboard, device.formFactor as utils.FormFactor);

      // Append the OSK layer group container element to the containing element
      //osk.keyMap = divLayerContainer;
      Lkbd.appendChild(divLayerContainer);

      // Set base class - OS and keyboard added for Build 360
      this.kbdDiv = Lkbd;

      if(this.isStatic) {
        // The 'documentation' format uses the base element's child as the actual display base.
        (Lkbd.childNodes[0] as HTMLDivElement).className = device.formFactor + '-static kmw-osk-inner-frame';
      } else {
        Lkbd.className = device.formFactor + ' kmw-osk-inner-frame';
      }
    }

    public get element(): HTMLDivElement {
      return this.kbdDiv;
    }

    public postInsert(): void { }

    get width(): number {
      if(this._width) {
        return this._width;
      } else {
        this.loadSizeFromCookie();
        return this.width;
      }
    }

    get height(): number {
      if(this._height) {
        return this._height;
      } else {
        this.loadSizeFromCookie();
        return this.height;
      }
    }

    protected loadSizeFromCookie() {
      let keyman = com.keyman.singleton;
      let util = keyman.util;

      // If no prior cookie exists, it merely returns an empty object / cookie.
      var c = util.loadCookie('KeymanWeb_OnScreenKeyboard');
      var newWidth: number, newHeight: number;

      // Restore OSK size - font size now fixed in relation to OSK height, unless overridden (in em) by keyboard
      newWidth=util.toNumber(c['width'], 0.333 * screen.width); // Default - 1/3rd of screen's width.

      if(newWidth < 0.2*screen.width) {
        newWidth = 0.2*screen.width;
      } else if(newWidth > 0.9*screen.width) {
        newWidth=0.9*screen.width;
      }

      // Default height decision made here:
      // https://github.com/keymanapp/keyman/pull/4279#discussion_r560453929
      newHeight=util.toNumber(c['height'], 0.333 * newWidth);

      if(newHeight < 0.15*screen.height) {
        newHeight = 0.15 * screen.height;
      } else if(newHeight > 0.5*screen.height) {
        newHeight=0.5*screen.height;
      }

      this.setSize(newWidth, newHeight);
    }

    /**
     * Sets & tracks the size of the VisualKeyboard's primary element.
     * @param width
     * @param height
     * @param pending Set to `true` if called during a resizing interaction
     */
    public setSize(width: number, height: number, pending?: boolean) {
      this._width = width;
      this._height = height;

      if(!pending && this.kbdDiv) {
        this.kbdDiv.style.width=this._width+'px';
        this.kbdDiv.style.height=this._height+'px';
        this.kbdDiv.style.fontSize=(this._height/8)+'px';
      }
    }

    public defaultFontSize(): number {
      return this.height / 8;
    }

    /**
     * Called by OSKManager after resize operations in order to determine the final
     * size actually used by the visual keyboard.
     */
    public refit() {
      this._width=this.kbdDiv.offsetWidth;
      this._height=this.kbdDiv.offsetHeight;
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

    /**
     * Create the OSK for a particular keyboard and device
     *
     * @param       {Object}              layout      OSK layout definition
     * @param       {string}              formFactor  layout form factor
     * @return      {Object}                          fully formatted OSK object
     */
    deviceDependentLayout(keyboard: keyboards.Keyboard, formFactor: utils.FormFactor): HTMLDivElement {
      if(!keyboard) {
        // May occasionally be null in embedded contexts; have seen this when iOS engine sets
        // keyboard height during change of keyboards.
        keyboard = new keyboards.Keyboard(null);
      }
      let layout = keyboard.layout(formFactor);
      let oskManager = com.keyman.singleton.osk;

      var lDiv=document.createElement('div'), ls=lDiv.style;

      // Set OSK box default style
      lDiv.className='kmw-key-layer-group';

      // Return empty DIV if no layout defined
      if(layout == null) {
        return lDiv;
      }

      // Set default OSK font size (Build 344, KMEW-90)
      let layoutFS = layout['fontsize'];
      if(typeof layoutFS == 'undefined' || layoutFS == null || layoutFS == '') {
        ls.fontSize='1em';
      } else {
        ls.fontSize=layout['fontsize'];
      }

      this.fontSize=ls.fontSize;       //TODO: move outside function*********

      // Create a separate OSK div for each OSK layer, only one of which will ever be visible
      var n: number, i: number, j: number;
      var layers: keyboards.LayoutLayer[], gDiv: HTMLDivElement;
      var rowHeight: number, rDiv: HTMLDivElement;
      var keys: keyboards.ActiveKey[], key: keyboards.ActiveKey, rs: CSSStyleDeclaration, gs: CSSStyleDeclaration;

      layers=layout['layer'];

      // Set key default attributes (must use exportable names!)
      var tKey=this.getDefaultKeyObject();
      tKey['fontsize']=ls.fontSize;

      // ***Delete any empty rows at the end added by compiler bug...
      for(n=0; n<layers.length; n++) {
        let layer=layers[n];
        let rows=layer['row'];
        for(i=rows.length; i>0; i--) {
          if(rows[i-1]['key'].length > 0) {
            break;
          }
        }

        if(i < rows.length) {
          rows.splice(i-rows.length,rows.length-i);
        }
      }
      // ...remove to here when compiler bug fixed ***

      // Set the OSK row height, **assuming all layers have the same number of rows**

      // Calculate default row height
      rowHeight = 100/layers[0].row.length;

      // Get the actual available document width and scale factor according to device type
      var objectWidth : number;
      if(formFactor == 'desktop' || this.isStatic) {
        objectWidth = 100;
      } else {
        objectWidth = oskManager.getWidth();
      }

      if(!this.isStatic && this.device.touchable) { //  /*&& ('ontouchstart' in window)*/ // Except Chrome emulation doesn't set this.
                                                                        // Not to mention, it's rather redundant.
        lDiv.addEventListener('touchstart', this.touch, true);
        // The listener below fails to capture when performing automated testing checks in Chrome emulation unless 'true'.
        lDiv.addEventListener('touchend', this.release,true);
        lDiv.addEventListener('touchmove', this.moveOver,false);
        //lDiv.addEventListener('touchcancel', osk.cancel,false); //event never generated by iOS
      }

      let precalibrated = (keyboard.getLayoutState(formFactor) == keyboards.LayoutState.CALIBRATED);

      for(n=0; n<layers.length; n++) {
        let layer=layers[n] as keyboards.ActiveLayer;
        gDiv=document.createElement('div'), gs=gDiv.style;
        gDiv.className='kmw-key-layer';

        // Always make the first layer visible
        gs.display=(n==0?'block':'none');
        gs.height=ls.height;

        // Set font for layer if defined in layout
        if('font' in layout) gs.fontFamily=layout['font']; else gs.fontFamily='';

        gDiv['layer']=gDiv['nextLayer']=layer['id'];
        if(typeof layer['nextlayer'] == 'string') gDiv['nextLayer']=layer['nextlayer'];

        // Create a DIV for each row of the group
        let rows=layer['row'];

        for(i=0; i<rows.length; i++) {
          rDiv=document.createElement('div');
          rDiv.className='kmw-key-row';
          // The following event trap is needed to prevent loss of focus in IE9 when clicking on a key gap.
          // Unclear why normal _CreateElement prevention of loss of focus does not seem to work here.
          // Appending handler to event handler chain does not work (other event handling remains active).
          rDiv.onmousedown = function(e: MouseEvent) {
            if(e) {
              e.preventDefault();
            }
          }

          let row=rows[i];
          rs=rDiv.style;

          // Set row height. (Phone and tablet heights are later recalculated
          // and set in px, allowing for viewport scaling.)
          rs.maxHeight=rs.height=rowHeight+'%';

          // Apply defaults, setting the width and other undefined properties for each key
          keys=row['key'];

          if(!precalibrated || this.isStatic) {
            // Calculate actual key widths by multiplying by the OSK's width and rounding appropriately,
            // adjusting the width of the last key to make the total exactly 100%.
            // Overwrite the previously-computed percent.
            // NB: the 'percent' suffix is historical, units are percent on desktop devices, but pixels on touch devices
            // All key widths and paddings are rounded for uniformity
            for(j=0; j<keys.length; j++) {
              key = keys[j];
              // TODO:  reinstate rounding?
              key['widthpc'] = key.proportionalWidth * objectWidth;
              key['padpc'] = key.proportionalPad * objectWidth;
            }
          }

          //Create the key square (an outer DIV) for each key element with padding, and an inner DIV for the button (btn)
          var totalPercent=0;
          for(j=0; j<keys.length; j++) {
            key=keys[j];

            var keyGenerator = new OSKBaseKey(key as OSKKeySpec, layer['id']);
            var keyTuple = keyGenerator.construct(this, layout, rs, totalPercent);

            rDiv.appendChild(keyTuple.element);
            totalPercent += keyTuple.percent;
          }
          // Add row to layer
          gDiv.appendChild(rDiv);
        }
        // Add layer to group
        lDiv.appendChild(gDiv);
      }

      // Now that we've properly processed the keyboard's layout, mark it as calibrated.
      keyboard.markLayoutCalibrated(formFactor);
      return lDiv;
    }
    //#endregion

    //#region OSK touch handlers
    getTouchCoordinatesOnKeyboard(touch: Touch) {
      let keyman = com.keyman.singleton;

      // We need to compute the 'local', keyboard-based coordinates for the touch.
      let kbdCoords = keyman.util.getAbsolute(this.kbdDiv as HTMLElement);
      let offsetCoords = {x: touch.pageX - kbdCoords.x, y: touch.pageY - kbdCoords.y};

      let layerGroup = this.kbdDiv.firstChild as HTMLDivElement;  // Always has proper dimensions, unlike kbdDiv itself.
      offsetCoords.x /= layerGroup.offsetWidth;
      offsetCoords.y /= this.kbdDiv.offsetHeight;

      return offsetCoords;
    }

    getTouchProbabilities(touch: Touch): text.KeyDistribution {
      let keyman = com.keyman.singleton;
      if(!keyman.core.languageProcessor.mayCorrect) {
        return null;
      }

      // Note:  if subkeys are active, they will still be displayed at this time.
      // TODO:  In such cases, we should build an ActiveLayout (of sorts) for subkey displays,
      //        update their geometries to the actual display values, and use the results here.
      let touchKbdPos = this.getTouchCoordinatesOnKeyboard(touch);
      let layerGroup = this.kbdDiv.firstChild as HTMLDivElement;  // Always has proper dimensions, unlike kbdDiv itself.
      let width = layerGroup.offsetWidth, height = this.kbdDiv.offsetHeight;
      // Prevent NaN breakages.
      if(!width || !height) {
        return null;
      }

      let kbdAspectRatio = layerGroup.offsetWidth / this.kbdDiv.offsetHeight;
      let baseKeyProbabilities = this.layout.getLayer(this.layerId).getTouchProbabilities(touchKbdPos, kbdAspectRatio);

      if(!this.subkeyGesture || !this.subkeyGesture.baseKey.key) {
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
        if(this.keyPending && this.keyPending.key) {
          popupKeyMass = 3.0;
          popupKeyID = this.keyPending.key.spec.coreID;
        }

        // If the base key appears in the subkey array and was selected, merge the probability masses.
        if(popupKeyID == baseKeyID) {
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
        for(let i=0; i < baseKeyProbabilities.length; i++) {
          let entry = baseKeyProbabilities[i];
          if(entry.keyId == baseKeyID) {
            baseKeyMass += entry.p * scalar;
            baseKeyProbabilities.splice(i, 1);
            i--;
          } else if(entry.keyId == popupKeyID) {
            popupKeyMass =+ entry.p * scalar;
            baseKeyProbabilities.splice(i, 1);
            i--;
          } else {
            entry.p *= scalar;
          }
        }

        let finalArray: {keyId: string, p: number}[] = [];

        if(popupKeyMass > 0) {
          finalArray.push({keyId: popupKeyID, p: popupKeyMass * scalar});
        }

        finalArray.push({keyId: baseKeyID, p: baseKeyMass * scalar});

        finalArray = finalArray.concat(baseKeyProbabilities);
        return finalArray;
      }
    }

    /**
     * The main OSK touch start event handler
     *
     *  @param  {Event} e   touch start event object
     *
     */
    touch: (e: TouchEvent) => void = function(this: VisualKeyboard, e: TouchEvent) {
      // Identify the key touched
      var t = <HTMLElement> e.changedTouches[0].target, key = this.keyTarget(t);

      // Save the touch point
      this.touchX = e.changedTouches[0].pageX;
      // Used for quick-display of popup keys (defined in highlightSubKeys)
      this.touchY = e.changedTouches[0].pageY;

      // Set the key for the new touch point to be current target, if defined
      this.currentTarget = key;

      // Clear repeated backspace if active, preventing 'sticky' behavior.
      this.cancelDelete();

      // Prevent multi-touch if popup displayed
      if(this.subkeyGesture && this.subkeyGesture.isVisible()) {
        return;
      }

      // Keep track of number of active (unreleased) touch points
      this.touchCount = e.touches.length;

      // Get nearest key if touching a hidden key or the end of a key row
      if((key && ((key.className.indexOf('key-hidden') >= 0) || (key.className.indexOf('key-blank') >= 0)))
        || t.className.indexOf('kmw-key-row') >= 0) {
        key = this.findNearestKey(e,t);
      }
      // Do not do anything if no key identified!
      if(key == null) {
        return;
      }

      // Get key name (K_...) from element ID
      let keyName = key['keyId'];

      // Highlight the touched key
      this.highlightKey(key,true);

      // Special function keys need immediate action
      if(keyName == 'K_LOPT' || keyName == 'K_ROPT')      {
        window.setTimeout(function(this: VisualKeyboard){
          this.modelKeyClick(key);
          // Because we immediately process the key, we need to re-highlight it after the click.
          this.highlightKey(key, true);
          // Highlighting'll be cleared automatically later.
        }.bind(this),0);
        this.keyPending = null;
        this.touchPending = null;

        // Also backspace, to allow delete to repeat while key held
      } else if(keyName == 'K_BKSP') {
        // While we could inline the execution of the delete key here, we lose the ability to
        // record the backspace key if we do so.
        this.modelKeyClick(key, e.changedTouches[0]);
        this.deleteKey = key;
        this.deleting = window.setTimeout(this.repeatDelete,500);
        this.keyPending = null;
        this.touchPending = null;
      } else {
        if(this.keyPending) {
          this.highlightKey(this.keyPending, false);

          if(this.subkeyGesture && this.subkeyGesture instanceof browser.SubkeyPopup) {
            let subkeyPopup = this.subkeyGesture as browser.SubkeyPopup;
            subkeyPopup.updateTouch(e.changedTouches[0]);
            subkeyPopup.finalize(e.changedTouches[0]);
          } else {
            this.modelKeyClick(this.keyPending, this.touchPending);
          }
          // Decrement the number of unreleased touch points to prevent
          // sending the keystroke again when the key is actually released
          this.touchCount--;
        } else {
          // If this key has subkey, start timer to display subkeys after delay, set up release
          this.initGestures(key, e.changedTouches[0]);
        }
        this.keyPending = key;
        this.touchPending = e.changedTouches[0];
      }
    }.bind(this);

    /**
     * OSK touch release event handler
     *
     *  @param  {Event} e   touch release event object
     *
     **/
    release: (e: TouchEvent) => void = function(this: VisualKeyboard, e: TouchEvent) {
      // Prevent incorrect multi-touch behaviour if native or device popup visible
      var t = this.currentTarget;

      // Clear repeated backspace if active, preventing 'sticky' behavior.
      this.cancelDelete();

      if((this.subkeyGesture && this.subkeyGesture.isVisible())) {
        // Ignore release if a multiple touch
        if(e.touches.length > 0) {
          return;
        }

        if(this.subkeyGesture instanceof browser.SubkeyPopup) {
          let subkeyPopup = this.subkeyGesture as browser.SubkeyPopup;
          subkeyPopup.finalize(e.changedTouches[0]);
        }
        this.highlightKey(this.keyPending,false);
        this.keyPending = null;
        this.touchPending = null;

        return;
      }

      // Handle menu key release event
      if(t && t.id) {
        this.optionKey(t, t.id, false);
      }

      // Test if moved off screen (effective release point must be corrected for touch point horizontal speed)
      // This is not completely effective and needs some tweaking, especially on Android
      var x = e.changedTouches[0].pageX;
      var beyondEdge = ((x < 2 && this.touchX > 5) || (x > window.innerWidth - 2 && this.touchX < window.innerWidth - 5));

      // Save then decrement current touch count
      var tc=this.touchCount;
      if(this.touchCount > 0) {
        this.touchCount--;
      }

      // Process and clear highlighting of pending target
      if(this.keyPending) {
        this.highlightKey(this.keyPending,false);
        // Output character unless moved off key
        if(this.keyPending.className.indexOf('hidden') < 0 && tc > 0 && !beyondEdge) {
          this.modelKeyClick(this.keyPending, e.changedTouches[0]);
        }
        this.clearPopup();
        this.keyPending = null;
        this.touchPending = null;
        // Always clear highlighting of current target on release (multi-touch)
      } else {
        var tt = e.changedTouches[0];
        t = this.keyTarget(tt.target);
        if(!t) {
          var t1 = document.elementFromPoint(tt.clientX,tt.clientY);
          t = this.findNearestKey(e, <HTMLElement> t1);
        }

        this.highlightKey(t,false);
      }
    }.bind(this);

    /**
     * OSK touch move event handler
     *
     *  @param  {Event} e   touch move event object
     *
     **/
    moveOver: (e: TouchEvent) => void = function(this: VisualKeyboard, e: TouchEvent) {
      let keyman = com.keyman.singleton;
      e.preventDefault();
      e.cancelBubble=true;

      if(typeof e.stopImmediatePropagation == 'function') {
        e.stopImmediatePropagation();
      } else if(typeof e.stopPropagation == 'function') {
        e.stopPropagation();
      }

      // Shouldn't be possible, but just in case.
      if(this.touchCount == 0) {
        this.cancelDelete();
        return;
      }

      // Get touch position
      var x=typeof e.touches == 'object' ? e.touches[0].clientX : e.clientX,
          y=typeof e.touches == 'object' ? e.touches[0].clientY : e.clientY;

      // Move target key and highlighting
      this.touchPending = e.changedTouches[0];
      var t1 = <HTMLElement> document.elementFromPoint(x,y),
          key0 = this.keyPending,
          key1 = this.keyTarget(t1); // Not only gets base keys, but also gets popup keys!

      // Find the nearest key to the touch point if not on a visible key
      if((key1 && key1.className.indexOf('key-hidden') >= 0) ||
        (t1 && (!key1) && t1.className.indexOf('key-row') >= 0)) {
          key1 = this.findNearestKey(e,t1);
      }

      // Stop repeat if no longer on BKSP key
      if(key1 && (typeof key1.id == 'string') && (key1.id.indexOf('-K_BKSP') < 0)) {
        this.cancelDelete();
      }

      // Do not attempt to support reselection of target key for overlapped keystrokes.
      // Perform _after_ ensuring possible sticky keys have been cancelled.
      if(e.touches.length > 1) {
        return;
      }

      // Update all gesture tracking.  The function returns true if further input processing
      // should be blocked.
      if(this.updateGestures(key1, key0, e.changedTouches[0])) {
        return;
      }

      // Identify current touch position (to manage off-key release)
      this.currentTarget = key1;

      // _Box has (most of) the useful client values.
      let _Box = this.kbdDiv.parentElement ? this.kbdDiv.parentElement : keyman.osk._Box;
      let height = this.kbdDiv.offsetHeight;
      // We need to adjust the offset properties by any offsets related to the active banner.

      // Determine the y-threshold at which touch-cancellation should automatically occur.
      let rowCount = this.layers[this.layerIndex].row.length;
      let yBufferThreshold = (0.333 * height / rowCount); // Allows vertical movement by 1/3 the height of a row.
      var yMin = (this.kbdDiv && _Box) ? Math.max(5, this.kbdDiv.offsetTop - yBufferThreshold) : 5;
      if(key0 && e.touches[0].pageY < yMin) {
        this.highlightKey(key0,false);
        this.showKeyTip(null,false);
        this.keyPending = null;
        this.touchPending = null;
      }

      // Replace the target key, if any, by the new target key
      // Do not replace a null target, as that indicates the key has already been released
      if(key1 && this.keyPending) {
        this.keyPending = key1;
        this.touchPending = e.touches[0];
      }

      if(this.keyPending) {
        if(key0 != key1 || key1.className.indexOf('kmw-key-touched') < 0) {
          this.highlightKey(key1,true);
        }
      }
    }.bind(this);

    /**
     * Get the current key target from the touch point element within the key
     *
     * @param   {Object}  t   element at touch point
     * @return  {Object}      the key element (or null)
     **/
    keyTarget(target: HTMLElement | EventTarget): KeyElement {
      let t = <HTMLElement> target;

      try {
        if(t) {
          if(t.classList.contains('kmw-key')) {
            return getKeyFrom(t);
          }
          if(t.parentNode && (t.parentNode as HTMLElement).classList.contains('kmw-key')) {
            return getKeyFrom(t.parentNode);
          }
          if(t.firstChild && (t.firstChild as HTMLElement).classList.contains('kmw-key')) {
            return getKeyFrom(t.firstChild);
          }
        }
      } catch(ex) {}
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
    findNearestKey(e: TouchEvent, t: HTMLElement): KeyElement {
      if((!e) || (typeof e.changedTouches == 'undefined')
        || (e.changedTouches.length == 0)) {
        return null;
      }

      // Get touch point on screen
      var x = e.changedTouches[0].pageX;

      // Get key-row beneath touch point
      while(t && t.className !== undefined && t.className.indexOf('key-row') < 0) {
        t = <HTMLElement> t.parentNode;
      }
      if(!t) {
        return null;
      }

      // Find minimum distance from any key
      var k, k0=0, dx, dxMax=24, dxMin=100000, x1, x2;
      for(k = 0; k < t.childNodes.length; k++) {
        let keySquare = t.childNodes[k] as HTMLElement; // gets the .kmw-key-square containing a key
        // Find the actual key element.
        let childNode = keySquare.firstChild ? keySquare.firstChild as HTMLElement: keySquare;

        if(childNode.className !== undefined
            && (childNode.className.indexOf('key-hidden') >= 0
             || childNode.className.indexOf('key-blank') >= 0)) {
          continue;
        }
        x1 = keySquare.offsetLeft;
        x2 = x1 + keySquare.offsetWidth;
        if(x >= x1 && x <= x2) {
          // Within the key square
          return <KeyElement> childNode;
        }
        dx = x1 - x;
        if(dx >= 0 && dx < dxMin) {
          // To right of key
          k0 = k; dxMin = dx;
        }
        dx = x - x2;
        if(dx >= 0 && dx < dxMin) {
          // To left of key
          k0 = k; dxMin = dx;
        }
      }

      if(dxMin < 100000) {
        t = <HTMLElement> t.childNodes[k0];
        x1 = t.offsetLeft;
        x2 = x1 + t.offsetWidth;

        // Limit extended touch area to the larger of 0.6 of key width and 24 px
        if(t.offsetWidth > 40) {
          dxMax = 0.6 * t.offsetWidth;
        }

        if(((x1 - x) >= 0 && (x1 - x) < dxMax) || ((x - x2) >= 0 && (x - x2) < dxMax)) {
          return <KeyElement> t.firstChild;
        }
      }
      return null;
    }

    /**
     *  Repeat backspace as long as the backspace key is held down
     **/
    repeatDelete: () => void = function(this: VisualKeyboard) {
      if(this.deleting) {
        this.modelKeyClick(this.deleteKey);
        this.deleting = window.setTimeout(this.repeatDelete,100);
      }
    }.bind(this);

    /**
     * Cancels any active repeatDelete() timeouts, ensuring that
     * repeating backspace operations are properly terminated.
     */
    cancelDelete() {
      // Clears the delete-repeating timeout.
      if(this.deleting) {
        window.clearTimeout(this.deleting);
      }
      this.deleting = 0;
    }
    //#endregion

    modelKeyClick(e: osk.KeyElement, touch?: Touch) {
      let keyEvent = this.initKeyEvent(e, touch);

      // TODO:  convert into an actual event, raised by the VisualKeyboard.
      //        Its code is intended to lie outside of the OSK-Core library/module.
      PreProcessor.raiseKeyEvent(keyEvent);
    }

    initKeyEvent(e: osk.KeyElement, touch?: Touch) {
      // Turn off key highlighting (or preview)
      this.highlightKey(e,false);

      // Future note:  we need to refactor osk.OSKKeySpec to instead be a 'tag field' for
      // keyboards.ActiveKey.  (Prob with generics, allowing the Web-only parts to
      // be fully specified within the tag.)
      //
      // Would avoid the type shenanigans needed here because of our current type-abuse setup
      // for key spec tracking.
      let keySpec = (e['key'] ? e['key'].spec : null) as unknown as keyboards.ActiveKey;
      if(!keySpec) {
        console.error("OSK key with ID '" + e.id + "', keyID '" + e.keyId + "' missing needed specification");
        return null;
      }
      
      // Return the event object.
      return this.keyEventFromSpec(keySpec, touch);
    }

    keyEventFromSpec(keySpec: keyboards.ActiveKey, touch?: Touch) {
      let core = com.keyman.singleton.core; // only singleton-based ref currently needed here.

      // Start:  mirrors _GetKeyEventProperties

      // First check the virtual key, and process shift, control, alt or function keys
      let Lkc = keySpec.constructKeyEvent(core.keyboardProcessor, this.device.coreSpec);

      // If it's actually a state key modifier, trigger its effects immediately, as KeyboardEvents would do the same.
      switch(Lkc.kName) {
        case 'K_CAPS':
        case 'K_NUMLOCK':
        case 'K_SCROLL':
          core.keyboardProcessor.stateKeys[Lkc.kName] = ! core.keyboardProcessor.stateKeys[Lkc.kName];
      }

      // End - mirrors _GetKeyEventProperties

      if(core.languageProcessor.isActive && touch) {
        Lkc.source = touch;
        Lkc.keyDistribution = this.getTouchProbabilities(touch);;
      }

      // Return the event object.
      return Lkc;
    }

    // cancel = function(e) {} //cancel event is never generated by iOS

    /**
     * Function     findKeyElement
     * Scope        Private
     * @param       {string}   layerId
     * @param       {string}   keyId
     * Description  Finds the DOM element associated with the specified key, if it exists.
     */
    findKeyElement(layerId: string, keyId: string) {
      let layerGroup = this.kbdDiv.firstChild as HTMLDivElement;

      for(let i = 0; i < layerGroup.childElementCount; i++) {
        // TODO:  At some point, our OSK construction should 'link' a TS metadata type to this,
        // like with OSKKey / KeyElement for keys.
        let layer = layerGroup.childNodes[i] as HTMLDivElement;
        // row -> key-square -> actual KeyElement.
        let currentLayerId = (layer.firstChild.firstChild.firstChild as KeyElement).key.layer
        if(currentLayerId == layerId) {
          // Layer identified!  Now to find the key.  First - iterate over rows.
          for(let r = 0; r < layer.childElementCount; r++) {
            let row = layer.childNodes[r] as HTMLDivElement;
            for(let k = 0; k < row.childElementCount; k++) {
              let key = row.childNodes[k].firstChild as KeyElement;
              if(key.keyId == keyId) {
                return key;
              }
            }
          }
        }
      }

      return null;
    }

    /**
     * Function     _UpdateVKShiftStyle
     * Scope        Private
     * @param       {string=}   layerId
     * Description  Updates the OSK's visual style for any toggled state keys
     */
    _UpdateVKShiftStyle(layerId?: string) {
      var i, n, layer=null;
      let core = com.keyman.singleton.core;

      if(layerId) {
        for(n=0; n<this.layers.length; n++) {
          if(this.layers[n]['id'] == this.layerId) {
            break;
          }
        }

        return;  // Failed to find the requested layer.
      } else {
        n=this.layerIndex;
        layerId=this.layers[n]['id'];
      }

      layer=this.layers[n];

      // Set the on/off state of any visible state keys.
      var states = ['K_CAPS',      'K_NUMLOCK',  'K_SCROLL'];
      var keys   = [layer.capsKey, layer.numKey, layer.scrollKey];

      for(i=0; i < keys.length; i++) {
        // Skip any keys not in the OSK!
        if(keys[i] == null) {
          continue;
        }

        keys[i]['sp'] = core.keyboardProcessor.stateKeys[states[i]] ? keyboards.Layouts.buttonClasses['SHIFT-ON'] : keyboards.Layouts.buttonClasses['SHIFT'];
        let keyId = layerId+'-'+states[i]
        var btn = document.getElementById(keyId) as KeyElement;

        if(btn == null) {
          //This can happen when using BuildDocumentationKeyboard, as the OSK isn't yet in the
          //document hierarchy.  Sometimes.  (It's weird.)
          btn = this.findKeyElement(layerId, states[i]);
        }

        if(btn != null) {
          btn.key.setButtonClass(this);
        } else {
          console.warn("Could not find key to apply style: \"" + keyId + "\"");
        }
      }
    }

    clearPopup() {
      // Remove the displayed subkey array, if any, and cancel popup request
      if(this.subkeyGesture) {
        this.subkeyGesture.clear();
        this.subkeyGesture = null;
      }

      if(this.pendingSubkey) {
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

      if(activeStub) {
        if(activeStub['displayName']) {
          displayName = activeStub['displayName'];
        } else {
          let
            lgName: string = activeStub['KL'],
            kbdName: string = activeStub['KN'];
          kbdName = kbdName.replace(/\s*keyboard\s*/i,'');
          switch(keyman.options['spacebarText']) {
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
        var t=<HTMLElement> this.spaceBar.firstChild;
        let tParent = <HTMLElement> t.parentNode;
        if(typeof(tParent.className) == 'undefined' || tParent.className == '') {
          tParent.className='kmw-spacebar';
        } else if(tParent.className.indexOf('kmw-spacebar') == -1) {
          tParent.className +=' kmw-spacebar';
        }

        if(t.className != 'kmw-spacebar-caption') {
          t.className='kmw-spacebar-caption';
        }

        // It sounds redundant, but this dramatically cuts down on browser DOM processing;
        // but sometimes innerText is reported empty when it actually isn't, so set it
        // anyway in that case (Safari, iOS 14.4)
        if(t.innerText != displayName || displayName == '') {
          t.innerText = displayName;
        }
      }
      catch(ex){}
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
      if(!key || !key.key || (key.className == '') || (key.className.indexOf('kmw-key-row') >= 0)) return;

      // For phones, use key preview rather than highlighting the key,
      var usePreview = (this.keytip != null) && key.key.allowsKeyTip();

      if(usePreview) {
        this.showKeyTip(key,on);
      } else {
        if(on) {
          // May be called on already-unhighlighted keys, so we don't remove the tip here.
          this.showKeyTip(null,false);
        }
        key.key.highlight(on);
      }
    }

    //#region Mouse-event handling
    /**
     * Mouse down/mouse over event handler (desktop only)
     *
     * @param   {Event}  e  mouse over/mouse down event object
     */
    mouseOverMouseDownHandler = function(this: VisualKeyboard, e: MouseEvent) {
      let keyman = com.keyman.singleton;
      let util = keyman.util;

      var t = <HTMLElement> util.eventTarget(e);
      if(t === null || this.device.formFactor != 'desktop') {
        return;
      }

      if(t.nodeName == 'SPAN') {
        t = <HTMLElement> t.parentNode;
      }

      let key = this.keyTarget(t);

      if(util.eventType(e) == 'mousedown') {
        this.currentKey=key.id;
        util._CancelMouse(e);
        this.highlightKey(key, true);
      } else if(key.id == this.currentKey) {
        this.highlightKey(key, true);
      }
    }.bind(this);

    /**
     * Mouse up/mouse out event handler (desktop only)
     *
     * @param   {Event}  e  mouse up/out event object
     */
    mouseUpMouseOutHandler = function(this: VisualKeyboard, e: MouseEvent) {
      let keyman = com.keyman.singleton;
      let util = keyman.util;

      var t=<HTMLElement> util.eventTarget(e);
      if(t === null || this.device.formFactor != 'desktop') {
        return;
      }

      if(t.nodeName == 'SPAN') {
        t = <HTMLElement> t.parentNode;
      }

      let key = this.keyTarget(t);
      this.highlightKey(key, false);

      // Process as click if mouse button released anywhere over key
      if(util.eventType(e) == 'mouseup') {
        if(key.id == this.currentKey) {
          this.modelKeyClick(key);
        }
        this.currentKey='';
      }
    }.bind(this);
    //#endregion

    /**
     * Use of `getComputedStyle` is ideal, but in many of our use cases its preconditions are not met.
     * This function allows us to calculate the font size in those situations.
     */
    getKeyEmFontSize() {
      let keyman = com.keyman.singleton;

      if(this.device.formFactor == 'desktop') {
        let kbdFontSize = this.defaultFontSize();
        let keySquareScale = 0.8; // Set in kmwosk.css, is relative.
        return kbdFontSize * keySquareScale;
      } else {
        let emSizeStr = getComputedStyle(document.body).fontSize;
        let emSize = getFontSizeStyle(emSizeStr).val;

        var emScale = 1;
        if(!this.isStatic) {
          // Reading this requires the OSK to be active, so we filter out
          // BuildVisualKeyboard calls here.
          let boxFontStyle = getFontSizeStyle(keyman.osk._Box);

          // Double-check against the font scaling applied to the _Box element.
          if(boxFontStyle.absolute) {
            return boxFontStyle.val;
          } else {
            emScale = boxFontStyle.val;
          }
        }
        return emSize * emScale;
      }
    }

        /**
     *  Set the reference to a special function key for the
     *  currently visible OSK layer
     *
     *  @param    {number}  nLayer  Index of visible layer
     *  @param    {string}  keyId   key identifier
     *  @return   {Object}          Reference to key
     */
    getSpecialKey(nLayer: number, keyId: string): KeyElement {
      let layers = this.kbdDiv.childNodes[0].childNodes;

      if(nLayer >= 0 && nLayer < layers.length) {
        // Special function keys will always be in bottom row (must modify code if not)
        let rows = layers[nLayer].childNodes;
        let keys = rows[rows.length-1].childNodes;
        for(var k=0; k<keys.length; k++) {
          let key = getKeyFrom(keys[k].firstChild);
          if(key && key['keyId'] == keyId) {
            return key;
          }
        }
      }
      return null;
    }

    updateState() {
      let device = this.device;
      var n,nLayer=-1, b = this.kbdDiv.childNodes[0].childNodes;

      for(n=0; n < b.length; n++) {
        let layerElement = <HTMLDivElement> b[n];
        if(layerElement['layer'] == this.layerId) {
          layerElement.style.display='block';
          //b[n].style.visibility='visible';
          this.nextLayer=this.layerId;
          this.layerIndex=nLayer=n;
          if(typeof this.layers[n]['nextlayer'] == 'string') {
            this.nextLayer=this.layers[n]['nextlayer'];
          }

          // If osk._Show has been called, there's probably been a change in modifier or state key state.  Keep it updated!
          this._UpdateVKShiftStyle();
        } else {
          layerElement.style.display='none';
          //layerElement.style.visibility='hidden';
        }
      }

      if(device.touchable) {
        // Identify and save references to the language key, hide keyboard key, and space bar
        this.lgKey=this.getSpecialKey(nLayer,'K_LOPT');     //TODO: should be saved with layer
        this.hkKey=this.getSpecialKey(nLayer,'K_ROPT');
      }

      // Define for both desktop and touchable OSK
      this.spaceBar=this.getSpecialKey(nLayer,'K_SPACE'); //TODO: should be saved with layer
    }

    /**
     * Adjust the absolute height of each keyboard element after a rotation
     *
     **/
    adjustHeights(height: number) {
      let keyman = com.keyman.singleton;
      let device = this.device;

      if(!this.kbdDiv || !this.kbdDiv.firstChild || !this.kbdDiv.firstChild.firstChild.childNodes) {
        return false;
      }

      var fs=1.0;
      // TODO: Logically, this should be needed for Android, too - may need to be changed for the next version!
      if(device.OS == 'iOS' && !keyman.isEmbedded) {
        fs=fs/keyman.util.getViewportScale();
      }

      let paddedHeight = this.computedAdjustedOskHeight(height);

      let b = this.kbdDiv.firstChild as HTMLElement;
      let gs = this.kbdDiv.style;
      let bs=b.style;
      // Sets the layer group to the correct height.
      gs.height=gs.maxHeight=paddedHeight+'px';
      bs.fontSize=fs+'em';

      this.adjustLayerHeights(paddedHeight, height);

      return true;
    }

    /*private*/ computedAdjustedOskHeight(allottedHeight: number): number {
      let device = this.device;

      var layers=this.kbdDiv.firstChild.childNodes;
      let oskHeight = 0;

      // In case the keyboard's layers have differing row counts, we check them all for the maximum needed oskHeight.
      for(let i = 0; i < layers.length; i++) {
        let nRows = layers[i].childNodes.length;
        let rowHeight = Math.floor(allottedHeight/(nRows == 0 ? 1 : nRows));
        let layerHeight = nRows * rowHeight;

        if(layerHeight > oskHeight) {
          oskHeight = layerHeight;
        }
      }

      // This isn't set anywhere else; it's a legacy part of the original methods.
      const oskPad = 0;
      let oskPaddedHeight = oskHeight + oskPad;

      if(device.OS == 'Android' && 'devicePixelRatio' in window) {
        oskPaddedHeight /= window.devicePixelRatio;
      }

      return oskPaddedHeight;
    }

    private adjustLayerHeights(paddedHeight: number, trueHeight: number) {
      let device = this.device;
      let layers = this.kbdDiv.firstChild.childNodes;

      for(let nLayer=0;nLayer<layers.length; nLayer++) {
        // Check the heights of each row, in case different layers have different row counts.
        let layer = layers[nLayer] as HTMLElement;
        let nRows=layers[nLayer].childNodes.length;
        (<HTMLElement> layers[nLayer]).style.height=(paddedHeight)+'px';

        let rowHeight = Math.floor(trueHeight/(nRows == 0 ? 1 : nRows));

        if(device.OS == 'Android' && 'devicePixelRatio' in window) {
          layer.style.height = layer.style.maxHeight = paddedHeight + 'px';
          rowHeight /= window.devicePixelRatio;
        }

        // Sets the layers to the correct height
        let rowPad = Math.round(0.15*rowHeight);

        for(let nRow=0; nRow<nRows; nRow++) {
          let rs=(<HTMLElement> layers[nLayer].childNodes[nRow]).style;
          let bottom = (nRows-nRow-1)*rowHeight+1;
          if(!this.isStatic) {
            rs.bottom=bottom+'px';
          }
          rs.maxHeight=rs.lineHeight=rs.height=rowHeight+'px';

          // Calculate the exact vertical coordinate of the row's center.
          this.layout.layer[nLayer].row[nRow].proportionalY = ((paddedHeight - bottom) - rowHeight/2) / paddedHeight;

          let keys=layers[nLayer].childNodes[nRow].childNodes as NodeListOf<HTMLElement>;
          this.adjustRowHeights(keys, rowHeight, bottom, rowPad);
        }
      }
    }

    private adjustRowHeights(keys: NodeListOf<HTMLElement>, rowHeight: number, bottom: number, pad: number) {
      let util = com.keyman.singleton.util;
      let device = this.device;

      let resizeLabels = (device.OS == 'iOS' && device.formFactor == 'phone' && util.landscapeView());

      let nKeys=keys.length;
      for(let nKey=0;nKey<nKeys;nKey++) {
        let keySquare=keys[nKey] as HTMLElement;
        //key.style.marginTop = (device.formFactor == 'phone' ? pad : 4)+'px';
        //**no longer needed if base key label and popup icon are within btn, not container**

        // Must set the height of the btn DIV, not the label (if any)
        var j;
        for(j=0; j<keySquare.childNodes.length; j++) {
          if((keySquare.childNodes[j] as HTMLElement).classList.contains('kmw-key')) {
            break;
          }
        }

        // Set the kmw-key-square position
        let ks=keySquare.style;
        if(!this.isStatic) {
          ks.bottom=(bottom-pad/2)+'px';
        }
        ks.height=ks.minHeight=(rowHeight)+'px';

        // Set the kmw-key position
        let keyElement = keySquare.childNodes[j] as KeyElement;
        ks=keyElement.style;
        if(!this.isStatic) {
          ks.bottom=bottom+'px';
        }
        ks.height=ks.lineHeight=ks.minHeight=(rowHeight-pad)+'px';

        // Get the kmw-key-text element & style.
        for(j=0; j<keyElement.childNodes.length; j++) {
          if((keyElement.childNodes[j] as HTMLElement).classList.contains('kmw-key-text')) {
            break;
          }
        }

        let keyTextSpan = keyElement.childNodes[j] as HTMLElement;
        if(keyElement.key && keyTextSpan) { // space bar may not define the text span!
          keyTextSpan.style.fontSize = keyElement.key.getIdealFontSize(this, ks);
        }

        // Rescale keycap labels on iPhone (iOS 7)
        if(resizeLabels && (j > 0)) {
          (keySquare.childNodes[0] as HTMLElement).style.fontSize='6px';
        }
      }
    }

    // /**
    //  * Function     _VKeyGetTarget
    //  * Scope        Private
    //  * @param       {Object}    e     OSK event
    //  * @return      {Object}          Target element for key in OSK
    //  * Description  Identify the OSK key clicked
    //  */
    // _VKeyGetTarget(e: Event) {
    //   var Ltarg;
    //   e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
    //   Ltarg = util.eventTarget(e);
    //   if (Ltarg == null) {
    //     return null;
    //   }
    //   if (Ltarg.nodeType == 3) { // defeat Safari bug
    //     Ltarg = Ltarg.parentNode;
    //   }
    //   if (Ltarg.tagName == 'SPAN') {
    //     Ltarg = Ltarg.parentNode;
    //   }
    //   return Ltarg;
    // }

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
      if(activeStub == null) {
        return;
      }

      // First remove any existing keyboard style sheet
      if(this.styleSheet) {
        util.removeStyleSheet(this.styleSheet);
      }

      var i, kfd=activeStub['KFont'], ofd=activeStub['KOskFont'];

      // Add style sheets for embedded fonts if necessary (each font-face style will only be added once)
      util.addFontFaceStyleSheet(kfd);
      util.addFontFaceStyleSheet(ofd);

      // Temporarily hide duplicated elements on non-desktop browsers
      keymanweb.hideInputs();

      // Build the style string and append (or replace) the font style sheet
      // Note: Some browsers do not download the font-face font until it is applied,
      //       so must apply style before testing for font availability
      // Extended to allow keyboard-specific custom styles for Build 360
      var customStyle=this.addFontStyle(kfd,ofd);
      if( activeKeyboard != null && typeof(activeKeyboard.oskStyling) == 'string')  // KMEW-129
        customStyle=customStyle+activeKeyboard.oskStyling;

      this.styleSheet = util.addStyleSheet(customStyle); //Build 360

      // Wait until font is loaded then align duplicated input elements with page elements
      if(this.waitForFonts(kfd,ofd)) {
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
      var fn=keymanweb.baseFont;
      if(typeof(kfd) != 'undefined' && typeof(kfd['family']) != 'undefined') {
        fn=kfd['family'];
      }

      // Unquote font name in base font (if quoted)
      fn = fn.replace(/\u0022/g,'');

      // Set font family chain for mapped elements and remove any double quotes
      var rx=new RegExp('\\s?'+fn+',?'), ff=keymanweb.appliedFont.replace(/\u0022/g,'');

      // Remove base font name from chain if present
      ff = ff.replace(rx,'');
      ff = ff.replace(/,$/,'');

      // Then replace it at the head of the chain
      if(ff == '') {
        ff=fn;
      } else {
        ff=fn+','+ff;
      }

      // Re-insert quotes around individual font names
      ff = '"' + ff.replace(/\,\s?/g,'","') + '"';

      // Add to the stylesheet, quoted, and with !important to override any explicit style
      var s='.keymanweb-font{\nfont-family:' + ff + ' !important;\n}\n';

      // Set font family for OSK text
      if(typeof(ofd) != 'undefined') {
        s=s+'.kmw-key-text{\nfont-family:"'+ofd['family'].replace(/\u0022/g,'').replace(/,/g,'","')+'";\n}\n';
      } else if(typeof(kfd) != 'undefined') {
        s=s+'.kmw-key-text{\nfont-family:"'+kfd['family'].replace(/\u0022/g,'').replace(/,/g,'","')+'";\n}\n';
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
     *  @param  {Object}            height          Target height for the rendered keyboard 
     *                                              (currently required for legacy reasons)
     *  @return {Object}                            DIV object with filled keyboard layer content
     */
    static buildDocumentationKeyboard(PKbd: com.keyman.keyboards.Keyboard, argFormFactor,argLayerId, height: number): HTMLElement { // I777
      if(!PKbd) {
        return null;
      }

      var formFactor=(typeof(argFormFactor) == 'undefined' ? 'desktop' : argFormFactor),
          layerId=(typeof(argLayerId) == 'undefined' ? 'default' : argLayerId),
          device = new Device();

      // Device emulation for target documentation.
      device.formFactor = formFactor;
      if(formFactor != 'desktop') {
        device.OS = 'iOS';
      }

      let layout = PKbd.layout(formFactor);

      let kbdObj = new VisualKeyboard(PKbd, device, true);
      let kbd = kbdObj.kbdDiv.childNodes[0] as HTMLDivElement; // Gets the layer group.

      // Select the layer to display, and adjust sizes
      if(layout != null) {
        kbdObj.layerId = layerId;
        kbdObj.updateState();
        // This still feels fairly hacky... but something IS needed to constrain the height.
        // There are plans to address related concerns through some of the later aspects of 
        // the Web OSK-Core design.
        kbdObj.adjustHeights(height); // Necessary for the row heights to be properly set!
        // Relocates the font size definition from the main VisualKeyboard wrapper, since we don't return the whole thing.
        kbd.style.fontSize = kbdObj.kbdDiv.style.fontSize;
      } else {
        kbd.innerHTML="<p style='color:#c40; font-size:0.5em;margin:10px;'>No "+formFactor+" layout is defined for "+PKbd.name+".</p>";
      }
      // Add a faint border
      kbd.style.border='1px solid #ccc';
      return kbd;
    }

    onHide() {
      // Remove highlighting from hide keyboard key, if applied
      if(this.hkKey) {
        this.highlightKey(this.hkKey,false);
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
      pendingLongpress.promise.then(function(subkeyPopup) {
        // In-browser-specific handling.
        if(subkeyPopup) {
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
    initGestures(key: KeyElement, touch: Touch) {
      if(key['subKeys']) {
        let _this = this;

        let pendingLongpress = this.startLongpress(key);
        if(pendingLongpress == null) {
          return;
        }
        this.pendingSubkey = pendingLongpress;

        pendingLongpress.promise.then(function(subkeyPopup) {
          if(_this.pendingSubkey == pendingLongpress) {
            _this.pendingSubkey = null;
          }

          if(subkeyPopup) {
            // Clear key preview if any
            _this.showKeyTip(null,false);
  
            _this.subkeyGesture = subkeyPopup;
            subkeyPopup.promise.then(function(keyEvent: text.KeyEvent) {
              // Allow active cancellation, even if the source should allow passive.
              // It's an easy and cheap null guard.
              if(keyEvent) {
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
     * @param touch         The current touch-coordinate for the gesture
     * @returns true if should fully capture input, false if input should 'fall through'.
     */
    updateGestures(currentKey: KeyElement, previousKey: KeyElement, touch: Touch): boolean {
      let key0 = previousKey;
      let key1 = currentKey;

      // Clear previous key highlighting, allow subkey controller to highlight as appropriate.
      if(this.subkeyGesture) {
        if(key0) {
          key0.key.highlight(false);
        }
        this.subkeyGesture.updateTouch(touch);

        this.keyPending = null;
        this.touchPending = null;

        return true;
      }

      this.currentTarget = null;

      // If popup is visible, need to move over popup, not over main keyboard
      // Could be turned into a browser-longpress specific implementation within browser.PendingLongpress?
      if(key1 && key1['subKeys'] != null) {
        // Show popup keys immediately if touch moved up towards key array (KMEW-100, Build 353)
        if((this.touchY - touch.pageY > 5) && this.pendingSubkey && this.pendingSubkey instanceof browser.PendingLongpress) {
          this.pendingSubkey.resolve();
        }
      }

      // If there is an active popup menu (which can occur from the previous block),
      // a subkey popup exists; do not allow base key output.
      if(this.subkeyGesture) {
        return true;
      }

      if(key0 && key1 && (key1 != key0) && (key1.id != '')) {
        // While there may not be an active subkey menu, we should probably update which base key
        // is being highlighted by the current touch & start a pending longpress for it.
        this.clearPopup();
        this.initGestures(key1, touch);
      }
      return false;
    }

  optionKey(e: KeyElement, keyName: string, keyDown: boolean) {
    let keyman = com.keyman.singleton;
    let oskManager = keyman.osk;
    if(keyDown) {
      if(keyName.indexOf('K_LOPT') >= 0) {
        oskManager.showLanguageMenu();
      } else if(keyName.indexOf('K_ROPT') >= 0) {
        keyman.uiManager.setActivatingUI(false);
        oskManager._Hide(true);
        let active = keyman.domManager.getActiveElement();
        if(dom.Utils.instanceof(active, "TouchAliasElement")) {
          (active as dom.TouchAliasElement).hideCaret();
        }
        keyman.domManager.clearLastActiveElement();
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
      var tip=this.keytip;

      // Do not change the key preview unless key or state has changed
      if(tip == null || (key == tip.key && on == tip.state)) {
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

      if(this.device.formFactor == 'phone') {
        if(this.keytip == null) {
          // For now, should only be true (in production) when keyman.isEmbedded == true.
          let constrainPopup = keyman.isEmbedded;
          this.keytip = new browser.KeyTip(constrainPopup);
        }

        // Always append to _Box (since cleared during OSK Load)
        if(this.keytip && this.keytip.element) {
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
      var kReady=util.checkFontDescriptor(kfd), oReady=util.checkFontDescriptor(ofd);
      if(kReady && oReady) {
        return true;
      }

      keymanweb.fontCheckTimer=window.setInterval(function() {
        if(util.checkFontDescriptor(kfd) && util.checkFontDescriptor(ofd)) {
          window.clearInterval(keymanweb.fontCheckTimer);
          keymanweb.fontCheckTimer=null;
          keymanweb.alignInputs();
        }
      }, 100);

      // Align anyway as best as can if font appears to remain uninstalled after 5 seconds
      window.setTimeout(function() {
        if(keymanweb.fontCheckTimer)
        {
          window.clearInterval(keymanweb.fontCheckTimer);
          keymanweb.fontCheckTimer=null;
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
      if(this.styleSheet) {
        keyman.util.removeStyleSheet(this.styleSheet);
      }
    }
  }
}
