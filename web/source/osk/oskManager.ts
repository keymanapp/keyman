/// <reference path="../kmwexthtml.ts" />  // Includes KMW-added property declaration extensions for HTML elements.
// Includes the touch-mode language picker UI.
/// <reference path="languageMenu.ts" />
// Includes the banner
/// <reference path="./bannerManager.ts" />
// Defines desktop-centric OSK positioning + sizing behavior
/// <reference path="layouts/targetedFloatLayout.ts" />
// Generates the visual keyboard specific to each keyboard.  (class="kmw-osk-inner-frame")
/// <reference path="visualKeyboard.ts" />
// Models keyboards that present a help page, rather than a standard OSK.
/// <reference path="helpPageView.ts" />
/// <reference path="emptyView.ts" />

/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/

namespace com.keyman.osk {
  type OSKRect = {'left'?: number, 'top'?: number, 'width'?: number, 'height'?: number,
    'nosize'?: boolean, 'nomove'?: boolean};
  type OSKPos = {'left'?: number, 'top'?: number};

  export class OSKManager {
    // Important OSK elements (and container classes)
    _Box: HTMLDivElement;
    banner: BannerManager;
    vkbd: VisualKeyboard;

    desktopLayout: layouts.TargetedFloatLayout;

    // OSK state fields
    _Visible: boolean = false;
    _Enabled: boolean = true;
    vpScale: number = 1;

    // OSK positioning fields
    userPositioned: boolean = false;
    x: number;
    y: number;
    noDrag: boolean = false;
    dfltX: string;
    dfltY: string;

    // Fields used to store target OSK size when no keyboard has been loaded
    // If a keyboard has been loaded, the VisualKeyboard values override these.
    _baseWidth: number;
    _baseHeight: number;

    // Key code definition aliases for legacy keyboards  (They expect window['keyman']['osk'].___)
    modifierCodes = text.Codes.modifierCodes;
    modifierBitmasks = text.Codes.modifierBitmasks;
    stateBitmasks = text.Codes.stateBitmasks;
    keyCodes = text.Codes.keyCodes;

    public constructor() {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      // OSK initialization - create DIV and set default styles

      this._Box = util._CreateElement('div');   // Container for OSK (Help DIV, displayed when user clicks Help icon)
      document.body.appendChild(this._Box);

      // Install the default OSK stylesheet
      util.linkStyleSheet(keymanweb.getStyleSheetPath('kmwosk.css'));

      // For mouse click to prevent loss of focus
      util.attachDOMEvent(this._Box, 'mousedown', function(obj){
        keymanweb.uiManager.setActivatingUI(true);
        return false;
      });

      // And to prevent touch event default behaviour on mobile devices
      // TODO: are these needed, or do they interfere with other OSK event handling ????
      if(util.device.touchable) { // I3363 (Build 301)
        var cancelEventFunc = function(e) {
          if(e.cancelable) {
            e.preventDefault();
          }
          e.stopPropagation();
          return false;
        };

        util.attachDOMEvent(this._Box, 'touchstart', function(e) {
          keymanweb.uiManager.setActivatingUI(true);
          return cancelEventFunc(e);
        });

        util.attachDOMEvent(this._Box, 'touchend', cancelEventFunc);
        util.attachDOMEvent(this._Box, 'touchmove', cancelEventFunc);
        util.attachDOMEvent(this._Box, 'touchcancel', cancelEventFunc);

        // Can only get (initial) viewport scale factor after page is fully loaded!
        this.vpScale=util.getViewportScale();
      }

      this.loadCookie();
      this.banner = new BannerManager();
    }

    /**
     * Function     _Unload
     * Scope        Private
     * Description  Clears OSK variables prior to exit (JMD 1.9.1 - relocation of local variables 3/9/10)
     */
    _Unload() {
      this.vkbd = null;
      this.banner = null;
      this._Box = null;
    }

    /**
     * Function     _Load
     * Scope        Private
     * Description  OSK initialization when keyboard selected
     */
    _Load() { // Load Help
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;
      let device = util.device;

      var activeKeyboard = keymanweb.core.activeKeyboard;

      this._Visible = false;  // I3363 (Build 301)
      var s = this._Box.style;
      s.zIndex='9999'; s.display='none'; s.width= device.touchable ? '100%' : 'auto';
      s.position = (device.formFactor == 'desktop' ? 'absolute' : 'fixed');

      // Use smaller base font size for mobile devices
      //if(screen.availHeight < 500) s.fontSize='10pt';
      //else if(screen.availHeight < 800) s.fontSize='11pt';
      //else s.fontSize='12pt';

      // Set scaling for mobile devices here.
      if(device.touchable) {
        var fontScale: number = 1;
        if(device.formFactor == 'phone') {
          fontScale = 1.6 * (keymanweb.isEmbedded ? 0.65 : 0.6) * 1.2;  // Combines original scaling factor with one previously applied to the layer group.
        } else {
          // The following is a *temporary* fix for small format tablets, e.g. PendoPad
          var pixelRatio = 1;
          if(device.OS == 'Android' && 'devicePixelRatio' in window) {
            pixelRatio = window.devicePixelRatio;
          }

          if(device.OS == 'Android' && device.formFactor == 'tablet' && this.getHeight() < 300 * pixelRatio) {
            fontScale *= 1.2;
          } else {
            fontScale *= 2; //'2.5em';
          }
        }

        // Finalize the font size parameter.
        s.fontSize = fontScale + 'em';
      }

      if(this.vkbd) {
        this.vkbd.shutdown();
      }
      this.vkbd = null;

      // Instantly resets the OSK container, erasing / delinking the previously-loaded keyboard.
      this._Box.innerHTML = '';

      this._Box.onmouseover = this._VKbdMouseOver;
      this._Box.onmouseout = this._VKbdMouseOut;

      // START:  construction of the actual internal layout for the overall OSK
      let layout: layouts.TargetedFloatLayout = null;

      // Add header element to OSK only for desktop browsers
      if(util.device.formFactor == 'desktop') {
        layout = this.desktopLayout = new layouts.TargetedFloatLayout();
        layout.attachToView(this);
        this.desktopLayout.titleBar.setTitleFromKeyboard(activeKeyboard);
        this._Box.appendChild(layout.titleBar.element);
      }

      // Add suggestion banner bar to OSK
      if (this.banner) {
        this._Box.appendChild(this.banner.element);
      }

      let kbdView: KeyboardView = this._GenerateKeyboardView(activeKeyboard);
      this._Box.appendChild(kbdView.element);
      if(kbdView instanceof VisualKeyboard) {
        this.vkbd = kbdView;
      }
      kbdView.postInsert();

      // Add footer element to OSK only for desktop browsers
      if(this.desktopLayout) {
        if(kbdView instanceof VisualKeyboard) {
          this._Box.appendChild(layout.resizeBar.element);
        }
        // For other devices, adjust the object heights, allowing for viewport scaling
      } else {
        this.vkbd.refreshLayout(this.getKeyboardHeight());

        let b: HTMLElement = this._Box, bs=b.style;
        bs.height=bs.maxHeight=this.vkbd.computedAdjustedOskHeight(this.getHeight())+'px';
      }

      // END:  construction of the actual internal layout for the overall OSK

      // Correct the classname for the (inner) OSK frame (Build 360)
      var kbdID: string = (activeKeyboard ? activeKeyboard.id.replace('Keyboard_','') : '');
      if(keymanweb.isEmbedded && kbdID.indexOf('::') != -1) {
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

      if(this._Enabled) {
        this._Show();
      }
    }

    private get width(): number {
      if(this.vkbd) {
        return this.vkbd.width;
      } else {
        return this._baseWidth;
      }
    }

    private get height(): number {
      if(this.vkbd) {
        return this.vkbd.height;
      } else {
        return this._baseHeight;
      }
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

    /**
     * Display build number
     */
    showBuild() {
      let keymanweb = com.keyman.singleton;
      keymanweb.util.internalAlert('KeymanWeb Version '+keymanweb['version']+'.'+keymanweb['build']+'<br /><br />'
          +'<span style="font-size:0.8em">Copyright &copy; 2017 SIL International</span>');
    }

    /**
     * Function     restorePosition
     * Scope        Public
     * @param       {boolean?}      keepDefaultPosition  If true, does not reset the default x,y set by `setRect`.
     *                                                   If false or omitted, resets the default x,y as well.
     * Description  Move OSK back to default position, floating under active input element
     */
    ['restorePosition']: (keepDefaultPosition?: boolean) => void = function(this: OSKManager, keepDefaultPosition?: boolean) {
      let isVisible = this._Visible;
      if(isVisible) {
        com.keyman.singleton.domManager.focusLastActiveElement();  // I2036 - OSK does not unpin to correct location
      }

      this.loadCookie();
      this.userPositioned=false;
      if(!keepDefaultPosition) {
        delete this.dfltX;
        delete this.dfltY;
      }
      this.saveCookie();

      if(isVisible) {
        this._Show();
      }

      this.doResizeMove(); //allow the UI to respond to OSK movements
      if(this.desktopLayout) {
        this.desktopLayout.titleBar.showPin(false);
      }
    }.bind(this);

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
     * Function     _VKbdMouseOver
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Activate the KMW UI on mouse over
     */
    private _VKbdMouseOver = function(this: OSKManager, e) {
      com.keyman.singleton.uiManager.setActivatingUI(true);
    }.bind(this);

    /**
     * Function     _VKbdMouseOut
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Cancel activation of KMW UI on mouse out
     */
    private _VKbdMouseOut = function(this: OSKManager, e) {
      com.keyman.singleton.uiManager.setActivatingUI(false);
    }.bind(this);

    /**
     * Save size, position, font size and visibility of OSK
     */
    saveCookie() {
      let util = com.keyman.singleton.util;

      var c = util.loadCookie('KeymanWeb_OnScreenKeyboard');
      var p = this.getPos();

      c['visible'] = this._Enabled ? 1 : 0;
      c['userSet'] = this.userPositioned ? 1 : 0;
      c['left'] = p.left;
      c['top'] = p.top;

      if(this.vkbd) {
        c['width'] = this.width;
        c['height'] = this.height;
      }

      util.saveCookie('KeymanWeb_OnScreenKeyboard',c);
    }

    /**
     * Restore size, position, font size and visibility of desktop OSK
     *
     *  @return {boolean}
     */
    loadCookie(): void {
      let util = com.keyman.singleton.util;

      var c = util.loadCookie('KeymanWeb_OnScreenKeyboard');

      this._Enabled = util.toNumber(c['visible'], 1) == 1;
      this.userPositioned = util.toNumber(c['userSet'], 0) == 1;
      this.x = util.toNumber(c['left'],-1);
      this.y = util.toNumber(c['top'],-1);

      // Restore OSK size - font size now fixed in relation to OSK height, unless overridden (in em) by keyboard
      var dfltWidth=0.3*screen.width;
      //if(util.toNumber(c['width'],0) == 0) dfltWidth=0.5*screen.width;
      var newWidth=util.toNumber(c['width'],dfltWidth),
          newHeight=util.toNumber(c['height'],0.15*screen.height);

      // Limit the OSK dimensions to reasonable values
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
        newHeight=0.5*screen.height;
      }

      this.setSize(newWidth, newHeight);

      // and OSK position if user located
      if(this.x == -1 || this.y == -1 || (!this._Box)) {
        this.userPositioned = false;
      }

      if(this.x < window.pageXOffset-0.8*newWidth) {
        this.x=window.pageXOffset-0.8*newWidth;
      }
      if(this.y < 0) {
        this.x=-1;
        this.y=-1;
        this.userPositioned=false;
      }

      if(this.userPositioned && this._Box) {
        this.setPos({'left': this.x, 'top': this.y});
      }
    }

    /*private*/ setSize(width?: number, height?: number, pending?: boolean) {
      if(width && height) {
        this._baseWidth = width;
        this._baseHeight = height;
      }

      if(this.vkbd) {
        this.vkbd.setSize(width, height, pending);
      }
    }

    /**
     * Get the wanted height of the banner (does not include the keyboard)
     *  @return   {number}    height in pixels
     */
    getBannerHeight(): number {
      return (this.banner != null) ? this.banner.height : 0;
    }

    /**
     * Get the wanted height of the OSK for touch devices (does not include banner height)
     *  @return   {number}    height in pixels
     **/
    getKeyboardHeight(): number {
      let keymanweb = com.keyman.singleton;
      let device = keymanweb.util.device;

      // KeymanTouch - get OSK height from device
      if(typeof(keymanweb['getOskHeight']) == 'function') {
        return keymanweb['getOskHeight']();
      }

      var oskHeightLandscapeView=Math.floor(Math.min(screen.availHeight,screen.availWidth)/2),
          height=oskHeightLandscapeView;

      if(device.formFactor == 'phone') {
        var sx=Math.min(screen.height,screen.width),
            sy=Math.max(screen.height,screen.width);

        if(keymanweb.util.portraitView())
          height=Math.floor(Math.max(screen.availHeight,screen.availWidth)/3);
        else
          height=height*(sy/sx)/1.6;  //adjust for aspect ratio, increase slightly for iPhone 5
      }

      // Correct for viewport scaling (iOS - Android 4.2 does not want this, at least on Galaxy Tab 3))
      if(device.OS == 'iOS') {
        height=height/keymanweb.util.getViewportScale();
      }

      // Correct for devicePixelratio - needed on Android 4.1.2 phones,
      // for Opera, Chrome and Firefox, but not for native browser!   Exclude native browser for Build 344.
      if(device.OS == 'Android' && device.formFactor == 'phone' && 'devicePixelRatio' in window) {
        var bMatch=/Firefox|Chrome|OPR/;
        if(bMatch.test(navigator.userAgent)) {
          height = height*window.devicePixelRatio;
        }
      }

      return height;
    }

    /**
     * Get the wanted height of the OSK for touch devices (banner height + rows of keys)
     *  @return   {number}    height in pixels
     **/
    getHeight(): number {
      return this.getBannerHeight() + this.getKeyboardHeight();
    }

    /**
     * Get the wanted width of the OSK for touch devices
     *
     *  @return   {number}    height in pixels
     **/
    getWidth(): number {
      let keymanweb = com.keyman.singleton;
      let device = keymanweb.util.device;

      // KeymanTouch - get OSK height from device
      if(typeof(keymanweb['getOskWidth']) == 'function') {
        return keymanweb['getOskWidth']();
      }

      var width: number;
      if(device.OS == 'iOS') {
        // iOS does not interchange these values when the orientation changes!
        //width = util.portraitView() ? screen.width : screen.height;
        width = window.innerWidth;
      } else if(device.OS == 'Android') {
        try {
          width=document.documentElement.clientWidth;
        } catch(ex) {
          width=screen.availWidth;
        }
      } else {
        width=screen.width;
      }

      return width;
    }

    /**
     * Allow UI to update OSK position and properties
     *
     * @param       {Object=}     p       object with coordinates and userdefined flag
     *
     */
    doResizeMove(p?) {
      return com.keyman.singleton.util.callEvent('osk.resizemove',p);
    }

    /**
     * Function     getRect //TODO:  This is probably not correct, anyway!!!!!
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

      if(this.vkbd) {
        p['width']  = p.width  = this.vkbd.kbdDiv.offsetWidth;
        p['height'] = p.height = this.vkbd.kbdDiv.offsetHeight;
      } else {
        p['width']  = p.width  = dom.Utils.getAbsoluteX(this._Box) + this._Box.offsetWidth;
        p['height'] = p.height = dom.Utils.getAbsoluteY(this._Box) + this._Box.offsetHeight;
      }
      return p;
    }

    /**
     * Allow the UI or page to set the position and size of the OSK
     * and (optionally) override user repositioning or sizing
     *
     * @param       {Object.<string,number>}   p  Array object with position and size of OSK container
    **/
    ['setRect'](p: OSKRect) {
      let util = com.keyman.singleton.util;
      if(this._Box == null || util.device.formFactor != 'desktop') {
        return;
      }

      var b = this._Box, bs = b.style;
      if('left' in p) {
        this.x = p['left'] - dom.Utils.getAbsoluteX(b) + b.offsetLeft;
        bs.left= this.x + 'px';
        this.dfltX=bs.left;
      }

      if('top' in p) {
        this.y = p['top'] - dom.Utils.getAbsoluteY(b) + b.offsetTop;
        bs.top = this.y + 'px';
        this.dfltY=bs.top;
      }

      //Do not allow user resizing for non-standard keyboards (e.g. EuroLatin)
      if(this.vkbd != null) {
        var d=this.vkbd.kbdDiv, ds=d.style;

        // Set width, but limit to reasonable value
        if('width' in p) {
          var w=(p['width']-(b.offsetWidth-d.offsetWidth));
          if(w < 0.2*screen.width) {
            w=0.2*screen.width;
          }
          if(w > 0.9*screen.width) {
            w=0.9*screen.width;
          }
          ds.width=w+'px';
          this.setSize(w, this.height, true);
        }

        // Set height, but limit to reasonable value
        // This sets the default font size for the OSK in px, but that
        // can be modified at the key text level by setting
        // the font size in em in the kmw-key-text class
        if('height' in p) {
          var h=(p['height']-(b.offsetHeight-d.offsetHeight));
          if(h < 0.1*screen.height) {
            h=0.1*screen.height;
          }
          if(h > 0.5*screen.height) {
            h=0.5*screen.height;
          }
          ds.height=h+'px'; ds.fontSize=(h/8)+'px';
          this.setSize(this.width, h, true);
        }

        // Fix or release user resizing
        if('nosize' in p) {
          if(this.desktopLayout) {
            this.desktopLayout.resizingEnabled = !p['nosize'];
          }
        }

      }
      // Fix or release user dragging
      if('nomove' in p) {
        this.noDrag=p['nomove'];
        if(this.desktopLayout) {
          this.desktopLayout.movementEnabled = !this.noDrag;
        }
      }
      // Save the user-defined OSK size
      this.saveCookie();
    }

    /**
     * Get position of OSK window
     *
     * @return      {Object.<string,number>}     Array object with OSK window position
    **/
    getPos(): OSKPos {
      var Lkbd=this._Box, p={
        left: this._Visible ? Lkbd.offsetLeft : this.x,
        top: this._Visible ? Lkbd.offsetTop : this.y
      };

      return p;
    }

    /**
     * Function     setPos
     * Scope        Private
     * @param       {Object.<string,number>}    p     Array object with OSK left, top
     * Description  Set position of OSK window, but limit to screen, and ignore if  a touch input device
     */
    ['setPos'](p: OSKPos) {
      if(typeof(this._Box) == 'undefined' || com.keyman.singleton.util.device.touchable) {
        return; // I3363 (Build 301)
      }

      if(this.userPositioned) {
        var Px=p['left'], Py=p['top'];

        if(typeof(Px) != 'undefined') {
          if(Px < -0.8*this._Box.offsetWidth) {
            Px = -0.8*this._Box.offsetWidth;
          }
          if(this.userPositioned) {
            this._Box.style.left=Px+'px';
            this.x = Px;
          }
        }
        // May not be needed - vertical positioning is handled differently and defaults to input field if off screen
        if(typeof(Py) != 'undefined') {
          if(Py < 0) {
            Py = 0;
          }

          if(this.userPositioned) {
            this._Box.style.top=Py+'px';
            this.y = Py;
          }
        }
      }

      if(this.desktopLayout) {
        this.desktopLayout.titleBar.showPin(this.userPositioned);
      }
    }

    /**
     * Display KMW OSK at specified position (returns nothing)
     *
     * @param       {number=}     Px      x-coordinate for OSK rectangle
     * @param       {number=}     Py      y-coordinate for OSK rectangle
     */
    _Show(Px?: number, Py?: number) {
      let keymanweb = com.keyman.singleton;
      let device = keymanweb.util.device;

      // Do not try to display OSK if undefined, or no active element
      if(this._Box == null || keymanweb.domManager.getActiveElement() == null) {
        return;
      }

      // Never display the OSK for desktop browsers unless KMW element is focused, and a keyboard selected
      if((!device.touchable) && (keymanweb.core.activeKeyboard == null || !this._Enabled)) {
        return;
      }

      var Ls = this._Box.style;

      // Do not display OSK until it has been positioned correctly
      if(device.touchable && Ls.bottom == '') {
        Ls.visibility='hidden';
      }

      if(device.touchable) {
        /* In case it's still '0' from a hide() operation.
         * Happens when _Show is called before the transitionend events are processed,
         * which can happen in bulk-rendering contexts.
         *
         * (Opacity is only modified when device.touchable = true, though a couple of extra
         * conditions may apply.)
         */
        Ls.opacity='1';
      }

      // TODO:  Move this into the VisualKeyboard class!
      // The following code will always be executed except for externally created OSK such as EuroLatin
      if(this.vkbd) {
        // Always adjust screen height if iPhone or iPod, to take account of viewport changes
        // Do NOT condition upon form-factor; this line prevents a bug with displaying
        // the predictive-text banner on the initial keyboard load.  (Issue #2907)
        if(device.touchable && device.OS == 'iOS') {
          this.vkbd.refreshLayout(this.getKeyboardHeight());

          var b: HTMLElement = this._Box, bs=b.style;
          bs.height=bs.maxHeight=this.vkbd.computedAdjustedOskHeight(this.getHeight())+'px';
        }
        // Enable the currently active keyboard layer and update the default nextLayer member
        this.vkbd.updateState();

        // Extra style changes and overrides for touch-mode.
        if(device.touchable) {
          let ks = this.vkbd.kbdDiv.style;
          ks.position = Ls.position='fixed';
          ks.bottom = Ls.left=Ls.bottom='0px';
          let vkbdHeight = (<HTMLElement> this.vkbd.kbdDiv).style.height;
          vkbdHeight = vkbdHeight.substr(0, vkbdHeight.indexOf('px'));
          Ls.height=Ls.maxHeight= (this.getBannerHeight() + parseInt(vkbdHeight, 10) + 5 /* kmw-banner-bar top in css */) + 'px';
          Ls.border='none';
          Ls.borderTop='1px solid gray';

          this._Enabled=true;
          this._Visible=true; // I3363 (Build 301)
        }
      }

      //TODO: may need to return here for touch devices??
      Ls.display='block'; //Ls.visibility='visible';

      if(this.vkbd) {
        this.vkbd.showLanguage();
      }

      if(device.formFactor == 'desktop') {
        Ls.position='absolute'; Ls.display='block'; //Ls.visibility='visible';
        Ls.left='0px';
        this.loadCookie();
        if(Px >= 0) { //probably never happens, legacy support only
          Ls.left = Px + 'px'; Ls.top = Py + 'px';
        } else {
          if(this.userPositioned) {
            Ls.left=this.x+'px';
            Ls.top=this.y+'px';
          } else {
            var el=keymanweb.domManager.getActiveElement();

            // Special case - design mode iframes.  Don't use the active element (inside the design-mode doc);
            // use its containing iframe from the doc itself.
            let ownerDoc = el.ownerDocument;
            if(ownerDoc.designMode == 'on' && ownerDoc.defaultView && ownerDoc.defaultView.frameElement) {
              el = ownerDoc.defaultView.frameElement as HTMLElement;
            }
            if(this.dfltX) {
              Ls.left=this.dfltX;
            } else if(typeof el != 'undefined' && el != null) {
              Ls.left=dom.Utils.getAbsoluteX(el) + 'px';
            }

            if(this.dfltY) {
              Ls.top=this.dfltY;
            } else if(typeof el != 'undefined' && el != null) {
              Ls.top=(dom.Utils.getAbsoluteY(el) + el.offsetHeight)+'px';
            }
          }
        }
        this._Enabled=true;
        this._Visible=true;
        if(this.vkbd) {
          this.vkbd.refit();
        }

        this.saveCookie();

        if(this.desktopLayout) {
          this.desktopLayout.titleBar.showPin(this.userPositioned);
        }
      }

      // If OSK still hidden, make visible only after all calculation finished
      if(Ls.visibility == 'hidden') {
        window.setTimeout(function(){
          this._Box.style.visibility='visible';
        }.bind(this), 0);
      }

      // Allow desktop UI to execute code when showing the OSK
      if(!device.touchable) {
        var Lpos={};
        Lpos['x']=this._Box.offsetLeft;
        Lpos['y']=this._Box.offsetTop;
        Lpos['userLocated']=this.userPositioned;
        this.doShow(Lpos);
      }
    }

    /**
     * Hide Keymanweb On Screen Keyboard
     *
     * @param       {boolean}   hiddenByUser    Distinguish between hiding on loss of focus and explicit hiding by user
     */
    _Hide(hiddenByUser: boolean) {
      let keymanweb = com.keyman.singleton;
      let device = keymanweb.util.device;
      // The test for CJK languages is necessary to prevent a picklist (displayed in the OSK) from being hidden by the user
      // Once picklist functionality is separated out, this will no longer be needed.
      // Logic is: execute always if hidden on lost focus, but if requested by user, only if not CJK

      if(keymanweb.isEmbedded) {
        // We never hide the keyboard in embedded mode
        return;
      }

      // Save current size if visible
      if(this._Box && this._Box.style.display == 'block' && this.vkbd) {
        this.vkbd.refit();
      }

      if(hiddenByUser) {
        //osk.loadCookie(); // preserve current offset and userlocated state
        this._Enabled = ((keymanweb.isCJK() || device.touchable)? true : false); // I3363 (Build 301)
        this.saveCookie();  // Save current OSK state, size and position (desktop only)
      } else if(device.formFactor == 'desktop') {
        //Allow desktop OSK to remain visible on blur if body class set
        if(document.body.className.indexOf('osk-always-visible') >= 0) {
          return;
        }
      }

      this._Visible = false;
      if(this._Box && device.touchable && this._Box.offsetHeight > 0) { // I3363 (Build 301)
        var os=this._Box.style;
        //Firefox doesn't transition opacity if start delay is explicitly set to 0!
        if(typeof(os.MozBoxSizing) == 'string') {
          os.transition='opacity 0.8s linear';
        } else {
          os.transition=os.msTransition=os.WebkitTransition='opacity 0.5s linear 0';
        }

        // Cannot hide the OSK smoothly using a transitioned drop, since for
        // position:fixed elements transitioning is incompatible with translate3d(),
        // and also does not work with top, bottom or height styles.
        // Opacity can be transitioned and is probably the simplest alternative.
        // We must condition on osk._Visible in case focus has since been moved to another
        // input (in which case osk._Visible will be non-zero)
        window.setTimeout(function(this: OSKManager) {
          var os=this._Box.style;
          if(this._Visible) {
            // Leave opacity alone and clear transition if another element activated
            os.transition=os.msTransition=os.MozTransition=os.WebkitTransition='';
          } else {
            // Set opacity to zero, should decrease smoothly
            os.opacity='0';

            // Actually hide the OSK at the end of the transition
            this._Box.addEventListener('transitionend', this.hideNow, false);
            this._Box.addEventListener('webkitTransitionEnd', this.hideNow, false);
          }
        }.bind(this), 200);      // Wait a bit before starting, to allow for moving to another element
      } else {
        if(this._Box) {
          this._Box.style.display = 'none';
        }
      }

      // Allow UI to execute code when hiding the OSK
      var p={};
      p['HiddenByUser']=hiddenByUser;
      this.doHide(p);

      // If hidden by the UI, be sure to restore the focus
      if(hiddenByUser) {
        keymanweb.domManager.focusLastActiveElement();
      }
    }

        /**
     * Function     hideNow
     * Scope        Private
     * Description  Hide the OSK unconditionally and immediately, cancel any pending transition
     */
    hideNow: () => void = function(this: OSKManager) { // I3363 (Build 301)
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
     * Display list of installed keyboards in pop-up menu
     **/
    showLanguageMenu() {
      let menu = new LanguageMenu(com.keyman.singleton);
      menu.show();
    }

    /**
     * Function     userPositioned
     * Scope        Public
     * @return      {(boolean|number)}          true if user located
     * Description  Test if OSK window has been repositioned by user
     */
    ['userLocated']() {
      return this.userPositioned;
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

    ['shutdown']() {
      // Remove the OSK's elements from the document, allowing them to be properly cleaned up.
      // Necessary for clean engine testing.
      var _box = this._Box;
      if(_box.parentElement) {
        _box.parentElement.removeChild(_box);
      }
    }
  }
}
