// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="../kmwexthtml.ts" />
// Includes the touch-mode language picker UI.
/// <reference path="languageMenu.ts" />
/// <reference path="lengthStyle.ts" />
// Defines desktop-centric OSK positioning + sizing behavior
/// <reference path="layouts/targetedFloatLayout.ts" />
/// <reference path="oskView.ts" />

/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/

namespace com.keyman.osk {
  type OSKRect = {'left'?: number, 'top'?: number, 'width'?: number, 'height'?: number,
    'nosize'?: boolean, 'nomove'?: boolean};
  type OSKPos = {'left'?: number, 'top'?: number};

  export class FloatingOSKView extends OSKView {
    readonly desktopLayout: layouts.TargetedFloatLayout;

    // OSK positioning fields
    userPositioned: boolean = false;
    specifiedPosition: boolean = false;
    x: number;
    y: number;
    noDrag: boolean = false;
    dfltX: string;
    dfltY: string;

    // Key code definition aliases for legacy keyboards  (They expect window['keyman']['osk'].___)
    modifierCodes = text.Codes.modifierCodes;
    modifierBitmasks = text.Codes.modifierBitmasks;
    stateBitmasks = text.Codes.stateBitmasks;
    keyCodes = text.Codes.keyCodes;

    public constructor(modeledDevice: utils.DeviceSpec) {
      super(modeledDevice);

      document.body.appendChild(this._Box);

      this.loadCookie();

      // Add header element to OSK only for desktop browsers
      const layout = this.desktopLayout = new layouts.TargetedFloatLayout();
      this.headerView = layout.titleBar;
      layout.titleBar.attachHandlers(this);
    }

    /**
     * Function     _Unload
     * Scope        Private
     * Description  Clears OSK variables prior to exit (JMD 1.9.1 - relocation of local variables 3/9/10)
     */
    _Unload() {
      this.keyboardView = null;
      this.bannerView = null;
      this._Box = null;
    }

    protected setBoxStyling() {
      const s = this._Box.style;

      s.zIndex   = '9999';
      s.display  = 'none';
      s.width    = 'auto';
      s.position = 'absolute';
    }

    protected postKeyboardLoad() {
      this._Visible = false;  // I3363 (Build 301)

      this._Box.onmouseover = this._VKbdMouseOver;
      this._Box.onmouseout = this._VKbdMouseOut;

      // Add header element to OSK only for desktop browsers
      const layout = this.desktopLayout;
      layout.attachToView(this);
      if(this.activeKeyboard) {
        this.desktopLayout.titleBar.setTitleFromKeyboard(this.activeKeyboard);
      }

      if(this.vkbd) {
        this.footerView = layout.resizeBar;
        this._Box.appendChild(this.footerView.element);
      } else {
        if(this.footerView) {
          this._Box.removeChild(this.footerView.element);
        }
        this.footerView = null;
      }

      this.loadCookie();
      this.setNeedsLayout();

      if(this.displayIfActive) {
        this.present();
      }
    }

    /**
     * Function     restorePosition
     * Scope        Public
     * @param       {boolean?}      keepDefaultPosition  If true, does not reset the default x,y set by `setRect`.
     *                                                   If false or omitted, resets the default x,y as well.
     * Description  Move OSK back to default position, floating under active input element
     */
    ['restorePosition']: (keepDefaultPosition?: boolean) => void = function(this: FloatingOSKView, keepDefaultPosition?: boolean) {
      let isVisible = this._Visible;
      if(isVisible && this.activeTarget instanceof dom.targets.OutputTarget) {
        this.activeTarget?.focus();  // I2036 - OSK does not unpin to correct location
      }

      this.loadCookie();
      this.userPositioned=false;
      if(!keepDefaultPosition) {
        delete this.dfltX;
        delete this.dfltY;
      }
      this.saveCookie();

      if(isVisible) {
        this.present();
      }

      this.doResizeMove(); //allow the UI to respond to OSK movements
      this.desktopLayout.titleBar.showPin(false);
    }.bind(this);

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
     * Function     _VKbdMouseOver
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Activate the KMW UI on mouse over
     */
    private _VKbdMouseOver = function(this: AnchoredOSKView, e) {
      com.keyman.singleton.uiManager.setActivatingUI(true);
    }.bind(this);

    /**
     * Function     _VKbdMouseOut
     * Scope        Private
     * @param       {Object}      e      event
     * Description  Cancel activation of KMW UI on mouse out
     */
    private _VKbdMouseOut = function(this: AnchoredOSKView, e) {
      com.keyman.singleton.uiManager.setActivatingUI(false);
    }.bind(this);

    /**
     * Save size, position, font size and visibility of OSK
     */
    saveCookie() {
      let util = com.keyman.singleton.util;

      var c = util.loadCookie('KeymanWeb_OnScreenKeyboard');
      var p = this.getPos();

      c['visible'] = this.displayIfActive ? 1 : 0;
      c['userSet'] = this.userPositioned ? 1 : 0;
      c['left'] = p.left;
      c['top'] = p.top;
      c['_version'] = utils.Version.CURRENT.toString();

      if(this.vkbd) {
        c['width'] = this.width.val;
        c['height'] = this.height.val;
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

      this.displayIfActive = util.toNumber(c['visible'], 1) == 1;
      this.userPositioned = util.toNumber(c['userSet'], 0) == 1;
      this.x = util.toNumber(c['left'],-1);
      this.y = util.toNumber(c['top'],-1);
      let cookieVersionString = c['_version'];

      // Restore OSK size - font size now fixed in relation to OSK height, unless overridden (in em) by keyboard
      let dfltWidth=0.3*screen.width;
      let dfltHeight=0.15*screen.height;
      //if(util.toNumber(c['width'],0) == 0) dfltWidth=0.5*screen.width;
      let newWidth  = parseInt(c['width'], 10);
      let newHeight = parseInt(c['height'], 10);
      let isNewCookie = isNaN(newHeight);
      newWidth  = isNaN(newWidth)  ? dfltWidth  : newWidth;
      newHeight = isNaN(newHeight) ? dfltHeight : newHeight;

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

      // if(!cookieVersionString) - this component was not tracked until 15.0.
      // Before that point, the OSK's title bar and resize bar heights were not included
      // in the OSK's cookie-persisted height.
      if(isNewCookie || !cookieVersionString) {
        // Adds some space to account for the OSK's header and footer, should they exist.
        if(this.headerView && this.headerView.layoutHeight.absolute) {
          newHeight += this.headerView.layoutHeight.val;
        }

        if(this.footerView && this.footerView.layoutHeight.absolute) {
          newHeight += this.footerView.layoutHeight.val;
        }
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

    /**
     * Get the wanted height of the OSK for touch devices (does not include banner height)
     *  @return   {number}    height in pixels
     **/
    getDefaultKeyboardHeight(): number {
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

      return height;
    }

    /**
     * Get the wanted width of the OSK for touch devices
     *
     *  @return   {number}    height in pixels
     **/
    getDefaultWidth(): number {
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
          // Use of the `computed` variant is here temporary.
          // Shouldn't use `setSize` for this in the long-term.
          this.setSize(w, this.computedHeight, true);
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
          // Use of the `computed` variant is here temporary.
          // Shouldn't use `setSize` for this in the long-term.
          this.setSize(this.computedWidth, h, true);
        }

        // Fix or release user resizing
        if('nosize' in p) {
          this.desktopLayout.resizingEnabled = !p['nosize'];
        }

      }
      // Fix or release user dragging
      if('nomove' in p) {
        this.noDrag=p['nomove'];
        this.desktopLayout.movementEnabled = !this.noDrag;
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
      if(typeof(this._Box) == 'undefined') {
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

    public setDisplayPositioning() {
      var Ls = this._Box.style;

      Ls.position='absolute'; Ls.display='block'; //Ls.visibility='visible';
      Ls.left='0px';
      if(this.specifiedPosition || this.userPositioned) {
        Ls.left = this.x+'px';
        Ls.top  = this.y+'px';
      } else {
        let el: HTMLElement = null;
        if(this.activeTarget instanceof dom.targets.OutputTarget) {
          el = this.activeTarget?.getElement();
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

      // Unset the flag, keeping 'specified position' specific to single
      // presentAtPosition calls.
      this.specifiedPosition = false;
    }

    /**
     * Display KMW OSK at specified position (returns nothing)
     *
     * @param       {number=}     Px      x-coordinate for OSK rectangle
     * @param       {number=}     Py      y-coordinate for OSK rectangle
     */
    presentAtPosition(Px?: number, Py?: number) {
      if(!this.mayShow()) {
        return;
      }

      this.specifiedPosition = Px >= 0 || Py >= 0; //probably never happens, legacy support only
      if(this.specifiedPosition) {
        this.x = Px;
        this.y = Py;
      }

      // Combines the two paths with set positioning.
      this.specifiedPosition = this.specifiedPosition || this.userPositioned;

      this.present();
    }

    present() {
      if(!this.mayShow()) {
        return;
      }

      this.desktopLayout.titleBar.showPin(this.userPositioned);

      super.present();

      // Allow desktop UI to execute code when showing the OSK
      var Lpos={};
      Lpos['x']=this._Box.offsetLeft;
      Lpos['y']=this._Box.offsetTop;
      Lpos['userLocated']=this.userPositioned;
      this.doShow(Lpos);
    }

    public startHide(hiddenByUser: boolean) {
      super.startHide(hiddenByUser);

      if(hiddenByUser) {
        this.saveCookie();  // Save current OSK state, size and position (desktop only)
      }
    }

    ['show'](bShow: boolean) {
      super['show'](bShow);
      this.saveCookie();
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
  }
}
