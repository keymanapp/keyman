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

    // Key code definition aliases for legacy keyboards  (They expect window['keyman']['osk'].___)
    modifierCodes = text.Codes.modifierCodes;
    modifierBitmasks = text.Codes.modifierBitmasks;
    stateBitmasks = text.Codes.stateBitmasks;
    keyCodes = text.Codes.keyCodes;

    public constructor() {
      super(com.keyman.singleton.util.device.coreSpec);

      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      document.body.appendChild(this._Box);

      if(util.device.touchable) {
        // Can only get (initial) viewport scale factor after page is fully loaded!
        this.vpScale=util.getViewportScale();
      }

      this.loadCookie();

      // Predictive-text hooks.
      const bannerMgr = this.bannerView;
      const _this = this;

      // Register a listener for model change events so that we can hot-swap the banner as needed.
      // Handled here b/c banner changes may trigger a need to re-layout the OSK.
      keymanweb.core.languageProcessor.on('statechange', 
                                          function(state: text.prediction.StateChangeEnum) {
        let currentType = bannerMgr.activeType;
        bannerMgr.selectBanner(state);

        if(currentType != bannerMgr.activeType) {
          _this.refreshLayout();
        }

        return true;
      });

      // Add header element to OSK only for desktop browsers
      if(util.device.formFactor == 'desktop') {
        const layout = this.desktopLayout = new layouts.TargetedFloatLayout();
        this.headerView = layout.titleBar;
      }
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

    protected postKeyboardLoad() {
      this._Visible = false;  // I3363 (Build 301)

      this._Box.onmouseover = this._VKbdMouseOver;
      this._Box.onmouseout = this._VKbdMouseOut;

      // Add header element to OSK only for desktop browsers
      if(this.desktopLayout) {
        const layout = this.desktopLayout;
        layout.attachToView(this);
        this.desktopLayout.titleBar.setTitleFromKeyboard(this.activeKeyboard);

        if(this.vkbd) {
          this.footerView = layout.resizeBar;
          this._Box.appendChild(this.footerView.element);
        }
      }

      if(this._Enabled) {
        this._Show();
      }
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
      let device = this.device;

      // Do not try to display OSK if undefined, or no active element
      if(keymanweb.domManager.getActiveElement() == null) {
        return;
      }

      // Never display the OSK for desktop browsers unless KMW element is focused, and a keyboard selected
      if((!device.touchable) && (this.activeKeyboard == null || !this._Enabled)) {
        return;
      }

      this.makeVisible();

      var Ls = this._Box.style;

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

      // The following code will always be executed except for externally created OSK such as EuroLatin
      if(this.vkbd && device.touchable) {
        Ls.position='fixed';
        Ls.left=Ls.bottom='0px';
        Ls.border='none';
        Ls.borderTop='1px solid gray';

        this._Enabled=true;
        this._Visible=true; // I3363 (Build 301)
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
      const priorDisplayStyle = this._Box.style.display;
      this.makeHidden(hiddenByUser);

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
        // Prevent insta-hide behavior; we want an animated fadeout here.
        os.display = priorDisplayStyle;

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
  }
}
