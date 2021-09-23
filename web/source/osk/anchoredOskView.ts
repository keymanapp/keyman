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
  type OSKPos = {'left'?: number, 'top'?: number};

  export class AnchoredOSKView extends OSKView {
    desktopLayout: layouts.TargetedFloatLayout;

    // OSK positioning fields
    x: number;
    y: number;

    // Key code definition aliases for legacy keyboards  (They expect window['keyman']['osk'].___)
    modifierCodes = text.Codes.modifierCodes;
    modifierBitmasks = text.Codes.modifierBitmasks;
    stateBitmasks = text.Codes.stateBitmasks;
    keyCodes = text.Codes.keyCodes;

    public constructor(modeledDevice: utils.DeviceSpec) {
      super(modeledDevice);

      document.body.appendChild(this._Box);
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
      s.width    = '100%';
      s.position = 'fixed';
    }

    protected postKeyboardLoad() {
      // Initializes the size of a touch keyboard.
      if(this.vkbd && this.device.touchable) {
        let targetOSKHeight = this.vkbd.computedAdjustedOskHeight(this.getDefaultKeyboardHeight());
        this.setSize(this.getDefaultWidth(), targetOSKHeight + this.banner.height);
      }

      this._Visible = false;  // I3363 (Build 301)

      this._Box.onmouseover = this._VKbdMouseOver;
      this._Box.onmouseout = this._VKbdMouseOut;

      if(this._Enabled) {
        this._Show();
      }
    }

    /**
     * Function     restorePosition
     * Scope        Public
     * @param       {boolean?}      keepDefaultPosition  If true, does not reset the default x,y set by `setRect`.
     *                                                   If false or omitted, resets the default x,y as well.
     * Description  Move OSK back to default position, floating under active input element
     */
    ['restorePosition']: (keepDefaultPosition?: boolean) => void = function(this: AnchoredOSKView, keepDefaultPosition?: boolean) {
      return;
    }.bind(this);

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
     * Allow the UI or page to set the position and size of the OSK
     * and (optionally) override user repositioning or sizing
     *
     * @param       {Object.<string,number>}   p  Array object with position and size of OSK container
    **/
    ['setRect'](p: OSKRect) {
      return;
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
      return; // I3363 (Build 301)
    }

    /**
     * Display KMW OSK at specified position (returns nothing)
     *
     * @param       {number=}     Px      x-coordinate for OSK rectangle
     * @param       {number=}     Py      y-coordinate for OSK rectangle
     */
    _Show(Px?: number, Py?: number) {
      // Do not try to display OSK if no active element
      if(!this.activeTarget) {
        return;
      }

      this.makeVisible();

      var Ls = this._Box.style;

      /* In case it's still '0' from a hide() operation.
        * Happens when _Show is called before the transitionend events are processed,
        * which can happen in bulk-rendering contexts.
        *
        * (Opacity is only modified when device.touchable = true, though a couple of extra
        * conditions may apply.)
        */
      Ls.opacity='1';

      // The following code will always be executed except for externally created OSK such as EuroLatin
      if(this.vkbd) {
        Ls.position='fixed';
        Ls.left=Ls.bottom='0px';
        Ls.border='none';
        Ls.borderTop='1px solid gray';

        this._Enabled=true;
        this._Visible=true; // I3363 (Build 301)
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
        window.setTimeout(function(this: AnchoredOSKView) {
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
        this.lastActiveTarget.focus();
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
  }
}
