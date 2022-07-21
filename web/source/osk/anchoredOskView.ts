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

    private isResizing: boolean = false;

    // Key code definition aliases for legacy keyboards  (They expect window['keyman']['osk'].___)
    modifierCodes = text.Codes.modifierCodes;
    modifierBitmasks = text.Codes.modifierBitmasks;
    stateBitmasks = text.Codes.stateBitmasks;
    keyCodes = text.Codes.keyCodes;

    public constructor(modeledDevice: utils.DeviceSpec) {
      super(modeledDevice);

      document.body.appendChild(this._Box);

      let keymanweb = com.keyman.singleton;
      if(keymanweb.isEmbedded) {
        this.activationMode == ActivationMode.manual;
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

    protected setBoxStyling() {
      const s = this._Box.style;

      s.zIndex   = '9999';
      s.display  = 'none';
      s.width    = '100%';
      s.position = 'fixed';
    }

    /**
     * @override
     */
    public refreshLayout(pending?: boolean): void {
      // This function is generally triggered whenever the OSK's dimensions change, among other
      // things.
      if(this.isResizing) {
        return;
      }

      try {
        this.isResizing = true;
        // This resizes the OSK to what is appropriate for the device's current orientation,
        // which will often trigger a resize event... which in turn triggers a layout refresh.
        //
        // So, we mark and unmark the `isResizing` flag to prevent triggering a circular
        // call-stack chain from this call.
        this.doResize();
      } finally {
        this.isResizing = false;
      }
      super.refreshLayout(pending);
    }

    protected doResize() {
      if(this.vkbd && this.device.touchable) {
        let targetOSKHeight = this.getDefaultKeyboardHeight();
        this.setSize(this.getDefaultWidth(), targetOSKHeight + this.banner.height);
      }
    }

    protected postKeyboardLoad() {
      // Initializes the size of a touch keyboard.
      this.doResize();

      this._Visible = false;  // I3363 (Build 301)

      this._Box.onmouseover = this._VKbdMouseOver;
      this._Box.onmouseout = this._VKbdMouseOut;

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

      let baseWidth  = document?.documentElement?.clientWidth;
      let baseHeight = document?.documentElement?.clientHeight;
      if(typeof baseWidth == 'undefined') {
        baseWidth  = Math.min(screen.height, screen.width);
        baseHeight = Math.max(screen.height, screen.width);

        if(!keymanweb.util.portraitView()) {
          let temp = baseWidth;
          baseWidth = baseHeight;
          baseHeight = temp;
        }
      }

      var oskHeightLandscapeView=Math.floor(Math.min(baseHeight, baseWidth)/2),
          height=oskHeightLandscapeView;

      if(device.formFactor == 'phone') {
        if(keymanweb.util.portraitView())
          height=Math.floor(baseHeight/2.4);
        else
          height=Math.floor(baseHeight/1.6);  //adjust for aspect ratio, increase slightly for iPhone 5
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

      width = document?.documentElement?.clientWidth;
      if(typeof width == 'undefined') {
        if(device.OS == 'iOS') {
          // iOS does not interchange these values when the orientation changes!
          //width = util.portraitView() ? screen.width : screen.height;
          width = window.innerWidth;
        } else if(device.OS == 'Android') {
          width=screen.availWidth;
        } else {
          width=screen.width;
        }
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

    protected setDisplayPositioning() {
      let Ls = this._Box.style;

      // The following code will always be executed except for externally created OSK such as EuroLatin
      if(this.vkbd) {
        Ls.position='fixed';
        Ls.left=Ls.bottom='0px';
        Ls.border='none';
        Ls.borderTop='1px solid gray';
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
