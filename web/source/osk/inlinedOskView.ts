// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="../kmwexthtml.ts" />
// Includes the touch-mode language picker UI.
/// <reference path="languageMenu.ts" />
/// <reference path="lengthStyle.ts" />
// Defines desktop-centric OSK positioning + sizing behavior
/// <reference path="layouts/targetedFloatLayout.ts" />
/// <reference path="oskView.ts" />

/*
 * Keyman is copyright (c) SIL International.  MIT License.
 */

namespace com.keyman.osk {
  type OSKPos = {'left'?: number, 'top'?: number};

  /**
   * Defines a version of the OSK that produces an element designed for site-controlled
   * insertion into the DOM.  Rather than "floating" over the page, this version is inlined
   * as part of the host page's layout.
   */
  export class InlinedOSKView extends OSKView {
    // Key code definition aliases for legacy keyboards  (They expect window['keyman']['osk'].___)
    modifierCodes = text.Codes.modifierCodes;
    modifierBitmasks = text.Codes.modifierBitmasks;
    stateBitmasks = text.Codes.stateBitmasks;
    keyCodes = text.Codes.keyCodes;

    public constructor(modeledDevice: utils.DeviceSpec, hostDevice?: utils.DeviceSpec) {
      super(modeledDevice, hostDevice);

      this.activationMode = ActivationMode.manual;
    }

    public get element(): HTMLDivElement {
      return this._Box;
    }

    /**
     * Clears OSK variables prior to exit (JMD 1.9.1 - relocation of local variables 3/9/10)
     * 
     * This should probably be merged or incorporated into the `shutdown` method at some point.
     */
    _Unload() {
      this.keyboardView = null;
      this.bannerView = null;
      this._Box = null;
    }

    protected setBoxStyling() {
      const s = this._Box.style;
      s.display  = 'none';
      // Positioned with no relative offset from its default position.
      // This allows _Box to still serve as an offsetParent for keytip & subkey menu positioning.
      s.position = 'relative';
    }

    protected postKeyboardLoad() {
      this._Visible = false;  // I3363 (Build 301)

      this._Box.onmouseover = this._VKbdMouseOver;
      this._Box.onmouseout = this._VKbdMouseOut;

      if(this.displayIfActive) {
        this.present();
      }
    }

    /**
     * Moves the OSK back to default position, floating under active input element
     * 
     * Is a long-published API intended solely for use with the FloatingOSKView use pattern.
     * @param  keepDefaultPosition  If true, does not reset the default x,y set by `setRect`.
     *                              If false or omitted, resets the default x,y as well.
     */
    ['restorePosition']: (keepDefaultPosition?: boolean) => void = function(this: AnchoredOSKView, keepDefaultPosition?: boolean) {
      return;
    }.bind(this);

    /**
     * Activates the KMW UI on mouse over, allowing DOMManager to preserve the
     * active element's (conceptual) focus during OSK interactions.
     */
    private _VKbdMouseOver = function(this: AnchoredOSKView, e) {
      com.keyman.singleton.uiManager.setActivatingUI(true);
    }.bind(this);

    /**
     * Cancels activation of the KMW UI on mouse out, which is used to disable
     * DOMManager's focus-preservation mode.
     * 
     * @see _VKbdMouseOver
     */
    private _VKbdMouseOut = function(this: AnchoredOSKView, e) {
      com.keyman.singleton.uiManager.setActivatingUI(false);
    }.bind(this);

    /**
     * Get the default height for the OSK
     * @return  height in pixels
     **/
    getDefaultKeyboardHeight(): number {
      if(this.keyboardView instanceof VisualKeyboard) {
        return this.keyboardView.height;
      } else {
        // Should probably refine, but it's a decent stopgap.
        return this.computedHeight;
      }
    }

    /**
     * Get the default width for the OSK
     * @return width in pixels
     **/
    getDefaultWidth(): number {
      return this.computedWidth;
    }

    /**
     * Allow the UI or page to set the position and size of the OSK
     * and (optionally) override user repositioning or sizing
     * 
     * Designed solely for use with the FloatingOSKView use pattern, but is a
     * long-standing API endpoint that needs preservation.
     *
     * @param  p  Array object with position and size of OSK container
    **/
    ['setRect'](p: OSKRect) {
      return;
    }

    /**
     * Get position of OSK window
     *
     * @return Array object with OSK window position
    **/
    getPos(): OSKPos {
      var Lkbd=this._Box, p={
        left: this._Visible ? Lkbd.offsetLeft : undefined,
        top:  this._Visible ? Lkbd.offsetTop  : undefined
      };

      return p;
    }

    /**
     * Set position of OSK window, but limited to the screen.
     * 
     * Designed solely for use with the FloatingOSKView use pattern, but is a
     * long-standing API endpoint that needs preservation.
     * @param  p Array object with OSK left, top
     */
    ['setPos'](p: OSKPos) {
      return; // I3363 (Build 301)
    }

    protected setDisplayPositioning() {
      // no-op; an inlined OSK cannot control its own positioning.
    }

    /**
     * Allow UI to respond to OSK being shown (passing position and properties)
     *
     * @param  p  object with coordinates and userdefined flag
     * @return 
     *
     */
    doShow(p) {
      return com.keyman.singleton.util.callEvent('osk.show',p);
    }

    /**
     * Allows UI modules to update state when the OSK is being hidden
     *
     * @param  p  object with coordinates and userdefined flag
     * @return 
     */
    doHide(p) {
      return com.keyman.singleton.util.callEvent('osk.hide',p);
    }

    protected allowsDeviceChange(newSpec: com.keyman.utils.DeviceSpec): boolean {
      return true;
    }
  }
}
