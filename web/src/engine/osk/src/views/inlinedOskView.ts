import { DeviceSpec } from '@keymanapp/keyboard-processor';

import OSKView, { OSKPos, OSKRect } from './oskView.js';
import VisualKeyboard from '../visualKeyboard.js';
import Configuration from '../config/viewConfiguration.js';
import SimpleActivator from './simpleActivator.js';

/*
 * Keyman is copyright (c) SIL International.  MIT License.
 */

/**
 * Defines a version of the OSK that produces an element designed for site-controlled
 * insertion into the DOM.  Rather than "floating" over the page, this version is inlined
 * as part of the host page's layout.
 */
export default class InlinedOSKView extends OSKView {
  public constructor(config: Configuration) {
    config.activator = config.activator || new SimpleActivator();
    super(config);
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

  protected postKeyboardAdjustments() {
  }

  /**
   * Moves the OSK back to default position, floating under active input element
   *
   * Is a long-published API intended solely for use with the FloatingOSKView use pattern.
   * @param  keepDefaultPosition  If true, does not reset the default x,y set by `setRect`.
   *                              If false or omitted, resets the default x,y as well.
   */
  ['restorePosition']: (keepDefaultPosition?: boolean) => void = function(this: InlinedOSKView, keepDefaultPosition?: boolean) {
    return;
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

  public present() {
    super.present();

    this.legacyEvents.callEvent('show', {});
  }

  protected setDisplayPositioning() {
    // no-op; an inlined OSK cannot control its own positioning.
  }

  protected allowsDeviceChange(newSpec: DeviceSpec): boolean {
    return true;
  }
}
