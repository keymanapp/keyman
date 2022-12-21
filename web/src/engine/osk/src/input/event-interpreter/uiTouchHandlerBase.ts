import InputEventCoordinate from "../inputEventCoordinate.js";
import { getAbsoluteY } from 'keyman/engine/dom-utils';

/**
 * This class was added to facilitate scroll handling for overflow-x elements, though it could
 * be extended in the future to accept overflow-y if needed.
 *
 * This is necessary because of the OSK's need to use `.preventDefault()` for stability; that
 * same method blocks native handling of overflow scrolling for touch browsers.
 */
class ScrollState {
  // While we don't currently track y-coordinates here, the class is designed
  // to permit tracking them with minimal extra effort if we ever decide to do so.
  x: number;
  totalLength = 0;

  // The amount of coordinate 'noise' allowed during a scroll-enabled touch allowed
  // before interpreting the currently-ongoing touch command as having scrolled.
  static readonly HAS_SCROLLED_FUDGE_FACTOR = 10;

  constructor(coord: InputEventCoordinate) {
    this.x = coord.x;

    this.totalLength = 0;
  }

  updateTo(coord: InputEventCoordinate): {deltaX: number} {
    let x = this.x;
    this.x = coord.x;

    let deltas = {deltaX: this.x - x};
    this.totalLength += Math.abs(deltas.deltaX);

    return deltas;
  }

  public get hasScrolled(): boolean {
    // Allow an accidental fudge-factor for overflow element noise during a touch, but not much.
    return this.totalLength > ScrollState.HAS_SCROLLED_FUDGE_FACTOR;
  }
}

export default abstract class UITouchHandlerBase<Target extends HTMLElement> {
  private rowClassMatch: string;
  private selectedTargetMatch: string;

  private baseElement: HTMLElement;
  private scroller: HTMLElement;

  private touchX: number;
  private touchY: number;
  private touchCount: number;

  private currentTarget: Target;

  private scrollTouchState: ScrollState;
  private pendingTarget: Target;

  constructor(baseElement: HTMLElement, scroller: HTMLElement, rowClassMatch: string, selectedTargetMatch: string) {
    this.baseElement = baseElement;
    this.rowClassMatch = rowClassMatch;
    this.selectedTargetMatch = selectedTargetMatch;

    this.scroller = scroller || null;
  }

  /**
   * Finds the internally-preferred target element or submenu target element.
   * @param e The DOM element that actually received the touch event.
   * May be parent, child, or the actually-desired element itself.
   */
  abstract findTargetFrom(e: HTMLElement): Target;

  /**
   * Highlights the target element as visual feedback representing
   * a pending touch.
   * @param t The `Target` to highlight
   * @param state `true` to apply highlighting, `false` to remove it.
   */
  protected abstract highlight(t: Target, state: boolean): void;

  /**
   * Called whenever the touch-handling analysis determines that the Target has been selected
   * @param t The `Target` to activate/execute.
   */
  protected abstract select(t: Target): void;

  /**
   * Requests info on whether or not the indicated `Target` has subkeys or a submenu.
   * @param t A `Target`.
   */
  protected abstract hasSubmenu(t: Target): boolean;

  /**
   * Indicates that the user is maintaining a `Touch` on the specified `Target`.
   * Popups and-or longpress menus may be appropriate.
   * @param t The `Target` being held.
   */
  protected abstract hold(t: Target): void;

  /**
   * Signals that any popup elements (previews, subkey views, etc) should be cancelled.
   */
  protected abstract clearHolds(): void;

  /**
   * Requests a boolean indicating whether or not the UI is currently displaying any input-blocking popup elements.
   * Embedded mode should return `true` when the app is displaying popup menus.
   */
  protected abstract hasModalPopup(): boolean;

  /**
   * Designed to support highlighting of prepended base keys on phone form-factor subkey menus.
   * @param target The base element with a potential subkey menu alias.
   * @returns The aliased submenu version of the `Target`, or the original `Target` if no alias exists.
   */
  protected abstract dealiasSubTarget(target: Target): Target;

  /**
   * Should return true whenever a 'native'-mode submenu (or subkey) display is active.
   */
  protected abstract isSubmenuActive(): boolean;

  /**
   * For 'native' mode - requests that the submenu for the indicated `Target` be instantly displayed.
   * @param target The base element with a potential submenu
   */
  protected abstract displaySubmenuFor(target: Target);

  /**
   * Identify the key nearest to (but NOT under) the touch point if at the end of a key row,
   * but return null more than about 0.6 key width from the nearest key.
   *
   *  @param  {Object}  coord   A pre-analyzed input coordinate
   *  @param  {Object}  t   HTML object at touch point
   *  @param  {boolean} omitCurrent  Omits any target directly under the touch point.
   *  @return {Object}      nearest key to touch point
   *
   **/
  private findTargetFromTouch(coord: InputEventCoordinate, t: HTMLElement, forMove: boolean): Target {
    var x = coord.x;

    // Get the UI row beneath touch point (SuggestionBanner div, 'kmw-key-row' if OSK, ...)
    while(t && t.className !== undefined && t.className.indexOf(this.rowClassMatch) < 0) {
      t = <HTMLElement> t.parentNode;
    }
    if(!t) {
      return null;
    }

    // Find minimum distance from any key
    var k: number, bestMatch=0, dxMax=24, dxMin=100000, x1: number, x2: number;
    for(k = 0; k < t.childNodes.length; k++) {
      let childNode = t.childNodes[k] as HTMLElement;

      if(this.isInvalidTarget(this.findTargetFrom(childNode))) {
        continue;
      }

      x1 = childNode.offsetLeft;
      x2 = x1 + childNode.offsetWidth;

      // If it lies completely to the right and is the closest so far
      let dxRight = x1 - x;
      if(dxRight >= 0 && dxRight < dxMin) {
        bestMatch = k;
        dxMin = dxRight;
      }

      // If it lies completely to the left and is the closest so far
      let dxLeft = x - x2;
      if(dxLeft >= 0 && dxLeft < dxMin) {
        bestMatch = k;
        dxMin = dxLeft;
      }

      // If it is neither completely to the left nor completely to the right,
      // it's under the cursor.  Stop the search!
      if(dxLeft < 0 && dxRight < 0) {
        return this.findTargetFrom(childNode);
      }
    }

    if(dxMin < 100000) {
      t = <HTMLElement> t.childNodes[bestMatch];
      x1 = t.offsetLeft;
      x2 = x1 + t.offsetWidth;

      // Limit extended touch area to the larger of 0.6 of the potential target's width and 24 px
      if(t.offsetWidth > 40) {
        dxMax = 0.6 * t.offsetWidth;
      }

      if(((x1 - x) >= 0 && (x1 - x) < dxMax) || ((x - x2) >= 0 && (x - x2) < dxMax)) {
        return this.findTargetFrom(t);
      }
    }
    return null;
  }

  findBestTarget(coord: InputEventCoordinate, forMove?: boolean) {
    var eventTarget: HTMLElement;

    if(forMove) {
      const clientX = coord.x + document.body.scrollLeft;
      const clientY = coord.y + document.body.scrollTop;
      eventTarget = document.elementFromPoint(clientX, clientY) as HTMLElement;
    } else {
      eventTarget = coord.target as HTMLElement;
    }

    let target = this.findTargetFrom(eventTarget);

    // Should refactor this multi-check a bit for more overall reliability.
    if(!target) {
      // We didn't find a direct target, so we should look for the closest possible one.
      // Filters out invalid targets.
      target = this.findTargetFromTouch(coord, eventTarget, forMove);
    }

    return target;
  }

  /**
   * Reports whether or not a `Target` should be considered invalid.  Needed by the OSK for
   * hidden keys.
   * @param target A `Target` element to be validated.
   */
  protected isInvalidTarget(target: Target): boolean {
    return false;
  }

  touchStart(coord: InputEventCoordinate) {
    // Determine the selected Target, manage state.
    this.currentTarget = this.findBestTarget(coord);
    this.touchX = coord.x;
    this.touchY = coord.y;

    // If popup stuff, immediately return.

    this.touchCount = coord.activeInputCount;

    if(!this.currentTarget) {
      return;
    }

    // Establish scroll tracking.
    this.scrollTouchState = new ScrollState(coord);

    // Alright, Target acquired!  Now to use it:

    // Highlight the touched key
    this.highlight(this.currentTarget,true);

    // If used by the OSK, the special function keys need immediate action
    // Add a `checkForImmediates()` to facilitate this.
    if(this.pendingTarget) {
      this.highlight(this.pendingTarget, false);
      this.select(this.pendingTarget);
      this.clearHolds();
      // Decrement the number of unreleased touch points to prevent
      // sending the keystroke again when the key is actually released
      this.touchCount--;
    } else {
      // If this key has subkey, start timer to display subkeys after delay, set up release
      this.hold(this.currentTarget);
    }
    this.pendingTarget = this.currentTarget;
  }

  touchEnd(coord: InputEventCoordinate): void {
    // Prevent incorrect multi-touch behaviour if native or device popup visible
    let t = this.currentTarget;

    if(this.isSubmenuActive() || this.hasModalPopup()) {
      // Ignore release if a multiple touch
      if(coord.activeInputCount > 0) {
        return;
      }

      // Cancel (but do not execute) pending key if neither a popup key or the base key
      if(t == null || t.id.indexOf('popup') < 0) {
        if (this.pendingTarget) {
          this.highlight(this.pendingTarget,false);
        }
        this.clearHolds();
        this.pendingTarget = null;
      }
    }

    // Test if moved off screen (effective release point must be corrected for touch point horizontal speed)
    // This is not completely effective and needs some tweaking, especially on Android
    var x = coord.x;
    var beyondEdge = ((x < 2 && this.touchX > 5) || (x > window.innerWidth - 2 && this.touchX < window.innerWidth - 5));

    if(this.scrollTouchState) {
      beyondEdge = beyondEdge || this.scrollTouchState.hasScrolled;
    }

    // Save then decrement current touch count
    var tc=this.touchCount;
    if(this.touchCount > 0) {
      this.touchCount--;
    }

    // Process and clear highlighting of pending target
    if(this.pendingTarget) {
      this.highlight(this.pendingTarget,false);

      // Output character unless moved off key
      if(this.pendingTarget.className.indexOf('hidden') < 0 && tc > 0 && !beyondEdge) {
        this.select(this.pendingTarget);
      }
      this.clearHolds();
      this.pendingTarget = null;
      // Always clear highlighting of current target on release (multi-touch)
    } else {
      t = this.findBestTarget(coord);

      if(t) {
        this.highlight(t,false);
      }
    }
  }

  /**
   * OSK touch move event handler
   *
   *  @param  {Object}  coord   A pre-analyzed input coordinate
   *
   **/
  touchMove(coord: InputEventCoordinate) : void {
    //let keyman = com.keyman.singleton;

    // Do not attempt to support reselection of target key for overlapped keystrokes
    if(coord.activeInputCount > 1 || this.touchCount == 0) {
      return;
    }

    if(this.scrollTouchState != null) {
      // TODO:  Work on smoothing this out; looks like subpixel scroll info gets rounded out,
      // and this results in a mild desync.
      let deltaX = this.scrollTouchState.updateTo(coord).deltaX;
      if(this.scroller) {
        this.scroller.scrollLeft -= deltaX;
      }

      return;
    }

    // Get touch position
    var y = coord.y;

    // Move target key and highlighting
    var key0 = this.pendingTarget,
        key1 = this.findBestTarget(coord, true);  // For the OSK, this ALSO gets subkeys.

    // If option should not be selectable, how do we re-target?


    // Do not move over keys if device popup visible
    if(this.hasModalPopup()) {
      if(key0) {
        this.highlight(key0,false);
      }
      this.pendingTarget=null;
      return;
    }

    // Use the popup duplicate of the base key if a phone with a visible popup array
    key1 = this.dealiasSubTarget(key1);

    // Identify current touch position (to manage off-key release)
    this.currentTarget = key1;

    // Clear previous key highlighting
    if(key0 && key1 && key1 !== key0) {
      this.highlight(key0,false);
    }

    // Code below directly related to subkeys should only be triggered within 'native' mode.
    // The embedded version instead passes info to the apps to produce their own subkeys in-app.

    // If popup is visible, need to move over popup, not over main keyboard
    if(key1 && this.hasSubmenu(key1)) {
      //this.highlightSubKeys(key1,x,y);

      // Currently only used by the banner... which currently does not do submenus.
      // // Native-mode: show popup keys immediately if touch moved up towards key array (KMEW-100, Build 353)
      // if(!keyman.isEmbedded && (this.touchY-y > 5) && !this.isSubmenuActive()) {
      //   // Instantly show the submenu.
      //   this.displaySubmenuFor(key1);
      // }

      // Once a subkey array is displayed, do not allow changing the base key.
      // Keep that array visible and accept no other options until the touch ends.
      if(key1 && key1.id.indexOf('popup') < 0) { // TODO:  reliant on 'popup' in .id
        return;
      }

      // Highlight the base key on devices that do not append it to the subkey array.
      if(key1 && key1.className.indexOf(this.selectedTargetMatch) < 0) {
        this.highlight(key1,true);
      }
      // Cancel touch if moved up and off keyboard, unless popup keys visible
    } else {
      let base = this.baseElement;
      let top = getAbsoluteY(base);
      let height = base.offsetHeight;
      let yMin = Math.max(5, top - 0.25 * height);
      let yMax = (top + height) + 0.25 * height;
      if(key0 && (coord.y < yMin || coord.y > yMax)) {
        this.highlight(key0,false);
        this.clearHolds();
        this.pendingTarget = null;
      }
    }

    // Replace the target key, if any, by the new target key
    // Do not replace a null target, as that indicates the key has already been released
    if(key1 && this.pendingTarget) {
      this.pendingTarget = key1;
    }

    if(this.pendingTarget) {
      if(key1 && (key0 != key1 || key1.className.indexOf(this.selectedTargetMatch) < 0)) {
        this.highlight(key1,true);
      }
    }

    if(key0 && key1 && (key1 != key0) && (key1.id != '')) {
      //  Display the touch-hold keys (after a pause)
      this.hold(key1);
    }
  }
}
