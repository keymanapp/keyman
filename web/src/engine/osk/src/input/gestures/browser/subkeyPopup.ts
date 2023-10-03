import OSKSubKey from './oskSubKey.js';
import RealizedGesture from '../realizedGesture.interface.js';
import { type KeyElement } from '../../../keyElement.js';
import OSKBaseKey from '../../../keyboard-layout/oskBaseKey.js';
import VisualKeyboard from '../../../visualKeyboard.js';
import InputEventCoordinate from '../../../input/inputEventCoordinate.js';

import { DeviceSpec, KeyEvent, ActiveSubkey } from '@keymanapp/keyboard-processor';

/**
 * Represents a 'realized' longpress gesture's default implementation
 * within KeymanWeb.  Once a touch sequence has been confirmed to
 * correspond to a longpress gesture, implementations of this class
 * provide the following:
 * * The UI needed to present a subkey menu
 * * The state management needed to present feedback about the
 * currently-selected subkey to the user
 * * A `Promise` that will resolve to the user's selected subkey
 * once the longpress operation is complete.
 *
 * As selection of the subkey occurs after the subkey popup is
 * displayed, selection of the subkey is inherently asynchronous.
 * The `Promise` may also resolve to `null` if the user indicates
 * the desire to cancel subkey selection.
 */
export default class SubkeyPopup implements RealizedGesture {
  public readonly element: HTMLDivElement;
  public readonly shim: HTMLDivElement;

  private vkbd: VisualKeyboard;
  private currentSelection: KeyElement;

  private callout: HTMLDivElement;

  public readonly baseKey: KeyElement;
  public readonly promise: Promise<KeyEvent>;

  // Resolves the promise that generated this SubkeyPopup.
  private resolver: (keyEvent: KeyEvent) => void;

  constructor(vkbd: VisualKeyboard, e: KeyElement) {
    let _this = this;

    this.promise = new Promise<KeyEvent>(function(resolve) {
      _this.resolver = resolve;
    })

    this.vkbd = vkbd;
    this.baseKey = e;

    // If the user doesn't move their finger and releases, we'll output the base key
    // by default.
    this.currentSelection = e;
    e.key.highlight(true);

    // A tag we directly set on a key element during its construction.
    let subKeySpec: ActiveSubkey[] = e['subKeys'];

    // The holder is position:fixed, but the keys do not need to be, as no scrolling
    // is possible while the array is visible.  So it is simplest to let the keys have
    // position:static and display:inline-block
    var subKeys = this.element = document.createElement('div');

    var i;
    subKeys.id='kmw-popup-keys';

    // #3718: No longer prepend base key to popup array

    // Must set position dynamically, not in CSS
    var ss=subKeys.style;

    // Set key font according to layout, or defaulting to OSK font
    // (copied, not inherited, since OSK is not a parent of popup keys)
    ss.fontFamily=vkbd.fontFamily;

    // Copy the font size from the parent key, allowing for style inheritance
    const computedStyle = getComputedStyle(e);
    ss.fontSize=computedStyle.fontSize;
    ss.visibility='hidden';

    var nKeys=subKeySpec.length,nRows,nCols;
    nRows=Math.min(Math.ceil(nKeys/9),2);
    nCols=Math.ceil(nKeys/nRows);
    ss.width=(nCols*e.offsetWidth+nCols*5)+'px';

    // Add nested button elements for each sub-key
    for(i=0; i<nKeys; i++) {
      var needsTopMargin = false;
      let nRow=Math.floor(i/nCols);
      if(nRows > 1 && nRow > 0) {
        needsTopMargin = true;
      }

      let layer = e['key'].layer;
      if(typeof(layer) != 'string' || layer == '') {
        // Use the currently-active layer.
        layer = vkbd.layerId;
      }
      let keyGenerator = new OSKSubKey(subKeySpec[i], layer);
      let kDiv = keyGenerator.construct(vkbd, <KeyElement> e, needsTopMargin);

      subKeys.appendChild(kDiv);
    }

    // And add a filter to fade main keyboard
    this.shim = document.createElement('div');
    this.shim.id = 'kmw-popup-shim';

    // Highlight the duplicated base key or ideal subkey (if a phone)
    if(vkbd.device.formFactor == DeviceSpec.FormFactor.Phone) {
      this.selectDefaultSubkey(vkbd, e, subKeys /* == this.element */);
    }
  }

  finalize(input: InputEventCoordinate) {
    if(this.resolver) {
      let keyEvent: KeyEvent = null;
      if(this.currentSelection) {
        keyEvent = this.vkbd.initKeyEvent(this.currentSelection, input);
        this.currentSelection.key.highlight(false);
      }
      this.resolver(keyEvent);
    }
    this.resolver = null;
  }

  reposition(vkbd: VisualKeyboard) {
    let subKeys = this.element;
    let e = this.baseKey;

    // And correct its position with respect to that element
    const _Box = vkbd.topContainer;
    let rowElement = (e.key as OSKBaseKey).row.element;
    let ss=subKeys.style;
    var x = e.offsetLeft + (<HTMLElement>e.offsetParent).offsetLeft + 0.5*(e.offsetWidth-subKeys.offsetWidth);
    var xMax = vkbd.width - subKeys.offsetWidth;

    if(x > xMax) {
      x=xMax;
    }
    if(x < 0) {
      x=0;
    }
    ss.left=x+'px';

    let _BoxRect = _Box.getBoundingClientRect();
    let rowElementRect = rowElement.getBoundingClientRect();
    ss.top = (rowElementRect.top - _BoxRect.top - subKeys.offsetHeight - 3) + 'px';

    // Make the popup keys visible
    ss.visibility='visible';

    // For now, should only be true (in production) when keyman.isEmbedded == true.
    let constrainPopup = vkbd.isEmbedded;

    let cs = getComputedStyle(subKeys);
    let topY = parseFloat(cs.top);

    // Adjust the vertical position of the popup to keep it within the
    // bounds of the keyboard rectangle, when on iPhone (system keyboard)
    const topOffset = 0; // Set this when testing constrainPopup, e.g. to -80px
    let delta = 0;
    if(topY < topOffset && constrainPopup) {
      delta = topOffset - topY;
      ss.top = topOffset + 'px';
    }

    // Add the callout
    if(vkbd.device.formFactor == DeviceSpec.FormFactor.Phone && vkbd.device.OS == DeviceSpec.OperatingSystem.iOS) {
      this.callout = this.addCallout(e, delta);
    }
  }

  /**
   * Add a callout for popup keys (if KeymanWeb on a phone device)
   *
   * @param   {Object}  key   HTML key element
   * @return  {Object}        callout object
   */
  addCallout(key: KeyElement, delta?: number): HTMLDivElement {
    const _Box = this.vkbd.topContainer;

    delta = delta || 0;

    let calloutHeight = key.offsetHeight - delta + 6;

    if(calloutHeight > 0) {
      var cc = document.createElement('div'), ccs = cc.style;
      cc.id = 'kmw-popup-callout';
      _Box.appendChild(cc);

      // Create the callout
      let keyRect = key.getBoundingClientRect();
      let _BoxRect = _Box.getBoundingClientRect();

      // Set position and style
      // We're going to adjust the top of the box to ensure it stays
      // pixel aligned, otherwise we can get antialiasing artifacts
      // that look ugly
      let top = Math.floor(keyRect.top - _BoxRect.top - 9 + delta);
      ccs.top = top + 'px';
      ccs.left = (keyRect.left - _BoxRect.left) + 'px';
      ccs.width = keyRect.width + 'px';
      ccs.height = (keyRect.bottom - _BoxRect.top - top - 1) + 'px'; //(height - 1) + 'px';

      // Return callout element, to allow removal later
      return cc;
    } else {
      return null;
    }
  }

  selectDefaultSubkey(vkbd: VisualKeyboard, baseKey: KeyElement, popupBase: HTMLElement) {
    var bk: KeyElement;
    let subkeys = baseKey['subKeys'];
    for(let i=0; i < subkeys.length; i++) {
      let skSpec = subkeys[i];
      let skElement = <KeyElement> popupBase.childNodes[i].firstChild;

      // Preference order:
      // #1:  if a default subkey has been specified, select it.  (pending, for 15.0+)
      // #2:  if no default subkey is specified, default to a subkey with the same
      //      key ID and layer / modifier spec.
      //if(skSpec.isDefault) { TODO for 15.0
      //  bk = skElement;
      //  break;
      //} else
      if(!baseKey.key || !baseKey.key.spec) {
        continue;
      }

      if(skSpec.elementID == baseKey.key.spec.elementID) {
        bk = skElement;
        break; // Best possible match has been found.  (Disable 'break' once above block is implemented.)
      }
    }

    if(bk) {
      vkbd.keyPending = bk;
      // Subkeys never get key previews, so we can directly highlight the subkey.
      bk.key.highlight(true);
    }
  }

  isVisible(): boolean {
    return this.element.style.visibility == 'visible';
  }

  clear() {
    // Discard the reference to the Promise's resolve method, allowing
    // GC to clean it up.  The corresponding Promise's contract allows
    // passive cancellation.
    this.resolver = null;

    // Remove the displayed subkey array, if any
    if(this.element.parentNode) {
      this.element.parentNode.removeChild(this.element);
    }

    if(this.shim.parentNode) {
      this.shim.parentNode.removeChild(this.shim);
    }

    if(this.callout && this.callout.parentNode) {
      this.callout.parentNode.removeChild(this.callout);
    }
  }

  updateTouch(input: InputEventCoordinate) {
    this.currentSelection = null;
    this.baseKey.key.highlight(false);

    for(let i=0; i < this.baseKey['subKeys'].length; i++) {
      try {
        let sk = this.element.childNodes[i].firstChild as KeyElement;

        let onKey = sk.key.isUnderTouch(input);
        if(onKey) {
          this.currentSelection = sk;
        }
        sk.key.highlight(onKey);
      } catch(ex) {
        if(ex.message) {
          console.error("Unexpected error when attempting to update selected subkey:" + ex.message);
        } else {
          console.error("Unexpected error (and error type) when attempting to update selected subkey.");
        }
      }
    }

    // Use the popup duplicate of the base key if a phone with a visible popup array
    if(!this.currentSelection && this.baseKey.key.isUnderTouch(input)) {
      this.baseKey.key.highlight(true);
      this.currentSelection = this.baseKey;
    }
  }
}
