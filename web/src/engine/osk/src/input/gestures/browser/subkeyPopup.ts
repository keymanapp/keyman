import OSKSubKey from './oskSubKey.js';
import { type KeyElement } from '../../../keyElement.js';
import OSKBaseKey from '../../../keyboard-layout/oskBaseKey.js';
import VisualKeyboard from '../../../visualKeyboard.js';

import { DeviceSpec, ActiveSubKey, KeyDistribution, ActiveKeyBase } from '@keymanapp/keyboard-processor';
import { ConfigChangeClosure, GestureRecognizerConfiguration, GestureSequence, PaddedZoneSource, RecognitionZoneSource } from '@keymanapp/gesture-recognizer';
import { GestureHandler } from '../gestureHandler.js';
import { CorrectionLayout, CorrectionLayoutEntry, distributionFromDistanceMaps, keyTouchDistances } from '@keymanapp/input-processor';
import { GestureParams } from '../specsForLayout.js';

/**
 * The fraction of the base key's height to add to an unconstrained subkey-menu
 * callout.
 */
const CALLOUT_ROW_HEIGHT_RATIO = 0.2;

/**
 * The max fraction of a base key's width to use for a subkey-menu callout
 */
const MAX_CALLOUT_KEY_WIDTH = 1.2;

/**
 * Space to keep between the top of the base key and the bottom of the subkey menu,
 * if possible.
 */
const SUBKEY_MENU_VERT_OFFSET = 3;

/**
 * Should match the margin-left style value for 'kmw-key-square-ex' in kmwosk.css
 * if possible.
 */
const SUBKEY_DEFAULT_MARGIN_LEFT = 5;

/**
 * The minimum pixel-height value to use for subkey-menu callout height when not
 * obscured by an overlapping subkey menu (due to WebView boundary constraint effects)
 */
const CALLOUT_BASE_HEIGHT = 6 + SUBKEY_MENU_VERT_OFFSET;

/**
 * Represents a 'realized' longpress gesture's default implementation
 * within KeymanWeb.  Once a touch sequence has been confirmed to
 * correspond to a longpress gesture, implementations of this class
 * provide the following:
 * * The UI needed to present a subkey menu
 * * The state management needed to present feedback about the
 * currently-selected subkey to the user
 *
 * As selection of the subkey occurs after the subkey popup is
 * displayed, selection of the subkey is inherently asynchronous.
 */
export default class SubkeyPopup implements GestureHandler {
  readonly directlyEmitsKeys = true;

  public readonly element: HTMLDivElement;
  public readonly shim: HTMLDivElement;

  private currentSelection: KeyElement;

  private callout: HTMLDivElement;
  private readonly menuWidth: number;

  public readonly baseKey: KeyElement;
  public readonly subkeys: KeyElement[];

  private source: GestureSequence<KeyElement>;
  private readonly gestureParams: GestureParams;

  readonly shouldLockLayer: boolean = false;

  constructor(
    source: GestureSequence<KeyElement, string>,
    configChanger: ConfigChangeClosure<KeyElement>,
    vkbd: VisualKeyboard,
    e: KeyElement,
    gestureParams: GestureParams
  ) {
    this.baseKey = e;
    this.source = source;
    this.gestureParams = gestureParams;

    if(vkbd.layerLocked) {
      this.shouldLockLayer = true;
    }

    source.on('complete', () => {
      this.currentSelection?.key.highlight(false);
      this.clear();
    });

    // When subkey-selection is fully triggered, emit the selected key.
    source.on('stage', () => {
      const key = this.currentSelection;
      if(key) {
        const keyEvent = vkbd.keyEventFromSpec(key.key.spec);
        keyEvent.keyDistribution = this.currentStageKeyDistribution();

        vkbd.raiseKeyEvent(keyEvent, key);
      }
    });

    // From here, we want to make decisions based on only the subkey-menu portion of the gesture path.
    const subkeyComponent = source.stageReports[0].sources[0].constructSubview(true, false);

    // Watch for touchpoint selection of new keys.
    subkeyComponent.path.on('step', (sample) => {
      // Require a fudge-factor before dropping the default key.
      if(subkeyComponent.path.stats.netDistance >= 4) {
        this.currentSelection?.key.highlight(false);
        sample.item?.key.highlight(true);
        this.currentSelection = sample.item;
      }
    });

    // If the user doesn't move their finger and releases, we'll output the base key
    // by default.
    this.currentSelection = e;
    e.key.highlight(true);

    // A tag we directly set on a key element during its construction.
    let subKeySpec: ActiveSubKey[] = e['subKeys'];

    // The holder is position:fixed, but the keys do not need to be, as no scrolling
    // is possible while the array is visible.  So it is simplest to let the keys have
    // position:static and display:inline-block
    const elements = this.element = document.createElement('div');

    elements.id='kmw-popup-keys';

    // #3718: No longer prepend base key to popup array

    // Must set position dynamically, not in CSS
    var ss=elements.style;

    // Set key font according to layout, or defaulting to OSK font
    // (copied, not inherited, since OSK is not a parent of popup keys)
    ss.fontFamily=vkbd.fontFamily;

    // Copy the font size from the parent key, allowing for style inheritance
    const computedStyle = getComputedStyle(e);
    ss.fontSize=computedStyle.fontSize;
    ss.visibility='hidden';

    let layer = e['key'].layer;
    if (typeof (layer) != 'string' || layer == '') {
      // Use the currently-active layer.
      layer = vkbd.layerId;
    }

    const nKeys = subKeySpec.length;
    // Put a maximum of 9 keys in a row to reduce travel distance
    const nRows=Math.ceil(nKeys/9);
    const nCols=Math.ceil(nKeys/nRows);

    // Add nested button elements for each sub-key
    this.subkeys = [];
    let thisRowWidth = SUBKEY_DEFAULT_MARGIN_LEFT;
    let iRow = 0;
    for(let i=0, iCol=0; i<nKeys; i++, iCol++) {
      let subkeyWidth = (typeof subKeySpec[i]['width'] != 'undefined') ?
        subKeySpec[i]['width'] * e.offsetWidth / 100 :
        e.offsetWidth;

      if (subkeyWidth > vkbd.width - 2 * SUBKEY_DEFAULT_MARGIN_LEFT) {
        subkeyWidth = vkbd.width - 2 * SUBKEY_DEFAULT_MARGIN_LEFT;
      }

      if (thisRowWidth + subkeyWidth + SUBKEY_DEFAULT_MARGIN_LEFT > vkbd.width || iCol >= nCols) {
        // New subkey doesn't fit in the current row. Start a new row.
        // TODO: currently we don't check that the rows fit vertically,
        // so it's possible that the top or bottom of the subkey menu
        // is not visible.
        iRow++;
        iCol = 0;
        thisRowWidth = SUBKEY_DEFAULT_MARGIN_LEFT;
      }
      const keyGenerator = new OSKSubKey(subKeySpec[i], layer);
      const kDiv = keyGenerator.construct(vkbd, <KeyElement>e, subkeyWidth, iRow > 0);
      thisRowWidth += subkeyWidth + SUBKEY_DEFAULT_MARGIN_LEFT;
      this.menuWidth = Math.max(this.menuWidth ?? 0, thisRowWidth);
      this.subkeys.push(kDiv.firstChild as KeyElement);

      elements.appendChild(kDiv);
    }

    ss.width = this.menuWidth + 'px';

    // And add a filter to fade main keyboard
    this.shim = document.createElement('div');
    this.shim.id = 'kmw-popup-shim';

    // Highlight the duplicated base key or ideal subkey (if a phone)
    if(vkbd.device.formFactor == DeviceSpec.FormFactor.Phone) {
      this.selectDefaultSubkey(e, elements /* == this.element */);
    }

    vkbd.element.appendChild(this.element);
    // The shim should probably fade the banner, too.
    vkbd.topContainer.appendChild(this.shim);

    // Must be placed after its `.element` has been inserted into the DOM.
    this.reposition(vkbd);

    const config = this.buildPopupRecognitionConfig(vkbd);
    configChanger({
      type: 'push',
      config: config
    });
  }

  private buildPopupRecognitionConfig(vkbd: VisualKeyboard): GestureRecognizerConfiguration<KeyElement, string> {
    const baseBounding = this.element.getBoundingClientRect();
    const underlyingKeyBounding = this.baseKey.getBoundingClientRect();

    const subkeyStyle = this.subkeys[0].style;
    const subkeyHeight = Number.parseInt(subkeyStyle.height, 10);
    const basePadding = -0.666 * subkeyHeight;  // extends bounds by the absolute value.
    const topScalar = 3;

    const bottomDistance = underlyingKeyBounding.bottom - baseBounding.bottom;

    const roamBounding = new PaddedZoneSource(this.element, [
      // top
      basePadding * topScalar, // be extra-loose for the top!
      // left, right
      basePadding,
      // bottom: ensure the recognition zone includes the row of the base key.
      // basePadding is already negative, but bottomDistance isn't.
      -bottomDistance < basePadding ? -bottomDistance : basePadding
    ]);

    const sustainBounding: RecognitionZoneSource = {
      getBoundingClientRect() {
        // We don't want to actually use Number.NEGATIVE_INFINITY or Number.POSITIVE_INFINITY
        // because that produces a DOMRect with a few NaN fields, and we don't want _that_.

        // Way larger than any screen resolution should ever be.
        const base = Number.MAX_SAFE_INTEGER;
        return new DOMRect(-base, -base, 2*base, 2*base);
      },
    }

    return {
      targetRoot: this.element,
      inputStartBounds: vkbd.element,
      maxRoamingBounds: sustainBounding,
      safeBounds: sustainBounding, // if embedded, ensure top boundary extends outside the WebView!
      itemIdentifier: (coord, target) => {
        const roamingRect = roamBounding.getBoundingClientRect();

        let bestMatchKey: KeyElement = null;
        let bestYdist = Number.MAX_VALUE;
        let bestXdist = Number.MAX_VALUE;

        // Step 1:  is the coordinate within the range we permit for selecting _anything_?
        if(coord.clientX < roamingRect.left || coord.clientX > roamingRect.right) {
          return null;
        }
        if(coord.clientY < roamingRect.top || coord.clientY > roamingRect.bottom) {
          return null;
        }

        // Step 2:  okay, selection is permitted.  So... what to select?
        for(let key of this.subkeys) {
          const keyBounds = key.getBoundingClientRect();

          let xDist = Number.MAX_VALUE;
          let yDist = Number.MAX_VALUE;

          if(keyBounds.left <= coord.clientX && coord.clientX < keyBounds.right) {
            xDist = 0;
          } else {
            xDist = (keyBounds.left >= coord.clientX) ? keyBounds.left - coord.clientX : coord.clientX - keyBounds.right;
          }

          if(keyBounds.top <= coord.clientY && coord.clientY < keyBounds.bottom) {
            yDist = 0;
          } else {
            yDist = (keyBounds.top >= coord.clientY) ? keyBounds.top - coord.clientY : coord.clientY - keyBounds.bottom;
          }

          if(xDist == 0 && yDist == 0) {
            // Perfect match!
            return key;
          } else if(xDist < bestXdist || (xDist == bestXdist && yDist < bestYdist)) {
            bestXdist = xDist;
            bestMatchKey = key;
            bestYdist = yDist;
          }
        }

        return bestMatchKey;
      }
    }
  }

  reposition(vkbd: VisualKeyboard) {
    let subKeys = this.element;
    let e = this.baseKey;

    // And correct its position with respect to that element
    const _Box = vkbd.topContainer;
    let rowElement = (e.key as OSKBaseKey).row.element;
    let ss=subKeys.style;
    let parentOffsetLeft = e.offsetParent ? (<HTMLElement>e.offsetParent).offsetLeft : 0;
    var x = e.offsetLeft + parentOffsetLeft + 0.5*(e.offsetWidth-subKeys.offsetWidth);
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
    ss.top = (rowElementRect.top - _BoxRect.top - subKeys.offsetHeight - SUBKEY_MENU_VERT_OFFSET) + 'px';

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
    this.callout = this.addCallout(e, delta, vkbd.element, vkbd.topContainer, vkbd.device.formFactor == 'tablet');
  }

  /**
   * Add a callout for popup keys (if KeymanWeb on a phone device)
   *
   * @param   {Object}  key    HTML key element
   * @param   {number}  delta  The pixel offset for the callout from the position it would have if not
   *                           constrained due to WebView boundaries.
   * @return  {Object}         callout object
   */
  addCallout(key: KeyElement, delta: number, host: HTMLElement, _Box: HTMLElement, isTablet: boolean): HTMLDivElement {
    delta = delta || 0;

    // Uses content-box styling, so ignores border aspects for reported positions.
    /**
     * "Computed Menu Style"
     */
    const cms = getComputedStyle(this.element);
    const borderRadius = Math.max(Number.parseInt(cms.borderRadius), 0);

    // Create the callout
    let keyRect = key.getBoundingClientRect();
    let _BoxRect = _Box.getBoundingClientRect();

    // Set position and style
    // We're going to adjust the top of the box to ensure it stays
    // pixel aligned, otherwise we can get antialiasing artifacts
    // that look ugly
    let calloutTop = Math.floor(
      Number.parseInt(cms.top, 10) +
      Number.parseInt(cms.height, 10) +
      // Padding is not included in content-box (or in content-box's top positioning)...
      // but half the padding-top seems useful on all tested devices.
      // Not sure exactly why.
      Number.parseInt(cms.paddingTop, 10)/2 +
      Number.parseInt(cms.paddingBottom, 10)
    );

    const calloutProportionalHeight = CALLOUT_ROW_HEIGHT_RATIO * (keyRect.height - delta);
    const maxProportionalHeight = CALLOUT_ROW_HEIGHT_RATIO * keyRect.height;

    const targetHeight = calloutProportionalHeight + CALLOUT_BASE_HEIGHT;
    const calloutDownscaleRatio = targetHeight / (maxProportionalHeight + CALLOUT_BASE_HEIGHT)

    // Shorten the callout if the subkey menu is being constrained within WebView bounds, thus
    // overlapping the base key.  Do so (mostly) proportionally to how much is obscured.
    const maxHeight = (keyRect.bottom - _BoxRect.top) - calloutTop - 1;
    const selectedHeight = maxHeight < targetHeight ? maxHeight : targetHeight;

    if(selectedHeight > 0) {
      const cc = document.createElement('div');
      const ccs = cc.style;
      cc.id = 'kmw-popup-callout';
      host.appendChild(cc);

      ccs.top = calloutTop + 'px';
      ccs.borderTopWidth = (selectedHeight) + 'px';

      const calloutKeyWidthRatio = MAX_CALLOUT_KEY_WIDTH * calloutDownscaleRatio;

      const desiredCalloutWidth = keyRect.width * calloutKeyWidthRatio;
      const maxCalloutWidth = this.menuWidth - 2 * borderRadius;
      const targetCalloutWidth = maxCalloutWidth < desiredCalloutWidth ? maxCalloutWidth : desiredCalloutWidth;

      const calloutLeftOffset = (keyRect.left - _BoxRect.left - (targetCalloutWidth - keyRect.width)/2);

      // Avoid letting the callout overrun screen bounds or overshooting the transition to curved borders
      const calloutRightOverrun = Math.max(0, (calloutLeftOffset + targetCalloutWidth) - (_BoxRect.right - borderRadius));
      const calloutLeftOverrun =  Math.max(0, borderRadius-calloutLeftOffset);

      ccs.left = (keyRect.left - _BoxRect.left - (targetCalloutWidth - keyRect.width)/2 + calloutLeftOverrun) /*- ADJUSTMENT*/ + 'px';
      ccs.borderLeftWidth  = targetCalloutWidth/2 - calloutLeftOverrun  + 'px';
      ccs.borderRightWidth = targetCalloutWidth/2 - calloutRightOverrun + 'px';

      // Return callout element, to allow removal later
      return cc;
    } else {
      return null;
    }
  }

  selectDefaultSubkey(baseKey: KeyElement, popupBase: HTMLElement) {
    var bk: KeyElement;
    let subkeys = baseKey['subKeys'];
    for(let i=0; i < subkeys.length; i++) {
      let skSpec = subkeys[i];
      let skElement = <KeyElement> popupBase.childNodes[i].firstChild;

      // Preference order:
      // #1:  if a default subkey has been specified, select it.
      // #2:  if no default subkey is specified, default to a subkey with the same
      //      key ID and layer / modifier spec.
      if(skSpec.default) {
       bk = skElement;
       break;
      } else if(!baseKey.key || !baseKey.key.spec) {
        continue;
      }

      if(skSpec.elementID == baseKey.key.spec.elementID) {
        bk = skElement;
      }
    }

    if(bk) {
      this.currentSelection?.key.highlight(false);
      this.currentSelection = bk;

      // Subkeys never get key previews, so we can directly highlight the subkey.
      bk.key.highlight(true);
    }
  }

  get hasModalVisualization() {
    return this.element.style.visibility == 'visible';
  }

  buildCorrectiveLayout(): CorrectionLayout {
    const baseBounding = this.element.getBoundingClientRect();
    const aspectRatio = baseBounding.width / baseBounding.height;

    const keys = this.subkeys.map((keyElement) => {
      const subkeyBounds = keyElement.getBoundingClientRect();

      // Ensures we have the right typing.
      const correctiveData: CorrectionLayoutEntry = {
        keySpec: keyElement.key.spec,
        centerX: ((subkeyBounds.right - subkeyBounds.width / 2) - baseBounding.left) / baseBounding.width,
        centerY: ((subkeyBounds.bottom - subkeyBounds.height / 2) - baseBounding.top) / baseBounding.height,
        width: subkeyBounds.width / baseBounding.width,
        height: subkeyBounds.height / baseBounding.height
      }

      return correctiveData;
    });

    return {
      keys: keys,
      kbdScaleRatio: aspectRatio
    }
  }

  currentStageKeyDistribution(): KeyDistribution {
    const latestStage = this.source.stageReports[this.source.stageReports.length-1];
    const baseStage = this.source.stageReports[0];
    const gestureSource = latestStage.sources[0];
    const lastCoord = gestureSource.currentSample;

    const baseBounding = this.element.getBoundingClientRect();
    const mappedCoord = {
      x: lastCoord.targetX / baseBounding.width,
      y: lastCoord.targetY / baseBounding.height
    }

    // Lock the coordinate within base-element bounds; corrects for the allowed 'popup roaming' zone.
    //
    // To consider:  add a 'clipping' feature to `keyTouchDistances`?  It could make sense for base
    // keys, too - especially when emulating a touch OSK via the inline-OSK mode used in the
    // Developer host page.
    mappedCoord.x = mappedCoord.x < 0 ? 0 : (mappedCoord.x > 1 ? 1: mappedCoord.x);
    mappedCoord.y = mappedCoord.y < 0 ? 0 : (mappedCoord.y > 1 ? 1: mappedCoord.y);

    const rawSqDistances = keyTouchDistances(mappedCoord, this.buildCorrectiveLayout());
    const currentKeyDist = rawSqDistances.get(lastCoord.item.key.spec);

    /*
     * - how long has the subkey menu been visible?
     *   - Base key should be less likely if it's been visible a while,
     *     but reasonably likely if it only just appeared.
     *     - Especially if up-flicks are allowed.  Though, in that case, consider
     *       base-layer neighbors, and particularly the one directly under the touchpoint?
     * - raw distance traveled (since the menu appeared)
     *   - similarly, short distance = a more likely base key?
     */

    // The concept:  how likely is it that the user MEANT to output a subkey?
    let timeDistance = Math.min(
      // The full path is included by the model - meaning the base wait is included here in
      // in the stats;  we subtract it to get just the duration of the subkey menu.
      gestureSource.path.stats.duration - baseStage.sources[0].path.stats.duration,
      this.gestureParams.longpress.waitLength
      ) / (2 * this.gestureParams.longpress.waitLength);  // normalize:  max time distance of 0.5

    let pathDistance = Math.min(
      gestureSource.path.stats.rawDistance,
      this.gestureParams.longpress.noiseTolerance*4
    ) / (this.gestureParams.longpress.noiseTolerance * 8); // normalize similarly.

    // We only want to add a single distance 'dimension' - we'll choose the one that affects
    // the interpreted distance the least.  (This matters for upflick-shortcutting in particular)
    const layerDistance = Math.min(timeDistance * timeDistance, pathDistance * pathDistance);
    const baseKeyDistance = currentKeyDist + layerDistance;

    // Include the base key as a corrective option.
    const baseKeyMap = new Map<ActiveKeyBase, number>();
    const subkeyMatch = this.subkeys.find((entry) => entry.keyId == this.baseKey.keyId);
    if(subkeyMatch) {
      // Ensure that the base key's entry can be merged with that of its subkey.
      // (Assuming that always makes sense.)
      baseKeyMap.set(subkeyMatch.key.spec, baseKeyDistance);
    } else {
      baseKeyMap.set(this.baseKey.key.spec, baseKeyDistance);
    }

    return distributionFromDistanceMaps([rawSqDistances, baseKeyMap]);
  }

  cancel() {
    this.clear();
    this.source.cancel();
  }

  clear() {
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
}
