import { ActiveKey } from "@keymanapp/keyboard-processor";
import { KeyElement } from "../keyElement.js";
import { FlickNameCoordMap, OrderedFlickDirections } from "../input/gestures/browser/flick.js";

const FLICK_DIRS = ['n', 'ne', 'e', 'se', 's', 'sw', 'w', 'nw'] as const;



/**With edge lengths of 1, to keep flick-text invisible at the start, the
 * hypotenuse for an inter-cardinal path is sqrt(2).  To keep a perfect circle
 * for all flicks, then, requires the straight-edge length for pure cardinal
 * paths to match - sqrt(2).
 */
const FLICK_OVERFLOW_OFFSET = 1.4142;

export class GesturePreviewHost {
  private readonly div: HTMLDivElement;
  private readonly label: HTMLSpanElement;
  private readonly previewImgContainer: HTMLDivElement;

  private flickPreviews = new Map<string, HTMLDivElement>;
  private hintLabel: HTMLDivElement = null;
  private flickEdgeLength: number;

  private onCancel: () => void;

  get element(): HTMLDivElement {
    return this.div;
  }

  constructor(key: KeyElement, isPhone: boolean, width: number, height: number) {
    const keySpec = key.key.spec;
    const edgeLength = this.flickEdgeLength = Math.max(width, height);

    const base = this.div = document.createElement('div');
    base.className = base.id = 'kmw-gesture-preview';

    base.style.pointerEvents='none';

    // We want this to be distinct from the base element so that we can scroll it;
    // this matters greatly for doing flick things.
    const previewImgContainer = this.previewImgContainer = document.createElement('div');
    this.previewImgContainer.id = 'kmw-preview-img-container';

    const label = this.label = document.createElement('span');
    label.className='kmw-gesture-base-label kmw-key-text';
    label.id = 'kmw-gesture-base-label';
    previewImgContainer.appendChild(label);

    // Re-use the text value from the base key's label.
    label.textContent = key.key.label.textContent;

    this.div.appendChild(this.previewImgContainer);

    if(keySpec.flick) {
      const flickSpec = keySpec.flick || {};

      Object.keys(flickSpec).forEach((dir: typeof OrderedFlickDirections[number]) => {
        const flickPreview = document.createElement('div');
        flickPreview.className = 'kmw-flick-preview kmw-key-text';
        flickPreview.textContent = flickSpec[dir].text;

        const ps /* preview style */ = flickPreview.style;

        // is in polar coords, origin toward north, clockwise.
        const coords = FlickNameCoordMap.get(dir);
        const x = -Math.sin(coords[0]); // Put 'e' flick at left
        const y =  Math.cos(coords[0]); // Put 'n' flick at bottom

        ps.width = '100%';
        ps.textAlign = 'center';

        if(x < 0) {
          ps.right = (-x * FLICK_OVERFLOW_OFFSET * edgeLength) + 'px';
        } else if(x > 0) {
          ps.left  = ( x * FLICK_OVERFLOW_OFFSET * edgeLength) + 'px';
        } else {
          ps.left = '0px';
        }

        ps.height = '100%';
        ps.lineHeight = '100%';
        if(y < 0) {
          ps.bottom = (-y * FLICK_OVERFLOW_OFFSET * edgeLength) + 'px';
        } else if(y > 0) {
          ps.top    = ( y * FLICK_OVERFLOW_OFFSET * edgeLength) + 'px';
        } else {
          ps.top = '0px';
        }

        this.flickPreviews.set(dir, flickPreview);
        previewImgContainer.appendChild(flickPreview);
      });
    }

    // const hintLabel = this.hintLabel = document.createElement('div');
    // hintLabel.className='kmw-key-popup-icon';
    // hintLabel.textContent = keySpec == keySpec.hintSrc ? keySpec.hint : keySpec.hintSrc?.text;
    // hintLabel.style.fontWeight= hintLabel.textContent == '\u2022' ? 'bold' : '';

    // // Default positioning puts it far too close to the flick-preview bit.
    // let yAdjustment = 0;
    // hintLabel.style.marginTop = `-${yAdjustment}px`;

    // // b/c multitap's border forces position shifting
    // let xAdjustment = 0;
    // hintLabel.style.marginRight = `-${xAdjustment}px`;

    // base.appendChild(hintLabel);
  }

  public refreshLayout() {
    const compStyle = getComputedStyle(this.div);
    const height = Number.parseInt(compStyle.height, 10);

    this.flickPreviews.forEach((ele) => {
      ele.style.lineHeight = ele.style.height = `${height}px`;
    });
  }

  public cancel() {
    this.onCancel?.();
    this.onCancel = null;
  }

  public setCancellationHandler(handler: () => void) {
    this.onCancel = handler;
  }

  public scrollFlickPreview(x: number, y: number) {
    const scrollStyle = this.previewImgContainer.style;
    const edge = this.flickEdgeLength * FLICK_OVERFLOW_OFFSET;

    scrollStyle.marginLeft = `${edge * x}px`;
    scrollStyle.marginTop =  `${edge * y}px`;
  }

  // These may not exist like this longterm.
  public clearFlick() {
    this.previewImgContainer.style.marginTop = '0px';
    this.previewImgContainer.style.marginLeft = '0px';

    this.previewImgContainer.classList.add('flick-clear');
  }

  private clearHint() {
    this.hintLabel?.classList.add('hint-clear');
  }

  public clearAll() {
    this.clearFlick();
    this.clearHint();
  }
}