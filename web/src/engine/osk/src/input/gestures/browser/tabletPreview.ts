import { KeyElement } from '../../../keyElement.js';
import KeyTipInterface from '../../../keytip.interface.js';
import VisualKeyboard from '../../../visualKeyboard.js';
import { GesturePreviewHost } from '../../../keyboard-layout/gesturePreviewHost.js';

const BASE_CLASS = 'kmw-keypreview';
const OVERLAY_CLASS = 'kmw-preview-overlay';
const BASE_ID = 'kmw-keytip';

export class TabletKeyTip implements KeyTipInterface {
  public readonly element: HTMLDivElement;
  public key: KeyElement;
  public state: boolean = false;

  private previewHost: GesturePreviewHost;
  private preview: HTMLDivElement;
  private readonly vkbd: VisualKeyboard;

  /**
   * Pre-builds a reusable element to serve as a gesture-preview host for selected keys
   * on tablet-form factor devices.  This element hovers over the keyboard, staying in
   * place (and on top) even when the layer changes.
   */
  constructor(vkbd: VisualKeyboard) {
    this.vkbd = vkbd;
    const base = this.element=document.createElement('div');
    base.className=BASE_CLASS;
    base.id = 'kmw-keytip';

    // The following style is critical, so do not rely on external CSS
    base.style.pointerEvents='none';
    base.style.display='none';

    this.preview = document.createElement('div');
    base.appendChild(this.preview);
  }

  show(key: KeyElement, on: boolean, previewHost: GesturePreviewHost) {
    const vkbd = this.vkbd;
    const keyLayer = key?.key.spec.displayLayer;

    // During quick input sequences - especially during a multitap-modipress - it's possible
    // for a user to request a preview for a key from a layer that is currently active, but
    // currently not visible due to need previously-requested layout calcs for a different layer.
    if(on) {
      // Necessary for `key.offsetParent` and client-rect methods referenced below.
      // Will not unnecessarily force reflow if the layer is already in proper document flow,
      // but otherwise restores it.
      vkbd.layerGroup.blinkLayer(keyLayer);
    }

    // Create and display the preview
    // If !key.offsetParent, the OSK is probably hidden.  Either way, it's a half-
    // decent null-guard check.
    if(on && key?.offsetParent) {
      // May need adjustment for borders if ever enabled for the desktop form-factor target.

      // Note:  this.vkbd does not set relative or absolute positioning.  Nearest positioned
      // ancestor = the OSKView's _Box, accessible as this.vkbd.topContainer.
      const hostRect = this.vkbd.topContainer.getBoundingClientRect();
      const keyRect = key.getBoundingClientRect();

      // Used to apply box-shadow overlay styling when the preview is for a key on a layer not
      // currently active.  This is done in case the layers don't have perfect alignment for
      // all keys.
      const conditionalOverlayStyle = (keyLayer != vkbd.layerId) ? OVERLAY_CLASS : '';
      this.element.className = `${BASE_CLASS} ${key.className} ${conditionalOverlayStyle}`;

      // Some keyboards use custom CSS styling based on partial-matching the key ID
      // (like sil_cameroon_azerty); this lets us map the custom styles onto the tablet
      // preview, too.
      this.element.id = `${BASE_ID}-${key.id}`;

      const kts = this.element.style;

      // Some keyboards (such as `balochi_scientific`) do not _package_ a font but
      // specify an extremely common one, such as Arial.  In such cases, .kmw-key-text
      // custom styling doesn't exist, relying on the layer object to simply specify
      // the font-family.
      const fontFamily = this.vkbd.currentLayer.element.style.fontFamily;
      kts.fontFamily = key.key.spec.font || fontFamily;

      kts.left = (keyRect.left - hostRect.left) + 'px';
      kts.top = (keyRect.top - hostRect.top) + 'px';
      kts.width = keyRect.width + 'px';
      kts.height = keyRect.height + 'px';

      this.element.style.display = 'block';

      if(this.previewHost == previewHost) {
        return;
      }

      const oldHost = this.preview;
      this.previewHost = previewHost;

      if(previewHost) {
        this.preview = this.previewHost.element;
        this.element.replaceChild(this.preview, oldHost);
        previewHost.setCancellationHandler(() => this.show(null, false, null));
        previewHost.on('startFade', () => {
          this.element.classList.remove('kmw-preview-fade');
          // Note:  a reflow is needed to reset the transition animation.
          this.element.offsetWidth;
          this.element.classList.add('kmw-preview-fade');
        });
      }
    } else { // Hide the key preview
      this.element.style.display = 'none';
      this.element.className = BASE_CLASS;

      this.previewHost = null;
      const oldPreview = this.preview;
      this.preview = document.createElement('div');
      this.element.replaceChild(this.preview, oldPreview);
      this.element.classList.remove('kmw-preview-fade');
    }

    // Save the key preview state
    this.key = key;
    this.state = on;
  }
}