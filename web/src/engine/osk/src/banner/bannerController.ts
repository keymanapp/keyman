import { DeviceSpec } from '@keymanapp/web-utils';
import type { PredictionContext, StateChangeEnum } from 'keyman/engine/interfaces';
import { ImageBanner } from './imageBanner.js';
import { SuggestionBanner } from './suggestionBanner.js';
import { BannerView } from './bannerView.js';
import { Banner } from './banner.js';
import { BlankBanner } from './blankBanner.js';
import { HTMLBanner } from './htmlBanner.js';
import { JSKeyboard, KeyboardProperties } from 'keyman/engine/keyboard';

export class BannerController {
  private container: BannerView;

  private predictionContext?: PredictionContext;

  private readonly hostDevice: DeviceSpec;

  private _inactiveBanner: Banner;

  private keyboard: JSKeyboard;
  private keyboardStub: KeyboardProperties;

  /**
   * Builds a banner for use when predictions are not active, supporting a single image.
   */
  public readonly ImageBanner = ImageBanner;

  /**
   * Builds a banner for use when predictions are not active, supporting a more generalized
   * content pattern than ImageBanner via `innerHTML` specifications.
   */
  public readonly HTMLBanner = HTMLBanner;

  constructor(bannerView: BannerView, hostDevice: DeviceSpec, predictionContext?: PredictionContext) {
    // Step 1 - establish the container element.  Must come before this.setOptions.
    this.hostDevice = hostDevice;
    this.container = bannerView;
    this.predictionContext = predictionContext;

    this.inactiveBanner = new BlankBanner();
  }

  /**
   * Specifies the `Banner` instance to use when predictive-text is _not_ available to the user.
   *
   * Defaults to a hidden, "blank" `Banner` if not otherwise specified.  Changes to its value
   * when predictive-text is not active will result in banner hot-swapping.
   *
   * The assigned instance will persist until directly changed through a new assignment,
   * regardless of any keyboard swaps and/or activations of the suggestion banner that may
   * occur in the meantime.
   */
  public get inactiveBanner() {
    return this._inactiveBanner;
  }

  public set inactiveBanner(banner: Banner) {
    this._inactiveBanner = banner ?? new BlankBanner();

    if(!(this.container.banner instanceof SuggestionBanner)) {
      this.container.banner = this._inactiveBanner;
    }
  }

  /**
   * Sets the active `Banner` to match the specified state for predictive text.
   *
   * @param on   Whether prediction is active (`true`) or disabled (`false`).
   */
  public activateBanner(on: boolean) {
    const oldBanner = this.container.banner;
    if(oldBanner instanceof SuggestionBanner) {
      // Frees all handlers, etc registered previously by the banner.
      oldBanner.predictionContext = null;
    }

    if(!on) {
      this.container.banner = this.inactiveBanner;
    } else {
      let suggestBanner = new SuggestionBanner(this.hostDevice, this.container.activeBannerHeight);
      suggestBanner.predictionContext = this.predictionContext;

      // Registers for prediction-engine events & handles its needed connections.
      this.container.banner = suggestBanner;
    }
  }

  /**
   * Handles `LanguageProcessor`'s `'statechange'` events,
   * allowing logic to automatically hot-swap `Banner`s as needed.
   * @param state
   */
  selectBanner(state: StateChangeEnum) {
    // Only display a SuggestionBanner when LanguageProcessor states it is active.
    this.activateBanner(state == 'active' || state == 'configured');

    if(this.keyboard) {
      this.container.banner.configureForKeyboard(this.keyboard, this.keyboardStub);
    }
  }

  /**
   * Allows banners to adapt based on the active keyboard and related properties, such as
   * associated fonts.
   * @param keyboard
   * @param keyboardProperties
   */
  public configureForKeyboard(keyboard: JSKeyboard, keyboardProperties: KeyboardProperties) {
    this.keyboard = keyboard;
    this.keyboardStub = keyboardProperties;

    this.container.banner.configureForKeyboard(keyboard, keyboardProperties);
  }

  public shutdown() {
    if(this.container.banner instanceof SuggestionBanner) {
      this.container.banner.predictionContext = null;
    }
  }
}