import { DeviceSpec } from '@keymanapp/web-utils';
import type { PredictionContext, StateChangeEnum } from '@keymanapp/input-processor';
import { ImageBanner } from './imageBanner.js';
import { SuggestionBanner } from './suggestionBanner.js';
import { BannerView, BannerOptions, BannerType } from './bannerView.js';
import { Banner } from './banner.js';
import { BlankBanner } from './blankBanner.js';
import { HTMLBanner } from './htmlBanner.js';

export class BannerController {
  private container: BannerView;

  private predictionContext?: PredictionContext;

  private readonly hostDevice: DeviceSpec;

  private _inactiveBanner: Banner;

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

  public get inactiveBanner() {
    return this._inactiveBanner;
  }

  public set inactiveBanner(banner: Banner) {
    if(!banner) {
      banner = new BlankBanner();
    }
    this._inactiveBanner = banner;

    if(!(this.container.banner instanceof SuggestionBanner)) {
      this.container.banner = banner;
    }
  }

  /**
   * Sets the active `Banner` to match the specified state for predictive text.
   *
   * @param on   Whether prediction is active (`true`) or disabled (`false`).
   */
  public activateBanner(on: boolean) {
    let banner: Banner;

    const oldBanner = this.container.banner;
    if(oldBanner instanceof SuggestionBanner) {
      this.predictionContext.off('update', oldBanner.onSuggestionUpdate);
    }

    if(!on) {
      this.container.banner = this.inactiveBanner;
    } else {
      let suggestBanner = banner = new SuggestionBanner(this.hostDevice, this.container.activeBannerHeight);
      suggestBanner.predictionContext = this.predictionContext;
      suggestBanner.events.on('apply', (selection) => this.predictionContext.accept(selection.suggestion));

      this.predictionContext.on('update', suggestBanner.onSuggestionUpdate);
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
  }
}