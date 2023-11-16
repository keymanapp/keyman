import { DeviceSpec } from '@keymanapp/web-utils';
import type { PredictionContext, StateChangeEnum } from '@keymanapp/input-processor';
import { ImageBanner } from './imageBanner.js';
import { SuggestionBanner } from './suggestionBanner.js';
import { BannerView, BannerOptions, BannerType } from './bannerView.js';
import { Banner } from './banner.js';
import { BlankBanner } from './blankBanner.js';

export class BannerController {
  private _activeType: BannerType;
  private _options: BannerOptions = {};
  private container: BannerView;
  private alwaysShow: boolean;
  private imagePath?: string = "";

  private predictionContext?: PredictionContext;

  private readonly hostDevice: DeviceSpec;

  public static readonly DEFAULT_OPTIONS: BannerOptions = {
    alwaysShow: false,
    imagePath: ""
  }

  constructor(bannerView: BannerView, hostDevice: DeviceSpec, predictionContext?: PredictionContext) {
    // Step 1 - establish the container element.  Must come before this.setOptions.
    this.hostDevice = hostDevice;
    this.container = bannerView;
    this.predictionContext = predictionContext;

    // Initialize with the default options - any 'manually set' options come post-construction.
    // This will also automatically set the default banner in place.
    this.setOptions(BannerController.DEFAULT_OPTIONS);
  }

  /**
   * This function corresponds to `keyman.osk.banner.getOptions`.
   *
   * Gets the current control settings in use by `BannerManager`.
   */
  public getOptions(): BannerOptions {
    let retObj = {};

    for(let key in this._options) {
      retObj[key] = this._options[key];
    }

    return retObj;
  }

  /**
   * This function corresponds to `keyman.osk.banner.setOptions`.
   *
   * Sets options used to tweak the automatic `Banner`
   * control logic used by `BannerManager`.
   * @param optionSpec An object specifying one or more of the following options:
   * * `persistentBanner` (boolean) When `true`, ensures that a `Banner`
   *   is always displayed, even when no predictive model exists
   *   for the active language.
   *
   *   Default: `false`
   * * `imagePath` (URL string) Specifies the file path to use for an
   *   `ImageBanner` when `persistentBanner` is `true` and no predictive model exists.
   *
   *   Default: `''`.
   * * `enablePredictions` (boolean) Turns KMW predictions
   *   on (when `true`) and off (when `false`).
   *
   *   Default:  `true`.
   */
  public setOptions(optionSpec: BannerOptions) {
    for(let key in optionSpec) {
      switch(key) {
        // Each defined option may require specialized handling.
        case 'alwaysShow':
          // Determines the banner type to activate.
          this.alwaysShow = optionSpec[key];
          break;
        case 'imagePath':
          // Determines the image file to use for ImageBanners.
          this.imagePath = optionSpec[key];
          break;
        default:
          // Invalid option specified!
      }
      this._options[key] = optionSpec[key];

      // If no banner instance exists yet, go with a safe, blank initialization.
      if(!this.container.banner) {
        this.selectBanner('inactive');
      }
    }
  }

  /**
   * Sets the active `Banner` to the specified type, regardless of
   * existing management logic settings.
   *
   * @param type `'blank' | 'image' | 'suggestion'` - A plain-text string
   *        representing the type of `Banner` to set active.
   * @param height - Optional banner height in pixels.
   */
  public setBanner(type: BannerType) {
    var banner: Banner;

    let oldBanner = this.container.banner;
    if(oldBanner instanceof SuggestionBanner) {
      this.predictionContext.off('update', oldBanner.onSuggestionUpdate);
    }

    switch(type) {
      case 'blank':
        banner = new BlankBanner();
        break;
      case 'image':
        banner = new ImageBanner(this.imagePath, this.container.activeBannerHeight);
        break;
      case 'suggestion':
        let suggestBanner = banner = new SuggestionBanner(this.hostDevice, this.container.activeBannerHeight);
        suggestBanner.predictionContext = this.predictionContext;
        suggestBanner.events.on('apply', (selection) => this.predictionContext.accept(selection.suggestion));

        this.predictionContext.on('update', suggestBanner.onSuggestionUpdate);
        break;
      default:
        throw new Error("Invalid type specified for the banner!");
    }

    this._activeType = type;

    if(banner) {
      this.container.banner = banner;
    }
  }

  /**
   * Handles `LanguageProcessor`'s `'statechange'` events,
   * allowing logic to automatically hot-swap `Banner`s as needed.
   * @param state
   */
  selectBanner(state: StateChangeEnum) {
    // Only display a SuggestionBanner when LanguageProcessor states it is active.
    if(state == 'active' || state == 'configured') {
      this.setBanner('suggestion');
    } else if(state == 'inactive') {
      if(this.alwaysShow) {
        this.setBanner('image');
      } else {
        this.setBanner('blank');
      }
    }
  }

  public get activeType(): BannerType {
    return this._activeType;
  }
}