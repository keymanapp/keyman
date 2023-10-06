import EventEmitter from 'eventemitter3';

import { Banner, BlankBanner, ImageBanner, SuggestionBanner } from './banner.js';
import OSKViewComponent from '../components/oskViewComponent.interface.js';
import { ParsedLengthStyle } from '../lengthStyle.js';

import { DeviceSpec } from '@keymanapp/web-utils';
import type { PredictionContext, StateChangeEnum } from '@keymanapp/input-processor';
import { createUnselectableElement } from 'keyman/engine/dom-utils';

/**
 * This object is used to specify options by both `BannerManager.getOptions`
 * and `BannerManager.setOptions`.  Refer to the latter for specification of
 * each field.
 */
export interface BannerOptions {
  alwaysShow?: boolean;
  imagePath?: string;
}

export type BannerType = "blank" | "image" | "suggestion";

interface BannerViewEventMap {
  'bannerchange': () => void;
}

/**
 * The `BannerManager` module is designed to serve as a manager for the
 * different `Banner` types.
 * To facilitate this, it will provide a root element property that serves
 * as a container for any active `Banner`, helping KMW to avoid needless
 * DOM element shuffling.
 *
 * Goals for the `BannerManager`:
 *
 * * It will be exposed as `keyman.osk.banner` and will provide the following API:
 *   * `getOptions`, `setOptions` - refer to the `BannerOptions` class for details.
 *   * This provides a persistent point that the web page designers and our
 *     model apps can utilize and can communicate with.
 *   * These API functions are designed for live use and will allow
 *     _hot-swapping_ the `Banner` instance; they're not initialization-only.
 * * Disabling the `Banner` (even for suggestions) outright with
 *   `enablePredictions == false` will auto-unload any loaded predictive model
 *   from `ModelManager` and setting it to `true` will revert this.
 *   * This should help to avoid wasting computational resources.
 * * It will listen to ModelManager events and automatically swap Banner
 *   instances as appropriate:
 *   * The option `alwaysShow == true` is designed to replicate current
 *     iOS system keyboard behavior.
 *     * When true, an `ImageBanner` will be displayed.
 *     * If false, it will be replaced with a `BlankBanner` of zero height,
 *       corresponding to our current default lack of banner.
 *   * It will not automatically set `alwaysShow == true`;
 *     this must be set by the iOS app, and only under the following conditions:
 *     * `keyman.isEmbedded == true`
 *     * `device.OS == 'ios'`
 *     * Keyman is being used as the system keyboard within an app that
 *       needs to reserve this space (i.e: Keyman for iOS),
 *       rather than as its standalone app.
 */
export default class BannerView implements OSKViewComponent {
  private bannerContainer: HTMLDivElement;
  private activeBanner: Banner;
  private _activeBannerHeight: number = Banner.DEFAULT_HEIGHT;

  public readonly events = new EventEmitter<BannerViewEventMap>();

  constructor() {
    // Step 1 - establish the container element.  Must come before this.setOptions.
    this.constructContainer();
  }

  /**
   * Constructs the <div> element used to contain hot-swapped `Banner` instances.
   */
  private constructContainer(): HTMLDivElement {
    let d = createUnselectableElement('div');
    d.id = "keymanweb_banner_container";
    d.className = "kmw-banner-container";
    return this.bannerContainer = d;
  }

  /**
   * Returns the `Banner`-containing div element used to facilitate hot-swapping.
   */
  public get element(): HTMLDivElement {
    return this.bannerContainer;
  }

  /**
   * Applies any stylesheets needed by specific `Banner` instances.
   */
  public appendStyles() {
    if(this.activeBanner) {
      this.activeBanner.appendStyleSheet();
    }
  }

  public get banner(): Banner {
    return this.activeBanner;
  }

  /**
   * Sets the active `Banner` to the specified type, regardless of
   * existing management logic settings.
   *
   * @param banner The `Banner` instance to set as active.
   */
  public set banner(banner: Banner) {
    if(this.activeBanner) {
      if(banner == this.activeBanner) {
        return;
      } else {
        let prevBanner = this.activeBanner;
        this.activeBanner = banner;
        this.bannerContainer.replaceChild(banner.getDiv(), prevBanner.getDiv());
      }
    } else {
      this.activeBanner = banner;
      if(banner) {
        this.bannerContainer.appendChild(banner.getDiv());
      }
    }

    if(!(banner instanceof BlankBanner)) {
      banner.height = this.activeBannerHeight;
    }

    this.events.emit('bannerchange');
  }

  /**
   * Gets the height (in pixels) of the active `Banner` instance.
   */
  public get height(): number {
    if(this.activeBanner) {
      return this.activeBanner.height;
    } else {
      return 0;
    }
  }

  public get activeBannerHeight(): number {
    return this._activeBannerHeight;
  }

  /**
   * Sets the height (in pixels) of the active 'Banner' instance.
   */
  public set activeBannerHeight(h: number) {
    this._activeBannerHeight = h;

    if (this.activeBanner && !(this.activeBanner instanceof BlankBanner)) {
      this.activeBanner.height = h;
    }
  }

  public get layoutHeight(): ParsedLengthStyle {
    return ParsedLengthStyle.inPixels(this.height);
  }

  public refreshLayout() {};
}

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

  // Default to black image banner for Android
  public static readonly DEFAULT_ANDROID_OPTIONS: BannerOptions = {
    alwaysShow: true,
    imagePath: "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABoAAAARCAIAAABM7ytaAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAAEnQAABJ0Ad5mH3gAAAAVSURBVDhPYxgFo2AUjIJRQDpgYAAABT8AAcEGbxwAAAAASUVORK5CYII="
  }

  constructor(bannerView: BannerView, hostDevice: DeviceSpec, predictionContext?: PredictionContext) {
    // Step 1 - establish the container element.  Must come before this.setOptions.
    this.hostDevice = hostDevice;
    this.container = bannerView;
    this.predictionContext = predictionContext;

    // Initialize with the default options - any 'manually set' options come post-construction.
    // This will also automatically set the default banner in place.
    this.setOptions(
      this.hostDevice.OS != DeviceSpec.OperatingSystem.Android ? 
      BannerController.DEFAULT_OPTIONS : BannerController.DEFAULT_ANDROID_OPTIONS);
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
   * * `alwaysShow` (boolean) When `true`, ensures that a `Banner`
   *   is always displayed, even when no predictive model exists
   *   for the active language.
   *
   *   Default: `false`
   * * `imagePath` (URL string) Specifies the file path to use for an
   *   `ImageBanner` when `alwaysShow` is `true` and no predictive model exists.
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
          if(this.container.banner instanceof ImageBanner) {
            this.container.banner.setImagePath(optionSpec[key]);
          }
          break;
        default:
          // Invalid option specified!
      }
      this._options[key] = optionSpec[key];
    }

    // Move out of for loop to handle standup
    // If no banner instance exists yet, go with a safe, blank initialization.
    if(!this.container.banner) {
      this.selectBanner('inactive');
    }
  }

  /**
   * Sets the active `Banner` to the specified type, regardless of
   * existing management logic settings.
   *
   * @param type `'blank' | 'image' | 'suggestion'` - A plain-text string
   *        representing the type of `Banner` to set active.
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