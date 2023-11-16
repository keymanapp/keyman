import EventEmitter from 'eventemitter3';

import { createUnselectableElement } from 'keyman/engine/dom-utils';

import { Banner } from './banner.js';
import OSKViewComponent from '../components/oskViewComponent.interface.js';
import { ParsedLengthStyle } from '../lengthStyle.js';
import { BlankBanner } from './blankBanner.js';

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
 *   * The option `persistentBanner == true` is designed to replicate current
 *     iOS system keyboard behavior.
 *     * When true, an `ImageBanner` will be displayed.
 *     * If false, it will be replaced with a `BlankBanner` of zero height,
 *       corresponding to our current default lack of banner.
 *   * It will not automatically set `persistentBanner == true`;
 *     this must be set by the iOS app, and only under the following conditions:
 *     * `keyman.isEmbedded == true`
 *     * `device.OS == 'ios'`
 *     * Keyman is being used as the system keyboard within an app that
 *       needs to reserve this space (i.e: Keyman for iOS),
 *       rather than as its standalone app.
 */
export class BannerView implements OSKViewComponent {
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