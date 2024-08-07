import { EventEmitter } from 'eventemitter3';

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

export type BannerType = "blank" | "image" | "suggestion" | "html";

interface BannerViewEventMap {
  'bannerchange': () => void;
}

/**
 * The `BannerView` module is designed to serve as the hot-swap container for the
 * different `Banner` types, helping KMW to avoid needless DOM element shuffling.
 */
export class BannerView implements OSKViewComponent {
  private bannerContainer: HTMLDivElement;

  /**
   * The currently active banner.
   */
  private currentBanner: Banner;
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
    if(this.currentBanner) {
      this.currentBanner.appendStyleSheet();
    }
  }

  public get banner(): Banner {
    return this.currentBanner;
  }

  /**
   * The `Banner` actively being displayed to the user in the OSK's current state,
   * whether a `SuggestionBanner` (with predictive-text active) or a different
   * type for use when the predictive-text engine is inactive.
   */
  public set banner(banner: Banner) {
    if(this.currentBanner) {
      if(banner == this.currentBanner) {
        return;
      } else {
        let prevBanner = this.currentBanner;
        this.currentBanner = banner;
        this.bannerContainer.replaceChild(banner.getDiv(), prevBanner.getDiv());
        prevBanner.shutdown();
      }
    } else {
      this.currentBanner = banner;
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
    if(this.currentBanner) {
      return this.currentBanner.height;
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

    if (this.currentBanner && !(this.currentBanner instanceof BlankBanner)) {
      this.currentBanner.height = h;
    }
  }

  public get layoutHeight(): ParsedLengthStyle {
    return ParsedLengthStyle.inPixels(this.height);
  }

  public get width(): number | undefined {
    return this.currentBanner?.width;
  }

  public set width(w: number) {
    if(this.currentBanner) {
      this.currentBanner.width = w;
    }
  }

  public refreshLayout() {
    this.currentBanner.refreshLayout?.();
  }
}