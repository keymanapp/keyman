/// <reference path="banner.ts" />

namespace com.keyman.osk {
  /**
   * This object is used to specify options by both `BannerManager.getOptions`
   * and `BannerManager.setOptions`.  Refer to the latter for specification of
   * each field.
   */
  export interface BannerOptions {
    alwaysShow?: boolean;
    mayPredict?: boolean;
    mayCorrect?: boolean;
    imagePath?: string;
  }

  export type BannerType = "blank" | "image" | "suggestion";

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
  export class BannerManager {
    private _activeType: BannerType;
    private _options: BannerOptions = {};
    private bannerContainer: HTMLDivElement;
    private activeBanner: Banner;
    private alwaysShow: boolean;
    private imagePath?: string = "";

    public static readonly DEFAULT_OPTIONS: BannerOptions = {
      alwaysShow: false,
      mayPredict: true,
      mayCorrect: true,
      imagePath: ""
    }

    constructor() {
      // Step 1 - establish the container element.  Must come before this.setOptions.
      this.constructContainer();

      // Initialize with the default options - 
      // any 'manually set' options come post-construction.
      // This will also automatically set the default banner in place.
      this.setOptions(BannerManager.DEFAULT_OPTIONS);

      // Register a listener for model change events so that we can hot-swap the banner as needed.
      let keyman = com.keyman.singleton;
      keyman.core.languageProcessor.on('statechange', this.selectBanner.bind(this));
    }

    /**
     * Constructs the <div> element used to contain hot-swapped `Banner` instances.
     */
    private constructContainer(): HTMLDivElement {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;
      let d = util._CreateElement('div');
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
      let keyman = com.keyman.singleton;

      for(let key in optionSpec) {
        switch(key) {
          // Each defined option may require specialized handling.
          case 'alwaysShow':
            // Determines the banner type to activate.
            this.alwaysShow = optionSpec[key];
            break;
          case 'mayPredict':
            keyman.core.languageProcessor.mayPredict = optionSpec[key]
            break;
          case 'mayCorrect':
            keyman.core.languageProcessor.mayCorrect = optionSpec[key];
            break;
          case 'imagePath':
            // Determines the image file to use for ImageBanners.
            this.imagePath = optionSpec[key];
            break;
          default:
            // Invalid option specified!
        }
        this._options[key] = optionSpec[key];
      }

      this.selectBanner();
    }

    /**
     * Applies any stylesheets needed by specific `Banner` instances.
     */
    public appendStyles() {
      if(this.activeBanner) {
        this.activeBanner.appendStyleSheet();
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
    public setBanner(type: BannerType, height?: number) {
      var banner: Banner;

      switch(type) {
        case 'blank':
          banner = new BlankBanner();
          break;
        case 'image':
          banner = new ImageBanner(this.imagePath, Banner.DEFAULT_HEIGHT);
          break;
        case 'suggestion':
          banner = new SuggestionBanner(height);
          break;
        default:
          throw new Error("Invalid type specified for the banner!");
      }

      this._activeType = type;
      
      if(banner) {
        this._setBanner(banner);
        banner.activate();
      }
    }

    /**
     * Handles `LanguageProcessor`'s `'statechange'` events, 
     * allowing logic to automatically hot-swap `Banner`s as needed.
     * @param state 
     */
    private selectBanner(state?: text.prediction.StateChangeEnum) {
      let keyman = com.keyman.singleton;

      // Only display a SuggestionBanner when LanguageProcessor states it is active.s
      if(keyman.core.languageProcessor.isActive) {
        this.setBanner('suggestion');
      } else if(this.alwaysShow) {
        this.setBanner('image');
      } else {
        this.setBanner('blank');
      }
    }

    /**
     * Internal method used by the public API `setBanner`.  `setBanner` 
     * translates the string parameter into a new instance consumed by this method.
     * @param banner The `Banner` instance to set as active.
     */
    private _setBanner(banner: Banner) {
      if(this.activeBanner) {
        if(banner == this.activeBanner) {
          return;
        } else {
          let prevBanner = this.activeBanner;
          prevBanner.deactivate();
          this.bannerContainer.replaceChild(banner.getDiv(), prevBanner.getDiv());
        }
      }

      this.activeBanner = banner;
      this.bannerContainer.appendChild(banner.getDiv());

      // Don't forget to adjust the OSK in case we're now using a blank Banner!
      let keyman = com.keyman.singleton;
      keyman['osk']._Show();
    }

    public get activeType(): BannerType {
      return this._activeType;
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

    /**
     * Sets the height (in pixels) of the active 'Banner' instance.
     */
    public set height(h: number) {
      if (this.activeBanner) {
        this.activeBanner.height = h;
      }
    }
  }
}
