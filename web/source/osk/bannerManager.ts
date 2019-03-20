/// <reference path="banner.ts" />

namespace com.keyman.osk {
  export interface BannerOptions {
    persistentBanner?: boolean;
    enablePredictions?: boolean;
    imagePath?: string;
    // TODO: Remove test code for production
    setPrediction?: string;
  }

  export type BannerType = "blank" | "image" | "suggestion";

  export class BannerManager {
    private _options: BannerOptions = {};
    private bannerContainer: HTMLDivElement;
    private activeBanner: Banner;
    private alwaysShows: boolean;
    private imagePath?: string = "";

    public static readonly DEFAULT_OPTIONS: BannerOptions = {
      persistentBanner: false,
      enablePredictions: true,
      imagePath: ""
    }

    constructor() {
      // Step 1 - establish the container element.  Must come before this.setOptions.
      this.constructContainer();

      // Register a listener for model change events so that we can hot-swap the banner as needed.
      let keyman = com.keyman.singleton;
      let device = keyman.util.device;

      // TODO: Remove for production
      let initialOptions: BannerOptions;
      if (device.OS == 'iOS') {
        initialOptions = {
          persistentBanner: true,
          enablePredictions: false,
          imagePath: '../../ios/keyman/Keyman/SWKeyboard/Keyman Banner/banner-Portrait.png'
        };
        this.setBanner('image');
      } else if (device.OS == 'Android') {
        initialOptions = BannerManager.DEFAULT_OPTIONS;
        this.setBanner('blank');
      } else if (device.formFactor == 'desktop') {
        initialOptions = {
          persistentBanner: true,
          enablePredictions: true
        };
        this.setBanner('suggestion');
      } else {
        initialOptions = BannerManager.DEFAULT_OPTIONS;
      }

      // Initialize with the default options - any 'manually set' options come post-construction.
      // This will also automatically set the default banner in place.
      this.setOptions(initialOptions);

      keyman.modelManager['addEventListener']('modelchange', this.selectBanner.bind(this));
      keyman.modelManager['addEventListener']('invalidatesuggestions', this.invalidateSuggestions.bind(this));
      keyman.modelManager['addEventListener']('suggestionsready', this.updateSuggestions.bind(this));
    }

    private constructContainer(): HTMLDivElement {
      let height = 40;

      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      let d = util._CreateElement('div');
      d.id = "keymanweb_banner_container";
      return this.bannerContainer = d;
    }

    public get element(): HTMLDivElement {
      return this.bannerContainer;
    }

    public getOptions(): BannerOptions {
      let retObj = {};

      for(let key in this._options) {
        retObj[key] = this._options[key];
      }

      return retObj;
    }

    public setOptions(optionSpec: BannerOptions) {
      let keyman = com.keyman.singleton;

      for(let key in optionSpec) {
        switch(key) {
          // Each defined option may require specialized handling.
          case 'persistentBanner':
            // Determines the banner type to activate.
            this.alwaysShows = optionSpec['persistentBanner'];
            break;
          case 'enablePredictions':
            // If we're not going to show suggestions, it's best to turn off predictions.
            keyman.modelManager.enabled = optionSpec['enablePredictions'];
            break;
          case 'imagePath':
            // Determines the image file to use for ImageBanners.
            this.imagePath = optionSpec['imagePath'];
            break;
          case 'setPrediction':
            let suggestions:Suggestion[] = Array();
            for(var i=0; i<optionSpec['setPrediction'].length; i++) {
              let s: Suggestion = SuggestionBanner.BLANK_SUGGESTION();
              s.displayAs = optionSpec['setPrediction'][i] + (i+1) + ' ';
              suggestions.push(s);
            }
            this.updateSuggestions(suggestions);
            // Don't reselect the banner because that will reinitialize the suggestions
            return;
          default:
            // Invalid option specified!
        }

        this._options[key] = optionSpec['key'];
      }

      this.selectBanner();
    }

    public appendStyles() {
      if(this.activeBanner) {
        this.activeBanner.appendStyleSheet();
      }
    }

    public setBanner(type: BannerType) {
      switch(type) {
        case 'blank':
          this._setBanner(new BlankBanner());
          break;
        case 'image':
          this._setBanner(new ImageBanner(this.imagePath));
          break;
        case 'suggestion':
          this._setBanner(new SuggestionBanner());
          break;
        default:
          throw new Error("Invalid type specified for the banner!");
      }
    }

    private selectBanner(state?: text.prediction.ModelChangeEnum) {
      let keyman = com.keyman.singleton;

      if(keyman.modelManager.enabled) {
        this.setBanner('suggestion');
      } else if(this.alwaysShows) {
        this.setBanner('image');
      } else {
        this.setBanner('blank');
      }
    }

    private _setBanner(banner: Banner) {
      if(this.activeBanner) {
        if(banner == this.activeBanner) {
          return;
        } else {
          this.bannerContainer.replaceChild(banner.getDiv(), this.activeBanner.getDiv());
        }
      }

      this.activeBanner = banner;
      this.bannerContainer.appendChild(banner.getDiv());
    }

    private invalidateSuggestions() {
      if(this.activeBanner) {
        let banner = this.activeBanner as SuggestionBanner;
        banner.invalidateSuggestions();
        this._setBanner(banner);
      }
    }

    private updateSuggestions(suggestions: Suggestion[]) {
      if (this.activeBanner) {
        let banner = this.activeBanner as SuggestionBanner;
        banner.updateSuggestions(suggestions);
        this._setBanner(banner);
      }
    }

    public get height(): number {
      if(this.activeBanner) {
        return this.activeBanner.height;
      } else {
        return 0;
      }
    }
  }
}