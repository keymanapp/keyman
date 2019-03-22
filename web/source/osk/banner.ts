///<reference path="visualKeyboard.ts" />
namespace com.keyman.osk {
  // Base class for a banner above the keyboard in the OSK

  export abstract class Banner {
    private _visible: boolean;
    private _enabled: boolean;
    private _height: number; // pixels
    private div: HTMLDivElement;

    public DEFAULT_HEIGHT: number = 40; // pixels

    /**
     * Function     height
     * Scope        Public
     * @returns     {number} height in pixels
     * Description  Returns the height of the banner in pixels
     */
    public get height(): number {
      return this._height;
    }

    /**
     * Function     height
     * Scope        Public
     * @param       {number} height   the height in pixels
     * Description  Sets the height of the banner in pixels
     */
    public set height(height: number) {
      this._height = height;
      if (this.div) {
        let ds = this.div.style;
        ds.height = (height > 0) ? height + 'px' : '0px';
      }
    }

    /**
     * Function     visible
     * Scope        Public
     * @returns     {boolean} true if the banner is visible
     * Description  Returns whether the banner is visible or not
     */
    public get visible(): boolean {
      return this._visible;
    }

    /**
     * Function     visible
     * Scope        Public
     * @param       {boolean} visible   true if the banner is to be visible
     * Description  Sets the visiblity of the banner
     */
    public set visible(visible: boolean) {
      this._visible = visible;

      if (this.div) {
        let ds = this.div.style;
        ds.display=(this._visible) ? 'block': 'none';
      }
    }

    /**
     * Function     enable
     * Scope        Public
     * @return      {boolean} true if the banner is enabled
     * Description  Returns whether the banner is enabled or not
     */
    public get enable(): boolean {
      return this._enabled;
    }

    /**
     * Function     visible
     * Scope        Public
     * @param       {boolean} enable     true if the banner is to be enabled
     * Description  Sets whether the banner is enabled or not
     */
    public set enable(enable: boolean) {
      this._enabled = enable;
    }

    public constructor(visible: boolean, enabled: boolean, height?: number) {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      let d = util._CreateElement('div');
      d.id = "keymanweb_banner_bar";
      d.className = "kmw-banner-bar";
      this.div = d;

      this.height = (height >= 0) ? height : this.DEFAULT_HEIGHT;
      this.visible = visible;
      this.enable = enabled;
    }

    public appendStyleSheet() {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      // TODO: add stylesheets
    }

    /**
     * Function     getDiv
     * Scope        Public
     * @returns     {HTMLElement} Base element of the banner
     * Description  Returns the HTMLElelemnt of the banner
     */
    public getDiv(): HTMLElement {
      return this.div;
    }
  }

  /**
   * Function       BlankBanner
   * Description    A banner of height 0 that should not be shown
   */
  export class BlankBanner extends Banner {

    constructor() {
      super(false, true, 0);
    }
  }

  /**
   * Function       ImageBanner
   * @param         {string}        imagePath   Path of image to display in the banner
   * Description    Display an image in the banner
   */
  export class ImageBanner extends Banner {
    private img: HTMLElement;

    constructor(imagePath, height?: number) {
      if (imagePath.length > 0) {
        super(true, true);
      } else {
        super(false, true, 0);
      }

      this.img = document.createElement('img');
      this.img.setAttribute('src', imagePath);
      let ds = this.img.style;
      ds.width = '100%';
      ds.height = '100%';
      this.getDiv().appendChild(this.img);
    }

    /**
     * Function     setImagePath
     * Scope        Public
     * @param       {string}     imagePath   Path of image to display in the banner
     * Description  Update the image in the banner
     */
    public setImagePath(imagePath: string) {
      if (this.img) {
        this.img.setAttribute('src', imagePath);
      }
      if (imagePath.length > 0) {
        this.visible = true;
        this.enable = true;
      }
    }
  }

  export class BannerSuggestionSpec {
    id: string;
    languageID: string;
    text?: string;
    width: string;
    pad?: string;
    widthpc?: number; // Added during OSK construction.
    padpc?: number; // Added during OSK construction.

    constructor(id: string, languageID: string, text?: string, width?: string, pad?: string) {
      this.id = id;
      this.languageID = languageID;
      this.text = text;
      this.width = width ? width : '50';
      this.pad = pad;
    }
  }

  export class BannerSuggestion {
    spec: BannerSuggestionSpec;
    attachedElement: HTMLElement;

    constructor(spec: BannerSuggestionSpec) {
      this.spec = spec;
    }

    public update(suggestion: Suggestion) {
      this.spec.text = suggestion.displayAs;
    }

    // Produces a HTMLSpanElement with the key's actual text.
    public generateSuggestionText(): HTMLSpanElement {
      let util = (<KeymanBase>window['keyman']).util;
      let spec = this.spec;

      // Add OSK suggestion labels
      var suggestionText: string;
      var t=util._CreateElement('span'), ts=t.style;
      t.className = "kmw-suggestion-span";
      if(spec.text == null || spec.text == '') {
        suggestionText = '\xa0';  // default:  nbsp.
      } else {
        suggestionText = spec.text;
      }

      if (this.spec.languageID) {
        t.lang = this.spec.languageID;
      }

      //Override font spec if set for this key in the layout
      if(typeof spec['font'] == 'string' && spec['font'] != '') {
        ts.fontFamily=spec['font'];
      }

      if(typeof spec['fontsize'] == 'string' && spec['fontsize'] != 0) {
        ts.fontSize=spec['fontsize'];
      }

      let device = util.device;
      if (device.formFactor != 'desktop') {
        let oskManager = com.keyman.singleton.osk;
        ts.width = Math.floor(oskManager.getWidth() / SuggestionBanner.SUGGESTION_LIMIT) + 'px';
      }

      let keyboardManager = (<KeymanBase>window['keyman']).keyboardManager;
      if(keyboardManager.isRTL()) {
        // Add the RTL marker to ensure it displays correctly.
        suggestionText = '\u200f' + suggestionText;
      }

      // Finalize the suggestion text
      var d=util._CreateElement('div'), ds=d.style;
      d.className = 'kmw-suggestion-text';
      d.innerHTML = suggestionText;
      t.appendChild(d);

      return t;
    }
  }

  /**
   * Function     SuggestionBanner
   * Scope        Public
   * Description  Display lexical model suggestions in the banner
   */
  export class SuggestionBanner extends Banner {
    public static SUGGESTION_LIMIT: number = 3;
    private suggestionList : BannerSuggestion[];

    constructor() {
      super(true, true);
      let suggestionList: BannerSuggestion[] = new Array();
      this.suggestionList = suggestionList;
      for (var i=0; i<SuggestionBanner.SUGGESTION_LIMIT; i++) {
        let s = new BannerSuggestionSpec('suggestion' + i, 'en', '', '33', ' ');
        let d = new BannerSuggestion(s);
        this.suggestionList[i] = d;
        d.attachedElement = this.getDiv().appendChild(d.generateSuggestionText());
      }
    }

    public static BLANK_SUGGESTION(): Suggestion {
      let s: Suggestion = {
        displayAs: '',
        transform: {
          insert: '', deleteLeft: 0, deleteRight: 0
        }
      };
      return s;
    };

    /**
     * Function invalidateSuggestions
     * Scope        Public
     * Description  Clears the suggestions in the suggestion banner
     */
    public invalidateSuggestions: (this: SuggestionBanner) => boolean = function(this: SuggestionBanner) {
      this.suggestionList.forEach((suggestion) => {
        suggestion.spec.text = '';
        this.getDiv().replaceChild(suggestion.generateSuggestionText(), suggestion.attachedElement);
      });
    }.bind(this);

    /**
     * Function updateSuggestions
     * Scope       Public
     * @param {Suggestion[]}  suggestions   Array of suggestions from the lexical model.
     * Description    Update the displayed suggestions in the SuggestionBanner
     */
    public updateSuggestions: (this: SuggestionBanner, suggestions: Suggestion[]) => boolean =
      function(this: SuggestionBanner, suggestions: Suggestion[]) {
      this.suggestionList.forEach((suggestion, i) => {
        this.suggestionList[i].update(suggestions[i]);
        this.getDiv().replaceChild(suggestion.generateSuggestionText(), suggestion.attachedElement);
      });
    }.bind(this);
  }
}
