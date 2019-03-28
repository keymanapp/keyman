///<reference path="visualKeyboard.ts" />
namespace com.keyman.osk {
  // Base class for a banner above the keyboard in the OSK

  export abstract class Banner {
    private _height: number; // pixels
    private div: HTMLDivElement;

    public static DEFAULT_HEIGHT: number = 40; // pixels

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
     * Description  Sets the height of the banner in pixels. If a negative 
     *              height is given, set height to 0 pixels.
     *              Also updates the banner styling.
     */
    public set height(height: number) {
      this._height = (height > 0) ?  height : 0;
      this.update();
    }

    /**
     * Function      update
     * @return       {boolean}   true if the banner styling changed
     * Description   Update the height and display styling of the banner
     */
    private update() : boolean {
      let ds = this.div.style;
      let currentHeightStyle = ds.height;
      let currentDisplayStyle = ds.display;

      if (this._height > 0) {
        ds.height = this._height + 'px';
        ds.display = 'block';
      } else {
        ds.height = '0px';
        ds.display = 'none';
      }

      return (!(currentHeightStyle === ds.height) || 
        !(currentDisplayStyle === ds.display));
    }

    public constructor(height?: number) {
      let keymanweb = com.keyman.singleton;
      let util = keymanweb.util;

      let d = util._CreateElement('div');
      d.id = "keymanweb_banner_bar";
      d.className = "kmw-banner-bar";
      this.div = d;

      this.height = height;
      this.update();
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
      super(0);
    }
  }

  /**
   * Function       ImageBanner
   * @param         {string}        imagePath   Path of image to display in the banner
   * @param         {number}        height      If provided, the height of the banner in pixels
   * Description    Display an image in the banner
   */
  export class ImageBanner extends Banner {
    private img: HTMLElement;

    constructor(imagePath, height?: number) {
      if (imagePath.length > 0) {
        super();
        if (height) {
          this.height = height;
        }
      } else {
        super(0);
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
    }
  }

  export class BannerSuggestionSpec {
    id: string;
    languageID: string;
    text?: string;
    width: number;
    pad?: string;
    widthpc?: number; // Added during OSK construction.
    padpc?: number; // Added during OSK construction.

    public DEFAULT_SUGGESTION_WIDTH: number = 50; // pixels

    constructor(id: string, languageID: string, text?: string, width?: number, pad?: string) {
      this.id = id;
      this.languageID = languageID;
      this.text = text;
      this.width = width ? width : this.DEFAULT_SUGGESTION_WIDTH;
      this.pad = pad;
    }
  }

  export class BannerSuggestion {
    spec: BannerSuggestionSpec;
    suggestion: Suggestion;

    constructor(spec: BannerSuggestionSpec) {
      this.spec = spec;
    }

    /**
     * Function update
     * @param {string}     id           Element ID for the suggestion span
     * @param {Suggestion} suggestion   Suggestion from the lexical model
     * Description  Update the ID and text of the BannerSuggestionSpec
     */
    public update(id: string, suggestion: Suggestion) {
      this.spec.id = id;
      this.spec.text = suggestion.displayAs;
      this.suggestion = suggestion;
    }

    /**
     * Function apply
     * @param target (Optional) The OutputTarget to which the `Suggestion` ought be applied.
     * Description  Applies the predictive `Suggestion` represented by this `BannerSuggestion`.
     */
    public apply(target?: text.OutputTarget) {
      let keyman = com.keyman.singleton;
      
      // Find the state of the context at the time the prediction-triggering keystroke was applied.
      let original = keyman.modelManager.getPredictionState(this.suggestion.transformId);
      if(!original) {
        console.warn("Could not apply the Suggestion!");
        return;
      } else {
        if(!target) {
          /* Assume it's the currently-active `OutputTarget`.  We should probably invalidate 
           * everything if/when the active `OutputTarget` changes, though we haven't gotten that 
           * far in implementation yet.
           */
          target = text.Processor.getOutputTarget();
        }

        // Apply the Suggestion!
        target.restoreTo(original.preInput);
        target.apply(this.suggestion.transform);
      }
    }

    /**
     * Function generateSuggestionText
     * @return {HTMLSpanElement}  Span element of the suggestion
     * Description   Produces a HTMLSpanElement with the key's actual text.
     */
    //
    public generateSuggestionText(): HTMLSpanElement {
      let util = (<KeymanBase>window['keyman']).util;
      let spec = this.spec;

      // Add OSK suggestion labels
      var suggestionText: string;
      var t=util._CreateElement('span'), ts=t.style;
      t.id = spec.id;
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
      let oskManager = com.keyman.singleton.osk;
      if (device.formFactor != 'desktop') {
        ts.width = Math.floor(oskManager.getWidth() / SuggestionBanner.SUGGESTION_LIMIT) + 'px';
      } else {
        ts.width = Math.floor(oskManager.getWidthFromCookie() / SuggestionBanner.SUGGESTION_LIMIT) + 'px';
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
      super(SuggestionBanner.DEFAULT_HEIGHT);
      this.suggestionList = new Array();
      for (var i=0; i<SuggestionBanner.SUGGESTION_LIMIT; i++) {
        let s = new BannerSuggestionSpec('kmw-suggestion-' + i, 'en', '', 33, ' ');
        let d = new BannerSuggestion(s);
        this.suggestionList[i] = d;
        this.getDiv().appendChild(d.generateSuggestionText());
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
    public invalidateSuggestions: (this: SuggestionBanner) => boolean = 
        function(this: SuggestionBanner) {
      this.suggestionList.forEach((suggestion, i) => {
        this.suggestionList[i].update('kmw-suggestion-'+i, 
          SuggestionBanner.BLANK_SUGGESTION());
        this.getDiv().replaceChild(suggestion.generateSuggestionText(), 
          this.getDiv().childNodes.item(i));
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
        this.suggestionList[i].update('kmw-suggestion-'+i, suggestions[i]);
        this.getDiv().replaceChild(suggestion.generateSuggestionText(), 
          this.getDiv().childNodes.item(i));
      });
    }.bind(this);
  }
}
