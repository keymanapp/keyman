///<reference path="visualKeyboard.ts" />
///<reference path="../dom/uiTouchHandlerBase.ts" />

namespace com.keyman.osk {
  // Base class for a banner above the keyboard in the OSK

  export abstract class Banner {
    private _height: number; // pixels
    private div: HTMLDivElement;

    public static readonly DEFAULT_HEIGHT: number = 40; // pixels
    public static readonly BANNER_CLASS: string = 'kmw-banner-bar';
    public static readonly BANNER_ID: string = 'keymanweb_banner_bar';

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
      d.id = Banner.BANNER_ID;
      d.className = Banner.BANNER_CLASS;
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

  export class BannerSuggestion {
    div: HTMLDivElement;
    private display: HTMLSpanElement;
    private fontFamily?: string;

    private suggestion: Suggestion;

    private index: number;

    private static readonly BASE_ID = 'kmw-suggestion-';

    constructor(index: number) {
      let keyman = com.keyman.singleton;

      this.index = index;

      this.constructRoot();

      // Provides an empty, base SPAN for text display.  We'll swap these out regularly;
      // `Suggestion`s will have varying length and may need different styling.
      let display = this.display = keyman.util._CreateElement('span');
      this.div.appendChild(display);
    }

    private constructRoot() {
      let keyman = com.keyman.singleton;

      // Add OSK suggestion labels
      let div = this.div = keyman.util._CreateElement('div'), ds=div.style;
      div.className = "kmw-suggest-option";
      div.id = BannerSuggestion.BASE_ID + this.index;

      let kbdDetails = keyman.keyboardManager.activeStub;
      if(kbdDetails) {  
        if (kbdDetails['KLC']) {
          div.lang = kbdDetails['KLC'];
        }

        // Establish base font settings
        let font = kbdDetails['KFont'];
        if(font && font.family && font.family != '') {
          ds.fontFamily = this.fontFamily = font.family;
        }
      }

      // Ensures that a reasonable width % is set.
      let usableWidth = 100 - SuggestionBanner.MARGIN * (SuggestionBanner.SUGGESTION_LIMIT + 1);
      let widthpc = usableWidth / SuggestionBanner.SUGGESTION_LIMIT;

      ds.width = widthpc + '%';
      ds.marginLeft = SuggestionBanner.MARGIN + '%';

      this.div['suggestion'] = this;
    }

    /**
     * Function update
     * @param {string}     id           Element ID for the suggestion span
     * @param {Suggestion} suggestion   Suggestion from the lexical model
     * Description  Update the ID and text of the BannerSuggestionSpec
     */
    public update(suggestion: Suggestion) {
      this.suggestion = suggestion;
      this.updateText();
    }

    private updateText() {
      let display = this.generateSuggestionText();
      this.div.replaceChild(display, this.display);
      this.display = display;
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
      let keyman = com.keyman.singleton;
      let util = keyman.util;

      let suggestion = this.suggestion;
      var suggestionText: string;

      var s=util._CreateElement('span');
      s.className = 'kmw-suggestion-text';

      if(suggestion == null) {
        return s;
      }

      if(suggestion.displayAs == null || suggestion.displayAs == '') {
        suggestionText = '\xa0';  // default:  nbsp.
      } else {
        suggestionText = suggestion.displayAs;
      }

      let keyboardManager = (<KeymanBase>window['keyman']).keyboardManager;
      if(keyboardManager.isRTL()) {
        // Add the RTL marker to ensure it displays correctly.
        suggestionText = '\u200f' + suggestionText;
      }

      // TODO:  Dynamic suggestion text resizing.  (Refer to OSKKey.getTextWidth in visualKeyboard.ts.)

      // Finalize the suggestion text
      s.innerHTML = suggestionText;
      return s;
    }
  }

  class SuggestionTouchManager extends dom.UITouchHandlerBase<HTMLDivElement> {
    findTargetFrom(e: HTMLElement): HTMLDivElement {
      let keyman = com.keyman.singleton;
      let util = keyman.util;

      try {
        if(e) {
          if(util.hasClass(e,'kmw-suggest-option')) {
            return e as HTMLDivElement;
          }
          if(e.parentNode && util.hasClass(<HTMLElement> e.parentNode,'kmw-suggest-option')) {
            return e.parentNode as HTMLDivElement;
          }
          // if(e.firstChild && util.hasClass(<HTMLElement> e.firstChild,'kmw-suggest-option')) {
          //   return e.firstChild as HTMLDivElement;
          // }
        }
      } catch(ex) {}
      return null;
    }

    protected highlight(t: HTMLDivElement, on: boolean): void {
      let classes = t.className;
      let cs = ' ' + SuggestionBanner.TOUCHED_CLASS;

      if(on && classes.indexOf(cs) < 0) {
        t.className=classes+cs;
      } else {
        t.className=classes.replace(cs,'');
      }
    }

    protected select(t: HTMLDivElement): void {
      let suggestion = t['suggestion'] as BannerSuggestion;
      suggestion.apply();
    }

    //#region Long-press support
    protected hold(t: HTMLDivElement): void {
      // Temp, pending implementation of suggestion longpress submenus
      // - nothing worth doing with a hold yet -
    }
    protected clearHolds(): void {
      // Temp, pending implementation of suggestion longpress submenus
      // - nothing to clear without them -
    }
    protected hasModalPopup(): boolean {
      // Temp, pending implementation of suggestion longpress submenus
      return false;
    }
    protected dealiasSubTarget(target: HTMLDivElement): HTMLDivElement {
      return target;
    }
    protected hasSubmenu(t: HTMLDivElement): boolean {
      // Temp, pending implementation of suggestion longpress submenus:
      return false;
    }
    protected isSubmenuActive(): boolean {
      // Temp, pending implementation of suggestion longpress submenus:
      return false;
    }
    protected displaySubmenuFor(target: HTMLDivElement) {
      throw new Error("Method not implemented.");
    }
    //#endregion

    constructor(div: HTMLElement) {
      // TODO:  Determine appropriate CSS styling names, etc.
      super(div, Banner.BANNER_CLASS, SuggestionBanner.TOUCHED_CLASS);
    }
  }

  /**
   * Function     SuggestionBanner
   * Scope        Public
   * Description  Display lexical model suggestions in the banner
   */
  export class SuggestionBanner extends Banner {
    public static readonly SUGGESTION_LIMIT: number = 3;
    public static readonly MARGIN = 1;

    private suggestionList : BannerSuggestion[];
    private currentSuggestions: Suggestion[] = [];

    private touchHandler: SuggestionTouchManager;

    static readonly TOUCHED_CLASS: string = 'kmw-suggest-touched';

    constructor() {
      super(SuggestionBanner.DEFAULT_HEIGHT);
      this.suggestionList = new Array();
      for (var i=0; i<SuggestionBanner.SUGGESTION_LIMIT; i++) {
        let d = new BannerSuggestion(i);
        this.suggestionList[i] = d;
        this.getDiv().appendChild(d.div);
      }

      this.setupTouchHandling();
    }

    private setupTouchHandling() {
      let keyman = com.keyman.singleton;
      let div = this.getDiv();

      let th = this.touchHandler = new SuggestionTouchManager(div);

      if(keyman.util.device.touchable) { //  /*&& ('ontouchstart' in window)*/ // Except Chrome emulation doesn't set this.
        // Not to mention, it's rather redundant.
        div.addEventListener('touchstart', function(e: TouchEvent) {
          th.touchStart(e);
        }, true);
        // The listener below fails to capture when performing automated testing checks in Chrome emulation unless 'true'.
        div.addEventListener('touchend', function(e: TouchEvent) {
          th.touchEnd(e);
        }, true); 
        div.addEventListener('touchmove', function(e: TouchEvent) {
          th.touchMove(e);
        }, false);
        //lDiv.addEventListener('touchcancel', osk.cancel,false); //event never generated by iOS
      }
    }

    /**
     * Function invalidateSuggestions
     * Scope        Public
     * Description  Clears the suggestions in the suggestion banner
     */
    public invalidateSuggestions: (this: SuggestionBanner) => boolean = 
        function(this: SuggestionBanner) {
      this.suggestionList.forEach((option: BannerSuggestion) => {
        option.update(null);
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
      this.currentSuggestions = suggestions;
      
      this.suggestionList.forEach((option: BannerSuggestion, i: number) => {
        if(i < suggestions.length) {
          option.update(suggestions[i]);
        } else {
          option.update(null);
        }
      });
    }.bind(this);
  }
}
