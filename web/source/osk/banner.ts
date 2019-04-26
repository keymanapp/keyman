///<reference path="visualKeyboard.ts" />
///<reference path="../dom/uiTouchHandlerBase.ts" />

namespace com.keyman.osk {
  // Base class for a banner above the keyboard in the OSK

  export abstract class Banner {
    private _height: number; // pixels
    private div: HTMLDivElement;

    public static DEFAULT_HEIGHT: number = 40; // pixels; embedded apps can modify

    public static readonly BANNER_CLASS: string = 'kmw-banner-bar';
    public static readonly BANNER_ID: string = 'kmw-banner-bar';

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

    /**
     * Function     activate
     * Scope        Public
     * Description  Adds any relevant event listeners needed by this banner type.
     */
    public activate() {

    }

    /**
     * Function     activate
     * Scope        Public
     * Description  Removes any relevant event listeners previously added by this banner.
     */
    public deactivate() {

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

    private _suggestion: Suggestion;

    private index: number;

    static readonly BASE_ID = 'kmw-suggestion-';

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

    get suggestion(): Suggestion {
      return this._suggestion;
    }

    /**
     * Function update
     * @param {string}     id           Element ID for the suggestion span
     * @param {Suggestion} suggestion   Suggestion from the lexical model
     * Description  Update the ID and text of the BannerSuggestionSpec
     */
    public update(suggestion: Suggestion) {
      this._suggestion = suggestion;
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
    public apply(target?: text.OutputTarget): text.Transcription {
      let keyman = com.keyman.singleton;

      if(this.isEmpty()) {
        return null;
      }
      
      // Find the state of the context at the time the prediction-triggering keystroke was applied.
      let original = keyman.modelManager.getPredictionState(this._suggestion.transformId);
      if(!original) {
        console.warn("Could not apply the Suggestion!");
        return null;
      } else {
        if(!target) {
          /* Assume it's the currently-active `OutputTarget`.  We should probably invalidate 
           * everything if/when the active `OutputTarget` changes, though we haven't gotten that 
           * far in implementation yet.
           */
          target = text.Processor.getOutputTarget();
        }

        // Apply the Suggestion!

        // Step 1:  determine the final output text
        let final = text.Mock.from(original.preInput);
        final.apply(this._suggestion.transform);

        // Step 2:  build a final, master Transform that will produce the desired results from the CURRENT state.
        // In embedded mode, both Android and iOS are best served by calculating this transform and applying its
        // values as needed for use with their IME interfaces.
        let transform = final.buildTransformFrom(target);
        target.apply(transform);

        // Signal the necessary text changes to the embedding app, if it exists.
        if(keyman['oninserttext'] && keyman.isEmbedded) {
          keyman['oninserttext'](transform.deleteLeft, transform.insert, transform.deleteRight);
        }

        // Build a 'reversion' Transcription that can be used to undo this apply() if needed.
        let preApply = text.Mock.from(original.preInput);
        preApply.apply(original.transform);
        return preApply.buildTranscriptionFrom(target, null);
      }
    }

    public isEmpty(): boolean {
      return !this._suggestion;
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

      let suggestion = this._suggestion;
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

  /**
   * Function     SuggestionBanner
   * Scope        Public
   * @param {number} height - If provided, the height of the banner in pixels
   * Description  Display lexical model suggestions in the banner
   */
  export class SuggestionBanner extends Banner {
    public static readonly SUGGESTION_LIMIT: number = 3;
    public static readonly MARGIN = 1;

    private options : BannerSuggestion[];

    private manager: SuggestionManager;

    static readonly TOUCHED_CLASS: string = 'kmw-suggest-touched';

    constructor(height?: number) {
      super(height || SuggestionBanner.DEFAULT_HEIGHT);

      this.options = new Array();
      for (var i=0; i<SuggestionBanner.SUGGESTION_LIMIT; i++) {
        let d = new BannerSuggestion(i);
        this.options[i] = d;
        this.getDiv().appendChild(d.div);
      }

      this.manager = new SuggestionManager(this.getDiv(), this.options);

      this.setupTouchHandling();
    }

    private setupTouchHandling() {
      let keyman = com.keyman.singleton;
      let div = this.getDiv();

      let th = this.manager;

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

    activate() {
      let keyman = com.keyman.singleton;
      let manager = this.manager;

      keyman.modelManager['addEventListener']('invalidatesuggestions', manager.invalidateSuggestions);
      keyman.modelManager['addEventListener']('suggestionsready', manager.updateSuggestions);
      keyman.modelManager['addEventListener']('tryaccept', manager.tryAccept);
      keyman.modelManager['addEventListener']('tryrevert', manager.tryRevert);
    }

    deactivate() {
      let keyman = com.keyman.singleton;
      let manager = this.manager;

      keyman.modelManager['removeEventListener']('invalidatesuggestions', manager.invalidateSuggestions);
      keyman.modelManager['removeEventListener']('suggestionsready', manager.updateSuggestions);
      keyman.modelManager['removeEventListener']('tryaccept', manager.tryAccept);
      keyman.modelManager['removeEventListener']('tryrevert', manager.tryRevert);
    }
  }

  class SuggestionManager extends dom.UITouchHandlerBase<HTMLDivElement> {
    private selected: BannerSuggestion;

    //#region Touch handling implementation
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

      if(t.id.indexOf(BannerSuggestion.BASE_ID) == -1) {
        console.warn("Cannot find BannerSuggestion object for element to highlight!");
      } else {
        // Never highlight an empty suggestion button.
        let suggestion = this.selected = t['suggestion'] as BannerSuggestion;
        if(suggestion.isEmpty()) {
          on = false;
        }
      }

      if(on && classes.indexOf(cs) < 0) {
        t.className=classes+cs;
      } else {
        t.className=classes.replace(cs,'');
      }
    }

    protected select(t: HTMLDivElement): void {
      this.doAccept(t['suggestion'] as BannerSuggestion);
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
    //#endregion

    private options: BannerSuggestion[];

    private currentSuggestions: Suggestion[] = [];

    private recentAccept: boolean = false;
    private recentAccepted: Suggestion;
    private preAccept: text.Transcription = null;

    private recentRevert: boolean = false;
    private rejectedSuggestions: Suggestion[] = [];

    constructor(div: HTMLElement, options: BannerSuggestion[]) {
      // TODO:  Determine appropriate CSS styling names, etc.
      super(div, Banner.BANNER_CLASS, SuggestionBanner.TOUCHED_CLASS);
      this.options = options;
    }

    private doAccept(suggestion: BannerSuggestion) {
      this.preAccept = suggestion.apply();
      
      this.selected = null;
      this.recentAccept = true;
      this.recentRevert = false;
      this.recentAccepted = suggestion.suggestion;

      // TODO:  Request a 'new' prediction based on current context with a nil Transform.
    }

    private doRevert() {
      let keyman = com.keyman.singleton;
      let current = text.Processor.getOutputTarget();
      let priorState = this.preAccept;

      // Step 1:  construct the reverted state.
      let target = text.Mock.from(priorState.preInput);
      target.apply(priorState.transform);

      // Step 2:  build a final, master Transform that will produce the desired results from the CURRENT state.
      // In embedded mode, both Android and iOS are best served by calculating this transform and applying its
      // values as needed for use with their IME interfaces.
      let transform = target.buildTransformFrom(current);
      current.apply(transform);

      // Signal the necessary text changes to the embedding app, if it exists.
      if(keyman['oninserttext'] && keyman.isEmbedded) {
        keyman['oninserttext'](transform.deleteLeft, transform.insert, transform.deleteRight);
      }

      // Denote the previous suggestion as rejected and update the 'valid' suggestion list accordingly.
      this.rejectedSuggestions.push(this.recentAccepted);
      this.currentSuggestions.splice(this.currentSuggestions.indexOf(this.recentAccepted), 1);

      // Other state maintenance
      this.recentAccept = false;
      this.recentRevert = true;
      this.doUpdate();
    }

    /**
     * Receives messages from the keyboard that the 'accept' keystroke has been entered.
     * Should return 'false' if the current state allows accepting a suggestion and act accordingly.
     * Otherwise, return true.
     */
    tryAccept: (source: string) => boolean = function(this: SuggestionManager, source: string): boolean {
      if(!this.recentAccept && this.selected) {
        this.doAccept(this.selected);
        return false;
      } else if(this.recentAccept && source == 'space') {
        this.recentAccept = false;
        return false; // Swallows a single space post-accept.
      }
      return true;  // Not yet implemented
    }.bind(this);

    /**
     * Receives messages from the keyboard that the 'revert' keystroke has been entered.
     * Should return 'false' if the current state allows reverting a recently-applied suggestion and act accordingly.
     * Otherwise, return true.
     */
    tryRevert: () => boolean = function(this: SuggestionManager): boolean {
      if(this.recentAccept) {
        this.doRevert();
        return false;
      } else {
        return true;
      }
    }.bind(this);

    /**
     * Function invalidateSuggestions
     * Scope        Public
     * Description  Clears the suggestions in the suggestion banner
     */
    public invalidateSuggestions: (this: SuggestionManager) => boolean = 
        function(this: SuggestionManager) {

      this.recentAccept = false;
      this.recentRevert = false;
      this.rejectedSuggestions = [];

      this.options.forEach((option: BannerSuggestion) => {
        option.update(null);
      });
    }.bind(this);

    private doUpdate() {
      // TODO:  Insert 'current text' if/when valid as the leading option.
      //        We need the LMLayer to tell us this somehow.
      let suggestions = [].concat(this.currentSuggestions);

      this.options.forEach((option: BannerSuggestion, i: number) => {
        if(i < suggestions.length) {
          option.update(suggestions[i]);
        } else {
          option.update(null);
        }
      });
    }

    /**
     * Function updateSuggestions
     * Scope       Public
     * @param {Suggestion[]}  suggestions   Array of suggestions from the lexical model.
     * Description    Update the displayed suggestions in the SuggestionBanner
     */
    public updateSuggestions: (this: SuggestionManager, suggestions: Suggestion[]) => boolean =
        function(this: SuggestionManager, suggestions: Suggestion[]) {
      
      this.currentSuggestions = suggestions;

      // If we've gotten an update request like this, it's almost always user-triggered and means the context has shifted.
      this.recentAccept = false;
      this.recentRevert = false;
      this.rejectedSuggestions = [];

      // The rest is the same, whether from input or from "self-updating" after a reversion to provide new suggestions.
      this.doUpdate();
    }.bind(this);
  }
}
