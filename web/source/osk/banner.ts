///<reference path="visualKeyboard.ts" />
///<reference path="../dom/uiTouchHandlerBase.ts" />

namespace com.keyman.osk {
  // Base class for a banner above the keyboard in the OSK

  export abstract class Banner {
    private _height: number; // pixels
    private div: HTMLDivElement;

    public static DEFAULT_HEIGHT: number = 37; // pixels; embedded apps can modify

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
      // Default implementation - no listeners.
    }

    /**
     * Function     activate
     * Scope        Public
     * Description  Removes any relevant event listeners previously added by this banner.
     */
    public deactivate() {
      // Default implementation - no listeners.
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

    constructor(imagePath: string, height?: number) {
      if (imagePath.length > 0) {
        super();
        if (height) {
          this.height = height;
        }
      } else {
        super(0);
      }

      if(imagePath.indexOf('base64') >=0) {
        console.log("Loading img from base64 data");
      } else {
        console.log("Loading img with src '" + imagePath + "'");
      }
      this.img = document.createElement('img');
      this.img.setAttribute('src', imagePath);
      let ds = this.img.style;
      ds.width = '100%';
      ds.height = '100%';
      this.getDiv().appendChild(this.img);
      console.log("Image loaded.");
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
      let usableWidth = 100 - SuggestionBanner.MARGIN * (SuggestionBanner.SUGGESTION_LIMIT - 1);
      let widthpc = usableWidth / SuggestionBanner.SUGGESTION_LIMIT;

      ds.width = widthpc + '%';

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
    public apply(target?: text.OutputTarget): Promise<Reversion> {
      let keyman = com.keyman.singleton;

      if(this.isEmpty()) {
        return null;
      }
      
      if(!target) {
        /* Assume it's the currently-active `OutputTarget`.  We should probably invalidate 
          * everything if/when the active `OutputTarget` changes, though we haven't gotten that 
          * far in implementation yet.
          */
        target = dom.Utils.getOutputTarget();
      }

      if(this._suggestion.tag == 'revert') {
        keyman.core.languageProcessor.applyReversion(this._suggestion as Reversion, target);
        return null;
      } else {
        return keyman.core.languageProcessor.applySuggestion(this.suggestion, target);
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
        // Default the LTR ordering to match that of the active keyboard.
        let activeKeyboard = keyman.core.activeKeyboard;
        let rtl = activeKeyboard && activeKeyboard.isRTL;
        let orderCode = rtl ? 0x202e /* RTL */ : 0x202d /* LTR */;
        suggestionText = String.fromCharCode(orderCode) + suggestion.displayAs;
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
    static readonly BANNER_CLASS: string = 'kmw-suggest-banner';

    constructor(height?: number) {
      super(height || Banner.DEFAULT_HEIGHT);

      this.getDiv().className = this.getDiv().className + ' ' + SuggestionBanner.BANNER_CLASS;

      this.options = new Array();
      for (var i=0; i<SuggestionBanner.SUGGESTION_LIMIT; i++) {
        let d = new BannerSuggestion(i);
        this.options[i] = d;
      }

      /* LTR behavior:  the default (index 0) suggestion should be at the left
       * RTL behavior:  the default (index 0) suggestion should be at the right
       *
       * The cleanest way to make it work - simply invert the order in which
       * the elements are inserted for RTL.  This allows the banner to be RTL
       * for visuals/UI while still being internally LTR.
       */
      let activeKeyboard = com.keyman.singleton.core.activeKeyboard;
      let rtl = activeKeyboard && activeKeyboard.isRTL;
      for (var i=0; i<SuggestionBanner.SUGGESTION_LIMIT; i++) {
        let indexToInsert = rtl ? SuggestionBanner.SUGGESTION_LIMIT - i -1 : i;
        this.getDiv().appendChild(this.options[indexToInsert].div);

        if(i != SuggestionBanner.SUGGESTION_LIMIT) {
          // Adds a 'separator' div element for UI purposes.
          let separatorDiv = com.keyman.singleton.util._CreateElement('div');
          separatorDiv.className = 'kmw-banner-separator';

          let ds = separatorDiv.style;
          ds.marginLeft = (SuggestionBanner.MARGIN / 2) + '%';
          ds.marginRight = (SuggestionBanner.MARGIN / 2) + '%';

          this.getDiv().appendChild(separatorDiv);
        }
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

      keyman.core.languageProcessor.addListener('invalidatesuggestions', manager.invalidateSuggestions);
      keyman.core.languageProcessor.addListener('suggestionsready', manager.updateSuggestions);
      keyman.core.languageProcessor.addListener('tryaccept', manager.tryAccept);
      keyman.core.languageProcessor.addListener('tryrevert', manager.tryRevert);

      // Trigger a null-based initial prediction to kick things off.
      keyman.core.languageProcessor.predictFromTarget(dom.Utils.getOutputTarget());
    }

    deactivate() {
      let keyman = com.keyman.singleton;
      let manager = this.manager;

      keyman.core.languageProcessor.removeListener('invalidatesuggestions', manager.invalidateSuggestions);
      keyman.core.languageProcessor.removeListener('suggestionsready', manager.updateSuggestions);
      keyman.core.languageProcessor.removeListener('tryaccept', manager.tryAccept);
      keyman.core.languageProcessor.removeListener('tryrevert', manager.tryRevert);
    }
  }

  export class SuggestionManager extends dom.UITouchHandlerBase<HTMLDivElement> {
    private selected: BannerSuggestion;

    platformHold: (suggestion: BannerSuggestion, isCustom: boolean) => void;

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
          this.selected = null;
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
      let suggestionObj = t['suggestion'] as BannerSuggestion;

      // Is this the <keep> suggestion?  It's never in this.currentSuggestions, so check against that.
      let isCustom = this.currentSuggestions.indexOf(suggestionObj.suggestion) == -1;

      if(this.platformHold) {
        // Implemented separately for native + embedded mode branches.
        // Embedded mode should pass any info needed to show a submenu IMMEDIATELY.
        this.platformHold(suggestionObj, isCustom); // No implementation yet for native.
      }
    }
    protected clearHolds(): void {
      // Temp, pending implementation of suggestion longpress submenus
      // - nothing to clear without them -

      // only really used in native-KMW
    }
    
    protected hasModalPopup(): boolean {
      // Utilized by the mobile apps; allows them to 'take over' touch handling,
      // blocking it within KMW when the apps are already managing an ongoing touch-hold.
      let keyman = com.keyman.singleton;
      return keyman['osk'].vkbd.popupVisible;
    }

    protected dealiasSubTarget(target: HTMLDivElement): HTMLDivElement {
      return target;
    }

    protected hasSubmenu(t: HTMLDivElement): boolean {
      // Temp, pending implementation of suggestion longpress submenus

      // Only really used by native-KMW - see kmwnative's highlightSubKeys func.
      return false;
    }

    protected isSubmenuActive(): boolean {
      // Temp, pending implementation of suggestion longpress submenus

      // Utilized only by native-KMW - it parallels hasModalPopup() in purpose.
      return false;
    }

    protected displaySubmenuFor(target: HTMLDivElement) {
      // Utilized only by native-KMW to show submenus.
      throw new Error("Method not implemented.");
    }
    //#endregion
    //#endregion

    private options: BannerSuggestion[];

    private initNewContext: boolean = true;

    private currentSuggestions: Suggestion[] = [];
    private keepSuggestion: Keep;
    private revertSuggestion: Reversion;

    private recentAccept: boolean = false;
    private revertAcceptancePromise: Promise<Reversion>;

    private swallowPrediction: boolean = false;

    private doRevert: boolean = false;
    private recentRevert: boolean = false;

    constructor(div: HTMLElement, options: BannerSuggestion[]) {
      // TODO:  Determine appropriate CSS styling names, etc.
      super(div, Banner.BANNER_CLASS, SuggestionBanner.TOUCHED_CLASS);
      this.options = options;
    }

    private doAccept(suggestion: BannerSuggestion) {
      let _this = this;

      this.revertAcceptancePromise = suggestion.apply();
      if(!this.revertAcceptancePromise) {
        // We get here either if suggestion acceptance fails or if it was a reversion.
        if(suggestion.suggestion.tag == 'revert') {
          // Reversion state management
          this.recentAccept = false;
          this.doRevert = false;
          this.recentRevert = true;

          this.doUpdate();
        }
        return;
      }

      this.revertAcceptancePromise.then(function(suggestion) {
        // Always null-check!
        if(suggestion) {
          _this.revertSuggestion = suggestion;
        }
      });
      
      this.selected = null;
      this.recentAccept = true;
      this.doRevert = false;
      this.recentRevert = false;

      this.swallowPrediction = true;
      this.doUpdate();
    }

    private showRevert() {
      // Construct a 'revert suggestion' to facilitate a reversion UI component.
      this.doRevert = true;
      this.doUpdate();
    }

    /**
     * Receives messages from the keyboard that the 'accept' keystroke has been entered.
     * Should return 'false' if the current state allows accepting a suggestion and act accordingly.
     * Otherwise, return true.
     */
    tryAccept: (source: string) => boolean = function(this: SuggestionManager, source: string, returnObj: {shouldSwallow: boolean}) {
      let keyman = com.keyman.singleton;

      if(!this.recentAccept && this.selected) {
        this.doAccept(this.selected);
        returnObj.shouldSwallow = true;
      } else if(this.recentAccept && source == 'space') {
        this.recentAccept = false;
        // If the model doesn't insert wordbreaks, don't swallow the space.  If it does, 
        // we consider that insertion to be the results of the first post-accept space.
        returnObj.shouldSwallow = !!keyman.core.languageProcessor.wordbreaksAfterSuggestions;
      } else {
        returnObj.shouldSwallow = false;
      }
    }.bind(this);

    /**
     * Receives messages from the keyboard that the 'revert' keystroke has been entered.
     * Should return 'false' if the current state allows reverting a recently-applied suggestion and act accordingly.
     * Otherwise, return true.
     */
    tryRevert: () => boolean = function(this: SuggestionManager, returnObj: {shouldSwallow: boolean}) {
      // Has the revert keystroke (BKSP) already been sent once since the last accept?
      if(this.doRevert) {
        // If so, clear the 'revert' option and start doing normal predictions again.
        this.doRevert = false;
        this.recentAccept = false;
        // Otherwise, did we just accept something before the revert signal was received?
      } else if(this.recentAccept) {
        this.showRevert();
        this.swallowPrediction = true;
      }

      // We don't yet actually do key-based reversions.
      returnObj.shouldSwallow = false;
      return;
    }.bind(this);

    /**
     * Function invalidateSuggestions
     * Scope        Public
     * Description  Clears the suggestions in the suggestion banner
     */
    public invalidateSuggestions: (this: SuggestionManager, source: text.prediction.InvalidateSourceEnum) => boolean = 
        function(this: SuggestionManager, source: string) {
      
      // By default, we assume that the context is the same until we notice otherwise.
      this.initNewContext = false;

      if(!this.swallowPrediction || source == 'context') {
        this.recentAccept = false;
        this.doRevert = false;
        this.recentRevert = false;

        if(source == 'context') {
          this.swallowPrediction = false;
          this.initNewContext = true;
        }
      }

      this.options.forEach((option: BannerSuggestion) => {
        option.update(null);
      });
    }.bind(this);

    public activateKeep(): boolean {
      return !this.recentAccept && !this.recentRevert && !this.initNewContext;
    }

    private doUpdate() {
      let suggestions = [];
      // Insert 'current text' if/when valid as the leading option.
      // Since we don't yet do auto-corrections, we only show 'keep' whenever it's
      // a valid word (according to the model).
      if(this.activateKeep() && this.keepSuggestion && this.keepSuggestion.matchesModel) {
        suggestions.push(this.keepSuggestion);
      } else if(this.doRevert) {
        suggestions.push(this.revertSuggestion);
      }

      suggestions = suggestions.concat(this.currentSuggestions);

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
    public updateSuggestions: (this: SuggestionManager, prediction: text.prediction.ReadySuggestions) => boolean =
        function(this: SuggestionManager, prediction: text.prediction.ReadySuggestions) {
      
      let suggestions = prediction.suggestions;

      this.currentSuggestions = suggestions;

      // Do we have a keep suggestion?  If so, remove it from the list so that we can control its display position
      // and prevent it from being hidden after reversion operations.
      for(let s of suggestions) {
        if(s.tag == 'keep') {
          this.keepSuggestion = s as Keep;
        }
      }

      if(this.keepSuggestion) {
        this.currentSuggestions.splice(this.currentSuggestions.indexOf(this.keepSuggestion), 1);
      }

      // If we've gotten an update request like this, it's almost always user-triggered and means the context has shifted.
      if(!this.swallowPrediction) {
        this.recentAccept = false;
        this.doRevert = false;
        this.recentRevert = false;
      } else { // This prediction was triggered by a recent 'accept.'  Now that it's fulfilled, we clear the flag.
        this.swallowPrediction = false;
      }

      // The rest is the same, whether from input or from "self-updating" after a reversion to provide new suggestions.
      this.doUpdate();
    }.bind(this);
  }
}
