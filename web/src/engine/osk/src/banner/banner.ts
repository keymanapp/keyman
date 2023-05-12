import EventEmitter from 'eventemitter3';

import { DeviceSpec } from '@keymanapp/web-utils';
import { Keyboard, KeyboardProperties } from '@keymanapp/keyboard-processor';
import { type PredictionContext } from '@keymanapp/input-processor';
import InputEventEngine, { InputEventEngineConfig } from '../input/event-interpreter/inputEventEngine.js';
import MouseEventEngine from '../input/event-interpreter/mouseEventEngine.js';
import TouchEventEngine from '../input/event-interpreter/touchEventEngine.js';
import UITouchHandlerBase from '../input/event-interpreter/uiTouchHandlerBase.js';

import { createUnselectableElement } from 'keyman/engine/dom-utils';

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
    let d = createUnselectableElement('div');
    d.id = Banner.BANNER_ID;
    d.className = Banner.BANNER_CLASS;
    this.div = d;

    this.height = height;
    this.update();
  }

  public appendStyleSheet() {
    // TODO: add stylesheets
    // See VisualKeyboard's method + 'addFontStyle' for current handling.
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
   * Allows banners to adapt based on the active keyboard and related properties, such as
   * associated fonts.
   * @param keyboard
   * @param keyboardProperties
   */
  public configureForKeyboard(keyboard: Keyboard, keyboardProperties: KeyboardProperties) { }
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
  private rtl: boolean = false;

  private _suggestion: Suggestion;

  private index: number;

  static readonly BASE_ID = 'kmw-suggestion-';

  constructor(index: number, isRTL: boolean) {
    this.index = index;
    this.rtl = isRTL;

    this.constructRoot();

    // Provides an empty, base SPAN for text display.  We'll swap these out regularly;
    // `Suggestion`s will have varying length and may need different styling.
    let display = this.display = createUnselectableElement('span');
    this.div.appendChild(display);
  }

  private constructRoot() {
    // Add OSK suggestion labels
    let div = this.div = createUnselectableElement('div'), ds=div.style;
    div.className = "kmw-suggest-option";
    div.id = BannerSuggestion.BASE_ID + this.index;

    // Ensures that a reasonable width % is set.
    let usableWidth = 100 - SuggestionBanner.MARGIN * (SuggestionBanner.SUGGESTION_LIMIT - 1);
    let widthpc = usableWidth / SuggestionBanner.SUGGESTION_LIMIT;

    ds.width = widthpc + '%';

    this.div['suggestion'] = this;
  }

  public matchKeyboardProperties(keyboardProperties: KeyboardProperties) {
    const div = this.div;

    if(keyboardProperties) {
      if (keyboardProperties['KLC']) {
        div.lang = keyboardProperties['KLC'];
      }

      // Establish base font settings
      let font = keyboardProperties['KFont'];
      if(font && font.family && font.family != '') {
        div.style.fontFamily = this.fontFamily = font.family;
      }
    }
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
    let display = this.generateSuggestionText(this.rtl);
    this.div.replaceChild(display, this.display);
    this.display = display;
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
  public generateSuggestionText(rtl: boolean): HTMLSpanElement {
    let suggestion = this._suggestion;
    var suggestionText: string;

    var s=createUnselectableElement('span');
    s.className = 'kmw-suggestion-text';

    if(suggestion == null) {
      return s;
    }

    if(suggestion.displayAs == null || suggestion.displayAs == '') {
      suggestionText = '\xa0';  // default:  nbsp.
    } else {
      // Default the LTR ordering to match that of the active keyboard.
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

  public readonly events: EventEmitter<SuggestionInputEventMap>;

  private currentSuggestions: Suggestion[] = [];

  private options : BannerSuggestion[] = [];
  private hostDevice: DeviceSpec;

  private manager: SuggestionInputManager;

  private _predictionContext: PredictionContext;

  static readonly TOUCHED_CLASS: string = 'kmw-suggest-touched';
  static readonly BANNER_CLASS: string = 'kmw-suggest-banner';

  constructor(hostDevice: DeviceSpec, height?: number) {
    super(height || Banner.DEFAULT_HEIGHT);
    this.hostDevice = hostDevice;

    this.getDiv().className = this.getDiv().className + ' ' + SuggestionBanner.BANNER_CLASS;

    this.buildInternals(false);

    this.manager = new SuggestionInputManager(this.getDiv());
    this.events = this.manager.events;

    this.setupInputHandling();
  }

  buildInternals(rtl: boolean) {
    if(this.options.length > 0) {
      this.options.splice(0, this.options.length); // Clear the array.
    }
    for (var i=0; i<SuggestionBanner.SUGGESTION_LIMIT; i++) {
      let d = new BannerSuggestion(i, rtl);
      this.options[i] = d;
    }

    /* LTR behavior:  the default (index 0) suggestion should be at the left
      * RTL behavior:  the default (index 0) suggestion should be at the right
      *
      * The cleanest way to make it work - simply invert the order in which
      * the elements are inserted for RTL.  This allows the banner to be RTL
      * for visuals/UI while still being internally LTR.
      */
    for (var i=0; i<SuggestionBanner.SUGGESTION_LIMIT; i++) {
      let indexToInsert = rtl ? SuggestionBanner.SUGGESTION_LIMIT - i -1 : i;
      this.getDiv().appendChild(this.options[indexToInsert].div);

      if(i != SuggestionBanner.SUGGESTION_LIMIT) {
        // Adds a 'separator' div element for UI purposes.
        let separatorDiv = createUnselectableElement('div');
        separatorDiv.className = 'kmw-banner-separator';

        let ds = separatorDiv.style;
        ds.marginLeft = (SuggestionBanner.MARGIN / 2) + '%';
        ds.marginRight = (SuggestionBanner.MARGIN / 2) + '%';

        this.getDiv().appendChild(separatorDiv);
      }
    }
  }

  private setupInputHandling() {
    let inputEngine: InputEventEngine;
    if(this.hostDevice.touchable) { //  /*&& ('ontouchstart' in window)*/ // Except Chrome emulation doesn't set this.
      // Not to mention, it's rather redundant.
      inputEngine = this.touchEventConfig;
    } else {
      inputEngine = this.mouseEventConfig;
    }

    inputEngine.registerEventHandlers();

    this.manager.events.on('highlight', (suggestion, on) => {
      const elem = suggestion.div;
      let classes = elem.className;
      let cs = ' ' + SuggestionBanner.TOUCHED_CLASS;

      if(on && classes.indexOf(cs) < 0) {
        elem.className=classes+cs;
      } else {
        elem.className=classes.replace(cs,'');
      }
    });

    this.manager.events.on('apply', (option) => {
      if(this.predictionContext) {
        this.predictionContext.accept(option.suggestion);
      }
    });
  }

  public configureForKeyboard(keyboard: Keyboard, keyboardProperties: KeyboardProperties) {
    const rtl = keyboard.isRTL;

    // Removes all previous children.  (.replaceChildren requires Chrome for Android 86.)
    // Instantly replaces all children with an empty text node, bypassing the need to actually
    // parse incoming HTML.
    //
    // Just in case, alternative approaches: https://stackoverflow.com/a/3955238
    this.getDiv().textContent = '';

    // Builds new children to match needed RTL properties.
    this.buildInternals(rtl);

    this.options.forEach((option) => option.matchKeyboardProperties(keyboardProperties));
    this.onSuggestionUpdate(this.currentSuggestions); // restore suggestions
  }

  private get mouseEventConfig() {
    const config: InputEventEngineConfig = {
      targetRoot: this.getDiv(),
      // document.body is the event root b/c we need to track the mouse if it leaves
      // the VisualKeyboard's hierarchy.
      eventRoot: document.body,
      inputStartHandler: this.manager.touchStart.bind(this.manager),
      inputMoveHandler:  this.manager.touchMove.bind(this.manager),
      inputEndHandler:   this.manager.touchEnd.bind(this.manager),
      coordConstrainedWithinInteractiveBounds: function() { return true; }
    };

    return new MouseEventEngine(config);
  }

  private get touchEventConfig() {
    const config: InputEventEngineConfig = {
      targetRoot: this.getDiv(),
      // document.body is the event root b/c we need to track the mouse if it leaves
      // the VisualKeyboard's hierarchy.
      eventRoot: this.getDiv(),
      inputStartHandler: this.manager.touchStart.bind(this.manager),
      inputMoveHandler:  this.manager.touchMove.bind(this.manager),
      inputEndHandler:   this.manager.touchEnd.bind(this.manager),
      coordConstrainedWithinInteractiveBounds: function() { return true; }
    };

    return new TouchEventEngine(config);
  }

  public get predictionContext(): PredictionContext {
    return this._predictionContext;
  }

  public set predictionContext(context: PredictionContext) {
    if(this._predictionContext) {
      // disconnect the old one!
      this._predictionContext.off('update', this.onSuggestionUpdate);
    }

    // connect the new one!
    this._predictionContext = context;
    if(context) {
      context.on('update', this.onSuggestionUpdate);
      this.onSuggestionUpdate(context.currentSuggestions);
    }
  }

  public onSuggestionUpdate = (suggestions: Suggestion[]): void => {
    this.currentSuggestions = suggestions;

    this.options.forEach((option: BannerSuggestion, i: number) => {
      if(i < suggestions.length) {
        option.update(suggestions[i]);
      } else {
        option.update(null);
      }
    });
  }
}

interface SuggestionInputEventMap {
  highlight: (bannerSuggestion: BannerSuggestion, state: boolean) => void,
  apply: (bannerSuggestion: BannerSuggestion) => void;
  hold: (bannerSuggestion: BannerSuggestion) => void;
}

class SuggestionInputManager extends UITouchHandlerBase<HTMLDivElement> {
  public readonly events = new EventEmitter<SuggestionInputEventMap>();

  private eventDisablePromise: Promise<any>;

  platformHold: (suggestion: BannerSuggestion, isCustom: boolean) => void;

  //#region Touch handling implementation
  findTargetFrom(e: HTMLElement): HTMLDivElement {
    try {
      if(e) {
        if(e.classList.contains('kmw-suggest-option')) {
          return e as HTMLDivElement;
        }
        if(e.parentElement && e.parentElement.classList.contains('kmw-suggest-option')) {
          return e.parentElement as HTMLDivElement;
        }
        // if(e.firstChild && util.hasClass(<HTMLElement> e.firstChild,'kmw-suggest-option')) {
        //   return e.firstChild as HTMLDivElement;
        // }
      }
    } catch(ex) {}
    return null;
  }

  protected highlight(t: HTMLDivElement, on: boolean): void {
    let suggestion = t['suggestion'] as BannerSuggestion;

    // Never highlight an empty suggestion button.
    if(suggestion.isEmpty()) {
      on = false;
    }

    this.events.emit('highlight', suggestion, on);
  }

  protected select(t: HTMLDivElement): void {
    this.events.emit('apply', t['suggestion'] as BannerSuggestion);
  }

  //#region Long-press support
  protected hold(t: HTMLDivElement): void {
    // let suggestionObj = t['suggestion'] as BannerSuggestion;
    //
    // // Is this the <keep> suggestion?  It's never in this.currentSuggestions, so check against that.
    // let isCustom = this.currentSuggestions.indexOf(suggestionObj.suggestion) == -1;

    this.events.emit('hold', t['suggestion'] as BannerSuggestion);
  }
  protected clearHolds(): void {
    // Temp, pending implementation of suggestion longpress submenus
    // - nothing to clear without them -

    // only really used in native-KMW
  }

  protected hasModalPopup(): boolean {
    return this.eventsBlocked;
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

  public get eventsBlocked(): boolean {
    return !!this.eventDisablePromise;
  }

  /**
   * Intended for use by the mobile apps, which sometimes 'takes over' touch handling.
   * For such cases, input should be blocked within KMW when the apps are managing an
   * ongoing touch-hold for any other interaction.
   *
   * Formerly:
  ```
  let keyman = com.keyman.singleton;
  return keyman['osk'].vkbd.subkeyGesture && keyman.isEmbedded;
  ```
   */
  public temporarilyBlockEvents(promise: Promise<void>) { // TODO:  ensure connection for embedded mode!
    this.eventDisablePromise = promise;                   // Will require routing; this class is not exported!
    promise.finally(() => {
      this.eventDisablePromise = null;
    })
  }

  constructor(div: HTMLElement) {
    // TODO:  Determine appropriate CSS styling names, etc.
    super(div, Banner.BANNER_CLASS, SuggestionBanner.TOUCHED_CLASS);
  }
}
