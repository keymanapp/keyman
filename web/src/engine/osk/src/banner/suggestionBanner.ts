
import { type PredictionContext } from '@keymanapp/input-processor';
import { createUnselectableElement } from 'keyman/engine/dom-utils';

import InputEventEngine, { InputEventEngineConfig } from '../input/event-interpreter/inputEventEngine.js';
import MouseEventEngine from '../input/event-interpreter/mouseEventEngine.js';
import TouchEventEngine from '../input/event-interpreter/touchEventEngine.js';
import UITouchHandlerBase from '../input/event-interpreter/uiTouchHandlerBase.js';
import { DeviceSpec, Keyboard, KeyboardProperties } from '@keymanapp/keyboard-processor';
import { Banner } from './banner.js';
import EventEmitter from 'eventemitter3';
import { ParsedLengthStyle } from '../lengthStyle.js';
import { getFontSizeStyle } from '../fontSizeUtils.js';
import { getTextMetrics } from '../keyboard-layout/getTextMetrics.js';

// TODO:  finalize + document
interface OptionFormatSpec {
  minWidth?: number;
  paddingWidth: number,
  emSize: number,
  styleForFont: CSSStyleDeclaration

  collapsedWidth?: number
}
export class BannerSuggestion {
  div: HTMLDivElement;
  container: HTMLDivElement;
  private display: HTMLSpanElement;

  private _collapsedWidth: number;
  private _textWidth: number;
  private _minWidth: number;
  private _paddingWidth: number;

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
    display.className = 'kmw-suggestion-text';
    this.container.appendChild(display);
  }

  private constructRoot() {
    // Add OSK suggestion labels
    let div = this.div = createUnselectableElement('div'), ds=div.style;
    div.className = "kmw-suggest-option";
    div.id = BannerSuggestion.BASE_ID + this.index;

    this.div['suggestion'] = this;

    let container = this.container = document.createElement('div');
    container.className = "kmw-suggestion-container";

    // Ensures that a reasonable default width, based on % is set. (Since it's not yet in the DOM, we may not yet have actual width info.)
    let usableWidth = 100 - SuggestionBanner.MARGIN * (SuggestionBanner.SUGGESTION_LIMIT - 1);

    // The `/ 2` part:  Ensures that the full banner is double-wide, which is useful for demoing scrolling.
    let widthpc = usableWidth / (SuggestionBanner.SUGGESTION_LIMIT / 2);
    container.style.minWidth = widthpc + '%';

    div.appendChild(container);
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
   * @param fontStyle                 The CSS styling expected for the suggestion text
   * @param emSize                    The font size represented by 1em (in px, as from getComputedStyle on document.body)
   * @param targetWidth
   * @param collapsedTargetWidth
   * Description  Update the ID and text of the BannerSuggestionSpec
   */
  public update(suggestion: Suggestion, format: OptionFormatSpec) {
    this._suggestion = suggestion;

    let display = this.generateSuggestionText(this.rtl);
    this.container.replaceChild(display, this.display);
    this.display = display;

    // Set internal properties for use in format calculations.
    if(format.minWidth !== undefined) {
      this._minWidth = format.minWidth;
    }

    this._paddingWidth = format.paddingWidth;
    this._collapsedWidth = format.collapsedWidth;

    if(suggestion && suggestion.displayAs) {
      const rawMetrics = getTextMetrics(suggestion.displayAs, format.emSize, format.styleForFont);
      this._textWidth = rawMetrics.width;
    } else {
      this._textWidth = 0;
    }

    this.updateLayout();
  }

  public updateLayout() {
    if(!this.suggestion && this.index != 0) {
      this.div.style.width='0px';
      return;
    } else {
      this.div.style.width='';
    }

    // TODO:  if the option is highlighted, maybe don't disable transitions?
    this.container.style.transition = 'none'; // temporarily disable transition effects.

    const collapserStyle = this.container.style;
    collapserStyle.minWidth = this.collapsedWidth + 'px';

    if(this.rtl) {
      collapserStyle.marginRight = (this.collapsedWidth - this.expandedWidth) + 'px';
    } else {
      collapserStyle.marginLeft  = (this.collapsedWidth - this.expandedWidth) + 'px';
    }

    this.container.offsetWidth; // To 'flush' the changes before re-enabling transition animations.
    this.container.offsetLeft;

    this.container.style.transition = ''; // Re-enable them (it's set on the element's class)
  }



  public get targetCollapsedWidth(): number {
    return this._collapsedWidth;
  }

  public get textWidth(): number {
    return this._textWidth;
  }

  public get paddingWidth(): number {
    return this._paddingWidth;
  }

  public get minWidth(): number {
    return this._minWidth;
  }

  public set minWidth(val: number) {
    this._minWidth = val;
  }

  public get expandedWidth(): number {
    // minWidth must be defined AND greater for the conditional to return this.minWidth.
    return this.minWidth > this.spanWidth ? this.minWidth : this.spanWidth;
  }

  public get spanWidth(): number {
    let spanWidth = this.textWidth ?? 0;
    if(spanWidth) {
      spanWidth += this.paddingWidth ?? 0;
    }

    return spanWidth;
  }

  public get collapsedWidth(): number {
    let maxWidth = this.targetCollapsedWidth < this.expandedWidth ? this.targetCollapsedWidth : this.expandedWidth;

    // Will return maxWidth if this.minWidth is undefined.
    return (this.minWidth > maxWidth ? this.minWidth : maxWidth);
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
  public static readonly SUGGESTION_LIMIT: number = 6;
  public static readonly MARGIN = 1;

  public readonly events: EventEmitter<SuggestionInputEventMap>;

  private currentSuggestions: Suggestion[] = [];

  private options : BannerSuggestion[] = [];
  private separators: HTMLElement[] = [];

  private hostDevice: DeviceSpec;

  private manager: SuggestionInputManager;
  private readonly container: HTMLElement;

  readonly type = 'suggestion';

  private _predictionContext: PredictionContext;

  static readonly TOUCHED_CLASS: string = 'kmw-suggest-touched';
  static readonly BANNER_CLASS: string = 'kmw-suggest-banner';
  static readonly BANNER_SCROLLER_CLASS = 'kmw-suggest-banner-scroller';

  constructor(hostDevice: DeviceSpec, height?: number) {
    super(height || Banner.DEFAULT_HEIGHT);
    this.hostDevice = hostDevice;

    this.getDiv().className = this.getDiv().className + ' ' + SuggestionBanner.BANNER_CLASS;

    this.container = document.createElement('div');
    this.container.className = SuggestionBanner.BANNER_SCROLLER_CLASS;
    this.getDiv().appendChild(this.container);
    // TODO:  additional styling for the banner scroll container?

    this.buildInternals(false);

    this.manager = new SuggestionInputManager(this.container, this.container);
    this.events = this.manager.events;

    this.setupInputHandling();
  }

  buildInternals(rtl: boolean) {
    if(this.options.length > 0) {
      this.options = [];
      this.separators = [];
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
      this.container.appendChild(this.options[indexToInsert].div);

      if(i != SuggestionBanner.SUGGESTION_LIMIT - 1) {
        // Adds a 'separator' div element for UI purposes.
        let separatorDiv = createUnselectableElement('div');
        separatorDiv.className = 'kmw-banner-separator';

        let ds = separatorDiv.style;
        ds.marginLeft = `calc(${(SuggestionBanner.MARGIN / 2)}% - 0.5px)`;
        ds.marginRight = `calc(${(SuggestionBanner.MARGIN / 2)}% - 0.5px)`;

        this.container.appendChild(separatorDiv);
        this.separators.push(separatorDiv);
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
    this.container.textContent = '';

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

    const fontStyle = getComputedStyle(this.options[0].div);
    const emSizeStr = getComputedStyle(document.body).fontSize;
    const emSize    = getFontSizeStyle(emSizeStr).val;

    const textStyle = getComputedStyle(this.options[0].container.firstChild as HTMLSpanElement);

    // TODO:  polish up; do a calculation that leaves perfect, clean edges when displaying exactly three options.
    const targetWidth = this.width / 3; // Not fancy; it'll leave rough edges. But... it'll do for a demo.
    const textLeftPad = new ParsedLengthStyle(textStyle.paddingLeft   || '2px');   // computedStyle will fail if the element's not in the DOM yet.
    const textRightPad = new ParsedLengthStyle(textStyle.paddingRight || '2px');

    let optionFormat: OptionFormatSpec = {
      paddingWidth: textLeftPad.val + textRightPad.val, // Assumes fixed px padding.
      emSize: emSize,
      styleForFont: fontStyle,
      collapsedWidth: targetWidth,
      minWidth: 0,
    }

    let totalWidth = 0;
    let displayCount = 0;

    for (let i=0; i<SuggestionBanner.SUGGESTION_LIMIT; i++) {
      const d = this.options[i];

      if(suggestions.length > i) {
        const suggestion = suggestions[i];
        d.update(suggestion, optionFormat);

        totalWidth += d.collapsedWidth;
        displayCount++;
      } else {
        d.update(null, optionFormat);
      }
    }

    // Ensure one suggestion is always displayed, even if empty.  (Keep the separators out)
    displayCount = displayCount || 1;

    if(totalWidth < this.width) {
      let separatorWidth = (this.width * 0.01 * (displayCount-1));
      let fillPadding = (this.width - totalWidth - separatorWidth) / displayCount;

      for(let i=0; i < displayCount; i++) {
        const d = this.options[i];

        d.minWidth = d.collapsedWidth + fillPadding;
        d.updateLayout();
      }
    }

    // Hide any separators beyond the final displayed suggestion
    for(let i=0; i < SuggestionBanner.SUGGESTION_LIMIT - 1; i++) {
      this.separators[i].style.display = i < displayCount - 1 ? '' : 'none';
    }
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
        const parent = e.parentElement;
        if(!parent) {
          return null;
        }

        if(parent.classList.contains('kmw-suggest-option')) {
          return parent as HTMLDivElement;
        }

        const grandparent = parent.parentElement;
        if(!grandparent) {
          return null;
        }

        if(grandparent.classList.contains('kmw-suggest-option')) {
          return grandparent as HTMLDivElement;
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

  constructor(div: HTMLElement, scroller: HTMLElement) {
    // TODO:  Determine appropriate CSS styling names, etc.
    super(div, scroller, Banner.BANNER_CLASS, SuggestionBanner.TOUCHED_CLASS);
  }
}
