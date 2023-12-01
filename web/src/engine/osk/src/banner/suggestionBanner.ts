
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

/**
 * Defines various parameters used by `BannerSuggestion` instances for layout and formatting.
 * This object is designed first and foremost for use with `BannerSuggestion.update()`.
 */
interface BannerSuggestionFormatSpec {
  /**
   * Sets a minimum width to use for the `BannerSuggestion`'s element; this overrides any
   * and all settings that would otherwise result in a narrower final width.
   */
  minWidth?: number;

  /**
   * Sets the width of padding around the text of each suggestion.  This should generally match
   * the 'width' of class = `.kmw-suggest-option::before` and class = `.kmw-suggest-option::after`
   * elements as defined in kmwosk.css.
   */
  paddingWidth: number,

  /**
   * The default font size to use for calculations based on relative font-size specs
   */
  emSize: number,

  /**
   * The font style (font-size, font-family) to use for suggestion-banner display text.
   */
  styleForFont: {
    fontSize:   typeof CSSStyleDeclaration.prototype.fontSize,
    fontFamily: typeof CSSStyleDeclaration.prototype.fontFamily
  },

  /**
   * Sets a target width to use when 'collapsing' suggestions.  Only affects those long
   * enough to need said 'collapsing'.
   */
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
  public readonly rtl: boolean;

  private _suggestion: Suggestion;

  private index: number;

  static readonly BASE_ID = 'kmw-suggestion-';

  constructor(index: number, isRTL: boolean) {
    this.index = index;
    this.rtl = isRTL ?? false;

    this.constructRoot();

    // Provides an empty, base SPAN for text display.  We'll swap these out regularly;
    // `Suggestion`s will have varying length and may need different styling.
    let display = this.display = createUnselectableElement('span');
    display.className = 'kmw-suggestion-text';
    this.container.appendChild(display);
  }

  get computedStyle() {
    return getComputedStyle(this.display);
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
    let usableWidth = 100 - SuggestionBanner.MARGIN * (SuggestionBanner.LONG_SUGGESTION_DISPLAY_LIMIT - 1);

    let widthpc = usableWidth / (SuggestionBanner.LONG_SUGGESTION_DISPLAY_LIMIT);
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
  public update(suggestion: Suggestion, format: BannerSuggestionFormatSpec) {
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

    const collapserStyle = this.container.style;
    collapserStyle.minWidth = this.collapsedWidth + 'px';

    if(this.rtl) {
      collapserStyle.marginRight = (this.collapsedWidth - this.expandedWidth) + 'px';
    } else {
      collapserStyle.marginLeft  = (this.collapsedWidth - this.expandedWidth) + 'px';
    }
  }

  /**
   * Denotes the threshold at which the banner suggestion will no longer gain width
   * in its default form, resulting in two separate states:  "collapsed" and "expanded".
   */
  public get targetCollapsedWidth(): number {
    return this._collapsedWidth;
  }

  /**
   * The raw width needed to display the suggestion's display text without triggering overflow.
   */
  public get textWidth(): number {
    return this._textWidth;
  }

  /**
   * Width of the padding to apply equally on both sides of the suggestion's display text.
   * Is the sum of both, rather than the value applied to each side.
   */
  public get paddingWidth(): number {
    return this._paddingWidth;
  }

  /**
   * The absolute minimum width to allow for the represented suggestion's banner element.
   */
  public get minWidth(): number {
    return this._minWidth;
  }

  /**
   * The absolute minimum width to allow for the represented suggestion's banner element.
   */
  public set minWidth(val: number) {
    this._minWidth = val;
  }

  /**
   * The total width taken by the suggestion's banner element when fully expanded.
   * This may equal the `collapsed` width for sufficiently short suggestions.
   */
  public get expandedWidth(): number {
    // minWidth must be defined AND greater for the conditional to return this.minWidth.
    return this.minWidth > this.spanWidth ? this.minWidth : this.spanWidth;
  }

  /**
   * The total width used by the internal contents of the suggestion's banner element when not obscured.
   */
  public get spanWidth(): number {
    let spanWidth = this.textWidth ?? 0;
    if(spanWidth) {
      spanWidth += this.paddingWidth ?? 0;
    }

    return spanWidth;
  }

  /**
   * The actual width to be used for the `BannerSuggestion`'s display element when in the 'collapsed'
   * state and not transitioning.
   */
  public get collapsedWidth(): number {
    // Allow shrinking a suggestion's width if it has excess whitespace.
    let utilizedWidth = this.spanWidth < this.targetCollapsedWidth ? this.spanWidth : this.targetCollapsedWidth;
    // If a minimum width has been specified, enforce that minimum.
    let maxWidth = utilizedWidth < this.expandedWidth ? utilizedWidth : this.expandedWidth;

    // Will return maxWidth if this.minWidth is undefined.
    return (this.minWidth > maxWidth ? this.minWidth : maxWidth);
  }

  /**
   * The actual width currently utilized by the `BannerSuggestion`'s display element, regardless of
   * current state.
   */
  public get currentWidth(): number {
    return this.div.offsetWidth;
  }

  /**
   * The actual width currently utilized by the `BannerSuggestion`'s display element, regardless of
   * current state.
   */
  public set currentWidth(val: number) {
    // TODO:  probably should set up errors or something here...
    if(val < this.collapsedWidth) {
      val = this.collapsedWidth;
    } else if(val > this.expandedWidth) {
      val = this.expandedWidth;
    }

    if(this.rtl) {
      this.container.style.marginRight = `${val - this.expandedWidth}px`;
    } else {
      this.container.style.marginLeft = `${val - this.expandedWidth}px`;
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
  public static readonly SUGGESTION_LIMIT: number = 8;
  public static readonly LONG_SUGGESTION_DISPLAY_LIMIT: number = 3;
  public static readonly MARGIN = 1;

  public readonly events: EventEmitter<SuggestionInputEventMap>;

  private currentSuggestions: Suggestion[] = [];

  private options : BannerSuggestion[] = [];
  private separators: HTMLElement[] = [];

  private hostDevice: DeviceSpec;

  private manager: SuggestionInputManager;
  private readonly container: HTMLElement;
  private highlightAnimation: SuggestionExpandContractAnimation;

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
      let indexToInsert = rtl ? SuggestionBanner.SUGGESTION_LIMIT - i - 1 : i;
      this.container.appendChild(this.options[indexToInsert].div);

      // RTL should start right-aligned, thus @ max scroll.
      if(rtl) {
        this.container.scrollLeft = this.container.scrollWidth;
      }

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
        if(this.highlightAnimation) {
          this.highlightAnimation.decouple();
        }

        this.highlightAnimation = new SuggestionExpandContractAnimation(this.container, suggestion, false);
        this.highlightAnimation.expand();
      } else {
        elem.className=classes.replace(cs,'');
        if(!this.highlightAnimation) {
          this.highlightAnimation = new SuggestionExpandContractAnimation(this.container, suggestion, false);
        }
        this.highlightAnimation.collapse();
      }
    });

    this.manager.events.on('scrollLeft', (val) => {
      if(this.highlightAnimation) {
        this.highlightAnimation.setBaseScroll(val);
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

  /**
   * Produces a closure useful for updating the SuggestionBanner's UI to match newly-received
   * suggestions, including optimization of the banner's layout.
   * @param suggestions
   */
  public onSuggestionUpdate = (suggestions: Suggestion[]): void => {
    this.currentSuggestions = suggestions;

    const fontStyleBase = this.options[0].computedStyle;
    // Do NOT just re-use the returned object from the line above; it may spontaneously change
    // (in a bad way) when the underlying span is replaced!
    const fontStyle = {
      fontSize: fontStyleBase.fontSize,
      fontFamily: fontStyleBase.fontFamily
    }
    const emSizeStr = getComputedStyle(document.body).fontSize;
    const emSize    = getFontSizeStyle(emSizeStr).val;

    const textStyle = getComputedStyle(this.options[0].container.firstChild as HTMLSpanElement);

    const targetWidth = this.width / SuggestionBanner.LONG_SUGGESTION_DISPLAY_LIMIT;

    // computedStyle will fail if the element's not in the DOM yet.
    // Seeks to get the values specified within kmwosk.css.
    const textLeftPad = new ParsedLengthStyle(textStyle.paddingLeft   || '4px');
    const textRightPad = new ParsedLengthStyle(textStyle.paddingRight || '4px');

    let optionFormat: BannerSuggestionFormatSpec = {
      paddingWidth: textLeftPad.val + textRightPad.val, // Assumes fixed px padding.
      emSize: emSize,
      styleForFont: fontStyle,
      collapsedWidth: targetWidth,
      minWidth: 0,
    }

    let totalWidth = 0;
    let displayCount = 0;

    let collapsedOptions: BannerSuggestion[] = [];

    for (let i=0; i<SuggestionBanner.SUGGESTION_LIMIT; i++) {
      const d = this.options[i];

      if(suggestions.length > i) {
        const suggestion = suggestions[i];
        d.update(suggestion, optionFormat);
        if(d.collapsedWidth < d.expandedWidth) {
          collapsedOptions.push(d);
        }

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
      // Prioritize adding padding to suggestions that actually need it.
      // Use equal measure for each so long as it still could use extra display space.
      while(totalWidth < this.width && collapsedOptions.length > 0) {
        let maxFillPadding = (this.width - totalWidth - separatorWidth) / collapsedOptions.length;
        collapsedOptions.sort((a, b) => a.expandedWidth - b.expandedWidth);

        let shortestCollapsed = collapsedOptions[0];
        let neededWidth = shortestCollapsed.expandedWidth - shortestCollapsed.collapsedWidth;

        let padding = Math.min(neededWidth, maxFillPadding);

        // Check: it is possible that two elements were matched for equal length, thus the second loop's takes no additional padding.
        // No need to trigger re-layout ops for that case.
        if(padding > 0) {
          collapsedOptions.forEach((a) => a.minWidth = a.collapsedWidth + padding);
          totalWidth += padding * collapsedOptions.length;  // don't forget to record that we added the padding!
        }

        collapsedOptions.splice(0, 1);  // discard the element we based our judgment upon; we need not consider it any longer.
      }

      // If there's STILL leftover padding to distribute, let's do that now.
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
  scrollLeft: (val: number) => void;
}


class SuggestionExpandContractAnimation {
  private scrollContainer: HTMLElement | null;
  private option: BannerSuggestion;

  private collapsedScrollOffset: number;
  private rootScrollOffset: number;

  private startTimestamp: number;
  private pendingAnimation: number;

  private static TRANSITION_TIME = 250; // in ms.

  constructor(scrollContainer: HTMLElement, option: BannerSuggestion, forRTL: boolean) {
    this.scrollContainer = scrollContainer;
    this.option = option;
    this.collapsedScrollOffset = scrollContainer.scrollLeft;
    this.rootScrollOffset  = scrollContainer.scrollLeft;
  }

  public setBaseScroll(val: number) {
    this.collapsedScrollOffset = val;

    // If the user has shifted the scroll position to make more of the element visible, we can remove part
    // of the corresponding scrolling offset permanently; the user's taken action to view that area.
    if(this.option.rtl) {
      // A higher scrollLeft (scrolling right) will reveal more of an initially-clipped suggestion.
      if(val > this.rootScrollOffset) {
        this.rootScrollOffset = val;
      }
    } else {
      // Here, a lower scrollLeft (scrolling left).
      if(val < this.rootScrollOffset) {
        this.rootScrollOffset = val;
      }
    }

    // Synchronize the banner-scroller's offset update with that of the
    // animation for expansion and collapsing.
    window.requestAnimationFrame(this.setScrollOffset);
  }

  /**
   * Performs mapping of the user's touchpoint to properly-offset scroll coordinates based on
   * the state of the ongoing scroll operation.
   *
   * First priority:  this function aims to keep all currently-visible parts of a selected
   * suggestion visible when first selected.  Any currently-clipped parts will remain clipped.
   *
   * Second priority:  all animations should be smooth and continuous; aesthetics do matter to
   * users.
   *
   * Third priority:  when possible without violating the first two priorities, this (in tandem with
   * adjustments within `setBaseScroll`) will aim to sync the touchpoint with its original
   * location on an expanded suggestion.
   * - For LTR languages, this means that suggestions will "expand left" if possible.
   * - While for RTL languages, they will "expand right" if possible.
   * - However, if they would expand outside of the banner's effective viewport, a scroll offset
   *   will kick in to enforce the "first priority" mentioned above.
   *   - This "scroll offset" will be progressively removed (because second priority) if and as
   *     the user manually scrolls to reveal relevant space that was originally outside of the viewport.
   *
   * @returns
   */
  private setScrollOffset = () => {
    // If we've been 'decoupled', a different instance (likely for a different suggestion)
    // is responsible for counter-scrolling.
    if(!this.scrollContainer) {
      return;
    }

    // -- Clamping / "scroll offset" logic --

    // As currently written / defined below, and used internally within this function, "clamping"
    // refers to alterations to scroll-positioned mapping designed to keep as much of the expanded
    // option visible as possible via the offsets below (that is, "clamped" to the relevant border)
    // while not adding extra discontinuity by pushing already-obscured parts of the expanded option
    // into visible range.
    //
    // In essence, it's an extra "scroll offset" we apply that is dynamically adjusted depending on
    // scroll position as it changes. This offset may be decreased when it is no longer needed to
    // make parts of the element visible.

    // The amount of extra space being taken by a partially or completely expanded suggestion.
    const maxWidthToCounterscroll = this.option.currentWidth - this.option.collapsedWidth;
    const rtl = this.option.rtl;

    // If non-zero, indicates the pixel-width of the collapsed form of the suggestion clipped by the relevant screen border.
    const ltrOverflow = Math.max(this.rootScrollOffset - this.option.div.offsetLeft, 0);
    const rtlOverflow = Math.max(this.option.div.offsetLeft + this.option.collapsedWidth - (this.rootScrollOffset + this.scrollContainer.offsetWidth));

    const srcCounterscrollOverflow = Math.max(rtl ? rtlOverflow : ltrOverflow, 0);  // positive offset into overflow-land.

    // Base position for scrollLeft clamped within std element scroll bounds, including:
    // - an adjustment to cover the extra width from expansion
    // - preserving the base expected overflow levels
    // Does NOT make adjustments to force extra visibility on the element being highlighted/focused.
    const unclampedExpandingScrollOffset = Math.max(this.collapsedScrollOffset + (rtl ? 0 : 1) * maxWidthToCounterscroll, 0) + (rtl ? 0 : -1) * srcCounterscrollOverflow;
    // The same, but for our 'root scroll coordinate'.
    const rootUnclampedExpandingScrollOffset = Math.max(this.rootScrollOffset + (rtl ? 0 : 1) * maxWidthToCounterscroll, 0) + (rtl ? 0 : -1) * srcCounterscrollOverflow;

    // Do not shift an element clipped by the screen border further than its original scroll starting point.
    const elementOffsetForClamping = rtl
      ? Math.max(unclampedExpandingScrollOffset, rootUnclampedExpandingScrollOffset)
      : Math.min(unclampedExpandingScrollOffset, rootUnclampedExpandingScrollOffset);

    // Based on the scroll point selected, determine how far to offset scrolls to keep the option in visible range.
    // Higher .scrollLeft values make this non-zero and reflect when scroll has begun clipping the element.
    const elementOffsetFromBorder = rtl
      // RTL offset:                   "offsetRight"                                       based on "scrollRight"
      ? Math.max(this.option.div.offsetLeft + this.option.currentWidth - (elementOffsetForClamping + this.scrollContainer.offsetWidth), 0) // double-check this one.
      // LTR:       based on scrollLeft            offsetLeft
      : Math.max(elementOffsetForClamping - this.option.div.offsetLeft, 0);

    // If the element is close enough to the border, don't offset beyond the element!
    // If it is further, do not add excess padding - it'd effectively break scrolling.
    // Do maintain any remaining scroll offset that exists, though.
    const clampedExpandingScrollOffset = Math.min(maxWidthToCounterscroll, elementOffsetFromBorder);

    const finalScrollOffset = unclampedExpandingScrollOffset                   // base scroll-coordinate transform mapping based on extra width from element expansion
                              + (rtl ? 1 : -1) * clampedExpandingScrollOffset  // offset to scroll to put word-start border against the corresponding screen border, fully visible
                              + (rtl ? 0 :  1) * srcCounterscrollOverflow;     // offset to maintain original overflow past that border if it existed

    // -- Final step: Apply & fine-tune the final scroll positioning --
    this.scrollContainer.scrollLeft = finalScrollOffset;

    // Prevent "jitters" during counterscroll that occur on expansion / collapse animation.
    // A one-frame "error correction" effect at the end of animation is far less jarring.
    if(this.pendingAnimation) {
      // scrollLeft doesn't work well with fractional values, unlike marginLeft / marginRight
      const fractionalOffset = this.scrollContainer.scrollLeft - finalScrollOffset;
      // So we put the fractional difference into marginLeft to force it to sync.
      this.option.currentWidth += fractionalOffset;
    }
  }

  public decouple() {
    this.scrollContainer = null;
  }

  private clear() {
    this.startTimestamp = null;
    window.cancelAnimationFrame(this.pendingAnimation);
    this.pendingAnimation = null;
  }

  public expand() {
    // Cancel any prior iterating animation-frame commands.
    this.clear();

    // set timestamp, adjusting the current time based on intermediate progress
    this.startTimestamp = performance.now();

    let progress = this.option.currentWidth - this.option.collapsedWidth;
    let expansionDiff = this.option.expandedWidth - this.option.collapsedWidth;

    if(progress != 0) {
      // Offset the timestamp by noting what start time would have given rise to
      // the current position, keeping related animations smooth.
      this.startTimestamp -= (progress / expansionDiff) * SuggestionExpandContractAnimation.TRANSITION_TIME;
    }

    this.pendingAnimation = window.requestAnimationFrame(this._expand);
  }

  private _expand = (timestamp: number) => {
    if(this.startTimestamp === undefined) {
      return; // No active expand op exists.  May have been cancelled via `clear`.
    }

    let progressTime = timestamp - this.startTimestamp;
    let fin = progressTime > SuggestionExpandContractAnimation.TRANSITION_TIME;

    if(fin) {
      progressTime = SuggestionExpandContractAnimation.TRANSITION_TIME;
    }

    // -- Part 1:  handle option expand / collapse state --
    let expansionDiff = this.option.expandedWidth - this.option.collapsedWidth;
    let expansionRatio = progressTime / SuggestionExpandContractAnimation.TRANSITION_TIME;

    // expansionDiff * expansionRatio:  the total adjustment from 'collapsed' width, in px.
    const expansionPx = expansionDiff * expansionRatio;
    this.option.currentWidth = expansionPx + this.option.collapsedWidth;

    // Part 2:  trigger the next animation frame.
    if(!fin) {
      this.pendingAnimation = window.requestAnimationFrame(this._expand);
    } else {
      this.clear();
    }

    // Part 3:  perform any needed counter-scrolling, scroll clamping, etc
    // Existence of a followup animation frame is part of the logic, so keep this 'after'!
    this.setScrollOffset();
  };

  public collapse() {
    // Cancel any prior iterating animation-frame commands.
    this.clear();

    // set timestamp, adjusting the current time based on intermediate progress
    this.startTimestamp = performance.now();

    let progress = this.option.expandedWidth - this.option.currentWidth;
    let expansionDiff = this.option.expandedWidth - this.option.collapsedWidth;

    if(progress != 0) {
      // Offset the timestamp by noting what start time would have given rise to
      // the current position, keeping related animations smooth.
      this.startTimestamp -= (progress / expansionDiff) * SuggestionExpandContractAnimation.TRANSITION_TIME;
    }

    this.pendingAnimation = window.requestAnimationFrame(this._collapse);
  }

  private _collapse = (timestamp: number) => {
    if(this.startTimestamp === undefined) {
      return; // No active collapse op exists.  May have been cancelled via `clear`.
    }

    let progressTime = timestamp - this.startTimestamp;
    let fin = progressTime > SuggestionExpandContractAnimation.TRANSITION_TIME;
    if(fin) {
      progressTime = SuggestionExpandContractAnimation.TRANSITION_TIME;
    }

    // -- Part 1:  handle option expand / collapse state --
    let expansionDiff = this.option.expandedWidth - this.option.collapsedWidth;
    let expansionRatio = 1 - progressTime / SuggestionExpandContractAnimation.TRANSITION_TIME;

    // expansionDiff * expansionRatio:  the total adjustment from 'collapsed' width, in px.
    const expansionPx = expansionDiff * expansionRatio;
    this.option.currentWidth = expansionPx + this.option.collapsedWidth;

    // Part 2:  trigger the next animation frame.
    if(!fin) {
      this.pendingAnimation = window.requestAnimationFrame(this._collapse);
    } else {
      this.clear();
    }

    // Part 3:  perform any needed counter-scrolling, scroll clamping, etc
    // Existence of a followup animation frame is part of the logic, so keep this 'after'!
    this.setScrollOffset();
  };
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

  protected onScrollLeftUpdate(val: number): void {
    this.events.emit('scrollLeft', val);
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
