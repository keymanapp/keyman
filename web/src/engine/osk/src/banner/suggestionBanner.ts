
import { type PredictionContext } from 'keyman/engine/interfaces';
import { createUnselectableElement } from 'keyman/engine/dom-utils';

import {
  GestureRecognizer,
  GestureRecognizerConfiguration,
  GestureSource,
  InputSample,
  PaddedZoneSource,
  RecognitionZoneSource
} from '@keymanapp/gesture-recognizer';

import { BANNER_GESTURE_SET } from './bannerGestureSet.js';

import { DeviceSpec, Keyboard, KeyboardProperties, timedPromise } from 'keyman/engine/keyboard';
import { Banner } from './banner.js';
import { ParsedLengthStyle } from '../lengthStyle.js';
import { getFontSizeStyle } from '../fontSizeUtils.js';
import { getTextMetrics } from '../keyboard-layout/getTextMetrics.js';
import { BannerScrollState } from './bannerScrollState.js';

const TOUCHED_CLASS: string = 'kmw-suggest-touched';
const BANNER_SCROLLER_CLASS = 'kmw-suggest-banner-scroller';

const BANNER_VERT_ROAMING_HEIGHT_RATIO = 0.666;

/**
 * The style to temporarily apply when updating suggestion text in order to prevent
 * fade transitions at that time.
 */
const FADE_SWALLOW_STYLE = 'swallow-fade-transition';

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
    let div = this.div = createUnselectableElement('div');
    div.className = "kmw-suggest-option";
    div.id = BannerSuggestion.BASE_ID + this.index;

    // @ts-ignore // Tags the element with its backing object.
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
        div.style.fontFamily = font.family;
      }
    }
  }

  get suggestion(): Suggestion {
    return this._suggestion;
  }

  /**
   * Function update
   * @param {Suggestion} suggestion   Suggestion from the lexical model
   * @param {BannerSuggestionFormatSpec} format Formatting metadata to use for the Suggestion
   *
   * Update the ID and text of the BannerSuggestionSpec
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

    this.currentWidth = this.collapsedWidth;
    this.highlight(suggestion?.autoAccept);
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

    this.updateFade();
  }

  public updateFade() {
    // Note:  selected suggestion fade transitions are handled purely by CSS.
    // We want to prevent them when updating a suggestion, though.
    this.div.classList.add(FADE_SWALLOW_STYLE);
    // Be sure that our fade-swallow mechanism is able to trigger once;
    // we'll remove it after the current animation frame.
    window.requestAnimationFrame(() => {
      this.div.classList.remove(FADE_SWALLOW_STYLE);
    })

    // Never apply fading to the side that doesn't overflow.
    this.div.classList.add(`kmw-hide-fade-${this.rtl ? 'left' : 'right'}`);

    // Matches the side that overflows, depending on if LTR or RTL.
    const fadeClass = `kmw-hide-fade-${this.rtl ? 'right' : 'left'}`;

    // Is the suggestion already its ideal width?.
    if(!(this.expandedWidth - this.collapsedWidth)) {
      // Yes?  Don't do any fading.
      this.div.classList.add(fadeClass);
    } else {
      this.div.classList.remove(fadeClass);
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

  public highlight(on: boolean) {
    const elem = this.div;

    if(on) {
      elem.classList.add(TOUCHED_CLASS);
    } else {
      elem.classList.remove(TOUCHED_CLASS);
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

  public readonly type = "suggestion";

  private currentSuggestions: Suggestion[] = [];

  private options : BannerSuggestion[] = [];
  private separators: HTMLElement[] = [];

  private isRTL: boolean = false;

  /**
   * The banner 'container', which is also the root element for banner scrolling.
   */
  private readonly container: HTMLElement;
  private highlightAnimation: SuggestionExpandContractAnimation;

  private gestureEngine: GestureRecognizer<BannerSuggestion>;
  private scrollState: BannerScrollState;
  private selectionBounds: RecognitionZoneSource;

  private _predictionContext: PredictionContext;

  constructor(hostDevice: DeviceSpec, height?: number) {
    super(height || Banner.DEFAULT_HEIGHT);

    this.getDiv().className = this.getDiv().className + ' ' + SuggestionBanner.BANNER_CLASS;

    this.container = document.createElement('div');
    this.container.className = BANNER_SCROLLER_CLASS;
    this.getDiv().appendChild(this.container);
    this.buildInternals(false);

    this.gestureEngine = this.setupInputHandling();
  }

  shutdown() {
    this.gestureEngine.destroy();
  }

  buildInternals(rtl: boolean) {
    this.isRTL = rtl;
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
        // Ensure the separators are maintained in the same order as the
        // suggestion elements!
        this.separators[indexToInsert - (rtl ? 1 : 0)] = separatorDiv;
      }
    }
  }

  private setupInputHandling(): GestureRecognizer<BannerSuggestion> {
    // Auto-cancels suggestion-selection if the finger moves too far; having very generous
    // safe-zone settings also helps keep scrolls active on demo pages, etc.
    const safeBounds = new PaddedZoneSource(this.getDiv(), [-Number.MAX_SAFE_INTEGER]);
    this.selectionBounds = new PaddedZoneSource(
      this.getDiv(),
      [-BANNER_VERT_ROAMING_HEIGHT_RATIO * this.height, -Number.MAX_SAFE_INTEGER]
    );

    const config: GestureRecognizerConfiguration<BannerSuggestion> = {
      targetRoot: this.getDiv(),
      maxRoamingBounds: safeBounds,
      safeBounds: safeBounds,
      // touchEventRoot:  this.element, // is the default
      itemIdentifier: (sample) => {
        const selBounds = this.selectionBounds.getBoundingClientRect();

        // Step 1:  is the coordinate within the range we permit for selecting _anything_?
        if(sample.clientX < selBounds.left || sample.clientX > selBounds.right) {
          return null;
        }
        if(sample.clientY < selBounds.top || sample.clientY > selBounds.bottom) {
          return null;
        }

        // Step 2: find the best-matching selection.

        let bestMatch: BannerSuggestion = null;
        let bestDist = Number.MAX_VALUE;

        for(const option of this.options) {
          const optionBounding = option.div.getBoundingClientRect();

          if(optionBounding.left <= sample.clientX && sample.clientX < optionBounding.right) {
            // If there is no backing suggestion, then there's no real selection.
            // May happen when no suggestions are available.
            return option.suggestion ? option : null;
          } else {
            const dist = (sample.clientX < optionBounding.left ? -1 : 1) * (sample.clientX - optionBounding.left);

            if(dist < bestDist) {
              bestDist = dist;
              bestMatch = option;
            }
          }
        }

        // If there is no backing suggestion, then there's no real selection.
        return bestMatch.suggestion ? bestMatch : null;
      }
    };

    const engine = new GestureRecognizer<BannerSuggestion>(BANNER_GESTURE_SET, config);

    const sourceTracker: {
      source: GestureSource<BannerSuggestion>,
      scrollingHandler: (sample: InputSample<BannerSuggestion>) => void,
      suggestion: BannerSuggestion
    } = {
      source: null,
      scrollingHandler: null,
      suggestion: null
    };

    const markSelection = (suggestion: BannerSuggestion) => {
      suggestion.highlight(true);
      if(this.highlightAnimation) {
        this.highlightAnimation.cancel();
        this.highlightAnimation.decouple();
      }

      this.highlightAnimation = new SuggestionExpandContractAnimation(this.container, suggestion, false);
      this.highlightAnimation.expand();
    }

    const clearSelection = (suggestion: BannerSuggestion) => {
      suggestion.highlight(false);
      if(!this.highlightAnimation) {
        this.highlightAnimation = new SuggestionExpandContractAnimation(this.container, suggestion, false);
      }
      this.highlightAnimation.collapse();
    }

    engine.on('inputstart', (source) => {
      // The banner does not support multi-touch - if one is still current, block all others.
      if(sourceTracker.source) {
        source.terminate(true);
        return;
      }

      const autoselection = this._predictionContext.selected;
      this._predictionContext.selected = null;
      if(autoselection) {
        this.options.forEach((entry) => {
          if(entry.suggestion == autoselection) {
            entry.highlight(false);
          };
        });
      }

      this.scrollState = new BannerScrollState(source.currentSample, this.container.scrollLeft);
      const suggestion = source.baseItem;

      sourceTracker.source = source;
      sourceTracker.scrollingHandler = (sample) => {
        const newScrollLeft = this.scrollState.updateTo(sample);
        this.highlightAnimation?.setBaseScroll(newScrollLeft);

          // Only re-enable the original suggestion, even if the touchpoint finds
          // itself over a different suggestion.  Might happen if a scroll boundary
          // is reached.
        const incoming = sample.item ? suggestion : null;

        // It's possible to cancel selection while still scrolling.
        if(incoming != sourceTracker.suggestion) {
          if(sourceTracker.suggestion) {
            clearSelection(sourceTracker.suggestion);
          }

          sourceTracker.suggestion = incoming;
          if(incoming) {
            markSelection(incoming);
          }
        }
      };

      sourceTracker.suggestion = source.currentSample.item;
      if(sourceTracker.suggestion) {
        markSelection(sourceTracker.suggestion);
      }

      const terminationHandler = () => {
        const currentSuggestions = this.currentSuggestions;
        // First, schedule reselection of the autoselected suggestion.
        // We shouldn't do it synchronously, as suggestion acceptance triggers
        // _after_ this handler is called.
        // Delaying via the task queue is enough to get the desired order of events.
        timedPromise(0).then(async () => {
          // If the suggestion list instance has changed, our state has changed; do
          // not reselect.
          if(currentSuggestions != this.currentSuggestions) {
            return;
          }

          // The suggestions are still current?  Then restore the original
          // auto-correct suggestion and its highlighting.
          this._predictionContext.selected = autoselection;
          if(autoselection) {
            for(let entry of this.options) {
              if(entry.suggestion == autoselection) {
                entry.highlight(true);
                break;
              };
            }
          }
        });

        if(sourceTracker.suggestion) {
          clearSelection(sourceTracker.suggestion);
          sourceTracker.suggestion = null;
        }

        sourceTracker.source = null;
        sourceTracker.scrollingHandler = null;
      }

      source.path.on('complete', terminationHandler);
      source.path.on('invalidated', terminationHandler);
      source.path.on('step', sourceTracker.scrollingHandler);
    });

    engine.on('recognizedgesture', (sequence) => {
      // The actual result comes in via the sequence's `stage` event.
      sequence.once('stage', (result) => {
        const suggestion = result.item; // Should also == sourceTracker.suggestion.
        // 1. A valid suggestion has been selected
        // 2. The user wasn't scrolling the banner.  (If they were, they likely
        //    need to lift their finger to select a newly-visible suggestion!)
        // 3. The suggestions themselves are still valid; avoid suggestion
        //    double-application or similar.
        if(suggestion && !this.scrollState.hasScrolled && this.currentSuggestions.length > 0) {
          // Invalidate the suggestions internally, but don't visually update;
          // this will avoid banner-flicker.
          this.currentSuggestions = [];
          this.predictionContext.accept(suggestion.suggestion).then(() => {
            // Reset the scroll state
            this.container.scrollLeft = this.isRTL ? this.container.scrollWidth : 0;
          });
        }

        this.scrollState = null;
      });
    });

    return engine;
  }

  protected update() {
    const result = super.update();

    // Ensure the banner's extended recognition zone is based on proper, up-to-date layout info.
    // Note:  during banner init, `this.gestureEngine` may only be defined after
    // the first call to this setter!
    (this.selectionBounds as PaddedZoneSource)?.updatePadding(
      [-BANNER_VERT_ROAMING_HEIGHT_RATIO * this.height, -Number.MAX_SAFE_INTEGER]
    );

    return result;
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
    // Immediately stop all animations and reset options accordingly.
    this.highlightAnimation?.cancel();

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

    for (let i=0; i<SuggestionBanner.SUGGESTION_LIMIT; i++) {
      const d = this.options[i];

      if(suggestions.length > i) {
        const suggestion = suggestions[i];
        d.update(suggestion, optionFormat);
      } else {
        d.update(null, optionFormat);
      }
    }

    this.refreshLayout();
  }

  readonly refreshLayout = () => {
    let collapsedOptions: BannerSuggestion[] = [];
    let totalWidth = 0;

    let displayCount = Math.min(this.currentSuggestions.length, 8);
    for(let i=0; i < displayCount; i++) {
      // Note:  options is an array of pre-built suggestion-hosting elements, with
      // fixed SUGGESTIONS_LIMIT length - not a length that dynamically changes to
      // match the number of suggestions available.  Those without a suggestion
      // are hidden, but preserved.
      const opt = this.options[i];
      opt.minWidth = 0; // remove any previously-applied padding
      totalWidth += opt.collapsedWidth;

      if(opt.collapsedWidth < opt.expandedWidth) {
        collapsedOptions.push(opt);
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
    this.cancel();
    this.scrollContainer = null;
  }

  private clear() {
    this.startTimestamp = null;
    window.cancelAnimationFrame(this.pendingAnimation);
    this.pendingAnimation = null;
  }

  cancel() {
    this.clear();
    this.option.currentWidth = this.option.collapsedWidth;
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

