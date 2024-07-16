import { EventEmitter } from "eventemitter3";
import type LanguageProcessor from "./languageProcessor.js";
import { type ReadySuggestions, type InvalidateSourceEnum, StateChangeHandler } from './languageProcessor.js';
import { type KeyboardProcessor, type OutputTarget } from "@keymanapp/keyboard-processor";

interface PredictionContextEventMap {
  update: (suggestions: Suggestion[]) => void;
}

/**
 * Maintains predictive-text state information corresponding to the current context.
 */
export default class PredictionContext extends EventEmitter<PredictionContextEventMap> {
  // Historical note:  before 17.0, this code was intertwined with /web/source/osk/banner.ts's
  // SuggestionBanner class.  This class serves as the main implementation of the banner's core logic.

  // Designed for use with auto-correct behavior
  selected: Suggestion;

  private initNewContext: boolean = true;

  private _currentSuggestions: Suggestion[] = [];
  private keepSuggestion: Keep;
  private revertSuggestion: Reversion;

  // Set to null/undefined if there was no recent acceptance.
  private recentAcceptCause: 'key' | 'banner';
  private revertAcceptancePromise: Promise<Reversion>;

  private swallowPrediction: boolean = false;

  private doRevert: boolean = false;
  private recentRevert: boolean = false;

  private langProcessor: LanguageProcessor;
  private kbdProcessor: KeyboardProcessor;

  /**
   * Represents the active context used when requesting and applying predictive-text operations.
   */
  private _currentTarget: OutputTarget;

  public get currentTarget(): OutputTarget {
    return this._currentTarget;
  }

  public setCurrentTarget(target: OutputTarget): Promise<Suggestion[]> {
    const originalTarget = this._currentTarget;
    this._currentTarget = target;

    if(originalTarget != target) {
      // Note:  should be triggered after the corresponding new-context event rule has been processed,
      // as that may affect the value of layerId here.
      return this.resetContext();
    } else {
      return Promise.resolve([]);
    }
  }

  private readonly suggestionApplier: (suggestion: Suggestion) => Promise<Reversion>;
  private readonly suggestionReverter: (reversion: Reversion) => void;

  /**
   * Handler for post-processing once a suggestion has been applied: calls
   * into the active keyboard's `begin postKeystroke` entry point.
   *
   * Called after the suggestion is applied but _before_ new predictions are
   * requested based on the resulting context.
   */
  private readonly postApplicationHandler: () => void;

  public constructor(langProcessor: LanguageProcessor, kbdProcessor: KeyboardProcessor) {
    super();

    this.langProcessor = langProcessor;
    this.kbdProcessor = kbdProcessor;

    const validSuggestionState: () => boolean = () =>
      this.currentTarget && langProcessor.state == 'configured';

    this.suggestionApplier = (suggestion) => {
      if(validSuggestionState()) {
        return langProcessor.applySuggestion(suggestion, this.currentTarget, () => kbdProcessor.layerId);
      } else {
        return null;
      }
    }

    this.suggestionReverter = async (reversion) => {
      if(validSuggestionState()) {
        let suggestions = await langProcessor.applyReversion(reversion, this.currentTarget);

        // We bypass the 'standard' "new suggestions" pipeline, as these aren't NEW suggestions.
        // We also want to avoid altering flags that would indicate our post-reversion state.
        this._currentSuggestions = suggestions;
        this.sendUpdateEvent();
      }
    }

    // As it's called synchronously via event-callback during `this.suggestionApplier`,
    // `this.currentTarget` is guaranteed to remain unchanged.
    this.postApplicationHandler = () => {
      // Tell the keyboard that the current layer has not changed
      kbdProcessor.newLayerStore.set('');
      kbdProcessor.oldLayerStore.set('');
      // Call the keyboard's entry point.
      kbdProcessor.processPostKeystroke(kbdProcessor.contextDevice, this.currentTarget)
        // If we have a RuleBehavior as a result, run it on the target. This should
        // only change system store and variable store values.
        ?.finalize(kbdProcessor, this.currentTarget, true);
    };

    this.connect();
  }

  public get modelState() {
    return this.langProcessor.state;
  }

  private connect() {
    this.langProcessor.addListener('invalidatesuggestions', this.invalidateSuggestions);
    this.langProcessor.addListener('suggestionsready', this.updateSuggestions);
    this.langProcessor.addListener('tryaccept', this.doTryAccept);
    this.langProcessor.addListener('tryrevert', this.doTryRevert);
    this.langProcessor.addListener('statechange', this.onModelStateChange);

    this.langProcessor.addListener('suggestionapplied', this.postApplicationHandler);
  }

  public disconnect() {
    this.langProcessor.removeListener('invalidatesuggestions', this.invalidateSuggestions);
    this.langProcessor.removeListener('suggestionsready', this.updateSuggestions);
    this.langProcessor.removeListener('tryaccept', this.doTryAccept);
    this.langProcessor.removeListener('tryrevert', this.doTryRevert);
    this.langProcessor.removeListener('statechange', this.onModelStateChange);

    this.langProcessor.removeListener('suggestionapplied', this.postApplicationHandler);
    this.clearSuggestions();
  }

  public get currentSuggestions(): Suggestion[] {
    let suggestions: Suggestion[] = [];
    // Insert 'current text' if/when valid as the leading option.
    // Since we don't yet do auto-corrections, we only show 'keep' whenever it's
    // a valid word (according to the model).
    const mayShowKeep = this.activateKeep() && this.keepSuggestion;

    // If there is an auto-select option that doesn't match the current context,
    // we need to present the user a way to preserve the current context instead.
    const keepNeeded = this.selected && (this.keepSuggestion != this.selected);

    if(mayShowKeep && (keepNeeded || this.keepSuggestion.matchesModel)) {
      suggestions.push(this.keepSuggestion);
    } else if(this.doRevert) {
      suggestions.push(this.revertSuggestion);
    }

    return suggestions.concat(this._currentSuggestions);
  }

  /**
   * Function apply
   * Description  Applies the predictive `Suggestion` represented by this `BannerSuggestion`.
   */
  private acceptInternal(suggestion: Suggestion): Promise<Reversion> {
    if(!suggestion) {
      return null;
    }

    // Should be safe to convert into an event handled externally.
    // layerID can be obtained by whoever/whatever holds the InputProcessor instance.
    if(suggestion.tag == 'revert') {
      this.suggestionReverter(suggestion as Reversion);
      return null;
    } else {
      return this.suggestionApplier(suggestion);
    }
  }

  /**
   * Applies predictive-text suggestions and post-acceptance reversions to the current
   * prediction context.
   *
   * Note that both cases will additionally trigger a new asynchronous `predict` operation,
   * though no corresponding Promise is returned by this function.  As such, the current
   * suggestions should be considered outdated after calling this method, pending replacement
   * upon the completed async `predict`.
   *
   * @param suggestion Either a `Suggestion` or `Reversion`.
   * @returns if `suggestion` is a `Suggestion`, will return a `Promise<Reversion>`; else, `null`.
   */
  public accept(suggestion: Suggestion): Promise<Reversion> | Promise<null> {
    let _this = this;

    // Selecting a suggestion or a reversion should both clear selection
    // and clear the reversion-displaying state of the banner.
    this.selected = null;
    this.doRevert = false;

    this.revertAcceptancePromise = this.acceptInternal(suggestion);
    if(!this.revertAcceptancePromise) {
      // We get here either if suggestion acceptance fails or if it was a reversion.
      if(suggestion && suggestion.tag == 'revert') {
        // Reversion state management
        this.recentAcceptCause = null;
        this.recentRevert = true;
      }

      return Promise.resolve(null);
    }

    this.revertAcceptancePromise.then(function(suggestion) {
      // Always null-check!
      if(suggestion) {
        _this.revertSuggestion = suggestion;
      }
    });

    // By default, we assume we were triggered by the banner.
    // Acceptance by keystroke will overwrite this later (in `tryAccept`)
    this.recentAcceptCause = 'banner';
    this.recentRevert = false;

    this.swallowPrediction = true;

    return this.revertAcceptancePromise;
  }

  private showRevert() {
    // Construct a 'revert suggestion' to facilitate a reversion UI component.
    this.doRevert = true;
    this.sendUpdateEvent();
  }

  /**
   * Receives messages from the keyboard that the 'accept' keystroke has been entered.
   * Should return 'false' if the current state allows accepting a suggestion and act accordingly.
   * Otherwise, return true.
   */
  private doTryAccept = (source: string, returnObj: {shouldSwallow: boolean}): void => {
    const recentAcceptCause = this.recentAcceptCause;

    if(!recentAcceptCause && this.selected) {
      this.accept(this.selected);
      returnObj.shouldSwallow = true;

      // doTryAccept is the path for keystroke-based auto-acceptance.
      // Overwrite the cause to reflect this.
      this.recentAcceptCause = 'key';
    } else if(recentAcceptCause && source == 'space') {
      this.recentAcceptCause = null;
      if(recentAcceptCause == 'key') {
        // No need to swallow the keystroke's whitespace; we triggered the prior acceptance
        // FROM a space, so we've already aliased the suggestion's built-in space.
        returnObj.shouldSwallow = false;
        return;
      }

      // Standard whitespace applications from the banner, those we DO want to
      // swallow the first time.
      //
      // If the model doesn't insert wordbreaks, there's no space to alias, so
      // don't swallow the space.  If it does, we consider that insertion to be
      // the results of the first post-accept space.
      returnObj.shouldSwallow = !!this.langProcessor.wordbreaksAfterSuggestions; // can be handed outside
    } else {
      returnObj.shouldSwallow = false;
    }
  }

  /**
   * Receives messages from the keyboard that the 'revert' keystroke has been entered.
   * Should return 'false' if the current state allows reverting a recently-applied suggestion and act accordingly.
   * Otherwise, return true.
   */
  private doTryRevert = (/*returnObj: {shouldSwallow: boolean}*/): void => {
    // Has the revert keystroke (BKSP) already been sent once since the last accept?
    if(this.doRevert) {
      // If so, clear the 'revert' option and start doing normal predictions again.
      this.doRevert = false;
      this.recentAcceptCause = null;
      // Otherwise, did we just accept something before the revert signal was received?
    } else if(this.recentAcceptCause) {
      this.showRevert();
      this.swallowPrediction = true;
    }

    // // We don't yet actually do key-based reversions.
    // returnObj.shouldSwallow = false;
    return;
  }

  /**
   * Function invalidateSuggestions
   * Scope        Public
   * Description  Clears the suggestions in the suggestion banner
   */
  private invalidateSuggestions = (source: InvalidateSourceEnum): void => {
    // By default, we assume that the context is the same until we notice otherwise.
    this.initNewContext = false;
    this.selected = null;

    if(!this.swallowPrediction || source == 'context') {
      this.recentAcceptCause = null;
      this.doRevert = false;
      this.recentRevert = false;

      if(source == 'context') {
        this.swallowPrediction = false;
        this.initNewContext = true;
      }
    }

    // Not checking this can result in a perceptible 'flash' of sorts due to the suggestion-update delay.
    if(source != 'new') {
      this.clearSuggestions();
      // this.options.forEach((option: BannerSuggestion) => {
      //   option.update(null);
      // });
    }
  }

  private clearSuggestions() {
    this.updateSuggestions({
      suggestions: [],
      transcriptionID: 0
    });
  }

  private activateKeep(): boolean {
    return !this.recentAcceptCause && !this.recentRevert && !this.initNewContext;
  }

  /**
   * Function updateSuggestions
   * Scope       Public
   * @param {Suggestion[]}  suggestions   Array of suggestions from the lexical model.
   * Description    Update the displayed suggestions in the SuggestionBanner
   */
  private updateSuggestions = (prediction: ReadySuggestions): void => {
    let suggestions = prediction.suggestions;

    this._currentSuggestions = suggestions;
    this.selected = null;

    // Do we have a keep suggestion?  If so, remove it from the list so that we can control its display position
    // and prevent it from being hidden after reversion operations.
    this.keepSuggestion = null;
    for(let s of suggestions) {
      if(s.tag == 'keep') {
        this.keepSuggestion = s as Keep;
      }

      if(s.autoAccept && !this.selected) {
        this.selected = s;
      }
    }

    if(this.keepSuggestion) {
      this._currentSuggestions.splice(this._currentSuggestions.indexOf(this.keepSuggestion), 1);
    }

    // If we've gotten an update request like this, it's almost always user-triggered and means the context has shifted.
    if(!this.swallowPrediction) {
      this.recentAcceptCause = null;
      this.doRevert = false;
      this.recentRevert = false;
    } else { // This prediction was triggered by a recent 'accept.'  Now that it's fulfilled, we clear the flag.
      this.swallowPrediction = false;
    }

    // The rest is the same, whether from input or from "self-updating" after a reversion to provide new suggestions.
    this.sendUpdateEvent();
  }

  public sendUpdateEvent() {
    this.emit('update', this.currentSuggestions);
  }

  public resetContext(): Promise<Suggestion[]> {
    const target = this.currentTarget;

    if(target) {
      // Note:  should be triggered after the corresponding new-context event rule has been processed,
      // as that may affect the value of layerId here.
      return this.langProcessor.invalidateContext(target, this.kbdProcessor.layerId);
    } else {
      return Promise.resolve([]);
    }
  }

  private onModelStateChange: StateChangeHandler = (state) => {
    // Either way, the model has changed; either state marks the completion of such a transition.
    // The 'active' state displays the banner while a model loads... but its predictions are
    // only possible once fully 'configured'.  They may appear to 'blink on' after a small delay
    // as a result.
    if(state == 'configured' || state == 'inactive') {
      this.resetContext();
    }
  }
}