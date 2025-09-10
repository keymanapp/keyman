import { EventEmitter } from "eventemitter3";
import { LexicalModelTypes } from '@keymanapp/common-types';
import Keep = LexicalModelTypes.Keep;
import Reversion = LexicalModelTypes.Reversion;
import Suggestion = LexicalModelTypes.Suggestion;
import { type LanguageProcessorSpec , ReadySuggestions, type InvalidateSourceEnum, StateChangeHandler } from './languageProcessor.interface.js';
import { type OutputTarget, type RuleBehavior } from 'keyman/engine/js-processor';

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
  private _keepSuggestion: Keep;
  private _revertSuggestion: Reversion;

  public get keepSuggestion(): Keep {
    return this._keepSuggestion;
  }

  private _immediateReversion: Reversion;

  public get immediateReversion(): Reversion {
    return this._immediateReversion;
  }

  // Set to null/undefined if there was no recent acceptance.
  private _recentAcceptCause: 'key' | 'banner';

  public get recentAcceptCause(): 'key' | 'banner' {
    return this._recentAcceptCause;
  }

  private swallowPrediction: boolean = false;

  private recentRevert: boolean = false;

  private langProcessor: LanguageProcessorSpec;
  private getLayerId: () => string;

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

  private readonly suggestionApplier: (suggestion: Suggestion, ruleBehavior?: RuleBehavior) => RuleBehavior;
  private readonly suggestionReverter: (reversion: Reversion) => void;

  public constructor(langProcessor: LanguageProcessorSpec, getLayerId: () => string) {
    super();

    this.langProcessor = langProcessor;
    this.getLayerId = getLayerId;

    const validSuggestionState: () => boolean = () =>
      this.currentTarget && langProcessor.state == 'configured';

    this.suggestionApplier = (suggestion, ruleBehavior) => {
      if(validSuggestionState()) {
        const results = langProcessor.applySuggestion(suggestion, this.currentTarget, getLayerId, ruleBehavior);
        results.reversion.then((reversion) => this._immediateReversion = reversion);
        return results.appendedRuleBehavior;
      } else {
        return null;
      }
    }

    this.suggestionReverter = async (reversion) => {
      if(validSuggestionState()) {
        const suggestions = await langProcessor.applyReversion(reversion, this.currentTarget);
        // We want to avoid altering flags that indicate our post-reversion state.
        this.swallowPrediction = true;
        this.updateSuggestions(new ReadySuggestions(suggestions, reversion.id ? -reversion.id : undefined));
      }
    }

    this.connect();
  }

  private connect() {
    this.langProcessor.addListener('invalidatesuggestions', this.invalidateSuggestions);
    this.langProcessor.addListener('suggestionsready', this.updateSuggestions);
    this.langProcessor.addListener('statechange', this.onModelStateChange);
  }

  public disconnect() {
    this.langProcessor.removeListener('invalidatesuggestions', this.invalidateSuggestions);
    this.langProcessor.removeListener('suggestionsready', this.updateSuggestions);
    this.langProcessor.removeListener('statechange', this.onModelStateChange);
    this.clearSuggestions();
  }

  public get currentSuggestions(): Suggestion[] {
    const suggestions: Suggestion[] = [];
    // Insert 'current text' if/when valid as the leading option.
    // Since we don't yet do auto-corrections, we only show 'keep' whenever it's
    // a valid word (according to the model).
    const mayShowKeep = this.activateKeep() && this.keepSuggestion;

    // If there is an auto-select option that doesn't match the current context,
    // we need to present the user a way to preserve the current context instead.
    const keepNeeded = this.selected && (this.keepSuggestion != this.selected);

    if(this._revertSuggestion) {
      suggestions.push(this._revertSuggestion);
    }

    if(mayShowKeep && (keepNeeded || this.keepSuggestion.matchesModel)) {
      suggestions.push(this.keepSuggestion);
    }

    return suggestions.concat(this._currentSuggestions);
  }

  public triggerPredictions() {
    const outputTarget = this.currentTarget;
    if(!outputTarget) {
      return Promise.resolve([]);
    }
    const transcription = outputTarget.buildTranscriptionFrom(outputTarget, null, false);
    return this.langProcessor.predict(transcription, this.getLayerId());
  }

  /**
   * Function apply
   * Description  Applies the predictive `Suggestion` represented by this `BannerSuggestion`.
   */
  private acceptInternal(suggestion: Suggestion, ruleBehavior?: RuleBehavior): RuleBehavior {
    if(!suggestion) {
      return null;
    }

    // Should be safe to convert into an event handled externally.
    // layerID can be obtained by whoever/whatever holds the InputProcessor instance.
    if(suggestion.tag == 'revert') {
      this.suggestionReverter(suggestion as Reversion);
      this.recentRevert = true;
      return null;
    } else {
      return this.suggestionApplier(suggestion, ruleBehavior);
    }
  }

  /**
   * Applies predictive-text suggestions and post-acceptance reversions to the
   * current prediction context.
   *
   * Note that both cases will additionally trigger a new asynchronous `predict`
   * operation, though no corresponding Promise is returned by this function.
   * As such, the current suggestions should be considered outdated after
   * calling this method, pending replacement upon the completed async
   * `predict`.
   *
   * @param suggestion Either a `Suggestion` or `Reversion`.
   * @param ruleBehavior When set with the results of applying an incoming
   * keystroke, the effects of that keystroke will be appended to the suggestion
   * being applied.
   * @returns if `suggestion` is a `Suggestion` triggered via keystroke, will
   * return a RuleBehavior.  Else returns null.ß
   */
  public accept(suggestion: Suggestion, ruleBehavior?: RuleBehavior): RuleBehavior {
    // Selecting a suggestion or a reversion should both clear selection
    // and clear the reversion-displaying state of the banner.
    this.selected = null;

    const results = this.acceptInternal(suggestion, ruleBehavior);

    // By default, we assume we were triggered by the banner.
    this._recentAcceptCause = ruleBehavior ? 'key' : 'banner';
    this.recentRevert = false;

    this.swallowPrediction = true;

    return results;
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
      this._recentAcceptCause = null;
      this.recentRevert = false;
      this._immediateReversion = null;

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
    const suggestions = prediction.suggestions;

    this.selected = null;

    // Do we have a keep suggestion?  If so, remove it from the list so that we can control its display position
    // and prevent it from being hidden after reversion operations.
    this._keepSuggestion = null;
    this._revertSuggestion = null;
    for (const s of suggestions) {
      if(s.tag == 'keep') {
        this._keepSuggestion = s as Keep;
      } else if(s.tag == 'revert') {
        this._revertSuggestion = s as Reversion;
      }

      if (this.langProcessor.mayAutoCorrect && s.autoAccept && !this.selected) {
        this.selected = s;
      }
    }

    // Verify that the transition IDs are still valid and remove special entries.
    this._currentSuggestions = suggestions.filter(s => {
      return this.langProcessor.hasState(Math.abs(s.transformId)) &&
      s != this._keepSuggestion &&
      s != this._revertSuggestion
    });

    // If we've gotten an update request like this, it's almost always user-triggered and means the context has shifted.
    if(!this.swallowPrediction) {
      this._recentAcceptCause = null;
      this.recentRevert = false;
      this._immediateReversion = null;
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
      return this.langProcessor.invalidateContext(target, this.getLayerId());
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