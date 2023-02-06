import EventEmitter from "eventemitter3";
import type LanguageProcessor from "./languageProcessor.js";
import { type ReadySuggestions, type InvalidateSourceEnum } from './languageProcessor.js';
import type OutputTarget from "@keymanapp/keyboard-processor/build/obj/text/outputTarget.js";
import type KeyboardProcessor from "@keymanapp/keyboard-processor/build/obj/text/keyboardProcessor.js";

interface PredictionContextEventMap {
  update: (suggestions: Suggestion[]) => void;

}

/**
 * Maintains predictive-text state information corresponding to a specific context (and associated
 * OutputTarget).  Instances should be thrown out and replaced when the context is reset and/or the
 * active lexical model has changed.
 *
 * This should not be constructed if the `LanguageProcessor` is in an "inactive" state; if no model is
 * available, the 'lexical model' aspect of predictive context is effectively null.
 */
export default class PredictionContext extends EventEmitter<PredictionContextEventMap> {
  // Historical note:  before 17.0, this code was intertwined with /web/source/osk/banner.ts's
  // SuggestionBanner class.  This class serves as the main implementation of the banner's core logic.

  // Designed for use with auto-correct behavior
  private selected: Suggestion;

  private initNewContext: boolean = true;

  private _currentSuggestions: Suggestion[] = [];
  private keepSuggestion: Keep;
  private revertSuggestion: Reversion;

  private recentAccept: boolean = false;
  private revertAcceptancePromise: Promise<Reversion>;

  private swallowPrediction: boolean = false;

  private doRevert: boolean = false;
  private recentRevert: boolean = false;


  private langProcessor: LanguageProcessor;

  private readonly suggestionApplier: (suggestion: Suggestion) => Promise<Reversion>;
  private readonly suggestionReverter: (reversion: Reversion) => void;

  /**
   * Handler for post-processing once a suggestion has been applied: calls
   * into the active keyboard's `begin postKeystroke` entry point.
   */
  private readonly postApplicationHandler: (target: OutputTarget) => void;

  public constructor(langProcessor: LanguageProcessor, kbdProcessor: KeyboardProcessor, target: OutputTarget) {
    super();

    if(langProcessor.state == 'inactive') {
      throw new Error("Invalid state:  no predictive-text model is currently available.");
    }

    if(!target) {
      throw new Error("No text context is available to use for prediction!");
    }

    this.langProcessor = langProcessor;

    this.suggestionApplier = (suggestion) => {
      return langProcessor.applySuggestion(suggestion, target, () => kbdProcessor.layerId);
    }

    this.suggestionReverter = (reversion) => {
      langProcessor.applyReversion(reversion, target);
    }

    // This can probably be handled by... whatever holds the PredictionContext object
    // instead; possibly InputProcessor.  It was moved here since it's more closely-related
    // to the old banner code that landed here, hence its current location.
    //
    // The "whatever holds the PredictionContext object" bit is significant, as that hasn't
    // been fully worked out at this point of the ES module-conversion process.
    //
    // Note: moving this handler out of this specific class allows the constructor to
    // drop the `kbdProcessor` parameter in favor of a lambda/closure that's just
    // `() => kbdProcessor.layerId`.  Yay for narrower abstraction needs; that'd make
    // unit testing way easier to lightly 'mock' in a proper manner.
    //
    // Should not be static, as diff constructor calls may use diff kbdProcessor instances.
    this.postApplicationHandler = (outputTarget: OutputTarget) => {
      if(target != outputTarget) {
        return;
      }

      // Tell the keyboard that the current layer has not changed
      kbdProcessor.newLayerStore.set('');
      kbdProcessor.oldLayerStore.set('');
      // Call the keyboard's entry point.
      kbdProcessor.processPostKeystroke(kbdProcessor.contextDevice, outputTarget)
        // If we have a RuleBehavior as a result, run it on the target. This should
        // only change system store and variable store values.
        ?.finalize(kbdProcessor, outputTarget, true);
    };

    this.connect();

    this.initializeState = () => {
      if(langProcessor.isConfigured && target) {
        return langProcessor.predictFromTarget(target, kbdProcessor.layerId);
      } else {
        // if(langProcessor.state == 'active') // model is loadING, not loadED.
        return new Promise((resolve, reject) => {
          langProcessor.once('statechange', (state) => {
            if(state == 'configured') {
              resolve(langProcessor.predictFromTarget(target, kbdProcessor.layerId));
            } else { // == 'inactive' // an error occurred; the model load failed.
              reject(new Error("The set predictive-text model failed to load and/or configure properly."));
            }
            // 'active' does not occur; we got here because that was the previous value.
          });
        });
      }
    }
  }

  /**
   * Generates predictions based on the current state of the context, absent any incoming keystrokes.
   */
  public readonly initializeState: () => Promise<Suggestion[]>;

  private connect() {
    this.langProcessor.addListener('invalidatesuggestions', this.invalidateSuggestions);
    this.langProcessor.addListener('suggestionsready', this.updateSuggestions);
    this.langProcessor.addListener('tryaccept', this.doTryAccept);
    this.langProcessor.addListener('tryrevert', this.doTryRevert);

    this.langProcessor.addListener('suggestionapplied', this.postApplicationHandler);
  }

  public disconnect() {
    this.langProcessor.removeListener('invalidatesuggestions', this.invalidateSuggestions);
    this.langProcessor.removeListener('suggestionsready', this.updateSuggestions);
    this.langProcessor.removeListener('tryaccept', this.doTryAccept);
    this.langProcessor.removeListener('tryrevert', this.doTryRevert);

    this.langProcessor.removeListener('suggestionapplied', this.postApplicationHandler);
    this.clearSuggestions();
  }

  public get currentSuggestions(): Suggestion[] {
    let suggestions = [];
    // Insert 'current text' if/when valid as the leading option.
    // Since we don't yet do auto-corrections, we only show 'keep' whenever it's
    // a valid word (according to the model).

    if(this.activateKeep() && this.keepSuggestion && this.keepSuggestion.matchesModel) {
      suggestions.push(this.keepSuggestion);
    } else if(this.doRevert) {
      suggestions.push(this.revertSuggestion);
    }

    return suggestions.concat(this._currentSuggestions);
  }

  /**
   * Function apply
   * @param target (Optional) The OutputTarget to which the `Suggestion` ought be applied.
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
   * @returns if `suggestion` is a `Suggestion`, will return a Promise<Reversion>`; else, `null`.
   */
  public accept(suggestion: Suggestion): Promise<Reversion> | null {
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
        this.recentAccept = false;
        this.recentRevert = true;
      }
      return null;
    }

    this.revertAcceptancePromise.then(function(suggestion) {
      // Always null-check!
      if(suggestion) {
        _this.revertSuggestion = suggestion;
      }
    });

    this.recentAccept = true;
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
  private doTryAccept = (source: string /*, returnObj: {shouldSwallow: boolean}*/): void => {
    //let keyman = com.keyman.singleton;

    if(!this.recentAccept && this.selected) {
      this.accept(this.selected);
      // returnObj.shouldSwallow = true;
    } else if(this.recentAccept && source == 'space') {
      this.recentAccept = false;
      // // If the model doesn't insert wordbreaks, don't swallow the space.  If it does,
      // // we consider that insertion to be the results of the first post-accept space.
      // returnObj.shouldSwallow = !!keyman.core.languageProcessor.wordbreaksAfterSuggestions; // can be handed outside
    } else {
      // returnObj.shouldSwallow = false;
    }
  }

  /**
   * Receives messages from the keyboard that the 'revert' keystroke has been entered.
   * Should return 'false' if the current state allows reverting a recently-applied suggestion and act accordingly.
   * Otherwise, return true.
   */
  private doTryRevert = (/*returnObj: {shouldSwallow: boolean}*/): boolean => {
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

    if(!this.swallowPrediction || source == 'context') {
      this.recentAccept = false;
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
    return !this.recentAccept && !this.recentRevert && !this.initNewContext;
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

    // Do we have a keep suggestion?  If so, remove it from the list so that we can control its display position
    // and prevent it from being hidden after reversion operations.
    this.keepSuggestion = null;
    for(let s of suggestions) {
      if(s.tag == 'keep') {
        this.keepSuggestion = s as Keep;
      }
    }

    if(this.keepSuggestion) {
      this._currentSuggestions.splice(this._currentSuggestions.indexOf(this.keepSuggestion), 1);
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
    this.sendUpdateEvent();
  }

  public sendUpdateEvent() {
    this.emit('update', this.currentSuggestions);
  }
}