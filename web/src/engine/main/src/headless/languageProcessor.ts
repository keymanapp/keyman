import { EventEmitter } from "eventemitter3";
import { LMLayer, WorkerFactory } from "@keymanapp/lexical-model-layer/web";
import { OutputTarget, Transcription, Mock, RuleBehavior } from "keyman/engine/js-processor";
import { LanguageProcessorEventMap, ModelSpec, StateChangeEnum, ReadySuggestions } from 'keyman/engine/interfaces';
import ContextWindow from "./contextWindow.js";
import { TranscriptionCache } from "./transcriptionCache.js";
import { LexicalModelTypes } from '@keymanapp/common-types';
import Capabilities = LexicalModelTypes.Capabilities;
import Configuration = LexicalModelTypes.Configuration;
import Reversion = LexicalModelTypes.Reversion;
import Suggestion = LexicalModelTypes.Suggestion;

/* Is more like the model configuration engine */
export class LanguageProcessor extends EventEmitter<LanguageProcessorEventMap> {
  private lmEngine: LMLayer;
  private currentModel?: ModelSpec;
  private configuration?: Configuration;
  private currentPromise?: Promise<Suggestion[]>;

  private readonly recentTranscriptions: TranscriptionCache;

  private _mayPredict: boolean = true;
  private _mayCorrect: boolean = true;
  private _mayAutoCorrect: boolean = true;

  private _state: StateChangeEnum = 'inactive';

  public constructor(predictiveWorkerFactory: WorkerFactory, transcriptionCache: TranscriptionCache, supportsRightDeletions: boolean = false) {
    super();

    this.recentTranscriptions = transcriptionCache;

    // Establishes KMW's platform 'capabilities', which limit the range of context a LMLayer
    // model may expect.
    const capabilities: Capabilities = {
      maxLeftContextCodePoints: 64,
      // Since the apps don't yet support right-deletions.
      maxRightContextCodePoints: supportsRightDeletions ? 0 : 64
    }

    if(!predictiveWorkerFactory) {
      return;
    }

    let workerInstance: Worker;
    try {
      workerInstance = predictiveWorkerFactory?.constructInstance();
    } catch(e) {
      // We can condition on `lmEngine` being null/undefined.
      console.warn('Web workers are not available: ' + (e ?? '').toString());
      workerInstance = null;
    }
    if(workerInstance) {
      this.lmEngine = new LMLayer(capabilities, workerInstance);
    }
  }

  public get activeModel(): ModelSpec {
    return this.currentModel;
  }

  public get isConfigured(): boolean {
    return !!this.configuration;
  }

  public get state(): StateChangeEnum {
    return this._state;
  }

  public unloadModel() {
    if(!this.canEnable) {
      return;
    }

    this.lmEngine.unloadModel();
    delete this.currentModel;
    delete this.configuration;

    this._state = 'inactive';
    this.emit('statechange', 'inactive');
  }

  loadModel(model: ModelSpec): Promise<void> {
    if(!model) {
      throw new Error("Null reference not allowed.");
    }

    if(!this.canEnable) {
      return Promise.resolve();
    }

    const specType: 'file'|'raw' = model.path ? 'file' : 'raw';
    const source = specType == 'file' ? model.path : model.code;

    // We pre-emptively emit so that the banner's DOM elements may update synchronously.
    // Prevents an ugly "flash of unstyled content" layout issue during keyboard load
    // on our mobile platforms when embedded.
    this.currentModel = model;
    if(this.mayPredict) {
      this._state = 'active';
      this.emit('statechange', 'active');
    }

    return this.lmEngine.loadModel(source, specType).then((config: Configuration) => {
      this.configuration = config;
      if(this.mayPredict) {
        this._state = 'configured';
        this.emit('statechange', 'configured');
      }
    }).catch((error) => {
      // Does this provide enough logging information?
      let message: string;
      if(error instanceof Error) {
        message = error.message;
      } else {
        message = String(error);
      }
      console.error("Could not load model '" + model.id + "': " + message);

      // Since the model couldn't load, immediately deactivate.  Visually, it'll look
      // like the banner crashed shortly after load.
      this.currentModel = null;
      this._state = 'inactive';
      this.emit('statechange', 'inactive');
    });
  }

  public invalidateContext(outputTarget: OutputTarget, layerId: string): Promise<Suggestion[]> {
    // If there's no active model, there can be no predictions.
    // We'll also be missing important data needed to even properly REQUEST the predictions.
    if(!this.currentModel || !this.configuration) {
      return Promise.resolve([]);
    }

    // Don't attempt predictions when disabled!
    // invalidateContext otherwise bypasses .predict()'s check against this.
    if(!this.isActive) {
      return Promise.resolve([]);
    }

    // Signal to any predictive text UI that the context has changed, invalidating recent predictions.
    this.emit('invalidatesuggestions', 'context');

    if(outputTarget) {
      const transcription = outputTarget.buildTranscriptionFrom(outputTarget, null, false);
      return this.predict_internal(transcription, true, layerId);
    } else {
      // if there's no active context source, there's nothing to
      // provide suggestions for. In that case, there's no reason
      // to even request suggestions, so bypass the prediction
      // engine and say that there aren't any.
      return Promise.resolve([]);
    }
  }

  public wordbreak(target: OutputTarget, layerId: string): Promise<string> {
    if(!this.isActive) {
      return null;
    }

    const context = new ContextWindow(Mock.from(target, false), this.configuration, layerId);
    return this.lmEngine.wordbreak(context);
  }

  public hasState(transitionId: number) {
    return !!this.recentTranscriptions.get(transitionId);
  }

  public predict(transcription: Transcription, layerId: string): Promise<Suggestion[]> {
    if(!this.isActive) {
      return null;
    }

    // If there's no active model, there can be no predictions.
    // We'll also be missing important data needed to even properly REQUEST the predictions.
    if(!this.currentModel || !this.configuration) {
      return null;
    }

    // We've already invalidated any suggestions resulting from any previously-existing Promise -
    // may as well officially invalidate them via event.
    this.emit("invalidatesuggestions", 'new');

    return this.predict_internal(transcription, false, layerId);
  }

  /**
   *
   * @param suggestion
   * @param outputTarget
   * @param getLayerId      a function that returns the current layerId,
   *                        required because layerid can be changed by PostKeystroke
   * @returns
   */
  public applySuggestion(
    suggestion: Suggestion,
    outputTarget: OutputTarget,
    getLayerId: () => string,
    ruleBehavior: RuleBehavior
  ): {
    reversion: Promise<Reversion>,
    appendedRuleBehavior?: RuleBehavior
  } {
    if(!outputTarget) {
      throw "Accepting suggestions requires a destination OutputTarget instance."
    }

    if(!this.isActive) {
      return null;
    }

    if(!this.isConfigured) {
      // If we're in this state, the suggestion is now outdated; the user must have swapped keyboard and model.
      console.warn("Could not apply suggestion; the corresponding model has been unloaded");
      return null;
    }

    // Find the state of the context at the time the suggestion was generated.
    // This may refer to the context before an input keystroke or before application
    // of a predictive suggestion.
    const original = this.getPredictionState(suggestion.transformId);
    if(!original) {
      console.warn("Could not apply the Suggestion!");
      return null;
    }

    this.recentTranscriptions.rewindTo(suggestion.transformId);

    // Apply the Suggestion!

    // Step 1:  determine the intermediate and final output texts
    const intermediate = Mock.from(original.preInput, false);
    let finalTranscription: Transcription;
    let resultBehavior: RuleBehavior;
    intermediate.apply(suggestion.transform);
    let final = intermediate;
    if(suggestion.appendedTransform) {
      final = Mock.from(intermediate);
      final.apply(suggestion.appendedTransform);

      // Build the intermediate state - we may return here via reversion.

      // This condition is met when auto-correct is triggered by a (non-spacebar)
      // word-breaking mark.  In this case, this application also matches the
      // correct Transcription for the keystroke, so we need to build it here.
      if(ruleBehavior?.transcription?.transform == suggestion.appendedTransform) {
        finalTranscription = final.buildTranscriptionFrom(
          intermediate,
          ruleBehavior.transcription.keystroke,
          false,
          ruleBehavior.transcription.alternates
        );

        // Preserve all prior RuleBehavior properties; only replace the Transcription.
        resultBehavior = new RuleBehavior();
        Object.assign(resultBehavior, ruleBehavior);
        resultBehavior.transcription = finalTranscription;
      } else {
        // Simple case - a direct application of the suggestion (via banner or space key)
        finalTranscription = final.buildTranscriptionFrom(intermediate, null, false);
      }
      // Somewhere here, save-state the intermediate state!
      // It's fine to do even if it's for a keystroke-based version; it'll just
      // override itself in the cache if recorded again.
      this.recordTranscription(finalTranscription);
      // We set the appended transform with its own ID before passing it off to the predictive-text worker.
      suggestion.appendedTransform.id = finalTranscription.token;
    }

    // Step 2:  build a final, master Transform that will produce the desired results from the CURRENT state.
    // In embedded mode, both Android and iOS are best served by calculating this transform and applying its
    // values as needed for use with their IME interfaces.
    const transform = final.buildTransformFrom(outputTarget);
    outputTarget.apply(transform);

    // Tell the banner that a suggestion was applied, so it can call the
    // keyboard's PostKeystroke entry point as needed
    this.emit('suggestionapplied', outputTarget);

    // Build a 'reversion' Transcription that can be used to undo this apply() if needed,
    // replacing the suggestion transform with the original input text.
    const preApply = Mock.from(original.preInput, false);
    preApply.apply(original.transform);

    // Builds the reversion option according to the loaded lexical model's known
    // syntactic properties.
    const suggestionContext = new ContextWindow(original.preInput, this.configuration, getLayerId());

    // We must accept the Suggestion from its original context, which was before
    // `original.transform` was applied.
    return {
      reversion: this.lmEngine.acceptSuggestion(suggestion, suggestionContext, original.transform),
      appendedRuleBehavior: resultBehavior
    };
  }

  public applyReversion(reversion: Reversion, outputTarget: OutputTarget, appendedOnly?: boolean) {
    if(!outputTarget) {
      throw new Error("Accepting suggestions requires a destination OutputTarget instance.");
    }

    if(!this.isActive) {
      return null;
    }

    // Find the state of the context at the time the suggestion was generated.
    // This may refer to the context before an input keystroke or before application
    // of a predictive suggestion.
    //
    // Reversions use the additive inverse of the id token of the Transcription being
    // reverted to.
    const reversionId = appendedOnly ? reversion.appendedTransform.id : -reversion.transformId;
    const original = this.getPredictionState(reversionId);
    if(!original) {
      console.warn("Could not apply the Suggestion!");
      return Promise.resolve([] as Suggestion[]);
    }

    this.recentTranscriptions.rewindTo(reversionId);

    // Apply the Reversion!

    // Step 1:  determine the final output text
    const final = Mock.from(original.preInput, false);
    if(!appendedOnly) {
      final.apply(reversion.transform); // Should match original.transform, actually. (See applySuggestion)
    } // else: the retrieved transcription matches the applied Suggestion's root, without the appended part.

    // Step 2:  build a final, master Transform that will produce the desired results from the CURRENT state.
    // In embedded mode, both Android and iOS are best served by calculating this transform and applying its
    // values as needed for use with their IME interfaces.
    const transform = final.buildTransformFrom(outputTarget);
    outputTarget.apply(transform);

    // The reason we need to preserve the additive-inverse 'transformId' property on Reversions.
    const promise = this.currentPromise = this.lmEngine.revertSuggestion(
      reversion,
      new ContextWindow(final, this.configuration, null),
      appendedOnly
    );
    // If the "current Promise" is as set above, clear it.
    // If another one has been triggered since... don't.
    promise.then(() => this.currentPromise = (this.currentPromise == promise) ? null : this.currentPromise);

    return promise;
  }

  public predictFromTarget(outputTarget: OutputTarget, layerId: string): Promise<Suggestion[]> {
    if(!this.isActive || !outputTarget) {
      return null;
    }

    const transcription = outputTarget.buildTranscriptionFrom(outputTarget, null, false);
    return this.predict(transcription, layerId);
  }

  /**
   * Called internally to do actual predictions after any relevant "invalidatesuggestions" events
   * have been raised.
   * @param transcription The triggering transcription (if it exists)
   */
  private predict_internal(transcription: Transcription, resetContext: boolean, layerId: string): Promise<Suggestion[]> {
    if(!this.isActive || !transcription) {
      return null;
    }

    // We record the current context state before any prediction requests...
    const context = new ContextWindow(transcription.preInput, this.configuration, layerId);
    this.recordTranscription(transcription);

    // ... even those triggered by context-resets.
    if(resetContext) {
      this.lmEngine.resetContext(context, transcription.token);
    }

    let alternates = transcription.alternates;
    if(!this.mayCorrect || !alternates || alternates.length == 0) {
      alternates = [{
        sample: transcription.transform,
        p: 1.0
      }];
    }

    const transform = transcription.transform;
    const promise = this.currentPromise = this.lmEngine.predict(alternates, context);

    return promise.then((suggestions: Suggestion[]) => {
      if(promise == this.currentPromise) {
        const result = new ReadySuggestions(suggestions, transform.id);
        this.emit("suggestionsready", result);
        this.currentPromise = null;
      }

      return suggestions;
    });
  }

  private recordTranscription(transcription: Transcription) {
    this.recentTranscriptions.save(transcription);
  }

  /**
   * Retrieves the context and output state of KMW immediately before the prediction with
   * token `id` was generated.  Must correspond to a 'recent' one, as only so many are stored
   * in `ModelManager`'s history buffer.
   *
   * @param id A unique identifier corresponding to a recent `Transcription`.
   * @returns The matching `Transcription`, or `null` none is found.
   */
  public getPredictionState(id: number): Transcription {
    return this.recentTranscriptions.get(id);
  }

  public shutdown() {
    this.lmEngine?.shutdown();
    this.removeAllListeners();
  }

  public get isActive(): boolean {
    if(!this.canEnable) {
      this._mayPredict = false;
      return false;
    }
    return (this.activeModel || false) && this._mayPredict;
  }

  public get canEnable(): boolean {
    // Is not initialized if there is no worker.
    return !!this.lmEngine;
  }

  public get mayPredict() {
    return this._mayPredict;
  }

  public set mayPredict(flag: boolean) {
    if(!this.canEnable) {
      return;
    }

    const oldVal = this._mayPredict;
    this._mayPredict = flag;

    if(oldVal != flag) {
      // If there's no model to be activated and we've reached this point,
      // the banner should remain inactive, as it already was.
      // If it there was one and we've reached this point, we're globally
      // deactivating, so we're fine.
      if(this.activeModel) {
        // If someone toggles predictions on and off without changing the model, it is possible
        // that the model is already configured!
        const state: StateChangeEnum = flag ? 'active' : 'inactive';

        // We always signal the 'active' state here, even if 'configured', b/c of an
        // anti-banner-flicker optimization in the Android app.
        this._state = state;
        this.emit('statechange', state);

        // Only signal `'configured'` for a previously-loaded model if we're turning
        // things back on; don't send it if deactivated!
        if(flag && this.isConfigured) {
          this._state = 'configured';
          this.emit('statechange', 'configured');
        }
      }
    }
  }

  public get mayCorrect() {
    return this._mayCorrect;
  }

  public set mayCorrect(flag: boolean) {
    this._mayCorrect = flag;
  }

  public get mayAutoCorrect() {
    return this._mayAutoCorrect;
  }

  public set mayAutoCorrect(flag: boolean) {
    this._mayAutoCorrect = flag;
  }

  public get wordbreaksAfterSuggestions() {
    return this.configuration?.appendsWordbreaks;
  }
}
