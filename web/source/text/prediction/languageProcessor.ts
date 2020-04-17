namespace com.keyman.text.prediction {
  /**
   * Corresponds to the 'suggestionsready' LanguageProcessor event.
   */
  export type ReadySuggestionsHandler = (prediction: ReadySuggestions) => boolean;

  export type StateChangeEnum = 'active'|'inactive';
  /**
   * Corresponds to the 'statechange' LanguageProcessor event.
   */
  export type StateChangeHandler = (state: StateChangeEnum) => boolean;

  /**
   * Covers both 'tryaccept' and 'tryrevert' events.
   */
  export type TryUIHandler = (source: string) => boolean;

  export type InvalidateSourceEnum = 'new'|'context';

  /**
   * Corresponds to the 'invalidatesuggestions' LanguageProcessor event.
   */
  export type InvalidateSuggestionsHandler = (source: InvalidateSourceEnum) => boolean;

  export class LanguageProcessor {
    private lmEngine: LMLayer;
    private currentModel?: ModelSpec;
    private configuration?: Configuration;
    private currentPromise?: Promise<Suggestion[]>;

    private recentTranscriptions: Transcription[] = [];

    private _mayPredict: boolean = true;
    private _mayCorrect: boolean = true;

    private static readonly TRANSCRIPTION_BUFFER: 10;

    public init(supportsRightDeletions: boolean = false) {
      // Establishes KMW's platform 'capabilities', which limit the range of context a LMLayer
      // model may expect.
      let capabilities: Capabilities = {
        maxLeftContextCodePoints: 64,
        // Since the apps don't yet support right-deletions.
        maxRightContextCodePoints: supportsRightDeletions ? 0 : 64
      }

      if(!this.canEnable()) {
        return;
      }

      this.lmEngine = new LMLayer(capabilities);
    }

    public get activeModel(): ModelSpec {
      return this.currentModel;
    }

    public unloadModel() {
      this.lmEngine.unloadModel();
      delete this.currentModel;
      delete this.configuration;

      let keyman = com.keyman.singleton;
      keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'statechange', 'inactive');
    }

    loadModel(model: ModelSpec): Promise<void> {
      if(!model) {
        throw new Error("Null reference not allowed.");
      }

      let file = model.path;
      let mm = this;

      // We should wait until the model is successfully loaded before setting our state values.
      return this.lmEngine.loadModel(file).then(function(config: Configuration) { 
        mm.currentModel = model;
        mm.configuration = config;

        try {
          let keyman = com.keyman.singleton;
          keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'statechange', 'active');
        } catch (err) {
          // Does this provide enough logging information?
          console.error("Could not load model '" + model.id + "': " + (err as Error).message);
        }
      });
    }

    public invalidateContext() {
      let keyman = com.keyman.singleton;

      // Signal to any predictive text UI that the context has changed, invalidating recent predictions.
      keyman.util.callEvent(ModelManager.EVENT_PREFIX + "invalidatesuggestions", 'context');

      // If there's no active model, there can be no predictions.
      // We'll also be missing important data needed to even properly REQUEST the predictions.
      if(!this.currentModel || !this.configuration) {
        return;
      }
      
      this.predict_internal();
    }

    public wordbreak(target: OutputTarget): Promise<string> {
      if(!this.isActive) {
        return null;
      }

      let context = new TranscriptionContext(Mock.from(target), this.configuration);
      return this.lmEngine.wordbreak(context);
    }

    public predict(transcription?: Transcription) {
      if(!this.isActive) {
        return;
      }

      let keyman = com.keyman.singleton;

      // If there's no active model, there can be no predictions.
      // We'll also be missing important data needed to even properly REQUEST the predictions.
      if(!this.currentModel || !this.configuration) {
        return;
      }
            
      // We've already invalidated any suggestions resulting from any previously-existing Promise -
      // may as well officially invalidate them via event.
      keyman.util.callEvent(ModelManager.EVENT_PREFIX + "invalidatesuggestions", 'new');

      this.predict_internal(transcription);
    }

    /**
     * Called internally to do actual predictions after any relevant "invalidatesuggestions" events
     * have been raised.
     * @param transcription The triggering transcription (if it exists)
     */
    private predict_internal(transcription?: Transcription) {
      let keyman = com.keyman.singleton;

      if(!transcription) {
        let t = dom.Utils.getOutputTarget();
        if(t) {
          transcription = t.buildTranscriptionFrom(t, null);
        } else {
          return;
        }
      }

      let context = new TranscriptionContext(transcription.preInput, this.configuration);
      this.recordTranscription(transcription);

      let transform = transcription.transform;
      var promise = this.currentPromise = this.lmEngine.predict(transcription.alternates || transcription.transform, context);

      let mm = this;
      promise.then(function(suggestions: Suggestion[]) {
        if(promise == mm.currentPromise) {
          let result = new ReadySuggestions(suggestions, transform.id);
          keyman.util.callEvent(ModelManager.EVENT_PREFIX + "suggestionsready", result);
          mm.currentPromise = null;
        }
      })
    }

    private recordTranscription(transcription: Transcription) {
      this.recentTranscriptions.push(transcription);

      if(this.recentTranscriptions.length > LanguageProcessor.TRANSCRIPTION_BUFFER) {
        this.recentTranscriptions.splice(0, 1);
      }
    }

    /**
     * Retrieves the context and output state of KMW immediately before the prediction with 
     * token `id` was generated.  Must correspond to a 'recent' one, as only so many are stored
     * in `ModelManager`'s history buffer.
     * @param id A unique identifier corresponding to a recent `Transcription`.
     * @returns The matching `Transcription`, or `null` none is found.
     */
    public getPredictionState(id: number): Transcription {
      let match = this.recentTranscriptions.filter(function(t: Transcription) {
        return t.token == id;
      })

      return match.length == 0 ? null : match[0];
    }

    public shutdown() {
      this.lmEngine.shutdown();
    }

    public get isActive(): boolean {
      if(!this.canEnable()) {
        this._mayPredict = false;
        return false;
      }
      return this.activeModel && this._mayPredict;
    }

    public canEnable(): boolean {
      // Is overridden for dom-aware KMW in case of old IE versions.
      return true;
    }

    public get mayPredict() {
      return this._mayPredict;
    }

    public set mayPredict(flag: boolean) {
      if(!this.canEnable()) {
        return;
      }

      let oldVal = this._mayPredict;
      this._mayPredict = flag;

      if(oldVal != flag) {
        let keyman = com.keyman.singleton;
        keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'statechange', flag ? 'active' : 'inactive');
      }
    }

    public get mayCorrect() {
      return this._mayCorrect;
    }

    public set mayCorrect(flag: boolean) {
      this._mayCorrect = flag;
    }

    public tryAcceptSuggestion(source: string): boolean {
      let keyman = com.keyman.singleton;
      
      // Handlers of this event should return 'false' when the 'try' is successful.
      return !keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'tryaccept', source);
    }

    public tryRevertSuggestion(): boolean {
      let keyman = com.keyman.singleton;
      
      // Handlers of this event should return 'false' when the 'try' is successful.
      return !keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'tryrevert', null);
    }
  }
}