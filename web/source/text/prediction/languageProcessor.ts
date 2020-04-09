namespace com.keyman.text.prediction {
  export interface ModelSpec {
    /**
     * The model's unique identifier.
     */
    id: string;

    /**
     * The list of supported BCP-47 language codes.  Only one language should be supported,
     * although multiple variants based on region code (at min) may be specified.
     */
    languages: string[];

    /**
     * The path/URL to the file that defines the model.
     */
    path: string;
  }

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

  export class TranscriptionContext implements Context {
    left: string;
    right?: string;

    startOfBuffer: boolean;
    endOfBuffer: boolean;

    constructor(mock: Mock, config: Configuration) {
      this.left = mock.getTextBeforeCaret();
      this.startOfBuffer = this.left._kmwLength() > config.leftContextCodePoints;
      if(!this.startOfBuffer) {
        // Our custom substring version will return the last n characters if param #1 is given -n.
        this.left = this.left._kmwSubstr(-config.leftContextCodePoints);
      }

      this.right = mock.getTextAfterCaret();
      this.endOfBuffer = this.right._kmwLength() > config.leftContextCodePoints;
      if(!this.endOfBuffer) {
        this.right = this.right._kmwSubstr(0, config.leftContextCodePoints);
      }
    }
  }

  export class ReadySuggestions {
    suggestions: Suggestion[];
    transcriptionID: number;

    constructor(suggestions: Suggestion[], id: number) {
      this.suggestions = suggestions;
      this.transcriptionID = id;
    }
  }

  type SupportedEventNames = "suggestionsready" | "invalidatesuggestions" | "statechange" | "tryaccept" | "tryrevert";
  type SupportedEventHandler = InvalidateSuggestionsHandler | ReadySuggestionsHandler | StateChangeHandler | TryUIHandler;

  export class LanguageProcessor extends EventEmitter {
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

      this.emit('statechange', 'inactive');
    }

    loadModel(model: ModelSpec): Promise<void> {
      if(!model) {
        throw new Error("Null reference not allowed.");
      }

      let file = model.path;
      let lp = this;

      // We should wait until the model is successfully loaded before setting our state values.
      return this.lmEngine.loadModel(file).then(function(config: Configuration) { 
        lp.currentModel = model;
        lp.configuration = config;

        try {
          lp.emit('statechange', 'active');
        } catch (err) {
          // Does this provide enough logging information?
          console.error("Could not load model '" + model.id + "': " + (err as Error).message);
        }
      });
    }

    public invalidateContext(outputTarget?: OutputTarget) {
      // Signal to any predictive text UI that the context has changed, invalidating recent predictions.
      this.emit('invalidatesuggestions', 'context');

      // If there's no active model, there can be no predictions.
      // We'll also be missing important data needed to even properly REQUEST the predictions.
      if(!this.currentModel || !this.configuration) {
        return;
      }
      
      if(outputTarget) {
        this.predict_internal(outputTarget.buildTranscriptionFrom(outputTarget, null));
      }
    }

    public wordbreak(target: OutputTarget): Promise<string> {
      if(!this.isActive) {
        return null;
      }

      let context = new TranscriptionContext(Mock.from(target), this.configuration);
      return this.lmEngine.wordbreak(context);
    }

    public predict(transcription: Transcription): Promise<Suggestion[]> {
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

      return this.predict_internal(transcription);
    }

    public predictFromTarget(outputTarget: OutputTarget): Promise<Suggestion[]> {
      if(!outputTarget) {
        return null;
      }

      let transcription = outputTarget.buildTranscriptionFrom(outputTarget, null);
      return this.predict(transcription);
    }

    /**
     * Called internally to do actual predictions after any relevant "invalidatesuggestions" events
     * have been raised.
     * @param transcription The triggering transcription (if it exists)
     */
    private predict_internal(transcription: Transcription): Promise<Suggestion[]> {
      if(!transcription) {
        return null;
      }

      let context = new TranscriptionContext(transcription.preInput, this.configuration);
      this.recordTranscription(transcription);

      let transform = transcription.transform;
      var promise = this.currentPromise = this.lmEngine.predict(transcription.alternates || transcription.transform, context);

      let lp = this;
      return promise.then(function(suggestions: Suggestion[]) {
        if(promise == lp.currentPromise) {
          let result = new ReadySuggestions(suggestions, transform.id);
          lp.emit("suggestionsready", result);
          lp.currentPromise = null;
        }

        return suggestions;
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
        this.emit('statechange', flag ? 'active' : 'inactive');
      }
    }

    public get mayCorrect() {
      return this._mayCorrect;
    }

    public set mayCorrect(flag: boolean) {
      this._mayCorrect = flag;
    }

    public tryAcceptSuggestion(source: string): boolean {
      // Handlers of this event should return 'false' when the 'try' is successful.
      return !this.emit('tryaccept', source);
    }

    public tryRevertSuggestion(): boolean {
      // Handlers of this event should return 'false' when the 'try' is successful.
      return !this.emit('tryrevert', null);
    }
  }
}