// Defines the text processing KMW core.
///<reference path="../processor.ts" />
// Defines the LMLayer's outer shell
///<reference path="../../includes/lmlayer.ts" />
// Defines the ModelManager and its related types.
///<reference path="../../kmwkeyboards.ts" />

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

  class TranscriptionContext implements Context {
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

  export type InvalidateSourceEnum = 'new'|'context';

  /**
   * Corresponds to the 'invalidatesuggestions' ModelManager event.
   */
  export type InvalidateSuggestionsHandler = (source: InvalidateSourceEnum) => boolean;

  export class ReadySuggestions {
    suggestions: Suggestion[];
    transcriptionID: number;

    constructor(suggestions: Suggestion[], id: number) {
      this.suggestions = suggestions;
      this.transcriptionID = id;
    }
  }

  /**
   * Corresponds to the 'suggestionsready' ModelManager event.
   */
  export type ReadySuggestionsHandler = (prediction: ReadySuggestions) => boolean;

  export type ModelChangeEnum = 'loaded'|'unloaded';
  /**
   * Corresponds to the 'modelchange' ModelManager event.
   */
  export type ModelChangeHandler = (state: ModelChangeEnum) => boolean;

  /**
   * Covers both 'tryaccept' and 'tryrevert' events.
   */
  export type TryUIHandler = (source: string) => boolean;

  type SupportedEventNames = "suggestionsready" | "invalidatesuggestions" | "modelchange" | "tryaccept" | "tryrevert";
  type SupportedEventHandler = InvalidateSuggestionsHandler | ReadySuggestionsHandler | ModelChangeHandler | TryUIHandler;

  export class ModelManager {
    private lmEngine: LMLayer;
    private currentModel?: ModelSpec;
    private configuration?: Configuration;
    private currentPromise?: Promise<Suggestion[]>;

    private recentTranscriptions: Transcription[] = [];

    private _mayPredict: boolean = true;
    private _mayCorrect: boolean = true;

    // Tracks registered models by ID.
    private registeredModels: {[id: string]: ModelSpec} = {};

    // Allows for easy model lookup by language code; useful when switching keyboards.
    private languageModelMap: {[language:string]: ModelSpec} = {};

    private static EVENT_PREFIX: string = "kmw.mm.";
    private static readonly TRANSCRIPTION_BUFFER: 10 = 10;

    init() {
      let keyman = com.keyman.singleton;
      // Establishes KMW's platform 'capabilities', which limit the range of context a LMLayer
      // model may expect.
      let capabilities: Capabilities = {
        maxLeftContextCodePoints: 64,
        // Since the apps don't yet support right-deletions.
        maxRightContextCodePoints: keyman.isEmbedded ? 0 : 64
      }

      if(!this.canEnable()) {
        return;
      }

      this.lmEngine = new LMLayer(capabilities);
      
      // Registers this module for keyboard (language) and model change events.
      keyman['addEventListener']('keyboardchange', this.onKeyboardChange.bind(this));
    }

    public get activeModel(): ModelSpec {
      return this.currentModel;
    }

    public unloadModel() {
      this.lmEngine.unloadModel();
      delete this.currentModel;
      delete this.configuration;
    }

    private loadModel(model: ModelSpec): Promise<void> {
      if(!model) {
        throw new Error("Null reference not allowed.");
      }

      let file = model.path;
      let mm = this;

      // We should wait until the model is successfully loaded before setting our state values.
      return this.lmEngine.loadModel(file).then(function(config: Configuration) { 
        mm.currentModel = model;
        mm.configuration = config;
      });
    }

    onKeyboardChange(kbdInfo?: KeyboardChangeData | string) {
      let keyman = com.keyman.singleton;
      let mm: ModelManager = this;

      if(!this.mayPredict) {
        return Promise.resolve();
      }

      if(typeof kbdInfo == 'string') {  // This case refers to the active language code.
        kbdInfo = {
          ['internalName']: keyman.keyboardManager.getActiveKeyboardName(),
          ['languageCode']: kbdInfo,
          ['indirect']: true
        }
      }

      let lgCode = kbdInfo['languageCode'];
      let model = this.languageModelMap[lgCode];
      var loadPromise: Promise<void>;

      if(this.currentModel !== model) {
        if(this.currentModel) {
          this.unloadModel();
          keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'modelchange', 'unloaded');
        }

        if(model) {
          loadPromise = this.loadModel(model);
        }

        // If we're loading a model, we need to defer until its completion before we report a change of state.
        if(loadPromise) {
          let mm = this;
          loadPromise.then(function() {
            // Because this is executed from a Promise, it's possible to have a race condition
            // where the 'loaded' event triggers after an 'unloaded' event meant to disable the model.
            // (Especially in the embedded apps.)  This will catch these cases.
            if(mm.mayPredict) {
              keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'modelchange', 'loaded');
            } else {
              mm.unloadModel();
            }
          }).catch(function(failReason: any) {
            // Does this provide enough logging information?
            console.error("Could not load model '" + model.id + "': " + failReason);
          });
        }
      }
    }

    // Accessible publicly as keyman.modelManager.register(model: ModelSpec)
    register(model: ModelSpec): void {
      let keyman = com.keyman.singleton;
      let activeLanguage = keyman.keyboardManager.getActiveLanguage();

      this.registeredModels[model.id] = model;

      // Register the model for each targeted language code variant.
      let mm = this;
      model.languages.forEach(function(code: string) {
        mm.languageModelMap[code] = model;

        // The model's for our active language!  Activate it!
        if(code == activeLanguage) {
          // Manually trigger our model-update event function.
          mm.onKeyboardChange(code);
        }
      });
    }

    deregister(modelId: string): void {
      let keyman = com.keyman.singleton;
      let model: ModelSpec;

      // Remove the model from the id-lookup associative array.
      if(this.registeredModels[modelId]) {
        model = this.registeredModels[modelId];
        delete this.registeredModels[modelId];
      } else {
        return;
      }

      // Is it the active model?
      if(this.currentModel && this.currentModel.id == modelId) {
        this.unloadModel();
        keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'modelchange', 'unloaded');
      }

      // Ensure the model is deregistered for each targeted language code variant.
      let mm = this;
      model.languages.forEach(function(code: string) {
        if(mm.languageModelMap[code].id == modelId) {
          delete mm.languageModelMap[code];
        }
      });
    }

    isRegistered(model: ModelSpec): boolean {
      return !! this.registeredModels[model.id];
    }

    /**
     * Function     addEventListener
     * Scope        Public
     * @param       {string}            event     event to handle
     * @param       {function(Event)}   func      event handler function
     * @return      {boolean}                     value returned by util.addEventListener
     * Description  Wrapper function to add and identify handlers for ModelManager events
     */       
    ['addEventListener'](event: SupportedEventNames, func: SupportedEventHandler): boolean {
      let keyman = com.keyman.singleton;
      return keyman.util.addEventListener(ModelManager.EVENT_PREFIX + event, func);
    }

    /**
     * Function     removeEventListener
     * Scope        Public
     * @param       {string}            event     event to handle
     * @param       {function(Event)}   func      event handler function
     * @return      {boolean}                     value returned by util.addEventListener
     * Description  Wrapper function to remove previously-added handlers for ModelManager events
     */       
    ['removeEventListener'](event: SupportedEventNames, func: SupportedEventHandler): boolean {
      let keyman = com.keyman.singleton;
      return keyman.util.removeEventListener(ModelManager.EVENT_PREFIX + event, func);
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
      let keyman = com.keyman.singleton;

      let context = new TranscriptionContext(Mock.from(target), this.configuration);
      return this.lmEngine.wordbreak(context);
    }

    public predict(transcription?: Transcription) {
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
        let t = text.Processor.getOutputTarget();
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

      if(this.recentTranscriptions.length > ModelManager.TRANSCRIPTION_BUFFER) {
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

    public get enabled(): boolean {
      return this.activeModel && this._mayPredict;
    }

    private canEnable(): boolean {
      let keyman = com.keyman.singleton;

      if(keyman.util.getIEVersion() == 10) {
        console.warn("KeymanWeb cannot properly initialize its WebWorker in this version of IE.");
        return false;
      } else if(keyman.util.getIEVersion() < 10) {
        console.warn("WebWorkers are not supported in this version of IE.");
        return false;
      }

      return true;
    }

    private doEnable(flag: boolean) {
      let keyman = com.keyman.singleton;

      if(flag) {
        let lgCode = keyman.keyboardManager.getActiveLanguage();
        if(this.languageModelMap[lgCode]) {
          // Just reuse the existing model-change trigger code.
          this.onKeyboardChange(lgCode);
        }
      } else {
        if(this.activeModel) { // We only need to unload a model when one is actually loaded.
          this.unloadModel();
        }

        // Ensure that the banner is unloaded.
        keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'modelchange', 'unloaded');
      }
    }

    public get mayPredict() {
      return this._mayPredict;
    }

    public set mayPredict(flag: boolean) {
      let enabled = this.enabled;

      if(!this.canEnable()) {
        return;
      }

      this._mayPredict = flag;
      if(enabled != this.enabled || flag) {
        this.doEnable(flag);
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
