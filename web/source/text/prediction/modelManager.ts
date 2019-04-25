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
      this.startOfBuffer = this.left._kmwLength() > config.leftContextCodeUnits;
      if(!this.startOfBuffer) {
        // Our custom substring version will return the last n characters if param #1 is given -n.
        this.left = this.left._kmwSubstr(-config.leftContextCodeUnits);
      }

      this.right = mock.getTextAfterCaret();
      this.endOfBuffer = this.right._kmwLength() > config.rightContextCodeUnits;
      if(!this.endOfBuffer) {
        this.right = this.right._kmwSubstr(0, config.rightContextCodeUnits);
      }
    }
  }

  /**
   * Corresponds to the 'invalidatesuggestions' ModelManager event.
   */
  export type InvalidateSuggestionsHandler = () => boolean;

  /**
   * Corresponds to the 'suggestionsready' ModelManager event.
   */
  export type ReadySuggestionsHandler = (suggestions: Suggestion[]) => boolean;

  export type ModelChangeEnum = 'loaded'|'unloaded';
  /**
   * Corresponds to the 'modelchange' ModelManager event.
   */
  export type ModelChangeHandler = (state: ModelChangeEnum) => boolean;

  type SupportedEventNames = "suggestionsready" | "invalidatesuggestions" | "modelchange" | "tryaccept" | "tryrevert";
  type SupportedEventHandler = InvalidateSuggestionsHandler | ReadySuggestionsHandler | ModelChangeHandler;

  export class ModelManager {
    private lmEngine: LMLayer;
    private currentModel?: ModelSpec;
    private configuration?: Configuration;
    private currentPromise?: Promise<Suggestion[]>;

    private recentTranscriptions: Transcription[] = [];

    private _enabled: boolean = true;

    // Tracks registered models by ID.
    private registeredModels: {[id: string]: ModelSpec} = {};

    // Allows for easy model lookup by language code; useful when switching keyboards.
    private languageModelMap: {[language:string]: ModelSpec} = {};

    private static EVENT_PREFIX: string = "kmw.mm.";
    private static readonly TRANSCRIPTION_BUFFER: 10;

    init() {
      let keyman = com.keyman.singleton;
      // Establishes KMW's platform 'capabilities', which limit the range of context a LMLayer
      // model may expect.
      let capabilities: Capabilities = {
        maxLeftContextCodeUnits: 64,
        // Since the apps don't yet support right-deletions.
        maxRightContextCodeUnits: keyman.isEmbedded ? 0 : 64
      }
      this.lmEngine = new LMLayer(capabilities);
      
      // Registers this module for keyboard (language) and model change events.
      keyman['addEventListener']('keyboardchange', this.onKeyboardChange.bind(this));
    }

    public get activeModel(): ModelSpec {
      return this.currentModel;
    }

    private unloadModel() {
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

      if(!this._enabled) {
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
            keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'modelchange', 'loaded');
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

    public predict(transcription: Transcription) {
      // If there's no active model, there can be no predictions.
      // We'll also be missing important data needed to even properly REQUEST the predictions.
      if(!this.currentModel || !this.configuration) {
        return;
      }
      let context = new TranscriptionContext(transcription.preInput, this.configuration);
      this.recordTranscription(transcription);
      this.predict_internal(transcription.transform, context);
    }

    private predict_internal(transform: Transform, context: Context) {
      let keyman = com.keyman.singleton;
      var promise = this.currentPromise = this.lmEngine.predict(transform, context);

      // We've already invalidated any suggestions resulting from any previously-existing Promise -
      // may as well officially invalidate them via event.
      keyman.util.callEvent(ModelManager.EVENT_PREFIX + "invalidatesuggestions", null);

      let mm = this;
      promise.then(function(suggestions: Suggestion[]) {
        if(promise == mm.currentPromise) {
          keyman.util.callEvent(ModelManager.EVENT_PREFIX + "suggestionsready", suggestions);
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
      return this._enabled;
    }

    public set enabled(flag: boolean) {
      let keyman = com.keyman.singleton;
      this._enabled = flag;

      if(flag) {
        let lgCode = keyman.keyboardManager.getActiveLanguage();
        if(this.registeredModels[lgCode]) {
          // Just reuse the existing model-change trigger code.
          this.onKeyboardChange(lgCode);
        }
      } else {
        this.unloadModel();
        keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'modelchange', 'unloaded');
      }
    }

    public tryAcceptSuggestion(): boolean {
      let keyman = com.keyman.singleton;
      
      // Handlers of this event should return 'false' when the 'try' is successful.
      return !keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'tryaccept', null);
    }

    public tryRevertSuggestion(): boolean {
      let keyman = com.keyman.singleton;
      
      // Handlers of this event should return 'false' when the 'try' is successful.
      return !keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'tryrevert', null);
    }
  }
}
