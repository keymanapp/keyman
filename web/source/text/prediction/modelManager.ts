// Defines the text processing KMW core.
///<reference path="../processor.ts" />
// Defines the LMLayer's outer shell
///<reference path="../../includes/lmlayer.ts" />
// Defines the KeyboardManager and its related types.
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

  export type ModelChangeEnum = 'loaded'|'unloaded'|'changed';
  /**
   * Corresponds to the 'modelchange' ModelManager event.
   */
  export type ModelChangeHandler = (state: ModelChangeEnum) => boolean;

  export class ModelManager {
    private lmEngine: LMLayer;
    private currentModel?: ModelSpec;
    private configuration?: Configuration;
    private currentPromise?: Promise<Suggestion[]>;

    private recentTranscriptions: Transcription[] = [];

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
        maxLeftContextCodeUnits: 64
      }
      this.lmEngine = new LMLayer(capabilities);
      
      // Registers this module for keyboard (and thus, language) change events.
      keyman['addEventListener']('keyboardchange', this.onKeyboardChange.bind(this));
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

    onKeyboardChange(kbdInfo: KeyboardChangeData) {
      let lgCode = kbdInfo['languageCode'];
      let model = this.languageModelMap[lgCode];
      var loadPromise: Promise<number>;

      if(this.currentModel !== model) {
        let stateChange = 0; // Base value; will not remain the same 

        if(this.currentModel) {
          this.unloadModel();
          stateChange += 2;
        }

        if(model) {
          loadPromise = this.loadModel(model).then(function() {
            // Track the state bit flags appropriately.
            return stateChange + 1;
          });
        }

        if(stateChange == 0) {
          console.warn("Unexpected lack of model state change in ModelManager.onKeyboardChange!");
          return;
        }

        // If we're loading a model, we need to defer until its completion before we report a change of state.
        if(loadPromise) {
          let mm = this;
          loadPromise.then(function(stateBitFlags: number) {
            mm.doModelChange(stateBitFlags);
          }).catch(function(failReason: any) {
            // Does this provide enough logging information?
            console.error("Could not load model '" + model.id + "': " + failReason);
          });
        } else {
          // No promises?  Just directly trigger the event, then!
          this.doModelChange(stateChange);
        }
      }
    }

    private doModelChange(stateBitFlags: number) {
      let keyman = com.keyman.singleton;

      let changeParam: ModelChangeEnum = stateBitFlags == 1 ? 'loaded' : (stateBitFlags == 2 ? 'unloaded' : 'changed');
      keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'modelchange', changeParam);
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
          mm.onKeyboardChange({
            ['internalName']: keyman.keyboardManager.getActiveKeyboardName(),
            ['languageCode']: code,
            ['indirect']: true
          });
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
     * Description  Wrapper function to add and identify KeymanWeb-specific event handlers
     */       
    ['addEventListener'](event: string, func: ReadySuggestionsHandler | InvalidateSuggestionsHandler): boolean {
      let keyman = com.keyman.singleton;
      return keyman.util.addEventListener(ModelManager.EVENT_PREFIX + event, func);
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

    public shutdown() {
      this.lmEngine.shutdown();
    }
  }
}