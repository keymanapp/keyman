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

  type SupportedEventNames = "suggestionsready" | "invalidatesuggestions" | "modelchange";
  type SupportedEventHandler = InvalidateSuggestionsHandler | ReadySuggestionsHandler | ModelChangeHandler;

  export class ModelManager {
    private lmEngine: LMLayer;
    private currentModel?: ModelSpec;
    private currentPromise: Promise<Suggestion[]>;

    private _enabled: boolean = true;

    // Tracks registered models by ID.
    private registeredModels: {[id: string]: ModelSpec} = {};

    // Allows for easy model lookup by language code; useful when switching keyboards.
    private languageModelMap: {[language:string]: ModelSpec} = {};

    private static EVENT_PREFIX: string = "kmw.mm.";

    init() {
      let keyman = com.keyman.singleton;
      // Establishes KMW's platform 'capabilities', which limit the range of context a LMLayer
      // model may expect.
      let capabilities: Capabilities = {
        maxLeftContextCodeUnits: 64
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
    }

    private loadModel(model: ModelSpec) {
      if(!model) {
        throw new Error("Null reference not allowed.");
      }

      let file = model.path;
      this.lmEngine.loadModel(file);
      this.currentModel = model;
    }

    onKeyboardChange(kbdInfo?: KeyboardChangeData | string) {
      let keyman = com.keyman.singleton;

      if(!this._enabled) {
        return Promise.resolve();
      }

      if(typeof kbdInfo == 'string') {
        kbdInfo = {
          ['internalName']: keyman.keyboardManager.getActiveKeyboardName(),
          ['languageCode']: kbdInfo,
          ['indirect']: true
        }
      }

      let lgCode = kbdInfo['languageCode'];
      let model = this.languageModelMap[lgCode];

      if(this.currentModel !== model) {
        let stateChange = 0; // Base value; will not remain the same 

        if(this.currentModel) {
          this.unloadModel();
          stateChange += 2;
        }

        if(model) {
          this.loadModel(model);
          stateChange += 1;
        }

        if(stateChange == 0) {
          console.warn("Unexpected lack of model state change in ModelManager.onKeyboardChange!");
          return;
        }

        let changeParam: ModelChangeEnum = stateChange == 1 ? 'loaded' : (stateChange == 2 ? 'unloaded' : 'changed');
        keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'modelchange', changeParam);
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
     * Description  Wrapper function to add and identify KeymanWeb-specific event handlers
     */       
    ['addEventListener'](event: SupportedEventNames, func: SupportedEventHandler): boolean {
      let keyman = com.keyman.singleton;
      return keyman.util.addEventListener(ModelManager.EVENT_PREFIX + event, func);
    }

    public predict(transform: Transform, context: Context) {
      // If there's no active model, there can be no predictions.
      if(!this.currentModel) { // Will always be undefined when _enabled == false.
        return;
      }

      let keyman = com.keyman.singleton;
      var promise = this.currentPromise = this.lmEngine.predict(transform, context);

      // We've already invalidated any suggestions resulting from any previously-existing Promise -
      // may as well officially invalidate them via event.
      keyman.util.callEvent(ModelManager.EVENT_PREFIX + "invalidatesuggestions", null);

      let mm = this;
      promise.then(function(suggestions: Suggestion[]) {
        if(promise == mm.currentPromise) {
          keyman.util.callEvent(ModelManager.EVENT_PREFIX + "suggestionsready", suggestions);
        }
      })
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
  }
}