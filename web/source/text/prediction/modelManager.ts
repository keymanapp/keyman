// Defines the text processing KMW core.
///<reference path="../keyboardProcessor.ts" />
// Defines the LMLayer's outer shell
///<reference path="../../includes/lmlayer.ts" />
// Defines the ModelManager and its related types.
///<reference path="../../keyboards/kmwkeyboards.ts" />

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
    // Tracks registered models by ID.
    private registeredModels: {[id: string]: ModelSpec} = {};

    // Allows for easy model lookup by language code; useful when switching keyboards.
    languageModelMap: {[language:string]: ModelSpec} = {};

    static EVENT_PREFIX: string = "kmw.mm.";
    
    init() {
      let keyman = com.keyman.singleton;
      
      // Registers this module for keyboard (language) and model change events.
      keyman['addEventListener']('keyboardchange', this.onKeyboardChange.bind(this));
    }

    onKeyboardChange(kbdInfo?: keyboards.KeyboardChangeData | string) {
      let keyman = com.keyman.singleton;
      let core = keyman.core;

      if(!core.languageProcessor.mayPredict) {
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

      if(core.activeModel !== model) {
        if(core.activeModel) {
          core.languageProcessor.unloadModel();
          keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'modelchange', 'unloaded');
        }

        if(model) {
          loadPromise = core.languageProcessor.loadModel(model);
        }

        // If we're loading a model, we need to defer until its completion before we report a change of state.
        if(loadPromise) {
          let mm = this;
          loadPromise.then(function() {
            // Because this is executed from a Promise, it's possible to have a race condition
            // where the 'loaded' event triggers after an 'unloaded' event meant to disable the model.
            // (Especially in the embedded apps.)  This will catch these cases.
            if(core.languageProcessor.mayPredict) {
              keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'modelchange', 'loaded');
            } else {
              core.languageProcessor.unloadModel();
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
      let core = keyman.core;
      let model: ModelSpec;

      // Remove the model from the id-lookup associative array.
      if(this.registeredModels[modelId]) {
        model = this.registeredModels[modelId];
        delete this.registeredModels[modelId];
      } else {
        return;
      }

      // Is it the active model?
      if(core.activeModel && core.activeModel.id == modelId) {
        core.languageProcessor.unloadModel();
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

    doEnable(flag: boolean) {
      let keyman = com.keyman.singleton;

      if(flag) {
        let lgCode = keyman.keyboardManager.getActiveLanguage();
        if(keyman.modelManager.languageModelMap[lgCode]) {
          // Just reuse the existing model-change trigger code.
          keyman.modelManager.onKeyboardChange(lgCode);
        }
      } else {
        if(keyman.core.activeModel) { // We only need to unload a model when one is actually loaded.
          keyman.core.languageProcessor.unloadModel();
        }

        // Ensure that the banner is unloaded.
        keyman.util.callEvent(ModelManager.EVENT_PREFIX + 'modelchange', 'unloaded');
      }
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
  }
}
