// Defines the text processing KMW core.
///<reference path="../keyboardProcessor.ts" />
// Defines the LMLayer's outer shell
///<reference path="../../includes/lmlayer.ts" />
// Defines the ModelManager and its related types.
///<reference path="../../keyboards/kmwkeyboards.ts" />

namespace com.keyman.text.prediction {
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
        }

        if(model) {
          loadPromise = core.languageProcessor.loadModel(model);
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
  }
}
