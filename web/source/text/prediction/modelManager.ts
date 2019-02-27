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

  export class ModelManager {
    private lmEngine: LMLayer;

    // Tracks registered models by ID.
    private registeredModels: {[id: string]: ModelSpec} = {};

    // Allows for easy model lookup by language code; useful when switching keyboards.
    private languageModelMap: {[language:string]: ModelSpec} = {};

    init() {
      let keyman = com.keyman.singleton;
      this.lmEngine = new LMLayer();
      
      // Registers this module for keyboard (and thus, language) change events.
      keyman['addEventListener']('keyboardchange', this.onKeyboardChange.bind(this));
    }

    onKeyboardChange(kbdInfo: KeyboardChangeData) {
      let lgCode = kbdInfo['languageCode'];

      let model = this.languageModelMap[lgCode];

      if(model) {
        // TODO:  Activate this model within the LMLayer!
        let file = model.path;

        //this.lmEngine.initialize(file)  // Currently unsupported.
        console.log("Model detected!");
      }
    }

    // Accessible publicly as keyman.modelManager.register(model: ModelSpec)
    register(model: ModelSpec): void {
      this.registeredModels[model.id] = model;

      model.languages.forEach(function(code: string) {
        this.languageModelMap[code] = model;
      }, this);
    }

    isRegistered(model: ModelSpec): boolean {
      return !! this.registeredModels[model.id];
    }
  }
}