import { ModelSpec } from "@keymanapp/input-processor";

export default class ModelManager {
  // Tracks registered models by ID.
  private registeredModels: {[id: string]: ModelSpec} = {};

  // Allows for easy model lookup by language code; useful when switching keyboards.
  languageModelMap: {[language:string]: ModelSpec} = {};

  modelForLanguage(lgCode: string) {
    return this.languageModelMap[lgCode];
  }

  // Accessible publicly as keyman.modelCache.register(model: ModelSpec)
  register(model: ModelSpec): void {
    // Forcibly lowercase the model ID before proceeding.
    model.id = model.id.toLowerCase();

    if(JSON.stringify(model) == JSON.stringify(this.registeredModels[model.id])) {
      // We are already registered, let's not go through and re-register
      // because we'll already have the correct model active
      return;
    }
    this.registeredModels[model.id] = model;

    // Register the model for each targeted language code variant.
    let mm = this;
    model.languages.forEach(function(code: string) {
      // Prevent null / undefined codes; they're invalid.
      if(!code) {
        console.warn("Null / undefined language codes are not permitted for registration.");
        return;
      }

      mm.languageModelMap[code] = model;
    });
  }

  unregister(modelId: string): ModelSpec {
    let model: ModelSpec;

    modelId = modelId.toLowerCase();

    // Remove the model from the id-lookup associative array.
    if(this.registeredModels[modelId]) {
      model = this.registeredModels[modelId];
      delete this.registeredModels[modelId];
    } else {
      return null;
    }

    // Ensure the model is deregistered for each targeted language code variant.
    let mm = this;
    model.languages.forEach(function(code: string) {
      if(mm.languageModelMap[code].id == modelId) {
        delete mm.languageModelMap[code];
      }
    });

    return model;
  }

  isRegistered(model: ModelSpec): boolean {
    return !! this.registeredModels[model.id.toLowerCase()];
  }
}
