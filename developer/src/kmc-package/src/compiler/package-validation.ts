import { KmpJsonFile, CompilerCallbacks } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';

// const SLexicalModelExtension = '.model.js';

// The keyboard ID SHOULD adhere to this pattern:
const KEYBOARD_ID_PATTERN_PACKAGE = /^[a-z_][a-z0-9_]*\.(kps|kmp)$/;

// The model ID SHOULD adhere to this pattern:
//                                 author           .bcp47             .uniq
const MODEL_ID_PATTERN_PACKAGE = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.(kps|kmp)$/;
// const MODEL_ID_PATTERN_JS      = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.js$/;
// const MODEL_ID_PATTERN_TS      = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.ts$/;
// const MODEL_ID_PATTERN_PROJECT = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.kpj$/;

export class PackageValidation {

  constructor(private callbacks: CompilerCallbacks) {
  }

  public validate(filename: string, kmpJson: KmpJsonFile.KmpJsonFile) {
    if(!this.checkForModelsAndKeyboardsInSamePackage(kmpJson)) {
      return false;
    }
    if(!this.checkKeyboards(filename, kmpJson)) {
      return false;
    }
    if(!this.checkLexicalModels(filename, kmpJson)) {
      return false;
    }
    return true;
  }

  private checkForDuplicatedLanguages(resourceType: 'keyboard'|'model', id: string, languages: KmpJsonFile.KmpJsonFileLanguage[]) {
    let tags: {[index:string]: boolean} = {};
    for(let lang of languages) {
      const langTag = lang.id.toLowerCase();
      if(tags[langTag]) {
        this.callbacks.reportMessage(CompilerMessages.Warn_PackageShouldNotRepeatLanguages({resourceType:resourceType, id:id, tag:lang.id}));
      } else {
        tags[langTag] = true;
      }
    }
  }

  private checkForModelsAndKeyboardsInSamePackage(kmpJson: KmpJsonFile.KmpJsonFile): boolean {
    if(kmpJson.lexicalModels?.length > 0 && kmpJson.keyboards?.length > 0) {
      this.callbacks.reportMessage(CompilerMessages.Error_PackageCannotContainBothModelsAndKeyboards());
      return false;
    }

    return true;
  }

  private checkLexicalModels(filename: string, kmpJson: KmpJsonFile.KmpJsonFile): boolean {
    if(!kmpJson.lexicalModels || kmpJson.lexicalModels.length == 0) {
      return true;
    }

    filename = this.callbacks.path.basename(filename);

    if(!MODEL_ID_PATTERN_PACKAGE.test(filename)) {
      this.callbacks.reportMessage(CompilerMessages.Warn_PackageNameDoesNotFollowLexicalModelConventions({filename}));
    }

    for(let model of kmpJson.lexicalModels) {
      this.checkForDuplicatedLanguages('model', model.id, model.languages);
    }

    return true;
  }

  private checkKeyboards(filename: string, kmpJson: KmpJsonFile.KmpJsonFile): boolean {
    if(!kmpJson.keyboards || kmpJson.keyboards.length == 0) {
      return true;
    }

    filename = this.callbacks.path.basename(filename);

    if(!KEYBOARD_ID_PATTERN_PACKAGE.test(filename)) {
      this.callbacks.reportMessage(CompilerMessages.Warn_PackageNameDoesNotFollowKeyboardConventions({filename}));
    }

    for(let keyboard of kmpJson.keyboards) {
      this.checkForDuplicatedLanguages('keyboard', keyboard.id, keyboard.languages);
    }

    return true;
  }
}
