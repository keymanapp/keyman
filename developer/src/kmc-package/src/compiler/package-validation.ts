import { KmpJsonFile, CompilerCallbacks } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';

export class PackageValidation {

  constructor(private callbacks: CompilerCallbacks) {
  }

  public validate(kmpJson: KmpJsonFile.KmpJsonFile) {
    if(!this.checkKeyboards(kmpJson)) {
      return false;
    }
    if(!this.checkLexicalModels(kmpJson)) {
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

  private checkLexicalModels(kmpJson: KmpJsonFile.KmpJsonFile): boolean {
    if(kmpJson.lexicalModels?.length > 0 && kmpJson.keyboards?.length > 0) {
      this.callbacks.reportMessage(CompilerMessages.Error_PackageCannotContainBothModelsAndKeyboards());
      return false;
    }

    if(kmpJson.lexicalModels) {
      for(let model of kmpJson.lexicalModels) {
        this.checkForDuplicatedLanguages('model', model.id, model.languages);
      }
    }

    return true;
  }

  private checkKeyboards(kmpJson: KmpJsonFile.KmpJsonFile): boolean {
    if(kmpJson.keyboards) {
      for(let keyboard of kmpJson.keyboards) {
        this.checkForDuplicatedLanguages('keyboard', keyboard.id, keyboard.languages);
      }
    }

    return true;
  }
}
