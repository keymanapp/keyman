import { KmpJsonFile, CompilerCallbacks, CompilerOptions, KeymanFileTypes } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { keymanEngineForWindowsFiles, keymanForWindowsInstallerFiles, keymanForWindowsRedistFiles } from './redist-files.js';

// The keyboard ID SHOULD adhere to this pattern:
const KEYBOARD_ID_PATTERN_PACKAGE = /^[a-z_][a-z0-9_]*\.(kps|kmp)$/;

// The model ID SHOULD adhere to this pattern:
//                                 author           .bcp47             .uniq
const MODEL_ID_PATTERN_PACKAGE = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.(kps|kmp)$/;
// const MODEL_ID_PATTERN_JS      = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.js$/;
// const MODEL_ID_PATTERN_TS      = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.ts$/;
// const MODEL_ID_PATTERN_PROJECT = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.kpj$/;

// "Content files" within the package should adhere to these pattern:
const CONTENT_FILE_BASENAME_PATTERN = /^[a-z0-9_+.-]+$/i; // base names can be case insensitive
const CONTENT_FILE_EXTENSION_PATTERN = /^(\.[a-z0-9_-]+)?$/;  // extensions should be lower-case or empty

export class PackageValidation {

  constructor(private callbacks: CompilerCallbacks, private options: CompilerOptions) {
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
    if(!this.checkContentFiles(kmpJson)) {
      return false;
    }
    if(!this.checkPackageInfo(kmpJson)) {
      return false;
    }
    return true;
  }

  private checkForDuplicatedOrNonMinimalLanguages(resourceType: 'keyboard'|'model', id: string, languages: KmpJsonFile.KmpJsonFileLanguage[]): boolean {
    let minimalTags: {[tag: string]: string} = {};

    if(languages.length == 0) {
      if(resourceType == 'keyboard') {
        this.callbacks.reportMessage(CompilerMessages.Warn_KeyboardShouldHaveAtLeastOneLanguage({id}));
      } else {
        this.callbacks.reportMessage(CompilerMessages.Error_ModelMustHaveAtLeastOneLanguage({id}));
        return false;
      }
    }

    for(let lang of languages) {
      let locale;
      try {
        locale = new Intl.Locale(lang.id);
      } catch(e: any) {
        this.callbacks.reportMessage(CompilerMessages.Error_LanguageTagIsNotValid({resourceType, id, lang: lang.id, e}));
        return false;
      }

      const minimalTag = locale.minimize().toString();

      if(minimalTag.toLowerCase() !== lang.id.toLowerCase()) {
        this.callbacks.reportMessage(CompilerMessages.Hint_LanguageTagIsNotMinimal({resourceType, id, actual: lang.id, expected: minimalTag}));
      }

      if(minimalTags[minimalTag]) {
        this.callbacks.reportMessage(CompilerMessages.Hint_PackageShouldNotRepeatLanguages({resourceType, id, minimalTag, firstTag: lang.id, secondTag: minimalTags[minimalTag]}));
      }
      else {
        minimalTags[minimalTag] = lang.id;
      }
    }

    return true;
  }

  private checkForModelsAndKeyboardsInSamePackage(kmpJson: KmpJsonFile.KmpJsonFile): boolean {
    if(kmpJson.lexicalModels?.length > 0 && kmpJson.keyboards?.length > 0) {
      this.callbacks.reportMessage(CompilerMessages.Error_PackageCannotContainBothModelsAndKeyboards());
      return false;
    }

    if(!kmpJson.lexicalModels?.length && !kmpJson.keyboards?.length) {
      // Note: we require at least 1 keyboard or model in the package. This may
      // change in the future if we start to use packages to distribute, e.g.
      // localizations or themes.
      this.callbacks.reportMessage(CompilerMessages.Error_PackageMustContainAModelOrAKeyboard());
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
      if(!this.checkForDuplicatedOrNonMinimalLanguages('model', model.id, model.languages)) {
        return false;
      }
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
      if(!this.checkForDuplicatedOrNonMinimalLanguages('keyboard', keyboard.id, keyboard.languages)) {
        return false;
      }
      // Note: package-version-validation verifies that there is a corresponding
      // content file for each keyboard
    }

    return true;
  }

  private checkContentFiles(kmpJson: KmpJsonFile.KmpJsonFile): boolean {
    for(let file of kmpJson.files) {
      if(!this.checkContentFile(file)) {
        return false;
      }
    }

    return true;
  }

  private checkContentFile(file: KmpJsonFile.KmpJsonFileContentFile): boolean {
    const filename = this.callbacks.path.basename(file.name);
    const ext = this.callbacks.path.extname(filename);
    const base = filename.substring(0, filename.length-ext.length);
    if(this.options.checkFilenameConventions) {
      if(!CONTENT_FILE_BASENAME_PATTERN.test(base) || !CONTENT_FILE_EXTENSION_PATTERN.test(ext)) {
        this.callbacks.reportMessage(CompilerMessages.Warn_FileInPackageDoesNotFollowFilenameConventions({filename}));
      }
    }

    if(!this.checkIfContentFileIsDangerous(file)) {
      return false;
    }

    return true;
  }

  private checkIfContentFileIsDangerous(file: KmpJsonFile.KmpJsonFileContentFile): boolean {
    const filename = this.callbacks.path.basename(file.name).toLowerCase();

    // # Test for inclusion of redistributable files

    if(keymanForWindowsInstallerFiles.includes(filename) ||
        keymanForWindowsRedistFiles.includes(filename) ||
        keymanEngineForWindowsFiles.includes(filename)) {
      this.callbacks.reportMessage(CompilerMessages.Warn_RedistFileShouldNotBeInPackage({filename}));
    }

    // # Test for inclusion of .doc or .docx files

    if(filename.match(/\.doc(x?)$/)) {
      this.callbacks.reportMessage(CompilerMessages.Warn_DocFileDangerous({filename}));
    }

    // # Test for inclusion of keyboard source files
    //
    // We treat this as a hint, because it's not a dangerous problem, just
    // something that suggests that perhaps they are trying to distribute the
    // wrong files.
    //
    // Note: we allow .xml in the package because there are other files
    // which may be valid, not just LDML keyboards
    const fileType = KeymanFileTypes.sourceTypeFromFilename(file.name);
    if(fileType !== null && fileType !== KeymanFileTypes.Source.LdmlKeyboard) {
      this.callbacks.reportMessage(CompilerMessages.Hint_PackageContainsSourceFile({filename: file.name}));
    }

    return true;
  }

  private checkPackageInfo(file: KmpJsonFile.KmpJsonFile) {
    if(!file.info || !file.info.name || !file.info.name.description.trim()) {
      this.callbacks.reportMessage(CompilerMessages.Error_PackageNameCannotBeBlank());
      return false;
    }

    return true;
  }
}
