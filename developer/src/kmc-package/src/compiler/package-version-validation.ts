import { KmpJsonFile, CompilerCallbacks, KpsFile, KmxFileReader, KmxFileReaderError, KMX, KeymanFileTypes } from '@keymanapp/common-types';
import { KeymanTarget, TouchKeymanTargets } from './keyman-targets.js';
import { CompilerMessages } from './messages.js';
import { getCompiledWebKeyboardMetadata, WebKeyboardMetadata } from './web-keyboard-metadata.js';

// The DEFAULT_VERSION used to be '1.0', but we now use '0.0' to allow
// pre-release 0.x keyboards to be considered later than a keyboard without
// any version metadata at all.
// TODO: move to a better location
const DEFAULT_VERSION = '0.0';

export class PackageVersionValidation {

  constructor(private callbacks: CompilerCallbacks) {}

  /**
   * Verifies version information in corresponding keyboards and updates kmpJson
   * metadata as the version information can be out of sync in the .kps file
   * after updating a contained keyboard.
   * @param kpsFilename
   * @param kps
   * @param kmp
   * @returns
   */
  public validateAndUpdateVersions(kpsFilename: string, kps: KpsFile.KpsFile, kmp: KmpJsonFile.KmpJsonFile) {
    const followKeyboardVersion = kps.options?.followKeyboardVersion !== undefined;

    if(followKeyboardVersion) {
      if(!this.checkFollowKeyboardVersion(kps, kmp)) {
        return false;
      }
    }

    if(!kmp.keyboards) {
      // Lexical models don't have version metadata; only their packages.
      return true;
    }

    let result = true;

    // We now know we have at least one keyboard in the package

    for(let keyboard of kmp.keyboards) {
      const data = this.getKeyboardFileData(kpsFilename, kmp, keyboard);
      if(!data) {
        // Warnings or errors will have been raised by getKeyboardFileData
        result = false;
        continue;
      }

      if(data.kmx) {
        // get the targets from the .kmx
        this.verifyTargets(keyboard, data.kmx, kmp);
      }

      // Note: there is often version metadata in the .kps <Keyboard> element, but
      // we don't read from the metadata because we want to ensure we have the
      // most up-to-date keyboard version data here, from the compiled keyboard.

      const keyboardVersion = data.kmx ?
        this.getKeyboardVersionFromKmx(data.kmx) :
        data.js.keyboardVersion;

      if(followKeyboardVersion && keyboardVersion === null) {
        this.callbacks.reportMessage(CompilerMessages.Info_KeyboardFileHasNoKeyboardVersion({filename: keyboard.id}));
      }
      keyboard.version = keyboardVersion ?? DEFAULT_VERSION;

      if(result && followKeyboardVersion) {
        if(kmp.keyboards[0].version !== keyboard.version) {
          this.callbacks.reportMessage(CompilerMessages.Warn_KeyboardVersionsDoNotMatch({
            keyboard: keyboard.id,
            version: keyboard.version,
            firstKeyboard: kmp.keyboards[0].id,
            firstVersion: kmp.keyboards[0].version
          }));
        }
      }
    }

    if(result && followKeyboardVersion) {
      kmp.info.version = {description: kmp.keyboards[0].version};
    }

    return result;
  }

  private checkFollowKeyboardVersion(kps: KpsFile.KpsFile, kmp: KmpJsonFile.KmpJsonFile) {
    // Lexical model packages do not allow FollowKeyboardVersion
    if(kmp.lexicalModels && kmp.lexicalModels.length) {
      this.callbacks.reportMessage(CompilerMessages.Error_FollowKeyboardVersionNotAllowedForModelPackages());
      return false;
    }

    if(!kmp.keyboards || !kmp.keyboards.length) {
      this.callbacks.reportMessage(CompilerMessages.Error_FollowKeyboardVersionButNoKeyboards());
      return false;
    }

    return true;
  }

  private getKeyboardFileData(
    kpsFilename: string,
    kmp: KmpJsonFile.KmpJsonFile,
    keyboard: KmpJsonFile.KmpJsonFileKeyboard
  ): {kmx?: KMX.KEYBOARD, js?:WebKeyboardMetadata} {

    let isJavascript = false;
    let file = kmp.files.find(file => this.callbacks.path.basename(file.name, KeymanFileTypes.Binary.Keyboard) == keyboard.id);
    if(!file) {
      isJavascript = true;
      file = kmp.files.find(file => this.callbacks.path.basename(file.name, KeymanFileTypes.Binary.WebKeyboard) == keyboard.id);
      if(!file) {
        this.callbacks.reportMessage(CompilerMessages.Error_KeyboardContentFileNotFound({id:keyboard.id}));
        return null;
      }
    }

    const filename = this.callbacks.resolveFilename(kpsFilename, file.name);
    if(!this.callbacks.fs.existsSync(filename)) {
      this.callbacks.reportMessage(CompilerMessages.Error_KeyboardFileNotFound({filename}));
      return null;
    }

    //
    // load the data from file
    //
    let fileData;
    try {
      fileData = this.callbacks.loadFile(filename);
    } catch(e) {
      this.callbacks.reportMessage(CompilerMessages.Error_FileCouldNotBeRead({filename, e}));
      return null;
    }

    if(isJavascript) {
      const js = new TextDecoder().decode(fileData);
      return {js: getCompiledWebKeyboardMetadata(js)};
    }

    const kmxReader: KmxFileReader = new KmxFileReader();
    let kmx: KMX.KEYBOARD;
    try {
      kmx = kmxReader.read(fileData);
    } catch(e) {
      if(e instanceof KmxFileReaderError) {
        // The file couldn't be read, it might not be a .kmx file
        this.callbacks.reportMessage(CompilerMessages.Error_KeyboardFileNotValid({filename, e}));
        return null;
      } else {
        // Unknown error, bubble it up
        throw e;
      }
    }

    return {kmx};
  }

  private getKeyboardVersionFromKmx(
    kmx: KMX.KEYBOARD
  ): string {
    const store = kmx.stores.find(store => store.dwSystemID == KMX.KMXFile.TSS_KEYBOARDVERSION);
    return store ? store.dpString : null;
  }

  /**
   * Verify that the package contains a .js if the keyboard targets touch
   * devices
   */
  private verifyTargets(
    keyboard: KmpJsonFile.KmpJsonFileKeyboard,
    kmx: KMX.KEYBOARD,
    kmp: KmpJsonFile.KmpJsonFile
  ): void {
    // TODO: this function doesn't really belong in version validation, but we
    // don't want to read .kmx twice so we'll leave it here for now; a future
    // refactor could move .kmx loading into another class

    const store = kmx.stores.find(store => store.dwSystemID == KMX.KMXFile.TSS_TARGETS);
    if(!store) {
      // If there is no &TARGETS store, then there is nothing to check,
      // because default is windows-only
      return;
    }

    // Note, if we have gotten this far, we've already located and loaded a
    // .kmx, so no need to verify that the package includes a .kmx

    // If at least one target is a touch target, we need to check that the
    // package also includes the .js
    const targets = <KeymanTarget[]>store.dpString.split(' ');
    if(targets.some(target => ['any', ...TouchKeymanTargets].includes(target))) {
      if(!kmp.files.find(file => this.callbacks.path.basename(file.name, '.js') == keyboard.id)) {
        // .js version of the keyboard is not found, warn
        this.callbacks.reportMessage(CompilerMessages.Warn_JsKeyboardFileIsMissing({id: keyboard.id}));
      }
    }
  }
}
