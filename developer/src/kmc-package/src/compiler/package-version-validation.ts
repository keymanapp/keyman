import { KmpJsonFile, CompilerCallbacks, KpsFile, KmxFileReader, KMX } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';

export class PackageVersionValidation {

  constructor(private callbacks: CompilerCallbacks) {}

  /**
   * Verifies version information in corresponding keyboards and updates kmpJson
   * metadata as the version information can be out of sync in the .kps file
   * after update a contained keyboard.
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

    let result = true;

    if(!kmp.keyboards) {
      // Lexical model packages don't have version metadata
      return true;
    }

    // We now know we have at least one keyboard in the package

    for(let keyboard of kmp.keyboards) {
      result = this.updateKeyboardVersionFromKmx(kpsFilename, kmp, keyboard) && result;
      if(result) {
        if(kmp.keyboards[0].version !== keyboard.version) {
          this.callbacks.reportMessage(CompilerMessages.Warn_KeyboardVersionsDoNotMatch({
            keyboard:keyboard.id,
            version:keyboard.version,
            firstKeyboard:kmp.keyboards[0].id,
            firstVersion:kmp.keyboards[0].version
          }));
        }
      }
    }

    if(result) {
      if(followKeyboardVersion) {
        kmp.info.version.description = kmp.keyboards[0].version;
      }
      else if(kmp.info.version?.description != kmp.keyboards[0].version) {
        // Only need to compare against first keyboard as we compare keyboards above
        this.callbacks.reportMessage(CompilerMessages.Warn_KeyboardVersionsDoNotMatchPackageVersion({
          keyboard: kmp.keyboards[0].id,
          keyboardVersion: kmp.keyboards[0].version,
          packageVersion: kmp.info.version.description
        }));
      }
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

  private updateKeyboardVersionFromKmx(
    kpsFilename: string,
    kmp: KmpJsonFile.KmpJsonFile,
    keyboard: KmpJsonFile.KmpJsonFileKeyboard
  ): boolean {
    // The DEFAULT_VERSION used to be '1.0', but we now use '0.0' to allow
    // pre-release 0.x keyboards to be considered later than a keyboard without
    // any version metadata at all.
    const DEFAULT_VERSION = '0.0';

    // Note: there is often version metadata in the .kps <Keyboard> element, but
    // we don't read from the metadata because we want to ensure we have the
    // most up-to-date keyboard version data here, from the compiled keyboard.

    // Reset the keyboard version to the default in the kmp.json metadata, for
    // warning/failure code paths in this file
    keyboard.version = DEFAULT_VERSION;

    const file = kmp.files.find(file => this.callbacks.path.basename(file.name, '.kmx') == keyboard.id);
    if(!file) {
      this.callbacks.reportMessage(CompilerMessages.Error_KeyboardContentFileNotFound({id:keyboard.id}));
      return false;
    }

    const filename = this.callbacks.resolveFilename(kpsFilename, file.name);
    if(!this.callbacks.fs.existsSync(filename)) {
      this.callbacks.reportMessage(CompilerMessages.Error_KeyboardFileNotFound({filename}));
      return false;
    }

    //
    // load the .kmx and extract the version number
    //
    let kmxFileData;
    try {
      kmxFileData = this.callbacks.loadFile(filename);
    } catch(e) {
      this.callbacks.reportMessage(CompilerMessages.Error_FileCouldNotBeRead({filename, e}));
      return false;
    }
    const kmxReader: KmxFileReader = new KmxFileReader();
    const kmx: KMX.KEYBOARD = kmxReader.read(kmxFileData);
    if(!kmx) {
      // The file couldn't be read, it might not be a .kmx file
      this.callbacks.reportMessage(CompilerMessages.Error_KeyboardFileNotValid({filename}));
      return false;
    }

    const store = kmx.stores.find(store => store.dwSystemID == KMX.KMXFile.TSS_KEYBOARDVERSION);
    if(!store) {
      // We have no version number store, so use default version
      this.callbacks.reportMessage(CompilerMessages.Warn_KeyboardFileHasNoKeyboardVersion({filename}));
      return true;
    }

    keyboard.version = store.dpString;
    return true;
  }
}
