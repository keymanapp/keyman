import { KmpJsonFile } from '@keymanapp/common-types';
import { PackageCompilerMessages } from './package-compiler-messages.js';
import { KeyboardMetadataCollection } from './package-metadata-collector.js';
import { KpsFile, CompilerCallbacks } from '@keymanapp/developer-utils';

export const DEFAULT_KEYBOARD_VERSION = '1.0';
export const MIN_LM_FILEVERSION_KMP_JSON = '12.0';
export const MIN_KBD_FILEVERSION_KMP_JSON = '7.0';

export class PackageVersionValidator {

  constructor(private callbacks: CompilerCallbacks) {}

  public getMinKeymanVersion(keyboardMetadata: KeyboardMetadataCollection): string {
    let minKeymanVersion = MIN_KBD_FILEVERSION_KMP_JSON;
    for(const id of Object.keys(keyboardMetadata)) {
      // minKeymanVersion is always a float so float comparison is safe
      const kbdMinKeymanVersion = parseFloat(keyboardMetadata[id].data.minKeymanVersion);
      if(kbdMinKeymanVersion > 14.0) {
        // Keyman for Windows 14 and earlier only accepted version 7.0 for keyboard
        // packages, so we must not write any other version in order to allow
        // earlier versions of Keyman to load the package.
        if(parseFloat(minKeymanVersion) < kbdMinKeymanVersion) {
          minKeymanVersion = keyboardMetadata[id].data.minKeymanVersion;
        }
      }
    }
    return minKeymanVersion;
  }
  /**
   * Verifies version information in corresponding keyboards and updates kmpJson
   * metadata as the version information can be out of sync in the .kps file
   * after updating a contained keyboard.
   * @param kps
   * @param kmp
   * @returns
   */
  public validateAndUpdateVersions(kps: KpsFile.KpsFile, kmp: KmpJsonFile.KmpJsonFile, keyboardMetadata: KeyboardMetadataCollection) {
    const followKeyboardVersion = kps.Options?.FollowKeyboardVersion !== undefined;

    if(followKeyboardVersion) {
      if(!this.checkFollowKeyboardVersion(kmp)) {
        return false;
      }
    } else {
      if(!kmp.info.version) {
        this.callbacks.reportMessage(PackageCompilerMessages.Error_PackageFileHasEmptyVersion());
        return false;
      }
    }

    if(!kmp.keyboards) {
      // Lexical models don't have version metadata; only their packages.
      return true;
    }

    const result = true;

    // We now know we have at least one keyboard in the package

    for(const keyboard of kmp.keyboards) {
      const data = keyboardMetadata[keyboard.id];
      if(!data) {
        // Warnings or errors will have been raised by getKeyboardFileData
        // TODO raise error -- we should not have gotten here
        return false;
      }

      // Note: there is often version metadata in the .kps <Keyboard> element, but
      // we don't read from the metadata because we want to ensure we have the
      // most up-to-date keyboard version data here, from the compiled keyboard.

      if(followKeyboardVersion && data.data.keyboardVersion === null) {
        this.callbacks.reportMessage(PackageCompilerMessages.Info_KeyboardFileHasNoKeyboardVersion({filename: keyboard.id}));
      }
      keyboard.version = data.data.keyboardVersion ?? DEFAULT_KEYBOARD_VERSION;

      if(result && followKeyboardVersion) {
        if(kmp.keyboards[0].version !== keyboard.version) {
          this.callbacks.reportMessage(PackageCompilerMessages.Warn_KeyboardVersionsDoNotMatch({
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

    if(result && !isValidVersionNumber(kmp.info.version.description)) {
      this.callbacks.reportMessage(PackageCompilerMessages.Warn_PackageVersionIsUnrecognizedFormat({version: kmp.info.version.description}));
      return false;
    }

    return result;
  }

  private checkFollowKeyboardVersion(kmp: KmpJsonFile.KmpJsonFile) {
    // Lexical model packages do not allow FollowKeyboardVersion
    if(kmp.lexicalModels && kmp.lexicalModels.length) {
      this.callbacks.reportMessage(PackageCompilerMessages.Error_FollowKeyboardVersionNotAllowedForModelPackages());
      return false;
    }

    if(!kmp.keyboards || !kmp.keyboards.length) {
      this.callbacks.reportMessage(PackageCompilerMessages.Error_FollowKeyboardVersionButNoKeyboards());
      return false;
    }

    return true;
  }
}

function isValidVersionNumber(version: string) {
  return /^(0|[1-9][0-9]*)(\.(0|[1-9][0-9]*)){0,2}$/.test(version);
}

export const unitTestEndpoints = {
  isValidVersionNumber,
};
