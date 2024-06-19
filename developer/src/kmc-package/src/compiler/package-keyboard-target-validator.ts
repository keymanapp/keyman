import { KeymanTargets, CompilerCallbacks, KmpJsonFile } from "@keymanapp/common-types";
import { CompilerMessages } from "./package-compiler-messages.js";
import { KeyboardMetadataCollection } from "./package-metadata-collector.js";


export class PackageKeyboardTargetValidator {
  constructor(private callbacks: CompilerCallbacks) {}

  public verifyAllTargets(kmp: KmpJsonFile.KmpJsonFile, metadata: KeyboardMetadataCollection): void {
    for(let keyboard of Object.keys(metadata)) {
      let hasJS = true;

      if(metadata[keyboard].data.targets) {
        // get the targets from the .kmx (only the .kmx has the targets data)
        hasJS = this.verifyTargets(metadata[keyboard].keyboard, metadata[keyboard].data.targets, kmp);
      }
      if(hasJS && !metadata[keyboard].data.hasTouchLayout) {
        this.callbacks.reportMessage(CompilerMessages.Hint_JsKeyboardFileHasNoTouchTargets({id: metadata[keyboard].keyboard.id}));
      }
    }
  }

  /**
   * Verify that the package contains a .js if the keyboard targets touch
   * devices
   */
  private verifyTargets(
    keyboard: KmpJsonFile.KmpJsonFileKeyboard,
    targetsText: string,
    kmp: KmpJsonFile.KmpJsonFile
  ): boolean {
    // Note, if we have gotten this far, we've already located and loaded a
    // .kmx, so no need to verify that the package includes a .kmx

    // If at least one target is a touch target, we need to check that the
    // package also includes the .js
    const targets = KeymanTargets.keymanTargetsFromString(targetsText, {expandTargets: true});
    if(targets.some(target => KeymanTargets.TouchKeymanTargets.includes(target))) {
      if(!kmp.files.find(file => this.callbacks.path.basename(file.name ?? '', '.js') == keyboard.id)) {
        // .js version of the keyboard is not found, warn
        this.callbacks.reportMessage(CompilerMessages.Warn_JsKeyboardFileIsMissing({id: keyboard.id}));
        return false;
      }
      // A js file is included and targeted
      return true;
    }
    // js is not targeted
    return false;
  }
}