import { KeymanTargets, CompilerCallbacks, KmpJsonFile } from "@keymanapp/common-types";
import { CompilerMessages } from "./messages.js";
import { KeyboardMetadataCollection } from "./package-metadata-collector.js";


export class PackageKeyboardTargetValidator {
  constructor(private callbacks: CompilerCallbacks) {}

  public verifyAllTargets(kmp: KmpJsonFile.KmpJsonFile, metadata: KeyboardMetadataCollection): void {
    for(let keyboard of Object.keys(metadata)) {
      if(metadata[keyboard].data.targets) {
        // get the targets from the .kmx
        this.verifyTargets(metadata[keyboard].keyboard, metadata[keyboard].data.targets, kmp);
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
  ): void {
    // Note, if we have gotten this far, we've already located and loaded a
    // .kmx, so no need to verify that the package includes a .kmx

    // If at least one target is a touch target, we need to check that the
    // package also includes the .js
    const targets = KeymanTargets.keymanTargetsFromString(targetsText, {expandTargets: true});
    if(targets.some(target => KeymanTargets.TouchKeymanTargets.includes(target))) {
      if(!kmp.files.find(file => this.callbacks.path.basename(file.name, '.js') == keyboard.id)) {
        // .js version of the keyboard is not found, warn
        this.callbacks.reportMessage(CompilerMessages.Warn_JsKeyboardFileIsMissing({id: keyboard.id}));
      }
    }
  }
}