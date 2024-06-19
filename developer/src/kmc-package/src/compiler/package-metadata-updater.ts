import { KeyboardMetadataCollection } from './package-metadata-collector.js';

export class PackageMetadataUpdater {

  public updatePackage(metadata: KeyboardMetadataCollection) {
    for(let id of Object.keys(metadata)) {
      const keyboard = metadata[id];
      keyboard.keyboard.name = keyboard.data.keyboardName;
      keyboard.keyboard.rtl = keyboard.data.isRtl ? true : undefined;
      keyboard.keyboard.version = keyboard.data.keyboardVersion ?? '1.0';
    }
  }
}
