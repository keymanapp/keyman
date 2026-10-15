/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Assign collected metadata from keyboards to package metadata fields
 */
import { KeyboardMetadataCollection } from './package-metadata-collector.js';

export class PackageMetadataUpdater {

  public updatePackage(metadata: KeyboardMetadataCollection) {
    for(const id of Object.keys(metadata)) {
      const keyboard = metadata[id];
      keyboard.keyboard.name = keyboard.data.keyboardName ?? id;
      keyboard.keyboard.rtl = keyboard.data.isRtl ? true : undefined;
      keyboard.keyboard.version = keyboard.data.keyboardVersion ?? '1.0';
    }
  }
}
