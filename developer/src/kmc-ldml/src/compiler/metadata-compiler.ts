import { KMX, KMXPlus } from '@keymanapp/common-types';
import { LdmlCompilerOptions } from "./ldml-compiler-options.js";
import KEYMAN_VERSION from "@keymanapp/keyman-version";

import KMXPlusData = KMXPlus.KMXPlusData;
import KMXFile = KMX.KMXFile;
import KEYBOARD = KMX.KEYBOARD;

export class KMXPlusMetadataCompiler {
  /**
   * Look for metadata fields in the KMXPlus data and copy them
   * through to the relevant KMX stores
   * @param kmxplus const KMXPlusData
   */
  public static addKmxMetadata(kmxplus: KMXPlusData, keyboard: KEYBOARD, options: LdmlCompilerOptions): void {
    // Order of stores is not significant by kmx spec, but kmxplus compiler will
    // always store according to dwSystemID binary order for non-zero
    // dwSystemID, then by dpName binary order, and finally by dpString binary
    // order

    // TSS_NAME = 7
    // TSS_COMPILEDVERSION = 20
    // TSS_KEYBOARDVERSION = 36
    // TSS_TARGETS = 38

    // TSS_NAME: User friendly name of keyboard
    keyboard.stores.push({
      dpName: '&NAME',
      dpString: kmxplus.meta?.name?.value ?? 'unknown',  // Empty name should not happen, so ok to use 'unknown' here
      dwSystemID: KMXFile.TSS_NAME
    });

    if(options.shouldAddCompilerVersion) {
      // TSS_COMPILEDVERSION: version of the compiler
      keyboard.stores.push({
        dpName: '',
        dpString: KEYMAN_VERSION.VERSION_WITH_TAG,
        dwSystemID: KMXFile.TSS_COMPILEDVERSION
      });
    }

    // TSS_KEYBOARDVERSION: Version of the keyboard, should be semver
    keyboard.stores.push({
      dpName: '&KEYBOARDVERSION',
      dpString: kmxplus.meta?.version?.value ?? '1.0',  // 1.0 is inferred version
      dwSystemID: KMXFile.TSS_KEYBOARDVERSION
    });

    // TSS_TARGETS: which platforms are supported
    keyboard.stores.push({
      dpName: '&TARGETS',
      dpString: 'desktop',    // TODO-LDML: support touch layouts in #7238
      dwSystemID: KMXFile.TSS_TARGETS
    });
  }

}
