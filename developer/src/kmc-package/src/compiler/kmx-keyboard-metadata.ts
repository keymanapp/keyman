import { KMX } from "@keymanapp/common-types";
import { WebKeyboardMetadata } from "./web-keyboard-metadata.js";

export function getCompiledKmxKeyboardMetadata(kmx: KMX.KEYBOARD): WebKeyboardMetadata {
  return {
    isMnemonic: getStoreFromKmx(kmx, KMX.KMXFile.TSS_MNEMONIC) === '1',
    isRtl: getStoreFromKmx(kmx, KMX.KMXFile.TSS_KMW_RTL) === '1',
    keyboardName: getStoreFromKmx(kmx, KMX.KMXFile.TSS_NAME),
    keyboardVersion: getStoreFromKmx(kmx, KMX.KMXFile.TSS_KEYBOARDVERSION),
    minKeymanVersion: ((kmx.fileVersion & 0xFF00) >> 8).toString() + '.' + (kmx.fileVersion & 0xFF).toString(),
    targets: getStoreFromKmx(kmx, KMX.KMXFile.TSS_TARGETS) ?? 'windows',
    hasTouchLayout: !!getStoreFromKmx(kmx, KMX.KMXFile.TSS_LAYOUTFILE)
  };
}

function getStoreFromKmx(kmx: KMX.KEYBOARD, id: number): string {
  const store = kmx.stores.find(store => store.dwSystemID == id);
  return store ? store.dpString : null;
}
