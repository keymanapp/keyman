import { VariableStore, VariableStoreSerializer } from "@keymanapp/keyboard-processor";

export class VariableStoreCookieSerializer implements VariableStoreSerializer {
  loadStore(keyboardID: string, storeName: string): VariableStore {
    var cName='KeymanWeb_'+keyboardID+'_Option_'+storeName;
    let map = com.keyman.singleton.util.loadCookie(cName) as VariableStore;

    if(typeof map[storeName] != 'undefined') {
      // Since it was stored in a cookie.
      map[storeName] = decodeURIComponent(map[storeName]);
    }

    return map || {};
  }

  saveStore(keyboardID: string, storeName: string, storeMap: VariableStore) {
    // The cookie entry includes the store name...
    var cName='KeymanWeb_'+keyboardID+'_Option_'+storeName;
    storeMap[storeName] = encodeURIComponent(storeMap[storeName]);

    // And the lookup under that entry looks for the value under the store name, again.
    com.keyman.singleton.util.saveCookie(cName, storeMap);
  }
}