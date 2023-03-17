import { VariableStore, VariableStoreSerializer } from "@keymanapp/keyboard-processor";
import { CookieSerializer } from "keyman/engine/dom-utils";

// While there's little reason we couldn't store all of a keyboard's store values within
// the same cookie... that's not what we had implemented in the last pre-es-module version
// of KeymanWeb.  We're keeping this transformation _very_ straightforward.
//
// Also of note:  there's nothing we can do to allow TS to provide type-checking of
// dynamic property names; they'd have to be known at compile time to facilitate
// strict type checking.
class VarStoreSerializer extends CookieSerializer<VariableStore> {
  constructor(keyboardID: string, storeName: string) {
    super(`KeymanWeb_${keyboardID}_Option_${storeName}`);
  }

  load() {
    return super.load(decodeURIComponent);
  }

  save(storeMap: VariableStore) {
    super.save(storeMap, encodeURIComponent);
  }
}

export class VariableStoreCookieSerializer implements VariableStoreSerializer {
  loadStore(keyboardID: string, storeName: string): VariableStore {
    const storeCookieSerializer = new VarStoreSerializer(keyboardID, storeName);
    return storeCookieSerializer.load();
  }

  saveStore(keyboardID: string, storeName: string, storeMap: VariableStore) {
    const storeCookieSerializer = new VarStoreSerializer(keyboardID, storeName);
    storeCookieSerializer.save(storeMap);
  }
}