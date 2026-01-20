import { VariableStore, VariableStoreSerializer } from 'keyman/engine/keyboard';
import { CookieSerializer } from "keyman/engine/dom-utils";

// While there's little reason we couldn't store all of a keyboard's store values within
// the same cookie... that's not what we had implemented in the last pre-es-module version
// of KeymanWeb.  We're keeping this transformation _very_ straightforward.
//
// Also of note:  there's nothing we can do to allow TS to provide type-checking of
// dynamic property names; they'd have to be known at compile time to facilitate
// strict type checking.
class VarStoreSerializer extends CookieSerializer<VariableStore> {
  public constructor(keyboardID: string, storeName: string) {
    super(`KeymanWeb_${keyboardID}_Option_${storeName}`);
  }

  public load(): VariableStore {
    return super.load(decodeURIComponent);
  }

  public save(storeMap: VariableStore) {
    super.save(storeMap, encodeURIComponent);
  }

  /**
   * Find all variable stores associated with a given keyboard.
   *
   * @param {string}  keyboardID  The keyboard ID whose variable stores are to be found.
   *
   * @returns An array of VariableStore objects found for the keyboard.
   */
  public static findStores(keyboardID: string): VariableStore[] {
    const pattern = new RegExp(`^KeymanWeb_${keyboardID}_Option_`);
    const matching = CookieSerializer.loadAllMatching<VariableStore>(pattern, decodeURIComponent);
    return matching.map(m => m.value);
  }
}

export class VariableStoreCookieSerializer implements VariableStoreSerializer {
  public loadStore(keyboardID: string, storeName: string): VariableStore {
    const storeCookieSerializer = new VarStoreSerializer(keyboardID, storeName);
    return storeCookieSerializer.load();
  }

  public saveStore(keyboardID: string, storeName: string, storeMap: VariableStore) {
    const storeCookieSerializer = new VarStoreSerializer(keyboardID, storeName);
    storeCookieSerializer.save(storeMap);
  }

  /**
   * Find all variable stores associated with a given keyboard.
   *
   * @param {string}  keyboardID  The keyboard ID whose variable stores are to be found.
   *
   * @returns An array of VariableStore objects found for the keyboard.
   */
  public findStores(keyboardID: string): VariableStore[] {
    return VarStoreSerializer.findStores(keyboardID);
  }
}