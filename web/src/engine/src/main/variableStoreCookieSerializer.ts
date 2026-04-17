import { VariableStores, VariableStoreSerializer } from 'keyman/engine/keyboard';
import { CookieSerializer } from "keyman/engine/dom-utils";

export class VariableStoreCookieSerializer implements VariableStoreSerializer {
  private getStoreCookieName(keyboardID: string, storeName: string): string {
    return `KeymanWeb_${keyboardID}_Option_${storeName}`;
  }

  public loadStore(keyboardID: string, storeName: string): string {
    // Note, for historical reasons, we pass a VariableStores object into the
    // cookie serializer but expect only a single value to be loaded
    const storeCookieSerializer = new CookieSerializer<VariableStores>(this.getStoreCookieName(keyboardID, storeName));
    return storeCookieSerializer.load(decodeURIComponent)[storeName];
  }

  public saveStore(keyboardID: string, storeName: string, storeValue: string) {
    // Note, for historical reasons, we pass a VariableStores object into the
    // cookie serializer but save only a single value
    const storeCookieSerializer = new CookieSerializer<VariableStores>(this.getStoreCookieName(keyboardID, storeName));
    storeCookieSerializer.save({[storeName]: storeValue}, encodeURIComponent);
  }
}