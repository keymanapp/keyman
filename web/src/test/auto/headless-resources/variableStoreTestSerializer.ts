/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { VariableStoreSerializer } from "keyman/engine/keyboard";

export class VariableStoreTestSerializer implements VariableStoreSerializer {
  private stores: {[name: string]: string} = {};

  private getStoreName(keyboardID: string, storeName: string): string {
    return `KeymanWeb_${keyboardID}_Option_${storeName}`;
  }

  loadStore(keyboardID: string, storeName: string): string {
    return this.stores[this.getStoreName(keyboardID, storeName)] ?? '';
  }

  saveStore(keyboardID: string, storeName: string, storeValue: string): void {
    this.stores[this.getStoreName(keyboardID, storeName)] = storeValue;
  }
}
