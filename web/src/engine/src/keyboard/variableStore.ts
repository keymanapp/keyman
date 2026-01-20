/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Definitions for variable stores (see kmn reference)
 */

export interface VariableStoreDictionary {
  [name: string]: string;
};

export type VariableStore = { [name: string]: string; };

export interface VariableStoreSerializer {
  loadStore(keyboardID: string, storeName: string): VariableStore;
  saveStore(keyboardID: string, storeName: string, storeMap: VariableStore): void;
  findStores(keyboardID: string): VariableStore[];
}
