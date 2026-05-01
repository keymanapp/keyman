/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Definitions for variable stores (see kmn reference)
 */

export type VariableStores = { [name: string]: string; };

export interface VariableStoreSerializer {
  loadStore(keyboardID: string, storeName: string): string | undefined;
  saveStore(keyboardID: string, storeName: string, storeValue: string): void;
}
