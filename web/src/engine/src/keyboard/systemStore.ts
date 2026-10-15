/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Declaration of a 'system store' (see kmn reference)
 */

export enum SystemStoreIDs {
  TSS_LAYER = 33,
  TSS_PLATFORM = 31,
  TSS_NEWLAYER = 42,
  TSS_OLDLAYER = 43
}

export type SystemStoreDictionary = { [id: number]: string };

/**
 * Defines common behaviors associated with system stores.
 */
export abstract class SystemStore {
  public readonly id: number;

  constructor(id: number) {
    this.id = id;
  }

  abstract matches(value: string): boolean;

  public set(_value: string): void {
    throw new Error("System store with ID " + this.id + " may not be directly set.");
  }
}

/**
 * A handler designed to receive feedback whenever a system store's value is changed.
 * @param source    The system store being mutated, before the value change occurs.
 * @param newValue  The new value being set
 * @returns         `false` / `undefined` to allow the change, `true` to block the change.
 */
export type SystemStoreMutationHandler = (source: MutableSystemStore, newValue: string) => boolean;

export class MutableSystemStore extends SystemStore {
  private _value: string;
  public handler?: SystemStoreMutationHandler = null;

  constructor(id: number, defaultValue: string) {
    super(id);
    this._value = defaultValue;
  }

  public get value() {
    return this._value;
  }

  public matches(value: string) {
    return this._value == value;
  }

  public set(value: string) {
    // Even if things stay the same, we should still signal this.
    // It's important for tracking if a rule directly set the layer
    // versus if it passively remained.
    if(this.handler) {
      if(this.handler(this, value)) {
        return;
      }
    }

    this._value = value;
  }
}

