import { Strs, StrsItem } from './kmx-plus.js';

/**
 * A single entry in a ListItem.
 * Contains a StrsItem as its value.
 */
export class ListIndex {
  readonly value: StrsItem; // will become index into Strs table
  constructor(value: StrsItem) {
    this.value = value;
  }
  isEqual(a: ListIndex | string) {
    // so we can compare this to a string
    return a.toString() === this.toString();
  }
  toString(): string {
    return this.value.value;
  }
};

/**
 * A string list in memory. This will be replaced with an index
 * into the string table at finalization.
 */
export class ListItem extends Array<ListIndex> {
  /**
   * Construct a new list from an array of strings.
   * Use List. This is meant to be called by the List.allocString*() functions.
   * @param strs the Strs section is needed to construct this object.
   * @param source array of strings
   * @returns
   */
  constructor(strs: Strs, source: Array<string>) {
    super();
    if(!source) {
      return;
    }

    for (const str of source) {
        let index = new ListIndex(strs.allocString(str));
        this.push(index);
    }
  }
  isEqual(a: ListItem | string[]): boolean {
    if (a.length != this.length) {
      return false;
    }
    for (let i = 0; i < a.length; i++) {
      if (!this[i].isEqual(a[i])) {
        return false;
      }
    }
    return true;
  }
  compareTo(o: ListItem): number {
    for (let i = 0; i < Math.min(this.length, o.length); i++) {
      const r = this[i].value.compareTo(o[i].value);
      if (r !== 0) {
        return r;
      }
    }
    // prefix is the same, so go by length: shortest is first.
    if (this.length < o.length) {
      return -1;
    } else if (this.length > o.length) {
      return 1;
    } else {
      return 0;
    }
  }
  /** for debugging, print as single string */
  toString(): string {
    return this.toStringArray().join(' ');
  }
  /** for debugging, map to string array */
  toStringArray(): string[] {
    return this.map(v => v.value.value);
  }
};
