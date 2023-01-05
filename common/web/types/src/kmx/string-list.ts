// import { constants } from '@keymanapp/ldml-keyboard-constants';
import { Strs, StrsItem } from './kmx-plus.js';

export class ListIndex {
  value: StrsItem; // will become index into Strs table
  isEqual(a: ListIndex | string) {
    // so we can compare this to a string
    return a.toString() === this.toString();
  }
  toString(): string {
    return this.value.value;
  }
};

export class ListItem extends Array<ListIndex> {
  constructor(strs: Strs, source: Array<string>) {
    super();
    if(!source) {
      return;
    }

    for (const str of source) {
        let index = new ListIndex();
        index.value = strs.allocString(str);
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
  toString(): string {
    return this.map(v => v.value.value).join(' ');
  }
};
