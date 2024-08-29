// import { decodeNumber, decodeString } from './utils.js';

import { Strs } from "./strs.js";
import { decodeNumber } from "./utils.js";

export class List {
  readonly name = 'list';
  readonly data: Uint8Array;
  readonly entries: Readonly<Readonly<string[]>[]>;

  constructor(rawData: Uint8Array, strs: Strs) {
    this.data = rawData;

    const listCount = decodeNumber(rawData, 8);
    // const indexCount = decodeNumber(rawData, 12);

    const entries: string[][] = [];

    for(let i = 0; i < listCount; i++) {
      const rowOffset = 16 + i * 8;

      const listStart = decodeNumber(rawData, rowOffset);
      const listLen = decodeNumber(rawData, rowOffset + 4);

      const list: string[] = [];
      for(let j = 0; j < listLen; j++) {
        const strIndex = decodeNumber(rawData, listStart + 4 * j);
        list.push(strs.entries[strIndex]);
      }
      entries.push(list);
    }

    this.entries = entries;
  }
}