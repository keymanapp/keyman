import { decodeNumber, decodeString } from './utils.js';

export class Strs {
  readonly name = 'strs';
  readonly data: Uint8Array;
  readonly entries: Readonly<string[]>;

  constructor(rawData: Uint8Array) {
    this.data = rawData;

    const count = decodeNumber(rawData, 8);

    const entries: string[] = [];

    for(let i=0; i < count; i++) {
      const rowOffset = 12 + i * 8;

      const offset = decodeNumber(rawData, rowOffset);
      const len = decodeNumber(rawData, rowOffset + 4);

      entries.push(decodeString(rawData, offset, len));
    }

    this.entries = entries;
  }
}